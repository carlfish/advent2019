{-# LANGUAGE FlexibleContexts #-}

module IntCode 
  ( Computer
  , MWord
  , smallComputer
  , bigComputer
  , parser
  , runComputerPure
  ) where

import qualified Data.Attoparsec.ByteString.Char8 as AP
import qualified Data.Vector.Unboxed as V

import Lib (commaSeparated)
import Data.Maybe (maybe)
import Control.Monad.Except
import Control.Monad.State
import Conduit
import Data.Int (Int64(..))
import Data.Vector.Unboxed (Vector, (//), (!), fromList)


type MWord = Int64
type Heap = Vector MWord 
newtype Addr = Addr Int deriving (Show, Eq, Ord)
type Computer m a = ConduitT MWord MWord m a
type Program m = StateT RuntimeState (ConduitT MWord MWord m)
type ParamReader = MWord -> Param

data RuntimeState = RuntimeState  
  { interpreterState :: InterpreterState
  , heap :: Heap
  , instructionPointer :: Addr
  , relativeBase :: MWord 
  }


data Op
  = Sum Param Param Param
  | Mul Param Param Param
  | ReadIn Param
  | WriteOut Param
  | JumpIfTrue Param Param
  | JumpIfFalse Param Param
  | LessThan Param Param Param
  | Equals Param Param Param
  | AdjustRelBase Param
  | Halt
  deriving (Show)

data InterpreterState 
  = Reading
  | Running Op
  | Stopped
  deriving (Show)

data Param 
  = Immediate MWord 
  | Ref Addr
  | RelRef MWord
  deriving (Show)

intToWord :: Int -> MWord
intToWord = fromInteger . toInteger

wordToInt :: MWord -> Int
wordToInt = fromInteger . toInteger

truncateAddr :: MWord -> Addr
truncateAddr = Addr . wordToInt

addrVal :: Addr -> Int
addrVal (Addr i) = i

a0 :: Addr
a0 = Addr 0
  
incAddr :: Addr -> Addr
incAddr (Addr c) = Addr (c + 1)

noOp :: Program m ()
noOp = return ()

output :: Monad m => MWord -> Program m ()
output i = lift (yield i)

readInput :: Monad m => MonadError String m => Program m MWord
readInput = do
  mi <- lift await
  maybe (throwError "Ran out of input values") return mi

setInstructionPointer :: Addr -> Program m ()
setInstructionPointer a = modify (\s -> s{ instructionPointer = a})

setInstructionPointerFrom :: Param -> Program m ()
setInstructionPointerFrom p = setInstructionPointer =<< (truncateAddr <$> readParam p)

setIS :: InterpreterState -> Program m ()
setIS is = modify (\s -> s { interpreterState = is })

getRelativeBase :: Program m MWord
getRelativeBase = gets relativeBase

modifyRelativeBase :: MWord -> Program m ()
modifyRelativeBase m = modify (\s -> s { relativeBase = (relativeBase s) + m })

writeHeap :: Addr -> MWord -> Program m ()
writeHeap (Addr a) v = if a < 0 then undefined else  modify (\s -> s { heap = (heap s) // [(a, v)] })

readHeap :: Addr -> Program m MWord
readHeap (Addr a) = if a < 0 then undefined else gets (\s -> (heap s) ! a)

readParam :: Param -> Program m MWord
readParam (Immediate i) = return i
readParam (Ref a)       = readHeap a
readParam (RelRef i)    = (\offset -> readHeap (truncateAddr (i + offset))) =<< getRelativeBase

writeAtParam :: Param -> MWord -> Program m ()
writeAtParam (Immediate i) v = undefined
writeAtParam (Ref a)       v = writeHeap a v
writeAtParam (RelRef i)    v = (\offset -> writeHeap (truncateAddr (i + offset)) v) =<< getRelativeBase

readNext :: Program m MWord
readNext = state 
  (\s -> 
    ( (heap s) ! ((addrVal . instructionPointer) s)
    , s { instructionPointer = incAddr (instructionPointer s) }
    )
  )
  
binaryOp :: (MWord -> MWord -> MWord) -> Param -> Param -> Param -> Program m ()
binaryOp f ia ib io = writeAtParam io =<< (f <$> readParam ia <*> readParam ib)

branch :: a -> a -> Bool -> a
branch ifTrue ifFalse condition = if condition then ifTrue else ifFalse

test :: (MWord -> Bool) -> Param -> Program m Bool
test f p = f <$> readParam p

isLt, isEq :: Param -> Param -> Program m Bool
isLt p1 p2 = (\a b -> a < b) <$> readParam p1 <*> readParam p2
isEq p1 p2 = (\a b -> a == b) <$> readParam p1 <*> readParam p2

run :: MonadError String m => Op -> Program m ()
run op = run' >> setIS (nextState op)
  where 
    nextState Halt = Stopped
    nextState _    = Reading

    run' = case op of 
      Sum ia ib io     -> binaryOp (+) ia ib io
      Mul ia ib io     -> binaryOp (*) ia ib io                      
      ReadIn a         -> writeAtParam a =<< readInput
      WriteOut a       -> output =<< readParam a                       
      JumpIfTrue p a   -> branch (setInstructionPointerFrom a) noOp  =<< test (/= 0) p
      JumpIfFalse p a  -> branch (setInstructionPointerFrom a) noOp  =<< test (== 0) p
      LessThan p1 p2 p3 -> branch (writeAtParam p3 1) (writeAtParam p3 0) =<< isLt p1 p2
      Equals p1 p2 p3   -> branch (writeAtParam p3 1) (writeAtParam p3 0) =<< isEq p1 p2
      AdjustRelBase p1 -> modifyRelativeBase =<< readParam p1
      Halt             -> noOp

parseOpcode :: MonadError String m => MWord -> m (ParamReader, ParamReader, ParamReader, MWord)
parseOpcode opcode = 
  let
    parseParam 0 = return (Ref . truncateAddr)
    parseParam 1 = return Immediate
    parseParam 2 = return RelRef
    parseParam i = throwError ("Bad param designator: " <> (show i))
    digitsAt d n = opcode `div` ((10 ^ d)) `mod` (10 ^ n)
  in 
    (,,,) <$> (parseParam (digitsAt 2 1)) 
          <*> (parseParam (digitsAt 3 1)) 
          <*> (parseParam (digitsAt 4 1))
          <*> (return (digitsAt 0 2))

interpret :: MonadError String m => MWord -> Program m()
interpret opcode =
  let
    readNext3 f  = f <$> readNext <*> readNext <*> readNext
    readNext2 f  = f <$> readNext <*> readNext
    intToParam3 p1 p2 p3 f = \x y z -> f (p1 x) (p2 y) (p3 z)
    intToParam2 p1 p2 f = \x y -> f (p1 x) (p2 y)
  in (setIS . Running) =<< do
    (p1, p2, p3, code) <- parseOpcode opcode
    case code of
      1  -> readNext3 (intToParam3 p1 p2 p3 Sum)
      2  -> readNext3 (intToParam3 p1 p2 p3 Mul)
      3  -> ReadIn <$> p1 <$> readNext
      4  -> WriteOut <$> p1 <$> readNext
      5  -> readNext2 (intToParam2 p1 p2 JumpIfTrue)
      6  -> readNext2 (intToParam2 p1 p2 JumpIfFalse)
      7  -> readNext3 (intToParam3 p1 p2 p3 LessThan)
      8  -> readNext3 (intToParam3 p1 p2 p3 Equals)
      9  -> AdjustRelBase <$> p1 <$> readNext
      99 -> return Halt
      i  -> throwError ("Unknown opcode: " <> (show i))

runInterpreter :: MonadError String m => Program m ()
runInterpreter = do
  is <- gets interpreterState
  case is of
    Reading    -> readNext >>= interpret >> runInterpreter
    Running op -> run op >> runInterpreter
    Stopped    -> return ()

-- Big Computer has a whole ten kilobytes (!) of RAM
-- Note that things being what they are, increasing the
-- total RAM of the computer reduces performance by orders
-- of magnitude, so if we want to build one with more memory
-- we're going to have to rip everything out and go with
-- mutable arrays.
bigComputer :: MonadError String m => [ MWord ] -> Computer m ()
bigComputer startingHeap = 
  let 
    initHeap = V.replicate (10 * 1024) 0
    loadProgram = initHeap // (zip [0..] startingHeap)
    initState = RuntimeState 
      { interpreterState = Reading
      , heap = loadProgram 
      , instructionPointer = a0
      , relativeBase = 0
      }
  in 
    evalStateT runInterpreter initState

-- Small computer only has as much RAM as is required to load its starting heap
smallComputer :: MonadError String m => [ MWord ] -> Computer m ()
smallComputer startingHeap = 
  let 
    initMem = fromList startingHeap
    initState = RuntimeState 
      { interpreterState = Reading
      , heap = initMem 
      , instructionPointer = a0
      , relativeBase = 0
      }
  in 
    evalStateT runInterpreter initState

runComputerPure :: [ MWord ] -> Computer (Either String) () -> Either String [ MWord ]
runComputerPure input c = runConduit
  (  yieldMany input
  .| c
  .| sinkList
  )

parser :: AP.Parser [ MWord ]
parser = commaSeparated (AP.signed AP.decimal)