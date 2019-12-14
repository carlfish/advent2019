module IntCode 
  ( Computer
  , MWord
  , Program
  , smallComputer
  , bigComputer
  , bigComputer'
  , parser
  , runComputerPure
  , wordToInt
  , feedbackSource
  , modifyMemory
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
import Control.Concurrent.MVar

type MWord = Int64
type Heap = Vector MWord 
newtype Addr = Addr Int deriving (Show, Eq, Ord)

type Computer m a = ConduitT MWord MWord (ExceptT String m) a
type Program m = StateT RuntimeState (ConduitT MWord MWord (ExceptT String m))
type ParamReader = MWord -> Param

data RuntimeState = RuntimeState  
  { interpreterState :: InterpreterState
  , heap :: Heap
  , instructionPointer :: Addr
  , relativeBase :: MWord 
  }

data InterpreterState 
  = Reading
  | Running Op
  | Stopped
  deriving (Show)

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

data Param 
  = Immediate MWord 
  | Ref Addr
  | RelRef MWord
  deriving (Show)

-- Dealing with addresses and words

intToWord :: Int -> MWord
intToWord = fromInteger . toInteger

wordToInt :: MWord -> Int
wordToInt = fromInteger . toInteger

truncateAddr :: MWord -> Addr
truncateAddr = Addr . wordToInt

addrToInt :: Addr -> Int
addrToInt (Addr i) = i
 
incAddr :: Addr -> Addr
incAddr (Addr c) = Addr (c + 1)

-- Pieces of programs

noOp :: Program m ()
noOp = return ()

raiseError :: Monad m => String -> Program m a
raiseError e = (\is -> throwError (e <> " Last state: " <> (show is))) =<< gets interpreterState

output :: Monad m => MWord -> Program m ()
output i = lift (yield i)

readInput :: Monad m => Program m MWord
readInput = maybe (throwError "Ran out of input values") return =<< lift await

setInstructionPointer :: Addr -> Program m ()
setInstructionPointer a = modify (\s -> s{ instructionPointer = a})

setInstructionPointerFrom :: Monad m => Param -> Program m ()
setInstructionPointerFrom p = setInstructionPointer =<< (truncateAddr <$> readParam p)

setIS :: InterpreterState -> Program m ()
setIS is = modify (\s -> s { interpreterState = is })

getRelativeBase :: Program m MWord
getRelativeBase = gets relativeBase

modifyRelativeBase :: MWord -> Program m ()
modifyRelativeBase m = modify (\s -> s { relativeBase = (relativeBase s) + m })

writeHeap :: Monad m => Addr -> MWord -> Program m ()
writeHeap (Addr a) v = 
  if a < 0 then 
    raiseError ("Tried to write to negative address " <> (show a))
  else 
    modify (\s -> s { heap = (heap s) // [(a, v)] })

readHeap :: Monad m => Addr -> Program m MWord
readHeap (Addr a) = 
  if a < 0 then 
    raiseError ("Tried to read from negative address " <> (show a))
  else 
    gets (\s -> (heap s) ! a)

readParam :: Monad m => Param -> Program m MWord
readParam (Immediate i) = return i
readParam (Ref a)       = readHeap a
readParam (RelRef i)    = (\offset -> readHeap (truncateAddr (i + offset))) =<< getRelativeBase

writeAtParam :: Monad m => Param -> MWord -> Program m ()
writeAtParam (Immediate i) v = raiseError "Unsupported immediate param mode on write"
writeAtParam (Ref a)       v = writeHeap a v
writeAtParam (RelRef i)    v = (\offset -> writeHeap (truncateAddr (i + offset)) v) =<< getRelativeBase

readNext :: Program m MWord
readNext = state 
  (\s -> 
    ( (heap s) ! ((addrToInt . instructionPointer) s)
    , s { instructionPointer = incAddr (instructionPointer s) }
    )
  )
  
binaryOp :: Monad m => (MWord -> MWord -> MWord) -> Param -> Param -> Param -> Program m ()
binaryOp f ia ib io = writeAtParam io =<< (f <$> readParam ia <*> readParam ib)

branch :: a -> a -> Bool -> a
branch ifTrue ifFalse condition = if condition then ifTrue else ifFalse

test :: Monad m => (MWord -> Bool) -> Param -> Program m Bool
test f p = f <$> readParam p

isLt, isEq :: Monad m => Param -> Param -> Program m Bool
isLt p1 p2 = (\a b -> a < b) <$> readParam p1 <*> readParam p2
isEq p1 p2 = (\a b -> a == b) <$> readParam p1 <*> readParam p2

-- The guts of the interpreter

run :: Monad m => Op -> Program m ()
run op = run' >> setIS (nextState op)
  where 
    nextState Halt = Stopped
    nextState _    = Reading

    run' = case op of 
      Sum ia ib io      -> binaryOp (+) ia ib io
      Mul ia ib io      -> binaryOp (*) ia ib io                      
      ReadIn a          -> writeAtParam a =<< readInput
      WriteOut a        -> output =<< readParam a                       
      JumpIfTrue p a    -> branch (setInstructionPointerFrom a) noOp  =<< test (/= 0) p
      JumpIfFalse p a   -> branch (setInstructionPointerFrom a) noOp  =<< test (== 0) p
      LessThan p1 p2 p3 -> branch (writeAtParam p3 1) (writeAtParam p3 0) =<< isLt p1 p2
      Equals p1 p2 p3   -> branch (writeAtParam p3 1) (writeAtParam p3 0) =<< isEq p1 p2
      AdjustRelBase p1  -> modifyRelativeBase =<< readParam p1
      Halt              -> noOp

parseOpcode :: Monad m => MWord -> Program m (ParamReader, ParamReader, ParamReader, MWord)
parseOpcode opcode = 
  let
    parseParam 0 = return (Ref . truncateAddr)
    parseParam 1 = return Immediate
    parseParam 2 = return RelRef
    parseParam i = raiseError ("Bad param designator: " <> (show i))
    digitsAt d n = opcode `div` ((10 ^ d)) `mod` (10 ^ n)
  in 
    (,,,) <$> (parseParam (digitsAt 2 1)) 
          <*> (parseParam (digitsAt 3 1)) 
          <*> (parseParam (digitsAt 4 1))
          <*> (return (digitsAt 0 2))

interpret :: Monad m => MWord -> Program m ()
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
      i  -> raiseError ("Unknown opcode: " <> (show i))

runInterpreter :: Monad m => Program m ()
runInterpreter = do
  is <- gets interpreterState
  case is of
    Reading    -> readNext >>= interpret >> runInterpreter
    Running op -> run op >> runInterpreter
    Stopped    -> return ()

-- Frameworks for running code through the interpreter

computer :: Monad m => Program m () -> Vector MWord -> Computer m ()
computer runFirst startingHeap = 
  let 
    initState = RuntimeState 
      { interpreterState = Reading
      , heap = startingHeap 
      , instructionPointer = (Addr 0)
      , relativeBase = 0
      }
  in 
    evalStateT (runFirst >> runInterpreter) initState

-- Big Computer has a whole ten kilobytes (!) of RAM
-- Note that with immutable RAM, the total memory size has a HUGE impact on 
-- performance, so anything larger and we'll need to look into mutable state.
bigComputer :: Monad m => [ MWord ] -> Computer m ()
bigComputer = bigComputer' noOp

bigComputer' :: Monad m => Program m () -> [ MWord ] -> Computer m ()
bigComputer' runFirst programCode = computer runFirst $ (V.replicate (10 * 1024) 0) // (zip [0..] programCode)

-- Small computer only has as much RAM as is required to load its starting heap
smallComputer :: Monad m => [ MWord ] -> Computer m ()
smallComputer programCode = computer noOp (fromList programCode)

runComputerPure :: [ MWord ] -> Computer Identity () -> Either String [ MWord ]
runComputerPure input c = runIdentity $ runExceptT $ runConduit
  (  yieldMany input
  .| c
  .| sinkList
  )

-- Helpers

-- Some programs want us to twiddle a bit in RAM before running them
modifyMemory :: Monad m => MWord -> MWord -> Program m ()
modifyMemory a b = writeHeap (truncateAddr a) b

-- Simple MVar-backed way to send a single value from the output of
-- the computer back to the input. Assumes that a single input value
-- will always be sufficient to trigger the next one.
feedbackSource :: MonadIO m => MVar MWord -> ConduitT () MWord m ()
feedbackSource mv = do
  v <- liftIO (tryTakeMVar mv)
  maybe (return ()) (\vv -> yield vv >> feedbackSource mv) v

-- Parsing programs from input files

parser :: AP.Parser [ MWord ]
parser = commaSeparated (AP.signed AP.decimal)