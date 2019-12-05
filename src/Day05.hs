{-# LANGUAGE FlexibleContexts #-}

module Day05 where

import qualified Data.Attoparsec.ByteString.Char8 as AP

import Lib (runFile, runFile')
import Data.List (find)
import Data.Array
import Data.Sequence hiding (length)
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer hiding (Sum)
import Control.Monad.RWS hiding (Sum)

type Heap = Array Addr Int 
type RuntimeState = (ProgramState, Heap, Addr)
type Program = RWST Int (Seq Int, Seq String) RuntimeState (Either String)
type ParamReader = Int -> Param

newtype Addr = Addr Int deriving (Show, Eq, Ord, Ix)

debug :: Bool
debug = False

data Op
  = Sum Param Param Addr
  | Mul Param Param Addr
  | ReadIn Addr
  | WriteOut Param
  | JumpIfTrue Param Param
  | JumpIfFalse Param Param
  | LessThan Param Param Addr
  | Equals Param Param Addr
  | Halt
  deriving (Show)

data ProgramState 
  = Reading
  | Running Op
  | Stopped
  deriving (Show)

data Param 
  = Immediate Int 
  | Ref Addr
  deriving (Show)

a0 :: Addr
a0 = Addr 0
  
incAddr :: Addr -> Addr
incAddr (Addr c) = Addr (c + 1)

noOp :: Program ()
noOp = return ()

output :: Int -> Program ()
output i = tell (singleton i, empty)

trace :: Show a => a -> Program ()
trace a = if debug then tell (empty, singleton (show a)) else noOp

setProgramCounter :: Addr -> Program ()
setProgramCounter a = modify (\(ps, h, _) -> (ps, h , a))

setProgramCounterFrom :: Param -> Program()
setProgramCounterFrom p = setProgramCounter =<< (Addr <$> readParam p)

setPS :: ProgramState -> Program ()
setPS ps = modify (\(_, h, c) -> (ps, h, c))

writeHeap :: Addr -> Int -> Program ()
writeHeap a v = modify (\(ps, h, c) -> (ps, h // [(a, v)], c))

readHeap :: Addr -> Program Int
readHeap a = gets (\(ps, h, c) -> h ! a)

readParam :: Param -> Program Int
readParam (Immediate i) = return i
readParam (Ref a)       = readHeap a

readNext :: Program Int
readNext = state (\(ps, h, c) -> (h ! c, (ps, h, incAddr c)))

binaryOp :: (Int -> Int -> Int) -> Param -> Param -> Addr -> Program ()
binaryOp f ia ib io = writeHeap io =<< (f <$> readParam ia <*> readParam ib)

branch :: a -> a -> Bool -> a
branch ifTrue ifFalse condition = if condition then ifTrue else ifFalse

test :: (Int -> Bool) -> Param -> Program Bool
test f p = f <$> readParam p

isLt, isEq :: Param -> Param -> Program Bool
isLt p1 p2 = (\a b -> a < b) <$> readParam p1 <*> readParam p2
isEq p1 p2 = (\a b -> a == b) <$> readParam p1 <*> readParam p2

run :: Op -> Program ()
run op = trace op >> run' >> setPS (nextState op)
  where 
    nextState Halt = Stopped
    nextState _    = Reading

    run' = case op of 
      Sum ia ib io     -> binaryOp (+) ia ib io
      Mul ia ib io     -> binaryOp (*) ia ib io                      
      ReadIn a         -> writeHeap a =<< ask
      WriteOut a       -> output =<< readParam a                       
      JumpIfTrue p a   -> branch (setProgramCounterFrom a) noOp  =<< test (/= 0) p
      JumpIfFalse p a  -> branch (setProgramCounterFrom a) noOp  =<< test (== 0) p
      LessThan p1 p2 a -> branch (writeHeap a 1) (writeHeap a 0) =<< isLt p1 p2
      Equals p1 p2 a   -> branch (writeHeap a 1) (writeHeap a 0) =<< isEq p1 p2
      Halt             -> noOp

parseOpcode :: MonadError String m => Int -> m (ParamReader, ParamReader, ParamReader, Int)
parseOpcode opcode = 
  let
    parseParam 0 = return (Ref . Addr)
    parseParam 1 = return Immediate
    parseParam i = throwError ("Bad param designator: " <> (show i))
    digitAt d n = opcode `div` ((10 ^ d)) `mod` (10 ^ n)
  in 
    (,,,) <$> (parseParam (digitAt 2 1)) 
          <*> (parseParam (digitAt 3 1)) 
          <*> (parseParam (digitAt 4 1))
          <*> (return (digitAt 0 2))

interpret :: Int -> Program ()
interpret opcode =
  let
    readNext3 f  = f <$> readNext <*> readNext <*> readNext
    readNext2 f  = f <$> readNext <*> readNext
    intToParam3 p1 p2 f = \x y z -> f (p1 x) (p2 y) (Addr z)
    intToParam2 p1 p2 f = \x y -> f (p1 x) (p2 y)
    running op   = setPS (Running op)
  in running =<< do
    (p1, p2, _, code) <- parseOpcode opcode
    case code of
      1  -> readNext3 (intToParam3 p1 p2 Sum)
      2  -> readNext3 (intToParam3 p1 p2 Mul)
      3  -> ReadIn <$> Addr <$> readNext
      4  -> WriteOut <$> p1 <$> readNext
      5  -> readNext2 (intToParam2 p1 p2 JumpIfTrue)
      6  -> readNext2 (intToParam2 p1 p2 JumpIfFalse)
      7  -> readNext3 (intToParam3 p1 p2 LessThan)
      8  -> readNext3 (intToParam3 p1 p2 Equals)
      99 -> return Halt
      i  -> throwError ("Unknown opcode: " <> (show i))

runInterpreter :: Program Int
runInterpreter = do
  (ps, _, _) <- get
  case ps of
    Reading    -> readNext >>= interpret >> runInterpreter
    Running op -> run op >> runInterpreter
    Stopped    -> readHeap a0

runList :: Int -> [ Int ] -> Either String (Seq Int, Seq String)
runList input startingHeap = 
  let initState = (Reading, listArray (Addr 0, Addr (length startingHeap)) startingHeap, a0)
  in  snd <$> evalRWST runInterpreter input initState

ex1 :: IO (Either String (Seq Int, Seq String))
ex1 = runFile "data/day05/0501.txt" parser (runList 1)

ex2 :: IO (Either String (Seq Int, Seq String))
ex2 = runFile "data/day05/0501.txt" parser (runList 5)

parser :: AP.Parser [ Int ]
parser = AP.sepBy (AP.signed AP.decimal) (AP.char ',')