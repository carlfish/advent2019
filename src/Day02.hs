module Day02 where

import Data.Attoparsec.ByteString.Char8 as AP
import Lib (runFile, runFile')
import Data.List (find)
import Data.Array
import Control.Monad.State

type Heap = Array Int Int 
type RuntimeState = (ProgramState, Heap, Addr)
type Program = State RuntimeState

newtype Addr = Addr Int

data Op
  = Sum Addr Addr Addr
  | Mul Addr Addr Addr
  | Halt

data ProgramState 
  = Reading
  | Running Op
  | Stopped 
  | Failed String

run :: Op -> Program ()
run (Sum ia ib io) = binaryOp (+) ia ib io >> setPS Reading
run (Mul ia ib io) = binaryOp (*) ia ib io >> setPS Reading
run           Halt =                          setPS Stopped

binaryOp :: (Int -> Int -> Int) -> Addr -> Addr -> Addr -> Program ()
binaryOp f ia ib io = writeHeap io =<< (f <$> readHeap ia <*> readHeap ib)

writeHeap :: Addr -> Int -> Program ()
writeHeap (Addr i) v = modify (\(ps, h, c) -> (ps, h // [(i, v)], c))

readHeap :: Addr -> Program Int
readHeap (Addr i) = gets (\(ps, h, c) -> h ! i)

interpret :: Int -> Program ()
interpret 1  = running =<< next3Addr Sum
interpret 2  = running =<< next3Addr Mul
interpret 99 = running Halt
interpret i  = setPS (Failed ("Unknown opcode: " <> (show i)))

readNext :: Program Int
readNext = state (\(ps, h, (Addr c)) -> (h ! c, (ps, h, Addr (c + 1))))

readNextAddr :: Program Addr
readNextAddr = Addr <$> readNext

next3Addr :: (Addr -> Addr -> Addr -> a)  -> Program a
next3Addr f = f <$> readNextAddr <*> readNextAddr <*> readNextAddr

setPS :: ProgramState -> Program ()
setPS ps = modify (\(_, h, c) -> (ps, h, c))

running :: Op -> Program ()
running op = setPS (Running op)

runInterpreter :: Program (Either String Int)
runInterpreter = do
  (ps, _, _) <- get
  case ps of
    Reading    -> readNext >>= interpret >> runInterpreter
    Running op -> run op >> runInterpreter
    Stopped    -> gets (\(_, h, _) -> Right (h ! 0))
    Failed e   -> return (Left e)
  
runList :: Program() -> [ Int ] -> Either String Int
runList prelude is = 
  let 
    program = prelude >> runInterpreter 
    initState = (Reading, listArray (0, length is) is, Addr 0)
  in evalState program initState

fixInput :: Int -> Int -> Program ()
fixInput a b = writeHeap (Addr 1) a >> writeHeap (Addr 2) b

runPossibilities :: Int -> [ Int ] -> Maybe (Int, Int)
runPossibilities target is =
  let
    range = [0 .. (length is - 1)]
    pairs = [(x, y) | x <- range, y <- range]
  in
    find (\(a, b) -> runList (fixInput a b) is == Right target) pairs

ex1 :: IO (Either String Int)
ex1 = runFile "data/day02/0201.txt" parser (runList (fixInput 12 2))

ex2 :: IO (Either String (Maybe (Int, Int)))
ex2 = runFile' "data/day02/0201.txt" parser (runPossibilities 19690720)

parser :: AP.Parser [ Int ]
parser = AP.sepBy AP.decimal (AP.char ',')