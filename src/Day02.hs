module Day02 where

import qualified Data.Attoparsec.ByteString.Char8 as AP

import Lib (runFile, runFile', commaSeparated)
import Data.List (find)
import Data.Array
import Control.Monad.State

type Heap = Array Addr Int 
type RuntimeState = (ProgramState, Heap, Addr)
type Program = State RuntimeState

newtype Addr = Addr Int deriving (Eq, Ord, Ix)

data Op
  = Sum Addr Addr Addr
  | Mul Addr Addr Addr
  | Halt

data ProgramState 
  = Reading
  | Running Op
  | Stopped 
  | Failed String

a0 :: Addr
a0 = Addr 0
  
incAddr :: Addr -> Addr
incAddr (Addr c) = Addr (c + 1)

run :: Op -> Program ()
run (Sum ia ib io) = binaryOp (+) ia ib io >> setPS Reading
run (Mul ia ib io) = binaryOp (*) ia ib io >> setPS Reading
run           Halt =                          setPS Stopped

binaryOp :: (Int -> Int -> Int) -> Addr -> Addr -> Addr -> Program ()
binaryOp f ia ib io = writeHeap io =<< (f <$> readHeap ia <*> readHeap ib)

writeHeap :: Addr -> Int -> Program ()
writeHeap a v = modify (\(ps, h, c) -> (ps, h // [(a, v)], c))

readHeap :: Addr -> Program Int
readHeap a = gets (\(ps, h, c) -> h ! a)

interpret :: Int -> Program ()
interpret opcode =
  let 
    readNext3 f  = f <$> readNext <*> readNext <*> readNext
    intToAddr3 f = \x y z -> f (Addr x) (Addr y) (Addr z)
    running op   = setPS (Running op)
  in case opcode of
    1  -> running =<< readNext3 (intToAddr3 Sum)
    2  -> running =<< readNext3 (intToAddr3 Mul)
    99 -> running Halt
    i  -> setPS (Failed ("Unknown opcode: " <> (show i)))

readNext :: Program Int
readNext = state (\(ps, h, c) -> (h ! c, (ps, h, incAddr c)))

setPS :: ProgramState -> Program ()
setPS ps = modify (\(_, h, c) -> (ps, h, c))

runInterpreter :: Program (Either String Int)
runInterpreter = do
  (ps, _, _) <- get
  case ps of
    Reading    -> readNext >>= interpret >> runInterpreter
    Running op -> run op >> runInterpreter
    Stopped    -> Right <$> readHeap a0
    Failed e   -> return (Left e)
  
runList :: Program() -> [ Int ] -> Either String Int
runList prelude is = 
  let 
    program   = prelude >> runInterpreter 
    initState = (Reading, listArray (Addr 0, Addr (length is)) is, a0)
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
parser = commaSeparated AP.decimal