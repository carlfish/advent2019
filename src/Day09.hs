-- All of the interesting coding for this day's task was done in the
-- IntCode module, specifically adding the 09 opcode and offset addressing.

module Day09 where

import qualified Data.Attoparsec.ByteString.Char8 as AP

import Lib (runFile, commaSeparated)
import IntCode (MWord, parser, runComputerPure, bigComputer)

ex1 :: IO (Either String [ MWord ])
ex1 = runFile "data/day09/0901.txt" parser (runComputerPure [ 1 ] . bigComputer)

ex2 :: IO (Either String [ MWord ])
ex2 = runFile "data/day09/0901.txt" parser (runComputerPure [ 2 ] . bigComputer)
