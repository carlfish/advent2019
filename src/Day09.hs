-- All of the interesting coding for this day's task was done in the
-- IntCode module, specifically adding the 09 opcode, offset addressing,
-- and moving to 64 bit words and larger memory allocation.

module Day09 where

import Lib (runFile, commaSeparated)
import IntCode (MWord, parser, runComputerPure, bigComputer)

ex1 :: IO (Either String [ MWord ])
ex1 = runFile "data/day09/0901.txt" parser (runComputerPure [ 1 ] . bigComputer)

ex2 :: IO (Either String [ MWord ])
ex2 = runFile "data/day09/0901.txt" parser (runComputerPure [ 2 ] . bigComputer)
