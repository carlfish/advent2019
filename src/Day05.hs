-- Most of the functionality was moved into the IntCode module
-- to share with future days' problems. To see the original
-- solution you'll have to look back in the file history.

module Day05 where

import qualified Data.Attoparsec.ByteString.Char8 as AP

import Lib (runFile)
import IntCode (MWord, parser, runComputerPure, smallComputer)

ex1 :: IO (Either String [ MWord ])
ex1 = runFile "data/day05/0501.txt" parser (runComputerPure [ 1 ] . smallComputer)

ex2 :: IO (Either String [ MWord ])
ex2 = runFile "data/day05/0501.txt" parser (runComputerPure [ 5 ] . smallComputer)