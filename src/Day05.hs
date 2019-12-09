{-# LANGUAGE FlexibleContexts #-}

module Day05 where

import qualified Data.Attoparsec.ByteString.Char8 as AP

import Lib (runFile, commaSeparated)
import Data.List (find)
import Data.Array
import Data.Maybe (maybe)
import Control.Monad.Except
import Control.Monad.State
import Conduit
import IntCode (MWord, parser, runComputerPure, computer)

ex1 :: IO (Either String [ MWord ])
ex1 = runFile "data/day05/0501.txt" parser (runComputerPure [ 1 ] . computer)

ex2 :: IO (Either String [ MWord ])
ex2 = runFile "data/day05/0501.txt" parser (runComputerPure [ 5 ] . computer)