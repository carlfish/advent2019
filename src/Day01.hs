module Day01 where

import qualified Data.Attoparsec.ByteString.Char8 as AP

import Lib (runFile', onePerLine)

parser :: AP.Parser [ Integer ]
parser = onePerLine AP.decimal

naiveFuelRequirement :: Integer -> Integer
naiveFuelRequirement n 
  | n <= 6     = 0
  | otherwise  = (n `div` 3) - 2

-- You could also do `sum . (takeWhile (>0)) . tail . (iterate naiveFuelRequirement)` here,
-- but IMO that would be trading more work for less clarity.
completeFuelRequirement :: Integer -> Integer
completeFuelRequirement 0 = 0
completeFuelRequirement m =
  let fuel = naiveFuelRequirement m
  in  fuel + completeFuelRequirement fuel

calculate :: (Integer -> Integer) -> [ Integer ] -> Integer
calculate calculator input = sum (calculator <$> input)

runExercise :: ([ Integer ] -> Integer) -> IO (Either String Integer)
runExercise = runFile' "data/day01/0101.txt" parser

ex1 :: IO (Either String Integer)
ex1 = runExercise (calculate naiveFuelRequirement)

ex2 :: IO (Either String Integer)
ex2 = runExercise (calculate completeFuelRequirement)
