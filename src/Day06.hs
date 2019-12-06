module Day06 where

import qualified Data.Attoparsec.ByteString.Char8 as AP

import Lib (runFile')
import Prelude hiding (lookup)
import Data.Map (Map(..), lookup, keys, fromList)
import Data.Maybe (maybe)

newtype Obj = Obj String
  deriving (Eq, Ord, Show)

type Orbits = Map Obj Obj

allOrbits :: Orbits -> Int
allOrbits os = foldl ((+)) 0 (length <$> (flip orbitsFrom) os <$> keys os)

orbitsFrom :: Obj -> Orbits -> [ Obj ]
orbitsFrom o os = maybe [] (\oo -> oo : orbitsFrom oo os) (lookup o os)

orbits :: [ (Obj, Obj) ] -> Orbits
orbits = fromList

-- Since orbits are guaranteed to form a tree, the path between orbits can be 
-- calculated by finding the last common element of the two branches, and joining
-- them together at that point.
pathBetween :: Obj -> Obj -> Orbits -> [ Obj ]
pathBetween from to os = 
  let
    fromPath = reverse (orbitsFrom from os)
    toPath = reverse (orbitsFrom to os)
    -- There's almost certainly a cleverer way to do this.
    pathBetween' _ [] _ = []
    pathBetween' [] _ _ = []
    pathBetween' ff@(f : fs) tt@(t : ts) lst = 
      if (f == t) 
        then pathBetween' fs ts [ f ] 
        else (reverse ff) ++ lst ++ tt
  in
    pathBetween' fromPath toPath []

-- The distance is the number of hops between orbits in a path, i.e. one less than the
-- number of orbits.
distanceBetween :: Obj -> Obj -> Orbits -> Int
distanceBetween from to os = length (pathBetween from to os) - 1

-- Run the exercises

runEx :: String -> (Orbits -> a) -> IO (Either String a) 
runEx file f = runFile' file parser (f . orbits)

ex1 :: IO (Either String Int)
ex1 = runEx "data/day06/0601.txt" allOrbits

ex2 :: IO (Either String Int)
ex2 = runEx "data/day06/0601.txt" (distanceBetween (Obj "YOU") (Obj "SAN"))

tst1 :: IO (Either String Int)
tst1 = runEx "data/day06/06tst1.txt" allOrbits

tst2 :: IO (Either String Int)
tst2 = runEx "data/day06/06tst2.txt" (distanceBetween (Obj "YOU") (Obj "SAN"))

-- Input Parsers

pObj :: AP.Parser Obj
pObj = Obj <$> (AP.many1' (AP.choice [ AP.digit, AP.letter_ascii ]))

pOrbit :: AP.Parser (Obj, Obj)
pOrbit = (flip (,)) <$> pObj <*> (AP.char ')' *> pObj)

parser :: AP.Parser [ (Obj, Obj) ]
parser = AP.many' (pOrbit <* AP.choice [AP.endOfLine, AP.endOfInput])