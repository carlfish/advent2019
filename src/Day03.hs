module Day03 where

import qualified Data.Attoparsec.ByteString.Char8 as AP

import Data.List (sort, elemIndex, reverse)
import Lib (runFile')
import Data.Set (intersection, fromList, Set)
import Data.Maybe (fromMaybe)

data GridRef = GridRef { x :: Int, y :: Int } 
  deriving (Eq, Show, Ord)
data Direction = R | L | U | D
  deriving (Eq, Show)
data Offset = Offset Direction Int
  deriving (Eq, Show)

origin :: GridRef
origin = GridRef 0 0

taxiDistance :: GridRef -> GridRef -> Int
taxiDistance (GridRef x1 y1) (GridRef x2 y2) = abs (x1 - x2) + abs (y1 - y2)

traverseDistance :: [ GridRef ] -> [ GridRef ] -> GridRef -> Int
traverseDistance w1 w2 g = fromMaybe maxBound pathSum
  where 
    pathSum = do
      w1idx <- elemIndex g w1
      w2idx <- elemIndex g w2
      return (w1idx + w2idx)

move :: GridRef -> Direction -> GridRef
move (GridRef x y) U = GridRef x (y + 1)
move (GridRef x y) D = GridRef x (y - 1)
move (GridRef x y) L = GridRef (x - 1) y
move (GridRef x y) R = GridRef (x + 1) y

unrollWire :: [ Offset ] -> [ GridRef ]
unrollWire os =
  let
    toDirections (Offset d i) = replicate i d
  in
    scanl move origin (concatMap toDirections os)

closestBy :: Foldable f => (GridRef -> Int) -> f GridRef -> GridRef
closestBy distanceTo = 
  let 
    closer x y = 
      if      (x == origin)                 then y
      else if (y == origin)                 then x
      else if (distanceTo x < distanceTo y) then x
      else                                       y
  in 
    foldl closer origin

setIntersect :: Ord a => [ a ] -> [ a ] -> Set a
setIntersect x y = intersection (fromList x) (fromList y)

distanceToClosestIntersection :: (GridRef -> Int) -> [ GridRef ] -> [ GridRef ] -> Int
distanceToClosestIntersection distanceTo g1 g2 = distanceTo (closestBy distanceTo (setIntersect g1 g2))

-- Run the exercises

runEx :: ( [ GridRef ] -> [ GridRef ] -> Int ) -> IO (Either String Int)
runEx f = runFile' "data/day03/0301.txt" parser (\(gs1, gs2) -> f (unrollWire gs1) (unrollWire gs2))

ex1 :: IO (Either String Int)
ex1 = runEx (distanceToClosestIntersection (taxiDistance origin))

ex2 :: IO (Either String Int)
ex2 = runEx (\g1 g2 -> distanceToClosestIntersection (traverseDistance g1 g2) g1 g2)

-- Input Parser

parser :: AP.Parser ([ Offset ], [ Offset ])
parser = (,) <$> (offsets <* AP.endOfLine) <*> (offsets <* AP.endOfInput)

offsets :: AP.Parser [ Offset ]
offsets = AP.sepBy offset (AP.char ',')

offset :: AP.Parser Offset
offset = Offset <$> direction <*> AP.decimal

direction :: AP.Parser Direction
direction = 
  AP.choice [ u, d, l, r ]
  where
    u = (const U) <$> AP.char 'U'
    d = (const D) <$> AP.char 'D'
    l = (const L) <$> AP.char 'L'
    r = (const R) <$> AP.char 'R'