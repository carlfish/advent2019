module Day03 where

import qualified Data.Attoparsec.ByteString.Char8 as AP

import Data.List (sort, elemIndex, reverse)
import Lib (runFile')
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

move1 :: Direction -> GridRef -> GridRef
move1 U (GridRef x y) = GridRef x (y + 1)
move1 D (GridRef x y) = GridRef x (y - 1)
move1 L (GridRef x y) = GridRef (x - 1) y
move1 R (GridRef x y) = GridRef (x + 1) y

unrollWire :: [ Offset ] -> [ GridRef ]
unrollWire =
  let
    nextMove gs (Offset _ 0)        = gs
    nextMove (g : gs) (Offset d i)  =
      let nextg = move1 d g 
      in nextMove (nextg : g : gs) (Offset d (i - 1)) 
  in
    reverse . (foldl nextMove [ origin ])

intersections :: [ Offset ] -> [ Offset ] -> [ GridRef ]
intersections a b = fastIntersect (unrollWire a) (unrollWire b)

closest :: (GridRef -> Int) -> [ GridRef ] -> GridRef
closest f = 
  let 
    closer (GridRef 0 0) g = g
    closer g (GridRef 0 0) = g
    closer g1 g2 = if (f g1 < f g2) then g1 else g2
  in 
    foldl closer origin

-- n(logn) intersection
fastIntersect :: Ord a => [ a ] -> [ a ] -> [ a ]
fastIntersect x y = 
  let 
    sx = sort x
    sy = sort y
    is ss as [] = ss
    is ss [] bs = ss
    is ss (a : as) (b : bs) =
      if      (a == b) then is (a : ss) as bs
      else if (a < b)  then is ss as (b : bs)
      else                  is ss (a : as) bs
  in
    is [] sx sy

-- Run the exercises

runEx :: ( [ GridRef ] -> [ GridRef ] -> Int ) -> IO (Either String Int)
runEx f = runFile' "data/day03/0301.txt" parser (\(gs1, gs2) -> f (unrollWire gs1) (unrollWire gs2))

ex1 :: IO (Either String Int)
ex1 = runEx (\gs1 gs2 -> (taxiDistance origin (closest (taxiDistance origin) (fastIntersect gs1 gs2))))

ex2 :: IO (Either String Int)
ex2 = runEx (\gs1 gs2 -> (traverseDistance gs1 gs2 (closest (traverseDistance gs1 gs2) (fastIntersect gs1 gs2))))

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