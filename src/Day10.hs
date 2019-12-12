module Day10 where

import qualified Data.Attoparsec.ByteString.Char8 as AP
import Lib (runFile', onePerLine, maximumWith)
import Data.Set (Set, fromList, size)
import Data.Ord (Ordering(..), compare)
import Data.List (sortBy, groupBy, (!!), delete)

data Object = Empty | Asteroid deriving (Eq, Ord, Show)
data GridRef = GridRef { x :: Int, y :: Int, o :: Object } deriving (Eq, Ord, Show)
type Grid = [ GridRef ]

toGrid :: [ [ Object ] ] -> Grid
toGrid oos = do
  (y, line) <- zip [0..] oos
  (x, o) <- zip [0..] line
  return GridRef { x = x, y = y, o = o }

asteroids :: Grid -> Grid
asteroids = filter (\g -> (o g) == Asteroid)

-- Most of the work is done by re-centering the grid around the (potential) new base, 
-- and using polar coordinates from that origin to determine what we can shoot.
recenter :: Int -> Int -> Grid -> Grid
recenter x y = map (\(GridRef xx yy o) -> GridRef (xx - x) (yy - y) o)

unrecenter :: Int -> Int -> GridRef -> GridRef
unrecenter x y (GridRef xx yy o) = GridRef (xx + x) (yy + y) o

-- Find the GridRef closest to the origin that is exactly in line with this GridRef. 
-- Like a lot of things in this day's example, I'm doing a lot of (possibly unnecessary)
-- work to avoid floating-point comparisons.
pullToOrigin :: GridRef -> GridRef
pullToOrigin (GridRef x y o) = 
  let 
    g = gcd x y 
  in
    if g == 0 then
      GridRef x y o
    else 
      GridRef (x `div` g) (y `div` g) o

shrink :: Grid -> Grid
shrink = map pullToOrigin

unique :: Grid -> Set GridRef
unique = fromList

countVisibles :: Grid -> [ (GridRef, Int) ]
countVisibles as = zip as $ (\(GridRef x y o) -> ((subtract 1) . size . unique . shrink . (recenter x y)) as) <$> as

findBestAsteroid :: Grid -> (GridRef, Int)
findBestAsteroid = (maximumWith (\(_, i) -> i)) . countVisibles

-- I literally haven't done polar coordinate maths since 1992, so I'm sure I'm missing
-- a straightforward shortcut here. Once again, a lot of work to avoid dropping into
-- floating point calculation too soon.
sweepOrder :: GridRef -> GridRef -> Ordering
sweepOrder g1 g2 = if sameAngle then compareDistance g1 g2 else compareAngles
  where 
    sameAngle = (pullToOrigin g1) == (pullToOrigin g2)
    compareDistance (GridRef x1 y1 _) (GridRef x2 y2 _) = compare (x1^2 + y1^2) (x2^2 + y2^2)
    compareAngles = compare (radiansFromVertical g1) (radiansFromVertical g2)

radiansFromVertical :: GridRef -> Double
radiansFromVertical (GridRef x y _) = 
  if (rawAngle) <= 0 then (negate rawAngle) else ((2 * pi) - rawAngle)
  where
    rawAngle = atan2 (fromIntegral (-x)) (fromIntegral (-y))

groupBySweepOrder :: Grid -> [ [ GridRef ] ]
groupBySweepOrder = (groupBy (\a b -> (pullToOrigin a) == (pullToOrigin b))) . (sortBy sweepOrder)

annihilationOrder :: Grid -> Grid
annihilationOrder = unfold . groupBySweepOrder
  where 
    unfold ([]) = []
    unfold ([] : gs) = unfold gs
    unfold ((a : as) : gs) = a : (unfold (gs ++ [ as ]))

-- exercises

ex1 :: IO (Either String (GridRef, Int))
ex1 = runFile' "data/day10/1001.txt" parser (findBestAsteroid . asteroids)

runEx2 :: Int -> Grid -> GridRef
runEx2 i g = 
  let
    best = fst $ findBestAsteroid g    
  in
    unrecenter (x best) (y best) $ (annihilationOrder . (recenter (x best) (y best)) . delete best) g !! (i - 1)    

ex2 :: IO (Either String GridRef)
ex2 = runFile' "data/day10/1001.txt" parser (runEx2 200 . asteroids)

tst2 :: Int ->  IO (Either String GridRef)
tst2 i = runFile' "data/day10/10tst2.txt" parser (runEx2 i . asteroids)

-- parsers

parseObj :: AP.Parser Object
parseObj = AP.choice [ const Empty <$> AP.char '.', const Asteroid <$> AP.char '#' ]

parseLine :: AP.Parser [ Object ]
parseLine = AP.many1 parseObj

parser :: AP.Parser Grid
parser = toGrid <$> (onePerLine parseLine)