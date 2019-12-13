{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Day12 where

import Data.List ((!!))
import Data.Set (empty, insert, member)

newtype Velocity = Velocity (Int, Int, Int) deriving (Eq, Show)
newtype Position = Position (Int, Int, Int) deriving (Eq, Show)
data Body = Body { position :: Position, velocity :: Velocity } deriving (Eq, Show)

type System = [ Body ]

instance Semigroup Velocity where
  Velocity v1 <> Velocity v2 = Velocity (addCoords v1 v2)

instance Monoid Velocity where
  mempty = Velocity (0, 0, 0)

atRest :: Int -> Int -> Int -> Body
atRest x y z = Body (Position (x, y, z)) mempty

energy :: Body -> Int
energy (Body (Position p) (Velocity v)) = e p * e v
  where e (x, y, z) = abs x + abs y + abs z

move :: Velocity -> Position -> Position
move (Velocity v) (Position p) = Position (addCoords v p)

addCoords :: Num a => Num b => Num c => (a, b, c) -> (a, b, c) -> (a, b, c)
addCoords (a1, b1, c1)  (a2, b2, c2) = (a1 + a2, b1 + b2, c1 + c2)

gravity :: Position -> Position -> Velocity
gravity (Position (px1, py1, pz1)) (Position (px2, py2, pz2)) =
  let
    g a b = if a > b then -1 else if a < b then 1 else 0
  in
    Velocity ((g px1 px2), (g py1 py2), (g pz1 pz2))

applyGravity :: Body -> Position -> Body
applyGravity (Body p v) p2 = Body p (v <> gravity p p2)

applyGravities :: [ Body ] -> Body -> Body
applyGravities bs b = foldl (\b (Body p _) -> applyGravity b p) b bs

applyVelocity :: Body -> Body
applyVelocity (Body p v) = Body (move v p) v

tick :: System -> System
tick s = applyVelocity <$> applyGravities s <$> s

ticks :: System -> [ System ]
ticks = iterate tick

systemEnergy :: System -> Int
systemEnergy = sum . (fmap energy)

energyAfter :: System -> Int -> Int
energyAfter system steps = systemEnergy ((ticks system) !! steps)

xs, ys, zs :: Body -> (Int, Int)
xs (Body (Position (px, _, _)) (Velocity (vx, _, _))) = (px, vx)
ys (Body (Position (_, py, _)) (Velocity (_, vy, _))) = (py, vy)
zs (Body (Position (_, _, pz)) (Velocity (_, _, vz))) = (pz, vz)

dimensions :: (Body -> (Int, Int)) -> System -> [ (Int, Int) ]
dimensions d = fmap d

firstRepeatOnDimension :: (Body -> (Int, Int)) -> System -> Integer
firstRepeatOnDimension d s = firstRepeat' 0 empty (dimensions d <$> ticks s)
  where firstRepeat' i s (v : vs) = if (member v s) then i else firstRepeat' (i + 1) (insert v s) vs

firstCycle :: System -> Integer
firstCycle s = lcm firstXRepeat (lcm firstYRepeat firstZRepeat)
    where 
      firstXRepeat = firstRepeatOnDimension xs s
      firstYRepeat = firstRepeatOnDimension ys s
      firstZRepeat = firstRepeatOnDimension zs s

-- Exercises

ex1 :: Int
ex1 = energyAfter puzzleState 1000

ex2 :: Integer
ex2 = firstCycle puzzleState

-- Puzzle/test data

testState :: System
testState =
  [ atRest (-1)    0     2
  , atRest   2  (-10)  (-7)
  , atRest   4   (-8)    8
  , atRest   3     5   (-1)
  ]

puzzleState :: System
puzzleState =
  [ atRest  15  (-2)  (-6)
  , atRest (-5) (-4) (-11)
  , atRest   0  (-6)    0
  , atRest   5    9     6
  ]