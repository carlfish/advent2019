module Day08 where

import Lib (runFile', runFileIO, minimumWith)
import qualified Data.Attoparsec.ByteString.Char8 as AP
import Data.Char (digitToInt) 
import Data.Foldable (minimumBy, fold)

type Image = [ Layer ]
data Layer = Layer { w :: Int, h :: Int, imagemap :: [ Colour  ] }

data Colour = Black | White | Transparent deriving (Eq, Ord)

instance Semigroup Colour where
  Black <> _       = Black
  White <> _       = White
  Transparent <> c = c

instance Show Colour where
  show Black = "  "
  show White = "**"
  show Transparent = "__"

instance Show Layer where
  show (Layer w h d) = byLine d
    where
      byLine [] = [] 
      byLine cs = (fold (show <$> (take w cs))) <> "\n" <> (byLine (drop w cs))

colour :: Int -> Colour
colour 0 = Black
colour 1 = White
colour 2 = Transparent
colour _ = undefined

combine :: Layer -> Layer -> Layer
combine (Layer w h top) (Layer _ _ bottom) = 
  Layer w h ((uncurry (<>)) <$> (zip top bottom))

transparent :: Int -> Int -> Layer
transparent w h = Layer w h (take (w * h) (repeat Transparent))

combineLayers :: Int -> Int -> Image -> Layer
combineLayers w h = foldl combine (transparent w h)

parseLayer :: Int -> Int -> AP.Parser Layer
parseLayer w h = (Layer w h) <$> AP.count (w * h) (colour <$> digitToInt <$> AP.digit)

parser :: Int -> Int -> AP.Parser Image
parser w h = (AP.many' (parseLayer w h)) <* AP.choice [AP.endOfLine, AP.endOfInput]

countColours :: Colour -> Layer -> Int
countColours c (Layer _ _ d) = (length . filter (== c)) d

layerWithFewest :: Colour -> Image -> Layer
layerWithFewest c = minimumWith (countColours c)

ex1' :: Image -> Int
ex1' im = (countColours White l) * (countColours Transparent l)
  where l = layerWithFewest Black im

ex1 :: IO (Either String Int)
ex1 = runFile' "data/day08/0801.txt" (parser 25 6) ex1'

ex2 :: IO (Either String ())
ex2 = runFileIO "data/day08/0801.txt" (parser 25 6) (sequence . Right . putStr . show . (combineLayers 25 6))
