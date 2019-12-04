module Day04 where

input :: [ Int ]
input = [246540 .. 787419]

digits :: Int -> [ Int ]
digits = reverse . digits'
  where
    digits' 0 = [] 
    digits' xs = xs `mod` 10 : digits' (xs `div` 10)

zip4 :: [ a ] -> [ b ] -> [ c ] -> [ d ] -> [ (a, b, c, d) ]
zip4 (a:as) (b:bs) (c:cs) (d:ds) = (a, b, c, d) : zip4 as bs cs ds
zip4 _ _ _ _ = []

pairs :: [ Int ] -> [ (Int, Int) ]
pairs xs = zip xs (tail xs)

paddedQuads :: [ Int ] -> [ (Int, Int, Int, Int) ]
paddedQuads xs = zip4 pxs (tail pxs) (tail . tail $ pxs) (tail . tail . tail $ pxs)
    where pxs = 0 : xs ++ [ 99 ]

increasing :: [ (Int, Int) ] -> Bool
increasing = foldl (\v (x, y) -> v && x <= y) True

paired :: [ (Int, Int) ] -> Bool
paired = foldl (\v (x, y) -> v || x == y) False

onlyPaired :: [ (Int, Int, Int, Int) ] -> Bool
onlyPaired = foldl (\v (a, b, c, d) -> v || (b == c && a /= c && d /= c)) False

ex1 :: Int
ex1 = length ((filter increasing . filter paired) (pairs <$> digits <$> input))

ex2 :: Int
ex2 = length ((filterOnlyPaired . filterIncreasing) (digits <$> input))
    where 
      filterIncreasing = filter (increasing . pairs)
      filterOnlyPaired = filter (onlyPaired . paddedQuads)
