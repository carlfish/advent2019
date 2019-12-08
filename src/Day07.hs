{-# LANGUAGE FlexibleContexts #-}

module Day07 where

import Day05 (Computer, computer, parser)

import Lib (runFile)
import Conduit
import qualified Data.Conduit.Combinators as CC
import Data.Either (either)

prepend :: Monad m => Int -> ConduitT Int Int m ()
prepend i = yield i >> CC.map id

runComputer :: [ Int ] -> Computer () -> Either String [ Int ]
runComputer input c = runConduit
  (  yieldMany input
  .| c
  .| sinkList
  )

amplifierChain :: [ Int ] -> (Int, Int, Int, Int, Int) -> Computer ()
amplifierChain heap (pa, pb, pc, pd, pe)
  =  prepend pa
  .| computer heap
  .| prepend pb
  .| computer heap
  .| prepend pc
  .| computer heap
  .| prepend pd
  .| computer heap
  .| prepend pe
  .| computer heap

phases :: [ Int ]
phases = [0 .. 4]

-- I'm sure there's a clever combo of library functions that does this. Probably
-- involving 'tails'
possibilities :: [ (Int, Int, Int, Int, Int) ]
possibilities = do
  a <- phases
  let bs = filter (/=a) phases
  b <- bs
  let cs = filter (/=b) bs
  c <- cs
  let ds = filter (/=c) cs
  d <- ds
  let es = filter (/=d) ds
  e <- es
  return (a, b, c, d, e)

highestThrust :: [ Int ] -> Either String ((Int, Int, Int, Int, Int), Int)
highestThrust heap = (\r -> foldl (\(ap, a) (bp, b:bs) -> if (a > b) then (ap, a) else (bp, b)) ((0, 0, 0, 0, 0), 0) r) <$> runComputers
  where runComputers = (zip possibilities) <$> (sequence (runComputer [0] <$> amplifierChain heap <$> possibilities))

thrustFor :: (Int, Int, Int, Int, Int) -> [ Int ] -> Either String [ Int ]
thrustFor ps heap = runComputer [0] (amplifierChain heap ps)

ex1 :: IO (Either String ((Int, Int, Int, Int, Int), Int))
ex1 = runFile "data/day07/0701.txt" parser highestThrust
  
tst1 :: IO (Either String ((Int, Int, Int, Int, Int), Int))
tst1 = runFile "data/day07/07tst1.txt" parser highestThrust
