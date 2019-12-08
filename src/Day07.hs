{-# LANGUAGE FlexibleContexts #-}

module Day07 where

import Day05 (Computer, computer, parser, runComputerPure)

import Lib (runFile, runFileIO)
import Conduit
import qualified Data.Conduit.Combinators as CC
import Data.Either (either)
import Control.Monad.Except
import Control.Concurrent.MVar

prepend :: Monad m => Int -> ConduitT Int Int m ()
prepend i = yield i >> CC.map id

amplifierChain :: Monad m => (MonadError String) m => [ Int ] -> (Int, Int, Int, Int, Int) -> Computer m ()
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

-- I'm sure there's a clever combo of library functions that does this. Probably
-- involving 'tails'
possibilities :: [ Int ] -> [ (Int, Int, Int, Int, Int) ]
possibilities phases = do
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

-- Hook the output of the computer to an MVar that feeds the result back into
-- the input. This only works because the loop is always one in -> one out.

feedbackSource :: MonadIO m => MVar Int -> ConduitT () Int m ()
feedbackSource mv = do
  v <- liftIO (tryTakeMVar mv)
  maybe (return ()) (\vv -> yield vv >> feedbackSource mv) v

feedbackSink :: MonadIO m => MVar Int -> ConduitT Int Void m ()
feedbackSink mv = awaitForever (liftIO . putMVar mv)

runComputerFeedback :: Int -> Computer (ExceptT String IO) () -> ExceptT String IO Int
runComputerFeedback input c = do
  mv <- lift (newMVar input)
  runConduit
    (  feedbackSource mv
    .| c
    .| feedbackSink mv
    )
  rv <- lift (tryTakeMVar mv)
  maybe (throwError "No final value to return") return rv

-- Calculate max thrust. These are more complicated than they need to be because I want to also return the input parameters
-- that generated the max.

highestThrust :: [ Int ] -> Either String ((Int, Int, Int, Int, Int), Int)
highestThrust heap = (\r -> foldl (\(ap, a) (bp, b:bs) -> if (a > b) then (ap, a) else (bp, b)) ((0, 0, 0, 0, 0), 0) r) <$> runComputers
  where runComputers = (zip (possibilities [0..4])) <$> (sequence (runComputerPure [0] <$> amplifierChain heap <$> possibilities [0..4]))
  
highestThrustFeedback :: [ Int ] -> IO (Either String ((Int, Int, Int, Int, Int), Int))
highestThrustFeedback heap = runExceptT $ (\r -> foldl (\(ap, a) (bp, b) -> if (a > b) then (ap, a) else (bp, b)) ((0, 0, 0, 0, 0), 0) r) <$> runComputers
  where runComputers = (zip (possibilities [5..9])) <$> (sequence (runComputerFeedback 0 <$> amplifierChain heap <$> possibilities [5..9]))

-- Run the exercises

ex1 :: IO (Either String ((Int, Int, Int, Int, Int), Int))
ex1 = runFile "data/day07/0701.txt" parser highestThrust

ex2 :: IO (Either String ((Int, Int, Int, Int, Int), Int))
ex2 = runFileIO "data/day07/0701.txt" parser highestThrustFeedback

-- Debugging functions

tst1 :: IO (Either String ((Int, Int, Int, Int, Int), Int))
tst1 = runFile "data/day07/07tst1.txt" parser highestThrust

thrustFor :: (Int, Int, Int, Int, Int) -> [ Int ] -> Either String [ Int ]
thrustFor ps heap = runComputerPure [0] (amplifierChain heap ps)

thrustForFeedback :: (Int, Int, Int, Int, Int) -> [ Int ] -> IO (Either String Int)
thrustForFeedback ps heap = runExceptT $ runComputerFeedback 0 (amplifierChain heap ps)
