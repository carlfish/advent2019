{-# LANGUAGE FlexibleContexts #-}

-- A lot of the important code for this day was done in the IntCode
-- module, specifically updating it to use Conduit for IO.

module Day07 where

import IntCode (Computer, MWord, smallComputer, parser, runComputerPure, feedbackSource)

import Lib (runFile, runFileIO, maximumWith)
import Conduit
import qualified Data.Conduit.Combinators as CC
import Data.Either (either)
import Control.Monad.Except
import Control.Concurrent.MVar

prepend :: Monad m => MWord -> ConduitT MWord MWord m ()
prepend i = yield i >> CC.map id

amplifierChain :: Monad m => [ MWord ] -> (MWord, MWord, MWord, MWord, MWord) -> Computer m ()
amplifierChain heap (pa, pb, pc, pd, pe)
  =  prepend pa
  .| smallComputer heap
  .| prepend pb
  .| smallComputer heap
  .| prepend pc
  .| smallComputer heap
  .| prepend pd
  .| smallComputer heap
  .| prepend pe
  .| smallComputer heap

-- I'm sure there's a clever combo of library functions that does this. Probably
-- involving 'tails'
possibilities :: [ MWord ] -> [ (MWord, MWord, MWord, MWord, MWord) ]
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

feedbackSink :: MonadIO m => MVar MWord -> ConduitT MWord Void m ()
feedbackSink mv = awaitForever (liftIO . putMVar mv)

runComputerFeedback :: MWord -> Computer IO () -> ExceptT String IO MWord
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

highestThrust :: [ MWord ] -> Either String ((MWord, MWord, MWord, MWord, MWord), MWord)
highestThrust heap = ((\r -> maximumWith snd r) <$> runComputers)
  where runComputers = (zip (possibilities [0..4])) <$> sequence (fmap head <$> runComputerPure [0] <$> amplifierChain heap <$> possibilities [0..4])
  
highestThrustFeedback :: [ MWord ] -> IO (Either String ((MWord, MWord, MWord, MWord, MWord), MWord))
highestThrustFeedback heap = runExceptT $ (\r -> maximumWith snd r) <$> runComputers
  where runComputers = (zip (possibilities [5..9])) <$> (sequence (runComputerFeedback 0 <$> amplifierChain heap <$> possibilities [5..9]))

-- Run the exercises

ex1 :: IO (Either String ((MWord, MWord, MWord, MWord, MWord), MWord))
ex1 = runFile "data/day07/0701.txt" parser highestThrust

ex2 :: IO (Either String ((MWord, MWord, MWord, MWord, MWord), MWord))
ex2 = runFileIO "data/day07/0701.txt" parser highestThrustFeedback

-- Debugging functions

tst1 :: IO (Either String ((MWord, MWord, MWord, MWord, MWord), MWord))
tst1 = runFile "data/day07/07tst1.txt" parser highestThrust

thrustFor :: (MWord, MWord, MWord, MWord, MWord) -> [ MWord ] -> Either String [ MWord ]
thrustFor ps heap = runComputerPure [0] (amplifierChain heap ps)

thrustForFeedback :: (MWord, MWord, MWord, MWord, MWord) -> [ MWord ] -> IO (Either String MWord)
thrustForFeedback ps heap = runExceptT $ runComputerFeedback 0 (amplifierChain heap ps)
