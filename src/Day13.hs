{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Day13 where

import Lib (runFile, runFileIO)
import Data.Map.Strict (Map, insert, empty, size, toList)
import qualified Data.Map.Strict as M
import IntCode (Computer, MWord, Program, parser, bigComputer, bigComputer', wordToInt, modifyMemory)
import Control.Monad.Except
import Data.Maybe (maybe, fromMaybe, listToMaybe)
import Control.Concurrent.MVar
import Conduit

type Coords = (Int, Int)
type VMem = Map Coords Tile

data Update 
  = UpdateVMem Coords Tile
  | UpdateScore MWord

data State = State { vmem :: VMem, score :: MWord }

data Tile 
  = Empty
  | Wall
  | Block
  | HPaddle
  | Ball
  deriving (Eq, Show)

data Joystick
  = JoyLeft
  | JoyNeutral
  | JoyRight

newGame :: State
newGame = State empty 0

tile :: MonadError String m => MWord -> m Tile
tile 0 = return Empty
tile 1 = return Wall
tile 2 = return Block
tile 3 = return HPaddle
tile 4 = return Ball
tile i = throwError ("Unknown tile type: " <> (show i))

joystickToMWord :: Joystick -> MWord
joystickToMWord JoyLeft    = -1
joystickToMWord JoyNeutral =  0
joystickToMWord JoyRight   =  1

update :: Update -> State -> State
update (UpdateVMem c t) s = s { vmem = insert c t (vmem s) }
update (UpdateScore score) s = s { score = score }

-- Game state calculations

matchingTiles :: Tile -> VMem -> [ (Coords) ]
matchingTiles t v = fst <$> (filter (\(_, tt) -> tt == t) . toList) v

-- Assumption: there is only one ball
ballX :: VMem -> Maybe Int
ballX v = fst <$> (listToMaybe (matchingTiles Ball v))

-- Assumption: paddle coords form a contiguous line along the same Y axis
paddleCentreX :: VMem -> Maybe Int
paddleCentreX v = centerCoord (fst <$> (matchingTiles HPaddle v))
  where
    centerCoord [] = Nothing 
    centerCoord xs = Just (div ((minimum xs) + (maximum xs)) 2)

calculateMove :: VMem -> Joystick
calculateMove v = 
  let
    calculateMove' p b =     
      if      (p > b)  then JoyLeft
      else if (p < b)  then JoyRight
      else                  JoyNeutral
  in
    fromMaybe JoyNeutral (calculateMove' <$> paddleCentreX v <*> ballX v)

-- Computer parts

updateCoalescer :: Monad m => ConduitT MWord Update (ExceptT String m) ()
updateCoalescer = readX
  where
    readX = 
      maybe (return ())
      (\x -> readY x)
      =<< await
    readY x = 
      maybe (throwError "EOF reading next y value")
      (\y -> readParameter x y)
      =<< await
    readParameter (-1) 0 = 
      readScore
    readParameter x y = 
      readTile ((wordToInt x), (wordToInt y))
    readTile coords =
      maybe (throwError "EOF reading next tile value")
      (\t -> const readX =<< yield =<< ((UpdateVMem coords) <$> lift (tile t)))
      =<< await
    readScore =
      maybe (throwError "EOF reading next score value")
      (\i -> const readX =<< yield (UpdateScore i))
      =<< await
  
vmemSink :: Monad m => ConduitT Update Void (ExceptT String m) VMem
vmemSink = vmem <$> stateSink newGame 
  where
    stateSink s = maybe (return s) (\u -> stateSink (update u s))
      =<< await

dumpOutputToVMem :: Computer Identity () -> Either String VMem
dumpOutputToVMem c = (runIdentity . runExceptT) 
  (runConduit
    (  yieldMany []
    .| c
    .| updateCoalescer
    .| vmemSink
    )
  )
  
-- Assumption: the game ends itself when there are no more blocks
joystickController :: MonadIO m => MVar MWord -> State -> ConduitT Update Void (ExceptT String m) State
joystickController mv state =
  let
    playNextTurn newState = do
      liftIO (swapMVar mv (joystickToMWord (calculateMove (vmem newState)))) 
      joystickController mv newState
  in
      maybe (return state)
            (\u -> playNextTurn (update u state))
        =<< await
  
-- Source that reads the value out of an MVar without consuming it (input is
-- a queryable state rather than a sequence of values). It's up to the provider
-- of the MVar to make sure there's always something in there.
fixedStateSource :: MonadIO m => MVar MWord -> ConduitT () MWord m ()
fixedStateSource mv = do
  v <- liftIO (readMVar mv)
  yield v >> fixedStateSource mv


runGameForScore :: Computer IO () -> IO (Either String MWord)
runGameForScore c = runExceptT $ do
  mv <- lift (newMVar 0)
  s  <- runConduit
        (  fixedStateSource mv
        .| c
        .| updateCoalescer
        .| joystickController mv newGame
        )
  return (score s)

setToFreeMode :: Monad m => Program m ()
setToFreeMode = modifyMemory 0 2

-- exercises

countBlocks :: VMem -> Int
countBlocks = size . (M.filter (== Block))

ex1 :: IO (Either String Int)
ex1 = runFile "data/day13/1301.txt" parser (\c -> countBlocks <$> dumpOutputToVMem (bigComputer c))

ex2 :: IO (Either String MWord)
ex2 = runFileIO "data/day13/1301.txt" parser (\c -> runGameForScore (bigComputer' setToFreeMode c))