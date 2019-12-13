{-# LANGUAGE FlexibleContexts #-}

module Day11 where

import Lib (runFileIO)
import Data.Map.Strict (Map, findWithDefault, empty, insert, size, singleton, keys)
import Control.Monad.Except
import Data.Maybe (maybe)
import IntCode (Computer, MWord, parser, bigComputer)
import Conduit
import Control.Concurrent.MVar

type Coord = (Int, Int)
data Colour = Black | White deriving (Eq, Ord, Show)

data Direction = U | D | L | R
data Rotation 
  = Clockwise     {- right -} 
  | Anticlockwise {- left -}

data Robot = Robot { coords :: Coord, direction :: Direction }

data State = State { bot :: Robot, hull :: Hull }

type Hull = Map Coord Colour

data Orders = Orders Colour Rotation

data Paint = Paint Coord Colour

-- Robot and Hull operations

initRobot :: Robot
initRobot = Robot (0, 0) U

colourAt :: Hull -> Coord -> Colour
colourAt hull c = findWithDefault Black c hull

colourAtS :: State -> Colour
colourAtS (State (Robot c _) h) = colourAt h c

move :: Robot -> Robot
move (Robot (x, y) U) = Robot (x    , y + 1) U
move (Robot (x, y) D) = Robot (x    , y - 1) D
move (Robot (x, y) L) = Robot (x - 1, y    ) L
move (Robot (x, y) R) = Robot (x + 1, y    ) R

processOrders :: Orders -> State -> State
processOrders (Orders c r) (State bot hull) = State (move turnedRobot) (paintedHull)
  where
    paintedHull = insert (coords bot) c hull
    turnedRobot = bot { direction = turn r (direction bot)}


turn :: Rotation -> Direction -> Direction
turn Clockwise d = case d of
  U -> R
  R -> D
  D -> L
  L -> U
turn Anticlockwise d = case d of
  U -> L
  L -> D
  D -> R
  R -> U 

-- Drawing the hull image

printHull :: Hull -> IO ()
printHull h = 
  let 
    p Black = "  "
    p White = "**"
  in
    sequence_ $ (fmap putStrLn) (fmap ((=<<) p) (imageMap h))  

imageMap :: Hull -> [ [ Colour ] ]
imageMap h = fmap (fmap (colourAt h)) (imageCoords h)

imageCoords :: Hull -> [ [ Coord ] ]
imageCoords h = 
  let 
    ((minx, miny), (maxx, maxy)) = coordinateBounds (keys h)
  in 
    -- Y values need to be reversed to put high ones at the top of the image.
    reverse $ (\y -> (zip [minx..maxx]) (repeat y)) <$> [miny..maxy]

coordinateBounds :: [ (Int, Int) ] -> ((Int, Int), (Int, Int))
coordinateBounds = foldl 
  (\((mnx, mny), (mxx, mxy)) (x, y) ->  ((min mnx x, min mny y), (max mxx x, max mxy y)))
  ((0, 0), (0, 0))

-- Hooking up to IntCode

interpretColour :: MonadError String m => MWord -> m Colour
interpretColour 0 = return Black
interpretColour 1 = return White
interpretColour i = throwError ("Unknown colour value: " <> show i)

interpretRotation :: MonadError String m => MWord -> m Rotation
interpretRotation 0 = return Anticlockwise
interpretRotation 1 = return Clockwise
interpretRotation i = throwError ("Unknown rotation value: " <> show i)

colourToMWord :: Colour -> MWord
colourToMWord Black = 0
colourToMWord White = 1

orderInterpreter :: Monad m => ConduitT MWord Orders (ExceptT String m) ()
orderInterpreter = awaitColour
  where 
    awaitColour = 
      maybe (return ()) 
            (awaitRotation <=< interpretColour)
        =<< await
    awaitRotation c = 
      maybe (throwError ("Unexpected end of input waiting for rotation")) 
            (const awaitColour <=< yield . Orders c <=< interpretRotation) 
        =<< await

orderSink :: MonadIO m => MVar MWord -> State -> ConduitT Orders Void (ExceptT String m) Hull
orderSink mv state =
      maybe (return (hull state))
            ( \o -> case processOrders o state of
                      newState -> liftIO (putMVar mv (colourToMWord (colourAtS newState))) >> orderSink mv newState)
        =<< await

feedbackSource :: MonadIO m => MVar MWord -> ConduitT () MWord m ()
feedbackSource mv = do
  v <- liftIO (tryTakeMVar mv)
  maybe (return ()) (\vv -> yield vv >> feedbackSource mv) v

runRobotFromComputer :: State -> Computer IO () -> IO (Either String Hull)
runRobotFromComputer is c = runExceptT $ do
  mv <- lift (newMVar (colourToMWord (colourAtS is)))
  runConduit
    (  feedbackSource mv
    .| c
    .| orderInterpreter
    .| orderSink mv is
    )

printResult :: State -> Computer IO () -> IO (Either String ())
printResult is c = join $ do
  eh <- runRobotFromComputer is c
  return (sequence (printHull <$> eh))

countPaintedSquares :: State -> Computer IO () -> IO (Either String Int)
countPaintedSquares is c = fmap (fmap size) (runRobotFromComputer is c)

-- exercises

ex1 :: IO (Either String Int)
ex1 = runFileIO "data/day11/1101.txt" parser ((countPaintedSquares (State initRobot empty)) . bigComputer)

ex2 :: IO (Either String ())
ex2 = runFileIO "data/day11/1101.txt" parser ((printResult (State initRobot (singleton (0, 0) White))) . bigComputer)