module Lib 
    ( parseFile
    , runFile
    , runFile'
    ) where

import qualified Data.Attoparsec.ByteString.Char8 as AP

import System.IO (openFile, IOMode(..))
import Data.ByteString (hGetContents)
import Data.Monoid (Sum(..))
import Data.Foldable (fold)

parseFile :: String -> AP.Parser a -> IO (Either String a)
parseFile file parser = do
  handle <- openFile file ReadMode
  contents <- hGetContents handle
  return $ AP.parseOnly parser contents
  
runFile :: String -> AP.Parser a -> (a -> Either String b) -> IO (Either String b)
runFile file parser processor = ((=<<) processor) <$> parseFile file parser

runFile' :: String -> AP.Parser a -> (a -> b) -> IO (Either String b)
runFile' file parser processor = runFile file parser (pure . processor)
