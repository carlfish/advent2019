module Lib 
    ( parseFile
    , onePerLine
    , commaSeparated
    , runFile
    , runFile'
    , runFileIO
    , minimumWith
    , maximumWith
    ) where

import qualified Data.Attoparsec.ByteString.Char8 as AP

import Data.Attoparsec.ByteString.Char8 (Parser)
import System.IO (openFile, IOMode(..))
import Data.ByteString (hGetContents)
import Data.Either (either)
import Data.Foldable (minimumBy, maximumBy)

-- Helpers

minimumWith :: Foldable f => Ord b => (a -> b) -> f a -> a
minimumWith f = minimumBy (\a1 a2 -> f a1 `compare` f a2)

maximumWith :: Foldable f => Ord b => (a -> b) -> f a -> a
maximumWith f = maximumBy (\a1 a2 -> f a1 `compare` f a2)

-- Parsers

onePerLine :: Parser a ->Parser [ a ]
onePerLine p = AP.many' (p <* AP.choice [AP.endOfLine, AP.endOfInput])

commaSeparated :: Parser a ->Parser [ a ]
commaSeparated p = AP.sepBy p (AP.char ',')

parseFile :: String -> Parser a -> IO (Either String a)
parseFile file parser = do
  handle <- openFile file ReadMode
  contents <- hGetContents handle
  return $ AP.parseOnly parser contents
  
-- Exercise runners

runFileIO :: String -> Parser a -> (a -> IO (Either String b)) -> IO (Either String b)
runFileIO file parser processor = do
  p <- parseFile file parser
  either (return . Left) processor p

runFile :: String -> Parser a -> (a -> Either String b) -> IO (Either String b)
runFile file parser processor = ((=<<) processor) <$> parseFile file parser

runFile' :: String -> Parser a -> (a -> b) -> IO (Either String b)
runFile' file parser processor = runFile file parser (pure . processor)