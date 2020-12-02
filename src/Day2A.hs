{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.String (fromString)
import Data.Attoparsec.Text
import qualified Data.Text as T
import Data.Either (rights)

data Line = Line {minCount :: Int, maxCount :: Int, letter :: Char, passsword :: String}

parser :: Parser Line
parser = do
  mn <- read <$> many1 digit
  char '-'
  mx <- read <$> many1 digit
  char ' '
  l <- anyChar
  string ": "
  p <- many1 anyChar
  pure $ Line mn mx l p

isValid (Line mn mx l p) = (\c -> mn <= c && c <= mx) . length . filter (== l) $ p

solve :: [String] -> Int
solve =  length . filter isValid . rights . fmap (parseOnly parser . fromString)

main = getContents >>= (print . solve . fmap fromString . lines)