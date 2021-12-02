{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.Attoparsec.Text
import Data.String (fromString)
import qualified Data.Set as S

parseRule :: Parser Rule
parseRule = do
    name <- many1 (satisfy  (/= ':'))
    string ": "
    pairs <- sepBy1 ((,) <$> decimal <* char '-' <*> decimal) (string " or ")
    pure $ Rule name pairs

parseTicket :: Parser Ticket
parseTicket = sepBy1 decimal (char ',')

data Rule = Rule String [(Int, Int)]
type Ticket = [Int]

data Problem = Problem [Rule] Ticket [Ticket]

parser :: Parser Problem
parser = do
  rules <- sepBy1 parseRule endOfLine
  many1 endOfLine
  string "your ticket:\n"
  yours <- parseTicket
  many1 endOfLine
  string "nearby tickets:\n"
  rest <- sepBy1 parseTicket endOfLine
  pure $ Problem rules yours rest



solve (Problem rules mine rest) = foldr foldFn (fmap (const $ S.fromList ruleNames) mine) . filter (all filterOne) $ rest
  where
    ruleNames = fmap (\(Rule n _ ) -> n) rules
    foldFn = _
    filterOne n = all (\(Rule _ pairs) -> any (\(min, max) -> n >= min && n <= max) pairs) rules

main = getContents >>= (print . solve . (\(Right x) -> x) .parseOnly parser . fromString)