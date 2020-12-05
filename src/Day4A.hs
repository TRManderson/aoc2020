{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.Attoparsec.Text.Lazy
import Data.Char ( isSpace )
import Control.Applicative ((<|>))
import Data.Functor ( ($>) )
import qualified Data.Text.Lazy as T
import qualified Data.Text.IO as TIO
import qualified Data.Set as Set

field :: T.Text -> Parser T.Text
field x = do
  string (T.toStrict x)
  char ':'
  yr <- many1 (satisfy (not . isSpace))
  return x

types = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid", "cid"]
entry :: Parser (Set.Set T.Text)
entry = Set.fromList <$> (choice . fmap field $ types) `sepBy1` ((char ' ' $> ()) <|> endOfLine)

required = Set.fromList ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

solve :: [Set.Set T.Text] -> [Set.Set T.Text]
solve = filter (Set.isSubsetOf required)

go solve t = case t of
  Right x -> solve x
  Left x -> putStrLn "Failed" >> print x

main = TIO.getContents >>= (go (print . length . solve) . parseOnly (entry `sepBy` string "\n\n"))