{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.Attoparsec.Text.Lazy
import Control.Applicative ((<|>))
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import qualified Data.Set as Set
import Data.Either (rights)
import Control.Monad
import Data.Char (isSpace)

byr = do
    string "byr:"
    n <- decimal
    when (n < 1920 || n >2002) $ fail "Bad year"
    pure "byr"

iyr = do
    string "iyr:"
    n <- decimal
    when (n < 2010 || n >2020) $ fail "Bad year"
    pure "iyr"

eyr = do
    string "eyr:"
    n <- decimal
    when (n < 2020 || n >2030) $ fail "Bad year"
    pure "eyr"


hgt = do
    string "hgt:"
    n <- decimal
    t <- choice ["cm", "in"]
    case t of
        "cm" -> when (n < 150 || n > 193) $ fail "Bad height"
        "in" -> when (n < 59 || n > 76) $ fail "Bad height"
    pure "hgt"

hexChar = digit <|> choice (fmap char ['a'..'g'])
hcl = do
    string "hcl:#"
    count 6 hexChar
    pure "hcl"

ecl = do
    string "ecl:"
    choice . fmap string $ ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
    pure "ecl"

pid = do
    string "pid:"
    count 9 digit
    pure "pid"

cid = do
    string "cid:"
    many1 (satisfy (not . isSpace))
    pure "cid"

entry :: Parser (Set.Set T.Text)
entry = Set.fromList <$> (choice [byr, eyr, iyr, hgt, hcl, ecl, cid, pid]) `sepBy1` space

required = Set.fromList ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

main = do
    txt <- TIO.getContents 
    let entries = T.splitOn "\n\n" txt
    print . length . filter (Set.isSubsetOf required) . rights . fmap (parseOnly entry . T.toStrict) $ entries