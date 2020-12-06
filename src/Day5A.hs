{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.Attoparsec.Text ( choice, count, char, parseOnly )
import Data.Functor ( ($>) )
import Data.String (fromString)
import Data.Either (rights)

hor = choice [ char 'L' $> False
             , char 'R' $> True
             ]
ver = choice [ char 'F' $> False
             , char 'B' $> True
             ]

seat = (++) <$> count 7 ver <*> count 3 hor

toValue :: [Bool] -> Int
toValue vals = go vals 0 512
  where
    go :: [Bool] -> Int -> Int -> Int
    go [] n _ = n
    go (x:xs) n w =
      if x
        then go xs (n+w) (w `div` 2)
        else go xs n (w `div` 2)

main = getContents >>= (print . toValue . maximum . rights . fmap (parseOnly seat . fromString) . lines)