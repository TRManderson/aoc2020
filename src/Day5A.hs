{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.Attoparsec.Text ( choice, count, char, parseOnly )
import Data.Functor ( ($>) )
import Data.String (fromString)
import Data.Either (rights)
import Data.List (sort)

data Vertical = VFront | VBack
  deriving (Eq, Show, Ord)
-- bc Either
data Horizontal = HLeft | HRight
  deriving (Eq, Show, Ord)

hor = choice [ char 'L' $> HLeft
             , char 'R' $> HRight
             ]
ver = choice [ char 'F' $> VFront
             , char 'B' $> VBack
             ]

seat = (,) <$> count 7 ver <*> count 3 hor

toValue :: ([Vertical], [Horizontal]) -> Int
toValue (verts, hors) = (go vertsBool 0 64)*8 + (go horsBool 0 4)
  where
    vertsBool = (== VBack) <$> verts
    horsBool = (== HRight) <$> hors
    go :: [Bool] -> Int -> Int -> Int
    go [] n _ = n
    go (x:xs) n w =
      if x
        then go xs (n+w) (w `div` 2)
        else go xs n (w `div` 2)

main = getContents >>= (print . toValue . maximum . rights . fmap (parseOnly seat . fromString) . lines)