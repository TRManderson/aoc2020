{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.Attoparsec.Text ( choice, count, char, parseOnly )
import Data.Functor ( ($>) )
import Data.String (fromString)
import Data.Either (rights)
import Data.Bits (setBit)

hor = choice [ char 'L' $> False
             , char 'R' $> True
             ]
ver = choice [ char 'F' $> False
             , char 'B' $> True
             ]

seat = (++) <$> count 7 ver <*> count 3 hor

toValue :: [Bool] -> Int
toValue = fst . foldr (\b (v,i) -> (if b then setBit v i else v, i+1)) (0,0)

main = getContents >>= (print . toValue . maximum . rights . fmap (parseOnly seat . fromString) . lines)