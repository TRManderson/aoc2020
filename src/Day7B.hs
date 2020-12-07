{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.Attoparsec.Text
import qualified Data.Graph.Inductive as G
import Data.Functor ( ($>) )
import Data.List (nub)
import Data.String (fromString)
import Data.Either (rights)


colour = do
  c1 <- many1 letter
  space
  c2 <- many1 letter
  pure $ c1 ++ ' ':c2

noBags :: Parser [(Int, String)]
noBags = string "no other bags" $> []
aBag :: Parser (Int, String)
aBag = do
  n <- decimal
  space
  c <- colour
  space
  if n == 1 then string "bag" else "bags"
  pure (n, c)

line :: Parser (String, [(Int, String)])
line = do
  col <- colour
  string " bags contain "
  bags <- choice [noBags, aBag `sepBy1` string ", "]
  char '.'
  pure (col, bags)


mkGraph :: [(String, [(Int, String)])] -> (G.Gr String Int, Int)
mkGraph vals = (gr, n)
  where
    nodes = nub $ [z | (x, ys) <- vals, (_, y) <- ys, z <- [x, y]]
    edges = [(x, y, v) | (x, ys) <- vals, (v, y) <- ys]
    gr =fst $ G.mkMapGraph nodes edges
    n = fst . head . filter ((== "shiny gold"). snd) . G.labNodes $ gr

solve :: G.Gr String Int -> Int -> Int
solve g n = go n - 1
  where
    go :: G.Node -> Int
    go x = case G.match x g of
      (Just (_, _, _, adjs), _) -> (+1) . sum $ fmap (\(v, y) -> v * go y) adjs
      (Nothing, _) -> 0
      


main = getContents >>= (print . uncurry solve . mkGraph . rights . fmap (parseOnly line . fromString) . lines)