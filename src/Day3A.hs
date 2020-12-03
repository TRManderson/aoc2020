module Main where

data Cell = Tree | Open


solve :: [[Cell]] -> Int
solve vals = go 0 0 vals
  where
    toNum Tree = 1
    toNum Open = 0
    go tally start rows = case rows of
      [] -> tally
      (x:xs) -> go ((+ tally). toNum . head . drop (3*start) $ x) (start + 1) xs

parse = fmap (repeat . fmap mapCell)
  where
    repeat row = row ++ repeat row
    mapCell c = case c of
      '.' -> Open
      '#' -> Tree

main = getContents >>= (print . solve . parse . lines)