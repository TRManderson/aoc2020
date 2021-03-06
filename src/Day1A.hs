module Main where

solve :: [Int] -> Maybe Int
solve vals = case xs of
  [] -> Nothing
  (x:_) -> Just x
  where xs = [a * b | a <- vals, b <- vals, (a + b) == 2020]

main = getContents >>= (print . solve . fmap read . lines)