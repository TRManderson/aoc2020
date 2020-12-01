module Main where

solve :: [Int] -> Maybe Int
solve vals = case xs of
  [] -> Nothing
  (x:_) -> Just x
  where xs = [a * b * c| a <- vals, b <- vals, c <- vals, (a + b + c) == 2020]

main = getContents >>= (print . solve . fmap read . lines)