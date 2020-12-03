module Main where
import Control.Parallel.Strategies

data Cell = Tree | Open


solve :: Int -> Int -> [[Cell]] -> Int
solve rights downs vals = (go 0 0 vals) `using` rpar
  where
    toNum Tree = 1
    toNum Open = 0
    go tally start rows = case rows of
      [] -> tally
      (x:xs) -> go ((+ tally). toNum . head . drop (rights*start) $ x) (start + 1) (drop (downs - 1) xs)

parse = fmap (repeat . fmap mapCell)
  where
    repeat row = row ++ repeat row
    mapCell c = case c of
      '.' -> Open
      '#' -> Tree

data Slope = Slope Int Int

solveAll :: [Slope] -> [[Cell]] -> [Int]
solveAll slopes vals = fmap (\(Slope r d) -> solve r d vals) slopes

slopes = [
    Slope 1 1,
    Slope 3 1,
    Slope 5 1,
    Slope 7 1,
    Slope 1 2]

main = getContents >>= (print . product. solveAll slopes . parse . lines)