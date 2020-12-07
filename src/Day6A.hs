module Main where
import Data.List (nub)

group :: String -> [[String]]
group = foldr fn [] . lines
  where
    fn :: String -> [[String]] -> [[String]]
    fn [] xs = []:xs
    fn y []= [[y]]
    fn y (x:xs) = (y:x):xs

groupCount :: [String] -> Int
groupCount = length . nub . concat

main = getContents >>= (print . sum . fmap groupCount . group)