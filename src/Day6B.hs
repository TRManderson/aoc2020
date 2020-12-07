module Main where

group :: String -> [[String]]
group = foldr fn [] . lines
  where
    fn :: String -> [[String]] -> [[String]]
    fn [] xs = []:xs
    fn y []= [[y]]
    fn y (x:xs) = (y:x):xs

groupCount :: [String] -> Int
groupCount = length . foldr1 (\a b -> filter (`elem` b) a)

main = getContents >>= (print . sum . fmap groupCount . group)