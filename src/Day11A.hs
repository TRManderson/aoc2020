module Main where
import Control.Monad ( guard )
import qualified Data.Map.Strict as M
import Data.Monoid (Any(..), Endo(..))

type Grid = M.Map (Int, Int) Bool


surrounds :: Grid -> (Int, Int) -> [Bool]
surrounds grid (x, y) = do
  x' <- [x-1, x, x+1]
  y' <- [y-1, y, y+1]
  guard ((x', y') /= (x, y))
  case M.lookup (x', y') grid of
    Just v -> pure v
    Nothing -> []

parse :: [String] -> Grid
parse rows = ($ M.empty) . appEndo . mconcat . fmap Endo $ updates
  where
    parse1 '#' x = M.insert x True
    parse1 'L' x = M.insert x False
    parse1 _ _ = id
    idxd = zip [1..] . fmap (zip [1..]) $ rows 
    updates = [parse1 cell (x, y)  | (x, row) <- idxd, (y, cell) <- row]

update ::  Grid -> (Int, Int) -> Bool -> (Any, Bool)
update grid x v = if v' == v
                    then (Any False, v)
                    else (Any True, v')
  where
    vals = surrounds grid x
    count = length . filter id $ vals
    v' = if count >= 4 then False
         else if count == 0 then True
         else v

next :: Grid -> (Any, Grid)
next m = M.traverseWithKey (update m) m

run :: Grid -> Grid
run grid = case next grid of
  (Any True, grid') -> run grid'
  (Any False, _) -> grid


main = getContents >>= (print . length . filter id . M.elems . run . parse . lines)