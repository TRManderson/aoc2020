module Main where
import Control.Monad ( guard )
import qualified Data.Map.Strict as M
import Data.Monoid (Any(..), Endo(..))
import Data.Ix (inRange)

type Grid = (M.Map (Int, Int) Bool, (Int, Int))

inSight :: Grid -> (Int, Int) -> (Int, Int) -> Maybe Bool
inSight (m, b) (x, y) (x', y') = case inRange ((1,1), b) (x+x', y+y') of
  True -> case M.lookup (x+x', y+y') m of
    Just v -> Just v
    Nothing -> inSight (m, b) (x+x', y+y') (x', y')
  False -> Nothing

surrounds :: Grid -> (Int, Int) -> [Bool]
surrounds grid (x, y) = do
  x' <- [-1, 0, 1]
  y' <- [-1, 0, 1]
  guard ((x', y') /= (0,0))
  case inSight grid (x, y) (x', y') of
    Just v -> pure v
    Nothing -> []

parse :: [String] -> Grid
parse rows = ($ b). (,) .($ M.empty) . appEndo . mconcat . fmap Endo $ updates
  where
    b = (length rows, length (head rows))
    parse1 '#' x = M.insert x True
    parse1 'L' x = M.insert x False
    parse1 _ _ = id
    idxd = zip [1..] . fmap (zip [1..]) $ rows 
    updates = [parse1 cell (x, y)  | (x, row) <- idxd, (y, cell) <- row]

update :: Grid -> (Int, Int) -> Bool -> (Any, Bool)
update grid x v = if v' == v
                    then (Any False, v)
                    else (Any True, v')
  where
    vals = surrounds grid x
    count = length . filter id $ vals
    v' = if count >= 5 then False
         else if count == 0 then True
         else v

next :: Grid -> (Any, Grid)
next (m, b) = (,) <$> M.traverseWithKey (update (m, b)) m <*> pure b

run :: Grid -> Grid
run grid = case next grid of
  (Any True, grid') -> run grid'
  (Any False, _) -> grid


main = getContents >>= (print . length . filter id . M.elems . fst . run . parse . lines)