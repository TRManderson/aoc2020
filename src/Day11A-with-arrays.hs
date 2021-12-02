{-# LANGUAGE RankNTypes #-}
module Main where
import Control.Monad
import qualified Data.Array as A
import qualified Data.Array.ST as S
import Data.Monoid (Any(..))
import Control.Arrow (Kleisli(..))
import qualified Control.Monad.ST as ST

type Grid = A.Array (Int, Int) (Maybe Bool)
type MGrid s = S.STArray s (Int, Int) (Maybe Bool)

empty :: Int -> Int -> ST.ST s (MGrid s)
empty rows cols = S.newListArray ((1,1), (rows, cols)) (repeat Nothing)

surrounds :: Grid -> (Int, Int) -> [Bool]
surrounds grid (x, y) = do
  x' <- [x-1, x, x+1]
  y' <- [y-1, y, y+1]
  guard ((x', y') /= (x, y))
  guard (A.inRange (A.bounds grid) (x', y'))
  case grid A.! (x, y) of
    Just v -> pure v
    Nothing -> []

parse :: [String] -> Grid
parse rows@(cols:_) = S.runSTArray . (start >>=) . runKleisli . mconcat . fmap Kleisli $ updates
  where
    start = empty (length rows) (length cols)
    parse1 :: Char -> (Int, Int) -> (forall s. MGrid s -> ST.ST s (MGrid s))
    parse1 '#' x a = S.writeArray a x (Just True) >> pure a
    parse1 'L' x a = S.writeArray a x (Just False) >> pure a
    parse1 _ _ a = pure a
    idxd = zip [1..] . fmap (zip [1..]) $ rows 
    updates = [parse1 cell (x, y)  | (x, row) <- idxd, (y, cell) <- row]

update ::  Grid -> (Int, Int) -> Maybe Bool -> [((Int, Int), Maybe Bool)]
update _ _ Nothing = []
update grid x (Just v) = if v' == v
                    then []
                    else [(x, Just v')]
  where
    vals = surrounds grid x
    count = length . filter id $ vals
    v' = if count >= 4 then False
         else if count == 0 then True
         else v

next :: Grid -> (Bool, Grid)
next arr = ST.runST $ do
  arr' <- S.thaw arr
  ascs <- S.getAssocs arr'
  let updates = traverse (\ascs -> [((1, 2), Just False)]) ascs
  arr'' <- S.freeze arr
  pure (True, arr'')

run :: Grid -> Grid
run grid = case next grid of
  (True, grid') -> run grid'
  (False, _) -> grid


main = putStrLn "Hi"
--main = getContents >>= (print . length . filter id . M.elems . run . parse . lines)