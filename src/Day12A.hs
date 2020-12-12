module Main where
import Prelude hiding (Left, Right)
import Data.Attoparsec.Text
import Data.Functor (($>))
import Control.Applicative
import Data.Either (rights)
import Data.String (fromString)

data Direction = North | East | South | West
  deriving (Eq, Show, Ord, Bounded, Enum)
data Rotation = Left | Right
  deriving (Eq, Show, Ord, Bounded, Enum)
data Instruction = Turn Rotation Int | Go Direction Int | F Int
type Coord = (Int, Int)
data Ship = Ship Direction Coord

rotate1 :: Direction -> Direction
rotate1 North = West
rotate1 West = South
rotate1 South = East
rotate1 East = North


doN :: Int -> (a -> a) -> (a -> a)
doN 0 _ = id
doN x f = f . doN (x-1) f

rotate :: Rotation -> Int -> Direction -> Direction
rotate Left x = doN (x `div` 90) rotate1
rotate Right x = doN (x `div` 90) (rotate1 . rotate1 . rotate1)

move :: Direction -> Int -> Coord -> Coord
move North v (x, y) = (x, y+v)
move South v (x, y) = (x, y-v)
move East v (x, y) = (x+v, y)
move West v (x, y) = (x-v, y)

apply :: Instruction -> Ship -> Ship
apply (F x) (Ship dir coord) = Ship dir (move dir x coord)
apply (Turn r x) (Ship dir coord) = (Ship (rotate r x dir) coord)
apply (Go d x) (Ship dir coord) = (Ship dir (move d x coord))


instr = (go <|> turn <|> f) <*> decimal
  where
    f = char 'F' $> F
    turn = Turn <$> choice [char 'L' $> Left, char 'R' $> Right]
    go = Go <$> choice [char 'N' $> North, char 'W' $> West, char 'E' $> East, char 'S' $> South]

dist (Ship _ (x, y)) = (abs x) + (abs y)

main = getContents >>= (print . dist . foldr apply (Ship East (0,0)). rights . fmap (parseOnly instr . fromString). lines)