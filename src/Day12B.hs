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
data Ship = Ship Coord Coord

rotate1 :: Coord -> Coord
rotate1 (x, y) = (-y, x)


doN :: Int -> (a -> a) -> (a -> a)
doN 0 _ = id
doN x f = f . doN (x-1) f

rotate :: Rotation -> Int -> Coord -> Coord
rotate Left x = doN (x `div` 90) rotate1
rotate Right x = doN (x `div` 90) (rotate1 . rotate1 . rotate1)

move :: Direction -> Int -> Coord -> Coord
move North v (x, y) = (x, y+v)
move South v (x, y) = (x, y-v)
move East v (x, y) = (x+v, y)
move West v (x, y) = (x-v, y)

apply :: Instruction -> Ship -> Ship
apply (F v) (Ship (x, y) (x', y')) = Ship (x, y) (x' + (x*v), y' + (y*v))
apply (Turn r x) (Ship wp coord) = (Ship (rotate r x wp) coord)
apply (Go d x) (Ship wp coord) = (Ship (move d x wp) coord)


instr = (go <|> turn <|> f) <*> decimal
  where
    f = char 'F' $> F
    turn = Turn <$> choice [char 'L' $> Left, char 'R' $> Right]
    go = Go <$> choice [char 'N' $> North, char 'W' $> West, char 'E' $> East, char 'S' $> South]

dist (Ship _ (x, y)) = (abs x) + (abs y)

main = getContents >>= (print . dist . foldr apply (Ship (0,0) (0,0)). rights . fmap (parseOnly instr . fromString). lines)