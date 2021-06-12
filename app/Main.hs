module Main where

import Control.Monad.Loops
import Lib

height :: Integer
height = 20

screenHeight :: Integer
screenHeight = height + 2

width :: Integer
width = height

screenWidth :: Integer
screenWidth = width + 3

data Tile = Empty | Snake | Apple
  deriving (Eq, Show)

tileToChar :: Tile -> Char
tileToChar Empty = 'e'
tileToChar Snake = 's'
tileToChar Apple = 'a'

board :: [Tile]
board = [Empty | _ <- [0..(10 * 10)]]

onBoard :: Integer -> Bool
onBoard x
  | x < screenWidth = False
  | x > (screenWidth * (screenHeight - 1)) = False
  | mod x screenWidth == 1 = False
  | mod x screenWidth < (width + 2) = True
  | otherwise = False

toTile :: Integer -> Char
toTile x
  | mod x screenWidth == 0 = '\n'
  | onBoard x = ' '
  | otherwise = '*'

draw :: IO ()
draw = putStr [toTile x | x <- [0..(screenWidth * screenHeight)]]

menu :: IO ()
menu = do
  putStrLn "s -> Start"
  putStrLn "q -> Quit"

main :: IO ()
main = do
  menu
  choice <- getLine
  draw
