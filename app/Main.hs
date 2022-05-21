module Main where

import Lib
import Data

main :: IO ()
main = do
  let game = makeGame grid languages
  playTurn game

playTurn game = do
  putStrLn . formatGame $ game
  putStr "Please enter a word> "
  word <- getLine
  let newGame = playGame game word
  if completed newGame then
    putStrLn "Congratulations!"
  else  
    playTurn newGame