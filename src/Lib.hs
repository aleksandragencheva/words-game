module Lib
    ( formatGrid
    , outputGrid
    , findWord
    , findWords
    , findWordInLine
    , getLines
    , skew
    , zipOverGrid
    , zipOverGridWith
    , coordsGrid
    , gridWithCoords
    , cell2char
    , findWordInCellLinePrefix
    , makeGame
    , score
    , totalWords
    , playGame
    , formatGame
    , formatGameGrid
    , completed
    , Cell(Cell, Indent)
    , Game(gameGrid, gameWords)
    ) where

import Data.List (isInfixOf, transpose)
import Data.Maybe (catMaybes, listToMaybe)
import Data.Char
import qualified Data.Map as M

data Game = Game { 
              gameGrid :: (Grid Cell),
              gameWords :: (M.Map String (Maybe [Cell])) 
            } deriving Show

data Cell = Cell (Int, Int) Char 
          | Indent  
            deriving (Eq, Ord, Show)
type Grid a = [[a]]

makeGame :: Grid Char -> [String] -> Game
makeGame grid words = 
  let gwc = gridWithCoords grid
      tuplify word = (word, Nothing)
      list = map tuplify words
      dict = M.fromList list
  in Game gwc dict

totalWords :: Game -> Int
totalWords game = length . M.keys $ gameWords game

score :: Game -> Int
score game = length . catMaybes. M.elems $ gameWords game

playGame :: Game -> String -> Game
playGame game word | not $ M.member word (gameWords game) = game 
playGame game word = 
  let grid = gameGrid game
      foundWord = findWord grid word
  in case foundWord of
      Nothing -> game
      Just cs -> 
        let dict = gameWords game
            newDict = M.insert word foundWord dict
        in game { gameWords = newDict }

formatGame :: Game -> String
formatGame game = formatGameGrid game ++ "\n\n" ++ (show $ score game) ++ "/" ++ (show $ totalWords game)

completed :: Game -> Bool
completed game = score game == totalWords game

zipOverGrid :: Grid a -> Grid b -> Grid (a,b)
zipOverGrid = zipWith zip
zipOverGridWith :: (a -> b -> c) -> Grid a -> Grid b -> Grid c
zipOverGridWith = zipWith . zipWith

-- mapOverGrid :: (a -> b) -> Grid a -> Grid b
-- mapOverGrid = map . map

coordsGrid :: Grid (Int, Int)
coordsGrid = 
  let rows = map repeat [0..]
      cols = repeat [0..]
  in zipOverGrid rows cols

gridWithCoords :: Grid Char -> Grid Cell
gridWithCoords grid = zipOverGridWith Cell coordsGrid grid

outputGrid :: Grid Cell -> IO ()
outputGrid grid = putStrLn (formatGrid grid)

formatGrid :: Grid Cell -> String
formatGrid grid = 
  let charGrid = (map . map) cell2char grid
  in unlines charGrid

formatGameGrid :: Game -> String
formatGameGrid game = 
  let grid = gameGrid game
      dict = gameWords game :: M.Map String (Maybe [Cell])
      cellSet = concat . catMaybes . M.elems $ dict
      formatCell cell = 
         let char = cell2char cell
         in if cell `elem` cellSet then char else toLower char 
      charGrid = (map . map) formatCell grid
  in unlines charGrid

cell2char :: Cell -> Char
cell2char (Cell _ c) = c
cell2char Indent = '?'

getLines :: Grid Cell -> [[Cell]]
getLines grid = 
  let horizontal = grid
      vertical = transpose grid
      diagonal1 = diagonalize grid
      diagonal2 = diagonalize (map reverse grid)
      lines = horizontal ++ vertical ++ diagonal1 ++ diagonal2
  in lines ++ (map reverse lines)

diagonalize :: Grid Cell -> Grid Cell
diagonalize = (transpose . skew)

findWord :: Grid Cell -> String -> Maybe [Cell]
findWord grid word = 
  let lines = getLines grid
      foundWords = map (findWordInLine word) lines
  in listToMaybe (catMaybes foundWords)

findWords :: Grid Cell -> [String] -> [[Cell]]
findWords grid words = 
  let foundWords = map (findWord grid) words
  in catMaybes foundWords

findWordInLine :: String -> [Cell] -> Maybe [Cell]
findWordInLine _ [] = Nothing
findWordInLine word line = 
  let found = findWordInCellLinePrefix [] word line
  in case found of 
    Nothing -> findWordInLine word (tail line)
    cs@(Just _) -> cs

findWordInCellLinePrefix :: [Cell] -> String -> [Cell] -> Maybe [Cell]
findWordInCellLinePrefix acc (x:xs) (c:cs) | x == cell2char c
  = findWordInCellLinePrefix (c : acc) xs cs
findWordInCellLinePrefix acc [] _ = Just $ reverse acc
findWordInCellLinePrefix _ _ _ = Nothing

skew :: Grid Cell -> Grid Cell
skew [] = []
skew (l:ls) = l : skew (map indent ls)
  where indent line = Indent : line