module Logic.Generator where

import Logic.Board
import Types
import System.Random
import CodeWorld
import Data.Text (pack)

getRandomNumber :: (Int, Int) -> IO Int
getRandomNumber = randomRIO

generateMines :: Int -> Int -> Int -> [Position] -> StdGen -> ([Position], StdGen)
generateMines _ _ 0 mines gen = (mines, gen)
generateMines w h mineCount mines gen =
  let (x, newGen) = randomR (1, w) gen
      (y, finalGen) = randomR (1, h) newGen
  in
  if (x, y) `elem` mines then
    generateMines w h mineCount mines finalGen
  else
    generateMines w h (mineCount - 1) ((x, y) : mines) finalGen

generateEmptyBoard :: (Int, Int) -> Board
generateEmptyBoard (w, h) = [(pos, NotMine 0, Closed) | x <- [1..w], y <- [1..h], let pos = (x, y)]

generateBoard :: (Int, Int) -> Int -> Position -> StdGen -> Board
generateBoard (w, h) minesNumber start gen =
    let board = generateEmptyBoard (w, h)
        (mines, newGen) = generateMines w h minesNumber [] gen
        boardFilled = fillBoard board mines
    in if boardValid boardFilled start
        then boardFilled
        else generateBoard (w, h) minesNumber start newGen

-- Check if first click is save
boardValid :: Board -> Position -> Bool
boardValid [] _ = False
boardValid ((pos, mines, _) : board) start
    | pos == start && mines == NotMine 0    = True
    | pos == start                  = False
    | otherwise                     = boardValid board start

-- Fill board with numbers of mines
fillBoard :: Board -> [Position] -> Board
fillBoard board mines = fmap (fillCell mines) board

-- Fill cell with numbers of near mines
fillCell :: [Position] -> Cell -> Cell
fillCell mines ((x, y), _, state) =
    if (x, y) `elem` mines
        then ((x, y), Mine, state)
        else ((x, y), NotMine (countNearMines (x, y) mines), state)