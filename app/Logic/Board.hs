module Logic.Board where

import CodeWorld
import Types
import Logic.Cell

updateBoard :: Board -> (Double, Double) -> Settings -> Status -> (Board, GameStatus)
updateBoard board coords (w, h, _) status@(gameStatus, ctrl) =
  let pos = getPosition coords (w, h)
      (updBoard, updStatus) =
        if isEmptyCell pos board && ctrl == CtrlNotPressed
          then openNearEmptyCells pos (board, gameStatus)
          else case getCell pos board of
            Just (_, NotMine flags, Open) | flags == countNearFlags pos board -> openExceptFlagsCells pos (board, gameStatus)
            _ -> updateCell pos board status
      newStatus = if isAllOpen updBoard && updStatus /= Lose then Win else updStatus
  in (updBoard, newStatus)

-- WTF INT AND INTEGER NOT SAME BRUH
getPosition :: (Double, Double) -> (Int, Int) -> Position
getPosition (x, y) (w, h) = (round (x + fromIntegral (w + 1) / 2), round (y + fromIntegral (h + 1) / 2))

openExceptFlagsCells :: Position -> (Board, GameStatus) -> (Board, GameStatus)
openExceptFlagsCells (x, y) (board, gameStatus) = foldr openExceptFlagsCellsHelper updated directions
  where
    updated@(updBoard, updStatus) = updateCell (x, y) board (gameStatus, CtrlNotPressed)
    directions = [(x + dx, y + dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1], (dx, dy) /= (0, 0)]

openExceptFlagsCellsHelper :: Position -> (Board, GameStatus) -> (Board, GameStatus)
openExceptFlagsCellsHelper pos (board, gameStatus)
  | isEmptyCell pos board = openNearEmptyCells pos (board, gameStatus)
  | isClosedCell pos board = updateCell pos board (gameStatus, CtrlNotPressed)
  | otherwise = (board, gameStatus)

-- 3 Days to find fking inf loop error
openNearEmptyCells :: Position -> (Board, GameStatus) -> (Board, GameStatus)
openNearEmptyCells (x, y) (board, gameStatus)
  | isEmptyCell (x, y) updBoard = foldr openNearEmptyCellsHelper updated directions
  | otherwise = updated
  where
    updated@(updBoard, updStatus) = updateCell (x, y) board (InGame, CtrlNotPressed)
    directions = [(x + dx, y + dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1], (dx, dy) /= (0, 0)]

openNearEmptyCellsHelper :: Position -> (Board, GameStatus) -> (Board, GameStatus)
openNearEmptyCellsHelper (x, y) (board, gameStatus)
  | isClosedCell (x, y) board = openNearEmptyCells (x, y) updated
  | otherwise = updated
  where
    updated@(updBoard, updStatus) = updateCell (x, y) board (gameStatus, CtrlNotPressed)

countNearFlags :: Position -> Board -> Int
countNearFlags (x, y) board =
  length $ filter (\(cx, cy) -> isFlaggedCell (cx, cy) board) neighbors
  where
    neighbors = [(x + dx, y + dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1], (dx, dy) /= (0, 0)]

-- Function that count number of near mines in pos (x, y)
countNearMines :: Position -> [Position] -> Int
countNearMines _ [] = 0
countNearMines (x, y) ((x1, y1) : mines) =
  if abs (x - x1) <= 1 && abs (y - y1) <= 1
    then 1 + countNearMines (x, y) mines
    else countNearMines (x, y) mines

countLeftMines :: Board -> Int
countLeftMines [] = 0
countLeftMines ((_, _, Flagged) : rest) = 1 + countLeftMines rest
countLeftMines (_ : rest) = countLeftMines rest

-- Check winning situation
isAllOpen :: Board -> Bool
isAllOpen = all (\(_, _, state) -> state == Open || state == Flagged)

openAllMines :: Board -> Board
openAllMines [] = []
openAllMines ((pos, Mine, Closed) : rest) = (pos, Mine, Open) : openAllMines rest 
openAllMines ((pos, Mine, Flagged) : rest) = (pos, Mine, Open) : openAllMines rest 
openAllMines (cell : rest) = cell : openAllMines rest