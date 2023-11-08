module Logic where

import CodeWorld
import Types
import System.Random

changeBoard :: Board -> Double -> Double -> Bool -> Board
changeBoard board x y = updateCell (round x) (round y) board

updateCell :: Int -> Int -> Board -> Bool -> Board
updateCell _ _ [] _ = []
updateCell x y ((coords, mines, state):rest) ctrl
    | coords == (x, y) && ctrl && state == Closed   = (coords, mines, Flagged) : updateCell x y rest ctrl
    | coords == (x, y) && ctrl && state == Flagged  = (coords, mines, Closed) : updateCell x y rest ctrl
    | coords == (x, y) && not ctrl && state /= Open = (coords, mines, Open) : updateCell x y rest ctrl
    | otherwise                                     = (coords, mines, state) : updateCell x y rest ctrl

getRandomNumber :: Int -> Int -> IO Int
getRandomNumber a b = randomRIO (a, b)

generateMines :: Int -> Int -> Int -> [Position] -> IO [Position]
generateMines _ _ 0 mines = return mines
generateMines width height mineCount mines = do
    x <- getRandomNumber 1 width
    y <- getRandomNumber 1 height
    if (x, y) `elem` mines then
        generateMines width height mineCount mines
    else
        generateMines width height (mineCount - 1) ((x, y) : mines)


generateEmptyBoard :: Int -> Int -> Board
generateEmptyBoard w h = [(pos, 0, Closed) | x <- [1..w], y <- [1..h], let pos = (x, y)]

generateBoard :: Int -> Int -> Int -> Position -> IO Board
generateBoard width height minesNumber start = do
    let board = generateEmptyBoard width height
    mines <- generateMines width height minesNumber []
    let boardFilled = fillBoard board mines []
    return boardFilled
    -- if boardValid board start
    --     then return board
    --     else generateBoard width height minesNumber start

boardValid :: Board -> Position -> Bool
boardValid [] _ = False
boardValid ((pos, mines, _) : board) start
    | pos == start && mines == 0 = True
    | pos == start = False
    | otherwise = boardValid board start

fillBoard :: Board -> [Position] -> Board -> Board
fillBoard [] _ filled = filled
fillBoard (((x, y), _, state) : empty) mines filled = 
    if (x, y) `elem` mines then
        fillBoard empty mines (((x, y), 9, state) : filled)
    else
        fillBoard empty mines (((x, y), mine, state) : filled)
        where
            mine = countNearMines (x, y) mines

-- fillBoard :: Board -> [Position] -> Board
-- fillBoard board mines = map (fillCell mines) board

-- fillCell :: [Position] -> Cell -> Cell
-- fillCell mines ((x, y), _, state) =
--     if (x, y) `elem` mines
--         then ((x, y), 9, state)  -- Cell with mine
--         else let count = countNearMines (x, y) mines
--             in ((x, y), count, state)

countNearMines :: Position -> [Position] -> Int
countNearMines _ [] = 0
countNearMines (x, y) ((x1, y1) : mines) =
    if abs (x - x1) <= 1 && abs (y - y1) <= 1
        then 1 + countNearMines (x, y) mines
        else countNearMines (x, y) mines

-- Just for debug
-- printer :: [Position] -> IO ()
-- printer [] = return ()
-- printer ((x, y) : coords) = do
--     print (x, y)
--     printer coords

