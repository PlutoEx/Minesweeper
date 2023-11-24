module Logic where

import Data.List (find)
import CodeWorld
import Types
import System.Random

updateBoard :: Board -> (Double, Double) -> Settings -> Board
updateBoard board coords (ctrl, w, h, _) =
    let pos = getPosition coords (w, h)
    in if isEmptyCell pos board && not ctrl
        then openNearEmptyCells pos board
        else case getCell pos board of
            Just (_, flags, Open) | flags == getNearFlags pos board -> openExceptMinesCells pos board
            _                                                       -> updateCell pos board ctrl

openExceptMinesCells :: Position -> Board -> Board
openExceptMinesCells (x, y) board = foldr openExceptMinesCellsHelper updatedBoard directions
    where
        updatedBoard = updateCell (x, y) board False
        directions = [(x + dx, y + dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1], (dx, dy) /= (0, 0)]

openExceptMinesCellsHelper :: Position -> Board -> Board
openExceptMinesCellsHelper pos board 
    | isEmptyCell pos board = openNearEmptyCells pos board
    | isClosedCell pos board = updateCell pos board False
    | otherwise = board

-- WTF INT AND INTEGER NOT SAME BRUH
getPosition :: (Double, Double) -> (Int, Int) -> Position
getPosition (x, y) (w, h) = (round (x + fromIntegral (w + 1) / 2), round (y + fromIntegral (h + 1) / 2))

getNearFlags :: Position -> Board -> Int
getNearFlags (x, y) board =
    length $ filter (\(cx, cy) -> isFlaggedCell (cx, cy) board) neighbors
    where
        neighbors = [(x + dx, y + dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1], (dx, dy) /= (0, 0)]

updateCell :: Position -> Board -> Bool -> Board
updateCell _ [] _ = []
updateCell (x, y) ((coords, mines, state) : rest) ctrl
    | coords == (x, y) && ctrl && state == Closed = (coords, mines, Flagged) : updateRest
    | coords == (x, y) && ctrl && state == Flagged = (coords, mines, Closed) : updateRest
    | coords == (x, y) && not ctrl && state == Flagged  = (coords, mines, Open) : updateRest
    | coords == (x, y) && not ctrl && state == Closed = (coords, mines, Open) : updateRest
    | otherwise                                     = (coords, mines, state) : updateRest
    where
        updateRest = updateCell (x, y) rest ctrl

-- 3 Days to find fking inf loop error
openNearEmptyCells :: Position -> Board -> Board
openNearEmptyCells (x, y) board
    | isEmptyCell (x, y) updatedBoard = foldr openNearEmptyCellsHelper updatedBoard directions
    | otherwise = updatedBoard
    where
        updatedBoard = updateCell (x, y) board False
        directions = [(x + dx, y + dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1], (dx, dy) /= (0, 0)]

openNearEmptyCellsHelper :: Position -> Board -> Board
openNearEmptyCellsHelper (x, y) board
    | isClosedCell (x, y) board = openNearEmptyCells (x, y) updatedBoard
    | otherwise = updatedBoard
    where
        updatedBoard = updateCell (x, y) board False

getCell :: Position -> Board -> Maybe Cell
getCell pos = find (\ (coords, _, _) -> coords == pos)

isEmptyCell :: Position -> Board -> Bool
isEmptyCell pos board =
    case getCell pos board of
        Just (_, 0, _) -> True
        _              -> False

isClosedCell :: Position -> Board -> Bool
isClosedCell pos board =
    case getCell pos board of
        Just (_, _, Closed) -> True
        _                   -> False

isFlaggedCell :: Position -> Board -> Bool
isFlaggedCell pos board =
    case getCell pos board of
        Just (_, _, Flagged) -> True
        _                    -> False

isOpenCell :: Position -> Board -> Bool
isOpenCell pos board =
    case getCell pos board of
        Just (_, _, Open) -> True
        _                 -> False

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
-- generateEmptyBoard w h = [(pos, 0, Open) | x <- [1..w], y <- [1..h], let pos = (x, y)]     -- for debug

generateBoard :: Int -> Int -> Int -> Position -> IO Board
generateBoard width height minesNumber start = do
    let board = generateEmptyBoard width height
    mines <- generateMines width height minesNumber []
    let boardFilled = fillBoard board mines
    if boardValid boardFilled start
        then return boardFilled
        else generateBoard width height minesNumber start

-- Check if first click is save
boardValid :: Board -> Position -> Bool
boardValid [] _ = False
boardValid ((pos, mines, _) : board) start
    | pos == start && mines == 0    = True
    | pos == start                  = False
    | otherwise                     = boardValid board start

-- Fill board with numbers of mines
fillBoard :: Board -> [Position] -> Board
fillBoard board mines = fmap (fillCell mines) board

-- Fill cell with numbers of near mines
fillCell :: [Position] -> Cell -> Cell
fillCell mines ((x, y), _, state) =
    if (x, y) `elem` mines
        then ((x, y), 9, state)                      -- Cell with mine
        else ((x, y), countNearMines (x, y) mines, state)

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

-- Just for debug
-- printer :: [Position] -> IO ()
-- printer [] = return ()
-- printer ((x, y) : coords) = do
--     print (x, y)
--     printer coords

