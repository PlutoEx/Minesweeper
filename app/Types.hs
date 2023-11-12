module Types where

-- (x, y)
type Position = (Int, Int)
    
-- (coordinates, number of mines near, state of cell)
-- number of mines:
-- 0-8 = num of mines
-- 9   = mine
type Cell = (Position, Int, CellState)

data CellState = Open | Closed | Flagged
    deriving Eq

type Board = [Cell]

type World = (UI, Settings, GameState)

-- (board, time) "dont sure about time still"
type UI =  (Board, Double)
-- (Ctrl button, Width, Height, Number of Mines)
type Settings = (Bool, Int, Int, Int)
-- NotStarted means first click still wasnt
data GameState = InGame | Lose | Win | Pause | NotStarted
