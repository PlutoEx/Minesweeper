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

data ResultState = InGame | Lose | Win

type Board = [Cell]

-- ((max X, max Y), cells)
type BoardState = ((Int, Int), Board)

-- (board, and ctrl button for flags)
type World = (Board, Bool)
