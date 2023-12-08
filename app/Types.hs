module Types where

-- (x, y)
type Position = (Int, Int)
    
data CellStatus
    = Mine
    | NotMine Int
    deriving Eq

-- (coordinates, number of mines near, state of cell)
type Cell = (Position, CellStatus, CellState)

data CellState = Open | Closed | Flagged
    deriving Eq

type Board = [Cell]

type World = (UI, Settings, Status)

-- (board, time) "dont sure about time still"
type UI =  (Board, Double)
type Status = (GameStatus, CtrlStatus)

-- Board size, Number of Mines
type Settings = (Int, Int, Int)

-- NotStarted means first click still wasnt
data GameStatus = InGame | Lose | Win | Pause | PauseNotStarted | NotStarted
    deriving Eq
data CtrlStatus = CtrlPressed | CtrlNotPressed
    deriving Eq
