module Logic.Cell where

import Types    
import Data.List (find)

updateCell :: Position -> Board -> Status -> (Board, GameStatus)
updateCell _ [] (gameStatus, _) = ([], gameStatus)
updateCell (x, y) ((coords, mines, state) : rest) (gameStatus, ctrl)
  | coords == (x, y) && ctrl == CtrlPressed && state == Closed = ( (coords, mines, Flagged) : rest, gameStatus)
  | coords == (x, y) && ctrl == CtrlPressed && state == Flagged = ( (coords, mines, Closed) : rest, gameStatus)
  | coords == (x, y) && ctrl == CtrlNotPressed && state == Flagged = ( (coords, mines, Open) : rest, gameStatus)
  | coords == (x, y) && ctrl == CtrlNotPressed && state == Closed =
      if mines == Mine
        then ( (coords, mines, Open) : rest, Lose)
        else ( (coords, mines, Open) : rest, gameStatus)
  | otherwise = ( (coords, mines, state) : updateRest, finStatus)
  where
    (updateRest, finStatus) = updateCell (x, y) rest (gameStatus, ctrl)

getCell :: Position -> Board -> Maybe Cell
getCell pos = find (\(coords, _, _) -> coords == pos)

isEmptyCell :: Position -> Board -> Bool
isEmptyCell pos board =
  case getCell pos board of
    Just (_, NotMine 0, _) -> True
    _ -> False

isClosedCell :: Position -> Board -> Bool
isClosedCell pos board =
  case getCell pos board of
    Just (_, _, Closed) -> True
    _ -> False

isFlaggedCell :: Position -> Board -> Bool
isFlaggedCell pos board =
  case getCell pos board of
    Just (_, _, Flagged) -> True
    _ -> False

isOpenCell :: Position -> Board -> Bool
isOpenCell pos board =
  case getCell pos board of
    Just (_, _, Open) -> True
    _ -> False