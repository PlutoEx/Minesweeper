{-# LANGUAGE OverloadedStrings #-}
module UI.UI where

import Types
import CodeWorld
import UI.Cell
import Logic
import System.Random

drawBoard :: Board -> Picture
drawBoard = foldr ((<>) . drawCell) blank

drawWorld :: World -> Picture
drawWorld (board, _) = applyZoomAndPan (drawBoard board)

zoomFactor = 0.8
panVector = (0, 0)

applyZoomAndPan :: Picture -> Picture
applyZoomAndPan = uncurry translated panVector . scaled zoomFactor zoomFactor

initialWorld :: IO World
initialWorld = do
    board <- generateBoard 10 10 10 (1, 1)
    return (board, False)

initialWorld2 = (sampleBoard, False)

eventHandle :: Event -> World -> World
eventHandle event (board, ctrl) = do
    case event of
        PointerRelease (x, y)   -> (changeBoard board x y ctrl, ctrl)
        KeyPress "Ctrl"         -> (board, True)
        KeyRelease "Ctrl"       -> (board, False)
        _                       -> (board, ctrl)

testBoard1 = do
    world <- initialWorld
    activityOf world eventHandle drawWorld


sampleBoard = [((0, 0), 1, Open), ((0, 1), 1, Open), ((1, 0), 3, Open), ((1, 1), 4, Closed), ((0, 2), 9, Closed), ((1, 2), 0, Flagged)]


drawBoard1 = drawingOf (drawBoard sampleBoard)
drawBoard2 = do
    board <- generateBoard 10 10 10 (1, 1)
    drawingOf (drawBoard board)