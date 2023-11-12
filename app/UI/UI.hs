{-# LANGUAGE OverloadedStrings #-}
module UI.UI where

import Types
import CodeWorld
import UI.Cell
import Logic
import System.Random
import Constants

drawBoard :: Board -> Picture
drawBoard = foldr ((<>) . drawCell) blank

drawWorld :: World -> Picture
drawWorld ((board, time), _, state) = applyCentering (drawBoard board)

-- drawTime :: Double -> Picture
-- drawTime = 

applyCentering :: Picture -> Picture
applyCentering = uncurry translated panVector . scaled zoomFactor zoomFactor

applyZoom :: Picture -> Picture
applyZoom = scaled zoomFactor zoomFactor

initialWorld :: IO World
initialWorld = do
    let w = 10
    let h = 10
    let mines = 15
    let startPos = (1, 1)
    board <- generateBoard w h mines startPos
    return ((board, 0), (False, w, h, mines), NotStarted)

eventHandleBoard :: Event -> Board -> Bool -> (Board, Bool)
eventHandleBoard event board ctrl = do
    case event of
        PointerRelease (x, y)   -> (updateBoard board x y ctrl, ctrl)
        KeyPress "Ctrl"         -> (board, True)
        KeyRelease "Ctrl"       -> (board, False)
        _                       -> (board, ctrl)

-- Create eventHandle for Pause

eventHandle :: Event -> World -> World
eventHandle event ((board, time), (ctrl, w, h, mineCount), gameState) =
    case gameState of
        InGame -> ((newBoard, time), (newCtrl, w, h, mineCount), gameState)
        NotStarted -> ((newBoard, time), (newCtrl, w, h, mineCount), gameState)
        -- TODO Others conditions
    where
        (newBoard, newCtrl) = eventHandleBoard event board ctrl
testBoard1 = do
    world <- initialWorld
    activityOf world eventHandle drawWorld


sampleBoard = [((0, 0), 1, Open), ((0, 1), 1, Open), ((1, 0), 3, Open), ((1, 1), 4, Closed), ((0, 2), 9, Closed), ((1, 2), 0, Flagged)]


drawBoard1 = drawingOf (drawBoard sampleBoard)
drawBoard2 = do
    board <- generateBoard 10 10 10 (1, 1)
    drawingOf (drawBoard board)