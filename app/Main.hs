{-# LANGUAGE OverloadedStrings #-}
module Main where

import CodeWorld
import Types
import UI.UI
import Logic

main :: IO ()
main = testBoard1


initialWorld :: IO World
initialWorld = do
    let w = 10
    let h = 10
    let mines = 15
    let startPos = (1, 1)
    board <- generateBoard w h mines startPos
    return ((board, 0), (False, w, h, mines), NotStarted)

eventHandleBoard :: Event -> Board -> Settings -> (Board, Bool)
eventHandleBoard event board setting@(ctrl, w, h, _) = do
    case event of
        PointerRelease coords   -> (updateBoard board coords setting, ctrl)
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
        (newBoard, newCtrl) = eventHandleBoard event board (ctrl, w, h, mineCount)

testBoard1 = do
    world <- initialWorld
    activityOf world eventHandle drawWorld

