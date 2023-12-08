{-# LANGUAGE OverloadedStrings #-}
module Events where

import CodeWorld
import Types
import Logic.Logic
import UI.UI
import UI.Menu
import Data.Text (pack)
import System.Random (mkStdGen)


eventHandleInGame :: Event -> World -> (UI, Status)
eventHandleInGame event ((board, time), settings@(w, h, _), status@(gameStatus, ctrl)) =
  case event of
    PointerRelease coords   -> ((newBoard, time), (newStatus, ctrl))
      where
        (newBoard, newStatus) = updateBoard board coords settings (gameStatus, ctrl)
    KeyPress "Ctrl"         -> ((board, time), (gameStatus, CtrlPressed))
    KeyRelease "Ctrl"       -> ((board, time), (gameStatus, CtrlNotPressed))
    KeyRelease "Esc"     -> ((board, time), (Pause, ctrl))
    TimePassing newTime     -> ((board, time + newTime), status)
    _                       -> ((board, time), status)

eventHandleNotStarted :: Event -> Settings -> Double -> (Board, GameStatus)
eventHandleNotStarted event settings@(w, h, mines) time =
  case event of
    PointerRelease coords ->
      let (x, y) = getPosition coords (w, h)
      in
        if x >= 1 && x <= w && y >= 1 && y <= h
          then
            let board = generateBoard (w, h) mines (x, y) (mkStdGen (round (time * 100)))
                (updatedBoard, _) = updateBoard board coords settings (NotStarted, CtrlNotPressed)
            in (updatedBoard, InGame)
          else
            (generateEmptyBoard (w, h), NotStarted)
    KeyRelease "Esc"     -> (generateEmptyBoard (w, h), PauseNotStarted)
    _ -> (generateEmptyBoard (w, h), NotStarted)

eventHandlePause :: Event -> Settings -> GameStatus -> (Settings, GameStatus)
eventHandlePause event settings@(w, h, mines) status =
  case event of
    PointerRelease coords ->
        case getButton coords of
            Just boardSize -> ((boardSize, boardSize, mines), status)
            Nothing -> (settings, status)
    KeyRelease "Esc" ->
      case status of
        Pause -> (settings, InGame) 
        PauseNotStarted -> (settings, NotStarted)
        _ -> (settings, PauseNotStarted)
    _ -> (settings, status)

eventHandle :: Event -> World -> World
eventHandle event world@((board, time), settings@(w, h, mineCount), status@(gameState, ctrl)) =
  case gameState of
    InGame ->
      let ((newBoard, newTime), (newStatus, newCtrl)) = eventHandleInGame event world
      in ((newBoard, newTime), settings, (newStatus, newCtrl))
    NotStarted ->
      let (startBoard, newStatus) = eventHandleNotStarted event settings time
      in ((startBoard, time), settings, (newStatus, ctrl))
    Lose -> 
      let (newSettings, newStatus) = eventHandlePause event settings Lose
      in ((openAllMines board, time), newSettings, (newStatus, ctrl))
    Win -> 
      let (newSettings, newStatus) = eventHandlePause event settings Win
      in ((board, time), newSettings, (newStatus, ctrl))
    Pause -> 
      let (newSettings, newStatus) = eventHandlePause event settings Pause
      in ((board, time), newSettings, (newStatus, ctrl))
    PauseNotStarted -> 
      let (newSettings, newStatus) = eventHandlePause event settings PauseNotStarted
      in ((board, time), newSettings, (newStatus, ctrl))