module UI.UI where

import Data.Text (pack)
import Types
import CodeWorld
import UI.Cell
import UI.Menu
import Logic.Logic
import Logic.Board
import System.Random
import Constants

drawBoard :: Board -> Picture
drawBoard = foldr ((<>) . drawCell) blank

drawBorder :: Settings -> Picture
drawBorder (w, h, _) = translated 0 1 (rectangle (fromIntegral w+2) (fromIntegral h+4)) <>
    rectangle (fromIntegral w) (fromIntegral h)

drawWorld :: World -> Picture
drawWorld ((board, time), settings, (gameStatus, _)) = do
    if gameStatus == Pause || gameStatus == PauseNotStarted
        then drawMenu
        else blank
    <> applyCentering (drawBoard board) settings
    <> drawTime time settings
    <> drawStatus gameStatus settings
    <> drawBorder settings
    <> drawMines board settings

drawStatus :: GameStatus -> Settings -> Picture
drawStatus InGame (w, h, _) = translated 0 (fromIntegral h / 2 + 1.5) drawSmile
drawStatus NotStarted (w, h, _) = translated 0 (fromIntegral h / 2 + 1.5) drawSmile
drawStatus Lose (w, h, _) = translated 0 (fromIntegral h / 2 + 1.5) drawDeadSmile
drawStatus Win (w, h, _) = translated 0 (fromIntegral h / 2 + 1.5) drawCoolSmile
drawStatus Pause (w, h, _) = translated 0 (fromIntegral h / 2 + 1.5) drawPauseSmile
drawStatus PauseNotStarted (w, h, _) = translated 0 (fromIntegral h / 2 + 1.5) drawPauseSmile

drawSmile :: Picture
drawSmile = lettering (pack "🙂")

drawDeadSmile :: Picture
drawDeadSmile = lettering (pack "💀")

drawCoolSmile :: Picture
drawCoolSmile = lettering (pack "😎")

drawPauseSmile :: Picture
drawPauseSmile = lettering (pack "😴")

drawTime :: Double -> Settings -> Picture
drawTime time (w, h, _) = translated (fromIntegral (-w) / 2 + 1) (fromIntegral h / 2 + 1.5)
    (rectangle 2 1 <>
    scaled 0.8 0.8 (lettering (pack (show (round time)))))

-- drawButtons :: Settings -> Picture
-- drawButtons settings = drawRertyButton settings <> drawSettingsButton settings

-- drawRertyButton :: Settings -> Picture
-- drawRertyButton (w, h, _) = translated (fromIntegral (-w) / 2 + 2.5) (fromIntegral h / 2 + 1.5) (lettering (pack "⚙"))

-- drawSettingsButton :: Settings -> Picture
-- drawSettingsButton (w, h, _) = translated (fromIntegral (-w) / 2 + 1) (fromIntegral h / 2 + 1.5) (lettering (pack "🔄"))

drawMines :: Board -> Settings -> Picture
drawMines board (w, h, mines) = translated (fromIntegral w / 2 - 1) (fromIntegral h / 2 + 1.5)
    (rectangle 2 1 <>
    scaled 0.8 0.8 (lettering (pack (show (mines - countLeftMines board)))))

applyCentering :: Picture -> Settings -> Picture
applyCentering pic (w, h, _) = translated (-fromIntegral (w+1) / 2) (-fromIntegral (h+1) / 2) pic

applyZoom :: Picture -> Picture
applyZoom = scaled zoomFactor zoomFactor
