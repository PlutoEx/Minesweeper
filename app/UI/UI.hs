module UI.UI where

import Data.Text (pack)
import Types
import CodeWorld
import UI.Cell
import Logic
import System.Random
import Constants

drawBoard :: Board -> Picture
drawBoard = foldr ((<>) . drawCell) blank

drawBorder :: Settings -> Picture
drawBorder (_, w, h, _) = rectangle (fromIntegral w) (fromIntegral h)

drawWorld :: World -> Picture
drawWorld ((board, time), settings, state) = applyCentering (drawBoard board) settings <>
    drawTime time settings <>
    drawBorder settings <>
    drawMines board settings

drawTime :: Double -> Settings -> Picture
drawTime time (_, w, h, _) = translated (fromIntegral (-w) / 2 + 1) (fromIntegral h / 2 + 1.5)
    (rectangle 2 1 <>
    scaled 0.8 0.8 (lettering (pack (show time))))

drawMines :: Board -> Settings -> Picture
drawMines board (_, w, h, mines) = translated (fromIntegral w / 2 - 1) (fromIntegral h / 2 + 1.5)
    (rectangle 2 1 <>
    scaled 0.8 0.8 (lettering (pack (show (mines - countLeftMines board)))))

applyCentering :: Picture -> Settings -> Picture
applyCentering pic (_, w, h, _) = translated (-fromIntegral (w+1) / 2) (-fromIntegral (h+1) / 2) pic

applyZoom :: Picture -> Picture
applyZoom = scaled zoomFactor zoomFactor
