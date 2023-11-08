module UI.Cell where

import Data.Text (pack)
import CodeWorld
import Types
import Constants

openCellPicture :: Picture
openCellPicture = colored openColor1 cellPicture

closedCellPicture :: Picture
closedCellPicture = colored closedColor1 cellPicture

coloredNumMines :: Int -> Picture
coloredNumMines 0 = blank
coloredNumMines mines = colored (numberColor mines) (lettering (pack (show mines)))

-- TODO: Add font 
-- TODO: Add gray button like style and borders
drawCell :: Cell -> Picture
drawCell ((x, y), mines, Open) =
    translated (fromIntegral x) (fromIntegral y)
    (coloredNumMines mines) <>
    openCellPicture
drawCell ((x, y), mines, Closed) =
    translated (fromIntegral x) (fromIntegral y)
    closedCellPicture
drawCell ((x, y), _, Flagged) =
    translated (fromIntegral x) (fromIntegral y)
    (scaled 0.5 0.5 (lettering (pack "🚩")) <>
    closedCellPicture)