module UI.Cell where

import Data.Text (pack)
import CodeWorld
import Types
import Constants

openCellPicture :: Picture
openCellPicture = colored cellOpenColor cellOpenPicture

closedCellPicture :: Picture
closedCellPicture = colored cellClosedColor cellClosedPicture

coloredNumMines :: Int -> Picture
coloredNumMines 0 = blank
coloredNumMines 9 = colored cellMineColor cellOpenPicture
coloredNumMines mines = colored (numberColor mines) (lettering (pack (show mines)))

-- TODO: Add font 
drawCell :: Cell -> Picture
drawCell ((x, y), mines, Open) =
    translated (fromIntegral x) (fromIntegral y)
    (coloredNumMines mines <>
    openCellPicture)
drawCell ((x, y), mines, Closed) =
    translated (fromIntegral x) (fromIntegral y)
    closedCellPicture
drawCell ((x, y), _, Flagged) =
    translated (fromIntegral x) (fromIntegral y)
    (scaled 0.5 0.5 (lettering (pack "🚩")) <>
    closedCellPicture)