module UI.Cell where

import Data.Text (pack)
import CodeWorld
import Types
import Constants

cellOpenPicture :: Picture
cellOpenPicture = colored cellColor (solidRectangle cellShape cellShape)    

cellClosedPicture :: Picture
cellClosedPicture = cellClosedSubPicture <> triangleLightPicture <> triangleDarkPicture

cellClosedSubPicture :: Picture
cellClosedSubPicture = colored cellColor (solidRectangle cellClosedShape cellClosedShape)

triangleLightPicture :: Picture
triangleLightPicture = colored cellLightColor (solidPolygon [(-0.5, -0.5), (-0.5, 0.5), (0.5, 0.5)])

triangleDarkPicture :: Picture
triangleDarkPicture = colored cellDarkColor (solidPolygon [(-0.5, -0.5), (0.5, -0.5), (0.5, 0.5)])

coloredNumMines :: Int -> Picture
coloredNumMines 0 = blank
coloredNumMines mines = colored (numberColor mines) (lettering (pack (show mines)))

-- TODO: Add font 
drawCell :: Cell -> Picture
drawCell ((x, y), Mine, Open) =
    translated (fromIntegral x) (fromIntegral y)
    (colored cellMineColor cellOpenPicture <>
    cellOpenPicture)
drawCell ((x, y), NotMine mines, Open) =
    translated (fromIntegral x) (fromIntegral y)
    (coloredNumMines mines <>
    cellOpenPicture)
drawCell ((x, y), _, Closed) =
    translated (fromIntegral x) (fromIntegral y)
    cellClosedPicture
drawCell ((x, y), _, Flagged) =
    translated (fromIntegral x) (fromIntegral y)
    (scaled 0.5 0.5 (lettering (pack "🚩")) <>
    cellClosedPicture)