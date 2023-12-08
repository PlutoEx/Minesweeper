module UI.Menu where

import CodeWorld
import Constants
import Data.Text (pack)

getButton :: (Double, Double) -> Maybe Int
getButton (x, y)
    | y >= 1 && y <= 2 = case () of
        _ | x >= -3 && x <= -1 -> Just smallBoard
        _ | x >= -1 && x <= 1  -> Just mediumBoard
        _ | x >= 1 && x <= 3   -> Just largeBoard
        _                      -> Nothing
    | otherwise = Nothing

drawMenu :: Picture
drawMenu = 
    translated (-2) 1.5 drawSmallBoardButton
    <> translated 0 1.5 drawMediumBoardButton
    <> translated 2 1.5 drawLargeBoardButton
    <> colored gray (solidRectangle 7 7)
    <> colored black (thickRectangle 0.5 7 7)

drawSmallBoardButton :: Picture
drawSmallBoardButton = rectangle 2 1 <> (scaled 0.5 0.5 (lettering (pack "Small")))

drawMediumBoardButton :: Picture
drawMediumBoardButton = rectangle 2 1 <> (scaled 0.5 0.5 (lettering (pack "Medium")))

drawLargeBoardButton :: Picture
drawLargeBoardButton = rectangle 2 1 <> (scaled 0.5 0.5 (lettering (pack "Large")))