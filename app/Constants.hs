module Constants where

import CodeWorld

zoomFactor :: Double
zoomFactor = 0.8

cellShape :: Double
cellShape = 1

cellClosedShape :: Double
cellClosedShape = 0.8

cellColor :: Color
cellColor = gray

cellLightColor :: Color
cellLightColor = white

cellDarkColor :: Color
cellDarkColor = black

cellMineColor :: Color
cellMineColor = red

numberColor :: Int -> Color
numberColor 1 = blue
numberColor 2 = green
numberColor 3 = red
numberColor 4 = dark blue
numberColor 5 = brown
numberColor 6 = RGB 0 255 255 -- cyan
numberColor 7 = black
numberColor 8 = grey
numberColor _ = black