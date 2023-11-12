module Constants where

import CodeWorld

zoomFactor :: Double
zoomFactor = 0.8

panVector :: (Double, Double)
panVector = (-5, -5)

cellOpenPicture :: Picture
cellOpenPicture = solidRectangle cellShape cellShape

cellClosedPicture :: Picture
cellClosedPicture = cellClosedSubPicture <> triangleLightPicture <> triangleDarkPicture

cellClosedSubPicture :: Picture
cellClosedSubPicture = solidRectangle cellClosedShape cellClosedShape

triangleLightPicture :: Picture
triangleLightPicture = colored (light grey) (solidPolygon [(-0.5, -0.5), (-0.5, 0.5), (0.5, 0.5)])

triangleDarkPicture :: Picture
triangleDarkPicture = colored (dark grey) (solidPolygon [(-0.5, -0.5), (0.5, -0.5), (0.5, 0.5)])

cellShape :: Double
cellShape = 1

cellClosedShape :: Double
cellClosedShape = 0.8

cellOpenColor :: Color
cellOpenColor = RGB 192 192 192

cellClosedColor :: Color
cellClosedColor = dark gray -- RGB 128 128 128

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