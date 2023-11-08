module Constants where

import CodeWorld

cellPicture :: Picture
cellPicture = solidRectangle cellShape cellShape 

cellShape :: Double
cellShape = 1

openColor1 :: Color
openColor1 = yellow -- RGB 192 192 192

openColor2 :: Color
openColor2 = light gray

closedColor1 :: Color
closedColor1 = dark gray

closedColor2 :: Color
closedColor2 = dark green

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