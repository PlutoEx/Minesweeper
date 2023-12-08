module Main where

import CodeWorld
import Types
import UI.UI
import Logic.Logic
import Constants
import Events

main :: IO ()
main = testBoard1

initialWorld :: World
initialWorld =
  let w = largeBoard 
      h = mediumBoard
      mines = 15
      board = generateEmptyBoard (w, h)
  in
    ((board, 0), (w, h, mines), (NotStarted, CtrlNotPressed))


testBoard1 =
  activityOf initialWorld eventHandle drawWorld
