module AppConfig (
    winWidth,
    winHeight,
    display,
    fps,
    fullTime
) where

import Graphics.Gloss.Interface.Pure.Game 

-- Game Display Mode

winWidth :: Int 
winWidth = 400

winHeight :: Int 
winHeight = 600

display :: Display 
display = InWindow "Lumberjack" (winWidth, winHeight) (500, 500) 

-- FPS

fps :: Int
fps = 60 

-- Game timer 

fullTime :: Float 
fullTime = 10