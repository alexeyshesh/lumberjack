module Main where

import Graphics.Gloss.Interface.Pure.Game 
import Graphics.Gloss.Data.Bitmap
import System.Random 

-- Data types

data Direction = LeftSide | RightSide

data GameState = GameState { 
    score :: Int,
    alive :: Bool,
    randomGen :: StdGen,
    nextBranches :: [Direction],
    curSide :: Direction,
    player :: Picture 
}

-- Game Display Mode

display :: Display 
display = InWindow "Lumberjack" (400, 600) (500, 500) 

-- FPS

fps :: Int
fps = 60 

-- Draw world

drawApp :: GameState -> Picture 
drawApp (GameState s _ _ _ _ img) = img

-- Handle events

handleEvent :: Event -> GameState -> GameState 
handleEvent (EventKey (SpecialKey KeyRight) Down _ _) state = 
    state { score = (score state) + 1}
handleEvent (EventKey (SpecialKey KeyLeft) Down _ _) state =
    state {score = (score state) - 1}
handleEvent _ state = state

-- Simulation step

updateApp :: Float -> GameState -> GameState
updateApp _ x = x


main :: IO ()
main = do
    gen <- newStdGen 
    player <- loadBMP "assets/player.bmp"
    let initState = GameState {score=0, alive=True, randomGen=gen, nextBranches=[], curSide=LeftSide, player=player}
    play display red fps initState drawApp handleEvent updateApp
