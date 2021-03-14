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
    active :: Bool,
    playerLeft :: Picture,
    playerLeftActive :: Picture,
    playerRight :: Picture,
    playerRightActive :: Picture,
    background :: Picture
}

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

-- Draw world

drawApp :: GameState -> Picture 
drawApp (GameState s _ _ _ LeftSide False pl pla pr pra bg) = Pictures [bg, pl]
drawApp (GameState s _ _ _ RightSide False pl pla pr pra bg) = Pictures [bg, pr]
drawApp (GameState s _ _ _ LeftSide True pl pla pr pra bg) = Pictures [bg, pla]
drawApp (GameState s _ _ _ RightSide True pl pla pr pra bg) = Pictures [bg, pra]


-- Handle events

handleEvent :: Event -> GameState -> GameState 
handleEvent (EventKey (SpecialKey KeyRight) Down _ _) state = 
    state { curSide = RightSide, active=True}
handleEvent (EventKey (SpecialKey _) Up _ _) state = 
    state {active=False}
handleEvent (EventKey (SpecialKey KeyLeft) Down _ _) state =
    state { curSide = LeftSide, active=True}
handleEvent _ state = state

-- Simulation step

updateApp :: Float -> GameState -> GameState
updateApp _ x = x


main :: IO ()
main = do
    gen <- newStdGen 
    pl <- loadBMP "assets/pl.bmp"
    pla <- loadBMP "assets/pla.bmp"
    pr <- loadBMP "assets/pr.bmp"
    pra <- loadBMP "assets/pra.bmp"
    bg <- loadBMP "assets/bg.bmp"

    let initState = GameState {
        score=0, 
        alive=True, 
        randomGen=gen, 
        nextBranches=[], 
        curSide=LeftSide, 
        active=False,
        playerLeft=pl, 
        playerLeftActive=pla,
        playerRight=pr,
        playerRightActive=pra,
        background=bg
    }
    play display (makeColorI 72 219 251 255) fps initState drawApp handleEvent updateApp
