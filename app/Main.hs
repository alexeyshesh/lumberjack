module Main where

import Graphics.Gloss.Interface.Pure.Game 
import Graphics.Gloss.Data.Bitmap
import System.Random 


-- Data types 

data Direction = LeftSide | RightSide | Center

data Assets = Assets {
    playerLeft :: Picture,
    playerLeftActive :: Picture,
    playerRight :: Picture,
    playerRightActive :: Picture,
    background :: Picture,
    branchLeft :: Picture,
    branchRight :: Picture
}

data GameState = GameState { 
    score :: Int,
    alive :: Bool,
    time :: Float,
    randomGen :: StdGen,
    nextBranches :: [Direction],
    curSide :: Direction,
    active :: Bool,
    assets :: Assets
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
drawApp (GameState s _ _ _ branches side active assets) = Pictures [bg, player, brs]
    where 
        bg = background assets
        player = drawPlayer side active assets
        brs = drawBranches branches assets

drawPlayer :: Direction -> Bool -> Assets -> Picture 
drawPlayer LeftSide False = playerLeft 
drawPlayer LeftSide True = playerLeftActive 
drawPlayer RightSide False = playerRight 
drawPlayer RightSide True = playerRightActive 

drawBranches :: [Direction] -> Assets -> Picture 
drawBranches dir assets = Pictures(drawBranchesHelper 0.0 dir assets)

drawBranchesHelper :: Float -> [Direction] -> Assets -> [Picture]
drawBranchesHelper _ [] _ = []
drawBranchesHelper shift (LeftSide:dir) assets = 
    Translate 0 shift (branchLeft assets) : drawBranchesHelper (shift + 60) dir assets
drawBranchesHelper shift (RightSide:dir) assets = 
    Translate 0 shift (branchRight assets) : drawBranchesHelper (shift + 60) dir assets
drawBranchesHelper shift (Center:dir) assets = 
    drawBranchesHelper (shift + 60) dir assets

drawScore :: Int -> Assets -> Picture 
drawScore = undefined

drawTime :: Float -> Picture 
drawTime = undefined

-- World generator 

defaultBranches :: [Direction]
defaultBranches = [Center, LeftSide, Center, RightSide, Center, RightSide]

branchGenerator :: [Direction] -> StdGen -> ([Direction], StdGen)
branchGenerator [] gen = (defaultBranches, gen)
branchGenerator (Center:xs) gen = (xs ++ [Center], gen)
branchGenerator x gen = (tail x ++ [newBranch], newgen) 
    where 
        (dir, newgen) = randomR (0, 1) gen
        newBranch = [LeftSide, RightSide] !! dir

-- Handle events

handleEvent :: Event -> GameState -> GameState 
handleEvent (EventKey (SpecialKey KeyRight) Down _ _) state = 
    state { 
        curSide = RightSide, 
        active=True, 
        nextBranches=fst newBranches,
        randomGen =snd newBranches
    }
    where 
        newBranches = branchGenerator (nextBranches state) (randomGen state)
handleEvent (EventKey (SpecialKey _) Up _ _) state = 
    state {active=False}
handleEvent (EventKey (SpecialKey KeyLeft) Down _ _) state =
    state { 
        curSide = LeftSide, 
        active=True,
        nextBranches=fst newBranches,
        randomGen =snd newBranches
    }
    where 
        newBranches = branchGenerator (nextBranches state) (randomGen state)
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
    bl <- loadBMP "assets/bl.bmp"
    br <- loadBMP "assets/br.BMP"

    let initState = GameState {
        score=0, 
        alive=True, 
        time = 20,
        randomGen=gen, 
        nextBranches=defaultBranches, 
        curSide=LeftSide, 
        active=False,
        assets = Assets {
            playerLeft=pl, 
            playerLeftActive=pla,
            playerRight=pr,
            playerRightActive=pra,
            background=bg,
            branchLeft=bl,
            branchRight=br
        }
    }
    play display black fps initState drawApp handleEvent updateApp
