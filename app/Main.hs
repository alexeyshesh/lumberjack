module Main where

import Graphics.Gloss.Interface.Pure.Game 
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Data.Picture
import System.Random 


-- Data types 

data Direction = LeftSide | 
                 RightSide | 
                 Center
    deriving Eq

data Assets = Assets {
    playerLeft :: Picture,
    playerLeftActive :: Picture,
    playerRight :: Picture,
    playerRightActive :: Picture,
    background :: Picture,
    branchLeft :: Picture,
    branchRight :: Picture,
    dead :: Picture 
}

data GameState = GameState { 
    started :: Bool,
    score :: Int,
    alive :: Bool,
    time :: Float,
    randomGen :: StdGen,
    nextBranches :: [Direction],
    curSide :: Direction,
    active :: Bool,
    assets :: Assets
}

restart :: GameState -> GameState 
restart state = state{started=False, score=0, alive=True, time=fullTime}

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

timelineHeight :: Float 
timelineHeight = 10

branchOffset :: Float 
branchOffset = 60

drawApp :: GameState -> Picture 
drawApp (GameState _ s alive timeLeft _ branches side active assets) = Pictures [bg, brs, player, timeline]
    where 
        bg = background assets
        player = drawPlayer alive side active assets
        brs = drawBranches branches assets
        timeline = drawTime timeLeft

drawPlayer :: Bool -> Direction -> Bool -> Assets -> Picture 
drawPlayer False _ _ = dead
drawPlayer _ LeftSide False = playerLeft 
drawPlayer _ LeftSide True = playerLeftActive 
drawPlayer _ RightSide False = playerRight 
drawPlayer _ RightSide True = playerRightActive 

drawBranches :: [Direction] -> Assets -> Picture 
drawBranches dir assets = Pictures(drawBranchesHelper 0.0 dir assets)

drawBranchesHelper :: Float -> [Direction] -> Assets -> [Picture]
drawBranchesHelper _ [] _ = []
drawBranchesHelper shift (LeftSide:dir) assets = 
    Translate 0 shift (branchLeft assets) : drawBranchesHelper (shift + branchOffset) dir assets
drawBranchesHelper shift (RightSide:dir) assets = 
    Translate 0 shift (branchRight assets) : drawBranchesHelper (shift + branchOffset) dir assets
drawBranchesHelper shift (Center:dir) assets = 
    drawBranchesHelper (shift + 60) dir assets

drawScore :: Int -> Assets -> Picture 
drawScore = undefined

drawTime :: Float -> Picture 
drawTime timeLeft = Pictures [bg, timeLeftLine]
    where
        width = fromIntegral winWidth * (timeLeft / fullTime)
        verticalOffset = fromIntegral winHeight / 2 - timelineHeight / 2
        horizontalOffset = - fromIntegral winWidth / 2 + width / 2
        bg = Translate 0 verticalOffset $ color black $ rectangleSolid (fromIntegral winWidth) timelineHeight
        timeLeftLine = Translate horizontalOffset verticalOffset $ color green $ rectangleSolid width timelineHeight

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
fullTime :: Float 
fullTime = 10

handleEvent :: Event -> GameState -> GameState 
handleEvent (EventKey (SpecialKey key) Down _ _) state 
    | not (alive state) && key == KeySpace = restart state
    | time state <= 0 || not (alive state) = state {alive = False}
    | key == KeyRight || key == KeyLeft = state { 
            score = score state + 1,
            time = minimum [time state + 1, fullTime],
            started=True,
            curSide=newSide, 
            active=True, 
            nextBranches=fst newBranches,
            randomGen=snd newBranches,
            alive=head (nextBranches state) /= newSide
        }
    where 
        newSide = if key == KeyRight then RightSide else LeftSide
        newBranches = branchGenerator (nextBranches state) (randomGen state)
handleEvent (EventKey (SpecialKey _) Up _ _) state = 
    state {active=False}
handleEvent _ state = state

-- Simulation step

speedCoef :: Int -> Float
speedCoef score = 1 + fromIntegral score / 50

updateApp :: Float -> GameState -> GameState
updateApp n state 
    | alive state && started state = state {
        time = time state - delta,
        alive = alive state && time state - delta > 0
    } 
    | otherwise = state
        where 
            delta = n * speedCoef (score state)


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
    dead <- loadBMP "assets/dead.bmp"

    let initState = GameState {
        started=False,
        score=0, 
        alive=True, 
        time = fullTime,
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
            branchRight=br,
            dead=dead
        }
    }
    play display black fps initState drawApp handleEvent updateApp
