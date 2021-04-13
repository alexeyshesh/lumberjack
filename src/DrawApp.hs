module DrawApp (
    drawApp
) where

import Graphics.Gloss.Data.Picture
import AppConfig 
import DataTypes 
import Graphics.Gloss.Interface.Pure.Game 

-- Draw world

timelineHeight :: Float 
timelineHeight = 10

branchOffset :: Float 
branchOffset = 60

drawApp :: GameState -> Picture 
drawApp (GameState started s alive timeLeft _ branches side active assets) = Pictures [bg, brs, player, timeline, scoreImg]
    where 
        bg = background assets
        player = drawPlayer started alive side active assets
        brs = drawBranches branches assets
        timeline = drawTime timeLeft
        scoreImg = drawScore s assets

drawPlayer :: Bool -> Bool -> Direction -> Bool -> Assets -> Picture 
drawPlayer _ False _ _ assets = dead assets
drawPlayer False _ _ _ assets = Pictures [startScreen assets, playerRight assets]
drawPlayer _ _ LeftSide False assets = playerLeft assets
drawPlayer _ _ LeftSide True assets = playerLeftActive assets
drawPlayer _ _ RightSide False assets = playerRight assets
drawPlayer _ _ RightSide True assets = playerRightActive assets

drawBranches :: [Direction] -> Assets -> Picture 
drawBranches dir assets = Pictures(drawBranchesHelper 0.0 dir assets)

drawBranchesHelper :: Float -> [Direction] -> Assets -> [Picture]
drawBranchesHelper _ [] _ = []
drawBranchesHelper shift (LeftSide:dir) assets = 
    Translate 0 shift (branchLeft assets) : drawBranchesHelper (shift + branchOffset) dir assets
drawBranchesHelper shift (RightSide:dir) assets = 
    Translate 0 shift (branchRight assets) : drawBranchesHelper (shift + branchOffset) dir assets
drawBranchesHelper shift (Center:dir) assets = 
    drawBranchesHelper (shift + branchOffset) dir assets

numberOffset :: Float 
numberOffset = 40

drawScore :: Int -> Assets -> Picture 
drawScore score assets = Pictures(drawScoreHelper 0 (toDigits score) assets)

drawScoreHelper :: Float -> [Int] -> Assets -> [Picture]
drawScoreHelper _ [] _ = []
drawScoreHelper shift (n:ntail) assets = 
    Translate shift (-10) (numbers assets !! n) : drawScoreHelper (shift + numberOffset) ntail assets

toDigits :: Int -> [Int]
toDigits 0 = []
toDigits n = toDigits(n `div` 10) ++ [n `mod` 10]

timeColor :: Float -> Color 
timeColor time 
    | time > 5 = green 
    | otherwise = red

drawTime :: Float -> Picture 
drawTime timeLeft = Pictures [bg, timeLeftLine]
    where
        width = fromIntegral winWidth * (timeLeft / fullTime)
        verticalOffset = fromIntegral winHeight / 2 - timelineHeight / 2
        horizontalOffset = - fromIntegral winWidth / 2 + width / 2
        bg = Translate 0 verticalOffset $ color black $ rectangleSolid (fromIntegral winWidth) timelineHeight
        timeLeftLine = Translate horizontalOffset verticalOffset $ color (timeColor timeLeft) $ rectangleSolid width timelineHeight
