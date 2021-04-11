module DataTypes (
    Direction (..),
    Assets (..),
    GameState (..)
) where

import Graphics.Gloss.Data.Picture
import System.Random 

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
    dead :: Picture,
    numbers :: [Picture],
    startScreen :: Picture
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