module Main where

import Graphics.Gloss.Interface.Pure.Game 
import System.Random
import DataTypes
import DrawApp
import AppConfig
import ImageLoader
import Generator 
import Simulation
import EventHandler

main :: IO ()
main = do
    gen <- newStdGen 
    assets <- loadAssets
    case assets of 
        Nothing -> putStrLn "Error: can not load some images, check `assets` directory"
        Just assets -> do
            let initState = GameState {
                started=False,
                score=0, 
                alive=True, 
                time=fullTime,
                randomGen=gen, 
                nextBranches=defaultBranches, 
                curSide=LeftSide, 
                active=False,
                assets=assets
            }
            play display black fps initState drawApp handleEvent updateApp
