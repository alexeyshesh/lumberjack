module ImageLoader (
    loadAssets
) where

import Graphics.Gloss.Data.Picture
import Control.Exception
import DataTypes
import Graphics.Gloss.Data.Bitmap

-- loadBMP Exception handling

loadImage :: FilePath -> IO (Maybe Picture)
loadImage path = do
    result <- try(loadBMP path) :: IO (Either SomeException Picture)
    case result of
        Left ex -> return Nothing
        Right pic -> return $ Just pic

loadAssets :: IO (Maybe Assets)
loadAssets = do 
    pl <- loadImage "assets/pl.bmp"
    pla <- loadImage "assets/pla.bmp"
    pr <- loadImage "assets/pr.bmp"
    pra <- loadImage "assets/pra.bmp"
    bg <- loadImage "assets/bg.bmp"
    bl <- loadImage "assets/bl.bmp"
    br <- loadImage "assets/br.bmp"
    dead <- loadImage "assets/dead.bmp"
    startScreen <- loadImage "assets/start_screen.bmp"
    n0 <- loadImage "assets/0.bmp"
    n1 <- loadImage "assets/1.bmp"
    n2 <- loadImage "assets/2.bmp"
    n3 <- loadImage "assets/3.bmp"
    n4 <- loadImage "assets/4.bmp"
    n5 <- loadImage "assets/5.bmp"
    n6 <- loadImage "assets/6.bmp"
    n7 <- loadImage "assets/7.bmp"
    n8 <- loadImage "assets/8.bmp"
    n9 <- loadImage "assets/9.bmp"

    let loaded = sequence [pl, pla, pr, pra, bg, bl, br, dead, startScreen, n0, n1, n2, n3, n4, n5, n6, n7, n8, n9]
    case loaded of 
        Nothing -> return Nothing 
        Just pics -> return $ Just Assets {
            playerLeft=pics !! 0,
            playerLeftActive=pics !! 1,
            playerRight=pics !! 2,
            playerRightActive=pics !! 3,
            background=pics !! 4,
            branchLeft=pics !! 5,
            branchRight=pics !! 6,
            dead=pics !! 7,
            startScreen=pics !! 8,
            numbers=[
                pics !! 9,
                pics !! 10,
                pics !! 11,
                pics !! 12,
                pics !! 13,
                pics !! 14,
                pics !! 15,
                pics !! 16,
                pics !! 17,
                pics !! 18
            ]
        }