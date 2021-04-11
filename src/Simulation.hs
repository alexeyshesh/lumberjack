module Simulation (
    updateApp
) where 

import DataTypes

-- Simulation step

speedCoef :: Int -> Float
speedCoef score = 1 + fromIntegral score / 40

updateApp :: Float -> GameState -> GameState
updateApp n state 
    | alive state && started state = state {
        time = time state - delta,
        alive = alive state && time state - delta > 0
    } 
    | otherwise = state
    where 
        delta = n * speedCoef (score state)