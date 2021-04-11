module EventHandler (
    handleEvent
) where 

import Graphics.Gloss.Interface.Pure.Game 
import DataTypes
import AppConfig 
import Generator

-- Handle events

restart :: GameState -> GameState 
restart state = state{started=False, score=0, alive=True, time=fullTime}

handleEvent :: Event -> GameState -> GameState 
handleEvent (EventKey (SpecialKey key) Down _ _) state
    | not (alive state) && key == KeySpace = restart state
    | time state <= 0 || not (alive state) = state {alive = False}
    | key == KeyRight || key == KeyLeft = state { 
            score = score state + 1,
            time = min (time state + 1) fullTime,
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