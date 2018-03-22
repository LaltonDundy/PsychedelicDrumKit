module World where

import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Display
import Objects


data Pause  = Pause  {
              resumePressed :: Bool
                     } deriving (Show, Eq) 


data Play = Play {
               kick :: Bool
                       } deriving (Show, Eq) 


data GameState = First Play | Second Pause  deriving (Show, Eq) 



data World = World {

    isPaused :: Bool, 

    state :: GameState

    } deriving (Show, Eq) 


render :: World -> IO Picture
render w =
    case (state w) of
        
        First s     -> if (not.kick $ s) then return blank
                       else return (Circle 50.0)

        Second s    -> return pauseButton 

 
