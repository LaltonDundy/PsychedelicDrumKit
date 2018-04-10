module World where

import Control.Applicative
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Display
import Objects


data Pause  = Pause  {

     resumePressed :: Bool

     } deriving (Show, Eq) 


data Play = Play {

     kick  :: Bool,

     snare :: Bool

     } deriving (Show, Eq) 


data GameState =
    First Play | Second Pause  deriving (Show, Eq) 



data World = World {

    isPaused :: Bool, 

    state    :: GameState

    } deriving (Show, Eq) 


render :: World -> IO Picture
render w =
    case (state w) of
        
        First s     -> display s

        Second s    -> return pauseButton 


     where display p  =

            let pics =
                      (if (kick p) then 
                       fmap (\l ->  (Circle 70.0) : l)
                       else id ) .

                      (if (snare p) then 
                        (\l ->  (++) <$> pepe <*> l)
                        else id ) 
                    in
            pictures <$>  ( pics $  return [] )
                   
              
                             
            
