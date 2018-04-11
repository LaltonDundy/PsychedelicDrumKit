module Events (events) where

import Sounds
import World
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Display
import Control.Monad
import Control.Applicative 
 
events (EventMotion _) w = return w
events (EventResize _) w = return w

events (EventKey k s _ _) w = 

    case (state w) of
    
    First  p ->  keyBoard k
               where keyBoard v 

                       --Pause the app when 'p' is pressed
                       | v == (Char 'p')  = 

                          return w {
                             isPaused = True,
                             state    = Second $ World.Pause {resumePressed = False}       
                                    }

                       --Play Kick sound and change state when 'w' is pressed
                       | v == (Char 'w') =

                            if  (s == Down) then (
                               playKick *> return w  {
                               state = First $ p { kick = True } 
                                                      }                 ) 
                        --Return back to normal state when button is lifted
                            else return w { state = First $ p { kick = False } }


                       --Play Snare sound and change state when 'd' is pressed
                       | v == (Char 'd') =

                            if  (s == Down) then (
                               playSnare *> return w  {
                               state = First $ p { snare = True }
                                                      }                 ) 

                        --Return back to normal state when button is lifted
                            else return w { state = First $ p {snare = False}}



                       |otherwise         
                            = return w 

    Second _ -> return w
                                                     

