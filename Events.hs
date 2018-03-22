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
    
    First  _ ->  keyBoard k
               where keyBoard v 
                       | v == (Char 'p')                     = return w {
                                                                  isPaused = True,
                                                                  state = Second $ World.Pause {resumePressed = False}       
                                                                        }


                       | v == (Char 'w')                    = if  (s == Down) && (not.isPaused $ w ) then 
                                                                  (playKick *> 
                                                                   return w {
                                                                   state = First $  World.Play {kick = True}
                                                                             } ) 
                                                              else return w { state = First $ World.Play {kick = False} } 

                       |otherwise                            = return w 

    Second _ -> return w
                                                     

