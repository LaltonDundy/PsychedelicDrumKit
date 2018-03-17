import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Display
import World
import Sounds
import Sound.ALUT


main = withProgNameAndArgs runALUT $ \progName args -> do run --intitialize any state that needs be

             where run =         
                        playIO 
                        (InWindow "test" (1000,1000) (100,100) ) -- Display
                        (makeColorI 255 255 255 255)             -- Default Color (White)
                        30                                       -- Frames per second
                        myWorld                                  -- initial world state
                        World.render                             -- project/render world into IO
                        events                                   -- handle world state in regards to events
                        (\tm w -> return w)                      -- handle world state in regards to time
                        where 
                           myWorld = World.World { kick = False }

                           events (EventMotion _) w = return w
                           events (EventResize _) w = return w
                           events (EventKey k s _ _) w
                               | k == (Char 'w') && (s == Down)     = do playKick  
                                                                         return w{kick = True}
                               |otherwise                          = return w{kick = False}
        
          
