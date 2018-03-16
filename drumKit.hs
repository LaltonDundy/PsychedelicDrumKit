import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Display
import World
import Sounds
import Control.Concurrent.Async
import Sound.ALUT
import Control.Parallel 
import Control.Concurrent
main = withProgNameAndArgs runALUT $ \progName args -> do run
             where run =         
                        playIO 
                        (InWindow "test" (1000,1000) (100,100) )
                        (makeColorI 255 255 255 255)
                        30
                        myWorld
                        (\w ->( return blank))
                        events
                        (\tm w -> return w) 
                        where 
                           myWorld = World.World { val = 0 }
                           events (EventMotion _) w = return w
                           events (EventResize _) w = return w
                           events (EventKey k s _ _) w
                               | k == (Char 'w') && (s == Down)     = playKick *>  return w
                               |otherwise                          = return w
        
          
