module Objects where

import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Display

button = Polygon $ [ (2,1), (-2,1), (-2,-1), (2, -1) ]

pauseButton = scale 20.0 20.0 button 



