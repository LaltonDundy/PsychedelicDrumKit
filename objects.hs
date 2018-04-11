module Objects where

import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Display
import Graphics.Gloss.Data.Bitmap

button =
     Polygon $ [ (2,1), (-2,1), (-2,-1), (2, -1) ]

pauseButton =
     scale 20.0 20.0 (color red  button) 

pepe :: IO [Picture]
pepe = 
  fmap (\x -> [x]) $  loadBMP "./BMPfiles/clip.bmp" 



