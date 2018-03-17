module World where
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Display





data World = World {

    kick :: Bool

    } deriving (Show, Eq) 


render :: World -> IO Picture
render w = if (not.kick $ w) then return Blank
           else return (circle 150.0)

 
