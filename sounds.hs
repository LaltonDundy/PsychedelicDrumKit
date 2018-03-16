
module Sounds where

import Control.Monad 
import Control.Applicative
import Sound.ALUT


kickBuf =  "./Bass-Drum-1.wav"

playSound :: String -> IO ()
playSound file =  do buf <- createBuffer (File file);
                     source <- genObjectName;
                     buffer source $= Just buf;
                     play [source];
                       
                     
                     
                     
playKick =   
      (playSound kickBuf  )   
