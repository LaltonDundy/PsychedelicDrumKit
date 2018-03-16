
module Sounds where
{-# LANGUAGE TemplateHaskell #-}

import Graphics.UI.SDL.General as SDL
import Control.Monad 
import Control.Concurrent.Async
import Control.Applicative
import Control.Concurrent
import Sound.ALUT


kickBuf =  "./Bass-Drum-1.wav"

playSound :: String -> IO ()
playSound file =  do buf <- createBuffer (File file);
                     source <- genObjectName;
                     buffer source $= Just buf;
                     play [source];
                       
                     
                     
                     
playKick =   
      (playSound kickBuf  )   
