
module Sounds where
{-# LANGUAGE TemplateHaskell #-}

import Graphics.UI.SDL.General as SDL
import Control.Monad 
import Control.Concurrent.Async
import Control.Applicative
import Control.Concurrent
import Graphics.UI.SDL.Mixer
kickBuf =  "./Bass-Drum-1.wav"

playSound file = do  SDL.init [SDL.InitAudio]
                     openAudio 44100 AudioS16LSB 2 4096
                     allocateChannels 16
                     wave <- loadWAV file
                     playChannel (-1) wave 0
playKick =   
      (playSound kickBuf  )   
