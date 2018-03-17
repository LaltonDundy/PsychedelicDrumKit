module Sounds where

import Sound.ALUT


playSound :: String -> IO ()
playSound file =  do buf <- createBuffer (File file);
                     source <- genObjectName;
                     buffer source $= Just buf;
                     play [source];
                       
                     
playKick =  playSound "./Bass-Drum-1.wav"
