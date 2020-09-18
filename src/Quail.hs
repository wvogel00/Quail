module Quail where

import System.Process (callCommand, callProcess)
import Data.List (tail)
import Quail.Types
import Quail.Utils

sampleRate = 44100

playFile :: FilePath -> IO ()
playFile path = callProcess "ffplay"
    ["-f", "f32le", "-ar", show sampleRate, "-showmode", "1", path]

playScore :: MusicalScore -> IO ()
playScore ms = return ()

a0 = 440 :: Freq -- base sound : 440Hz (A)