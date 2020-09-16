module Quail where

import System.Process (callCommand, callProcess)
import Data.List (tail)

play :: FilePath -> IO ()
play path = callProcess "ffplay"
    ["-f", "f32le", "-ar", show sampleRate, "-showmode", "1", path]

save :: Music -> IO ()



toScaleOctave :: Freq -> (Scale, Octave)
toScale fq = (scale , octave) where
    r = 1.059463094
    scales = a0 * r^[-48,-47..39] -- the range of piano
    scale = findScale $ zip scales (tail scales)
    findScale (a,b) = if a <= fq && fq <= b then 

a0 = 440 :: Freq -- base sound : 440Hz (A)