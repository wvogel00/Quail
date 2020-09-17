module Quail where

import System.Process (callCommand, callProcess)
import Data.List (tail)

play :: FilePath -> IO ()
play path = callProcess "ffplay"
    ["-f", "f32le", "-ar", show sampleRate, "-showmode", "1", path]

-- save :: Music -> IO ()



toScaleOctave :: Freq -> (Scale, Octave)
toScale fq = (scale , octave) where
    r = 1.059463094
    scales = a0 * r^[-48,-47..39] -- the range of piano
    scale = findScale $ zip scales (tail scales)
    findScale (a,b) = if a <= fq && fq <= b then 

a0 = 440 :: Freq -- base sound : 440Hz (A)

toFreq :: Note -> Freq
toFreq n = 

sharp :: Scale -> Scale
sharp Rest = Rest
sharp C = CS
sharp CS = D
sharp D = DS
sharp DS = E
sharp E = F
sharp F = FS
sharp FS = G
sharp G = GS
sharp GS = A
sharp A = AS
sharp AS = B
sharp B = C

-- double sharp
sharp2 = sharp.sharp

flat :: Scale -> Scale
flat Rest = Rest
flat C = B
flat CS = C
flat D = CS
flat DS = D
flat E = DS
flat F = E
flat FS = F
flat G = FS
flat GS = G
flat A = GS
flat AS = A
flat B = AS

flat2 = flat.flat

len :: Longth -> [Dot] -> Float
len l ds = len' l ++ sum (map (\v -> 1/2^v) $take (length ds) [1..])
