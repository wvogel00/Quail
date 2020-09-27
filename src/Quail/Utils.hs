module Quail.Utils where


import System.Process (callCommand, callProcess)
import Data.List
import Data.Maybe
import Quail.Types


sampleRate = 44100

initMusicalScore = MusicalScore (Metronome 60) []
                    [(GClef,[initNote{len = (L16,[])},initNote{len = (L2,[])}])]

initNote = Note
    { index = 0
    , scale = C
    , sign = None
    , oct = 4
    , len = (L4,[])
    , str = Nothing
    , varStr = Nothing
    , tempStr = []
    , isVibrato = False
    , tie = Nothing
    , slur = Nothing
    }

playFile :: FilePath -> IO ()
playFile path = callProcess "ffplay"
    ["-f", "f32le", "-ar", show sampleRate, "-showmode", "1", path]


noteSound :: Note -> Freq
noteSound n = let r = 1.059463094
                  a0 = 440 :: Freq
              in a0/2**(fromIntegral $ 4-oct n) * r**(fromIntegral $ scale n `sub` A)

sub Rest _ = 0
sub _ Rest = 0
sub s1 s2 = f s1 - f s2
    where
    table = zip [C .. B] [-9,-8..2]
    f s = fromJust $ lookup s table

isFlip :: Note -> Bool
isFlip n = scale n >= C && oct n >= 5

soundLen :: Bar -> Float
soundLen = sum.map noteLen

noteLen :: Note -> Float
noteLen n = (getLen l) + sum (map (\v -> 1/2^v) $take (length dots) [1..])
    where
        (l,dots) = len n
        getLen l = (\v -> 1/2^v). fromJust . lookup l $ zip [Full .. ] [0,1..]


applySign :: Sign ->  Scale -> Scale
applySign None = id
applySign Natural = id
applySign Sharp = sharp
applySign DoubleSharp = sharp.sharp
applySign Flat = flat
applySign DoubleFlat = flat.flat

sharp :: Scale -> Scale
sharp Rest = Rest
sharp s = fromJust . lookup s $ zip (B:[C ..]) $ [C ..]

flat :: Scale -> Scale
flat Rest = Rest
flat s = fromJust . lookup s $ zip [C ..] $ B:[C ..]