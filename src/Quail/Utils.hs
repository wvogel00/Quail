module Quail.Utils where


import System.Process (callCommand, callProcess)
import Data.List
import Data.Maybe
import Quail.Types


sampleRate = 44100

initMusicalScore = MusicalScore
    { metro = Metronome 60
    , beatRate = (4,4)
    , bars = [Bar { clef = GClef
                  , keys = []
                  , notes = [initNote{index = 0, len = (L32,[])}
                            ,initNote{index = 1, len = ( L2,[])}]
                  }]
    } 

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

barLen :: Bar -> Float
barLen bar = sum.map noteLen $ notes bar

noteLen :: Note -> Float
noteLen n = (getLen l) + sum (map (\v -> 1/2^v) $take (length dots) [1..])
    where
        (l,dots) = len n
        getLen l = (\v -> 1/2^v). fromJust . lookup l $ zip (reverse [L32 .. ]) [0,1..]


addSharp :: Int -> MusicalScore -> MusicalScore
addSharp i ms = opeNote i sharp ms

addFlat :: Int -> MusicalScore -> MusicalScore
addFlat  i ms = opeNote i flat ms

addNatural :: Int -> MusicalScore -> MusicalScore
addNatural i ms = opeNote i (\n -> n{sign = Natural}) ms

opeNote :: Int -> (Note -> Note) -> MusicalScore -> MusicalScore
opeNote i f ms = ms{bars = map ope (bars ms)}
    where
    ope bar = bar{notes = map ope' (notes bar)}
    ope' n = if i == index n then f n else n

applySign :: Sign ->  Note -> Note
applySign None = id
applySign Natural = id
applySign Sharp = sharp
applySign DoubleSharp = sharp.sharp
applySign Flat = flat
applySign DoubleFlat = flat.flat

sharp :: Note -> Note
sharp n = case scale n of
    Rest -> n
    s    -> n {scale = sharp' s, oct = oct n + if sharp' s == C then 1 else 0}
    where
    sharp' s = fromJust . lookup s $ zip (B:[C ..]) $ [C ..]

flat :: Note -> Note
flat n = case scale n of
    Rest -> n
    s    -> n {scale = flat' s, oct = oct n - if s == C then 1 else 0}
    where
    flat' s = fromJust . lookup s $ zip [C ..] $ B:[C ..]