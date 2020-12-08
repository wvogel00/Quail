module Quail.Utils where


import System.Process (callCommand, callProcess)
import Data.List
import Data.Maybe
import Quail.Types
import Control.Lens
import System.FilePath.Posix (takeExtension)


sampleRate = 44100

initMusicalScore = MusicalScore
    { _metro = Metronome 60
    , _beatRate = (4,4)
    , _bars = [Bar { _clef = GClef
                   , _keys = []
                   , _notes = []
                  }]
    } 

initNote = Note
    { _no = 0
    , _scale = C
    , _sign = None
    , _oct = 4
    , _len = (L4,[])
    , _str = Nothing
    , _varStr = Nothing
    , _tempStr = []
    , _isVibrato = False
    , _tie = Nothing
    , _slur = Nothing
    }

playFile :: FilePath -> IO ()
playFile path = callProcess "ffplay"
    ["-f", "f32le", "-ar", show sampleRate, "-showmode", "1", path]


getFreq :: Note -> Freq
getFreq n = let r = 1.059463094
                a0 = 440 :: Freq
            in a0/2**(fromIntegral $ 4-n^.oct) * r**(fromIntegral $ (n^.scale) `sub` A)

sub Rest _ = 0
sub _ Rest = 0
sub s1 s2 = f s1 - f s2
    where
    table = zip [C .. B] [-9,-8..2]
    f s = fromJust $ lookup s table

isFlip :: Note -> Bool
isFlip n = n^.scale >= C && n^.oct >= 5

barLen :: Bar -> Float
barLen bar = sum.map noteLen $ bar^.notes

noteLen :: Note -> Float
noteLen n = (getLen l) + sum (map (\v -> 1/2^v) $take (length dots) [1..])
    where
        (l,dots) = n^.len
        getLen l = (\v -> 1/2^v). fromJust . lookup l $ zip (reverse [L32 .. ]) [0,1..]


addSharp, addFlat, shorten, lengthen :: Int -> MusicalScore -> MusicalScore
addSharp i ms = opeNote i sharp ms
addFlat  i ms = opeNote i flat ms
shorten  i ms = opeNote i shorten' ms
    where
    shorten' n = case n^.len of
        ( L32,dots) -> n&len .~ (L32,dots)
        ( L16,dots) -> n&len .~ (L32,dots)
        (  L8,dots) -> n&len .~ (L16,dots)
        (  L4,dots) -> n&len .~ ( L8,dots)
        (  L2,dots) -> n&len .~ ( L4,dots)
        (Full,dots) -> n&len .~ ( L2,dots)
lengthen i ms = opeNote i lengthen' ms
    where
    lengthen' n = case n^.len of
        ( L32,dots) -> n&len .~ ( L16,dots)
        ( L16,dots) -> n&len .~ (  L8,dots)
        (  L8,dots) -> n&len .~ (  L4,dots)
        (  L4,dots) -> n&len .~ (  L2,dots)
        (  L2,dots) -> n&len .~ (Full,dots)
        (Full,dots) -> n&len .~ (Full,dots)

addNatural :: Int -> MusicalScore -> MusicalScore
addNatural i ms = opeNote i (\n -> n&sign .~ Natural) ms

opeNote :: Int -> (Note -> Note) -> MusicalScore -> MusicalScore
opeNote i f ms = ms&bars.~ map ope (ms^.bars)
    where
    ope bar = bar&notes .~ map ope' (bar^.notes)
    ope' n = if i == n^.no then f n else n

applySign :: Sign ->  Note -> Note
applySign None = id
applySign Natural = id
applySign Sharp = sharp
applySign DoubleSharp = sharp.sharp
applySign Flat = flat
applySign DoubleFlat = flat.flat

noteCount :: MusicalScore -> Int
noteCount ms = length $ concatMap (^.notes) $ ms^.bars

sharp :: Note -> Note
sharp n = case n^.scale of
    Rest -> n
    s    -> (\a -> a&oct +~ if sharp' s == C then 1 else 0) $ n&scale.~sharp' s
    where
    sharp' s = fromJust . lookup s $ zip (B:[C ..]) $ [C ..]

flat :: Note -> Note
flat n = case n^.scale of
    Rest -> n
    s    -> (\a -> a&oct -~ if s == C then 1 else 0) $ n&scale.~flat' s
    where
    flat' s = fromJust . lookup s $ zip [C ..] $ B:[C ..]

isQuailFile :: String -> Bool
isQuailFile file = takeExtension file == ".quail"