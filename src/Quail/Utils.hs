module Quail.Utils where

import Quail.Types
import Data.List
import Data.Maybe

applySign :: Sign ->  Scale -> Scale
applySign None = id
applySign Natural = id
applySign Sharp = sharp
applySign DoubleSharp = sharp.sharp
applySign Flat = flat
applySign DoubleFlat = flat.flat

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

noteLen :: Note -> Float
noteLen n = (getLen l) + sum (map (\v -> 1/2^v) $take (length dots) [1..])
    where
        (l,dots) = len n

getLen :: Length -> Float
getLen l = (\v -> 1/2^v). fromJust . lookup l $ zip [Full .. ] [0,1..]

