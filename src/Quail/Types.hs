module Quail.Types where

data Music = GSound [Note] | FSound [Note]
    deriving (Eq, Show)

data Note = Note Scale Longth

type Octave = Int

data  Scale = C | CS | D | DS | E | F | FS | G | GS | A | AS | B
    deriving (Eq, Show, Enum)

type Freq = Float