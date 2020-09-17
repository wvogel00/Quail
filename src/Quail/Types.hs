module Quail.Types where

-- 楽譜
data MusicalScore = GSound [KeySignature] [Bar]
    | FSound [KeySignature] [Bar]
    deriving (Eq, Show)

-- 小節
type Bar = [Note]

-- 音符
data Note = Note Scale (Maybe Sign) Octave Longth [Dot]

-- 音階
data  Scale = Rest | C | CS | D | DS | E | F | FS | G | GS | A | AS | B
    deriving (Eq, Show, Enum)

-- オクターブ
type Octave = Int

-- 周波数
type Freq = Float

-- 音符長．全音符から32分音符まで
data Longth = Full | L2 | L4 | L8 | L16 | L32 deriving (Eq, Show)

-- 付点
data Dot = Dot deriving (Eq, Show)

-- 調号
type KeySignature = (Scale, Sign)

data Sign = Sharp | DoubleSharp | Flat | DoubleFlat | Natural
    deriving (Eq, Show)