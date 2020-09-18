module Quail.Types where

-- 楽譜
data MusicalScore = GSound [KeySignature] [Bar]
    | FSound [KeySignature] [Bar]
    deriving (Eq, Show)

-- 小節
type Bar = [Note]

-- 音符
data Note = Note
    { scale :: Scale
    , sign :: Maybe Sign
    , oct :: Octave
    , len :: (Length, [Dot])
    , keySig :: Maybe KeySignature
    , str :: Maybe Strength
    , varStr :: Maybe VariableStrength
    , tempStr :: [TempEffect]
    , isVibrato :: Bool
    , tie :: Maybe TieID
    , slur :: Maybe SlurID
    }

-- タイ・スラーの管理ID
type TieID = Int
type SlurID = Int

-- 音階
data  Scale = Rest | C | CS | D | DS | E | F | FS | G | GS | A | AS | B
    deriving (Eq, Show, Enum)

-- オクターブ
type Octave = Int

-- 周波数
type Freq = Float

-- 音符長．全音符から32分音符まで
data Length = Full | L2 | L4 | L8 | L16 | L32 deriving (Eq, Show)

-- 付点
data Dot = Dot deriving (Eq, Show)

-- 調号
type KeySignature = (Scale, Sign)

data Sign = Sharp | DoubleSharp | Flat | DoubleFlat | Natural
    deriving (Eq, Show)

data Strength =
    Pianissimo -- pp
    | Piano -- p
    | MezzoPiano -- mp
    | MessoForte -- mf
    | Forte -- f
    | Fortissimo -- ff
    | Pianississimo -- ppp
    | Pianissississimo -- pppp
    | Fortississimo -- fff
    | Fortissississimo -- ffff
    | Molto -- molto
    | Poco -- poco
    | UnPoco -- un poco
    | Piu -- piu
    | Meno -- memo
    | SottoVoce -- sotto voce
    | MezzaVoce -- mezza voce
    deriving Eq

-- 小節単位の効果
data BarEffect = Repeat | DaCapo | DalSegno | Segno
    deriving Eq

-- 変化する音の強弱
data VariableStrength = 
    Crescendo
    | Decrescendo

-- 一時的な音効果
data TempEffect = 
    Accent
    | Staccato