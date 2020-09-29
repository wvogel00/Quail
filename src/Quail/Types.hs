{-# LANGUAGE TemplateHaskell #-}

module Quail.Types where

import Data.Text
import Control.Lens

-- タイトル，ヘッダ
data ScoreInfo = ScoreInfo Title Header
type Title = Text
type Header = Text

-- 楽譜
data MusicalScore =　MusicalScore
    { _metro :: Metronome
    , _beatRate :: (Int,Int)
    , _bars :: [Bar]
    }
    deriving (Eq, Show)

-- テンポ
data Metronome = Metronome Int
    deriving (Eq,Show)

-- 音部記号
data Clef = GClef | FClef
    deriving (Eq, Show)

-- 小節
data Bar = Bar
    { _clef :: Clef
    , _keys :: [KeySignature]
    , _notes :: [Note]
    }
    deriving (Eq, Show)

-- 音符
data Note = Note
    { _no :: Int
    , _scale :: Scale
    , _sign :: Sign
    , _oct :: Octave
    , _len :: (Length, [Dot])
    , _str :: Maybe Strength
    , _varStr :: Maybe VariableStrength
    , _tempStr :: [TempEffect]
    , _isVibrato :: Bool
    , _tie :: Maybe TieID
    , _slur :: Maybe SlurID
    }
    deriving (Eq, Show)

-- タイ・スラーの管理ID
type TieID = Int
type SlurID = Int

-- 音階
data  Scale = Rest | C | CS | D | DS | E | F | FS | G | GS | A | AS | B
    deriving (Eq, Show, Enum, Ord)

-- オクターブ
type Octave = Int

-- 周波数
type Freq = Float

-- 音符長．全音符から32分音符まで
data Length = L32 | L16 | L8 | L4 | L2 | Full deriving (Eq, Show, Enum, Ord)

-- 付点
data Dot = Dot deriving (Eq, Show)

-- 調号
type KeySignature = (Scale, Sign)

data Sign = None | Sharp | DoubleSharp | Flat | DoubleFlat | Natural
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
    deriving (Eq, Show)

-- 小節単位の効果
data BarEffect = Repeat | DaCapo | DalSegno | Segno
    deriving (Eq, Show)

-- 変化する音の強弱
data VariableStrength = 
    Crescendo
    | Decrescendo
    deriving (Eq, Show)

-- 一時的な音効果
data TempEffect = Accent | Staccato
    deriving (Eq, Show)

data QuailEvent =
    Quit
    | NotImplemented
    | AddKeySignature KeySignature
    | AddClef Clef
    | AddNote Scale
    | AddSharp
    | AddFlat
    | AddNatural
    | Extend Note
    | Shorten Note
    | AddSign Note
    | AddSlur [Note]
    | AddTie [Note]
    | PlaySound
    | StopSound
    | ResumeSound
    | MousePos Int Int
    deriving (Eq, Show)

$(makeLenses ''MusicalScore)
$(makeLenses ''Bar)
$(makeLenses ''Note)