{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Quail.Types where

import Data.Text ( Text )
import Control.Lens
import Data.Aeson
import GHC.Generics (Generic)

jsonOpts :: Options
jsonOpts = defaultOptions { 
    fieldLabelModifier = dropWhile (== '_')
  , tagSingleConstructors = True
}

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
    deriving (Eq, Show, Generic)

instance FromJSON MusicalScore where
    parseJSON = genericParseJSON jsonOpts
instance ToJSON MusicalScore where
    toEncoding = genericToEncoding jsonOpts

-- テンポ
data Metronome = Metronome Int
    deriving (Eq,Show,Generic)

instance FromJSON Metronome where
    parseJSON = genericParseJSON jsonOpts
instance ToJSON Metronome where
    toEncoding = genericToEncoding jsonOpts

-- 音部記号
data Clef = GClef | FClef
    deriving (Eq, Show, Generic)

instance FromJSON Clef where
    parseJSON = genericParseJSON jsonOpts
instance ToJSON Clef where
    toEncoding = genericToEncoding jsonOpts

-- 小節
data Bar = Bar
    { _clef :: Clef
    , _keys :: [KeySignature]
    , _notes :: [Note]
    }
    deriving (Eq, Show, Generic)

instance FromJSON Bar where
    parseJSON = genericParseJSON jsonOpts
instance ToJSON Bar where
    toEncoding = genericToEncoding jsonOpts

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
    deriving (Eq, Show, Generic)

instance FromJSON Note where
    parseJSON = genericParseJSON jsonOpts
instance ToJSON Note where
    toEncoding = genericToEncoding jsonOpts

-- タイ・スラーの管理ID
type TieID = Int
type SlurID = Int

-- 音階
data  Scale = Rest | C | CS | D | DS | E | F | FS | G | GS | A | AS | B
    deriving (Eq, Show, Enum, Ord, Generic)

instance FromJSON Scale where
    parseJSON = genericParseJSON jsonOpts
instance ToJSON Scale where
    toEncoding = genericToEncoding jsonOpts


-- オクターブ
type Octave = Int

-- 周波数
type Freq = Float

-- 音符長．全音符から32分音符まで
data Length = L32 | L16 | L8 | L4 | L2 | Full deriving (Eq, Show, Enum, Ord, Generic)

instance FromJSON Length where
    parseJSON = genericParseJSON jsonOpts
instance ToJSON Length where
    toEncoding = genericToEncoding jsonOpts

-- 付点
data Dot = Dot deriving (Eq, Show, Generic)

instance FromJSON Dot where
    parseJSON = genericParseJSON jsonOpts
instance ToJSON Dot where
    toEncoding = genericToEncoding jsonOpts

-- 調号
type KeySignature = (Scale, Sign)

data Sign = None | Sharp | DoubleSharp | Flat | DoubleFlat | Natural
    deriving (Eq, Show, Generic)

instance FromJSON Sign where
    parseJSON = genericParseJSON jsonOpts
instance ToJSON Sign where
    toEncoding = genericToEncoding jsonOpts

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
    deriving (Eq, Show, Generic)

instance FromJSON Strength where
    parseJSON = genericParseJSON jsonOpts
instance ToJSON Strength where
    toEncoding = genericToEncoding jsonOpts

-- 小節単位の効果
data BarEffect = Repeat | DaCapo | DalSegno | Segno
    deriving (Eq, Show)

-- 変化する音の強弱
data VariableStrength = 
    Crescendo
    | Decrescendo
    deriving (Eq, Show, Generic)

instance FromJSON VariableStrength where
    parseJSON = genericParseJSON jsonOpts
instance ToJSON VariableStrength where
    toEncoding = genericToEncoding jsonOpts

-- 一時的な音効果
data TempEffect = Accent | Staccato
    deriving (Eq, Show, Generic)

instance FromJSON TempEffect where
    parseJSON = genericParseJSON jsonOpts
instance ToJSON TempEffect where
    toEncoding = genericToEncoding jsonOpts

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