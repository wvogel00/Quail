# Quail

##作曲アプリケーション：うずら

## 音符型
音符型にほとんど全ての情報が載っている．
```haskell
data Note = Note
    { index :: Int                      -- 管理番号
    , scale :: Scale                    -- 音階
    , sign :: Sign                      -- #,♭,♮記号
    , oct :: Octave                     -- オクターブ情報をもつ
    , len :: (Length, [Dot])            -- 音符の長さ．基本長+半長
    , str :: Maybe Strength             -- 強弱情報
    , varStr :: Maybe VariableStrength  -- 時間変化する強弱情報
    , tempStr :: [TempEffect]           -- 一時的な強弱情報
    , isVibrato :: Bool                 -- ビブラート情報
    , tie :: Maybe TieID                --　タイ
    , slur :: Maybe SlurID              -- スラー
    }
    deriving (Eq, Show)
```
描画は動的に行われるため，描画位置は音符型には含まれない．

## 調号と臨時記号
調号は，オクターブが異なるすべての音階の音に作用する．
臨時記号は，指定した音階，オクターブの音にのみ作用する．
また，小節をまたがっていても，タイで結ばれている音は，タイが切れるまで臨時記号の効果が及ぶ

## #と♭の扱い
型定義の通り，内部的に音符を扱う際にはすべて#で扱う．
描画の際にのみ使い分けが発生し，一つ前の音よりも高い場合は#，低い場合は♭とする．また和音の場合，基本は三度差という決まりがあるため，これについても取り扱えるようにする必要がある．

## setup
```
brew install sdl2
brew install sdl2_image
brew install sdl2_mixer
brew install sdl2_gfx
brew install sdl2_ttf
```
