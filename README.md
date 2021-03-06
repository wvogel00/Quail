# Quail
作曲アプリケーション：うずら

## 動作環境
Mac OS Catalina 10.15.6
intel core i3, 8GB

### 操作マニュアル(2020/12/10時点)
* 音符追加
  * C,D,E,F,G,A,Bキーを押すと，それぞれに対応した音階が追加される
  * 'Shift'キーを同時に押すと半音上がる (e.g. 'Shift'+'c' = C#)
* 音高調整
　最後の音符に対してのみ有効．'↑'又は'↓'キーで半音ずつ音高が変わる
* 音長調整
　最後の音符に対してのみ有効．'←'又は'→'キーで全音符から32分音符の間で変更可能．（付点調整は未実装）
* 楽曲再生
  'p'押下で再生する．
* 楽曲停止
  's'押下で停止する．
* 保存
  'Command'+'s'で保存モードに突入する．保存するファイル名を入力する(ファイル操作は未実装)
* 読み込み
'Command'+'o'で読み込みモードに突入する．開くファイルを選択する(ファイル操作は未実装)
* テンポ調整
  'm'押下でテンポ調整モードに突入する．数字キー，'backspace'キーでテンポを入力．'Return'押下で反映される．'Esc'押下すると，テンポ調整モードに入る前の値を保持する．
* 終了
  'q'押下で終了する．


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
