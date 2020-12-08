{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Quail.GUI where

import SDL (($=))
import qualified SDL
import qualified SDL.Font as Font
import qualified SDL.Image as I
import qualified SDL.Internal.Numbered as SDLN
import SDL.Raw.Video as V
import Linear (V4(..))
import Control.Monad (void, unless)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Extra (whileM)
import Data.Int (Int16)
import Data.Text (Text, pack)
import Data.List (lookup)
import Data.Maybe (fromJust)
import Data.IORef (newIORef, writeIORef, readIORef, IORef)
import Control.Lens
import Quail.Utils
import Quail.Audio
import Quail.Types
import System.Directory
import Debug.Trace (trace)

(width, height) = (600,480)
fontpath = "ttf/Raleway-Regular.ttf"

gray = SDL.V4 128 128 128 255
black = SDL.V4 0 0 0 255
saveScreenColor = V4 200 200 200 100
loadScreenColor = V4 220 180 180 100

withSDL :: (MonadIO m) => m a -> m ()
withSDL op = do
    SDL.initialize []
    void op
    SDL.quit

withWindow :: (MonadIO m) => Text -> (Int, Int) -> (SDL.Window -> m a) -> m ()
withWindow title (x, y) op = do
    w <- SDL.createWindow title p
    SDL.showWindow w
    void $ op w
    SDL.destroyWindow w
    where
      p = SDL.defaultWindow { SDL.windowInitialSize = z }
      z = SDL.V2 (fromIntegral x) (fromIntegral y)

rendererConfig :: SDL.RendererConfig
rendererConfig = SDL.RendererConfig
  { SDL.rendererType = SDL.AcceleratedVSyncRenderer
  , SDL.rendererTargetTexture = False
  }

startGUI :: IO ()
startGUI = do
    SDL.initializeAll
    Font.initialize
    withSDL $ withWindow "うずら" (width,height) $
        \w -> do
            sound <- newIORef []
            audioDev <- openAudio sound
            r <- SDL.createRenderer w (-1) rendererConfig
            ts <- loadNoteTextures r
            void $ renderLoop (audioDev,sound) r ts initMusicalScore
            SDL.destroyRenderer r


drawClipped r t (w,h) (w',h') (px,py) = SDL.copyEx r t
    (Just $ SDL.Rectangle (SDL.P $ SDL.V2 0 0) (SDL.V2 w h))
    (Just $ SDL.Rectangle (SDL.P $ SDL.V2 px py) (SDL.V2 w' h'))
    0
    Nothing
    (pure False) -- x,y軸方向に反転する時は使用

renderLoop audio r ts mscore = do
    staff <- I.loadTexture r "imgs/staffpaper.png"
    SDL.rendererDrawColor r $= V4 255 255 255 30
    event <- getEvent <$> SDL.pollEvent
    mscore' <- dealEvent event audio r mscore
    
    -- 楽譜描画
    SDL.clear r
    drawClipped r staff (1000,150) (500,75) (20,20)
    drawClipped r staff (1000,150) (500,75) (20,110)
    drawClipped r staff (1000,150) (500,75) (20,200)
    drawClipped r staff (1000,150) (500,75) (20,290)
    drawClipped r staff (1000,150) (500,75) (20,380)
    drawMusicalScore r ts mscore'

    -- テンポの描画
    drawText r 16 (30,10) $ pack $ show (mscore^.metro)

    -- 更新
    SDL.destroyTexture staff
    -- mapM_ (SDL.destroyTexture.(\(_,_,t) -> t)) $ ts -- 使われていないtextureがあると落ちるので要修正
    SDL.present r
    unless (event == Quit) $ renderLoop audio r ts mscore'


drawText r fontsize (x,y) text = do
    font <- Font.load fontpath fontsize
    textSurface <- Font.solid font black text
    (w,h) <- Font.size font text
    textTexture <- SDL.createTextureFromSurface r textSurface
    Font.free font
    SDL.copyEx r textTexture
        (Just $ SDL.Rectangle (SDL.P $ SDL.V2 0 0) (SDL.V2 (fromIntegral width) (fromIntegral height)))
        (Just $ SDL.Rectangle (SDL.P $ SDL.V2 x y) (SDL.V2 (fromIntegral w) (fromIntegral h)))
        0
        Nothing
        (pure False)

loadNoteTextures :: SDL.Renderer -> IO [(Scale, Length, SDL.Texture)]
loadNoteTextures r = do
    t7  <- (Rest,Full,) <$>I.loadTexture r "imgs/restfull.png"
    t8  <- (Rest,L2,) <$> I.loadTexture r "imgs/rest2.png"
    t9  <- (Rest,L4,) <$> I.loadTexture r "imgs/rest4.png"
    t10 <- (Rest,L8,) <$> I.loadTexture r "imgs/rest8.png"
    t11 <- (Rest,L16,) <$> I.loadTexture r "imgs/rest16.png"
    t1  <- (C,Full,) <$> I.loadTexture r "imgs/notefull.png"
    t2  <- (C,L2,)  <$> I.loadTexture r "imgs/note2.png"
    t3  <- (C,L4,) <$> I.loadTexture r "imgs/note4.png"
    t4  <- (C,L8,) <$> I.loadTexture r "imgs/note8.png"
    t5  <- (C,L16,) <$> I.loadTexture r "imgs/note16.png"
    t6  <- (C,L32,) <$> I.loadTexture r "imgs/note32.png"
    return [t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11]

drawMusicalScore :: SDL.Renderer -> [(Scale, Length, SDL.Texture)] -> MusicalScore -> IO ()
drawMusicalScore r ts (MusicalScore metro keys bars) = drawBars bars
    where
    drawBars :: [Bar] -> IO ()
    drawBars [] = return ()
    drawBars (bar:bars) = foldlM_ drawNotes (30,28) (bar^.notes) >> drawBars bars
    drawNotes (x,y) n = do
        SDL.copyEx r (fromJust $ findTexture n ts)
                (Just $ SDL.Rectangle (SDL.P $ SDL.V2 0 0) (SDL.V2 960 960))
                (Just $ SDL.Rectangle (SDL.P $ SDL.V2 x (posY n y)) (SDL.V2 65 65))
                0
                Nothing
                (pure False) -- x,y軸方向に反転する時は使用
        if elem (n^.scale) [CS,DS,FS,GS,AS]
            then do
                sharpImg <- I.loadTexture r "imgs/sharp.png"
                SDL.copyEx r sharpImg
                    (Just $ SDL.Rectangle (SDL.P $ SDL.V2 0 0) (SDL.V2 200 200))
                    (Just $ SDL.Rectangle (SDL.P $ SDL.V2 (x+15) (posY n y+20)) (SDL.V2 20 20))
                    0
                    Nothing
                    (pure False)
                    else return ()
        return (x+30, y)

--posY :: Note -> Int -> Int
posY n y = (-) y $ fromJust . lookup (n^.scale) $ zip [C ..] [-9,-9,-6,-6,-1,3,3, 7,7,12,12,16]
foldlM_ :: Monad m => (a -> b -> m a) -> a -> [b] -> m ()
foldlM_ f _ [] = return ()
foldlM_ f a (n:ns) = f a n >>= \a' -> foldlM_ f a' ns

findTexture :: Note -> [(Scale, Length, SDL.Texture)] -> Maybe SDL.Texture
findTexture _ [] = Nothing
findTexture n ((s,l,t):ts) = case n^.scale of
            Rest -> if fst (n^.len) == l              then Just t else findTexture n ts
            s'   -> if fst (n^.len) == l && s /= Rest then Just t else findTexture n ts


getEvent :: Maybe SDL.Event -> QuailEvent
getEvent Nothing = NotImplemented
getEvent (Just e) = case SDL.eventPayload e of
    SDL.KeyboardEvent keyEvent -> getEventType keyEvent
    SDL.MouseMotionEvent ev -> MousePos 0 0
    ev -> {-trace (show ev)-} NotImplemented


dealEvent :: QuailEvent -> (SDL.AudioDevice, IORef [Int16]) -> SDL.Renderer -> MusicalScore -> IO MusicalScore
dealEvent ev (audio,sound) r ms = case ev of
    PlaySound   -> do
        buildMusic ms sound
        playAudio audio sound >> return ms
    StopSound   -> lockAudio audio >> return ms
    ResumeSound -> resumeAudio audio >> return ms
    AddNote s   -> do
        let n = (\a -> a&no.~noteCount ms+1)$ initNote&scale .~ s
        return $ trace (show n) $ addNote n ms
    DeleteNote -> do
        let lastbar = last (ms^.bars)
            lastbar' = lastbar&notes .~ init (lastbar^.notes)
        return $ ms&bars .~ (init (ms^.bars)) ++ [lastbar']
    AddSharp -> return $ addSharp (noteCount ms) ms
    AddFlat  -> return $ addFlat  (noteCount ms) ms
    Shorten  -> return $ shorten  (noteCount ms) ms
    Lengthen -> return $ lengthen (noteCount ms) ms
    LoadEvent -> do
        loadResult <- loadLoop r
        case loadResult of
            Just loadms -> return loadms
            Nothing -> return ms
    SaveEvent -> saveLoop r [] >> return ms
    _ -> return ms


loadLoop r = do
    event <- getLoadEvent <$> SDL.pollEvent
    SDL.rendererDrawColor r $= loadScreenColor
    SDL.clear r
    drawText r 20 (100,40) "choose the filename:"
    files <- filter isQuailFile <$> getDirectoryContents "scores"
    foldlM_ (drawTextList r 20) (100,65) $ map pack files
    SDL.present r
    case event of
        QuitLoad -> return Nothing
        Load -> return.Just $ initMusicalScore -- 未実装
        LoadChoice_Up -> print "up" >> loadLoop r
        LoadChoice_Down -> print "down" >> loadLoop r
        ContinueLoad ->  loadLoop r

drawTextList r fontsize (x,y) t = do
    drawText r fontsize (x,y) t
    return (x,y+fromIntegral fontsize+5)


getLoadEvent :: Maybe SDL.Event -> LoadEvent
getLoadEvent Nothing = ContinueLoad
getLoadEvent (Just e) = case SDL.eventPayload e of
    SDL.KeyboardEvent keyEvent -> getLoadEventType keyEvent
    ev -> ContinueLoad


getLoadEventType :: SDL.KeyboardEventData -> LoadEvent
getLoadEventType event
    | catchOn event (Nothing, SDL.KeycodeReturn,  SDL.Pressed) = Load
    | catchOn event (Nothing, SDL.KeycodeEscape,  SDL.Pressed) = QuitLoad
    | catchOn event (Nothing, SDL.KeycodeUp,    SDL.Pressed) = LoadChoice_Up
    | catchOn event (Nothing, SDL.KeycodeDown,  SDL.Pressed) = LoadChoice_Down
    | otherwise = ContinueLoad


saveLoop r filename = do
    event <- getSaveEvent <$> SDL.pollEvent
    SDL.rendererDrawColor r $= saveScreenColor
    SDL.clear r
    drawText r 20 (100,40) "choose or type the filename:"
    unless (null filename) $ drawText r 20 (100,65) $ pack filename
    SDL.present r
    case event of
        QuitSave -> return ()
        Save -> do
            print $ "save file as" ++ filename ++ " (not implemented)"
            saveLoop r filename
        SaveName_Input c -> saveLoop r (filename++[c])
        SaveChoice_Up -> print "up" >> saveLoop r filename
        SaveChoice_Down -> print "down" >> saveLoop r filename
        ContinueSave -> saveLoop r filename


getSaveEvent :: Maybe SDL.Event -> SaveEvent
getSaveEvent Nothing = ContinueSave
getSaveEvent (Just e) = case SDL.eventPayload e of
    SDL.KeyboardEvent keyEvent -> getSaveEventType keyEvent
    ev -> ContinueSave


getSaveEventType :: SDL.KeyboardEventData -> SaveEvent
getSaveEventType event
    | catchOn event (Nothing, SDL.KeycodeReturn,  SDL.Pressed) = Save
    | catchOn event (Nothing, SDL.KeycodeEscape,  SDL.Pressed) = QuitSave
    | catchOn event (Nothing, SDL.KeycodeUp,    SDL.Pressed) = SaveChoice_Up
    | catchOn event (Nothing, SDL.KeycodeDown,  SDL.Pressed) = SaveChoice_Down
    | 32 <= v && v <= 122 && SDL.keyboardEventKeyMotion event == SDL.Pressed = SaveName_Input $ fromJust $ lookup v sdlKeyCodeTable
    | otherwise = ContinueSave
            where v = SDLN.toNumber (SDL.keysymKeycode (SDL.keyboardEventKeysym event))

sdlKeyCodeTable = zip ([32..64]++[91..122]) " !\"#$%&'()*+,-./0123456789:;<=>?@[\\]^_`abcdefghijklmnopqrstuvwxyz"


addNote :: Note -> MusicalScore -> MusicalScore
addNote n ms = ms&bars .~ (init (ms^.bars) ++ addNote' n (last $ ms^.bars))
    where
    addNote' :: Note -> Bar -> [Bar]
    addNote' n bar = if barLen bar + noteLen n <= 4
        then [ bar&notes .~ bar^.notes ++ [n]]
        else [bar, bar&notes .~ [n]]


getEventType :: SDL.KeyboardEventData -> QuailEvent
getEventType event
    | catchOn event (Just SDL.keyModifierLeftGUI  , SDL.KeycodeO,   SDL.Pressed) = LoadEvent
    | catchOn event (Just SDL.keyModifierLeftGUI  , SDL.KeycodeS,   SDL.Pressed) = SaveEvent
    | catchOn event (Nothing,                       SDL.KeycodeQ,   SDL.Pressed) = Quit
    | catchOn event (Nothing,                       SDL.KeycodeB,   SDL.Pressed) = AddNote B
    | catchOn event (Just SDL.keyModifierLeftShift, SDL.KeycodeA,   SDL.Pressed) = AddNote AS
    | catchOn event (Nothing,                       SDL.KeycodeA,   SDL.Pressed) = AddNote A
    | catchOn event (Just SDL.keyModifierLeftShift, SDL.KeycodeG,   SDL.Pressed) = AddNote GS
    | catchOn event (Nothing,                       SDL.KeycodeG,   SDL.Pressed) = AddNote G
    | catchOn event (Just SDL.keyModifierLeftShift, SDL.KeycodeF,   SDL.Pressed) = AddNote FS
    | catchOn event (Nothing,                       SDL.KeycodeF,   SDL.Pressed) = AddNote F
    | catchOn event (Nothing,                       SDL.KeycodeE,   SDL.Pressed) = AddNote E
    | catchOn event (Just SDL.keyModifierLeftShift, SDL.KeycodeD,   SDL.Pressed) = AddNote DS
    | catchOn event (Nothing,                       SDL.KeycodeD,   SDL.Pressed) = AddNote D
    | catchOn event (Just SDL.keyModifierLeftShift, SDL.KeycodeC,   SDL.Pressed) = AddNote CS
    | catchOn event (Nothing,                       SDL.KeycodeC,   SDL.Pressed) = AddNote C
    | catchOn event (Nothing,                       SDL.KeycodeP,   SDL.Pressed) = PlaySound
    | catchOn event (Nothing,                       SDL.KeycodeS,   SDL.Pressed) = StopSound
    | catchOn event (Nothing,                       SDL.KeycodeR,   SDL.Pressed) = ResumeSound
    | catchOn event (Nothing,                       SDL.KeycodeUp,  SDL.Pressed) = AddSharp
    | catchOn event (Nothing,                       SDL.KeycodeDown,SDL.Pressed) = AddFlat
    | catchOn event (Nothing,                       SDL.KeycodeLeft,  SDL.Pressed) = Shorten
    | catchOn event (Nothing,                       SDL.KeycodeRight,SDL.Pressed) = Lengthen
    | catchOn event (Nothing,                       SDL.KeycodeBackspace,SDL.Pressed) = DeleteNote
    | otherwise = NotImplemented

-- キーの入力検知
catchOn event (Nothing,key,mode) = SDL.keyboardEventKeyMotion event == mode
    && SDL.keysymKeycode (SDL.keyboardEventKeysym event) == key
catchOn event (Just modifierF,key,mode) = catchOn event (Nothing, key,mode) && catchOn' modifierF mode
    where
    -- 特殊キーの入力検知
    catchOn' modifierF mode = SDL.keyboardEventKeyMotion event == mode
        && modifierF (SDL.keysymModifier (SDL.keyboardEventKeysym event))


