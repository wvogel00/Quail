{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Quail.GUI where

import SDL (($=))
import qualified SDL
import qualified SDL.Image as I
import SDL.Raw.Video as V
import Linear (V4(..))
import Control.Monad (void, unless)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Extra (whileM)
import Data.Text (Text)
import Data.IORef (newIORef)
import Quail.Types
import Quail.Audio
import Debug.Trace (trace)

(width, height) = (600,480)

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
    withSDL $ withWindow "うずら" (width,height) $
        \w -> do
            sampleSound <- newIORef sinSamples
            audio <- openAudio sampleSound
            r <- SDL.createRenderer w (-1) rendererConfig
            ts <- loadNoteTextures r
            void $ renderLoop audio r ts initMusicalScore
            SDL.destroyRenderer r

initMusicalScore = MusicalScore [] [(GClef,[Note{scale=C, len = (L16,[])},Note{scale=C, len = (L2,[])}])]
initNote = Note
    { index = 0
    , scale = C
    , sign = None
    , oct = 4
    , len = (L4,[])
    , str = Nothing
    , varStr = Nothing
    , tempStr = []
    , isVibrato = False
    , tie = Nothing
    , slur = Nothing
    }

drawClipped r t (w,h) (w',h') (px,py) = SDL.copyEx r t
    (Just $ SDL.Rectangle (SDL.P $ SDL.V2 0 0) (SDL.V2 w h))
    (Just $ SDL.Rectangle (SDL.P $ SDL.V2 px py) (SDL.V2 w' h'))
    0
    Nothing
    (pure False) -- x,y軸方向に反転する時は使用

renderLoop audio r ts mscore = do
    staff <- I.loadTexture r "imgs/staffpaper.png"
    SDL.rendererDrawColor r $= V4 200 200 200 30
    event <- getEvent <$> SDL.pollEvent
    mscore' <- dealEvent event audio r mscore
    -- draw objects
    SDL.clear r
    drawClipped r staff (900,150) (450,75) (20,20)
    drawMusicalScore r ts mscore'

    SDL.destroyTexture staff
    -- mapM_ (SDL.destroyTexture.(\(_,_,t) -> t)) $ ts -- 使われていないtextureがあると落ちるので要修正
    SDL.present r
    unless (event == Quit) $ renderLoop audio r ts mscore'


loadNoteTextures :: SDL.Renderer -> IO [(Scale, Length, SDL.Texture)]
loadNoteTextures r = do
    t1  <- (C,Full,) <$> I.loadTexture r "imgs/notefull.png"
    t2  <- (C,L2,)  <$> I.loadTexture r "imgs/note2.png"
    t3  <- (C,L4,) <$> I.loadTexture r "imgs/note4.png"
    t4  <- (C,L8,) <$> I.loadTexture r "imgs/note8.png"
    t5  <- (C,L16,) <$> I.loadTexture r "imgs/note16.png"
    t6  <- (C,L32,) <$> I.loadTexture r "imgs/note32.png"
    t7  <- (Rest,Full,) <$>I.loadTexture r "imgs/restfull.png"
    t8  <- (Rest,L2,) <$> I.loadTexture r "imgs/rest2.png"
    t9  <- (Rest,L4,) <$> I.loadTexture r "imgs/rest4.png"
    t10 <- (Rest,L8,) <$> I.loadTexture r "imgs/rest8.png"
    t11 <- (Rest,L16,) <$> I.loadTexture r "imgs/rest16.png"
    return [t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11]

drawMusicalScore :: SDL.Renderer -> [(Scale, Length, SDL.Texture)] -> MusicalScore -> IO ()
drawMusicalScore r ts (MusicalScore keys bars) = drawBars bars
    where
    drawBars [] = return ()
    drawBars ((_,ns):bars) = foldlM_ drawNotes (30,30) ns >> drawBars bars
    drawNotes (x,y) n = do
        SDL.copyEx r (findTexture n ts)
                (Just $ SDL.Rectangle (SDL.P $ SDL.V2 0 0) (SDL.V2 960 960))
                (Just $ SDL.Rectangle (SDL.P $ SDL.V2 x y) (SDL.V2 80 80))
                0
                Nothing
                (pure False) -- x,y軸方向に反転する時は使用
        return (x+30, y)

foldlM_ f _ [] = return ()
foldlM_ f a (n:ns) = f a n >>= \a' -> foldlM_ f a' ns

findTexture :: Note -> [(Scale, Length, SDL.Texture)] -> SDL.Texture
findTexture _ [(_,_,a)] = a
findTexture n ((s,l,t):ts) = if scale n == s && fst (len n) == l then t else findTexture n ts


getEvent :: Maybe SDL.Event -> QuailEvent
getEvent Nothing = NotImplemented
getEvent (Just e) = case SDL.eventPayload e of
    SDL.KeyboardEvent keyEvent -> getEventType keyEvent
    ev -> trace (show ev) NotImplemented


dealEvent :: QuailEvent -> SDL.AudioDevice -> SDL.Renderer -> MusicalScore -> IO MusicalScore
dealEvent ev audio r ms = case ev of
    PlaySound   -> playAudio audio >> return ms
    StopSound   -> lockAudio audio >> return ms
    ResumeSound -> resumeAudio audio >> return ms
    AddNote s   -> return $ addNote (initNote{scale=s}) ms
    _ -> return ms


addNote n (MusicalScore keys bars) = MusicalScore keys $ init bars ++ addNote' n (last bars)
    where
    addNote' :: Note -> (Clef,Bar) -> [(Clef,Bar)]
    addNote' n (GClef, bar) = if soundLen (n:bar) <= 4
                                    then [(GClef,bar++[n])]
                                    else [(GClef,bar), (GClef, [n])]


soundLen :: Bar -> Float
soundLen = sum.map noteLen

noteLen :: Note -> Float
noteLen n = let (l,ds) = len n in f l + 0.5*(fromIntegral $ length ds)
    where
    f  L32 = 1/32
    f  L16 = 1/16
    f   L8 =  1/8
    f   L4 =  1/4
    f   L2 =  1/2
    f Full =    1

getEventType :: SDL.KeyboardEventData -> QuailEvent
getEventType event
    | catchOn event (SDL.KeycodeQ, SDL.Pressed) = Quit
    | catchOn event (SDL.KeycodeA, SDL.Pressed) = AddNote C
    | catchOn event (SDL.KeycodeP, SDL.Pressed) = PlaySound
    | catchOn event (SDL.KeycodeS, SDL.Pressed) = StopSound
    | catchOn event (SDL.KeycodeR, SDL.Pressed) = ResumeSound
    | otherwise = NotImplemented

catchOn event (key,mode) = SDL.keyboardEventKeyMotion event == mode
    && SDL.keysymKeycode (SDL.keyboardEventKeysym event) == key