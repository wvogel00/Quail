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

withRenderer :: (MonadIO m) => SDL.Window -> (SDL.Renderer -> m a) -> m ()
withRenderer w op = do
    r <- SDL.createRenderer w (-1) rendererConfig
    void $ op r
    SDL.destroyRenderer r

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
            putStrLn "audio device loaded"
            withRenderer w (renderLoop audio)


renderLoop audio r = do
    ts <- loadNoteTextures r
    SDL.rendererDrawColor r $= V4 200 200 200 30
    event <- getEvent <$> SDL.pollEvent
    dealEvent event audio r
    ts <- loadNoteTextures r
    --t <- I.loadTexture r "imgs/note4.png"
    --t2 <- I.loadTexture r "imgs/rest4.png"

    SDL.clear r
    drawNotes r ts [Note{scale=C, len = (L16,[])},Note{scale=C, len = (L2,[])}]
    --SDL.copy r t Nothing Nothing
    --SDL.copy r t2 Nothing Nothing
    --SDL.destroyTexture t
    --SDL.destroyTexture t2
    SDL.present r
    unless (event == Quit) $ renderLoop audio r

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

drawNotes :: SDL.Renderer -> [(Scale, Length, SDL.Texture)] -> [Note] -> IO ()
drawNotes r ts ns = foldlM_ drawNote (30, 30) ns
    where
    drawNote (x,y) n = do
        SDL.copyEx r (findTexture n ts)
                (Just $ SDL.Rectangle (SDL.P $ SDL.V2 0 0) (SDL.V2 960 960))
                (Just $ SDL.Rectangle (SDL.P $ SDL.V2 x y) (SDL.V2 80 80))
                0
                Nothing
                (pure False)
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
    _ -> NotImplemented


dealEvent :: QuailEvent -> SDL.AudioDevice -> SDL.Renderer -> IO ()
dealEvent ev audio r = case ev of
    PlaySound -> playAudio audio
    StopSound -> lockAudio audio
    ResumeSound -> resumeAudio audio
    _ -> return ()


addNote :: SDL.Renderer -> Scale -> IO SDL.Texture
addNote r s = I.loadTexture r "imgs/rest4.png"


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