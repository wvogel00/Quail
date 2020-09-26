{-# LANGUAGE OverloadedStrings #-}

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
    SDL.rendererDrawColor r $= V4 200 200 200 30
    event <- getEvent <$> SDL.pollEvent
    dealEvent event audio r
    t <- I.loadTexture r "imgs/note4.png"
    t2 <- I.loadTexture r "imgs/rest4.png"
    SDL.clear r
    SDL.copy r t Nothing Nothing
    SDL.copy r t2 Nothing Nothing
    SDL.destroyTexture t
    SDL.destroyTexture t2
    SDL.present r
    unless (event == Quit) $ renderLoop audio r


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