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
import Quial.Types

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
startGUI = withSDL $ withWindow "うずら" (width,height) $
    \w -> withRenderer w renderLoop

renderLoop r = do
    events <- SDL.pollEvents
    let eventlsQPress event = case SDL.eventPayload event of
            SDL.KeyboardEvent keyEvent -> keyEvent `isEventOn` (SDL.KeycodeQ, SDL.Pressed)
            _ -> False
        qPressed = any eventlsQPress events
    SDL.rendererDrawColor r $= V4 200 200 200 30
    t <- I.loadTexture r "imgs/note4.png"
    SDL.clear r
    SDL.copy r t Nothing Nothing
    SDL.present r
    unless qPressed (renderLoop r)

event `isEventOn` (key,mode) = SDL.keyboardEventKeyMotion event == mode
    && SDL.keysymKeycode (SDL.keyboardEventKeysym event) == key