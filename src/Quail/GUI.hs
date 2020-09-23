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
import Quail.Types

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
    SDL.rendererDrawColor r $= V4 200 200 200 30
    event <- dealEvent <$> SDL.pollEvent
    t <- I.loadTexture r "imgs/note4.png"
    t2 <- I.loadTexture r "imgs/rest4.png"
    SDL.clear r
    SDL.copy r t Nothing Nothing
    SDL.copy r t2 Nothing Nothing
    SDL.destroyTexture t
    SDL.destroyTexture t2
    SDL.present r
    unless (event == Quit) $ renderLoop r

dealEvent :: Maybe SDL.Event -> QuailEvent
dealEvent Nothing = NotImplemented
dealEvent (Just e) = case SDL.eventPayload e of
    SDL.KeyboardEvent keyEvent -> getEventType keyEvent
    _ -> NotImplemented



addNote :: SDL.Renderer -> Scale -> IO SDL.Texture
addNote r s = I.loadTexture r "imgs/rest4.png"


getEventType :: SDL.KeyboardEventData -> QuailEvent
getEventType event
    | catchOn event (SDL.KeycodeQ, SDL.Pressed) = Quit
    | catchOn event (SDL.KeycodeA, SDL.Pressed) = AddNote C
    | otherwise = NotImplemented

catchOn event (key,mode) = SDL.keyboardEventKeyMotion event == mode
    && SDL.keysymKeycode (SDL.keyboardEventKeysym event) == key