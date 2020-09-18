{-# LANGUAGE OverloadedStrings #-}

module Quail.GUI where

import qualified SDL
import qualified SDL.Image as I
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Extra (whileM)

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

isContinue :: Maybe SDL.Event -> Bool
isContinue = maybe True (not . isQuitEvent)

startGUI :: IO ()
startGUI = withSDL $ withWindow "Quail" (width,height) $
    \w -> do
        screen <- SDL.getWindowSurface w
        pixelFormat <- SDL.surfaceFormat screen

        image <- I.load "imgs/note4.png"
        surface <- SDL.convertSurface image pixelFormat

        whileM $
            isContinue <$> SDL.pollEvent
            >>= conditionallyRun (draw w screen surface)

        SDL.freeSurface image
        SDL.freeSurface surface
        SDL.freeSurface screen

draw :: (MonadIO m) => SDL.Window -> SDL.Surface -> SDL.Surface -> m ()
draw w s1 s2 = SDL.surfaceBlitScaled s2 Nothing s1 Nothing
            >> SDL.updateWindowSurface w