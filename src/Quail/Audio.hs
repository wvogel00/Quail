{-# LANGUAGE GADTs #-}

module Quail.Audio where

import SDL (($=))
import qualified SDL
import Control.Monad (zipWithM_)
import Data.Int (Int16,Int32)
import Data.IORef
import qualified Data.Vector.Storable.Mutable as V
import Quail.Types

noteSound :: Note -> [Int16]
noteSound n = []

sinSamples :: [Int16]
sinSamples = map f [0..] where
    f :: Int32 -> Int16
    f n = round (fromIntegral (div maxBound 2 :: Int16) * sin (2*pi*t*freq))
        where t = fromIntegral n / 48000
    freq = 440

audioCB :: IORef [Int16] -> SDL.AudioFormat sampleType -> V.IOVector sampleType -> IO()
audioCB sound format buf = case format of
    SDL.Signed16BitLEAudio -> do
        sound' <- readIORef sound
        let n = V.length buf
        zipWithM_ (V.write buf) [0..] $ take n sound'
        writeIORef sound $ drop n sound'
    _ -> putStrLn "データ形式が誤っています (Int16のみ対応)"

openAudio :: IORef [Int16] -> IO SDL.AudioDevice
openAudio sound = fst <$> SDL.openAudioDevice SDL.OpenDeviceSpec
    { SDL.openDeviceFreq = SDL.Mandate 48000
    , SDL.openDeviceFormat = SDL.Mandate SDL.Signed16BitNativeAudio
    , SDL.openDeviceChannels = SDL.Mandate SDL.Mono
    , SDL.openDeviceSamples = 4096 * 2
    , SDL.openDeviceCallback = audioCB sound
    , SDL.openDeviceUsage = SDL.ForPlayback
    , SDL.openDeviceName = Nothing
    }

playAudio :: SDL.AudioDevice -> IO ()
playAudio dev = SDL.setAudioDevicePlaybackState dev SDL.Play

lockAudio :: SDL.AudioDevice -> IO ()
lockAudio dev = SDL.setAudioDeviceLocked dev SDL.Locked

resumeAudio :: SDL.AudioDevice -> IO ()
resumeAudio dev = SDL.setAudioDeviceLocked dev SDL.Unlocked
