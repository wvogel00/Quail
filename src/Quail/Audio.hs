{-# LANGUAGE GADTs #-}

module Quail.Audio where

import SDL (($=))
import qualified SDL
import Control.Monad (zipWithM_)
import Data.Int (Int16,Int32)
import Data.IORef
import qualified Data.Vector.Storable.Mutable as V
import Quail.Utils (getFreq, noteLen)
import Quail.Types
import Data.Word

sampleFreq = 48000

noteSound :: Metronome -> Note -> [Int16]
noteSound (Metronome t) n = map f [0,1/fromIntegral sampleFreq .. soundLen*l]
    where
    f t = round $ fromIntegral (div maxBound 2 :: Int16) * sin (2*pi*fq*t)
    (l,fq) = ( 4 * noteLen n, getFreq n) -- noteの長さを，4分音符の個数に変換
    soundLen = 60/fromIntegral t -- 4分音符一つあたりの音の長さ

sinSamples :: [Int16]
sinSamples = map f [0..] where
    f :: Int32 -> Int16
    f n = round (fromIntegral (div maxBound 2 :: Int16) * cos (2*pi*freq*t))
        where t = fromIntegral n /fromIntegral sampleFreq
    freq = 440

audioCB :: IORef (Maybe Word8) -> IORef [Int16] -> SDL.AudioFormat sampleType -> V.IOVector sampleType -> IO()
audioCB silence sound format buf = case format of
    SDL.Signed16BitLEAudio -> do
        sound' <- readIORef sound
        silence' <- readIORef silence
        let n = V.length buf
        case silence' of
            Just s -> zipWithM_ (V.write buf) [0..] $ take n $ sound' ++ (repeat $ fromIntegral s)
            Nothing -> zipWithM_ (V.write buf) [0..] $ take n $ sound'
        writeIORef sound $ drop n sound'
    _ -> putStrLn "データ形式が誤っています (Int16のみ対応)"

openAudio :: IORef [Int16] -> IO SDL.AudioDevice
openAudio sound = do
    silence <- newIORef Nothing
    (device, spec) <- SDL.openAudioDevice SDL.OpenDeviceSpec
        { SDL.openDeviceFreq = SDL.Mandate sampleFreq
        , SDL.openDeviceFormat = SDL.Mandate SDL.Signed16BitNativeAudio
        , SDL.openDeviceChannels = SDL.Mandate SDL.Mono
        , SDL.openDeviceSamples = 4096 * 2
        , SDL.openDeviceCallback = audioCB silence sound
        , SDL.openDeviceUsage = SDL.ForPlayback
        , SDL.openDeviceName = Nothing
        }
    writeIORef silence $ Just $ SDL.audioSpecSilence spec
    return device

closeAudio :: SDL.AudioDevice -> IO ()
closeAudio = SDL.closeAudioDevice

playAudio :: SDL.AudioDevice -> IORef [a] -> IO ()
playAudio dev sound = do
    sound' <- readIORef sound
    if null sound'
        then SDL.setAudioDeviceLocked dev SDL.Locked
        else SDL.setAudioDevicePlaybackState dev SDL.Play

lockAudio :: SDL.AudioDevice -> IO ()
lockAudio dev = SDL.setAudioDeviceLocked dev SDL.Locked

resumeAudio :: SDL.AudioDevice -> IO ()
resumeAudio dev = SDL.setAudioDeviceLocked dev SDL.Unlocked
