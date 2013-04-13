module SoundUtility where

import Data.Int (Int32)
import Waves


samplesPS = 16000
bitrate = 32

samples :: [[Int32]]
samples =  map (:[]) $ getSound (SineWave (maxBound `div` 2) 600 1) samplesPS 10

getSound :: Wave -> Int -> Double -> [Int32]
getSound wave samples len = take (round $ len * (fromIntegral samples)) $ sineWave wave samples

applys :: [Wave -> Wave] -> Wave -> Wave
applys [] wave = wave
applys (x:xs) wave = applys (xs) (x wave)
