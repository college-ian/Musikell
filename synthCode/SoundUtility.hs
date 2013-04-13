module SoundUtility where
import Data.WAVE
import Data.Int (Int32)
import Data.List.Split (splitOn)
import Waves


samplesPS = 16000
bitrate = 32

samples :: [[WAVESample]]
samples = map (:[]) $ getSound (SawWave 1 600 1) samplesPS 1

getSound :: Wave -> Int -> Double -> [WAVESample]
--getSound wave samples len = map doubleToSample $ take (round $ len * (fromIntegral samples)) 
	-- $ saw_ "none" id [1..10] wave samples
--getSound wave samples len = concat $ take (round len) $ repeat  (sineWave samples wave)


-- Rewrite the function so that it can take both saw and sine waves 
getSound wave samples len = map doubleToSample $ take (round $ len * (fromIntegral samples)) $ saw_ "none"  id [1..100]  wave samples

--getSound wave samples len = map doubleToSample $ take (round $ len * (fromIntegral samples)) $ square [1..100]  wave samples



 -- 

applys :: [Wave -> Wave] -> Wave -> Wave
applys [] wave = wave
applys (x:xs) wave = applys (xs) (x wave)

header = WAVEHeader 1 samplesPS bitrate Nothing
waveData = WAVE header samples

-- last step in creating the wave
generateWAVFile :: WAVE -> IO()
generateWAVFile wav = putWAVEFile "testSaw.wav" wav