module Waves where

import Graphics.Gnuplot.Simple
import Data.Int (Int32)
import SynthFilters
type Frequency = Double
type Amplitude = Int32
type AmplitudeF = Double 
type Phase = Double
type Harmonics = Double
data Wave = Empty
            | SineWave Amplitude Frequency Phase 
            | SawWave AmplitudeF Frequency Phase deriving (Show)

sineWave :: Int -> Wave -> [Int32]
sineWave samples (SineWave amp freq phase)  =  map (round . (* fromIntegral amp)) $
                                        map sin [0.0, ((2*pi*freq)/fromIntegral samples)..]

--sineWave samples (SineWave amp freq phase)  =  map (round . (* fromIntegral amp)) $ sinMap
                                        

sinMap takeAmount freq samples = zip (take takeAmount ([0.0, ((2*pi*freq)/fromIntegral samples)..])) 
                         (take takeAmount (map sin [0.0, ((2*pi*freq)/fromIntegral samples)..]))

-- [(time, value at time)]


-- give it a number and it will return a value and

-- filter functions that 

-- pairing out two frequencies


saw :: String -> (Double -> Double) -> Double -> Double -> Int -> Double
saw  "attenuate" pFilter time freq harmonic = attenuate * (sin(2*pi*newFreq*time))/(fromIntegral harmonic)
    where 
        newFreq = (fromIntegral harmonic)*freq
        attenuate = pFilter newFreq

saw  "none" pFilter time freq harmonic = (sin(2*pi*newFreq*time))/(fromIntegral harmonic)
    where 
        newFreq = (fromIntegral harmonic)*freq    
--test freq harmonic time = (time, (sin(2*pi*freq*time)))
--testSaw freq sample = map (test freq 1) [0.0, 1/sample..] 


saw' :: String -> (Double -> Double) -> Double -> Double -> Int -> Double
saw' string pFilter time freq harmonic= ((-1)^ (harmonic + 1)) * (saw string pFilter time freq harmonic)

-- this one takes a list of harmonics
--saw'' :: [Double] -> Int32 -> Double -> [Double]
--saw'' harmonics freq time =  (time, (2/pi) *  (foldr (+) 0 $ map (saw' time freq) harmonics))

-- adding filter so that a person can attenuate  

--saw'' string pFilter harmonics freq time =  (time, (2/pi) *  (foldr (+) 0 $ map (saw' string pFilter time freq) harmonics))
saw'' string pFilter harmonics freq time =  ((foldr (+) 0 $ map (saw' string pFilter time freq) harmonics))

-- TODO:  Modify this code so that I can carry the values of the waves around
-- what would this require? If I generate the values, to get the general wave
-- perform the saw the first time, and return the value - then as filters are
-- passed we can recompute them just before output


saw_ :: String -> (Double -> Double) -> [Int] -> Wave -> Int -> [Double]
saw_ string pFilter harmonics saw@(SawWave amp freq phase)  sample = map (saw'' string pFilter harmonics freq) [0.0, 1/(fromIntegral sample)..]
-- the summation of all the sine waves generated
--sawWave wave numHarms samples = map takemap (sineWave' samples) $ sineWithHarmonics wave numHarms

-- need to write 
plot filt takeRate harmonics sample = plotLists [] $ [(take takeRate $  saw_ "none" filt harmonics (SawWave 1 200 1) sample),
     (take takeRate $  saw_ "attenuate" filt harmonics (SawWave 1 200 1) sample)]


square harmonics wave sample = saw_ "none" id (filter odd harmonics) wave sample



-- basefrequency, ignore harmonics for the minute - set to 5 
-- sawWave wave t = foldl (+) 0 (map (sineWave t) (sineWithHarmonics wave 5000))

-- sawtooth wave is a wave that uses several harmonics up to the nyqusit frequency (half the sampling frequency)
-- give a fundamental frequncy and then generate the wave from that...

-- before calling that we need to make sure that it's no more than the nyquist frequency
