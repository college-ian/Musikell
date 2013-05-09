{-----------------------------------------------------------------------------------
 Module that Deals with Synthesiser Functions that will be used in the main program
------------------------------------------------------------------------------------}
module SynthesiserFunctions where 
import Data.Int (Int32)
import DataTypes
import Control.DeepSeq

-- TODO - implement square + triangle wave, add new type Wave and pass as argument to function

--wavPoints samplesPS start freq cv = (map sin [start, ((2*pi*(freq * (2 ** (fromIntegral cv))))/fromIntegral samplesPS)..])

wavPoints samplePS start freq cv =  map (temp' freq cv) $ generateWaves samplePS start
 
{-saw_ :: Harmonics -> Int -> Double -> Double -> Int -> [Double]
saw_   harmonics samplesPS start freq cv  = map (saw'' harmonics freq cv)
saw''  harmonics freq cv =  ((foldr (+) 0 $ map (saw' freq cv) harmonics))
saw'   freq cv harmonic  =  ((-1)^ (harmonic + 1)) * (saw freq cv harmonic)
saw    freq cv harmonic  =  (sin(2*pi*newFreq*(2**(fromIntegral cv))))/(fromIntegral harmonic)
    where 
        newFreq = (fromIntegral harmonic)*freq-}

genValues :: (Double -> Double) -> Double -> (Harmonic, Int) -> Double -> Double
genValues pFilter f (harmonic, num) t =  attenuate * (num' * (sin (2*pi*harmF*t)/harm))
    where
        harm = fromIntegral harmonic
        num' = fromIntegral num
        harmF = f * harm
        attenuate = pFilter harmF

temp'  f cv t = sin (2*pi*(f*(2^ (fromIntegral cv)))*t)
    
        
-- push this over the list of times
--temp' f (num, harm)  = map temp f (num, harm) 

-- the method to create
--temp'' = map f 

pair :: Harmonics -> [(Harmonic, Int)]
pair harmonics = zip harmonics $ map (\x -> (-1)^ (x+1)) harmonics

functionGen :: (Double -> Double) ->  Double -> Harmonics -> [Double -> Double]
functionGen pFilter freq harmonics = map (genValues pFilter freq) $ pair harmonics

generateWaves samplesPS start = [start, step..]
    where
        step = start + 1/(fromIntegral samplesPS)
        
sawWave pFilter samplesPS start freq harmonics = superFold $ applyList (functionGen pFilter freq harmonics) $ generateWaves samplesPS start

applyList [] num = []
applyList (x:xs) num = [(map x num)] ++ (applyList xs num)

-- method that merges x number of lists and adds them together
superFold = foldr (zipWith (+)) [0,0..]

squareWaves pFilter samplePS start freq harmonics = sawWave pFilter samplePS start freq (filter odd harmonics)

------------------------------------------------------------------------------------------------------------------------------

{-gatherSamples :: Int -> ([Double], [Double], WaveInfo, Amplitude) -> ([Double],[Double], WaveInfo, Amplitude)
gatherSamples sample (accum, rest, (WaveInfo s f cv False), amp) = (newAccum, newRest, (WaveInfo (s +fromIntegral sample) f cv False),aRest)
    where 
        newAccum = accum ++ (force values')
        (values, newRest) = splitAt sample rest
        (aValues, aRest) = splitAt sample amp
        values' = zipWith (*) values aValues

gatherSamples sample (accum, rest, (WaveInfo s f cv True), amp) = (newAccum, newRest, (WaveInfo (s+(fromIntegral sample)) f cv False), aRest)
    where 
        newAccum = accum ++ (force values')
        --(values, _) = splitAt sample rest         
        (_,newRest') = splitAt (round s) (wavPoints sample 0 f cv)
        (values, newRest) = splitAt sample newRest'
        (aValues, aRest) = splitAt sample amp
        values' = zipWith (*) values aValues-}


gatherSamples :: Int -> ([Double], [Double], WaveInfo, Amplitude) -> ([Double],[Double], WaveInfo, Amplitude)
gatherSamples sample (accum, rest, (WaveInfo s f cv False), amp) = (newAccum, newRest, (WaveInfo (s +fromIntegral sample) f cv False),aRest)
    where 
        newAccum = accum ++ (force values')
        (values, newRest) = splitAt sample rest
        (aValues, aRest) = splitAt sample amp
        values' = zipWith (*) values aValues

gatherSamples sample (accum, rest, (WaveInfo s f cv True), amp) = (newAccum, newRest, (WaveInfo (s+(fromIntegral sample)) f cv False), aRest)
    where 
        newAccum = accum ++ (force values')
        --(values, _) = splitAt sample rest         
        (_,newRest') = splitAt (round s) (wavPoints sample 0 f cv)
        (values, newRest) = splitAt sample newRest'
        (aValues, aRest) = splitAt sample amp
        values' = zipWith (*) values aValues




------------------------------------------------------------------------------------------------------------------------------

changeCV :: Direction -> ([Double], [Double], WaveInfo, Amplitude) -> ([Double],[Double], WaveInfo, Amplitude)
changeCV UP (x,y, (WaveInfo s f cv c), amp) = (x, y, (WaveInfo s f (cv+1) True), amp)

changeCV DOWN (x,y, (WaveInfo s f cv c),amp) = (x, y, (WaveInfo s f cv' True), amp)
    where 
        cv' | cv == 0 = cv
            | otherwise = (cv-1)
            
--------------------------------------------------------------------------------------------------------------------------------

beginGenEnvelope :: Int -> [StepPair] ->  ([Double], [Double], WaveInfo, Amplitude) ->  ([Double], [Double], WaveInfo, Amplitude)
beginGenEnvelope samplesPS pairs (x,y,z,amplitude) = (x, y, z, newAmp)
    where newAmp =  beginGenEnvelope' samplesPS [] (head amplitude) pairs 


-- Force users to put in times in MSs for the moment 
beginGenEnvelope' :: Int -> [Double] -> Level -> [StepPair] -> [Double]
-- empty list we can just use the accumulated values + a lazy list of sustain values
beginGenEnvelope' samplesPS acc startLevel [] = acc ++ (repeat startLevel)
-- recursive element 
beginGenEnvelope' samplesPS acc startLevel ((time, nextLevel):xs) = beginGenEnvelope' samplesPS (acc ++ [startLevel, sl'..nextLevel]) nextLevel xs
    where 
        samples = (time/1000) * (fromIntegral samplesPS)
        sl' | startLevel > nextLevel =  (startLevel - 1/samples)
            | otherwise =  (startLevel + 1/samples)


endGenEnvelope :: Int -> StepPair -> ([Double], [Double], WaveInfo, Amplitude) ->  ([Double], [Double], WaveInfo, Amplitude)
endGenEnvelope samplesPS pair (x,y,z,amplitude) = (x, y, z, newAmp)
    where newAmp =  endGenEnvelope'  samplesPS (head amplitude) pair

-- this only needs the release time so deal with only that - note that nextLevel will always be 0
endGenEnvelope' :: Int -> Level -> StepPair -> [Double]
endGenEnvelope' samplesPS startLevel (time, nextLevel) = [startLevel, sl'..nextLevel] ++ (repeat nextLevel)
    where
        samples = (time/1000) * (fromIntegral samplesPS)
        sl' = startLevel - (1/samples)

