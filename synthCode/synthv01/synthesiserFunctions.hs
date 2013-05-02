{-----------------------------------------------------------------------------------
 Module that Deals with Synthesiser Functions that will be used in the main program
------------------------------------------------------------------------------------}
module SynthesiserFunctions where 
import Data.Int (Int32)
import DataTypes

wavPoints samplesPS start freq  cv= (map sin [start, ((2*pi*(freq * (2 ** (fromIntegral cv))))/fromIntegral samplesPS)..])

------------------------------------------------------------------------------------------------------------------------------

gatherSamples :: Int -> ([Double], [Double], WaveInfo, Amplitude) -> ([Double],[Double], WaveInfo, Amplitude)
gatherSamples sample (accum, rest, (WaveInfo s f cv False), amp) = (newAccum, newRest, (WaveInfo (s +fromIntegral sample) f cv False),aRest)
    where 
        newAccum = accum ++ values'
        (values, newRest) = splitAt sample rest
        (aValues, aRest) = splitAt sample amp
        values' = zipWith (*) values aValues

gatherSamples sample (accum, rest, (WaveInfo s f cv True), amp) = (newAccum, newRest, (WaveInfo (s+(fromIntegral sample)) f cv False), aRest)
    where 
        newAccum = accum ++ values'
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

