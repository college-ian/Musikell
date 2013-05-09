module Waves where
import DataTypes

-- main wave function that can take the different types of waves and generate them
generateWave :: SampleRate -> WaveInfo -> [Double]
generateWave samplePS (WaveInfo s (SinWave f a fil)   cv _) = (sineWave samplePS s f fil cv)
generateWave samplePS (WaveInfo s (SawWave f a h fil) cv _) = (sawWave  samplePS s f h fil cv)
generateWave samplePS (WaveInfo s (SquWave f a h fil) cv _) = (squareWave samplePS s f h fil cv)

-------------------------------------------------------------------------------------------------------------------

sineWave samplePS start freq fil cv =  map (sine freq cv) $ generateWaves samplePS start
sine f cv t = sin (2*pi*(f*(2^ (fromIntegral cv)))*t)

-------------------------------------------------------------------------------------------------------------------

squareWave samplePS start freq harmonics fil cv = sawWave samplePS start freq (filter odd harmonics) fil cv

-------------------------------------------------------------------------------------------------------------------

sawWave samplesPS start freq harmonics fil cv = superFold $ applyList (functionGen fil freq' harmonics) $ generateWaves samplesPS start
    where freq' = freq * (2 ^ fromIntegral cv)

genValues :: (Double -> Double) -> Double -> (Harmonic, Int) -> Double -> Double
genValues pFilter f (harmonic, num) t =  attenuate * (num' * (sin (2*pi*harmF*t)/harm))
    where
        harm = fromIntegral harmonic
        num' = fromIntegral num
        harmF = f * harm
        attenuate = pFilter harmF


    
pair :: Harmonics -> [(Harmonic, Int)]
pair harmonics = zip harmonics $ map (\x -> (-1)^ (x+1)) harmonics

functionGen :: (Double -> Double) ->  Double -> Harmonics -> [Double -> Double]
functionGen pFilter freq harmonics = map (genValues pFilter freq) $ pair harmonics


-------------------------------------------------------------------------------------------------------------------

generateWaves samplesPS start = [start, step..]
    where
        step = start + 1/(fromIntegral samplesPS)
        

applyList [] num = []
applyList (x:xs) num = [(map x num)] ++ (applyList xs num)

-- method that merges x number of lists and adds them together
superFold = foldr (zipWith (+)) [0,0..]


