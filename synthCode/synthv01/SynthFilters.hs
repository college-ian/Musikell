module SynthFilters where
 import Text.Printf
{-Class that deals with the filtering of waves
 Designs combinators that also allow for band filters also-}

 -- needs to be a higher order function
 -- will take 2 areas, a low range and a high range

 -- low pass filter will take a minimum cutoff frequency and everything below 
 -- that will be 1, and during the transition band will be interpolated,
 -- and everything above the upper limit will be 0
 
 -- just so that we can have several higher order functions 
 noFilt frequency = 1 
 
 hPassFilter :: Double -> Double -> Double -> Double
 hPassFilter lowCut highCut frequency
    | frequency < lowCut = 0
    | frequency > highCut = 1 
    --  this here will be the interpolation between the two cutoff points
    | otherwise  = interpolated
        where interpolated = (frequency-lowCut)/(highCut - lowCut)

 -- unsuprisingly a low pass filter is just the inverse of a highpass filter
 lPassFilter :: Double -> Double -> Double -> Double
 lPassFilter lowCut highCut frequency = 1 - hPassFilter lowCut highCut frequency
 
 -- everything not in the range is (i.e set to 0)

 -- the transition regions can be independant 
 bandPassFilter :: Double -> Double -> Double -> Double
 bandPassFilter lo hi f =  (hPassFilter lo (lo+200) f) * (lPassFilter (hi-200) hi f)
 {-bandPassFilter lowRange highRange frequency 
  = if (lpass == 1) then (hPassFilter highRange (highRange + diff) frequency) 
     else lpass 
   where
       lpass = lPassFilter lowRange highRange frequency
       diff = (highRange - lowRange)
-}

 testX filt = map (printf "%2.4f") (map (filt 500 1500) [1, 100..2000])
 testBP :: [ String ]
 testBP = testX bandPassFilter
 testLP :: [ String ]
 testLP = testX lPassFilter
 testHP :: [ String ]
 testHP = testX hPassFilter