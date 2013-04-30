{-----------------------------------------------------------------------------
    reactive-banana-wx
    
    Example: Print out list of values lazily but based on timer...if that makes sense
------------------------------------------------------------------------------}

{-# LANGUAGE ScopedTypeVariables #-} -- allows "forall t. Moment t"

import Control.Monad
import qualified Data.List as List
import Data.Maybe
import Data.Ord
import Data.WAVE
import Data.Int (Int32)
import Graphics.UI.WX hiding (Event)
import Reactive.Banana
import Reactive.Banana.WX

--wavPoints :: Behavior t [Double]
--wavPoints = pure (map sin [0.0, ((2*pi*200)/fromIntegral 16000)..])
wavPoints start freq  = (map sin [start, ((2*pi*freq)/fromIntegral sampleRate)..])

sampleRate :: Int
sampleRate = 5400 

generateWAVFile :: WAVE -> IO()
generateWAVFile wav = putWAVEFile "whatever.wav" wav

samplesPS = sampleRate
bitrate = 32

header = WAVEHeader 1 samplesPS bitrate Nothing


dt = 500
main = start $ do 
    f <- frame [text := "Timer Go"]
    t <- timer f [enabled := False] -- we're going to need a bigger timer :P
    start   <- button f [text := "Start"]
    stop    <- button f [text := "Stop"] 
    write   <- button f [text := "Write"]
    freqUp  <- button f [text := "Freq Up"]
    freqDo  <- button f [text := "Freq Down"]
    
    set f [layout := margin 10 $
            column 10 [row 5 [widget start, widget stop, widget write], 
                row 5 [widget freqUp, widget freqDo]]
        ]
  
   
    let networkDescription :: forall t. Frameworks t => Moment t ()
        networkDescription = do 
            eAlarm <- event0 t command
            eStart <- event0 start command
            eStop  <- event0 stop  command
            eWrite <- event0 write command
            eFreqU <- event0 freqUp command
            startTimer t eStart
            stopTimer  t eStop 
            
             -- a behavior that holds the number of samples we've taken so far
           -- let bSampleNumber = accumB 0 ((sampleRate+) < eAlarm)
            --reactimate $ (\n -> putStrLn "Timer Fired") <$> eAlarm
            
            let bSampleValues :: Behavior t ([Double], [Double])
                bSampleValues = accumB ([], (wavPoints 0.0 200)) (specialSplit' sampleRate <$ eAlarm)
           
            
            let bFirst = (pure fst) <*> bSampleValues
            
            writeSoundFile bFirst eWrite
            
            
            --eFirst <- changes bFirst 
            --reactimate $ (\n -> putStrLn $ show n) <$> eFirst
    network <- compile networkDescription
    actuate network
 
specialSplit :: Int -> ([a], [a]) -> ([a],[a])
specialSplit sample = \(_, xs) -> splitAt sample xs 
 
-- different version of split that holds all the values that have been taken up till then 
specialSplit' :: Int -> ([Double], [Double]) -> ([Double],[Double])
specialSplit' sample (accum, rest ) 
    = (newAccum, newRest)
    where 
        newAccum = accum ++ values
        (values, newRest) = splitAt sample rest        

startTimer :: Frameworks t => Timer -> Event t a -> Moment t ()
startTimer t e = do
    let 
        (eStart, _) = mapAccum [] (restart <$ e)
        restart ys = (set t [enabled := True, interval := dt], ys)
    reactimate $ (\n -> putStrLn "Timer Start") <$> eStart
    reactimate eStart
    
stopTimer :: Frameworks t => Timer -> Event t a -> Moment t ()
stopTimer  t e = do
    let 
         (eStop, _) = mapAccum [] (stop <$ e)
         stop ys = (set t [enabled := False], ys)
    reactimate $ (\n -> putStrLn "Timer Stopped") <$> eStop
    reactimate  eStop

-- Going to need the entire list of samples that are output up till then  
-- should be stored in a behavior and then when event write is pressed
-- we take that list
writeSoundFile :: Frameworks  t => Behavior t [Double] -> Event t a -> Moment t ()
writeSoundFile b e = do
    let 
        bStep1 = (pure (map doubleToSample)) <*> b
        bStep2 = (pure $ map (:[])) <*> bStep1
        bStep3 = (pure (WAVE header)) <*> bStep2
        eFinal = bStep3 <@ e
    -- this seems to cause a trigger whenever b is updated
    -- eOut <-  changes bStep1
    reactimate $ (\n -> putStrLn "Writing to File") <$> eFinal
    reactimate $ (generateWAVFile) <$> eFinal
    --reactimate $ (\n -> putStrLn $ show n) <$> eFinal
    
    return ()
    