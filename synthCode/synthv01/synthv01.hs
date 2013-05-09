{-----------------------------------------------------------------------------
    reactive-banana-wx
    
    Example: Print out list of values lazily but based on timer...if that makes sense
------------------------------------------------------------------------------}

{-# LANGUAGE ScopedTypeVariables #-} -- allows "forall t. Moment t"

import Data.Int (Int32)
import Graphics.UI.WX hiding (Event)
import Reactive.Banana
import Reactive.Banana.WX
import DataTypes
import SynthesiserFunctions
import SoundSupport
import SynthFilters
--wavPoints :: Behavior t [Double]
--wavPoints = pure (map sin [0.0, ((2*pi*200)/fromIntegral 16000)..])



envelope :: [StepPair]
envelope = [(4000,1), (3000,0.4)]{-
 (200,0.6),(500,0),
 (200,0.6),(500,0),
 (200,0.6),(500,0),
 (200,0.6),(500,0),
 (200,0.6),(500,0)]-}

samplesPS :: Int
samplesPS = 8000 





main = start $ do 
    f <- frame [text := "Timer Go"]
    t <- timer f [enabled := False] -- we're going to need a bigger timer :P
    start   <- button f [text := "Start"]
    stop    <- button f [text := "Stop"] 
    write   <- button f [text := "Write"]
    freqUp  <- button f [text := "Freq Up"]
    freqDo  <- button f [text := "Freq Down"]
    startE  <- button f [text := "Start Envelope"]
    stopE   <- button f [text := "Stop Envelope"]
    
    set f [layout := margin 10 $
            column 10 [row 5 [widget start, widget stop, widget write], 
                row 5 [widget freqUp, widget freqDo, widget startE, widget stopE]]
        ]
  
    -- rw = runningWave
    let rw :: WaveInfo
        rw = WaveInfo 0.0 200 0 False
    let networkDescription :: forall t. Frameworks t => Moment t ()
        networkDescription = do 
            
            eAlarm  <- event0 t command
            eStart  <- event0 start command
            eStop   <- event0 stop  command
            eWrite  <- event0 write command
            eFreqU  <- event0 freqUp command
            eFreqD  <- event0 freqDo command 
            eStartE <- event0 startE command
            eStopE  <- event0 stopE command
            
            startTimer t eStart
            stopTimer  t eStop 
            
             -- a behavior that holds the number of samples we've taken so far
           
            --reactimate $ (\n -> putStrLn "Timer Fired") <$> eAlarm
            
            let bSampleValues :: Behavior t ([Double], [Double], WaveInfo, Amplitude)
                bSampleValues = accumB ([], (sawWave (hPassFilter 500 1000) samplesPS (startPosition rw)  (frequency rw) [1..20]{-(controlValue rw)-}), rw, (repeat 0))
                          $     (gatherSamples samplesPS <$ eAlarm) 
                         `union`(changeCV UP <$ eFreqU) 
                         `union`(changeCV DOWN <$ eFreqD)
                         `union`(beginGenEnvelope samplesPS envelope <$ eStartE) 
                         `union`(endGenEnvelope samplesPS (1000,0) <$ eStopE)
                bFirst = (pure $ \(x, y, z,w) -> x) <*> bSampleValues
            
            writeSoundFile samplesPS bFirst eWrite
            
            
            --eFirst <- changes bFirst 
            --reactimate $ (\n -> putStrLn $ show n) <$> eFirst
    network <- compile networkDescription
    actuate network







     











