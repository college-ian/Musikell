module SoundSupport where
import Data.Int (Int32)
import Graphics.UI.WX hiding (Event)
import Reactive.Banana
import Reactive.Banana.WX
import Data.WAVE

dt = 1000
generateWAVFile :: WAVE -> IO()
generateWAVFile wav = putWAVEFile "testFilter.wav" wav


bitrate = 32

header samplesPS = WAVEHeader 1 samplesPS bitrate Nothing

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

printNoBrackets [] = do
    print '0'
    return ()
printNoBrackets (x:xs) = do
    print x
    printNoBrackets xs

realTimePlay :: Frameworks  t => Int -> Behavior t [Double] -> Moment t ()
realTimePlay samples b = do
    let 
        bStep1 = (pure (map doubleToSample)) <*> b
    e <- changes bStep1
    reactimate $ (\n -> putStrLn $ show n) <$> e

writeSoundFile :: Frameworks  t => Int -> Behavior t [Double] -> Event t a -> Moment t ()
writeSoundFile samplesPS b e = do
    let 
        bStep1 = (pure (map doubleToSample)) <*> b
        bStep2 = (pure $ map (:[])) <*> bStep1
        bStep3 = (pure (WAVE (header samplesPS))) <*> bStep2
        eFinal = bStep3 <@ e
    -- this seems to cause a trigger whenever b is updated
    -- eOut <-  changes bStep1
    reactimate $ (\n -> putStrLn "Writing to File") <$> eFinal
    reactimate $ (generateWAVFile) <$> eFinal
    --reactimate $ (\n -> putStrLn $ show n) <$> eFinal
    
    return ()
    