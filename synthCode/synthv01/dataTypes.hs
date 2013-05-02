{- Types needed in the synth. -}
module DataTypes where 

type Amplitude = [Double]
type Level = Double
type Time = Double
type StepPair = (Time, Level)

data Direction = UP | DOWN
data WaveInfo = WaveInfo { startPosition :: Double
                     , frequency :: Double
                     , controlValue :: Int
                     , changed :: Bool  
                     }


