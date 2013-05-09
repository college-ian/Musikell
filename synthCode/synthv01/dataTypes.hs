{- Types needed in the synth. -}
module DataTypes where 

type Amplitude = [Double]

type Level = Double

type Time = Double

type StepPair = (Time, Level)

type Harmonic = Int

type Harmonics = [Harmonic] 

data Direction = UP | DOWN

data WaveInfo = WaveInfo { startPosition :: Double
                     , frequency :: Double
                     , controlValue :: Int
                     , changed :: Bool  
                     }


