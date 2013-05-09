{- Types needed in the synth. -}
module DataTypes where 

data Wave = SinWave Frequency Amplitude' Filter 
            | SawWave Frequency Amplitude' Harmonics Filter
            | SquWave Frequency Amplitude' Harmonics Filter 



type Amplitude' = Double
type SampleRate = Int
type ControlValue = Integer

type Filter = (Double -> Double)

type Amplitude = [Double]

type Level = Double

type Time = Double

type StepPair = (Time, Level)

type Harmonic = Int

type Frequency = Double

type Harmonics = [Harmonic] 

data Direction = UP | DOWN

-- wave meta data that we need 
data WaveInfo = WaveInfo { startPosition :: Double
                     , wave :: Wave
                     , controlValue :: ControlValue
                     , changed :: Bool  
                     }



-- Important types for the use of the Synth State

type SamplesGathered = [Double]
type SamplesGenerated = [Double]

type SynthState = (SamplesGathered, SamplesGenerated, WaveInfo, Amplitude) 

