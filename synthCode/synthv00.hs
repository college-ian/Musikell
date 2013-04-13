module Synth where

import Control.Monad
import qualified Data.List as List
import Data.WAVE
import Data.Maybe
import Data.Ord
import SoundUtility

-- create a header for when outputting the file
main = generateWAVFile waveData 