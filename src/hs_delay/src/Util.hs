module Util
  ( ptpt
  , mkSignalParams
  ) where

import qualified Interop
import qualified Signal
import qualified Data.Vector.Primitive as V

ptpt :: Signal.SignalData -> [Interop.Point]
ptpt (Signal.SignalData xs ys) = zipWith Interop.Point (V.toList xs) (V.toList ys)

mkSignalParams :: Interop.Params -> Signal.SignalParams
mkSignalParams Interop.Params {
  Interop.paramBitrate      = br,
  Interop.paramSamplingRate = sr,
  Interop.paramCarrier      = c,
  Interop.paramSpan         = ts
} = Signal.SignalParams ts c sr br
