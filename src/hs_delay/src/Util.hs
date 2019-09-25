module Util
  ( ptpt
  , mkSignalParams
  ) where

import qualified Interop
import qualified Signal

ptpt :: Signal.Point -> Interop.Point
ptpt (Signal.Point x y) = Interop.Point x y

mkSignalParams :: Interop.Params -> Signal.SignalParams
mkSignalParams Interop.Params {
  Interop.paramBitrate      = br,
  Interop.paramSamplingRate = sr,
  Interop.paramCarrier      = c,
  Interop.paramSpan         = ts
} = Signal.SignalParams ts c sr br
