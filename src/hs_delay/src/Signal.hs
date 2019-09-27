module Signal
  ( Modulation(..)
  , SignalParams(..)
  , BinData(..)
  , SignalData(..)

  , mkSignal
  , shift
  , noisify
  , correlate
  , argmax

  ) where

import System.Random
import Control.Monad.ST
import Data.STRef
import Data.List.Split(chunksOf)

import qualified Data.Vector.Primitive as V

data Modulation = AM | PM | FM deriving (Eq, Show)

data SignalParams = SignalParams {
  timespan    :: Double,
  carrier     :: Double,
  sampling    :: Double,
  bitrate     :: Double
} deriving (Eq, Show)

newtype BinData = BinData { binData :: [Bool] } deriving (Eq, Show)
data SignalData = SignalData {
  xs :: V.Vector Double,
  ys :: V.Vector Double
} deriving (Eq, Show)

data GenState = GenState {
  gstAmDataTail :: BinData,
  gstAmDataIdx  :: Int
} deriving (Eq, Show)

mkSignal :: SignalParams -> BinData -> Modulation -> SignalData
mkSignal p d m = runST $ do
  s <- newSTRef $ GenState d 0
  SignalData <$> (pure $ sampleTime p n)
             <*> V.generateM n (gen' s) where
    n = fromTime p (timespan p)

    gen' :: STRef a GenState -> Int -> ST a Double
    gen' s i = do
      s0 <- readSTRef s
      let (s1, p) = gen s0 i
      writeSTRef s s1
      return p

    gen :: GenState -> Int -> (GenState, Double)
    gen st0 i = (st, v) where
      -- unpack current state essentials
      (d0:ds)     = (binData . gstAmDataTail) st0
      di0         = gstAmDataIdx st0
      -- signal-related params
      t           = toTime p i
      v           = case m of
        AM -> a * sin (2 * pi * f * t)              where a  = toReal d0
        PM -> sin (2 * pi * f * t + b * pi)         where b  = toReal d0
        FM -> sin (2 * pi * (f + df) * t + a * pi)  where a  = toReal d0
                                                          df = b * br / 2 where b  = toSgn d0
                                                                                br = (bitrate p)
        where f = carrier p
      -- generate next state
      di1         = toSeqIdx t1 where t1 = toTime p (i + 1)
      st          | di0 == di1  = st0
                  | otherwise   = GenState (BinData ds) di1
      -- helpers
      toReal d0   = fromIntegral . fromEnum $ d0 :: Double
      toSgn d0    = fromIntegral . (normalize . fromEnum) $ d0 :: Double
      toSeqIdx t0 = round (t0 * (bitrate p)) :: Int

noisify :: StdGen -> Double -> SignalData -> SignalData
noisify g snr d' = SignalData (xs d') r where
  d = ys d'
  n = V.length d
  ns' = V.fromListN n $ noise g
  
  esToEn = exp (snr / 10)
  es = energy d
  en' = energy ns'
  en = es / esToEn

  ns = V.map (*(en/en')) ns'

  r = V.zipWith (+) d ns

  energy :: V.Vector Double -> Double
  energy l = V.sum $ V.map (^2) l

shift :: SignalParams -> Double -> SignalData -> SignalData
shift p tau' d' = SignalData newXs newYs where
  d = ys d'
  n = V.length d

  newYs = (V.replicate off 0) V.++ d V.++ (V.replicate up 0) where
    off = fromTime p tau'
    up = n - off
  newXs = sampleTime p (V.length newYs)

correlate :: SignalData -> SignalData -> SignalData
correlate d1' d2' = SignalData ts ss where
  d1 = ys d1'
  d2 = ys d2'
  n1 = V.length d1
  n2 = V.length d2

  ss = V.generate n1 corr where
    corr i = V.sum $ V.zipWith (*) d1 (V.slice i n1 d2)
  ts = V.take n1 (xs d2')

noise :: StdGen -> [Double]
noise g = sum `fmap` chunksOf 12 (fmap normalize $ randoms g)

normalize :: Num a => a -> a
normalize = (subtract 1) . (*2)

toTime :: SignalParams -> Int -> Double
toTime p i = (fromIntegral i) / (sampling p)

fromTime :: SignalParams -> Double -> Int
fromTime p t = ceiling $ t * (sampling p)

sampleTime :: SignalParams -> Int -> V.Vector Double
sampleTime p n = V.generate n (toTime p)

argmax :: SignalData -> Double
argmax s = (xs s) V.! (V.maxIndex (ys s))
