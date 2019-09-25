module Signal
  ( Modulation(..)
  , SignalParams(..)
  , Point(..)
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

data Modulation = AM | PM | FM deriving (Eq, Show)

data SignalParams = SignalParams {
  timespan    :: Double,
  carrier     :: Double,
  sampling    :: Double,
  bitrate     :: Double
} deriving (Eq, Show)

data Point = Point {
  x   :: Double,
  y   :: Double
} deriving (Eq, Show)

newtype BinData = BinData { binData :: [Bool] } deriving (Eq, Show)
newtype SignalData = SignalData { signalData :: [Point] } deriving (Eq, Show)

data GenState = GenState {
  gstAmDataTail :: BinData,
  gstAmDataIdx  :: Int
} deriving (Eq, Show)

mkSignal :: SignalParams -> BinData -> Modulation -> SignalData
mkSignal p d m = runST $ do
  s <- newSTRef $ GenState d 0
  SignalData <$> (((gen' s) `mapM` (take n [0..]))) where
    n = ceiling ((timespan p) * (sampling p)) :: Int

    gen' :: STRef a GenState -> Int -> ST a Point
    gen' s i = do
      s0 <- readSTRef s
      let (s1, p) = gen s0 i
      writeSTRef s s1
      return p

    gen :: GenState -> Int -> (GenState, Point)
    gen st0 i = (st, Point t v) where
      -- unpack current state essentials
      (d0:ds)     = (binData . gstAmDataTail) st0
      di0         = gstAmDataIdx st0
      -- signal-related params
      t           = toTime i
      v           = case m of
        AM -> a * sin (2 * pi * f * t)              where a  = toReal d0
        PM -> sin (2 * pi * f * t + b * pi)         where b  = toReal d0
        FM -> sin (2 * pi * (f + df) * t + a * pi)  where a  = toReal d0
                                                          df = b * br / 2 where b  = toSgn d0
                                                                                br = (bitrate p)
        where f = carrier p
      -- generate next state
      di1         = toSeqIdx t1 where t1 = toTime (i + 1)
      st          | di0 == di1  = st0
                  | otherwise   = GenState (BinData ds) di1
      -- helpers
      toTime i0   = (fromIntegral i0) / (sampling p)
      toReal d0   = fromIntegral . fromEnum $ d0 :: Double
      toSgn d0    = fromIntegral . (normalize . fromEnum) $ d0 :: Double
      toSeqIdx t0 = round (t0 * (bitrate p)) :: Int

noisify :: StdGen -> Double -> SignalData -> SignalData
noisify g snr d' = SignalData r where
  d = signalData d'
  n = length d
  ns' = take n $ noise g
  
  esToEn = exp (snr / 10)
  es = energy $ fmap y d
  en' = energy $ ns'
  en = es / esToEn

  ns = fmap (*(en/en')) ns'

  r = zipWith pls d ns where
    pls pt n0 = pt { y = (y pt) + n0 }

  energy :: [Double] -> Double
  energy l = foldl (+) 0 $ fmap (^2) l

shift :: SignalParams -> Double -> SignalData -> SignalData
shift p tau' d' = SignalData ((zeros 0 off) ++ (move tau d) ++ (zeros next up)) where
  d = signalData d'
  n = length d
  off = fromTime tau'
  tau = toTime off
  next = off + n + 1
  up = n - off

  move :: Double -> [Point] -> [Point]
  move t0 pts = fmap gen pts where
    gen p = p { x = (x p) + t0 }

  zeros :: Int -> Int -> [Point]
  zeros s n = take n $ zeros_ s

  zeros_ :: Int -> [Point]
  zeros_ s = gen `fmap` [s..] where
    gen i = Point (toTime i) 0

  toTime i0 = (fromIntegral i0) / (sampling p)
  fromTime t0 = ceiling $ t0 * (sampling p) :: Int

correlate :: SignalData -> SignalData -> SignalData
correlate d1' d2' = SignalData r where
  d1 = signalData d1'
  d2 = signalData d2'
  n1 = length d1
  n2 = length d2

  r = go d1 d2 n2 where
    go d1 d2 n2 | n2 < n1   = []
                | otherwise = (corr d1 d2) : (go d1 d2' (n2-1)) where
                  (_:d2') = d2
    corr d1 d2 = Point (x t) (sum $ zipWith (*) (seq d1) (seq d2)) where
      (t:_)  = d2
      seq ds = fmap y ds

noise :: StdGen -> [Double]
noise g = sum `fmap` chunksOf 12 (fmap normalize $ randoms g)

normalize :: Num a => a -> a
normalize a = a * 2 - 1

argmax :: SignalData -> Double
argmax s = go (signalData s) where
  go (p0:ps) = go' ps p0
  go' [] p = x p
  go' (p0:ps) p = go' ps (max' p0 p) where
    max' p1 p2 | (y p1 < y p2) = p2
               | otherwise     = p1
