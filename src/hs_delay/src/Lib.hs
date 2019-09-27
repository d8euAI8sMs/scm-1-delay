module Lib
    ( someFunc
    ) where

import Interop
import Signal hiding (Point(..))
import Util
import System.Random
import Control.Parallel.Strategies
import Control.Monad(replicateM)
import qualified Data.Vector.Primitive as V

data Signals = Signals {
    am :: (SignalData, SignalData),
    pm :: (SignalData, SignalData),
    fm :: (SignalData, SignalData)
}

data Correlations = Correlations {
    amCors :: (Double, SignalData),
    pmCors :: (Double, SignalData),
    fmCors :: (Double, SignalData)
}

newtype Probs = Probs (Double, Double, Double)

data State = Empty | DemoState {
    signals :: Signals
} | SimState {
    params  :: Params,
    rands   :: BinData,
    it      :: Int
}

newState = Empty
newSimState p bd = SimState p bd 0
newDemoState s = DemoState s

mkGenData :: Signals -> GenData
mkGenData s = GenData n ns (ptpt $ fst $ am s)
                           (ptpt $ fst $ pm s)
                           (ptpt $ fst $ fm s)
                           (ptpt $ snd $ am s)
                           (ptpt $ snd $ pm s)
                           (ptpt $ snd $ fm s) where
    n  = V.length $ xs $ fst (am s)
    ns = V.length $ xs $ snd (am s)

mkSignals :: StdGen -> SignalParams -> Double -> Double -> BinData -> Signals
mkSignals g p tau snr bd = Signals { am = mk AM, pm = mk PM, fm = mk FM } where
    mk m = (d1, d2) where
        d1' = mkSignal p bd m
        d2' = shift p tau d1'
        d1  = noisify g 10 d1'
        d2  = noisify g snr d2'

correlateAll :: Signals -> Correlations
correlateAll (Signals am pm fm) = Correlations (corr am) (corr pm) (corr fm) where
    corr (s1, s2) = (tau, pts) where
        pts = correlate s1 s2
        tau = argmax pts

mkDemoData :: Correlations -> DemoData
mkDemoData cs = DemoData n (ptpt $ snd $ amCors cs)
                           (ptpt $ snd $ pmCors cs)
                           (ptpt $ snd $ fmCors cs)
                           (fst $ amCors cs)
                           (fst $ pmCors cs)
                           (fst $ fmCors cs) where
    n = V.length $ xs $ snd (amCors cs)

-- TODO: I hope we may do this much better...
simStep' :: StdGen -> [StdGen] -> SignalParams -> Modulation -> BinData -> Double -> Int -> Double
simStep' g gs p m bd snr n = (sum $ parMap rdeepseq gen $ take n $ zipWith (,) gs (randomRs (0, timespan p) g)) / (fromIntegral n) where
    gen (g', ts) | ok r      = 1
                 | otherwise = 0 where
        ok r = (abs (r - ts)) < (1 / (bitrate p))
        r = argmax corr
        corr = correlate s1 s2
        s1' = mkSignal p bd m
        s2' = shift p ts s1
        s1  = noisify g' 10 s1'
        s2  = noisify g' snr s2'

simStep g gs p bd snr n = Probs (simStep' g gs p AM bd snr n, simStep' g gs p PM bd snr n, simStep' g gs p FM bd snr n)

mkSimData :: Double -> Probs -> SimData
mkSimData snr (Probs (am, pm, fm)) = SimData snr am pm fm

defaultHsdCallbacks :: HsdCallbacks State
defaultHsdCallbacks = mkHsdCallbacks {
    hsdPostInitCb = \s0 -> putStrLn "postInit" >> return s0,
    hsdPreShowCb = \s0 -> putStrLn "preShow" >> return s0,
    hsdActionCb = \s0 a -> case a of
        HsdacDemoStart -> do
            p <- hsdGetParams
            putStrLn $ show p
            gen <- newStdGen
            let bd = BinData (randoms gen :: [Bool])
            let s = mkSignals gen (mkSignalParams p) (paramShift p) (paramSnr p) bd
            let gd = mkGenData s
            hsdSetSignals gd
            putStrLn $ show (genSize gd)
            hsdRefresh
            return (newDemoState s)
        HsdacDemoStep -> do
            let s = signals s0
            let cs = correlateAll s
            let dd = mkDemoData cs
            hsdSetCurDemo dd
            hsdRefresh
            return newState
        HsdacSimStart -> do
            putStrLn "simStart"
            p <- hsdGetParams
            putStrLn $ show p
            gen <- newStdGen
            let bd = BinData (randoms gen :: [Bool])
            hsdRefresh
            return $ newSimState p bd
        HsdacSimStep -> do
            gen <- newStdGen
            putStrLn "simStep"
            let p = params s0
            let snrStep = ((paramSnrFrom p) - (paramSnrTo p)) / (fromIntegral $ paramSnrCount p)
            let snr = (paramSnrFrom p) - (fromIntegral $ it s0) * snrStep
            -- an awful solution to 'precreate' a known number of generators
            -- and pass all of them to simulation iterator
            -- TODO: migrate to some sort of State monad to manage all of this
            gs <- replicateM (paramNumOfTests p) newStdGen
            let res = simStep gen gs (mkSignalParams p) (rands s0) snr (paramNumOfTests p)
            let res' = mkSimData snr res
            putStrLn $ show res'
            hsdSetCurSim $ res'
            hsdRefresh
            if (it s0) >= (paramSnrCount p)
                then hsdStopSim >> return newState
                else return s0 { it = (it s0) + 1 }
        HsdacSimEnd -> do
            hsdRefresh
            return newState
        _ -> return s0
        ,
    hsdPreExitCb = \s -> putStrLn "preExit" >> return s
}

someFunc :: IO ()
someFunc = do
    hsdInit
    hsdShowAndWait newState defaultHsdCallbacks
    hsdExit
