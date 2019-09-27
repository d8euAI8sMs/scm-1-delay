module Lib
    ( someFunc
    ) where

import Interop
import Signal hiding (Point(..))
import Util
import System.Random
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

data State = Empty | DemoState {
    signals :: Signals
} | SimState {
    it :: Int
}

newState = Empty
newSimState = SimState 0
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
            let sd = SimData 0 0.1 0.1 0.0
            hsdSetCurSim sd
            hsdRefresh
            return newSimState
        HsdacSimStep -> do
            putStrLn "simStep"
            let i = it s0
            let x = fromIntegral $ i + 1
                y = x * x * 0.01
            let sd = SimData x y (0.3 * y) (0.2 * y)
            hsdSetCurSim sd
            hsdRefresh
            return s0 { it = (it s0) + 1 }
        HsdacSimEnd -> do
            let i = fromIntegral $ it s0
            let sd = SimData i 1 0.3 0.8
            hsdSetCurSim sd
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
