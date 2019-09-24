module Lib
    ( someFunc
    ) where

import Interop
import Foreign.Ptr(FunPtr, freeHaskellFunPtr)

foreign import ccall dummy :: IO ()

defaultHsdCallbacks :: HsdCallbacks Int
defaultHsdCallbacks = mkHsdCallbacks {
    hsdPostInitCb = \s0 -> putStrLn "postInit" >> return s0,
    hsdPreShowCb = \s0 -> putStrLn "preShow" >> return s0,
    hsdActionCb = \s0 a -> case a of
        HsdacDemoStart -> do
            p <- hsdGetParams
            putStrLn $ show p
            let xs = take 10 $ [0..] :: [Double]
            let ys = take 10 $ [0,5..] :: [Double]
            let s = zipWith Point xs ys
            let gd = GenData 10 10 s s s s s s
            hsdSetSignals gd
            hsdRefresh
            return s0
        HsdacDemoStep -> do
            let xs = take 100 $ [0..] :: [Double]
            let ys = take 100 $ [0,3..] :: [Double]
            let s = zipWith Point xs ((^2) `fmap` ys)
            let dd = DemoData 100 s s s 10 11 12
            hsdSetCurDemo dd
            hsdRefresh
            return 0
        HsdacSimStart -> do
            putStrLn "simStart"
            let sd = SimData (fromIntegral s0) 0.1 0.1 0.0
            hsdSetCurSim sd
            hsdRefresh
            return s0
        HsdacSimStep -> do
            putStrLn "simStep"
            let x = fromIntegral (s0 + 1)
                y = x * x * 0.01
            let sd = SimData x y (0.3 * y) (0.2 * y)
            hsdSetCurSim sd
            hsdRefresh
            return (s0 + 1)
        HsdacSimEnd -> do
            let sd = SimData (fromIntegral (s0 + 2)) 1 0.3 0.8
            hsdSetCurSim sd
            hsdRefresh
            return 0
        _ -> return s0
        ,
    hsdPreExitCb = \s -> putStrLn "preExit" >> return s
}

someFunc :: IO ()
someFunc = do
    hsdInit
    let i = 0 :: Int
    hsdShowAndWait i defaultHsdCallbacks
    hsdExit
