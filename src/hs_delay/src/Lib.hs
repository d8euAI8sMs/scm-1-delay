module Lib
    ( someFunc
    ) where

import Interop
import Foreign.Ptr(FunPtr, freeHaskellFunPtr)

foreign import ccall dummy :: IO ()

defaultHsdCallbacks = mkHsdCallbacks {
    hsdPostInitCb = putStrLn "postInit",
    hsdPreShowCb = putStrLn "preShow",
    hsdActionCb = \a -> case a of
        HsdacDemoStart -> do
            p <- hsdGetParams
            putStrLn $ show p
            let xs = take 10 $ [0..] :: [Double]
            let ys = take 10 $ [0,5..] :: [Double]
            let s = zipWith Point xs ys
            let gd = GenData 10 10 s s s s s s
            hsdSetSignals gd
            hsdRefresh
        HsdacDemoStep -> do
            let xs = take 100 $ [0..] :: [Double]
            let ys = take 100 $ [0,3..] :: [Double]
            let s = zipWith Point xs ((^2) `fmap` ys)
            let dd = DemoData 100 s s s 10 11 12
            hsdSetCurDemo dd
            hsdRefresh
        HsdacSimStart -> do
            putStrLn "simStart"
            let sd = SimData 0 0.1 0.1 0.0
            hsdSetCurSim sd
            hsdRefresh
        HsdacSimStep -> do
            putStrLn "simStep"
            let sd = SimData 1 0.6 0.3 0.2
            hsdSetCurSim sd
            hsdRefresh
        HsdacSimEnd -> do
            let sd = SimData 2 1 0.3 0.8
            hsdSetCurSim sd
            hsdRefresh
        _ -> return ()
        ,
    hsdPreExitCb = putStrLn "preExit"
}

someFunc :: IO ()
someFunc = do
    hsdInit
    hsdShowAndWait defaultHsdCallbacks
    hsdExit
