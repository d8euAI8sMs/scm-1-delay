module Lib
    ( someFunc
    ) where

import Interop
import Foreign.Ptr(FunPtr, freeHaskellFunPtr)

foreign import ccall dummy :: IO ()

defaultHsdCallbacks = mkHsdCallbacks {
    hsdPostInitCb = putStrLn "postInit",
    hsdPreShowCb = putStrLn "preShow",
    hsdPreExitCb = putStrLn "preExit"
}

someFunc :: IO ()
someFunc = do
    hsdInit
    hsdShowAndWait defaultHsdCallbacks
    hsdExit
