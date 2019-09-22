module Interop
    ( HsdCallbacks(..)
    , mkHsdCallbacks

    , hsdInit
    , hsdRefresh
    , hsdExit
    , hsdShowAndWait
    ) where

import Foreign.Ptr(Ptr, FunPtr, freeHaskellFunPtr)
import Foreign.C

foreign import ccall hsd_init :: IO ()
foreign import ccall hsd_refresh :: IO ()
foreign import ccall hsd_exit :: IO ()

foreign import ccall "wrapper"
    wrapCbIO :: (CInt -> CInt -> (Ptr ()) -> IO ()) -> IO (FunPtr (CInt -> CInt -> (Ptr ()) -> IO ()))

foreign import ccall "interop/types.h hsd_show_and_wait"
    hsd_show_and_wait :: FunPtr (CInt -> CInt -> (Ptr ()) -> IO ()) -> IO ()

hsdInit     = hsd_init
hsdRefresh  = hsd_refresh
hsdExit     = hsd_exit

data HsdCallbacks = HsdCallbacks {
    hsdPostInitCb :: IO (),
    hsdPreExitCb :: IO (),
    hsdPreShowCb :: IO (),
    hsdPreHideCb :: IO (),
    hsdRefreshCb :: IO ()
}

mkHsdCallbacks = HsdCallbacks {
    hsdPostInitCb = return (),
    hsdPreExitCb = return (),
    hsdPreShowCb = return (),
    hsdPreHideCb = return (),
    hsdRefreshCb = return ()
}

selectCb :: HsdCallbacks -> CInt -> CInt -> (Ptr ()) -> IO ()
selectCb cb e = case e of
    0 -> \_ _ -> hsdPostInitCb cb
    1 -> \_ _ -> hsdPreExitCb cb
    2 -> \_ _ -> hsdPreShowCb cb
    3 -> \_ _ -> hsdPreHideCb cb
    4 -> \_ _ -> hsdRefreshCb cb
    _ -> \_ _ -> return ()

hsdShowAndWait :: HsdCallbacks -> IO()
hsdShowAndWait cb = do
    cbW <- wrapCbIO $ selectCb cb
    hsd_show_and_wait cbW
