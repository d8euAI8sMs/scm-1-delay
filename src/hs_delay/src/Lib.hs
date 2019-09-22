module Lib
    ( someFunc
    ) where

foreign import ccall dummy :: IO ()

someFunc :: IO ()
someFunc = dummy
