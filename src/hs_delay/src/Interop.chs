#include <interop/types.h>

module Interop
    ( HsdCallbacks(..)
    , mkHsdCallbacks

    , HsdAction(..)

    , Point(..)
    , Params(..)
    , GenData(..)
    , SimData(..)
    , DemoData(..)

    , hsdGetParams
    , hsdSetSignals
    , hsdSetCurSim
    , hsdSetCurDemo
    , hsdStopSim

    , hsdInit
    , hsdRefresh
    , hsdExit
    , hsdShowAndWait
    ) where

import Control.Applicative(liftA, liftA2)
import Foreign.Ptr(Ptr, FunPtr, freeHaskellFunPtr, plusPtr)
import Foreign.C
import Foreign.Storable
import Data.Coerce(coerce)
import Data.IORef(IORef, newIORef, readIORef, writeIORef)
import Foreign.Marshal.Alloc(alloca)
import Foreign.Marshal.Array(peekArray, withArray)
import Foreign.Marshal.Utils(with)

-- enums

{#enum hsd_event as HsdEvent {underscoreToCase} deriving (Eq, Show)#}
{#enum hsd_action as HsdAction {underscoreToCase} deriving (Eq, Show)#}
{#enum hsd_mode as HsdMode {underscoreToCase} deriving (Eq, Show)#}

-- haskell types and pointers

data Params = Params {
    paramMode           :: HsdMode,
    paramSpan           :: Double,
    paramShift          :: Double,
    paramCarrier        :: Double,
    paramBitrate        :: Double,
    paramSamplingRate   :: Double,
    paramSnr            :: Double,
    paramSnrFrom        :: Double,
    paramSnrTo          :: Double,
    paramSnrCount       :: Int,
    paramNumOfTests     :: Int
} deriving (Eq, Show)

{#pointer *params_t as ParamsPtr -> Params#}

data Point = Point { x :: Double, y :: Double } deriving (Eq, Show)

{#pointer *point_t as PointPtr -> Point#}

data GenData = GenData {
    genSize         :: Int,
    shiftedGenSize  :: Int,
    dataAm          :: [Point],
    dataPm          :: [Point],
    dataFm          :: [Point],
    dataAmShifted   :: [Point],
    dataPmShifted   :: [Point],
    dataFmShifted   :: [Point]
} deriving (Eq, Show)

data GenDataRaw = GenDataRaw {
    rawGenSize          :: Int,
    rawShiftedGenSize   :: Int,
    rawDataAm           :: PointPtr,
    rawDataPm           :: PointPtr,
    rawDataFm           :: PointPtr,
    rawDataAmShifted    :: PointPtr,
    rawDataPmShifted    :: PointPtr,
    rawDataFmShifted    :: PointPtr
} deriving (Eq, Show)

{#pointer *gen_data_t as GenDataPtr -> GenDataRaw#}

data SimData = SimData {
    simCurSnr           :: Double,
    simProbAm           :: Double,
    simProbPm           :: Double,
    simProbFm           :: Double
} deriving (Eq, Show)

{#pointer *sim_data_t as SimDataPtr -> SimData#}

data DemoDataRaw = DemoDataRaw {
    rawCorrelationSize    :: Int,
    rawCorrelationDataAm  :: PointPtr,
    rawCorrelationDataPm  :: PointPtr,
    rawCorrelationDataFm  :: PointPtr,
    rawDemoShiftAm        :: Double,
    rawDemoShiftPm        :: Double,
    rawDemoShiftFm        :: Double
} deriving (Eq, Show)

data DemoData = DemoData {
    correlationSize    :: Int,
    correlationDataAm  :: [Point],
    correlationDataPm  :: [Point],
    correlationDataFm  :: [Point],
    demoShiftAm        :: Double,
    demoShiftPm        :: Double,
    demoShiftFm        :: Double
} deriving (Eq, Show)

{#pointer *demo_data_t as DemoDataPtr -> DemoDataRaw#}

-- storable instances

instance Storable Point where
    sizeOf _ = {#sizeof point_t#}
    alignment _ = {#alignof point_t#}
    peek p = Point <$> liftA coerce ({#get point_t.t #} p)
                   <*> liftA coerce ({#get point_t.x #} p)
    poke p s = do {#set point_t.t #} p (coerce $ x s)
                  {#set point_t.x #} p (coerce $ y s)

instance Storable Params where
    sizeOf _ = {#sizeof params_t#}
    alignment _ = {#alignof params_t#}
    peek p = Params <$> liftA (toEnum . fromIntegral)   ({#get params_t.mode #} p)
                    <*> liftA coerce                    ({#get params_t.span #} p)
                    <*> liftA coerce                    ({#get params_t.shift #} p)
                    <*> liftA coerce                    ({#get params_t.carrier #} p)
                    <*> liftA coerce                    ({#get params_t.bitrate #} p)
                    <*> liftA coerce                    ({#get params_t.sampling_rate #} p)
                    <*> liftA coerce                    ({#get params_t.snr #} p)
                    <*> liftA coerce                    ({#get params_t.snr_from #} p)
                    <*> liftA coerce                    ({#get params_t.snr_to #} p)
                    <*> liftA fromIntegral              ({#get params_t.snr_count #} p)
                    <*> liftA fromIntegral              ({#get params_t.num_of_tests #} p)
    -- poke is not really necessary, but provided for completeness
    poke p s = do {#set params_t.mode #} p (fromIntegral . fromEnum $ paramMode s)
                  {#set params_t.span #} p (coerce $ paramSpan s)
                  {#set params_t.shift #} p (coerce $ paramShift s)
                  {#set params_t.carrier #} p (coerce $ paramCarrier s)
                  {#set params_t.bitrate #} p (coerce $ paramBitrate s)
                  {#set params_t.sampling_rate #} p (coerce $ paramSamplingRate s)
                  {#set params_t.snr #} p (coerce $ paramSnr s)
                  {#set params_t.snr_from #} p (coerce $ paramSnrFrom s)
                  {#set params_t.snr_to #} p (coerce $ paramSnrTo s)
                  {#set params_t.snr_count #} p (fromIntegral $ paramSnrCount s)
                  {#set params_t.num_of_tests #} p (fromIntegral $ paramNumOfTests s)

-- it is not possible to do that:
--
--     instance Storable GenData where
--         peek p = GenData <$> count <*> countShifted
--                          <*> ((\a -> (peekArray a) =<< ({#get gen_data_t.data_am_base #} p))    =<< count)
--                          ...
--         poke p s = do ...
--                       withArray ...
--
-- since allocated array is invalid outside withArray function

instance Storable GenDataRaw where
    sizeOf _ = {#sizeof gen_data_t#}
    alignment _ = {#alignof gen_data_t#}
    -- peek is provided in demonstration purpose only
    peek p = GenDataRaw <$> liftA fromIntegral ({#get gen_data_t.count_base #} p)
                        <*> liftA fromIntegral ({#get gen_data_t.count_shifted #} p)
                        <*> ({#get gen_data_t.data_am_base #} p)
                        <*> ({#get gen_data_t.data_pm_base #} p)
                        <*> ({#get gen_data_t.data_fm_base #} p)
                        <*> ({#get gen_data_t.data_am_shifted #} p)
                        <*> ({#get gen_data_t.data_pm_shifted #} p)
                        <*> ({#get gen_data_t.data_fm_shifted #} p)
    poke p s = do {#set gen_data_t.count_base #} p (fromIntegral $ rawGenSize s)
                  {#set gen_data_t.count_shifted #} p (fromIntegral $ rawShiftedGenSize s)
                  {#set gen_data_t.data_am_base #} p (rawDataAm s)
                  {#set gen_data_t.data_pm_base #} p (rawDataPm s)
                  {#set gen_data_t.data_fm_base #} p (rawDataFm s)
                  {#set gen_data_t.data_am_shifted #} p (rawDataAmShifted s)
                  {#set gen_data_t.data_pm_shifted #} p (rawDataPmShifted s)
                  {#set gen_data_t.data_fm_shifted #} p (rawDataFmShifted s)

withRawGenData :: GenData -> (GenDataRaw -> IO b) -> IO b
withRawGenData g f = withArray (dataAm g) $ \amPtr ->
                     withArray (dataPm g) $ \pmPtr ->
                     withArray (dataFm g) $ \fmPtr ->
                     withArray (dataAmShifted g) $ \amShiftedPtr ->
                     withArray (dataPmShifted g) $ \pmShiftedPtr ->
                     withArray (dataFmShifted g) $ \fmShiftedPtr ->
                        f $ GenDataRaw (genSize g) (shiftedGenSize g) amPtr pmPtr fmPtr amShiftedPtr pmShiftedPtr fmShiftedPtr

instance Storable SimData where
    sizeOf _ = {#sizeof sim_data_t#}
    alignment _ = {#alignof sim_data_t#}
    peek p = SimData <$> liftA coerce ({#get sim_data_t.snr #} p)
                     <*> liftA coerce ({#get sim_data_t.prob_am #} p)
                     <*> liftA coerce ({#get sim_data_t.prob_pm #} p)
                     <*> liftA coerce ({#get sim_data_t.prob_fm #} p)
    poke p s = do {#set sim_data_t.snr #} p (coerce $ simCurSnr s)
                  {#set sim_data_t.prob_am #} p (coerce $ simProbAm s)
                  {#set sim_data_t.prob_pm #} p (coerce $ simProbPm s)
                  {#set sim_data_t.prob_fm #} p (coerce $ simProbFm s)

instance Storable DemoDataRaw where
    sizeOf _ = {#sizeof demo_data_t#}
    alignment _ = {#alignof demo_data_t#}
    peek p = DemoDataRaw <$> liftA fromIntegral ({#get demo_data_t.correlation_count #} p)
                         <*> ({#get demo_data_t.correlation_am #} p)
                         <*> ({#get demo_data_t.correlation_pm #} p)
                         <*> ({#get demo_data_t.correlation_fm #} p)
                         <*> liftA coerce ({#get demo_data_t.shift_am #} p)
                         <*> liftA coerce ({#get demo_data_t.shift_pm #} p)
                         <*> liftA coerce ({#get demo_data_t.shift_fm #} p)
    poke p s = do {#set demo_data_t.correlation_count #} p (fromIntegral $ rawCorrelationSize s)
                  {#set demo_data_t.correlation_am #} p (rawCorrelationDataAm s)
                  {#set demo_data_t.correlation_pm #} p (rawCorrelationDataPm s)
                  {#set demo_data_t.correlation_fm #} p (rawCorrelationDataFm s)
                  {#set demo_data_t.shift_am #} p (coerce $ rawDemoShiftAm s)
                  {#set demo_data_t.shift_pm #} p (coerce $ rawDemoShiftPm s)
                  {#set demo_data_t.shift_fm #} p (coerce $ rawDemoShiftFm s)

withRawDemoData :: DemoData -> (DemoDataRaw -> IO b) -> IO b
withRawDemoData g f = withArray (correlationDataAm g) $ \amPtr ->
                      withArray (correlationDataPm g) $ \pmPtr ->
                      withArray (correlationDataFm g) $ \fmPtr ->
                         f $ DemoDataRaw (correlationSize g) amPtr pmPtr fmPtr (demoShiftAm g) (demoShiftPm g) (demoShiftFm g)

-- functions and callbacks

{#fun unsafe hsd_get_params as hsdGetParams' {`ParamsPtr'} -> `()' #}
{#fun unsafe hsd_set_signals as hsdSetSignals' {`GenDataPtr'} -> `()' #}
{#fun unsafe hsd_set_cur_demo as hsdSetCurDemo' {`DemoDataPtr'} -> `()' #}
{#fun unsafe hsd_set_cur_sim as hsdSetCurSim' {`SimDataPtr'} -> `()' #}
{#fun unsafe hsd_stop_sim as hsdStopSim {} -> `()' #}

hsdGetParams :: IO Params
hsdGetParams = alloca $ \p -> do
    hsdGetParams' p
    peek p

hsdSetSignals :: GenData -> IO ()
hsdSetSignals g = withRawGenData g $ \d ->
    with d $ hsdSetSignals'

hsdSetCurSim :: SimData -> IO ()
hsdSetCurSim d = with d $ hsdSetCurSim'

hsdSetCurDemo :: DemoData -> IO ()
hsdSetCurDemo g = withRawDemoData g $ \d ->
    with d $ hsdSetCurDemo'

foreign import ccall hsd_init :: IO ()
foreign import ccall hsd_refresh :: IO ()
foreign import ccall hsd_exit :: IO ()

foreign import ccall "wrapper"
    wrapCbIO :: (CInt -> CInt -> Ptr () -> IO ()) -> IO (FunPtr (CInt -> CInt -> Ptr () -> IO ()))

foreign import ccall "hsd_show_and_wait"
    hsd_show_and_wait :: FunPtr (CInt -> CInt -> Ptr () -> IO ()) -> IO ()

hsdInit     = hsd_init
hsdRefresh  = hsd_refresh
hsdExit     = hsd_exit

data HsdCallbacks s = HsdCallbacks {
    hsdPostInitCb   :: s -> IO s,
    hsdPreExitCb    :: s -> IO s,
    hsdPreShowCb    :: s -> IO s,
    hsdPreHideCb    :: s -> IO s,
    hsdRefreshCb    :: s -> IO s,
    hsdActionCb     :: s -> HsdAction -> IO s
}

mkHsdCallbacks = HsdCallbacks {
    hsdPostInitCb   = \s -> return s,
    hsdPreExitCb    = \s -> return s,
    hsdPreShowCb    = \s -> return s,
    hsdPreHideCb    = \s -> return s,
    hsdRefreshCb    = \s -> return s,
    hsdActionCb     = \s _ -> return s
}

selectCb :: IORef s -> HsdCallbacks s -> CInt -> CInt -> Ptr () -> IO ()
selectCb s cb e = case (toEnum . fromIntegral) e of
    HsdevPostInit       -> \_ _ -> withState s $ \s0 -> (hsdPostInitCb cb) s0
    HsdevPreExit        -> \_ _ -> withState s $ \s0 -> (hsdPreExitCb cb) s0
    HsdevPreShow        -> \_ _ -> withState s $ \s0 -> (hsdPreShowCb cb) s0
    HsdevPreHide        -> \_ _ -> withState s $ \s0 -> (hsdPreHideCb cb) s0
    HsdevPostRefresh    -> \_ _ -> withState s $ \s0 -> (hsdRefreshCb cb) s0
    HsdevOnAction       -> \a _ -> withState s $ \s0 -> (hsdActionCb cb) s0 ((toEnum . fromIntegral) a)
    unmatched           -> error ("Unexpected enum value " ++ show unmatched)

withState :: IORef s -> (s -> IO s) -> IO ()
withState s f = do
    s0 <- readIORef s
    s1 <- f s0
    writeIORef s s1

hsdShowAndWait :: s -> HsdCallbacks s -> IO()
hsdShowAndWait s0 cb = do
    s <- newIORef s0
    cbW <- wrapCbIO $ selectCb s cb
    hsd_show_and_wait cbW
