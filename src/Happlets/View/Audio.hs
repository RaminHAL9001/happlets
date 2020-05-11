{-# LANGUAGE KindSignatures #-}

-- | This module provides an minimal "view" API for audio providers. It is a "view" in the sense
-- that this code is designed to output a signal -- although it is an audio signal, rather than a
-- video signal. It is minimalist in that it provides an abstraction for a pulse-code modulator
-- (PCM). At the time of this writing, most modern commercially available computer systems provide a
-- PCM with a 44.1 KHz sample rate, signed 16 bit samples per channel, and 2 (left and right) stereo
-- output channels.
--
-- APIs for compression and reading/writing audio to/from persistent storage are not provided, MIDI
-- or any kind of sequencing APIs are not provided either. If they are provided, it would be in a
-- separate package that provides a "Happlets.Control.MIDI" module.
--
-- These specifications are mostly good enough for general purpose use cases. For those evaluating
-- whether Happlets could be useful for audio signal processing and synthesis, Happlets may still be
-- useful for /previewing/ hi-fi synthesized audio output. Happlets is designed for programming GUIs
-- used by ordinary end users, not necessarily audio engineers, so rendering the actual hi-fi audio
-- output represented by the preview be done using a separate Haskell library.
--
-- Should this version of Happlets ever be ported to embedded devices that do not allow for specs as
-- high as these (say limited to 22,050 Hz sample rate 8-bit mono), it is recommended that
-- information provided by Happlets be dithered-down to fit within lower specs. The actual hardware
-- specifications provided should not be known to Happlet programmers.
--
-- Audio output and audio input are separated into different type classes, as audio input typically
-- requires additional security measures (you don't want hackers using your microphone and listening
-- to you over the internet), and some audio providers may not want to provide any audio recording
-- at all.
module Happlets.View.Audio
  ( -- * The PCM Function Type
    PCMControl, newPCMControl, signalPCMControl, checkPCMControl,
    PCM, runPCM, stereoPCM, monoPCM, StereoChannel, MonoChannel,
    -- * The StatePCM Function Type
    StatePCM(..), PCMRef, newPCMRef, readPCMRef,
    runStatePCM, getPCMRef, putPCMRef, modifyPCMRef,
    monoStatePCMGenerator, stereoStatePCMGenerator,
    monoStatePCMRecorder,  stereoStatePCMRecorder,
    -- ** Re-exporting "Control.Monad.State"
    module Control.Monad.State,
    -- * Lift PCM into StatePCM
    MonadPCM(..),
    -- * Callback Function Types
    PCMGenerator(..), PCMGenerateStereo, PCMGenerateMono,
    PCMRecorder(..),  PCMRecordStereo,   PCMRecordMono,
    -- ** PCMGenerator Constructors
    mapTimeToStereoPCM, mapTimeToMonoPCM, mapTimeToStereo, mapTimeToMono,
    -- * Common data types
    BufferSizeRequest, PCMActivation(..), FrameCounter,
    -- * Constants
    AudioRealApprox, Frequency, Duration, Moment, SampleCount, SampleIndex,
    Sample, LeftSample, RightSample, PulseCode, LeftPulseCode, RightPulseCode,
    audioSampleRate, unitQuanta, minFrequency, maxFrequency,
    -- * Conveniences
    toPulseCode, toSample, modTimeIndex, timeIndex, indexToTime,
    sampleCountDuration, durationSampleCount,
  ) where

import           Control.Arrow
import           Control.Concurrent
import           Control.Monad.Reader
import           Control.Monad.State

import qualified Data.Text as Strict
import           Data.Int

----------------------------------------------------------------------------------------------------

-- | Functions of type indicate a callback that is used to produce samples for a PCM 'AudioPlayback'
-- device, or to consume samples for a PCM 'AudioCapture' device. Callback functions of this type are
-- called repeatedly and rapidly in real time. To prevent other parts of the Happlets library from
-- interfearing too much with the operation of a PCM callback, communicating with a PCM is
-- restricted to sending signals via a mutex called the 'PCMControl'. A 'PCM' callback function will
-- poll a 'PCMControl' and decide how to behave on every iteration.
newtype PCM a = PCM (IO a)
  deriving (Functor, Applicative, Monad)

-- | This data type is essentially a mutex that can only be written in from within the 'GUI'
-- function, and can only be read out from within a 'PCM' function.
newtype PCMControl signal = PCMControl (MVar ([signal] -> [signal]))

-- | This data type allows a 'PCM' type of function to report information about itself to the world
-- outside of it's generator thread. The 'modifyPCMRef' is an signalling function that can be
-- called within a 'PCM' function, and 'readPCMRef' is an @IO@ function that allows other threads
-- to view the content of a 'PCMRef' register.
newtype PCMRef st = PCMRef (MVar st)

-- | The 'StatePCM' function type is simply a stateful version of the 'PCM' function. Evaluating the
-- 'runStatePCM' function takes a 'PCMRef' register which must be defined before hand using
-- 'newPCMRef'. Evaluating 'runStatePCM' will produce a 'PCMGenerator' or 'PCMRecorder' depending
-- on the calling context in which 'runStatePCM' is evaluated.
newtype StatePCM st a = StatePCM { unwrapStatePCM :: ReaderT (PCMRef st) PCM a }
  deriving (Functor, Applicative, Monad)

instance MonadState st (StatePCM st) where
  state = StatePCM . ReaderT . flip modifyPCMRef

runStatePCM :: PCMRef st -> StatePCM st a -> PCM a
runStatePCM status (StatePCM f) = runReaderT f status

pcmStateRunner :: Functor f => (f (PCM a) -> pcm) -> PCMRef st -> f (StatePCM st a) -> pcm
pcmStateRunner constr stat = constr . fmap (flip runReaderT stat . unwrapStatePCM)

-- | Construct a mono-signal 'PCMGenerator' from a 'StatePCM' function.
monoStatePCMGenerator :: PCMRef st -> (FrameCounter -> StatePCM st PulseCode) -> PCMGenerator
monoStatePCMGenerator = pcmStateRunner PCMGenerateMono

-- | Construct a stereo-signal 'PCMGenerator' from a 'StatePCM' function.
stereoStatePCMGenerator
  :: PCMRef st
  -> (FrameCounter -> StatePCM st (LeftPulseCode, RightPulseCode))
  -> PCMGenerator
stereoStatePCMGenerator = pcmStateRunner PCMGenerateStereo

-- | Construct mono-signal 'PCMRecorder' from a 'StatePCM' function.
monoStatePCMRecorder :: PCMRef st -> (FrameCounter -> PulseCode -> StatePCM st ()) -> PCMRecorder
monoStatePCMRecorder stat = pcmStateRunner (PCMRecordMono . curry) stat . uncurry

-- | Construct stereo-signal 'PCMRecorder' from a 'StatePCM' function.
stereoStatePCMRecorder
  :: PCMRef st
  -> (FrameCounter -> LeftPulseCode -> RightPulseCode -> StatePCM st ())
  -> PCMRecorder
stereoStatePCMRecorder stat f =
  pcmStateRunner (\ f -> PCMRecordStereo $ \ a b c -> f (a, b, c)) stat (\ (a, b, c) -> f a b c)

-- | This function should only be used by Happlet 'Happlets.Initialize.Provider's, or in the case
-- you want to test your own 'PCM' function and observe it's output.
--
-- To actually use a 'PCM' function in a Happlet program, wrap your 'PCM' function in the 'monoPCM'
-- or 'stereoPCM' constructor to construct a 'PCMGenerator', and then pass the 'PCMGenerator' to the
-- 'audioPlayback' function (in the 'AudioPlayback' typeclass, defined in the "Happlets.GUI"
-- module).
runPCM :: PCM a -> IO a
runPCM (PCM f) = f

-- | Create a new 'PCMControl'. Every call to 'checkPCMSignal' will return 'Nothing' until
-- 'signalPCM' is called with a value, at which point, the value will be returned by
-- 'checkPCMSignal' once and removed on every call after it.
newPCMControl :: IO (PCMControl signal)
newPCMControl = PCMControl <$> newMVar id

-- | This function allows a 'PCM' function to be lifted into the 'StatePCM' function. Any monad
-- transformer that lifts a 'PCM' can instantiate this class. It should be noted, however, that
-- there is only one function in this entire API which makes use of the 'MonadPCM' class, and that
-- is 'checkPCMControl'.
class Monad m => MonadPCM m where { liftPCM :: PCM a -> m a; }
instance MonadPCM PCM where { liftPCM = id; }
instance MonadPCM (StatePCM st) where { liftPCM = StatePCM . lift; }

-- | Poll a 'PCMControl' for whether @signal@ value has been stored into the 'PCMControl' via the
-- 'signalPCM' function.
checkPCMControl :: MonadPCM m => PCMControl signal -> m [signal]
checkPCMControl (PCMControl mvar) = liftPCM $ PCM $ liftIO $
  modifyMVar mvar $ return . (,) id . ($ [])

-- | Send a signal to a 'PCM' function via a 'PCMControl'.
signalPCMControl :: MonadIO m => PCMControl signal -> signal -> m ()
signalPCMControl (PCMControl mvar) signal = liftIO $ modifyMVar_ mvar $ return . (. (signal :))

-- | Define a new 'PCMRef' register. You must provide an initial @st@ value.
newPCMRef :: MonadIO m => st -> m (PCMRef st)
newPCMRef = liftIO . fmap PCMRef . newMVar

-- | A function which updates a 'PCMRef' register.
modifyPCMRef :: PCMRef st -> (st -> (a, st)) -> PCM a
modifyPCMRef (PCMRef mvar) = PCM . modifyMVar mvar . ((return . (\ (a,b)->(b,a))) .)

-- | A function which gets a copy of the 'PCMRef' register. This function can only be used from
-- within a 'PCM' function. To observe the 'PCMRef' from outside of a 'PCM' function, use
-- 'readPCMRef'.
getPCMRef :: PCMRef st -> PCM st
getPCMRef (PCMRef mvar) = PCM $ readMVar mvar

-- | Overwrite the value of a 'PCMRef' register.
putPCMRef :: PCMRef st -> st -> PCM ()
putPCMRef stat = modifyPCMRef stat . const . (,) ()

-- | A function which reads a 'PCMRef' register. As an @IO@ function, this function can be used
-- outside of a 'PCM' function, and cannot be used from within a 'PCM' function. To view the
-- 'PCMRef' from within a 'PCM' function, use 'getPCMRef'.
readPCMRef :: PCMRef st -> IO st
readPCMRef (PCMRef mvar) = readMVar mvar

----------------------------------------------------------------------------------------------------

-- | This class provides easier-to-remember constructors for 'PCMGenerateStereo', and
-- 'PCMRecordStereo'. The 'stereoPCM' functions will create the correct type of function (either
-- 'PCMGenerateStereo' or 'PCMRecordStereo') based on whether it is being passed as a value to the
-- the 'Happlets.GUI.audioPlayback' or 'Happlets.GUI.audioCapture' functions. For example:
--
-- @
-- myPlayback :: 'FrameCounter' -> 'PCM' MyPlayer ('LeftPulseCode', 'RightPulseCode')
-- myPlayback i = do { ... }
--
-- myRecorder :: 'FrameCounter' -> 'LeftPulseCode' -> 'RightPulseCode' -> PCM MyRecorder ()
-- myRecorder i left right = do { ... }
--
-- playMySound :: 'Happlets.GUI.GUI' win model ()
-- playMySound = 'Happlets.GUI.audioPlayback' $ 'stereoPCM' myPlayback
--
-- recordMySound :: 'Happlets.GUI.GUI' win model ()
-- recordMySound = 'Happlet.GUI.audioCapture' $ 'stereoPCM' myRecorder
-- @
--
-- Notice above how 'stereoPCM' can be used with both 'myPlaback' and 'myRecorder' even though the
-- function types are completely different. This is the convenience provided by the 'StereoChannel'
-- typeclass: using the same name for different types.
class StereoChannel pcm stereo | pcm -> stereo where
  stereoPCM :: stereo -> pcm

-- | Like tye StereoChannel' function, but for constructing 'PCMGenerateMono' and 'PCMRecordMono'
-- for use with the 'audioPlayback' or 'audioCapture' functions.
class MonoChannel pcm mono | pcm -> mono where
  monoPCM   :: mono   -> pcm

instance StereoChannel PCMGenerator (FrameCounter -> PCM (LeftPulseCode, RightPulseCode)) where
  stereoPCM = PCMGenerateStereo

instance MonoChannel PCMGenerator (FrameCounter -> PCM PulseCode) where
  monoPCM = PCMGenerateMono

instance StereoChannel PCMRecorder (FrameCounter -> LeftPulseCode -> RightPulseCode -> PCM ()) where
  stereoPCM = PCMRecordStereo

instance MonoChannel PCMRecorder (FrameCounter -> PulseCode -> PCM ()) where
  monoPCM = PCMRecordMono

----------------------------------------------------------------------------------------------------

-- | The buffer size request is a value measured in seconds.
--
-- This value is used by the 'startupAudioPlayback' function, and requests that PCM samples be saved to
-- a buffer that is flushed to the hardware PCM device only when the buffer is full. Smaller buffers
-- are desireable as it determines the amount of delay that exists between the moment in time that a
-- change in the 'PCMGenerator' function occurs and the moment in time that the hardware PCM device
-- begins receiving the changed PCM output.
-- 
-- For example, if you can tolerate a delay of 1 second between the time 'PCMGenerator' events are
-- sent and the time the hardwar PCM device receives the update, then set this value to @1.0@, and
-- (assuming a 44,100 Hz sample rate), you may be allocated a buffer large enough to hold of 44,100
-- samples. A value of @0.5@ may allocate a buffer large enough to hold 22,050 samples, a value of
-- @0.01@ may allocate a buffer for 441 samples. The actual buffer allocation is implementation
-- dependent, and the 'BufferSizeRequest' value may well be ignored entirely.
type BufferSizeRequest = Double

-- | This value serves as a report on the state of the PCM device, namely whether or not the PCM
-- device is active or not, or whether a recent call to 'startupAudioPlayback' or 'startupAudioCapture'
-- resulted in an error.
data PCMActivation
  = PCMDeactivated
  | PCMActivated
  | PCMError  Strict.Text
  deriving (Eq, Ord, Show)

-- | When performing computations related to audio signals, 32-bit floating point is used throughout
-- the Happlets library.
type AudioRealApprox = Float

-- | Units used for frequency values.
type Frequency = AudioRealApprox

-- | Units used for time duration values, which is the inverse of frequencies. 'Duration's can also
-- be thought of as wavelengths, but usually in audio applications the wavelength varies a lot
-- depending on the physical environment of the audio equipment, so it is better to think of
-- 'Duration' values measures of time intervals.
type Duration  = AudioRealApprox

-- | When computing sample values based on 'Frequency' it is convenient to think of the result of
-- the computation as the amplitude of a 'Sample' that is to be sent to the hardware PCM device. The
-- actual sample used is a signed 16-bit ('Int16') integer value, so it is necessary to convert
-- value of type 'Sample' to values of type 'Int16', for this use the 'toPulseCode' function.
type Sample    = AudioRealApprox

-- | A 'Sample' sent to the left stereo channel.
type LeftSample = Sample

-- | A 'Sample' sent to the right stereo channel.
type RightSample = Sample

-- | The actual format of the information sent to the hardware pulse code modulator (PCM) device
-- must be a signed 16-bit integer value.
type PulseCode = Int16

-- | A 'PulseCode' sent to or received from a left stereo channel.
type LeftPulseCode = PulseCode

-- | A 'PulseCode' sent to or received from a right stereo channel.
type RightPulseCode = PulseCode

-- | Units used for tracking moments in time. This is a time duration measured in relation to an
-- arbitrary initial time.
type Moment    = AudioRealApprox

-- | Units of this value are used to count the number of samples, e.g. the number of samples that
-- exist in some unit of time 'Duration', or the number of samples that can fit into a buffer in
-- memory.
type SampleCount n = n

-- | Units of this value are used as indicies into buffers in memory containing 'Sample' values.
type SampleIndex n = n

-- | The universal sample rate used by all Happlets. Whether or not the hardware PCM device runs at
-- this rate is not generally of any concern Happlet programmers.
audioSampleRate :: Frequency
audioSampleRate = 44100.0

-- | The reciporical of the 'audioSampleRate', this value is used quite often and it is useful to
-- have it defined as it's own constant.
unitQuanta :: Duration
unitQuanta = recip audioSampleRate

-- | The maximum frequency is the Nyquist frequency, which is defined as exactly one-half the
-- 'audioSampleRate'. This specifies the limit of the frequencies that can be reliably carried by
-- the hardware PCM device. This value is also used fairly often in bounds checking, and so it is
-- useful to have it defined as it's own constant.
maxFrequency :: Frequency
maxFrequency = audioSampleRate / 2.0

-- | The minimum frequency is arbitrarily set to 15.0 Hz, this is approximately the lowest possible
-- frequency detectable by human hearing.
minFrequency :: Frequency
minFrequency = 15.0

----------------------------------------------------------------------------------------------------

-- | Convert a floating-point 'Sample' value to a signed integer 'PulseCode' value. The given
-- 'Sample' is clamped to a value between of negative one and positive one, then multiplied by the
-- 'Prelude.maxBound' of the 'PulseCode' data type, then 'Prelude.round'ed to the nearest integer
-- value.
toPulseCode :: Sample -> PulseCode
toPulseCode =
  round . (* (realToFrac (maxBound :: PulseCode) :: AudioRealApprox)) . max (-1.0) . min 1.0

-- | Inverse of 'toPulseCode', returns a 'Sample' value between negative one and positive one.
toSample :: PulseCode -> Sample
toSample = (/ (0.5 + realToFrac (maxBound :: PulseCode))) . realToFrac

-- | Compute the number of 'unitQuanta' exist in the time interval between zero and the given
-- moment. This value can be used to select an index from a vector/array containing samples. The
-- 'Moment' value given may not be an exact integer multiple of the 'SampleIndex' returned, so a
-- "modulus" value (the remainder of an arithmetic division) indicating the 'Moment' in time where
-- the given 'Moment' lies relative to the returned 'SampleIndex' is returned paird with the
-- 'SampleIndex' value. This value can be used to antialias.
modTimeIndex :: forall n . Integral n => Moment -> (SampleIndex n, Moment)
modTimeIndex t = let { d = t * audioSampleRate; r = floor d :: n; } in (r, d - realToFrac r)

-- | Like 'modTimeIndex' but discards the modulus 'Moment' value.
timeIndex :: Integral n => Moment -> SampleIndex n
timeIndex = fst . modTimeIndex

-- | When converting some index of a 'Data.Vector.Vector' of 'Sample's in a quantized time domain
-- function, this function converts that index value into the point in time at which that sample
-- exists. This function is actually identical to 'sampleCountDuration'.
indexToTime :: Integral n => SampleIndex n -> Moment
indexToTime = (/ audioSampleRate) . realToFrac

-- | Convert a 'SampleCount' (the number of samples in a quantized time domain function) to a time
-- 'Duration' measured in seconds. This function is actually identical to 'indexToTime'.
sampleCountDuration :: Integral n => SampleCount n -> Duration
sampleCountDuration = indexToTime

-- | Convert a 'Duration' measured in seconds to a 'SampleCount' (the number of samples in a
-- quantized time domain function), and round up so the value returned represents the minimum number
-- of samples required to span the given time 'Duration'.
durationSampleCount :: Integral n => Duration -> SampleCount n
durationSampleCount = ceiling . (* audioSampleRate)

----------------------------------------------------------------------------------------------------

-- | A 'PCMGenerator' is a callback function for producing samples to be output to a PCM
-- device. Functions of this type are passed to the 'audioPlayback' function. There are two modes
-- of opeation: mono and stereo. It is expected that the callback is evaluated in it's own thread
-- that has exclusive access to the operating system's PCM output interface.
--
-- A 'FrameCounter' is passed to the callback. The frame counter can reliably be used to compute the
-- amount of time that has traversed since the PCM thread was first launched.
data PCMGenerator
  = PCMGenerateMono   (FrameCounter -> PCM PulseCode)
  | PCMGenerateStereo (FrameCounter -> PCM (LeftPulseCode, RightPulseCode))

-- | This is a type with the same name as the 'PCMGenerateStereo' constructor function. This type is
-- a shorthand for the type of the function parameter that you pass to the 'PCMGenerateStereo'
-- constructor function, which is why this type has the exact same name as the constructor.
type PCMGenerateStereo = (FrameCounter -> PCM (LeftPulseCode, RightPulseCode))

-- | This is a type with the same name as the 'PCMGenerateMono' constructor function. This type is a
-- shorthand for the type of the function parameter that you pass to the 'PCMGenerateMono'
-- constructor function, which is why this type has the exact same name as the constructor.
type PCMGenerateMono   = (FrameCounter -> PCM PulseCode)

-- | A 'PCMRecorder' is a callback function for consuming samples recieved from a PCM
-- device. Functions of this type are passed to the 'audioCapture' function. There are two modes
-- of opeation: mono and stereo. It is expected that the callback is evaluated in it's own thread
-- that has exclusive access to the operating system's PCM input interface.
--
-- A 'FrameCounter' is passed to the callback. The frame counter can reliably be used to compute the
-- amount of time that has traversed since the PCM thread was first launched.
data PCMRecorder
  = PCMRecordMono     (FrameCounter -> PulseCode -> PCM ())
  | PCMRecordStereo   (FrameCounter -> LeftPulseCode -> RightPulseCode -> PCM ())

-- | This is a type with the same name as the 'PCMRecordStereo' constructor function. This type is a
-- shorthand for the type of the function parameter that you pass to the 'PCMRecordStereo'
-- constructor function, which is why this type has the exact same name as the constructor.
type PCMRecordStereo = (FrameCounter -> LeftPulseCode -> RightPulseCode -> PCM ())

-- | This is a type with the same name as the 'PCMRecordMono' constructor function. This type is a
-- shorthand for the type of the function parameter that you pass to the 'PCMRecordMono' constructor
-- function, which is why this type has the exact same name as the constructor.
type PCMRecordMono   = (FrameCounter -> PulseCode -> PCM ())

----------------------------------------------------------------------------------------------------

-- | A frame is an information packet that describes the state of the hardware PCM device at single
-- moment in time. We assume the PCM device operates at 44,000 samples per second, therefore a
-- single frame describes the state of the PCM as it exists for 1/44,100th of a second. The
-- information in the frame depends on how many output channels there are, and the bit depth
type FrameCounter = Int64

mapPair :: (a -> fa) -> (a, a) -> (fa, fa)
mapPair f = f *** f

mapTimePCM
  :: ((FrameCounter -> PCM b) -> PCMGenerator) -> (a -> b)
  -> (Moment -> PCM a) -> PCMGenerator
mapTimePCM constr toPC f = constr $ fmap toPC . f . indexToTime

-- | Construct a 'PCMGenerator' from a pure function that generates stereo PCM 'Sample's using only
-- each 'Moment' in time as input.
mapTimeToStereoPCM :: (Moment -> PCM (LeftSample, RightSample)) -> PCMGenerator
mapTimeToStereoPCM = mapTimePCM PCMGenerateStereo (mapPair toPulseCode)

-- | Construct a 'PCMGenerator' from a pure function that generates mono PCM 'Sample's using only
-- each 'Moment' in time as input.
mapTimeToMonoPCM :: (Moment -> PCM Sample) -> PCMGenerator
mapTimeToMonoPCM = mapTimePCM PCMGenerateMono toPulseCode

mapTimePure
  :: ((FrameCounter -> PCM b) -> PCMGenerator) -> (a -> b)
  -> (Moment -> a) -> PCMGenerator
mapTimePure constr toPC = mapTimePCM constr toPC . (return .)

-- | Construct a 'PCMGenerator' from a pure function that generates stereo PCM 'Sample's using only
-- each 'Moment' in time as input.
mapTimeToStereo :: (Moment -> (LeftSample, RightSample)) -> PCMGenerator
mapTimeToStereo = mapTimePure PCMGenerateStereo (mapPair toPulseCode)

-- | Construct a 'PCMGenerator' from a pure function that generates mono PCM 'Sample's using only
-- each 'Moment' in time as input.
mapTimeToMono :: (Moment -> Sample) -> PCMGenerator
mapTimeToMono = mapTimePure PCMGenerateMono toPulseCode
