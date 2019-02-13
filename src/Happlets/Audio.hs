-- | This module provides an minimal API for audio providers. Minimalist means it allows for access
-- to a pulse-code modulator (PCM). At the time of this writing, most modern commercially available
-- computer systems provide a PCM with a 44.1 KHz sample rate, signed 16 bit samples per channel,
-- and 2 (left and right) stereo output channels.
--
-- APIs for compression and reading/writing audio to/from persistent storage are not provided, MIDI
-- or any kind of sequencing APIs are not provided either.
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
module Happlets.Audio
  ( BufferSizeRequest, PCMActivation, FrameCounter,
    AudioOutput(..), PCMGenerator(..),
    AudioInput(..), PCMRecorder(..),
    -- * Constants
    AudioRealApprox, Frequency, Duration, Sample, PulseCode, Moment, SampleCount,
    audioSampleRate, unitQuanta, minFrequency, maxFrequency,
    -- * Conveniences
    toPulseCode, toSample, modTimeIndex, timeIndex, indexToTime,
    sampleCountDuration, durationSampleCount,
  ) where

import           Control.Monad.IO.Class

import qualified Data.Text as Strict
import           Data.Int

----------------------------------------------------------------------------------------------------

-- | The buffer size request is a value measured in seconds.
--
-- This value is used by the 'activatePCMOutput' function, and requests that PCM samples be saved to
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
-- device is active or not, or whether a recent call to 'activatePCMOutput' or 'activatePCMInput'
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

-- | The actual format of the information sent to the hardware pulse code modulator (PCM) device
-- must be a signed 16-bit integer value.
type PulseCode = Int16

-- | A 'PulseCode' sent to or received from a left-stereo channel.
type LeftPulseCode = PulseCode

-- | A 'PulseCode' sent to or received from a right-stereo channel.
type RightPulseCode = PulseCode

-- | Units used for tracking moments in time. This is a time duration measured in relation to an
-- arbitrary initial time.
type Moment    = AudioRealApprox

-- | Units of this value are used to count the number of samples, e.g. the number of samples that
-- exist in some unit of time 'Duration', or the number of samples that can fit into a buffer in
-- memory.
type SampleCount = Int

-- | Units of this value are used as indicies into buffers in memory containing 'Sample' values.
type SampleIndex = Int

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
modTimeIndex :: Moment -> (SampleIndex, Moment)
modTimeIndex t = let { d = t * audioSampleRate; r = floor d :: Int; } in (r, d - realToFrac r)

-- | Like 'modTimeIndex' but discards the modulus 'Moment' value.
timeIndex :: Moment -> SampleIndex
timeIndex = fst . modTimeIndex

-- | When converting some index of a 'Data.Vector.Vector' of 'Sample's in a quantized time domain
-- function, this function converts that index value into the point in time at which that sample
-- exists. This function is actually identical to 'sampleCountDuration'.
indexToTime :: SampleIndex -> Moment
indexToTime = (/ audioSampleRate) . realToFrac

-- | Convert a 'SampleCount' (the number of samples in a quantized time domain function) to a time
-- 'Duration' measured in seconds. This function is actually identical to 'indexToTime'.
sampleCountDuration :: SampleCount -> Duration
sampleCountDuration = indexToTime

-- | Convert a 'Duration' measured in seconds to a 'SampleCount' (the number of samples in a
-- quantized time domain function), and round up so the value returned represents the minimum number
-- of samples required to span the given time 'Duration'.
durationSampleCount :: Duration -> SampleCount
durationSampleCount = ceiling . (* audioSampleRate)

----------------------------------------------------------------------------------------------------

-- | A 'PCMGenerator' is a callback function for producing samples to be output to a PCM
-- device. Functions of this type are passed to the 'setPCMGenerator' function. There are two modes
-- of opeation: mono and stereo. It is expected that the callback is evaluated in it's own thread
-- that has exclusive access to the operating system's PCM output interface. The thread shall also
-- maintain an arbitrary state value of type @st@ and pass this value to the callback every time the
-- callback is evaluated, and accept an updated @st@ value after the callback returns to be used for
-- the next call to the callback.
--
-- Along with the state value @st@, a 'FrameCounter' is also passed to the callback. The frame
-- counter can reliably be used to compute the amount of time that has traversed since the PCM
-- thread was first launched.
data PCMGenerator pcm st
  = PCMGenerateMono   (FrameCounter -> st -> pcm (st, PulseCode))
  | PCMGenerateStereo (FrameCounter -> st -> pcm (st, (LeftPulseCode, RightPulseCode)))

-- | A 'PCMRecorder' is a callback function for consuming samples recieved from a PCM
-- device. Functions of this type are passed to the 'setPCMRecorder' function. There are two modes
-- of opeation: mono and stereo. It is expected that the callback is evaluated in it's own thread
-- that has exclusive access to the operating system's PCM input interface. The thread shall also
-- maintain an arbitrary state value of type @st@ and pass this value to the callback every time the
-- callback is evaluated, and accept an updated @st@ value after the callback returns to be used for
-- the next call to the callback.
--
-- Along with the state value @st@, a 'FrameCounter' is also passed to the callback. The frame
-- counter can reliably be used to compute the amount of time that has traversed since the PCM
-- thread was first launched.
data PCMRecorder pcm st
  = PCMRecordMono     (FrameCounter -> st -> PulseCode -> pcm st)
  | PCMRecordStereo   (FrameCounter -> st -> LeftPulseCode -> RightPulseCode -> pcm st)

-- | A frame is an information packet that describes the state of the hardware PCM device at single
-- moment in time. We assume the PCM device operates at 44,000 samples per second, therefore a
-- single frame describes the state of the PCM as it exists for 1/44,100th of a second. The
-- information in the frame depends on how many output channels there are, and the bit depth
type FrameCounter = Int64

----------------------------------------------------------------------------------------------------

-- | The typeclass of monadic functions which include stateful information for communicating with
-- an audio output PCM device.
class (Functor pcm, Applicative pcm, Monad pcm, MonadIO pcm) => AudioOutput pcm where
  -- | The PCM signal generator is a callback function which produces pulse code samples to be sent
  -- to a pulse code modulator (PCM). This function can be called at any time, the @pcm@ function is
  -- expected to keep a reference to the given callback. When 'activatePCMOutput' is executed, a
  -- thread is launched which loops over calling the 'PCMGenerator' to generate each PCM sample. If
  -- this function is called while the PCM output is currently activated, the new PCM generator will
  -- not replace the old one until the current PCM buffer has been filled and sent to the operating
  -- system. Depending on the buffer size, this may take a noticable amount of time. Ultimately the
  -- amount of delay is unspecified and implementation dependent.
  --
  -- It is not a good idea to rely on this 'setPCMGenerator' function to do sequencing, rather the
  -- 'PCMGenerator' callback function should be initialized with access to an
  -- 'Control.Concurrent.MVar.MVar' which polls for information about what signal to generate, when,
  -- and for how long.
  --
  -- The 'PCMGenerator' takes an arbitrary state value @st@, and this state value may be updated
  -- over the course of sample generation.
  setPCMGenerator     :: PCMGenerator pcm st -> st -> pcm ()

  -- | This function must request of the operating system to begin sending information to the
  -- hardware PCM device, and launch a thread that loops over calling the 'PCMGenerator' callback
  -- that was set by the 'setPCMGenerator' function. Activating the PCM may be a (relatively) time
  -- consuming process, and so it is expected that this will occur maybe once per session, or
  -- perhaps every time the user requests audio output be enabled.
  --
  -- You can request a the PCM use a smaller buffer to decrease the delay between changes in the
  -- 'PCMGenerator' and the time the hardware PCM device receives these changes, however the audio
  -- 'Happlets.Initialize.Provider' is not required to honor your request, or report to you on how
  -- big the buffer actually is.
  activatePCMOutput   :: BufferSizeRequest -> pcm PCMActivation

  -- | This function checks if the PCM output device is active, and if so, it kills the
  -- 'PCMGenerator' thread and informs the operating system that the Happlet shall no longer use the
  -- harware PCM device.
  deactivatePCMOutput :: pcm PCMActivation

  -- | Enquire as to whether the the PCM output device is active or not, or whether an error has
  -- occurred.
  queryPCMOutputState :: pcm PCMActivation

----------------------------------------------------------------------------------------------------

-- | The typeclass of monadic functions which include stateful information for communicating with an
-- audio output PCM device.
--
-- Instantiating this class does have security implications for end users, specifically that a
-- Happlet may be made to spy on someone over a network by accessing the microphone built into an
-- end user's computer. Not all Happlet 'Happlets.Initialize.Provider's will instantiate this
-- typeclass, and if so, there will hopefully be security measures in place to allow use of
-- 'AudioInput' only when end users provide informed consent to have the input PCM activated.
class (Functor pcm, Applicative pcm, Monad pcm, MonadIO pcm) => AudioInput pcm where
  -- | The PCM signal recorder is a callback function which consumes pulse code samples produced by
  -- a pulse code modulator (PCM).
  --
  -- When 'activatePCMInput' is executed, a thread is launched which loops over calling the
  -- 'PCMGenerator' to generate each PCM sample. If this function is called while the PCM input is
  -- currently activated, the new PCM generator will not replace the old one until the current PCM
  -- buffer provided by the operating system has been consumed. Depending on the buffer size, this
  -- may take a noticable amount of time. Ultimately the amount of delay is unspecified and
  -- implementation dependent.
  --
  -- The 'PCMRecorder' takes an arbitrary state value @st@, and this state value may be updated over
  -- the course of sample recording.
  setPCMRecorder :: PCMRecorder pcm st -> st -> pcm ()

  -- | This function must request of the operating system to begin retrieving pulse code information
  -- from the hardware PCM device, and launch a thread that loops over calling the 'PCMGenerator'
  -- callback that was set by the 'setPCMGenerator' function. Activating the PCM will likely be a
  -- (relatively) time consuming process, especially if the 'Happlet.Initialize.Provider' must ask
  -- the end user for permission for access to the microphone (which hopefully it does) and so it is
  -- expected that this will occur maybe once per session, or perhaps every time the user requests
  -- audio output be enabled.
  --
  -- You can request a the PCM use a smaller buffer to decrease the delay between changes in the
  -- 'PCMGenerator' and the time the hardware PCM device receives these changes, however the audio
  -- 'Happlets.Initialize.Provider' is not required to honor your request, or report to you on how
  -- big the buffer actually is.
  activatePCMInput   :: BufferSizeRequest -> pcm PCMActivation

  -- | This function checks if the PCM input device is active, and if so, it kills the 'PCMRecorder'
  -- thread and informs the operating system that the Happlet shall no longer use the harware PCM
  -- input device.
  deactivatePCMInput :: pcm PCMActivation

  -- | Enquire as to whether the the PCM input device is active or not, or whether an error has
  -- occurred.
  queryPCMInputState :: pcm PCMActivation
