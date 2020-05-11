-- | This module provides typeclasses that are meant to extend the functionality of a
-- 'Happlets.Model.GUI.GUI' with the ability for triggering audio playback, taking an unspecified
-- audio model and sending it to an audio view. The callback functions installed by 'audioPlayback'
-- are called repeatedly to fill a PCM buffer with a signal.
module Happlets.Model.Audio
  ( AudioPlayback(..), AudioCapture(..),
  ) where

import           Happlets.View.Audio

----------------------------------------------------------------------------------------------------

-- | The typeclass of monadic functions which include stateful information for communicating with
-- an audio output PCM device.
class (Functor gui, Applicative gui, Monad gui, MonadIO gui) => AudioPlayback gui where
  -- | The PCM signal generator is a callback function which produces pulse code samples to be sent
  -- to a pulse code modulator (PCM). This function can be called at any time, the @pcm@ function is
  -- expected to keep a reference to the given callback. When 'startupAudioPlayback' is executed, a
  -- thread is launched which loops over calling the 'PCMGenerator' to generate each PCM sample. If
  -- this function is called while the PCM output is currently activated, the new PCM generator will
  -- not replace the old one until the current PCM buffer has been filled and sent to the operating
  -- system. Depending on the buffer size, this may take a noticable amount of time. Ultimately the
  -- amount of delay is unspecified and implementation dependent.
  --
  -- It is not a good idea to rely on this 'audioPlayback' function to do sequencing, rather the
  -- 'PCMGenerator' callback function should be initialized with access to a
  -- 'Happlets.Audio.PCMControl' and then poll this controller using
  -- 'Happlets.Audio.checkPCMControl' for information about what signal to generate, when, and for
  -- how long.
  --
  -- The 'PCMGenerator' takes an arbitrary state value @st@, and this state value may be updated
  -- over the course of sample generation. Construct your 'PCMGenerator' using either the
  -- 'Happlets.Audio.stereoPCM' or 'Happlets.Audio.monoPCM' constructors.
  audioPlayback :: PCMGenerator -> gui PCMActivation

  -- | This function must request of the operating system to begin sending information to the
  -- hardware PCM device, and launch a thread that loops over calling the 'PCMGenerator' callback
  -- that was set by the 'audioPlayback' function. Activating the PCM may be a (relatively) time
  -- consuming process, and so it is expected that this will occur maybe once per session, or
  -- perhaps every time the user requests audio output be enabled.
  --
  -- You can request a the PCM use a smaller buffer to decrease the delay between changes in the
  -- 'PCMGenerator' and the time the hardware PCM device receives these changes, however the audio
  -- 'Happlets.Initialize.Provider' is not required to honor your request, or report to you on how
  -- big the buffer actually is.
  startupAudioPlayback :: BufferSizeRequest -> gui PCMActivation

  -- | This function checks if the PCM output device is active, and if so, it kills the
  -- 'PCMGenerator' thread and informs the operating system that the Happlet shall no longer use the
  -- harware PCM device.
  shutdownAudioPlayback :: gui PCMActivation

  -- | Enquire as to whether the the PCM output device is active or not, or whether an error has
  -- occurred.
  audioPlaybackState :: gui PCMActivation

----------------------------------------------------------------------------------------------------

-- | The typeclass of monadic functions which include stateful information for communicating with an
-- audio output PCM device.
--
-- Instantiating this class does have security implications for end users, specifically that a
-- Happlet may be made to spy on someone over a network by accessing the microphone built into an
-- end user's computer. Not all Happlet 'Happlets.Initialize.Provider's will instantiate this
-- typeclass, and if so, there will hopefully be security measures in place to allow use of
-- 'AudioCapture' only when end users provide informed consent to have the input PCM activated.
class (Functor gui, Applicative gui, Monad gui, MonadIO gui) => AudioCapture gui where
  -- | The PCM signal recorder is a callback function which consumes pulse code samples produced by
  -- a pulse code modulator (PCM).
  --
  -- When 'startupAudioCapture' is executed, a thread is launched which loops over calling the
  -- 'PCMGenerator' to generate each PCM sample. If this function is called while the PCM input is
  -- currently activated, the new PCM generator will not replace the old one until the current PCM
  -- buffer provided by the operating system has been consumed. Depending on the buffer size, this
  -- may take a noticable amount of time. Ultimately the amount of delay is unspecified and
  -- implementation dependent.
  --
  -- The 'PCMRecorder' takes an arbitrary state value @st@, and this state value may be updated over
  -- the course of sample recording. Construct the 'PCMRecorder' function using the 'stereoPCM' or
  -- 'monoPCM' constructor functions.
  audioCapture :: PCMRecorder -> gui PCMActivation

  -- | This function must request of the operating system to begin retrieving pulse code information
  -- from the hardware PCM device, and launch a thread that loops over calling the 'PCMGenerator'
  -- callback that was set by the 'audioCapture' function. Activating the PCM will likely be a
  -- (relatively) time consuming process, especially if the 'Happlet.Initialize.Provider' must ask
  -- the end user for permission for access to the microphone (which hopefully it does) and so it is
  -- expected that this will occur maybe once per session, or perhaps every time the user requests
  -- audio output be enabled.
  --
  -- You can request a the PCM use a smaller buffer to decrease the delay between changes in the
  -- 'PCMGenerator' and the time the hardware PCM device receives these changes, however the audio
  -- 'Happlets.Initialize.Provider' is not required to honor your request, or report to you on how
  -- big the buffer actually is.
  startupAudioCapture   :: BufferSizeRequest -> gui PCMActivation

  -- | This function checks if the PCM input device is active, and if so, it kills the 'PCMRecorder'
  -- thread and informs the operating system that the Happlet shall no longer use the harware PCM
  -- input device.
  deactivatePCMInputu :: gui PCMActivation

  -- | Enquire as to whether the the PCM input device is active or not, or whether an error has
  -- occurred.
  audioCaptureState :: gui PCMActivation
