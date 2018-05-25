-- | This module provides the 'Provider' data type used by various back-ends to initialize a working
-- GUI. In object-oriented jargon, this module provides an abstract interface for Happlets, and the
-- back-end providers are "factories" which must instantiate all of the methods in this interface.
module Happlets.Provider where

import           Happlets.Config
import           Happlets.Draw.SampCoord
import           Happlets.GUI

----------------------------------------------------------------------------------------------------

-- | The @provider@ is a data type containing all information necessary for a high-level 'Happlet' to
-- interface with the low-level back-end provider. The @provider@ is associated with a @window@ handle
-- type (usually a wrapper around a 'Control.Concurrent.MVar') and an @image@ type which store image
-- buffers. The @window@ and @image@ types are functionally dependent on the @provider@ type so it is
-- only necessary to specify the @provider@ type to the type checker in ambiguous situations where the
-- type variables cannot be resolved.
--
-- Furthermore, the @provider@ type may provided different types of @window@ which display different
-- types of @image@. One example of how this might be used is for a Gtk+ back-end might provide a
-- single @window@ type but create two different instances of @provider@, one with Cairo-based @image@
-- buffers, and one with OpenGL-based @image@ buffers.
data Provider window
  = Provider
    { -- | The default 'Happlet.Config.Config' parameters best suited for this partuclar back-end.
      defaultConfig :: Config

      -- | This is the pre-initialization function that should run before performing any GUI-related
      -- action at all.
    , doInitializeGUI :: IO ()

      -- | This function launches the event loop for the given @window@ and does not return until
      -- the GUI application's event handler thread has quit. In the case of Gtk+, since there can
      -- only be one event loop per application, calling this function on any window handle will
      -- launch the event loop for __all__ window handles.
    , doGUIEventLoopLaunch :: Config -> IO ()

      -- | This function creates a @window@ without installing any event handlers, and without
      -- making the window visible. The @window@ craeted can then have 'Happlet's attached to it
      -- using 'doWindowAttach'.
    , doWindowNew :: Config -> IO window

      -- | This function asks the operating system or desktop environment to delete the window
      -- associated with the given @window@. If the 'Happlets.Config.quitOnWindowClose' configuration
      -- parameter is 'Prelude.True', calling this function should also quit the whole application.
    , doWindowDelete :: window -> IO ()

      -- | This function __must__ delete all event handlers currently installed into the @window@
      -- and evaluate a new 'Happlets.GUI.GUI' function which can install new event handlers.
    , doWindowAttach
        :: forall model
        .  Bool -- ^ whether or not to make the window visible
        -> window -- ^ the window to which the 'Happlets.Happlet.Happlet' will be attached
        -> Happlet model -- ^ the 'Happlets.Happlet.Happlet' to attach.
        -> (PixSize -> GUI window model ())
        -> IO ()
    }
