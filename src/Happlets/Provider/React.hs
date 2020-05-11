-- | This module defines some functional reactive programming (FRP) data structures that are useful
-- for various different back-end providers.
module Happlets.Provider.React where

import           Happlets.View.Types2D

import           Control.Lens (Lens', use, (.=))
import           Control.Monad.State

----------------------------------------------------------------------------------------------------

data ConnectReact m event
  = Disconnected
  | ConnectReact{ doDisconnect :: IO (), reactTo :: event -> m () }

evalConnectReact
  :: MonadState st m
  => Lens' st (ConnectReact m event)
  -> event -> m ()
evalConnectReact connectReact event = use connectReact >>= \ case
  Disconnected -> return ()
  connected@ConnectReact{} -> reactTo connected event

forceDisconnect
  :: (MonadIO m, MonadState provider m)
  => Lens' provider (ConnectReact m event) -> m ()
forceDisconnect connectReact = use connectReact >>= \ case
  connection@ConnectReact{} -> do
    liftIO $ doDisconnect connection
    connectReact .= Disconnected
  Disconnected -> return ()

-- | All providers will probably have a reaction for initializing the Happlet, which should get
-- called after the window system has been initialized, and resources for the Happlet window have
-- been allocated and are ready to begin drawing.
class HasInitReaction provider where
  initReaction :: MonadState provider m => Lens' provider (ConnectReact m PixSize)

-- | All providers will probably have a reaction for cleaning-up when the Happlet is shut down, or
-- when a context switch occurs.
class HasDetatchHandler provider where
  detatchHandler :: MonadState provider m => Lens' provider (ConnectReact m ())

-- | All providers will probably have more than one 'ConnectReact' value stored within it's state.
class HasManyReactions provider where
  -- | This function must call 'forceDisconnect' on every 'ConnectReact' value wihin the
  -- @provider@'s state.
  disconnectAll :: MonadState provider m => m ()
