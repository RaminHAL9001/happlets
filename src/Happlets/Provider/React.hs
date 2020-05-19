-- | This module defines some functional reactive programming (FRP) data structures that are useful
-- for various different back-end providers.
module Happlets.Provider.React where

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
