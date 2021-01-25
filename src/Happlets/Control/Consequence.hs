-- | This module provides the 'Consequence' data type, which is used internally to monadic functions
-- that react to events. A 'Consequence' is the result of a reaction to an event.
module Happlets.Control.Consequence where

import           Prelude                 hiding (fail)

import           Control.Applicative     (Alternative(..))
import           Control.Monad           (MonadPlus(..))
import           Control.Monad.Except    (MonadError(..))
import           Control.Monad.Fail      (MonadFail(..))
import           Control.Monad.IO.Class  (MonadIO(..))
import           Control.Monad.Reader    (MonadReader(..), ReaderT(..))
import           Control.Monad.State     (MonadState(..), StateT(..))
import           Control.Monad.Trans     (MonadTrans(..))

import qualified Data.Text               as Strict

----------------------------------------------------------------------------------------------------

-- | A 'Consequence' is the result of a reaction to an event. The semantics of each result is very
-- specific:
data Consequence a
  = ActionOK a
    -- ^ This is the 'return' or 'pure' function. It indicates the reaction is successful, and also
    -- passes a value to the next monad in the procedure when used with bind operator '(>>=)'.
  | ActionHalt 
    -- ^ This is the 'mzero' of 'empty' function. It indicates that the reaction gave up but will
    -- not delete itself. Use 'empty' in an event handler when you received input that is not usable
    -- for the time being, but may be usable later if called again with the same value. The '(<|>)'
    -- operator and the 'mplus' function can catch 'ActionHalt' and evaluate an alternative action.
  | ActionCancel
    -- ^ This is the 'cancel' function. It indicates that the event handler is not to continue
    -- reacting to events, and it must delete itself from the event signal handler. Use 'cancel' in
    -- an event handler when you receive a signal that indicates the end of input events. Like
    -- 'ActionHalt', the '(<|>)' operator and 'mplus' function will catch a 'cancel'ed action and
    -- evaluate an alternative action.
  | ActionFail !Strict.Text
    -- ^ This is the 'fail' function. It indicates not only that the event handler is to 'cancel'
    -- itself, but an error message should be reported to the user as well. 'ActionFail' is neither
    -- caught by the '(<|>)' operator nor the 'mplus' function, but can be caught by the
    -- 'catchError' function.
  deriving Functor

-- | Monadic functions that use 'Consequence' internally are 'Consequential', which means that
-- functions of this type can 'cancel' themselves.
class Applicative m => Consequential m where
  -- | The 'cancel' function is similar to 'empty' in the 'Alternative' class, or 'mzero' in the
  -- 'MonadPlus' class. But a 'CancelableAction' is usually an action that is stored in an event
  -- handler context and is repeatedly called to handle events. The 'cancel' function, unlike
  -- 'mzero' or 'empty', will indicate to the event handler context that this function of type @m@
  -- will be removed from the event handler context and never called again to handle an event.
  --
  -- For the 'GUI' monad, 'cancel' is equivalent to 'cancelGUIEventHandler'.
  cancel :: m void

discontinue :: String -> Consequence a -> Consequence b
discontinue msg = fmap $ const $ error $ "internal: Consequence." ++ msg

instance Applicative Consequence where
  pure = ActionOK
  (<*>) = \ case
    ActionOK f -> \ case
      ActionOK a -> ActionOK (f a)
      a          -> discontinue "(<*>)" a
    f          -> const $ discontinue "(<*>)" f

instance Alternative Consequence where
  empty = ActionHalt
  (<|>) = \ case
    ActionOK a   -> const $ ActionOK a
    ActionHalt     -> id
    ActionCancel   -> id
    ActionFail msg -> const $ ActionFail msg

instance Monad Consequence where
  return = pure
  (>>=) = \ case
    ActionOK a -> ($ a)
    a          -> const $ discontinue "(>>=)" a

instance MonadPlus Consequence where { mzero = empty; mplus = (<|>); }

instance MonadFail Consequence where { fail = ActionFail . Strict.pack; }

instance MonadError Strict.Text Consequence where
  throwError = ActionFail
  catchError = \ case
    ActionFail msg -> ($ msg)
    ActionOK   a   -> const $ ActionOK a
    a              -> const $ discontinue "catchError" a

instance Consequential Consequence where { cancel = ActionCancel; }

----------------------------------------------------------------------------------------------------

-- | A 'Consequence' monad transformer similar to 'ExceptT' but uses 'Consequence' rather than
-- 'Either' as the decision/control type.
newtype ConsequenceT m a = ConsequenceT { runConsequenceT :: m (Consequence a) }

instance Applicative m => Consequential (ConsequenceT m) where
  cancel = ConsequenceT $ pure ActionCancel

instance Functor m => Functor (ConsequenceT m) where
  fmap f (ConsequenceT a) = ConsequenceT $ fmap f <$> a

instance Applicative m => Applicative (ConsequenceT m) where
  pure = ConsequenceT . pure . pure
  (ConsequenceT f) <*> (ConsequenceT a) = ConsequenceT $ (<*>) <$> f <*> a

instance Monad m => Alternative (ConsequenceT m) where
  empty = ConsequenceT $ pure ActionHalt
  (ConsequenceT a) <|> (ConsequenceT b) = ConsequenceT $ a >>= \ case
    a@ActionOK{}   -> pure a
    a@ActionFail{} -> pure a
    _              -> b

instance Monad m => Monad (ConsequenceT m) where
  return = ConsequenceT . return . return
  (ConsequenceT a) >>= f = ConsequenceT $ a >>= \ case
    ActionOK a -> runConsequenceT $ f a
    otherwise  -> return $ const (error "internal: ConsequenceT.Monad.>>=") <$> otherwise

instance Monad m => MonadPlus (ConsequenceT m) where { mzero = empty; mplus = (<|>); }

instance Monad m => MonadFail (ConsequenceT m) where
  fail = ConsequenceT . pure . fail

instance Monad m => MonadError Strict.Text (ConsequenceT m) where
  throwError = ConsequenceT . pure . ActionFail
  catchError (ConsequenceT try) catch = ConsequenceT $ try >>= \ case
    ActionFail msg -> runConsequenceT $ catch msg
    otherwise      -> return otherwise

instance MonadTrans ConsequenceT where
  lift = ConsequenceT . fmap ActionOK

instance MonadIO m => MonadIO (ConsequenceT m) where
  liftIO = lift . liftIO

instance MonadState s m => MonadState s (ConsequenceT (StateT s m)) where
  state = lift . state

instance MonadReader r m => MonadReader r (ConsequenceT (ReaderT r m)) where
  ask = lift ask
  local f (ConsequenceT a) = ConsequenceT $ local f a

-- | Construct a 'ConsequenceT' function that immediately evaluates to the given 'Consequence'
-- value.
consequence :: Monad m => Consequence a -> ConsequenceT m a
consequence = ConsequenceT . return
