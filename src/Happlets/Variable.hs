-- | Provides a getter/setter mechanism that are used in a way similar to environment variables,
-- that is to say, these are impure, stateful setters and getters.  Unlike lenses, can modify impure
-- stateful values and also, as a side effect, send a signal triggering an update to respond to a
-- change in state.
module Happlets.Variable where

import           Control.Lens (Lens', assign, use)
import           Control.Monad.State.Class

-- | This data structure contains two functions, one for updating the state, and one for querying
-- the state. It is like a lens, but must evaluate in a monadic function type @m@ which can perform
-- a side effect as a result of the change to the state.
--
-- Throughout the Happlets framework, there are classes in which the member functions are
-- 'Context's. For example, the 'Happlets.Draw' module has the 'Happlet2DGraphics' class providing
-- various context functions such as
--
-- @
-- 'Happlets.Draw.foreColor' :: 'Context' render 'Happlets.Draw.FillColor'
-- @
--
-- So to set the fore color of the @render@ context, you would write:
--
-- @
-- 'setVal' 'Happlets.Draw.foreColor' 'Happlets.Draw.Color.red'
-- @
data Variable m a
  = Variable
    { getVal :: m a
      -- ^ Get a value from the context, and possibly execute a side-effect (like setting a counter)
    , setVal :: a -> m ()
      -- ^ Set a value in the context, and possibly execute a side-effect (like redrawing an image).
    }

updateVal :: Monad m => Variable m a -> (a -> a) -> m ()
updateVal ctx f = getVal ctx >>= setVal ctx . f

variableFromLens :: MonadState st m => Lens' st a -> Variable m a
variableFromLens lens = Variable
  { setVal = assign lens
  , getVal = use lens
  }

fmapVariableMonad
  :: (Monad m0, Monad m1)
  => (forall any . m0 any -> m1 any)
  -> Variable m0 a -> Variable m1 a
fmapVariableMonad f var = Variable
  { setVal = f <$> setVal var
  , getVal = f $ getVal var
  }
