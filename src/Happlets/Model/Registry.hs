-- | A registry is a mutable vector for containing elements that need to be queried in a way such
-- that the order in which they are stored is important, and usually the most recently stored
-- elements toward the top or the bottom need to be queried more quickly that elements toward the
-- middle.
--
-- It is expected that a 'Registry' will be stored in an immutable data structure, but if the
-- structure can be updated in a 'StateT' monad transformer, then the 'Registry' can grow and the
-- number of elements in the registry can change. Functions like 'registryEnqueue' return a new
-- 'Registry' data structure without re-allocating the mutable array unless the array needed to be
-- grown to make room for the enqueued object.
module Happlets.Model.Registry
  ( -- * Registry
    Registry, newRegistry, registrySize, registryAllocation,
    -- *** Registry Enqueue
    registryEnqueueNew, registryEnqueue, registryMoveElem,
    -- *** Registry Clean
    registryClean, registryForceClean,
    -- ** Folds and Maps
    FoldMapRegistry, reactEventRegistry, reactEventRegistryIO,
  ) where

import           Happlets.Control.Consequence (Consequence(..))

import           Control.Lens            (Lens', lens, use, (^.), (.=), (+=))
import           Control.Monad           (void, when, (>=>))
import           Control.Monad.Cont      (MonadCont(..), ContT(..), runContT, callCC)
import           Control.Monad.IO.Class  (MonadIO(..))
import           Control.Monad.State     (MonadState(..), get, gets, StateT(..), evalStateT)
import           Control.Monad.Trans     (MonadTrans(..))

import           Data.Function           (fix)
import           Data.IORef              (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Vector.Mutable     as MVec
import           Data.Unique             (Unique, newUnique)

----------------------------------------------------------------------------------------------------

-- | Objects stored in a 'Registry'.
data ObjectNode obj
  = NullObject
  | ObjectNode !(IORef obj)

-- | A place to store objects that are active in a 'Scene'.
newtype Registry obj = Registry{ theRegistryStore :: IORef (Store obj) }
  deriving Eq

nullObjectNode :: ObjectNode obj -> Bool
nullObjectNode = \ case
  NullObject -> True
  _          -> False

-- | Create a new 'Registry' with the enough space in it's internal vector pre-allocated to store at
-- least the given number of objects.
newRegistry :: Int -> IO (Registry obj)
newRegistry = fmap Registry . newStore

-- | Lookup the number of elements stored into the given registry.
registrySize :: Registry obj -> IO Int
registrySize (Registry{theRegistryStore=storeref}) = storeSize <$> readIORef storeref

-- | Lookup the number of elements that the current 'Registry' has been allocated to hold.
registryAllocation :: Registry obj -> IO Int
registryAllocation (Registry{theRegistryStore=storeref}) = storeAllocation <$> readIORef storeref

-- | Place an object into the registry. The @obj@ value you provide will have it's own 'IORef'
-- allocated by this function, and the 'IORef' is returned.
registryEnqueueNew :: obj -> Registry obj -> IO (IORef obj)
registryEnqueueNew obj reg = do
  ref <- newIORef obj
  registryEnqueue ref reg
  return ref

-- | Like 'registryEnqueueNew', but you are required to allocate your own 'IORef', and wrap your
-- registered object in an 'IndexSelf' constructor yourself, in order to store the object.
registryEnqueue :: IORef obj -> Registry obj -> IO ()
registryEnqueue objref (Registry{theRegistryStore=storeref}) =
  withStoreIO storeref $
  storeEnqueue $ ObjectNode objref

-- | Evaluating @'registryMoveElem' from to'@ will move an element in the 'Registry' at the @from@
-- position to the @to@ position in the 'Registry'. This can be thought of as altering the
-- Z-ordering of the registry object.
registryMoveElem :: Int -> Int -> Registry obj -> IO ()
registryMoveElem from to (Registry{theRegistryStore=storeref}) =
  withStoreIO storeref $
  storeMoveElem to from

-- | This function will trigger a 'registryForceClean' operation but only when a certain number of
-- elements have been marked as deleted.
--
-- It is a good idea to call this function after completing the calls to all of the
-- 'triggerEventHandlers' functions associatd with the 'Registry', because it does not force a
-- cleaning on every single call and so, on average, will not slow the event reaction time down for
-- registries with lots and lots of objects.
registryClean :: MonadIO m => Registry obj -> m Int
registryClean (Registry{theRegistryStore=storeref}) =
  withStoreIO storeref $
  fst <$> storeClean ()

-- | Force a sweep of all elements in a 'Registry' and remove all elements that have died. Returns
-- the number of elements that were cleaned and (equivalently) the amount of space reclaimed in the
-- 'Registry'.
registryForceClean :: MonadIO m => Registry obj -> m Int
registryForceClean (Registry{theRegistryStore=storeref}) =
  withStoreIO storeref $
  fst <$> storeForceClean ()

----------------------------------------------------------------------------------------------------

-- | You will really only ever use a function of this type as a continuation passed to the
-- 'reactAllToEvent' function.
--
-- Functions of this type are used to scan through a 'Reactor' and evaluate all continuations stored
-- within the 'Reactor' while also recording some arbitrary accounting information of type
-- @fold@. To modify the @fold@, use 'Control.Monad.State.get' and 'Control.Monad.State.put'. You
-- may also use 'callCC' to construct functions of this type, which makes this function good for
-- folding over larger data structures.
newtype FoldMapRegistry obj fold m a
  = FoldMapRegistry
    { unwrapFoldMapRegistry :: ContT (Int, fold) (StateT (Int, fold) (ModifyStore obj m)) a
    }
  deriving (Functor, Applicative, Monad, MonadIO, MonadCont)

instance MonadTrans (FoldMapRegistry obj fold) where
  lift = foldMapReactorLiftModifyStore . lift

instance Monad m => MonadState fold (FoldMapRegistry obj fold m) where
  state f = FoldMapRegistry $
    state $ \ (del, fold0) -> let (a, fold) = f fold0 in (a, (del, fold))

runFoldMapRegistry
  :: Monad m
  => fold -> FoldMapRegistry obj fold m void -> ModifyStore obj m (Int, fold)
runFoldMapRegistry fold f =
  evalStateT (runContT (unwrapFoldMapRegistry $ f >> FoldMapRegistry get) return) (0, fold)

foldMapReactorLiftModifyStore :: Monad m => ModifyStore obj m a -> FoldMapRegistry obj fold m a
foldMapReactorLiftModifyStore = FoldMapRegistry . lift . lift

foldMapReactorCountDeleted :: Monad m => (Int -> Int) -> FoldMapRegistry obj fold m Int
foldMapReactorCountDeleted inc =
  FoldMapRegistry $
  state $ \ (del0, fold) ->
  let del = inc del0 in seq del $!
  (del, (del, fold))

----------------------------------------------------------------------------------------------------

-- | Iterate through all items in a 'Reactor', evaluating a reaction function on each @obj@
-- element. Iteration may be halted by evaluating the continuation halting function provided as the
-- first parameter to the reaction function.
--
-- It is a good idea to evaluate 'reactorClean' after evaluating this function.
--
-- The semantics of the 'Consequence' returned by this function control whether the @obj@ value is
-- updated in the registry's storage vector:
--
--   - Return 'pure' or 'return' (e.g. @return (pure obj)@) with an updated @obj@ value to write a
--     new value to the vector.
--
--   - Return to 'empty' or 'mzero' (e.g. @return empty@) to indicate success but no update to the
--   - vector.
--
--   - Return 'cancel' or 'fail' (e.g. @return (fail "event handler failed")@) "to indicate that the
--     @obj@ should be deleted from the registry. Evaluating 'cancel' indicates that the @obj@
--     exited normally, 'fail' indicates it exited with an error.
--
reactEventRegistryIO
  :: MonadIO m
  => Bool
  -> ((Consequence obj -> FoldMapRegistry obj fold m void)
      -> obj
      -> FoldMapRegistry obj fold m (Consequence obj)
     )
  -> Registry obj
  -> fold
  -> m fold
reactEventRegistryIO upward = reactEventRegistry upward liftIO

-- | Same as 'reactEventRegistryIO', except does not restrict the type variable @m@ to be a member
-- of 'MonadIO', rather you pass any @liftIO@ function suitable for the type @m@ regardless of
-- whether it provides access to the underlying IO layer. This is useful in monadic functions that
-- can lift @IO@ but do not want to instantiate the 'liftIO' API so that access to IO can be
-- carefully restricted.
reactEventRegistry
  :: Monad m
  => Bool
  -> (forall a . IO a -> m a)
  -> ((Consequence obj -> FoldMapRegistry obj fold m void)
      -> obj
      -> FoldMapRegistry obj fold m (Consequence obj)
     )
  -> Registry obj
  -> fold
  -> m fold
reactEventRegistry upward liftIO action (Registry{theRegistryStore=storeref}) fold =
  withStore liftIO storeref $
  use storeCount >>= \ count ->
  if count <= 0 then return fold else
  let top = count - 1 in
  use storeVector >>= \ vec ->
  fmap snd $
  runFoldMapRegistry fold $
  callCC $ \ halt -> mapM_ 
  (\ i ->
    let delete = foldMapReactorLiftModifyStore $ storeDelete liftIO i in
    lift (liftIO (MVec.read vec i)) >>= \ case
      NullObject        -> pure ()
      ObjectNode objref ->
        let evalConsequence = \ case
              ActionHalt   -> pure ()
              ActionCancel -> delete
              ActionFail{} -> delete
              ActionOK upd -> lift $ liftIO $ writeIORef objref upd
        in
        lift (liftIO (readIORef objref)) >>=
        action (evalConsequence >=> halt) >>=
        -- Note that ^ here evalConsequence is evaluated just before halt. This closure is is passed
        -- to the callback as the halting function. So if the callback evaluates the halting
        -- closure, it is actually evaluating 'evalConsequences' one final time and then halting.
        evalConsequence
  )
  (if upward then [0 .. top] else takeWhile (>= 0) $ iterate (subtract 1) top)

----------------------------------------------------------------------------------------------------

-- | An immutable data structure containing references to objects.
data Store obj
  = Store
    { theStoreId           :: !Unique
    , theStoreCount        :: !Int -- ^ the number of items
    , theStoreDeleted      :: !Int -- ^ counts the number of deleted items
    , theStoreCleanTrigger :: !(Int -> Int -> Bool) -- ^ see 'storeCleanTrigger'
    , theStoreVector       :: !(MVec.IOVector (ObjectNode obj))
    }

instance Eq (Store obj) where { a == b = theStoreId a == theStoreId b; }

newtype ModifyStore obj m a = ModifyStore (StateT (Store obj) m a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadState (Store obj))

instance MonadTrans (ModifyStore obj) where { lift = ModifyStore . lift; }

withStoreIO :: MonadIO m => IORef (Store obj) -> ModifyStore obj m a -> m a
withStoreIO = withStore liftIO

withStore
  :: Monad m
  => (forall a . IO a -> m a)
  -> IORef (Store obj)
  -> ModifyStore obj m a -> m a
withStore liftIO ref (ModifyStore f) =
  liftIO (readIORef ref) >>=
  runStateT f >>= \ (a, store) ->
  liftIO (writeIORef ref store) >>
  return a

-- | How many elements have been registered.
storeCount :: Lens' (Store obj) Int
storeCount = lens theStoreCount $ \ a b -> a{ theStoreCount = b }

-- | How many elements have been deleted. This value is checked before expanding the store.
storeDeleted :: Lens' (Store obj) Int
storeDeleted = lens theStoreDeleted $ \ a b -> a{ theStoreDeleted = b }

-- | Store a function used to decide when a 'Store' should trigger a 'storeClean'. It takes 2
-- parameters:
--
-- 1. the store allocation size, and
-- 2. the number of deletions since the last cleaning.
--
-- This default function will trigger a 'storeClean' when the number of deletions equals or exceeds
-- a quarter of the store allocation size.
--
-- Of course, a store can be forcibly cleaned at any time by simply calling 'storeForceClean'.
storeCleanTrigger :: Lens' (Store obj) (Int -> Int -> Bool)
storeCleanTrigger = lens theStoreCleanTrigger $ \ a b -> a{ theStoreCleanTrigger = b }

-- | The vector.
storeVector :: Lens' (Store obj) (MVec.IOVector (ObjectNode obj))
storeVector = lens theStoreVector $ \ a b -> a{ theStoreVector = b }

-- | Create a new Store, pre-allocating an amount of space for the given 'Int' number of
-- elements. Requires the null @obj@ value be passed as an argument to be used to initialize the
-- vector store.
newStore :: Int -> IO (IORef (Store obj))
newStore size = do
  vec <- MVec.replicate size NullObject
  storeId <- newUnique
  newIORef Store
    { theStoreId           = storeId
    , theStoreCount        = 0
    , theStoreDeleted      = 0
    , theStoreCleanTrigger = storeCleanCondition
    , theStoreVector       = vec
    }

-- | The default value for 'storeCleanTrigger'.
storeCleanCondition
  :: Int -- ^ the store allocation size
  -> Int -- ^ the number of deletions since the last cleaning
  -> Bool
storeCleanCondition alloc delcount = delcount > 0 &&
  let (frac, rem) = divMod alloc delcount in
  frac < 4 || frac == 4 && rem == 0

-- | Get the number of elements stored into this store.
storeSize :: Store obj -> Int
storeSize = theStoreCount

-- | Get the pre-allocation size of vector within a store.
storeAllocation :: Store obj -> Int
storeAllocation = MVec.length . theStoreVector

storeEnqueue
  :: MonadIO m
  => ObjectNode obj -> ModifyStore obj m ()
storeEnqueue obj = do
  count <- use storeCount
  let newcount = count + 1
  vec <- use storeVector
  let size = MVec.length vec
  -- First check if the current element count is equal to the allocation size. If so, double the
  -- size of the allocation. This happens regardless of whether there are deleted elements who's
  -- space can be re-claimed, because we want to prevent the situation where the last empty space in
  -- the vector is constantly filled and deleted, which would force a 'storeClean' after every
  -- 'storeEnqueue' operation. We want to be sure that there is always more than enough space for
  -- all elements so as to prevent a 'storeClean' from being forced every time the allocation is
  -- completely used up.
  vec <- if count < size then return vec else liftIO $
    let newsize = head $ dropWhile (< newcount) $ iterate (* 2) size in
    MVec.new newsize >>= \ newvec ->
    MVec.copy (MVec.slice 0 count newvec) vec >>
    return newvec
  storeVector .= vec
  -- Now perform a 'storeClean' operation, if the old allocation count turned out to be not enough.
  when (count >= size) $ void $ storeClean ()
  -- The allocation resize and 'storeClean' opreation may have updated the 'count' value, get the
  -- updated value.
  count <- use storeCount
  liftIO $ MVec.write vec count obj
  storeCount += 1

-- | Move an element within the 'Store', shifting elements around without re-allocating anything.
storeMoveElem
  :: MonadIO m
  => Int -- ^ The destination index to which the element should be moved.
  -> Int -- ^ The source index from which the element should be moved.
  -> ModifyStore obj m ()
storeMoveElem to from = get >>= \ store -> liftIO $
  let vec = store ^. storeVector in
  if to == from then return ()
  else if to == from - 1 || to == from + 1 then
    MVec.read vec to >>= ((MVec.read vec from >>= MVec.write vec to) >>) . MVec.write vec from
  else if to > from
  then do
    swap <- MVec.read vec from
    let len = to - from
    MVec.move (MVec.slice from len vec) (MVec.slice (from + 1) len vec)
    MVec.write vec to swap
  else if to < from
  then do
    swap <- MVec.read vec to
    let len = from - to
    MVec.move (MVec.slice (to + 1) len vec) (MVec.slice to len vec)
    MVec.write vec from swap
  else error "internal: 'storeMoveElem', non-exhaustive if-else conditional"

-- | Write a null object to the store and mark it as deleted. Warning: if the item is already marked
-- as deleted, the deleted @obj@ value given here is still written and 'storeDeleted' is still
-- incremented. This is because there is no function defined within this context that could test if
-- the @obj@ is already null.
storeDelete :: Monad m => (forall a . IO a -> m a) -> Int -> ModifyStore obj m ()
storeDelete liftIO i =
  use storeVector >>= \ vec ->
  lift (liftIO (MVec.write vec i NullObject)) >>
  storeDeleted += 1

-- | First checks the 'storeCleanCondition', and if conditions for cleaning are met, calls
-- 'storeForceClean'. Returns the number of dead elements that were removed.
storeClean
  :: MonadIO m
  => fold
  -> ModifyStore obj m (Int, fold)
storeClean fold = do
  trigger  <- use storeCleanTrigger
  alloc    <- gets storeAllocation
  delcount <- use storeDeleted
  if trigger alloc delcount then storeForceClean fold else return (0, fold)

-- | This function scans through a 'Store' and removes elements that have been deleted.
--
-- It is parameterized over the function 'ObjectIsAlive' which judges whether an element has been
-- deleted, and over the function 'StoreReassignIndex' which informs an element what it's new index
-- within the 'Store' is, allowing the element to update it's own state with it's own index so that
-- it can remove itself from the 'Store' quickly.
storeForceClean
  :: MonadIO m
  => fold
  -> ModifyStore obj m (Int, fold)
storeForceClean fold =
  use storeDeleted >>= \ del ->
  if del <= 0 then return (0, fold) else
  use storeVector >>= \ vec ->
  use storeCount  >>= \ top ->
  runFoldMapRegistry fold $ fix -- Step into FoldMapRegistry monad, loop.
  (\ loop rem0 i ->  -- i: Loop cursor, scans through vector.
    if i >= top then foldMapReactorLiftModifyStore $ do
      storeCount .= rem0 -- Set the new 'storeCount' after removing deleted elements.
      storeDeleted .= 0  -- Reset 'storeDeleted' to indicate store is already clean.
    else
      liftIO (MVec.read vec i) >>= \ obj -> -- Read the object at index i.
      if nullObjectNode obj then do
        foldMapReactorCountDeleted (+ 1)
        (loop rem0) $! i + 1
      else
        let rem = rem0 + 1 in seq rem $!           -- rem: accounting for remaining elements.
        foldMapReactorCountDeleted id >>= \ del -> -- del: counts number of deleted elements.
        if del <= 0 then loop rem $! i + 1 else    -- Bubble down if any have been deleted.
        liftIO (MVec.write vec rem obj) >>
        (loop rem $! i + 1)
  ) 0 0