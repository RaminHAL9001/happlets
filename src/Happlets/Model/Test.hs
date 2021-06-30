module Happlets.Model.Test where

import Happlets.Control.Consequence (Consequence, cancel)
import Happlets.Model.Registry
       ( Registry, newRegistry, registryEnqueueNew, registryForceClean,
         registrySize, registryAllocation, debugShowRegistry,
         FoldMapRegistry, reactEventRegistryIO
       )

import Control.Applicative (empty)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Class (modify)
import Data.IORef (IORef, readIORef, writeIORef)
import qualified Data.Text as Strict
import Data.String (IsString(fromString))

data Elem = Elem !Bool !Strict.Text

instance Show Elem where
  show (Elem ok msg) = (if ok then '+' else '-') : ' ' : Strict.unpack msg

instance IsString Elem where
  fromString = Elem True . fromString

printPropIO :: Show a => String -> IO a -> IO ()
printPropIO msg f = f >>= putStrLn . (\ a -> msg <> "=" <> a) . show

printRegistrySize :: Registry r -> IO ()
printRegistrySize = printPropIO "registrySize" . registrySize

printRegistryAllocation :: Registry r -> IO ()
printRegistryAllocation = printPropIO "registryAllocation" . registryAllocation

printRegistryInfo :: Registry r -> IO ()
printRegistryInfo r = printRegistrySize r >> printRegistryAllocation r

type Halt fold = Consequence () -> FoldMapRegistry Elem fold IO (Consequence ())

displayElem :: Halt () -> IORef Elem -> FoldMapRegistry Elem () IO (Consequence ())
displayElem _ ref = liftIO $ readIORef ref >>= putStrLn . show >>= pure . pure

-- | Evaluate a predicate on the 'Text' of each 'Elem', set it's boolean value to the result of the
-- predicate.
markAllElems
  :: (Strict.Text -> Bool) -> Halt Int -> IORef Elem
  -> FoldMapRegistry Elem Int IO (Consequence ())
markAllElems check _halt ref = do
  (Elem _ txt) <- liftIO $ readIORef ref
  let ok = check txt
  liftIO $ writeIORef ref (Elem ok txt)
  when ok $ modify (+ 1)
  return empty

-- | Search through the 'Registry' by evaluating a predicate on the 'Text' of each 'Elem', if the
-- predicate is 'True' then toggle the boolean value and halt the search.
toggleElem
  :: (Strict.Text -> Bool) -> Halt Int -> IORef Elem
  -> FoldMapRegistry Elem Int IO (Consequence ())
toggleElem check halt ref = do
  (Elem bool txt) <- liftIO (readIORef ref)
  let doToggle = check txt
  if doToggle then do
      liftIO $ writeIORef ref (Elem (not bool) txt)
      modify (+ 1)
      halt empty
    else
      return empty

clearFalseElems :: Halt Int -> IORef Elem -> FoldMapRegistry Elem Int IO (Consequence ())
clearFalseElems _halt ref =
  liftIO (readIORef ref) >>= \ (Elem ok _) ->
  if not ok then modify (+ 1) >> return cancel
  else return empty

up :: Bool
up = True

down :: Bool
down = False

runTest :: IO ()
runTest = do
  reg <- newRegistry 8 :: IO (Registry Elem)
  let enqueue = flip registryEnqueueNew reg
  let display = debugShowRegistry id reg
  let test :: forall a . String -> Bool -> IO a -> IO a
      test msg dir f = do
        let dirtxt = (if dir then "upward" else "downward") <> " scan: "
        putStrLn ("begin " <> dirtxt <> msg)
        f <* display <* putStrLn ("end " <> msg)
  --let update msg dir f = test msg dir $ reactEventRegistryIO dir f reg ()
  let count  msg after dir f = test msg dir $ do
        c <- reactEventRegistryIO dir f reg (0::Int)
        putStrLn $ after <> " " <> show c <> " elements"
  test "enqueue 4 items" up $ do
    enqueue "zero"
    enqueue "one"
    enqueue "two"
    enqueue "three"
  printRegistryInfo reg
  test "display" up $ pure ()
  test "display" down $ pure ()
  count "toggle" "toggled" down $ toggleElem (== "two")
  count "clear" "cleared" up clearFalseElems
  test "enqueue 4 items" up $ do
    enqueue "four"
    enqueue "five"
    enqueue "six"
    enqueue "seven"
  test "force clean" up $ registryForceClean reg
  test "enqueue 4 items" up $ do
    enqueue "eight"
    enqueue "nine"
    enqueue "ten"
    enqueue "eleven"
  printRegistryInfo reg
