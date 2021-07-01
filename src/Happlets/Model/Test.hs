module Happlets.Model.Test where

import Happlets.Model.Registry
       ( Registry, newRegistry, registryEnqueueNew, registryForceClean,
         registrySize, registryAllocation, debugShowRegistry,
         FoldMapRegistry, KeepOrDelete(..), reactEventRegistryIO
       )

import Control.Monad (when, (>=>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Class (modify)
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

type UpdateElem fold = Elem -> FoldMapRegistry Elem fold IO ()

displayElem :: UpdateElem () -> Elem -> FoldMapRegistry Elem () IO KeepOrDelete
displayElem _ = liftIO . putStrLn . show >=> return . const KeepObject

-- | Evaluate a predicate on the 'Text' of each 'Elem', set it's boolean value to the result of the
-- predicate.
markAllElems
  :: (Strict.Text -> Bool)
  -> UpdateElem Int -> Elem -> FoldMapRegistry Elem Int IO KeepOrDelete
markAllElems check update (Elem _ txt) = do
  let ok = check txt
  update (Elem ok txt)
  when ok $ modify (+ 1)
  return KeepObject

-- | Search through the 'Registry' by evaluating a predicate on the 'Text' of each 'Elem', if the
-- predicate is 'True' then toggle the boolean value and halt the search.
toggleElem
  :: Bool -> (Strict.Text -> Bool)
  -> UpdateElem Int -> Elem -> FoldMapRegistry Elem Int IO KeepOrDelete
toggleElem all check update (Elem bool txt) =
  let doToggle = check txt in
  if doToggle then do
    update (Elem (not bool) txt)
    modify (+ 1)
    return (if all then DeleteObject else DeleteObjectHalt)
  else
    return KeepObject

clearFalseElems :: UpdateElem Int -> Elem -> FoldMapRegistry Elem Int IO KeepOrDelete
clearFalseElems _halt (Elem ok _) =
  if not ok then modify (+ 1) >> return DeleteObject
  else return KeepObject

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
  let clear msg p = do
        count ("toggle (" <> msg <> ")") "toggled" down $ toggleElem True p
        count "clear" "cleared" up clearFalseElems
        test "force clean" up $ registryForceClean reg
  test "enqueue 4 items" up $ do
    enqueue "zero"
    enqueue "one"
    enqueue "two"
    enqueue "three"
  printRegistryInfo reg
  test "display" up $ pure ()
  test "display" down $ pure ()
  count ("toggle " <> "== \"two\"") "toggled" down $ toggleElem False (== "two")
  count "clear" "cleared" up clearFalseElems
  test "enqueue 4 items" up $ do
    enqueue "four"
    enqueue "five"
    enqueue "six"
    enqueue "seven"
  test "force clean" up $ registryForceClean reg
  test "enqueue 9 items" up $ do
    enqueue "eight"
    enqueue "nine"
    enqueue "ten"
    enqueue "eleven"
    enqueue "twelve"
    enqueue "thirteen"
    enqueue "fourteen"
    enqueue "fifteen"
    enqueue "sixteen"
  clear "any 'v'" (Strict.any (== 'v'))
  clear "any 'e'" (Strict.any (== 'e'))
  printRegistryInfo reg
