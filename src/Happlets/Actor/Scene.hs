-- | A 'Scene' makes use of the 'Happlets.Actor.Presence' data type to construct an array of untyped
-- actors that can respond to events.
module Happlets.Actor.Scene
  ( -- ** Scenes
    Scene, newScene, sceneBracket, grabFocus, delegateSceneEvents,
    debugSceneElements, debugPrintSceneRegistry
    sceneRedraw, forceSceneRedraw,
    sceneKeyboardHandler, actAnimationHandler,
    sceneMouseDown, sceneMouseClick, sceneMouseDoubleClick,
    sceneMouseOver, sceneMouseDrag,
  )
  where

import           Happlets.Actor
                 (
                 )

import           Happlets.Logging
                 ( CanWriteReports(report), LogReporter,
                   ReportLevel(ERROR, WARN, INFO, OBJECT, EVENT, DEBUG_ALL)
                 )
import           Happlets.Initialize (Initialize, newHappletIO, theActualLogReporter)
import           Happlets.Model.GUI
                 ( GUI, Happlet, ProvidesLogReporter,
                   onSubModel, guiLogReportWriter
                 )
import           Happlets.Model.Registry
                 ( Registry, KeepOrDelete(..), newRegistry, registryEnqueue,
                   reactEventRegistry, reactEventRegistryIO,
                   Store, debugPrintRegistry
                 )
import           Happlets.View
                 ( Happlet2DGraphics(draw2D),
                   HappletWindow(onCanvas),
                   Sized2DRaster(getViewSize),
                   Has2DOrigin(origin2D)
                 )
import           Happlets.View.Types2D
                 ( SampCoord, PixSize, PixCoord, Point2D, V2(..),
                   Rect2D, rect2D, point2D, rect2DSize, rect2DHead, rect2DTail,
                   Drawing, drawingIsNull, canonicalize2DShape,
                   rect2DUnion, rect2DUnionNull, theBoundingBox,
                 )
import           Happlets.Control.Animate (CanAnimate(stepFrameEvents))
import           Happlets.Control.Consequence
                 ( Consequential(cancel),
                   CatchConsequence(catchConsequence), ThrowConsequence(throwConsequence),
                   Consequence(..), ConsequenceT(..), runConsequenceT
                 )
import           Happlets.Control.Mouse
                 ( CanMouse(mouseSignals), MouseSignal(..),
                   MouseSignalPattern(MouseButton, MouseDrag, MouseAll),
                   MouseButtonSignal(MotionOnly,LeftClick,RightClick),
                   similarMouseSignals
                 )
import           Happlets.Control.Keyboard
                 ( CanKeyboard(keyboardEvents),
                   Keyboard(Keyboard, RawKey),
                   ModifierBits
                 )
import           Happlets.Control.WindowManager

import           Control.Applicative (Alternative(..))
import           Control.Lens
                 ( Lens', lens, cloneLens, view, set, use, assign,
                   (&), (^.), (%~), (.~), (<>=), (.=), (%=)
                 )
import           Control.Monad (MonadPlus(..), guard, when, unless, (>=>))
import           Control.Monad.Except (MonadError(throwError, catchError))
import           Control.Monad.Fail (MonadFail(fail))
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.State
                 ( MonadState(state, get), StateT(..),
                   gets, modify, runStateT
                 )
import           Control.Monad.Trans (MonadTrans(..))

--import           Data.Functor.Const         (Const(..))
import           Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Text as Strict
import           Data.Time.Clock (UTCTime, NominalDiffTime, getCurrentTime, diffUTCTime)

import           System.IO (hPutStrLn, stderr)

----------------------------------------------------------------------------------------------------

-- | A 'Scene' is a model of a 2D canvas containing many 'Presence' objects, within which all can
-- update the canvas and can respond to canvas events. When an 'Act' is first constructed by
-- 'newActHapplet' it already has a new 'Scene' ready to be populated with 'Presence's, so it is not
-- necessary to create a new 'Scene', but keeping 'Presence's organized into 'Scene's and changing
-- between 'Scene's is a good way to keep a user interface less cluttered.
--
-- A 'Scene' itself can be used as the @model@ of a 'Actor', so you can use the 'Scene' as an
-- 'Presence' in another scene, and events can be delegated to the 'Scene' through the 'Actor'
-- event handler APIs. When you create a 'Scene' using 'newScene', a 'Actor' containing the
-- 'Scene' is returned.
data Scene
  = Scene
    { theSceneRegistry   :: !(Registry (Role Presence))
      -- ^ The objects in this scene
    , theSceneFocus      :: !(Maybe Presence)
      -- ^ A reference to the object in 'theSceneRegistry' that currently responds to keyboard
      -- events or double-click events. Also, an object that is being dragged necessarily has focus.
    , theSceneStats      :: !ActorEventHandlerStats
      -- ^ Statistics about the number of event handlers installed across all 'Presence's within the
      -- 'Scene', this helps to determine if the 'Scene' should install it's own event handler to
      -- delegate events to any of it's 'Presence's.
    , theSceneGlobalBounds :: !(Rect2D SampCoord)
      -- ^ The 'Rect2D', in global coordinates of this 'Scene's viewable window, in the 'GUI'
      -- monad's coordinate system.
    }

-- | not for export
--
-- The Registry for the 'Scene' contains a reference to all 'Actors'.
sceneRegistry :: Lens' Scene (Registry (Role Presence))
sceneRegistry = lens theSceneRegistry $ \ a b -> a{ theSceneRegistry = b }

-- | The 'Presence' that currently has focus.
sceneFocus :: Lens' Scene (Maybe Presence)
sceneFocus = lens theSceneFocus $ \ a b -> a{ theSceneFocus = b }

-- | The 'Rect2D', in global coordinates of this 'Scene's viewable window, in the 'GUI' monad's
-- coordinate system.
sceneGlobalBounds :: Lens' Scene (Rect2D SampCoord)
sceneGlobalBounds = lens theSceneGlobalBounds $ \ a b -> a{ theSceneGlobalBounds = b }

-- | not for export
--
-- Tracks statistics on how many elements in a 'Scene' respond to particular events. It is
-- determined by an aggregate computation over all @('Role' 'Presence')@ items in a 'Scene's
-- 'Registry'.
sceneStats :: Lens' Scene ActorEventHandlerStats
sceneStats = lens theSceneStats $ \ a b -> a{ theSceneStats = b }

-- | not for export
--
-- Place a 'Presence' on stage, making it visible and able to respond to events.
stagePresence :: Presence -> Script Scene ()
stagePresence (Presence actorRef) = do
  use sceneRegistry >>= scriptIO . registryEnqueue actorRef
  role <- scriptIO (readIORef actorRef)
  let stats = roleEventStats role
  report OBJECT $
    "Staging actor: " <> (role ^. roleLabel) <>
    "\n  stats:\n" <> Strict.pack (show stats)
  sceneStats <>= stats
  stats <- use sceneStats
  report OBJECT ("Scene stats after staging actor:\n" <> Strict.pack (show stats))

-- | Place a 'Actor' on stage, making it visible and able to respond to events. After defining
-- a 'Actor' using 'actor' or 'actress', it is necessary to call this 'onStage' function in
-- order for the 'Actor' begin acting on stage and responding to events and cues.
onStage :: Actor model -> Script Scene ()
onStage = stagePresence . thePresenceActor

-- | Function used internally by 'newActHapplet' to create a new scene. Scenes that can be
-- manipulated by end users of this API are always wrapped in a 'Actor'.
makeEmptyScene :: Int -> IO Scene
makeEmptyScene size = do
  registry <- newRegistry size
  return Scene
    { theSceneRegistry     = registry
    , theSceneFocus        = Nothing
    , theSceneStats        = mempty
    , theSceneGlobalBounds = rect2D
    }

----------------------------------------------------------------------------------------------------

-- | Print a debug reporrt of the content of the current 'Scene' to standard output. Note that if
-- @model@ type is bound to 'Scene', that is, if you evaluate 'debugSceneElements' in a 'Script'
-- context of type @'Script' 'Scene' ()@ this function will not necessarily produce a debug report
-- about the same 'Scene' that would be returned by 'get', the debug report is only about the
-- internal (hidden) 'Scene' that is part of every 'Script' context regardless of the @model@ type.
debugSceneElements :: Strict.Text -> Script Scene ()
debugSceneElements msg =
  gets theSceneRegistry >>= debugPrintSceneRegistry scriptIO msg

debugPrintSceneRegistry
  :: CanWriteReports m
  => (forall a . IO a -> m a)
  -> Strict.Text
  -> Registry (Role Presence) -> m ()
debugPrintSceneRegistry liftIO msg registry = do
  report INFO $ "scene registry " <> msg
  debugPrintRegistry liftIO registry debugInfoRole debugInfoStore

-- | Steals the spotlight -- meaning it becomes the target non-mouse events such as keyboard
-- events. This function can be evaluated within the event handler for an 'Actor', it will change
-- the 'sceneFocus' of the current 'Scene' to be the 'Actor' that evaluates this function.
grabFocus :: Script Scene ()
grabFocus =
  scriptGets (thePresenceActor . theScriptActor) >>=
  assign sceneFocus . Just

----------------------------------------------------------------------------------------------------

-- | not for export
--
-- Calls 'actor'' to initialize a new 'Actor' without staging the constructed actor.
sceneToActor :: Script Scene a -> Scene -> Script any (a, Actor Scene)
sceneToActor init =
  makeActor $
  init <*
  ( use sceneRegistry >>=
    scriptIO . sceneRecountActionStats >>=
    assign sceneStats
  )

-- | A 'Scene' is a sub-group of 'Actors' that can be created within the current top-level
-- 'Scene'. This function creates an empty 'Scene' with space pre-allocated for an integer number of
-- 'Presence's to be stored, although 16 is the minimum pre-allocation size. It then evaluates an
-- initializer function that should fill the 'Scene' with 'Presence's.
--
-- The state value of the 'Script' continuation will be a value of the 'Scene' data type, but there
-- will not be much you can do to manipulate it directly. The 'Script' continuation given here for
-- the most part will simply call 'actor' or 'actress' to place actors into the scene.
--
-- __NOTE:__ that the @'Script' 'Scene' ()@ function is really only to allow you to install 'Presence's
-- into the 'Scene' using the 'actor' or 'actress' functions. You may install event handlers into
-- the 'Scene' using functions like 'onKeyboard', but these event handlers will not be used in a
-- situation where all other actors in the scene have not already captured and dispatched the events
-- received. So any event handlers you install during the initialize 'Script' evaluation should only
-- be the "last resort" event handlers that are only triggered when no other 'Presence's have responded
-- to the event.
--
-- See also: newActHapplet
newScene :: Int -> Script Scene () -> Script any (Actor Scene)
newScene size init =
  scriptIO (makeEmptyScene size) >>=
  sceneToActor init >>= \ ((), actor) ->
  return actor

sceneRecountActionStats :: Registry (Role Presence) -> IO ActorEventHandlerStats
sceneRecountActionStats registry =
  reactEventRegistryIO True
  (const $ modify . mappend . roleEventStats >=> return . const KeepObject)
  registry
  mempty

-- | This function creates an 'EventAction' that evaluates 'sceneEventHandlers' on all elements in
-- a 'Scene'.
reactScene
  :: (forall model . Lens' (Role model) (Maybe (EventAction event model)))
  -> EventAction event Scene
reactScene handler =
  EventAction
  { theActionText = ""
  , theAction = sceneEventHandlers scriptIO ((.) catchConsequence . scriptWithPresence) handler
  }

-- | Check current statistics on the current 'Scene' and update delegate event handlers.
delegateEvents
  :: (ActorEventHandlerStats -> Int)
  -> (forall model . Lens' (Role model) (Maybe (EventAction event model)))
  -> Script Scene ()
delegateEvents checkStats handle = do
  s <- use sceneStats
  scriptModify $
    scriptRole . cloneLens handle .~
    if checkStats s > 0 then Just (reactScene handle) else Nothing

-- | Call 'delegateEvents' on a 'RoleMouseEvents' handler.
delegateButtonEvents
  :: (MouseEventHandlerStats -> Int)
  -> (forall model . Lens' (RoleMouseEvents model) (Maybe (EventAction event model)))
  -> MouseButton
  -> Script Scene ()
delegateButtonEvents checkStats handle button =
  delegateEvents
  (maybe 0 checkStats . roleMouseEventStatsFor button)
  (actionMouseButton button . handle)

delegateSelectEvents :: Script Scene ()
delegateSelectEvents = delegateEvents countActionSelect actionSelect

delegateKeyboardEvents :: Script Scene ()
delegateKeyboardEvents = delegateEvents countActionKeyboard actionKeyboard

delegateMouseOverEvents :: Script Scene ()
delegateMouseOverEvents = delegateEvents countActionMouseOver actionMouseOver

delegateMouseDownEvents :: MouseButton -> Script Scene ()
delegateMouseDownEvents = delegateButtonEvents countActionMouseDown mouseButtonDown

delegateMouseClickEvents :: MouseButton -> Script Scene ()
delegateMouseClickEvents = delegateButtonEvents countActionMouseClick mouseButtonClick

delegateMouseDoubleClickEvents :: MouseButton -> Script Scene ()
delegateMouseDoubleClickEvents = delegateButtonEvents countActionMouseDouble mouseButtonDouble

delegateMouseDragEvents :: MouseButton -> Script Scene ()
delegateMouseDragEvents = delegateButtonEvents countActionMouseDrag mouseButtonDrag

-- | Delegate all mouse button event handlers for a particular 'MouseButton'.
delegateMouseButtonEvents :: MouseButton -> Script Scene ()
delegateMouseButtonEvents a = do
  delegateMouseDownEvents a
  delegateMouseClickEvents a
  delegateMouseDoubleClickEvents a
  delegateMouseDragEvents a

delegateAnimationEvents :: Script Scene ()
delegateAnimationEvents = delegateEvents countActionAnimation actionAnimation

-- | This function may be used to update the event hanlders on a @'Actor' 'Scene'@ whenever the
-- contents of the 'Scene' changes.
--
-- What this function actually does is check 'theSceneStats' and installs the correct event handlers
-- into the 'Scene's top-level event handler to automatically delegate events to the 'Presence's that
-- have been staged into the 'Scene'. If the scene has no staged 'Presence's which respond to a
-- particular event, the event handler for that particular event is set to 'Nothing'.
--
-- Note that event delegation can only be performed by 'Scene' values, as these are the only values
-- that can contain other 'Presence's.
--
-- This function calls all of 'delegateSelectEvents', 'delegateKeyboardEvents',
-- 'delegateMosueOverEvents', 'delegateMouseButtonEvents' (for both 'RightMouseButton' and
-- 'LeftMouseButton), and 'delegateAnimationEvents'.
delegateSceneEvents :: Script Scene ()
delegateSceneEvents = do
  delegateSelectEvents
  delegateKeyboardEvents
  delegateMouseOverEvents
  delegateMouseButtonEvents RightMouseButton
  delegateMouseButtonEvents LeftMouseButton
  delegateAnimationEvents

-- Functions for evaluating 'GUI' functions using elements of a 'Scene', such as event handlers.

onScene :: GUI provider Scene a -> GUI provider (Act Scene) a
onScene f = do
  ref <- theActorRole <$> use actCurrentActor
  role <- liftIO $ readIORef ref
  (a, scene) <- onSubModel f $ theRoleModel role
  liftIO $ writeIORef ref (role{ theRoleModel = scene })
  return a

-- | Not for export
--
-- Functions that operate on 'Scene' data types can broadcast delegate events to all 'Presence'
-- agents within the 'Registry', this function does that broadcasting of events to a particular
-- event handler selected by a given 'Lens'.
sceneEventHandlers
  :: (Monad m, MonadState Scene m, CanWriteReports m)
  => (forall a . IO a -> m a)
  -> (Script Presence () -> Presence -> m (Consequence ()))
  -> Lens' (Role Presence) (Maybe (EventAction event Presence))
  -> event
  -> m ()
sceneEventHandlers liftIO liftScript handler event = do
  report EVENT "sceneEventHandlers"
  scene <- get
  let registry = scene ^. sceneRegistry
  reactEventRegistry False liftIO
    (\ _update role -> case role ^. handler of
      Nothing -> pure KeepObject
      Just action ->
        lift (liftScript (runEventAction action event) (role ^. roleModel)) >>= \ case
          ActionOK ()    -> pure KeepObjectHalt
          ActionHalt     -> pure KeepObject
          ActionCancel   -> do
            lift (report OBJECT $ "delete object: " <> (role ^. roleLabel))
            pure DeleteObjectHalt
          ActionFail msg -> do
            lift (report ERROR msg)
            pure DeleteObjectHalt
    )
    registry ()
  -- Warning: this function does not update 'theSceneStats' or re-delegate any of the event
  -- handlers. It is expected that the 'sceneRedraw' or similar function will be triggered shortly
  -- after this function is called, and 'sceneRedraw' will update the statistics properly.
  debugPrintSceneRegistry liftIO "sceneEventHandlers" registry -- DEBUG

guirSceneEventHandlers
  :: (HappletWindow provider render, Happlet2DGraphics render, ProvidesLogReporter provider)
  => Lens' (Role Presence) (Maybe (EventAction event Presence))
  -> event
  -> GUI provider (Act Scene) ()
guiSceneEventHandlers lens event = do
  guiRunScript $
    sceneEventHandlers scriptIO ((.) catchConsequence . scriptWithPresence) lens event
  sceneRedraw

-- | A GUI action to force the redraw of all elements in a Scene, regardless of whether they lie
-- within the updated clip region, or whether they have requested a redraw. The bounding box in
-- 'actVisibleFrame' is still passed to the 'onCavnas' function, so the low-level drawing APIs will
-- clip all drawings to this bounding box.
--
-- This operation will slow down your application if used more often. Use 'sceneRedraw' to do a more
-- efficient redraw.
forceSceneRedraw
  :: (HappletWindow provider render, Happlet2DGraphics render, ProvidesLogReporter provider)
  => GUI provider (Act Scene) ()
forceSceneRedraw =
  flip rect2DUnion 1 . pure <$> use actVisibleFrame >>= \ uframe ->
  onScene $ 
  report EVENT "forceSceneRedraw" >>
  use sceneRegistry >>= \ registry ->
  reactEventRegistry True liftIO
  (\ update role0 -> do
      let role = role0 &
            actionDraw %~ maybe id const (role0 ^. actionRedraw) &
            actionRedraw .~ Nothing
      let drawing = (role ^. actionDraw)
      report EVENT $ "Redraw " <> (role ^. roleLabel) <> "\n" <> Strict.pack (show drawing)
      lift $ onCanvas $ draw2D uframe drawing
      modify $ (<> (roleEventStats role))
      update role
      return KeepObject
  ) registry mempty >>=
  assign sceneStats >>
  debugPrintSceneRegistry liftIO "forceSceneRedraw" registry

-- | Redraw all elements that have changed. Each element is erased (by redrawing all elements below
-- it that intersect with it's bounding box), and then redrawn.
sceneRedraw
  :: (HappletWindow provider render, ProvidesLogReporter provider, Happlet2DGraphics render)
  => GUI provider (Act Scene) ()
sceneRedraw =
  report EVENT "sceneRedraw" >>
  use actVisibleFrame >>= \ frame ->
  onScene $
  use sceneRegistry >>= \ registry ->
  -- first scan the registry for 'Presence's that need to be erased
  canonicalize2DShape . uncurry rect2DUnion <$>
  reactEventRegistryIO True
  (\ _update role -> do
     modify $ \ (stack, count) -> case role ^. actionRedraw of
       Nothing -> (stack, count)
       Just {} -> (theBoundingBox (role ^. actionDraw) : stack, seq count $! count + 1)
     return KeepObject
  ) registry ([frame], 1) >>= \ eraseRegion ->
  -- now redraw
  unless (rect2DUnionNull eraseRegion)
  ( reactEventRegistryIO True
    (\ update role0 -> case role0 ^. actionRedraw of
      Nothing -> do
        lift $ onCanvas $ draw2D eraseRegion $ role0 ^. actionDraw
        return KeepObject
      Just newDraw -> do
        lift $ onCanvas $ draw2D eraseRegion newDraw
        let role = role0 & actionDraw .~ newDraw & actionRedraw .~ Nothing
        update role
        modify $ (<> (roleEventStats role))
        return KeepObject
    ) registry mempty >>=
    assign sceneStats
  ) >>
  debugPrintSceneRegistry liftIO "sceneRedraw" registry

----------------------------------------------------------------------------------------------------

-- | Force a 'Keyboard' event to occur in the current 'Act'.
sceneKeyboardHandler
  :: ProvidesLogReporter provider
  => Keyboard -> GUI provider Scene ()
sceneKeyboardHandler evt = case evt of
  Keyboard True _ _ -> run evt
  RawKey   True _ _ -> run evt
  _                 -> return ()
  where
  run evt =
    report EVENT "sceneKeyboardHandler" >>
    use sceneFocus >>= \ case
      Nothing                -> return ()
      Just actor@(Presence ref) ->
        liftIO (readIORef ref) >>= \ role ->
        case theActionKeyboard role of
          Nothing     -> return ()
          Just script -> do
            let unfocus = sceneFocus .= Nothing
            guiRunScript (runEventAction script evt) actor >>= \ case
              ActionOK ()    -> return ()
              ActionHalt     -> empty
              ActionCancel   -> unfocus >> cancel
              ActionFail msg -> unfocus >> throwError msg

-- | Force a 'MouseSignal'-over event to occur in the current 'Act'.
sceneMouseOver
  :: ( HappletWindow provider render, Managed provider, Happlet2DGraphics render
     , ProvidesLogReporter provider
     )
  => PixelMouse -> GUI provider Scene ()
sceneMouseOver = guiSceneEventHandlers actionMouseOver

-- | Force a 'Mouse' context menu button click event to occur in the current 'Act'.
sceneMouseDown
  :: (HappletWindow provider render, Happlet2DGraphics render, ProvidesLogReporter provider)
  => MouseButton -> PixelMouse -> GUI provider Scene ()
sceneMouseDown button event = do
  report EVENT "sceneMouseDown"
  guiSceneEventHandlers (actionMouseDown button) event

-- | Force a 'Mouse' context menu button click event to occur in the current 'Act'.
sceneMouseClick
  :: (HappletWindow provider render, Happlet2DGraphics render, ProvidesLogReporter provider)
  => MouseButton -> PixelMouse -> GUI provider Scene ()
sceneMouseClick button event = do
  report EVENT "sceneMouseClick"
  guiSceneEventHandlers (actionMouseClick button) event

-- | Force a 'Mouse' action button double click event to occur in the current 'Act'.
sceneMouseDoubleClick
  :: (HappletWindow provider render, Happlet2DGraphics render, ProvidesLogReporter provider)
  => MouseButton -> PixelMouse -> GUI provider Scene ()
sceneMouseDoubleClick button event = do
  report EVENT "sceneMouseDoubleClick"
  guiSceneEventHandlers (actionMouseDouble button) event

-- | Force a 'MouseSignal' drag event to occur in the current 'Act'.
sceneMouseDrag
  :: ( HappletWindow provider render, Managed provider, Happlet2DGraphics render
     , ProvidesLogReporter provider
     )
  => MouseButton -> Maybe PixelMouse -> GUI provider Scene ()
sceneMouseDrag button event = do
  report DEBUG_ALL "sceneMouseDrag"
  guiSceneEventHandlers (actionMouseDrag button) event
