-- | A 'Stage' makes use of the 'Happlets.Actor.Presence' data type to construct an array of untyped
-- actors that can respond to events.
module Happlets.Actor.Stage
  ( -- ** Stages
    Stage, newStage, stageBracket, grabFocus, delegateStageEvents,
    debugStageElements, debugPrintStageRegistry
    stageRedraw, forceStageRedraw,
    stageKeyboardHandler, actAnimationHandler,
    stageMouseDown, stageMouseClick, stageMouseDoubleClick,
    stageMouseOver, stageMouseDrag,
  )
  where

import           Happlets.Actor
                 ( Actor, Presence,
                   OnQueue, onDraw, scriptRedraw, onSelect, onKeyPress, onAnimate,
                   onMouseOver, onMouseDown, onMouseClick, onMouseDoubleClick, onMouseDrag,
                   actorFocus, actorKeyPress,
                   actorMouseDown, actorMouseClick, actorMouseDoubleClick, actorMouseDrag,
                   actorStepAnimate,
                   ActorEventHandlerStats(..), MouseEventHandlerStats(..),
                   getEventHandlerStats, diffActorEventHandlerStats
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

-- | A 'Stage' is a model of a 2D canvas containing many 'Presence' objects, within which all can
-- update the canvas and can respond to canvas events. When an 'Act' is first constructed by
-- 'newActHapplet' it already has a new 'Stage' ready to be populated with 'Presence's, so it is not
-- necessary to create a new 'Stage', but keeping 'Presence's organized into 'Stage's and changing
-- between 'Stage's is a good way to keep a user interface less cluttered.
--
-- A 'Stage' itself can be used as the @model@ of a 'Actor', so you can use the 'Stage' as an
-- 'Presence' in another stage, and events can be delegated to the 'Stage' through the 'Actor'
-- event handler APIs. When you create a 'Stage' using 'newStage', a 'Actor' containing the
-- 'Stage' is returned.
data Stage
  = Stage
    { theStageRegistry   :: !(Registry (Role Presence))
      -- ^ The objects in this stage
    , theStageFocus      :: !(Maybe Presence)
      -- ^ A reference to the object in 'theStageRegistry' that currently responds to keyboard
      -- events or double-click events. Also, an object that is being dragged necessarily has focus.
    , theStageStats      :: !ActorEventHandlerStats
      -- ^ Statistics about the number of event handlers installed across all 'Presence's within the
      -- 'Stage', this helps to determine if the 'Stage' should install it's own event handler to
      -- delegate events to any of it's 'Presence's.
    , theStageGlobalBounds :: !(Rect2D SampCoord)
      -- ^ The 'Rect2D', in global coordinates of this 'Stage's viewable window, in the 'GUI'
      -- monad's coordinate system.
    }

-- | not for export
--
-- The Registry for the 'Stage' contains a reference to all 'Actors'.
stageRegistry :: Lens' Stage (Registry (Role Presence))
stageRegistry = lens theStageRegistry $ \ a b -> a{ theStageRegistry = b }

-- | The 'Presence' that currently has focus.
stageFocus :: Lens' Stage (Maybe Presence)
stageFocus = lens theStageFocus $ \ a b -> a{ theStageFocus = b }

-- | The 'Rect2D', in global coordinates of this 'Stage's viewable window, in the 'GUI' monad's
-- coordinate system.
stageGlobalBounds :: Lens' Stage (Rect2D SampCoord)
stageGlobalBounds = lens theStageGlobalBounds $ \ a b -> a{ theStageGlobalBounds = b }

-- | not for export
--
-- Tracks statistics on how many elements in a 'Stage' respond to particular events. It is
-- determined by an aggregate computation over all @('Role' 'Presence')@ items in a 'Stage's
-- 'Registry'.
stageStats :: Lens' Stage ActorEventHandlerStats
stageStats = lens theStageStats $ \ a b -> a{ theStageStats = b }

-- | not for export
--
-- Place a 'Presence' on stage, making it visible and able to respond to events.
stagePresence :: Presence -> Script Stage ()
stagePresence (Presence actorRef) = do
  use stageRegistry >>= scriptIO . registryEnqueue actorRef
  role <- scriptIO (readIORef actorRef)
  let stats = roleEventStats role
  report OBJECT $
    "Staging actor: " <> (role ^. roleLabel) <>
    "\n  stats:\n" <> Strict.pack (show stats)
  stageStats <>= stats
  stats <- use stageStats
  report OBJECT ("Stage stats after staging actor:\n" <> Strict.pack (show stats))

-- | Place a 'Actor' on stage, making it visible and able to respond to events. After defining
-- a 'Actor' using 'actor' or 'actress', it is necessary to call this 'onStage' function in
-- order for the 'Actor' begin acting on stage and responding to events and cues.
onStage :: Actor model -> Script Stage ()
onStage = stagePresence . thePresenceActor

-- | Function used internally by 'newActHapplet' to create a new stage. Stages that can be
-- manipulated by end users of this API are always wrapped in a 'Actor'.
makeEmptyStage :: Int -> IO Stage
makeEmptyStage size = do
  registry <- newRegistry size
  return Stage
    { theStageRegistry     = registry
    , theStageFocus        = Nothing
    , theStageStats        = mempty
    , theStageGlobalBounds = rect2D
    }

----------------------------------------------------------------------------------------------------

-- | Print a debug reporrt of the content of the current 'Stage' to standard output. Note that if
-- @model@ type is bound to 'Stage', that is, if you evaluate 'debugStageElements' in a 'Script'
-- context of type @'Script' 'Stage' ()@ this function will not necessarily produce a debug report
-- about the same 'Stage' that would be returned by 'get', the debug report is only about the
-- internal (hidden) 'Stage' that is part of every 'Script' context regardless of the @model@ type.
debugStageElements :: Strict.Text -> Script Stage ()
debugStageElements msg =
  gets theStageRegistry >>= debugPrintStageRegistry scriptIO msg

debugPrintStageRegistry
  :: CanWriteReports m
  => (forall a . IO a -> m a)
  -> Strict.Text
  -> Registry (Role Presence) -> m ()
debugPrintStageRegistry liftIO msg registry = do
  report INFO $ "stage registry " <> msg
  debugPrintRegistry liftIO registry debugInfoRole debugInfoStore

debugInfoStore :: CanWriteReports m => Store (Role Presence) -> m ()
debugInfoStore = report INFO . Strict.pack . show

-- | Steals the spotlight -- meaning it becomes the target non-mouse events such as keyboard
-- events. This function can be evaluated within the event handler for an 'Actor', it will change
-- the 'stageFocus' of the current 'Stage' to be the 'Actor' that evaluates this function.
grabFocus :: Script Stage ()
grabFocus =
  scriptGets (thePresenceActor . theScriptActor) >>=
  assign stageFocus . Just

----------------------------------------------------------------------------------------------------

-- | not for export
--
-- Calls 'actor'' to initialize a new 'Actor' without staging the constructed actor.
stageToActor :: Script Stage a -> Stage -> Script any (a, Actor Stage)
stageToActor init =
  makeActor $
  init <*
  ( use stageRegistry >>=
    scriptIO . stageRecountActionStats >>=
    assign stageStats
  )

-- | A 'Stage' is a sub-group of 'Actors' that can be created within the current top-level
-- 'Stage'. This function creates an empty 'Stage' with space pre-allocated for an integer number of
-- 'Presence's to be stored, although 16 is the minimum pre-allocation size. It then evaluates an
-- initializer function that should fill the 'Stage' with 'Presence's.
--
-- The state value of the 'Script' continuation will be a value of the 'Stage' data type, but there
-- will not be much you can do to manipulate it directly. The 'Script' continuation given here for
-- the most part will simply call 'actor' or 'actress' to place actors into the stage.
--
-- __NOTE:__ that the @'Script' 'Stage' ()@ function is really only to allow you to install 'Presence's
-- into the 'Stage' using the 'actor' or 'actress' functions. You may install event handlers into
-- the 'Stage' using functions like 'onKeyboard', but these event handlers will not be used in a
-- situation where all other actors in the stage have not already captured and dispatched the events
-- received. So any event handlers you install during the initialize 'Script' evaluation should only
-- be the "last resort" event handlers that are only triggered when no other 'Presence's have responded
-- to the event.
--
-- See also: newActHapplet
newStage :: Int -> Script Stage () -> Script any (Actor Stage)
newStage size init =
  scriptIO (makeEmptyStage size) >>=
  stageToActor init >>= \ ((), actor) ->
  return actor

stageRecountActionStats :: Registry (Role Presence) -> IO ActorEventHandlerStats
stageRecountActionStats registry =
  reactEventRegistryIO True
  (const $ modify . mappend . roleEventStats >=> return . const KeepObject)
  registry
  mempty

-- | This function creates an 'EventAction' that evaluates 'stageEventHandlers' on all elements in
-- a 'Stage'.
reactStage
  :: (forall model . Lens' (Role model) (Maybe (EventAction event model)))
  -> EventAction event Stage
reactStage handler =
  EventAction
  { theActionText = ""
  , theAction = stageEventHandlers scriptIO ((.) catchConsequence . scriptWithPresence) handler
  }

-- | Check current statistics on the current 'Stage' and update delegate event handlers.
delegateEvents
  :: (ActorEventHandlerStats -> Int)
  -> (forall model . Lens' (Role model) (Maybe (EventAction event model)))
  -> Script Stage ()
delegateEvents checkStats handle = do
  s <- use stageStats
  scriptModify $
    scriptRole . cloneLens handle .~
    if checkStats s > 0 then Just (reactStage handle) else Nothing

-- | Call 'delegateEvents' on a 'RoleMouseEvents' handler.
delegateButtonEvents
  :: (MouseEventHandlerStats -> Int)
  -> (forall model . Lens' (RoleMouseEvents model) (Maybe (EventAction event model)))
  -> MouseButton
  -> Script Stage ()
delegateButtonEvents checkStats handle button =
  delegateEvents
  (maybe 0 checkStats . roleMouseEventStatsFor button)
  (actionMouseButton button . handle)

delegateSelectEvents :: Script Stage ()
delegateSelectEvents = delegateEvents countActionSelect actionSelect

delegateKeyboardEvents :: Script Stage ()
delegateKeyboardEvents = delegateEvents countActionKeyboard actionKeyboard

delegateMouseOverEvents :: Script Stage ()
delegateMouseOverEvents = delegateEvents countActionMouseOver actionMouseOver

delegateMouseDownEvents :: MouseButton -> Script Stage ()
delegateMouseDownEvents = delegateButtonEvents countActionMouseDown mouseButtonDown

delegateMouseClickEvents :: MouseButton -> Script Stage ()
delegateMouseClickEvents = delegateButtonEvents countActionMouseClick mouseButtonClick

delegateMouseDoubleClickEvents :: MouseButton -> Script Stage ()
delegateMouseDoubleClickEvents = delegateButtonEvents countActionMouseDouble mouseButtonDouble

delegateMouseDragEvents :: MouseButton -> Script Stage ()
delegateMouseDragEvents = delegateButtonEvents countActionMouseDrag mouseButtonDrag

-- | Delegate all mouse button event handlers for a particular 'MouseButton'.
delegateMouseButtonEvents :: MouseButton -> Script Stage ()
delegateMouseButtonEvents a = do
  delegateMouseDownEvents a
  delegateMouseClickEvents a
  delegateMouseDoubleClickEvents a
  delegateMouseDragEvents a

delegateAnimationEvents :: Script Stage ()
delegateAnimationEvents = delegateEvents countActionAnimation actionAnimation

-- | This function may be used to update the event hanlders on a @'Actor' 'Stage'@ whenever the
-- contents of the 'Stage' changes.
--
-- What this function actually does is check 'theStageStats' and installs the correct event handlers
-- into the 'Stage's top-level event handler to automatically delegate events to the 'Presence's that
-- have been staged into the 'Stage'. If the stage has no staged 'Presence's which respond to a
-- particular event, the event handler for that particular event is set to 'Nothing'.
--
-- Note that event delegation can only be performed by 'Stage' values, as these are the only values
-- that can contain other 'Presence's.
--
-- This function calls all of 'delegateSelectEvents', 'delegateKeyboardEvents',
-- 'delegateMosueOverEvents', 'delegateMouseButtonEvents' (for both 'RightMouseButton' and
-- 'LeftMouseButton), and 'delegateAnimationEvents'.
delegateStageEvents :: Script Stage ()
delegateStageEvents = do
  delegateSelectEvents
  delegateKeyboardEvents
  delegateMouseOverEvents
  delegateMouseButtonEvents RightMouseButton
  delegateMouseButtonEvents LeftMouseButton
  delegateAnimationEvents

-- Functions for evaluating 'GUI' functions using elements of a 'Stage', such as event handlers.

onStage :: GUI provider Stage a -> GUI provider (Act Stage) a
onStage f = do
  ref <- theActorRole <$> use actCurrentActor
  role <- liftIO $ readIORef ref
  (a, stage) <- onSubModel f $ theRoleModel role
  liftIO $ writeIORef ref (role{ theRoleModel = stage })
  return a

-- | Not for export
--
-- Functions that operate on 'Stage' data types can broadcast delegate events to all 'Presence'
-- agents within the 'Registry', this function does that broadcasting of events to a particular
-- event handler selected by a given 'Lens'.
stageEventHandlers
  :: (Monad m, MonadState Stage m, CanWriteReports m)
  => (forall a . IO a -> m a)
  -> (Script Presence () -> Presence -> m (Consequence ()))
  -> Lens' (Role Presence) (Maybe (EventAction event Presence))
  -> event
  -> m ()
stageEventHandlers liftIO liftScript handler event = do
  report EVENT "stageEventHandlers"
  stage <- get
  let registry = stage ^. stageRegistry
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
  -- Warning: this function does not update 'theStageStats' or re-delegate any of the event
  -- handlers. It is expected that the 'stageRedraw' or similar function will be triggered shortly
  -- after this function is called, and 'stageRedraw' will update the statistics properly.
  debugPrintStageRegistry liftIO "stageEventHandlers" registry -- DEBUG

guiStageEventHandlers
  :: (HappletWindow provider render, Happlet2DGraphics render, ProvidesLogReporter provider)
  => Lens' (Role Presence) (Maybe (EventAction event Presence))
  -> event
  -> GUI provider (Act Stage) ()
guiStageEventHandlers lens event = do
  guiRunScript $
    stageEventHandlers scriptIO ((.) catchConsequence . scriptWithPresence) lens event
  stageRedraw

-- | A GUI action to force the redraw of all elements in a Stage, regardless of whether they lie
-- within the updated clip region, or whether they have requested a redraw. The bounding box in
-- 'actVisibleFrame' is still passed to the 'onCavnas' function, so the low-level drawing APIs will
-- clip all drawings to this bounding box.
--
-- This operation will slow down your application if used more often. Use 'stageRedraw' to do a more
-- efficient redraw.
forceStageRedraw
  :: (HappletWindow provider render, Happlet2DGraphics render, ProvidesLogReporter provider)
  => GUI provider (Act Stage) ()
forceStageRedraw =
  flip rect2DUnion 1 . pure <$> use actVisibleFrame >>= \ uframe ->
  onStage $ 
  report EVENT "forceStageRedraw" >>
  use stageRegistry >>= \ registry ->
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
  assign stageStats >>
  debugPrintStageRegistry liftIO "forceStageRedraw" registry

-- | Redraw all elements that have changed. Each element is erased (by redrawing all elements below
-- it that intersect with it's bounding box), and then redrawn.
stageRedraw
  :: (HappletWindow provider render, ProvidesLogReporter provider, Happlet2DGraphics render)
  => GUI provider (Act Stage) ()
stageRedraw =
  report EVENT "stageRedraw" >>
  use actVisibleFrame >>= \ frame ->
  onStage $
  use stageRegistry >>= \ registry ->
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
    assign stageStats
  ) >>
  debugPrintStageRegistry liftIO "stageRedraw" registry

----------------------------------------------------------------------------------------------------

-- | Force a 'Keyboard' event to occur in the current 'Act'.
stageKeyboardHandler
  :: ProvidesLogReporter provider
  => Keyboard -> GUI provider Stage ()
stageKeyboardHandler evt = case evt of
  Keyboard True _ _ -> run evt
  RawKey   True _ _ -> run evt
  _                 -> return ()
  where
  run evt =
    report EVENT "stageKeyboardHandler" >>
    use stageFocus >>= \ case
      Nothing                -> return ()
      Just actor@(Presence ref) ->
        liftIO (readIORef ref) >>= \ role ->
        case theActionKeyboard role of
          Nothing     -> return ()
          Just script -> do
            let unfocus = stageFocus .= Nothing
            guiRunScript (runEventAction script evt) actor >>= \ case
              ActionOK ()    -> return ()
              ActionHalt     -> empty
              ActionCancel   -> unfocus >> cancel
              ActionFail msg -> unfocus >> throwError msg

-- | Force a 'MouseSignal'-over event to occur in the current 'Act'.
stageMouseOver
  :: ( HappletWindow provider render, Managed provider, Happlet2DGraphics render
     , ProvidesLogReporter provider
     )
  => PixelMouse -> GUI provider Stage ()
stageMouseOver = guiStageEventHandlers actionMouseOver

-- | Force a 'Mouse' context menu button click event to occur in the current 'Act'.
stageMouseDown
  :: (HappletWindow provider render, Happlet2DGraphics render, ProvidesLogReporter provider)
  => MouseButton -> PixelMouse -> GUI provider Stage ()
stageMouseDown button event = do
  report EVENT "stageMouseDown"
  guiStageEventHandlers (actionMouseDown button) event

-- | Force a 'Mouse' context menu button click event to occur in the current 'Act'.
stageMouseClick
  :: (HappletWindow provider render, Happlet2DGraphics render, ProvidesLogReporter provider)
  => MouseButton -> PixelMouse -> GUI provider Stage ()
stageMouseClick button event = do
  report EVENT "stageMouseClick"
  guiStageEventHandlers (actionMouseClick button) event

-- | Force a 'Mouse' action button double click event to occur in the current 'Act'.
stageMouseDoubleClick
  :: (HappletWindow provider render, Happlet2DGraphics render, ProvidesLogReporter provider)
  => MouseButton -> PixelMouse -> GUI provider Stage ()
stageMouseDoubleClick button event = do
  report EVENT "stageMouseDoubleClick"
  guiStageEventHandlers (actionMouseDouble button) event

-- | Force a 'MouseSignal' drag event to occur in the current 'Act'.
stageMouseDrag
  :: ( HappletWindow provider render, Managed provider, Happlet2DGraphics render
     , ProvidesLogReporter provider
     )
  => MouseButton -> Maybe PixelMouse -> GUI provider Stage ()
stageMouseDrag button event = do
  report DEBUG_ALL "stageMouseDrag"
  guiStageEventHandlers (actionMouseDrag button) event
