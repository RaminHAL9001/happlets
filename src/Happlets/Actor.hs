-- | This module provides an 'Scene', 'Actor', and 'Presence' data types, and the 'Script' function
-- type. An 'Scene' is initialized into a 'Happlet' using 'newActHapplet', and it then takes control
-- over the low-level Happlets event signal stream, especially the 'MouseSignal' events, and
-- translates these into higher-level discrete events like 'onMouseClick' and 'onDoubleClick'. These
-- events can then be delegated to 'Actor's which in turn execute 'Script's that can draw elements
-- on the Happlet canvas. Most of the lower-level details of the Happlets 'GUI' are abstracted away
-- behind the 'Script' function type, which simplifies program logic a great deal.
module Happlets.Actor
  ( -- ** Scenes
    Scene, sceneWindow, newSceneHapplet, sceneLiftIO,

    -- ** The Script function type
    Script, scriptWithActor, scriptWithPresence,

    -- ** The Actor data type
    Actor, actor, actress, modifySelfLabel, getSelfLabel,

    -- *** The Presence data type
    Presence, thePresenceActor, actorPresence, getPresenceLabel,

    -- ** Event Handlers
    OnQueue, onDraw, onFocus, onKeyPress, onStepAnimate,
    onMouseOver, onMouseDown, onMouseClick, onMouseDoubleClick, onMouseDrag,

    -- *** Event Types
    Mouse2D(..), PixelMouse, MouseButton(..), mouse2DPosition, mouse2DModifiers,
    module Happlets.Control.Keyboard,

    -- *** Event Handler Types
    EventAction(..), runEventAction,

    -- *** Delegating Events
    --
    -- These functions trigger an event handler on some other 'Actor' which is known to the current
    -- 'Actor'. You can simulate events in animations using these functions as well.
    actorDraw, actorRedraw, actorFocus, actorKeyPress,
    actorMouseOver, actorMouseDown, actorMouseClick, actorMouseDoubleClick, actorMouseDrag,
    actorStepAnimate,
    -- ** Executing Scripts from within 'GUI'
    --
    -- If you like the whole 'Actor'-'Scene'-'Script' model provided in this code library, but you
    -- don't like how events are dispatched, you can come up with your own method of executing
    -- 'Script's within the 'GUI' function, and you can force event handlers to trigger.
    guiRunScriptWith, guiRunScript, unsafeScriptIO,

    -- *** Forcing Events from within GUI
    guiForceRedraw, guiRedraw, guiKeyPress, guiFocus,
    guiMouseDown, guiMouseClick, guiMouseDoubleClick, guiMouseDrag,
    guiStepAnimate,

    -- ** Actor event handler accounting
    --
    -- Used to count which event handlers are set for particular 'Actor's. This is useful for
    -- dealing with a group of 'Presence' actors where you cannot know precisely which 'Actor's are
    -- present, but you can know what events to which their 'Presence' will effect the state of the
    -- app.
    ActorEventHandlerStats(..), MouseEventHandlerStats(..),
    getEventHandlerStats, diffActorEventHandlerStats,
    debugEventHandlerStats,
  ) where

import           Happlets.Logging
                 ( CanWriteReports(report), LogReporter,
                   ReportLevel(ERROR, INFO, OBJECT, EVENT, SIGNAL, DEBUG_ALL)
                 )
import           Happlets.Initialize (Initialize, newHappletIO, theActualLogReporter)
import           Happlets.Model.GUI
                 ( GUI, Happlet, ProvidesLogReporter, guiLogReportWriter
                 )
import           Happlets.View
                 ( Happlet2DGraphics(draw2D, clearRegions),
                   HappletWindow(onCanvas),
                   Sized2DRaster(getViewSize), getViewRect,
                   Has2DOrigin(origin2D)
                 )
import           Happlets.View.Color (black, alphaChannel)
import           Happlets.View.Types2D
                 ( SampCoord, PixSize, PixCoord, Point2D, V2(..),
                   Rect2D, rect2D, point2D, rect2DSize, rect2DHead, rect2DTail,
                   Drawing, drawingIsNull, theRect2DUnionMask, rect2DUnionSingle
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
                   Keyboard(Keyboard, RawKey), keyIsPressed,
                   ModifierBits
                 )
import           Happlets.Control.WindowManager

import           Control.Applicative (Alternative(..))
import           Control.Arrow ((&&&))
import           Control.Lens
                 ( Lens', lens, cloneLens, view, set, use,
                   (&), (^.), (%~), (.~), (.=), (%=)
                 )
import           Control.Monad (MonadPlus(..), guard, when, (>=>))
import           Control.Monad.Except (MonadError(throwError, catchError))
--import           Control.Monad.Fail -- uncomment if building with GHC version less than 8.8
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

-- | The 'Script' function type is a procedure (a script) that defines the behavior of an 'Actor'
-- while is active in an 'Scene'. The 'Script' operates on a 'Actor' containing @model@ state data,
-- and so the 'Script' @model@ type will always be the same as the 'Actor's @model@ type.
--
-- The 'Script' function type is monadic and instantiates 'MonadState' so that the @model@ data
-- can be inspected and updated using the ordinary 'get', 'put', 'modify', and 'state' functions.
--
-- There are also several APIs defined below, many of which begin with the name "self", which are
-- used to alter the event handlers of the 'Actor' that the 'Script' is currently operating on.
--
-- 'Script' instantiates the 'CancelableAction' function, so you can evaluate 'cancel' somwhere in
-- the 'Script' to indicate that this particular event should no longer be reacted to by an 'Actor'.
--
-- The 'Script' function type also instantiates 'Sized2DRaster', so it is possible to use
-- 'getViewSize' and use this information to render 'Drawing's to within a particular area.
newtype Script model a
  = Script{ unwrapScript :: ConsequenceT (StateT (ScriptState model) IO) a }
  deriving Functor -- NOT deriving MonadIO

data ScriptState model
  = ScriptState
    { theScriptRole        :: !(Role model) -- ^ The model currently being updated.
    , theScriptActor       :: !(Actor model)
    , theScriptFrame       :: !(Rect2D SampCoord)
    , theScriptLogger      :: !(LogReporter IO)
    , theScriptRoleUpdated :: !Bool
      -- ^ Records whether event handlers (and only event handlers) in 'theScriptRole' have been
      -- modified.
    }

instance Applicative (Script model) where
  pure = Script . pure
  (Script f) <*> (Script a) = Script $ f <*> a

instance Monad (Script model) where
  return = pure
  (Script a) >>= f = Script $ a >>= unwrapScript . f

instance MonadState model (Script model) where
  state f = Script $ do
    (a, widget) <- fmap f $ lift $  use $ scriptRole . roleModel
    lift $ scriptRole . roleModel .= widget
    return a

instance MonadError Strict.Text (Script model) where
  throwError = Script . throwError
  catchError (Script try) catch = Script $ catchError try $ unwrapScript . catch

instance MonadFail (Script model) where { fail = throwError . Strict.pack; }

instance Alternative (Script model) where
  empty = Script empty
  (Script a) <|> (Script b) = Script $ a <|> b

instance MonadPlus (Script model) where { mzero = empty; mplus = (<|>); }

instance Consequential (Script model) where { cancel = Script cancel; }

instance CatchConsequence (Script model) where
  catchConsequence (Script (ConsequenceT f)) =
    Script . ConsequenceT . StateT $
    runStateT f >=> \ (result, st) ->
    pure (pure result, st)

instance ThrowConsequence (Script model) where
  throwConsequence = Script . throwConsequence

instance CanWriteReports (Script any) where
  report lvl msg =
    scriptGets theScriptLogger >>= \ log -> unsafeScriptIO $ log lvl msg

instance Monoid a => Semigroup (Script model a) where
  a <> b = mappend <$> (a <|> pure mempty) <*> (b <|> pure mempty)

instance Monoid a => Monoid (Script model a) where
  mempty = pure mempty
  mappend = (<>)

instance Sized2DRaster (Script model) where
  getViewSize = scriptGets $ rect2DSize . theScriptFrame

-- | Ask a 'Actor' to perform a 'Script', which updates the @model@ of a 'Actor'. Returns
-- a value indicating whether the internals of the 'Actor' (either the @model@ or any of the
-- event handlers) have changed.
runScript
  :: Script model a
  -> Rect2D SampCoord
  -> LogReporter IO
  -> Actor model
  -> IO (Consequence a, ScriptState model)
runScript (Script f) frame logger actor@(Actor{thePresenceActor=(Presence untyped)}) = do
  role0 <- readIORef (theActorRole actor)
  (result, st) <- runStateT (runConsequenceT f) ScriptState
    { theScriptRole   = role0
    , theScriptActor  = actor
    , theScriptFrame  = frame
    , theScriptLogger = logger
    , theScriptRoleUpdated = False
    } 
  when (theScriptRoleUpdated st) $ do
    let role  = theScriptRole st
    writeIORef (theActorRole actor) role
    writeIORef untyped $ encloseRole actor role
  return (result, st)

-- | Perform an arbitrary @IO@ action within a 'Script' function. This NOT guaranteed to be thread
-- safe at all, not even a little bit. If you are only making updates to mutable arrays or IORefs,
-- then it should be OK to use. But if you are spawning threads or updating 'MVar', all such
-- behavior is undefined.
unsafeScriptIO :: IO a -> Script model a
unsafeScriptIO = Script . liftIO

-- | not for export
scriptGets :: (ScriptState model -> a) -> Script model a
scriptGets = Script . lift . gets

scriptModify :: (ScriptState model -> ScriptState model) -> Script model ()
scriptModify = Script . lift . modify

scriptGetsRole :: (Role model -> a) -> Script model a
scriptGetsRole = scriptGets . (. theScriptRole)

-- | Access to the @model@ that is being updated by this 'Script'.
scriptRole :: Lens' (ScriptState model) (Role model)
scriptRole = lens theScriptRole $ \ a b -> a{ theScriptRole = b, theScriptRoleUpdated = True }

-- | Access to the 'Rect2D' bounding box that delimits the region into which the 'Drawing's can be
-- drawn.
scriptFrame :: Lens' (ScriptState model) (Rect2D SampCoord)
scriptFrame = lens theScriptFrame $ \ a b -> a{ theScriptFrame = b }

-- | A 'Actor' allows you to assign an arbitrary text string to it called a "label" to identify
-- actors, which is especially useful when debugging. This text string can be used for any reason at
-- all, it need not be unique. It is usually used to provide some kind of information to end users
-- about the 'Actor', like what it is and where it originated, or perhaps even the code that
-- constructed it.
--
-- Provide a function for modifying the description, the modified description is returned. Evaluate
-- @('selfDescribe' 'id')@ to retrieve the description without modifying it.
modifySelfLabel :: (Strict.Text -> Strict.Text) -> Script model Strict.Text
modifySelfLabel f = scriptModify (scriptRole . roleLabel %~ f) >> getSelfLabel

-- | Get the self-applied label set by 'modifyLabel'. This arbitrary text can be used to identify
-- actors, and is especially useful for debugging.
getSelfLabel :: Script model Strict.Text
getSelfLabel = scriptGetsRole theRoleLabel

-- | This function hands control over to another 'Actor' to act out another 'Script' function, then
-- returns control to the current 'Script' and 'Actor'.
--
-- See also: 'guiRunScriptWith'
scriptWithActor
  :: Script other a
  -> Actor other
  -> Script model a
scriptWithActor f ref =
  Script . ConsequenceT . StateT $ \ st0 ->
  runScript f (st0 ^. scriptFrame) (theScriptLogger st0) ref >>= \ (result, st) -> pure
  ( result
  , st0
    { theScriptFrame = theScriptFrame st
    , theScriptLogger = theScriptLogger st
    , theScriptRoleUpdated = theScriptRoleUpdated st
    }
  )

-- | Evaluates the continuation 'Script' (the first argument) via the 'Presence' by evaluating
-- 'scriptWithActor' on an 'actorEventActor' of the 'Actor'. When evaluating 'runScript' on a , if
-- 'Actor' in this way, the 'runScript' function is actually evaluated twice. The first invocation
-- triggeres event handlers on the 'Actor', and this in turn invokes the event handlers on the
-- 'Actor'. The 'Actor' event handler updates both itself and the 'Actor's event handlers and
-- returns control to the first invocation on 'Actor'. If the continuation 'Script' makes __any__
-- update to it's own model or event handlers, these updates will be silently discarded because they
-- are triggering updates on an out-of-date copy of the 'Actor's state.
--
-- See also: 'guiInterpretScript'.
scriptWithPresence :: Script Presence a -> Presence -> Script model a
scriptWithPresence f =
  scriptWithActor
  ( f <*
    scriptModify (scriptRoleUpdated .~ False)
    -- Force updates directly on the @'Actor' 'Presence' to be ignored, updates are made by the
    -- event handlers and then copied over to the 'Presence'
  ) . actorPresence

----------------------------------------------------------------------------------------------------

-- | While not an 'OnQueue' function, this function can be used to queue a drawing operation for the
-- 'Actor' for the current @model@ that is being 'Script'ed.
onDraw :: Drawing SampCoord -> Script model ()
onDraw = scriptModify . set (scriptRole . actionRedraw) . Just

-- | A function of type 'OnQueue' is an instruction to modify the behavior of a 'Actor', it
-- sets an 'EventAction' handler for a 'Actor'. All 'OnQueue' functions take a continuation
-- function that takes the currently installed 'EventAction' handler function (if any) and returns a
-- new 'EventAction' handler which may or may not evaluate the currently installed function as part
-- of it's behavior.
--
-- An 'OnQueue' function evaluates to a 'Script' type, so any function of type 'OnQueue' can be used
-- within the context of a 'Script', as long as you pass to the 'OnQueue' a continuation function of
-- the expected type.
type OnQueue event model =
      (Maybe (EventAction event model) -> EventAction event model) -> Script model ()

-- | not for export
--
-- A function that constructs an 'OnQueue' function.
onQueue :: Lens' (Role model) (Maybe (EventAction event model)) -> OnQueue event model
onQueue handle f = do
  scriptModify $
    (scriptRoleUpdated .~ True) .
    (scriptRole . cloneLens handle %~ Just . f)
  role <- scriptGets theScriptRole
  report OBJECT $ Strict.pack $
    "Updated event handlers for " ++
    show (theRoleLabel role) ++ ":\n" ++
    show (roleEventStats role)

-- | Alter the selection behavior of the 'Actor'.
--
-- Object that responds to clicks often need a function to decide whether the click lands on the
-- selectable portion of the 'Role's visualization, this field sets that function, and whether or
-- not a click landing on an object causes the object to become selected. The top-level 'Actor' will
-- receive focus events if the window system can enable or disable windows. The event data is simply
-- a 'Bool' value set to 'True' if the focus is received and 'False' if the focus is lost.
onFocus :: OnQueue Bool model
onFocus = onQueue actionFocus

-- | Alter the keyboard event handler function.
onKeyPress :: OnQueue Keyboard model
onKeyPress = onQueue actionKeyPress

-- | Mouse-over events are events that occur when the mouse moves, but none of the mouse buttons are
-- depressed.
onMouseOver ::OnQueue PixelMouse model
onMouseOver = onQueue actionMouseOver

-- | Set an event handler for a leading-edge mouse-down event. A mouse down an event triggered as
-- soon as the button press is detected. It is different from an 'onMouseClick' event which is
-- triggered after one complete mouse down event and one complete mouse up event. One example of
-- where an 'onMouseDown' event is useful would be for a file manager which selects files with mouse
-- down events. Usually a mouse down immediately highlights the item under the mouse cursor, while a
-- second mouse down (see 'onMouseDoubleClick') triggers an open file action on the item.
--
-- Specify the left or right mouse button that that should trigger the event handler continuation
-- supplied as the second argument.
onMouseDown :: MouseButton -> OnQueue PixelMouse model
onMouseDown button = onQueue $ cloneLens (actionMouseDown button)

-- | Set an event handler for a trailing-edge mouse-down event. A mouse click is a mouse-down event
-- followed by a mouse-up event, the action is triggered as soon as the mouse-up event is
-- received. One example of where an event like this is useful would be for a push button, which are
-- usually highlighted on a mouse-down event, and triggered on a mouse-up event.
--
-- Specify the left or right mouse button that that should trigger the event handler continuation
-- supplied as the second argument.
onMouseClick :: MouseButton -> OnQueue PixelMouse model
onMouseClick button = onQueue $ cloneLens (actionMouseClick button)

-- | Set an event handler for a double-click mouse event. A double click is a mouse-down event,
-- followed by a mouse up event, followed by another mouse down event. The event handler is
-- triggered on the second mouse down event.
onMouseDoubleClick :: MouseButton -> OnQueue PixelMouse model
onMouseDoubleClick button = onQueue $ cloneLens (actionMouseDouble button)

-- | Set an event handler for a mouse drag event. A drag event is a mouse down event followed by a
-- mouse-over event. Every mouse-over event triggers the drag event handler. When a mouse-up event
-- is received, the event handler is triggered one final time with 'Nothing' as the event data.
onMouseDrag :: MouseButton -> OnQueue (Maybe PixelMouse) model
onMouseDrag button = onQueue $ cloneLens (actionMouseDrag button)

-- | If any 'Actor' has an animation type event handler set, an animation event loop is enabled, and
-- animation frame step events are broadcast to all 'Actor's who have set this event handler,
-- repeatedly and at very fast regular intervals.
onStepAnimate :: OnQueue UTCTime model
onStepAnimate = onQueue actionStepAnimate

-- | Set to 'True' if any part of the 'scriptRole' is changed.
scriptRoleUpdated :: Lens' (ScriptState model) Bool
scriptRoleUpdated = lens theScriptRoleUpdated $ \ a b -> a{ theScriptRoleUpdated = b }

----------------------------------------------------------------------------------------------------

-- | A 'Role' contains some arbitrary typed @model@ data, and a set of event handlers that can
-- operate on this data.
--
-- In object oriented programming terms, this data type is more of a class than an object that
-- intantiates the class. The 'actor' and 'actress' functions creates an instance of the class.
data Role model
  = Role
    { theRoleModel         :: !model
    , theRoleLabel         :: !Strict.Text
    , theActionDraw        :: !(Drawing SampCoord)
    , theActionRedraw      :: !(Maybe (Drawing SampCoord))
    , theActionFocus       :: !(Maybe (EventAction Bool model))
    , theActionKeyPress    :: !(Maybe (EventAction Keyboard model))
    , theActionMouseOver   :: !(Maybe (EventAction PixelMouse model))
      -- ^ Mouse over event with no mouse buttons depressed
    , theActionRightMouse  :: !(Maybe (RoleMouseEvents model))
      -- ^ Event handlers for the right mouse button
    , theActionLeftMouse   :: !(Maybe (RoleMouseEvents model))
      -- ^ Event handlers for the left mouse button
    , theActionStepAnimate :: !(Maybe (EventAction UTCTime model))
    }

-- | A cluster of event handlers that can respond to either left or right mouse buttons. Each mouse
-- button has it's own FSM state and so can respond to events independently from one another, which
-- is why there needs to be two identical sets of event handlers.
data RoleMouseEvents model
  = RoleMouseEvents
    { theActionMouseDown   :: !(Maybe (EventAction PixelMouse model))
      -- ^ Action triggered on mouse button leading-edge click, that is, as soon as a mouse-down
      -- event occurs, the handler is triggered without waiting for a mouse-up event. An example of
      -- this is in a file browser, clicking down on a file immediately selects the file without
      -- waiting for a mouse-up event.
    , theActionMouseClick  :: !(Maybe (EventAction PixelMouse model))
      -- ^ Action triggered on mouse button trailing-edge click, that is, a complete mouse-down and
      -- mouse-up pair of events. An example of this is a push button which does not trigger it's
      -- event handler until a mouse-down and mouse-up event both occur within the visible button
      -- region.
    , theActionMouseDouble :: !(Maybe (EventAction PixelMouse model))
      -- ^ Left mouse button double-click, triggered on second leading edge click if the mouse is
      -- not moved.
    , theActionMouseDrag   :: !(Maybe (EventAction (Maybe PixelMouse) model))
      -- ^ Left mouse button drag, ends when event is 'Nothing'. Event handler does not need to
      -- remove itself when drag ends.
    }

-- | This is a mouse event type used within a 'Script' and can be transformed according to a
-- 'Script'. It instantiates 'Functor' so that you can transform 'theMouse2DPosition' to a local
-- coordinate system (if need be) before passing it on to some even higher-level event handler.
--
-- A 'Mouse2D' event is different from the system level 'MouseSignal' event data type defined in
-- "Happlets.Control.Mouse". 'MouseSignal' is usually too low-level to be of much use to application
-- developers, it only provides whether the mouse buttons are depressed or not, the position,
-- keyboard modifiers, and device id, while it does not say anything about whether the mouse event
-- is a click, a double-click, a drag, or a hover. This is the problem that 'Script' solves with the
-- 'Mouse2D' data type, it gives you a way to react to these higher-level events without needing to
-- invent your own finite state mahcine (FSM) for counting mouse-up and mouse-down events.
--
-- Said another way, using the terminology of functional reactive programming, the 'MouseSignal'
-- data type in "Happlets.Control.Mouse" is a "signal" whereas the 'Mouse2D' data type is an actual
-- "event".
data Mouse2D n
  = Mouse2D
    { theMouse2DPosition :: !(Point2D n)
    , theMouse2DModifiers :: !ModifierBits
    }
  deriving (Eq, Show, Functor)

-- | Events received directly from the operating system specify the event coordinate in
-- pixels. Events of this type can be transformed to other coordinate systems using the 'fmap'
-- function.
type PixelMouse = Mouse2D SampCoord

-- | This data type is used when installing event handlers to specify which mouse button to respond
-- to when an event occurs. You can install both left and right button event handlers
-- simultaneously, all event handlers will be triggered as events occur. The left and right mouse
-- button each have their own individual FSM state information so left and right event handlers may
-- be triggered simultaneously.
data MouseButton = RightMouseButton | LeftMouseButton
  deriving (Eq, Ord, Show, Enum)

instance Has2DOrigin Mouse2D where { origin2D = mouse2DPosition; }

roleMouseEvents :: RoleMouseEvents model
roleMouseEvents = RoleMouseEvents
  { theActionMouseDown = Nothing
  , theActionMouseClick = Nothing
  , theActionMouseDouble = Nothing
  , theActionMouseDrag = Nothing
  }

roleMouseEventsNull :: RoleMouseEvents model -> Bool
roleMouseEventsNull = \ case
  RoleMouseEvents
   { theActionMouseDown = Nothing
   , theActionMouseClick = Nothing
   , theActionMouseDouble = Nothing
   , theActionMouseDrag = Nothing
   } -> True
  _  -> False

roleMouseEventsMaybe :: RoleMouseEvents model -> Maybe (RoleMouseEvents model)
roleMouseEventsMaybe rme = guard (not $ roleMouseEventsNull rme) >> pure rme

-- | A lens into a 'Role', either 'rightMouseButtonActions' or 'leftMouseButtonActions', that is
-- constructed from a 'MouseButton' value
actionMouseButton :: MouseButton -> Lens' (Role model) (RoleMouseEvents model)
actionMouseButton = \ case
  RightMouseButton -> rightMouseButtonActions
  LeftMouseButton  -> leftMouseButtonActions

-- | This lens is used to instantiate the 'origin2D' instance for the 'MouseEvent' data type.
mouse2DPosition :: Lens' (Mouse2D n) (Point2D n)
mouse2DPosition = lens theMouse2DPosition $ \ a b -> a{ theMouse2DPosition = b }

-- | Keyboard modifiers received from the system-level 'Mouse' event data structure.
mouse2DModifiers :: Lens' (Mouse2D n) ModifierBits
mouse2DModifiers = lens theMouse2DModifiers $ \ a b -> a{ theMouse2DModifiers = b }

-- | not for export
--
-- A function you can use to define a widget using do-notation and lens 'State' operators like '.=',
-- '%=', and 'use'
role :: model -> Role model
role model = Role
  { theRoleModel         = model
  , theRoleLabel         = ""
  , theActionDraw        = mempty
  , theActionRedraw      = Nothing
  , theActionFocus       = Nothing
  , theActionKeyPress    = Nothing
  , theActionMouseOver   = Nothing
  , theActionLeftMouse   = Nothing
  , theActionRightMouse  = Nothing
  , theActionStepAnimate = Nothing
  }

-- | A lens to inspect and update the model inside of the 'Role'.
roleModel :: Lens' (Role model) model
roleModel = lens theRoleModel $ \ a b -> a{ theRoleModel = b }

-- | An arbitrary string label you can use to query 'Roles' in a set. Labels do not need to be
-- unique.
roleLabel :: Lens' (Role model) Strict.Text
roleLabel = lens theRoleLabel $ \ a b -> a{ theRoleLabel = b }

actionDraw :: Lens' (Role model) (Drawing SampCoord)
actionDraw = lens theActionDraw $ \ a b -> a{ theActionDraw = b }

actionRedraw :: Lens' (Role model) (Maybe (Drawing SampCoord))
actionRedraw = lens theActionRedraw $ \ a b -> a{ theActionRedraw = b }

actionFocus :: Lens' (Role model) (Maybe (EventAction Bool model))
actionFocus = lens theActionFocus $ \ a b -> a{ theActionFocus = b }

actionKeyPress :: Lens' (Role model) (Maybe (EventAction Keyboard model))
actionKeyPress = lens theActionKeyPress $ \ a b -> a{ theActionKeyPress = b }

-- | Set the action to perform when an animation step occurs.
actionStepAnimate :: Lens' (Role model) (Maybe (EventAction UTCTime model))
actionStepAnimate = lens theActionStepAnimate $ \ a b -> a{ theActionStepAnimate = b }

actionMouseOver :: Lens' (Role model) (Maybe (EventAction PixelMouse model))
actionMouseOver = lens theActionMouseOver $ \ a b -> a{ theActionMouseOver = b }

rightMouseButtonActions :: Lens' (Role model) (RoleMouseEvents model)
rightMouseButtonActions = lens
  (maybe roleMouseEvents id . theActionRightMouse)
  (\ a b -> a{ theActionRightMouse = roleMouseEventsMaybe b })

leftMouseButtonActions :: Lens' (Role model) (RoleMouseEvents model)
leftMouseButtonActions = lens
  (maybe roleMouseEvents id . theActionLeftMouse)
  (\ a b -> a{ theActionLeftMouse = roleMouseEventsMaybe b })

mouseButtonDown :: Lens' (RoleMouseEvents model) (Maybe (EventAction PixelMouse model))
mouseButtonDown = lens theActionMouseDown $ \ a b -> a{ theActionMouseDown = b }

mouseButtonClick :: Lens' (RoleMouseEvents model) (Maybe (EventAction PixelMouse model))
mouseButtonClick = lens theActionMouseClick $ \ a b -> a{ theActionMouseClick = b }

mouseButtonDouble :: Lens' (RoleMouseEvents model) (Maybe (EventAction PixelMouse model))
mouseButtonDouble = lens theActionMouseDouble $ \ a b -> a{ theActionMouseDouble = b }

mouseButtonDrag :: Lens' (RoleMouseEvents model) (Maybe (EventAction (Maybe PixelMouse) model))
mouseButtonDrag = lens theActionMouseDrag $ \ a b -> a{ theActionMouseDrag = b }

actionMouseDown :: MouseButton -> Lens' (Role model) (Maybe (EventAction PixelMouse model))
actionMouseDown b = cloneLens (actionMouseButton b) . mouseButtonDown

actionMouseClick :: MouseButton -> Lens' (Role model) (Maybe (EventAction PixelMouse model))
actionMouseClick b = cloneLens (actionMouseButton b) . mouseButtonClick

actionMouseDouble :: MouseButton -> Lens' (Role model) (Maybe (EventAction PixelMouse model))
actionMouseDouble b = cloneLens (actionMouseButton b) . mouseButtonDouble

actionMouseDrag :: MouseButton -> Lens' (Role model) (Maybe (EventAction (Maybe PixelMouse) model))
actionMouseDrag b = cloneLens (actionMouseButton b) . mouseButtonDrag

----------------------------------------------------------------------------------------------------

data EventAction event model
  = EventAction
    { theActionText :: !Strict.Text
      -- ^ The source code that defined this action, if any.
    , theAction :: !(event -> Script model ())
    }

-- | Create a closure for a 'Script' function that updates a value of type @private@ in reaction to
-- some @event@. This function implements the actual logic for how a closure updates it's private
-- data of type @private@ using the public data of type @public@. Except for 'encloseDrawAction',
-- all other "staging" functions call into this function. Although 'encloseDrawAction' has nearly
-- identical logic to 'encloseEventScript' it is a reader monad rather than a state monad, so must
-- be treated slightly differently.
--
-- Note that this function calls 'runScript' which may in turn call 'encloseRole', and 'encloseRole'
-- will in turn call 'ancloseEventAction', and 'encloseEventAction' calls this function
-- again. However, there is no infinite loop, because the 'encloseEventAction' function only returns
-- a thunk containing a call to this 'encloseEventScript' function, so it does not immediately
-- evaluate 'encloseEventScript' in an infinite loop.
encloseEventScript
  :: Actor model
  -> (event -> Script model a)
  -> (event -> Script Presence a)
encloseEventScript ref act event =
  Script $ ConsequenceT $ StateT $ \ st0 -> do
    (result, st) <- runScript (act event) (st0 ^. scriptFrame) (theScriptLogger st0) ref
    let actor@(Presence actorRef) = thePresenceActor (theScriptActor st0)
    role <- readIORef actorRef
    return
      ( result
      , ScriptState
        { theScriptRole  = role
        , theScriptActor = Actor
          { theActorRole = actorRef
          , thePresenceActor = actor
          }
        , theScriptFrame = theScriptFrame st
        , theScriptLogger = theScriptLogger st
        , theScriptRoleUpdated = theScriptRoleUpdated st
        }
      )

-- | Evaluates an 'EventAction' function.
runEventAction :: EventAction event model -> event -> Script model ()
runEventAction (EventAction{theAction=act}) = act

-- | Create a closure around an 'EventAction' function, hiding the data of type @private@. This
-- function calls 'encloseEventScript' to create the closure.
encloseEventAction
  :: Actor model
  -> EventAction event model
  -> EventAction event Presence
encloseEventAction ref (EventAction{theActionText=txt,theAction=act}) = EventAction
  { theActionText = txt
  , theAction     = encloseEventScript ref act
  }

----------------------------------------------------------------------------------------------------

-- | A 'Presence' is the untyped variant of an 'Actor', and is associated with an 'Actor'. A
-- 'Presence' can be used to group 'Actor's together into a list or some similar type in which all
-- elements are of the same type.
--
-- A 'Presence' is actually a bit like a body-double for an 'Actor', or you could think of it as a
-- puppeteer. The 'Presence' has it's own set of event handlers, but these event handlers are an
-- identical copy of the event handlers of the associated 'Actor', the 'Presence' event handlers
-- immediately transmit events to the associated 'Actor', and any changes to the 'Actor' are
-- immediately reflected back into the 'Presenece'. So a 'Presence' isn't precisely like a puppeteer
-- because every change made to the 'Actor' makes an identical change in the 'Presence'. Also,
-- 'Actor's can be placed into an 'Scene' without needing to have a 'Presence' associated with it, so
-- in these cases there is no "puppetry", and there are no "body-doubles" involved. Thus the more
-- abstract term 'Presence' is apt because it describes the intrinsic relationship between an
-- 'Actor' and it's 'Presence'.
--
-- The benefit of using a 'Presence', as opposed to a 'Actor' is that a 'Presence' is a simple
-- concrete type (it has no type variable indicating it's internal state), so you can store
-- 'Presence's that contain many different 'TypedActors' of many different @model@ types in a single
-- 'Data.Traversable.Traversable' data structure like a list or 'Registry', as though that data
-- structure contained heterogeneous data. The disadvantage of using a 'Presence' is that there no
-- way to directly manipulate the @model@ private data except indirectly via the event handlers.
newtype Presence = Presence (IORef (Role Presence))

-- | This is a typed variant of the 'Presence' data type in which the @model@ data type is attached,
-- allowing you to define 'Script's that manipulate the @model@ directly. In object-oriented
-- programming temrs, the @model@ is the "private data" for the 'Presence' object.
--
-- The 'Actor' contains a reference to the untyped 'Presence' which you can retrieve using
-- 'thePresenceActor'. Any changes made to the 'Actor' are immediately reflected on the untyped
-- 'Presence'. Any code that maintains a reference to the 'Actor' can make updates to the 'Presence's
-- hidden private data @model@ for long as that reference is in scope.
--
-- To create a 'Actor' and place it on the Happlets canvas, use the 'actor' function.
data Actor model
  = Actor
    { theActorRole :: !(IORef (Role model))
    , thePresenceActor   :: !Presence
      -- ^ The 'Presence' associated with the 'Actor'. The 'Actor' automatically updates the
      -- 'Presence' whenever an update to itself is made.
    }

actorPresence :: Presence -> Actor Presence
actorPresence self@(Presence ref) = Actor
  { theActorRole = ref
  , thePresenceActor = self
  }

-- | This function is used to report information about the label for a 'Presence' which was set by
-- 'modifyLabel'. This is useful when you have access only to a 'Presence' and not the original
-- actor, and you want some way to visualize which 'Actor' is assocated with the 'Presence'.
getPresenceLabel :: Presence -> Script model Strict.Text
getPresenceLabel (Presence ref) = unsafeScriptIO $ view roleLabel <$> readIORef ref

-- | Returns the current 'Drawing' for the current 'Actor'.
actorDraw :: Script model (Drawing SampCoord)
actorDraw = scriptGetsRole $ view actionDraw

-- | Returns the current 'actionDraw' and 'actionRedraw' value, and then if the 'actionRedraw' is
-- not 'Nothing', it sets the 'actionDraw' value to the value of 'actionRedraw' then clears
-- 'actionRedraw' to 'Nothing'. This is necessary for an update, for example, during animation or
-- mouse dragging.
actorRedraw :: Script model (Drawing SampCoord, Maybe (Drawing SampCoord))
actorRedraw = do
  drawings@(_, redraw) <- scriptGetsRole (view actionDraw &&& view actionRedraw)
  case redraw of
    Nothing -> pure ()
    Just redraw -> scriptModify $
      scriptRole %~
      (actionDraw .~ redraw) .
      (actionRedraw .~ Nothing)
  pure drawings

-- | Delegate or send a new 'Keyboard' event to the current 'Actor' of the 'Script' function
-- context.
actorFocus :: Bool -> Script model ()
actorFocus got =
  scriptGetsRole theActionFocus >>= maybe (pure ()) (flip runEventAction got)

-- | Delegate or send a new 'Keyboard' event to the current 'Actor' of the 'Script' function
-- context.
actorKeyPress :: Keyboard -> Script model ()
actorKeyPress key =
  scriptGetsRole theActionKeyPress >>= maybe (pure ()) (flip runEventAction key)

-- | Delegate or send a new 'PixelMouse' mouse-over event to the current 'Actor' of the
-- 'Script' function context.
actorMouseOver :: PixelMouse -> Script model ()
actorMouseOver mouse =
  scriptGetsRole theActionMouseOver >>= maybe (pure ()) (flip runEventAction mouse)

-- | Delegate or send a new mouse down event to the current 'Actor' of the 'Script' function
-- context.
actorMouseDown :: MouseButton -> PixelMouse -> Script model ()
actorMouseDown button mouse =
  scriptGetsRole (view $ actionMouseDown button) >>= maybe (pure ()) (flip runEventAction mouse)

-- | Delegate or send a new mouse click event to the current 'Actor' of the 'Script' function
-- context.
actorMouseClick :: MouseButton -> PixelMouse -> Script model ()
actorMouseClick button mouse =
  scriptGetsRole (view $ actionMouseClick button) >>= maybe (pure ()) (flip runEventAction mouse)

-- | Delegate or send a new 'MouseSignal' double click event to the current 'Actor' of the
-- 'Script' function context.
actorMouseDoubleClick :: MouseButton -> PixelMouse -> Script model ()
actorMouseDoubleClick button mouse =
  scriptGetsRole (view $ actionMouseDouble button) >>= maybe (pure ()) (flip runEventAction mouse)

-- | Delegate or send a new 'Mouse' double click event to the current 'Actor' of the 'Script'
-- function context.
actorMouseDrag :: MouseButton -> Maybe PixelMouse -> Script model ()
actorMouseDrag button mouse =
  scriptGetsRole (view $ actionMouseDrag button) >>= maybe (pure ()) (flip runEventAction mouse)

-- | Delegate or send a new animation event to the current 'Actor' of the 'Script' function
-- context.
actorStepAnimate :: UTCTime -> Script model ()
actorStepAnimate t =
  scriptGetsRole theActionStepAnimate >>= maybe (pure ()) (flip runEventAction t)

-- | Calls all relevant "enclose" functions to create a closure around an entire 'Role' by creating
-- a closure around an every 'EventAction' or 'DrawAction' function stored within the 'Role'.
encloseRole
  :: Actor model
  -- ^ the reference that will store the private data of the closure.
  -> Role model
  -- ^ the functions that operate on the private data of the closure.
  -> Role Presence
encloseRole ref pack = Role
  { theRoleModel         = thePresenceActor ref
  , theRoleLabel         = theRoleLabel pack
  , theActionDraw        = theActionDraw pack
  , theActionRedraw      = theActionRedraw pack
  , theActionFocus       = encloseEventAction ref <$> theActionFocus    pack
  , theActionKeyPress    = encloseEventAction ref <$> theActionKeyPress  pack
  , theActionMouseOver   = encloseEventAction ref <$> theActionMouseOver pack
  , theActionRightMouse  = encloseMouseEvents ref $ theActionRightMouse  pack
  , theActionLeftMouse   = encloseMouseEvents ref $ theActionLeftMouse   pack
  , theActionStepAnimate = encloseEventAction ref <$> theActionStepAnimate pack
  }

-- | Calls all relevant "enclose" functions to create a closure around an entire 'RoleMouseEvents'
-- data structure, just like 'encloseRole' but for 'RoleMouseEvents'.
encloseMouseEvents
  :: Actor model
  -> Maybe (RoleMouseEvents model)
  -> Maybe (RoleMouseEvents Presence)
encloseMouseEvents ref = maybe Nothing $ \ pack -> Just $
  RoleMouseEvents
  { theActionMouseDown   = encloseEventAction ref <$> theActionMouseDown pack
  , theActionMouseClick  = encloseEventAction ref <$> theActionMouseClick pack
  , theActionMouseDouble = encloseEventAction ref <$> theActionMouseDouble pack
  , theActionMouseDrag   = encloseEventAction ref <$> theActionMouseDrag pack
  }

-- | This function takes the label text set by 'modifyLabel', passes it to a continuation that lets
-- you append (or otherwise change) some reporting information to the label, and then this function
-- reports the string you return as an 'INFO'-level report. This function is commonly used for
-- debugging since it is a bit more concise than writing:
--
-- @'getSelfLabel' 'Control.Monad.>>=' 'report' 'INFO' ("Object label: " 'Data.Semigroup.<>')@
reportSelfLabel :: (Strict.Text -> Strict.Text) -> Script model ()
reportSelfLabel msg = getSelfLabel >>= report INFO . msg

-- | Print a debug message before and after evaluating a 'Script' function.
reportSubScript :: Strict.Text -> Script model a -> Script model a
reportSubScript msg f = do
  report OBJECT ("begin " <> msg)
  result <- f
  report OBJECT ("end " <> msg)
  return result

-- | not for export
--
-- This function creates a 'Actor' without registering it with a 'Registry'.
makeActorIO :: Role model -> IO (Actor model)
makeActorIO role = do
  roleref <- newIORef role
  actref  <- newIORef $ error "'newActor' failed to initialize reference"
  let actor = Presence actref
  let typed = Actor
        { theActorRole = roleref
        , thePresenceActor = actor
        }
  writeIORef actref $ encloseRole typed role
  return typed

-- | not for export
--
-- Creates a new 'Actor' and initializes it.
makeActor :: Script model a -> model -> Script any (a, Actor model)
makeActor init model =
  Script $ ConsequenceT $ StateT $ \ st0 -> do
    typed <- makeActorIO $ role model
    (result, st) <- runScript
      (reportSubScript "makeActor" $ init <* reportSelfLabel ("Initialized actor:" <>))
      (st0 ^. scriptFrame)
      (theScriptLogger st0)
      typed
    return
      ( (\ a -> (a, typed)) <$> result
      , st0
        { theScriptFrame = theScriptFrame st
        , theScriptRoleUpdated = theScriptRoleUpdated st
        }
      )

-- | Create a new 'Actor'.
actor :: Script model () -> model -> Script any (Actor model)
actor init model = snd <$> makeActor init model

-- | Same as the 'actor' function, but for 'actress' the @model@ is constrained to instantiate the
-- 'Monoid' typeclass. So 'actress' does not take an initial @model@ value as the 'Actor' will
-- be initialized with the default 'mempty' value.
--
-- The word 'actress' is used here only because it is a shorter name than @monoidActor@, and apart
-- from the 'Monoid' type constraint, there is otherwise no difference between an 'actor' and
-- 'actress'.
actress :: Monoid model => Script model () -> Script any (Actor model)
actress = flip actor mempty

----------------------------------------------------------------------------------------------------

-- | This is the root object of the 'Happlet'. It contains an 'Actor' (who you can think of as an
-- actor who also directs the 'Scene') and delegates events to other 'Actor's.
data Scene stage
  = Scene
    { theActCurrentActor :: !(Actor stage)
      -- ^ The root actor in the hierarchy that is immediately responsible for receiving events from
      -- the 'Scene' event handler.
    , theActVisibleFrame :: !(Rect2D SampCoord)
    , theLeftMouseState  :: !MouseState
    , theRightMouseState :: !MouseState
    }

sceneMouseButtonLens :: MouseButton -> Lens' (Scene stage) MouseState
sceneMouseButtonLens = \ case
  RightMouseButton -> sceneRightMouseState
  LeftMouseButton  -> sceneLeftMouseState

sceneVisibleFrame :: Lens' (Scene stage) (Rect2D SampCoord)
sceneVisibleFrame = lens theActVisibleFrame $ \ a b -> a{ theActVisibleFrame = b }

sceneLeftMouseState :: Lens' (Scene stage) MouseState
sceneLeftMouseState = lens theLeftMouseState $ \ a b -> a{ theLeftMouseState = b }

sceneRightMouseState :: Lens' (Scene stage) MouseState
sceneRightMouseState = lens theRightMouseState $ \ a b -> a{ theRightMouseState = b }

sceneCurrentActor :: Lens' (Scene stage) (Actor stage)
sceneCurrentActor = lens theActCurrentActor $ \ a b -> a{ theActCurrentActor = b }

sceneRoleGets :: ProvidesLogReporter provider => (Role stage -> a) -> GUI provider (Scene stage) a
sceneRoleGets get = guiRunScript (scriptGetsRole get) >>= throwConsequence

-- | Execute a 'Script' on a different 'Actor' than the current 'Actor' in the 'Scene'. The current
-- 'Actor' is not replaced. To change the current 'Actor', simply use the 'put' function. The
-- 'Consequence' of running the 'Script' is caught and returned, but you can use 'throwConsequence'
-- to make the 'GUI' function succeed or fail with the same 'Consequence'.
--
-- See also: 'scriptWithPresence', 'scriptWithActor', 'guiRunScript'
guiRunScriptWith
  :: ProvidesLogReporter provider
  => Script model a
  -> Actor model
  -> GUI provider (Scene stage) (Consequence a)
guiRunScriptWith f actor = do
  logger <- guiLogReportWriter
  frame <- use sceneVisibleFrame
  liftIO $ fst <$> runScript f frame logger actor

-- | Run a 'Script' with the current 'Actor' in the current 'Scene'.
--
-- See also: 'guiRunScriptWith', 'scriptWithActor', 'scriptWithPresence'
guiRunScript
  :: ProvidesLogReporter provider
  => Script model a
  -> GUI provider (Scene model) (Consequence a)
guiRunScript f = use sceneCurrentActor >>= guiRunScriptWith f

-- | Like 'liftIO' but only works in a the 'GUI' monad for an 'Scene' data structure.
sceneLiftIO :: (Scene stage -> IO a) -> GUI provider (Scene stage) a
sceneLiftIO = (get >>=) . (liftIO .)

-- | Use this function to initialize a new 'Scene'. This function is of type 'Initialize' so it can
-- only be used at initialization time. The initializing @'Script' 'stage' ()@ function is the first
-- 'Script' that executes which places 'Actor's onto the @stage@. You must provide an initial
-- @stage@ value, and executing the initialzing 'Script' sets the event handlers that will prompt
-- execution of various other 'Script's delegated to other 'Actor's.
newSceneHapplet
  :: ProvidesLogReporter provider
  => stage
  -> Script stage ()
  -> Initialize provider (Happlet (Scene stage))
newSceneHapplet stage init =
  gets theActualLogReporter >>= \ logger ->
  newHappletIO $
  liftIO (makeActorIO $ role stage) >>= \ actor ->
  runScript init rect2D logger actor >>= \ case
    (ActionFail err, _ ) -> error $ "app initializer failed: " ++ show err
    (_             , st) -> do
      now <- getCurrentTime
      let stats = roleEventStats $ theScriptRole st
      hPutStrLn stderr $ "newActHapplet:\n" <> show stats
      pure $ Scene
        { theActCurrentActor = actor
        , theRightMouseState = mouseState now
        , theLeftMouseState  = mouseState now
        , theActVisibleFrame = rect2D
        }

-- | Use this function with the 'attachWindow' function and the result of 'newActHapplet' to create
-- the OS window.
sceneWindow
  :: ( CanMouse provider, CanKeyboard provider, CanAnimate provider, Managed provider
     , HappletWindow provider render, Happlet2DGraphics render, ProvidesLogReporter provider
     )
  => PixSize -> GUI provider (Scene stage) ()
sceneWindow initSize = do
  sceneVisibleFrame %=
    (rect2DHead .~ initSize) .
    (rect2DTail .~ point2D)
  guiForceRedraw
  sceneDelegateEventHandlers

----------------------------------------------------------------------------------------------------
-- Event handler delegate resets

sceneDelegateKeyboard
  :: (CanKeyboard provider, ProvidesLogReporter provider)
  => GUI provider (Scene stage) ()
sceneDelegateKeyboard = do
  role <- sceneRoleGets id
  let keyHandler = role ^. actionKeyPress
  case keyHandler of
    Nothing -> pure ()
    Just{}  -> report OBJECT $ "-- ENABLE keyboard events for: " <> (role ^. roleLabel)
  keyboardEvents $ case keyHandler of
    Nothing -> const cancel
    Just keyHandler -> \ event ->
      if keyIsPressed event then
        guiRunScript (runEventAction keyHandler event) >>=
        throwConsequence
      else
        report SIGNAL $
        "-- IGNORE keyboard signal: " <> (Strict.pack $ show event)

sceneDelegateMouse
  :: ( CanAnimate provider, CanMouse provider, Managed provider
     , HappletWindow provider render, Happlet2DGraphics render
     , ProvidesLogReporter provider
     )
  => GUI provider (Scene stage) ()
sceneDelegateMouse =
  sceneRoleGets id >>= \ role ->
  let stats = roleEventStats role in
  let remark filter =
        report OBJECT $
        "-- ENABLE " <> (Strict.pack $ show filter) <>
        " events for: " <> (role ^. roleLabel)
  in
  let has f =
        maybe 0 f (countActionMouseRight stats) > 0 ||
        maybe 0 f (countActionMouseLeft  stats) > 0
  in
  let react which = mouseSignals which sceneMouseHandler in
  if countActionMouseOver stats > 0
  then remark MouseAll >> react MouseAll
  else if has countActionMouseDrag
  then remark MouseDrag >> react MouseDrag
  else if has countActionMouseDown || has countActionMouseClick || has countActionMouseDouble
  then remark MouseButton >> react MouseButton
  else mouseSignals MouseAll (const cancel)

sceneDelegateStepAnimate
  :: ( CanAnimate provider, HappletWindow provider render
     , Managed provider, Happlet2DGraphics render
     , ProvidesLogReporter provider
     )
  => GUI provider (Scene stage) ()
sceneDelegateStepAnimate = do
  role <- sceneRoleGets id
  let animHandler = role ^. actionStepAnimate
  case animHandler of
    Nothing -> pure ()
    Just{}  -> report OBJECT $ "-- ENABLE animation events for: " <> (role ^. roleLabel)
  stepFrameEvents $ case animHandler of
    Nothing -> const cancel
    Just animHandler -> \ event ->
      guiRunScript (runEventAction animHandler event) >>=
      throwConsequence

sceneDelegateEventHandlers
  :: ( CanMouse provider, CanKeyboard provider, CanAnimate provider, Managed provider
     , HappletWindow provider render, Happlet2DGraphics render
     , ProvidesLogReporter provider
     )
  => GUI provider (Scene stage) ()
sceneDelegateEventHandlers = do
  frame <- onCanvas getViewRect
  sceneVisibleFrame .= frame
  sceneDelegateMouse
  sceneDelegateKeyboard
  sceneDelegateStepAnimate

----------------------------------------------------------------------------------------------------
-- Event handler triggers

-- | This function clears the window and redraws everything using the 'actionRedraw', or if there is
-- no drawing for 'actionRedraw' then 'actionDraw' is used. The 'actorRedraw' function is
-- evaluated.
guiForceRedraw
  :: (HappletWindow provider render, Happlet2DGraphics render, ProvidesLogReporter provider)
  => GUI provider (Scene stage) ()
guiForceRedraw = do
  let bgcolor = black & alphaChannel .~ 0.9 -- TODO: make background color configurable
  frame <- use sceneVisibleFrame
  (draw, redraw) <- guiRunScript actorRedraw >>= throwConsequence
  report EVENT $ "guiForceRedraw: " <> Strict.pack (show (draw, redraw))
  onCanvas $ do
    clearRegions (rect2DUnionSingle frame) bgcolor
    draw2D frame $ maybe draw id redraw

-- | This function is used to perform an animation step. It clears the current region using the
-- bounding box of 'actionDraw', and then draws using 'actionRedraw'. The 'actionDraw' is then set
-- to the value of 'actionRedraw' and the value of 'actionRedraw' is set to 'Nothing'.
guiRedraw
  :: (HappletWindow provider render, Happlet2DGraphics render, ProvidesLogReporter provider)
  => GUI provider (Scene stage) ()
guiRedraw = do
  let bgcolor = black & alphaChannel .~ 0.9 -- TODO: make background color configurable
  (draw, redraw) <- guiRunScript actorRedraw >>= throwConsequence
  label <- sceneRoleGets (view roleLabel)
  case redraw of
    Nothing     -> report EVENT $ "-- IGNORE guiRedraw on " <> label
    Just redraw -> do
      report EVENT $ "guiRedraw on " <> label <> " " <> (Strict.pack $ show redraw)
      frame <- use sceneVisibleFrame
      onCanvas $ do
        flip clearRegions bgcolor $
          theRect2DUnionMask draw <>
          theRect2DUnionMask redraw
        draw2D frame draw

-- | Triggers other event handlers
sceneEventAction
  :: (Show event, ProvidesLogReporter provider,
      HappletWindow provider render, Happlet2DGraphics render
     )
  => ReportLevel -- ^ Animation and mouse drag events are at a whole other level
  -> Strict.Text -- ^ Used in reports to indicate the kind of event this is handling
  -> Lens' (Role model) (Maybe (EventAction event model))
  -> event
  -> GUI provider (Scene model) ()
sceneEventAction level actmsg handlerLens event = do
  role <- sceneRoleGets id
  report level $ "-- EVENT " <> actmsg <> " with data: " <> Strict.pack (show event)
  let runAction handler = do
        let txt = theActionText handler
        report level $
          ("-- ACTION on " <> (role ^. roleLabel)) &
          if Strict.null txt then id else ((" triggers:\n" <> txt) <>)
        theAction handler event
  let doCancel =
        guiRunScript (scriptModify $ scriptRole . handlerLens .~ Nothing) >>=
        throwConsequence
  result <- guiRunScript $
    scriptGetsRole (view handlerLens) >>=
    maybe cancel runAction
  guiRedraw
  report EVENT "-- AFTER REDRAW"
  case result of
    ActionCancel -> do
      doCancel
      report OBJECT $
        "-- DISABLE " <> actmsg <> " events for: " <> (role ^. roleLabel)
    ActionFail err -> do
      doCancel
      report ERROR $
        "-- FAILURE on " <> actmsg <>
        "\n--   in event handler for:\n-- " <> (role ^. roleLabel) <>
        "\n--   Reason given: " <> err
    _ -> return ()  

-- | Force an 'onFocus' event. Usually this happens when a Happlets app window which was not active
-- is activated by a mouse down occurring in the window.
guiFocus
  :: (ProvidesLogReporter provider, HappletWindow provider render, Happlet2DGraphics render)
  => Bool -> GUI provider (Scene model) ()
guiFocus = sceneEventAction EVENT "onFocus" actionFocus

-- | Force an 'onKeyPress' event on the 'Actor' in the current 'Scene'.
guiKeyPress
  :: (ProvidesLogReporter provider, HappletWindow provider render, Happlet2DGraphics render)
  => Keyboard -> GUI provider (Scene model) ()
guiKeyPress = sceneEventAction EVENT "onKeyPress" actionKeyPress

-- | Force an 'onMouseOver' event on the 'Actor' in the current 'Scene'.
guiMouseOver
  :: (ProvidesLogReporter provider, HappletWindow provider render, Happlet2DGraphics render)
  => PixelMouse -> GUI provider (Scene model) ()
guiMouseOver = sceneEventAction EVENT "onMouseOver" actionMouseOver

-- | Force an 'onMouseDown' event on the 'Actor' in the current 'Scene'.
guiMouseDown
  :: (ProvidesLogReporter provider, HappletWindow provider render, Happlet2DGraphics render)
  => MouseButton -> PixelMouse -> GUI provider (Scene model) ()
guiMouseDown button =
  sceneEventAction EVENT
  ("onMouseDown " <> Strict.pack (show button))
  (actionMouseDown button)

-- | Force an 'onMouseClick' event on the 'Actor' in the current 'Scene'.
guiMouseClick
  :: (ProvidesLogReporter provider, HappletWindow provider render, Happlet2DGraphics render)
  => MouseButton -> PixelMouse -> GUI provider (Scene model) ()
guiMouseClick button =
  sceneEventAction EVENT
  ("onMouseClick " <> Strict.pack (show button))
  (actionMouseClick button)

-- | Force an 'onMouseDoubleClick' event on the 'Actor' in the current 'Scene'.
guiMouseDoubleClick
  :: (ProvidesLogReporter provider, HappletWindow provider render, Happlet2DGraphics render)
  => MouseButton -> PixelMouse -> GUI provider (Scene model) ()
guiMouseDoubleClick button =
  sceneEventAction EVENT
  ("onMouseDoubleClick " <> Strict.pack (show button))
  (actionMouseDouble button)

-- | Force an 'onMouseDrag' event on the 'Actor' in the current 'Scene'.
guiMouseDrag
  :: (ProvidesLogReporter provider, HappletWindow provider render, Happlet2DGraphics render)
  => MouseButton -> Maybe PixelMouse -> GUI provider (Scene model) ()
guiMouseDrag button =
  sceneEventAction DEBUG_ALL
  ("onMouseDrag " <> Strict.pack (show button))
  (actionMouseDrag button)

-- | Force an 'onAnimation' event on the 'Actor' in the current 'Scene'.
guiStepAnimate
  :: (ProvidesLogReporter provider, HappletWindow provider render, Happlet2DGraphics render)
  => UTCTime -> GUI provider (Scene model) ()
guiStepAnimate = sceneEventAction DEBUG_ALL "onStepAnimate" actionStepAnimate

----------------------------------------------------------------------------------------------------
-- MouseSignal FSA for triggering events

-- | There are two 'MouseState' values in an act, one for the left and right mouse buttons, so each
-- mouse button has it's own state value. This data structure along with the 'MouseFSA' determines
-- how the low-level 'MouseSignal' stream is translated to mouse events.
data MouseState
  = MouseState
    { theMouseStateTime       :: !UTCTime
      -- ^ Used for debouncing when animation events are not set.
    , theMouseStateSignal     :: !(Maybe MouseSignal)
      -- ^ Used for timing mouse reactions with animations.
    , theMouseStateCoordinate :: !(Maybe (PixCoord, UTCTime))
      -- ^ If a mouse down event has occurred, the pixel coordinate associated with that signal is
      -- registered here so that a drag distance can be computed.
    , theMouseStateFSA        :: !MouseFSA
      -- ^ Used to decide which 'MouseSignals' to accept, and which to ignore. Also used to
      -- determine when an actual 'PixelMouse' event triggers an event handler.
    }
  deriving Eq

-- | The 'MouseFSA' is the state of a finite state automata (FSA) that describes the mouse event
-- state, used to translate the 'MouseSignal' stream into higher-level events that trigger event
-- handlers like 'onMouseDrag' or 'onMouseDoubleClick'. Each mouse button (only right and
-- left, we do not assume a middle mouse button) has it's own FSA, so click, drag, and double-click
-- events are parameterized as either a right or left variety.
--
-- The FSA itself determines what next 'MouseSignal' events can transition to the next FSA state,
-- and if a 'MouseSignal' that is not expected by the FSA is received, this signal is ignored. The
-- result of this is that you may have a noisy 'MouseSignal' stream, but the behavior of the mouse
-- will remain somewhat predictable as event handlers are only fired when some recognizable sequence
-- of events occurs, regardless of how many intervening noisy signals may occur in between state
-- transitions. The FSA tends toward triggering the correct event handlers.
data MouseFSA
  = MouseOverState -- ^ The default state.
  | MouseDownState -- ^ A "down" occurs after a mouse-down but before a mouse-up.
  | MouseClickState
    -- ^ A "click" occurs after a single mouse-down and a single mouse-up. In this state, if another
    -- mouse-down" occurs within the mouse drag threshold, a "double-click" event is triggered and
    -- the FSA is reset to 'MouseOverState'.
  | MouseDragState
    -- ^ A "drag" occurs after a "down" occurs, and then another "down" occurs but moved some
    -- distance greater than a mouse drag threshold value. Mouse "drag" events continue to be
    -- generated for every 'MouseSignal' until a "mouse-up" signal is received.
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

-- | The empty 'MouseState'
mouseState :: UTCTime -> MouseState
mouseState t0 =
  MouseState
  { theMouseStateTime       = t0
  , theMouseStateSignal     = Nothing
  , theMouseStateCoordinate = Nothing
  , theMouseStateFSA        = MouseOverState
  }

mouseStateTime :: Lens' MouseState UTCTime
mouseStateTime = lens theMouseStateTime $ \ a b -> a{ theMouseStateTime = b }

mouseStateSignal :: Lens' MouseState (Maybe MouseSignal)
mouseStateSignal = lens theMouseStateSignal $ \ a b -> a{ theMouseStateSignal = b }

mouseStateCoordinate :: Lens' MouseState (Maybe (PixCoord, UTCTime))
mouseStateCoordinate = lens theMouseStateCoordinate $ \ a b -> a{ theMouseStateCoordinate = b }

mouseStateFSA :: Lens' MouseState MouseFSA
mouseStateFSA = lens theMouseStateFSA $ \ a b -> a{ theMouseStateFSA = b }

-- | The 'sceneMouseHandler' has some pretty complex logic in order to make sure the mouse user
-- experience is very consistent across all apps built with the "Happlets.Presence" API.
--
-- Left clicks are de-bounced and timed so that single-click and double-click events can be decoded
-- from the click stream and translated into calls to 'onMouseClick' and 'onMouseDoubleClick'.
--
-- All other mouse actions are checked whether they are button-down or button-up events, and if they
-- are "button-down" (leading edge triggered), the mouse event is sent as-is to the 'onMouseClick'
-- function.
sceneMouseHandler
  :: ( CanAnimate provider, Managed provider
     , HappletWindow provider render, Happlet2DGraphics render
     , ProvidesLogReporter provider
     )
  => MouseSignal -> GUI provider (Scene stage) ()
sceneMouseHandler new@(MouseSignal _ pressed mods signalButton coord) = case signalButton of
  MotionOnly | not pressed -> guiMouseOver $
    Mouse2D
    { theMouse2DPosition = coord
    , theMouse2DModifiers = mods
    }
  LeftClick  -> debounce sceneLeftMouseState
  RightClick -> debounce sceneRightMouseState
  _ -> report SIGNAL $ Strict.pack $ "ignored mouse signal (" <> show new <> ")"
  where
    debounce buttonLens = do
      -- TODO: certain constant values have been hard-coded into this function, like the
      -- double-click distance, and the minimum time required to elapse between events for the event
      -- to not be considered a "bounce". These hard-coded values should be taken from a global
      -- configuration variable.
      let minDelayBetweenEvents = 1/16
      present <- liftIO getCurrentTime
      past    <- use (cloneLens buttonLens . mouseStateTime)
      old     <- use (cloneLens buttonLens . mouseStateSignal)
      if maybe False (similarMouseSignals new) old &&
         diffUTCTime present past < minDelayBetweenEvents
        then do
          -- If the time difference between button-down and button-up signals are too small, the
          -- event is ignored, the latest event is recorded, overwriting the previously recorded
          -- event.
          report SIGNAL $ Strict.pack $ "sceneMouseHandler: debounce (" <> show new <> ")"
          cloneLens buttonLens . mouseStateSignal .= Just new
        else do
          -- Here we set the mouse signal to be processed and pass control over to
          -- 'sceneActualMouseHandler', unless there is an animation handler, in which case we do
          -- nothing and let the animation handler call 'sceneActualMouseHandler'.
          cloneLens buttonLens . mouseStateSignal .= Just new
          cloneLens buttonLens . mouseStateTime   .= present
          stats <- sceneGetActorStats
          if countActionStepAnimate stats <= 0 then sceneActualMouseHandler else
            report SIGNAL $ Strict.pack $ "sceneMouseHandler: defer event " <> show new

-- | This function implements the interpretation of the 'MouseFSA', so it sets 'theMouseStateFSA'
-- and will transition the state when expected 'MouseSignal's are received. Unexpected
-- 'MouseSignal's are simply ignored.
sceneActualMouseHandler
  :: ( HappletWindow provider render, Managed provider, Happlet2DGraphics render
     , ProvidesLogReporter provider
     )
  => GUI provider (Scene stage) ()
sceneActualMouseHandler = stepFSA LeftMouseButton >> stepFSA RightMouseButton where
  stepFSA button = do
    -- TODO: certain constant values have been hard-coded into this function, like the double-click
    -- distance, and the minimum time required to elapse between events for the event to not be
    -- considered a "bounce". These hard-coded values should be taken from a global configuration
    -- variable.
    let dragThreshold = 8
    let maxDoubleClickTimeDiff = 0.5 :: NominalDiffTime
    let buttonLens = sceneMouseButtonLens button
    stButton <- use (cloneLens buttonLens)
    let stFSA = stButton ^. mouseStateFSA
    case stButton ^. mouseStateSignal of
      Nothing -> pure ()
      Just (signal@(MouseSignal _dev pressed mods _button coord1)) -> do
        let t = stButton ^. mouseStateTime
        let (dt, dist) = maybe (999999.0 :: NominalDiffTime, 0)
              (\ (coord0, t0) ->
                 ( diffUTCTime t0 t
                 , let (V2 x y) = coord1 - coord0 in x * x + y * y
                 )
              )
              (stButton ^. mouseStateCoordinate)
        let keep  = cloneLens buttonLens . mouseStateCoordinate .= Just (coord1, t)
        let clear = cloneLens buttonLens . mouseStateCoordinate .= Nothing
        let nextStep stNext = cloneLens buttonLens . mouseStateFSA .= stNext
        let step stNext keepOrClear handler = do
              nextStep stNext
              keepOrClear
              handler button $ Mouse2D
                { theMouse2DPosition  = coord1
                , theMouse2DModifiers = mods
                }
        let ignore = report EVENT $ Strict.pack $
              "Mouse FSA " <> show stFSA <>
              ": unexpected signal ignored (" <> show signal <> ")"
        case stFSA of
          MouseOverState
            | pressed ->
              step MouseDownState keep guiMouseDown
          MouseDownState
            | not pressed ->
              step MouseClickState clear guiMouseClick
          MouseDownState
            | pressed && dist >= dragThreshold ->
              step MouseDragState (pure ()) $ \ b ->
              guiMouseDrag b . Just
          MouseClickState
            | pressed && dist < dragThreshold && dt < maxDoubleClickTimeDiff ->
              step MouseOverState clear guiMouseDoubleClick
          MouseClickState ->
              nextStep MouseOverState >> clear
          MouseDragState
            | pressed ->
              step MouseDragState clear $ \ b ->
              guiMouseDrag b . Just
          MouseDragState
            | not pressed ->
              step MouseOverState clear $ \ b evt -> do
                guiMouseDrag b (Just evt)
                guiMouseDrag b Nothing
          _ -> ignore

----------------------------------------------------------------------------------------------------

-- | Expresses information about which event handlers in a 'Actor' are set as an integer so that the
-- number of 'Role's that respond to a particular event type can be counted. This is used to
-- determine whether a group of 'Role's needs to maintain the event handler in the 'Happlet'
-- environment.
data ActorEventHandlerStats
  = ActorEventHandlerStats
    { countActors            :: !Int
      -- ^ Does the actor exist? This is here to simply count the number of 'Actor's in a group. It
      -- is zero only if this 'ActorEventHandlerStats' value is 'mempty'.
    , countActionDraw        :: !Int
      -- ^ Non-zero if an 'Actor' is drawn, meaning it can be drawn to the screen
    , countActionRedraw      :: !Int
      -- ^ Non-zero if an 'Actor' is to be redrawn
    , countActionFocus      :: !Int
      -- ^ Non-zero if an 'Actor' responds to selection actions
    , countActionKeyPress    :: !Int
      -- ^ Non-zero if an 'Actor' responds to 'Keyboard' actions
    , countActionMouseOver   :: !Int
      -- ^ Non-zero if an 'Actor' responds to mouse-over events.
    , countActionMouseRight  :: !(Maybe MouseEventHandlerStats)
      -- ^ Non-zero if an 'Actor' responds to any mouse button events.
    , countActionMouseLeft   :: !(Maybe MouseEventHandlerStats)
      -- ^ Non-zero if an 'Actor' responds to any mouse button events.
    , countActionStepAnimate :: !Int
      -- ^ Non-zero if an 'Actor' responds to animation step events
    }

data MouseEventHandlerStats
  = MouseEventHandlerStats
    { countActionMouseDown   :: !Int
    , countActionMouseClick  :: !Int
    , countActionMouseDouble :: !Int
    , countActionMouseDrag   :: !Int
    }

instance Monoid ActorEventHandlerStats where
  mappend = (<>)
  mempty = ActorEventHandlerStats
    { countActors            = 0
    , countActionDraw        = 0
    , countActionRedraw      = 0
    , countActionFocus       = 0
    , countActionKeyPress    = 0
    , countActionMouseOver   = 0
    , countActionMouseRight  = mempty
    , countActionMouseLeft   = mempty
    , countActionStepAnimate = 0
    }

instance Monoid MouseEventHandlerStats where
  mappend = (<>)
  mempty = MouseEventHandlerStats
    { countActionMouseDown   = 0
    , countActionMouseClick  = 0
    , countActionMouseDouble = 0
    , countActionMouseDrag   = 0
    }

instance Semigroup ActorEventHandlerStats where
  a <> b = let add f = f a + f b in ActorEventHandlerStats
    { countActors            = add countActors
    , countActionDraw        = add countActionDraw
    , countActionRedraw      = add countActionRedraw
    , countActionFocus       = add countActionFocus
    , countActionKeyPress    = add countActionKeyPress
    , countActionMouseOver   = add countActionMouseOver
    , countActionMouseRight  = countActionMouseRight a <> countActionMouseRight b
    , countActionMouseLeft   = countActionMouseLeft  a <> countActionMouseLeft  b
    , countActionStepAnimate = add countActionStepAnimate
    }

instance Semigroup MouseEventHandlerStats where
  a <> b = let add f = f a + f b in MouseEventHandlerStats
    { countActionMouseDown   = add countActionMouseDown
    , countActionMouseClick  = add countActionMouseClick
    , countActionMouseDouble = add countActionMouseDouble
    , countActionMouseDrag   = add countActionMouseDrag
    }

instance Show ActorEventHandlerStats where
  show a =
    (\ case
      []    -> "    (no event handlers set)\n"
      elems -> unlines $ ("    " ++) <$> elems
    ) $
    filter (not . null) $
    let f str item = if item a == 0 then "" else str ++ " = " ++ show (item a) in
    [ f "           countActors" countActors
    , f "       countActionDraw" countActionDraw
    , f "     countActionRedraw" countActionRedraw
    , f "      countActionFocus" countActionFocus
    , f "countActionStepAnimate" countActionStepAnimate
    , f "   countActionKeyPress" countActionKeyPress
    , f "  countActionMouseOver" countActionMouseOver
    , showMouseEventHandlerStats "countActionMouseRight" $ countActionMouseRight a
    , showMouseEventHandlerStats "countActionMouseLeft" $ countActionMouseLeft a
    ]

showMouseEventHandlerStats :: String -> Maybe MouseEventHandlerStats -> String
showMouseEventHandlerStats which = maybe "" $ \ a ->
  (\ case
      []    -> ""
      elems -> "  " ++ which ++ ":\n" ++ unlines (("       " ++) <$> elems)
  ) $
  filter (not . null) $
  let f str item = if item a == 0 then "" else str ++ " = " ++ show (item a) in
  [ f "countActionMouseDown" countActionMouseDown
  , f "countActionMouseClick" countActionMouseClick
  , f "countActionMouseDouble" countActionMouseDouble
  , f "countActionMouseDrag" countActionMouseDrag
  ]

-- | Takes two 'ActorEventHandlerStats' arguments, @a@ and @b@, and on each field @f@ of @a@ and
-- @b@, computes @(f a - f b)@. So if you have two sets of stats, of a 'Role' taken at 2 different
-- times during 'Script' evaluation, let's call them @statsT0@ and @statsT1@, then you will see
-- @'diffActorEventHandlerStats' statsT0 statsT1@ produce fields with a value of @1@ for fields that
-- have been added between T0 and T1, @-1@ for fields that have been removed, and @0@ for fields
-- that are unchanged.
diffActorEventHandlerStats
  :: ActorEventHandlerStats
  -> ActorEventHandlerStats
  -> ActorEventHandlerStats
diffActorEventHandlerStats a b =
  let diff f = f a - f b in
  ActorEventHandlerStats
  { countActors            = diff countActors
  , countActionDraw        = diff countActionDraw
  , countActionRedraw      = diff countActionRedraw
  , countActionFocus       = diff countActionFocus
  , countActionKeyPress    = diff countActionKeyPress
  , countActionMouseOver   = diff countActionMouseOver
  , countActionMouseRight  =
      diffButtonEventHandlerStats (countActionMouseLeft a) (countActionMouseLeft b)
  , countActionMouseLeft   =
      diffButtonEventHandlerStats (countActionMouseRight a) (countActionMouseRight b)
  , countActionStepAnimate = diff countActionStepAnimate
  }

diffButtonEventHandlerStats
  :: Maybe MouseEventHandlerStats
  -> Maybe MouseEventHandlerStats
  -> Maybe MouseEventHandlerStats
diffButtonEventHandlerStats a b = case (a, b) of
  (Nothing, Nothing) -> Nothing
  (Just  a, Nothing) -> Just a
  (Nothing, Just  b) -> Just b
  (Just  a, Just  b) -> 
    let diff f = f a - f b in Just $
    MouseEventHandlerStats
    { countActionMouseDown   = diff countActionMouseDown
    , countActionMouseClick  = diff countActionMouseClick
    , countActionMouseDouble = diff countActionMouseDouble
    , countActionMouseDrag   = diff countActionMouseDrag
    }

roleEventStats :: forall any . Role any -> ActorEventHandlerStats
roleEventStats r = ActorEventHandlerStats
  { countActors            = 1
  , countActionDraw        = if drawingIsNull (theActionDraw r) then 0 else 1
  , countActionRedraw      = inc theActionRedraw
  , countActionFocus       = inc theActionFocus
  , countActionKeyPress    = inc theActionKeyPress
  , countActionMouseOver   = inc theActionMouseOver
  , countActionMouseRight  = mouseEventStats $ theActionRightMouse r
  , countActionMouseLeft   = mouseEventStats $ theActionLeftMouse r
  , countActionStepAnimate = inc theActionStepAnimate
  } where
    inc :: (Role any -> Maybe void) -> Int
    inc fromRole = maybe 0 (const 1) (fromRole r)

mouseEventStats :: forall any . Maybe (RoleMouseEvents any) -> Maybe MouseEventHandlerStats
mouseEventStats = maybe Nothing $ \ r -> if roleMouseEventsNull r then Nothing else Just $
  MouseEventHandlerStats
  { countActionMouseDown = inc r theActionMouseDown
  , countActionMouseClick = inc r theActionMouseClick
  , countActionMouseDouble = inc r theActionMouseDouble
  , countActionMouseDrag = inc r theActionMouseDrag
  }
  where
    inc :: RoleMouseEvents any -> (RoleMouseEvents any -> Maybe void) -> Int
    inc r fromRole = maybe 0 (const 1) (fromRole r)

debugEventHandlerStats :: Strict.Text -> Script model ()
debugEventHandlerStats message = do
  role <- scriptGets theScriptRole
  report EVENT $ message <> "\n  Role:\n" <> theRoleLabel role <>
    "\n  Event handler stats:\n" <> Strict.pack (show $ roleEventStats role)

-- | Return the 'ActorEventHandlerStats' for the @'Actor' model@ of type that is currently
-- acting out this 'Script'.
getEventHandlerStats :: Script model ActorEventHandlerStats
getEventHandlerStats = scriptGetsRole roleEventStats

sceneGetActorStats :: ProvidesLogReporter provider => GUI provider (Scene stage) ActorEventHandlerStats
sceneGetActorStats = sceneRoleGets roleEventStats
