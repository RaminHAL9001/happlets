-- | To begin programming your 'Happlet', import the back-end 'Happlets.Provider.Provider' you want
-- to use, for example @import Happlets.Lib.Gtk@. The functions in this module provide a convenient
-- interface for evaluating the functions provided by the 'Happlets.Provider.Provider', making it
-- easy to setup your application from within the "main" function of your executable program.
--
-- If your Happlet is just a simple, single-window happlet, then use 'simpleHapplet' to initialize
-- your Happlet. The 'simpleHapplet' function takes a 'Happlets.Provider.Provider', a function for
-- setting the 'Happlets.Config.Config' parameters, an initial value for the document model data,
-- and an initializing 'Happlets.GUI.GUI' function for installing event handlers. See the
-- documentation for 'simpleHapplet' for an example.
--
-- It is also possible to define a Happlets where multiple Happlet windows are placed on screen when
-- the program is executed. To do this, the task of initializing the 'Happlets.GUI.Happlet'
-- container, creating a new window, and attaching a 'Happlets.GUI.Happlet' container to the window,
-- are all separate initialization steps which are evaluated in the 'Initialize' monad.
--
-- The 'happlet' function takes a back-end 'Happlets.Provider.Provider' and then evaluates an
-- 'Initialize' function, which behaves a lot like an imparative program in which you set parameter
-- variables and then call functions which make inspect the parameter variables.
--
-- In the 'Initialize' function, you can use the various
-- @'Control.Lens.Lens'' 'Happlets.Config.Config'@ lenses defined in the "Happlets.Config" module
-- with the @'(Control.Lens..=')@ operator to set various configuration settings in a stateful way,
-- much like setting environment variables.
--
-- After setting the 'Happlets.Config.Config' paramters, you must:
--
-- 1. call 'newWindow' to create a new window,
-- 2. call 'newHapplet' to create a new 'Happlets.GUI.Happlet' container which contains your
--    document object model.
-- 3. call 'attachWindow' to attach the 'Happlets.GUI.Happlet' to the window, passing an
--    initializing 'Happlets.GUI.GUI' function to setup the event handlers.
-- 4. perform the above 3 steps for as many windows as necessary.
-- 5. call 'launchGUIEventLoop' to start the application
-- 6. if necessary, call 'deleteWindow'.
--
-- @
-- main :: IO ()
-- main = 'happlet' gtkHapplet $ do
--     ---
--     --- setting the 'Happlets.Config.Config' paramters ---
--     ---
--     'Happlets.Config.registeredAppName' 'Control.Lens..=' "My First Happlet!"
--     'Happlets.Config.windowTitleBar' .= "Hello World Happlet"
--     'Happlets.Config.recommendWindowSize' 'Control.Lens..=' (640, 480)
--     ---
--     --- creating the new window ---
--     ---
--     win <- newWindow
--     ---
--     --- create a new 'Happlets.GUI.Happlet' to contain the document object model ---
--     ---
--     happ <- 'newHapplet' MyDocumentModel
--         { myFoo = emptyFoo
--         , myBar = emptyBar
--         }
--     ---
--     --- attaching the happlet container to the window and initializing the event handlers ---
--     ---
--     'attachWindow' win happ initMyHapplet
--     ---
--     'launchGUIEventLoop' -- Will not return until an exit condition halts the event loop.
--     ---
--     --- not always necessary, but delete the windows after the event loop halts ---
--     ---
--     'deleteWindow' win
-- @
--
-- The initializing function passed to 'attachWindow' takes a 'Happlets.Draw.SampCoord.PixSize', so
-- if your document model contains information sensitive to the geometry of the window, you can
-- initialize your document model using the given window size.
--
-- The document model stored in the 'Happlets.GUI.Happlet' container can be any data type at
-- all. Haskell's static type checker ensures that the 'GUI' function you use to initialize the
-- event handlers must match the content of the 'Happlets.GUI.Happlet' container. As your
-- 'Happlets.GUI.GUI' program begins receiving events from the operating system's window manager,
-- the document model is updated using the ordinary 'Control.Monad.State.Class.MonadState' API
-- functions like 'Control.Monad.State.Class.get', 'Control.Monad.State.Class.put',
-- 'Control.Monad.State.Class.modify', as well as the 'Control.Lens.Lens'' functions
-- 'Control.Lens.use', 'Control.Lens.assign' and it's infix form ('Control.Lens..='), and
-- 'Control.Lens.modifying' and it's infix form ('Control.Lens.%=').
--
-- Please also refer to the "Happlets.GUI" documentation on the 'Happlets.GUI.GUI' function type
-- which is the Happlets equivalent of the @IO@ function type. There you will find instructions on
-- how to install event handlers.
module Happlets.Initialize where

import           Happlets.Config
import           Happlets.Draw.SampCoord
import           Happlets.GUI
import           Happlets.Provider

import           Control.Monad.Reader
import           Control.Monad.State

----------------------------------------------------------------------------------------------------

-- | This function carries around with it a reference to a 'Happlets.Provider.Provider' and a
-- 'Happlets.Config.Config' data structure.
-- 
-- The real use of this function type is provided by the various classes that can be instantiated by
-- the back-end provider.
newtype Initialize window a
  = Initialize { unwrapGUI :: ReaderT (Provider window) (StateT Config IO) a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadReader (Provider window) (Initialize window) where
  ask = Initialize ask
  local f = Initialize . local f . unwrapGUI

instance MonadState Config (Initialize window) where
  state = Initialize . state

-- | This function is an all-in-one setup function that lets you create a 'Happlets.GUI.Happlet'
-- with a single function call. Pass the back-end 'Happlets.Provider.Provider', a stateful function
-- for updating the default 'Happlets.Config.Config', an initial @model@, and an initial
-- 'Happlets.GUI.GUI' function for installing the 'Happlets.GUI.Happlet' event handlers. This
-- function will automatically call 'newHapplet', 'newWindow', 'attachWindow', 'happlet', and then
-- return when the GUI event loop halts. Here is an example of how you might use it:
--
-- @
-- import Happlets
--
-- -- Here you define your document model data type, which will be updated by the 'Happlets.GUI.GUI'
-- -- function whenever an event occurs.
-- data MyDocModel = MyDocModel
--      { someFoo      :: FooType
--        someBar      :: BarType
--        myDocWinSize :: 'Happlets.Draw.SampCoord.PixSize'
--      }
-- 
-- -- Here you define your Haskell "main" function.
-- main :: IO ()
-- main = 'simpleHapplet' gtkHapplet <-- let's say we are using the Gtk+ 'Happlets.Provider.Provider'
--     (do registeredAppName   'Control.Lens..=' "My Happlet"
--         windowTitleBar      'Control.Lens..=' "New Document"
--         recommendWindowSize 'Control.Lens..=' (640, 480)
--         -- The type of this "do" block is ('Control.Monad.State.StateT' 'Happlets.Config.Config' IO ()).
--         -- You use the ('Control.Lens..=') function to set the various "Happlets.Config" parameters.
--         -- The configurable parameters are of type ('Control.Lens.Lens'' 'Happlets.Config.Config').
--     )
--     MyDocModel
--       { someFoo = initNewFoo
--       , someBar = emptyBar
--       , winSize = 'Linear.V2.V2' (-1) (-1)
--         -- Initialize your document model. Notice that your data type can contain a field
--         -- storing the size of the window. This is helpful for rendering graphics where the
--         -- geometry depends on the size of the window, for example, for graphics that you
--         -- like to center in the window.
--       }
--     (\\ winSiz -> do
--         -- This is the first function that runs before any event handlers are called.
--         -- Use this function to update your document model with the window size (if necessary),
--         -- and install the event handler functions.
--
--         'Happlets.GUI.modifyModel' $ \ mydoc ->
--             mydoc{ myDocWinSize = winSiz } --  <-- here you set the window size
--
--         'Happlets.GUI.mouseEvents' 'Happlets.GUI.MouseButton' $ \ mousEvt -> do
--             ---
--             --- some code for handling mouse events ...
--             ---
--
--         'Happlets.GUI.keyboardEvents' $ \ keyEvt -> do
--             ---
--             --- some code for handling key events ...
--             ---
--     )
-- @
simpleHapplet
  :: Provider window
  -> StateT Config IO ()
  -> model
  -> (PixSize -> GUI window model ())
  -> IO ()
simpleHapplet provider updcfg model init = happlet provider $ do
  get >>= liftIO . execStateT updcfg >>= put
  join $ attachWindow True <$> newWindow <*> newHapplet model <*> pure init

-- | Run the 'Initialize' function with the given back-end 'Happlets.Happlet.Provider'.
happlet :: Provider window -> Initialize window a -> IO a
happlet provider (Initialize f) = do
  doInitializeGUI provider
  (a, config) <- runStateT (runReaderT f provider) (defaultConfig provider)
  doGUIEventLoopLaunch provider config
  return a

-- | Create a new @window@. The @window@ itself only contains stateful information relevant to the
-- 'Happlets.GUI.GUI' back-end 'Happlets.Provider.Provider', it does not do anything on it's
-- own. This function does not place a window on the screen, only the 'attachWindow' function can do
-- that.
newWindow :: Initialize window window
newWindow = liftIO =<< asks doWindowNew <*> get

-- | Create a new 'Happlets.GUI.Happlet', which contains a document object model of any type. When
-- you call 'attachWindow' you must supply this 'Happlets.GUI.Happlet' container along with a
-- 'Happlets.GUI.GUI' function that installs all the necessary event handlers into the @window@. As
-- the @window@ begins reciving events from the operating system's window manager, these event
-- handlers are evaluated, and the content of this 'Happlets.GUI.Happlet' can be updated using the
-- 'Control.Monad.State.Class.MonadState' functions like 'Control.Monad.State.Class.get',
-- 'Control.Monad.State.Class.put', 'Control.Monad.State.Class.modify', as well as the
-- 'Control.Lens.Lens'' functions 'Control.Lens.use', 'Control.Lens.assign' and it's infix form
-- ('Control.Lens..='), and 'Control.Lens.modifying' and it's infix form ('Control.Lens.%=').
newHapplet :: model -> Initialize window (Happlet model)
newHapplet = liftIO . makeHapplet

-- | This function evaluates the given 'Happlets.GUI.GUI' function to install 'Happlets.GUI.GUI'
-- event handlers into the @window@ that can modify the content of the 'Happlets.GUI.Happlet'
-- container in response to events received from the operating system's window manager.
--
-- Note that it is possible to detach a 'Happlets.GUI.Happlet' container from a @window@ and
-- re-attach the @window@ to an entirely different type of 'Happlets.GUI.Happlet' container using
-- the 'Happlets.GUI.windowChangeHapplet' function as the program is running and receiving
-- events. This allows a single @window@ to switch between multiple 'Happlets.GUI.Happlet's during
-- program execution. Please refer to the 'Happlets.GUI.windowChangeHapplet' function documentation
-- for information about how to do this.
attachWindow
  :: Bool
  -> window
  -> Happlet model
  -> (PixSize -> GUI window model ())
  -> Initialize window ()
attachWindow vis win happ init = liftIO =<<
  asks doWindowAttach <*> pure vis <*> pure win <*> pure happ <*> pure init

-- | This function launches the GUI event loop. You must call this function once after all of your
-- Happlet setup has been performed. This function will not return until the GUI event loop has
-- halted, so it must be the last function called in the 'Initialize' "do" block, except perhaps for
-- the 'deleteWindow' function which can be evaluated after it.
launchGUIEventLoop :: Initialize window ()
launchGUIEventLoop = get >>= \ config -> asks doGUIEventLoopLaunch >>= liftIO . ($ config)

-- | Delete a window. It often is not necessary to delete a Happlet window because usually once the
-- 'Initialize' function has completed evaluating, there ought not be anything after the 'happlet'
-- function in the @main@ function, and so the executable program will quit, signaling to the
-- operating system window manager to delete all windows associated with this program. None the
-- less, this function exists for the purpose of explicitly deleting windows.
deleteWindow :: window -> Initialize window ()
deleteWindow win = liftIO =<< asks doWindowDelete <*> pure win
