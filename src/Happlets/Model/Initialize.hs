-- | To begin programming your 'Happlet', import the back-end 'Happlets.Provider.Provider' you want
-- to use, for example @import Happlets.Lib.Gtk@. The functions in this module provide a convenient
-- interface for evaluating the functions provided by the 'Happlets.Provider.Provider', making it
-- easy to setup your application from within the "main" function of your executable program.
--
-- If your Happlet is just a simple, single-window happlet, then use 'simpleHapplet' to initialize
-- your Happlet. The 'simpleHapplet' function takes a 'Happlets.Provider.Provider', a function for
-- setting the 'Happlets.Config.Config' parameters, an initial value for the document model data,
-- and an initializing 'Happlets.Model.GUI.GUI' function for installing event handlers. See the
-- documentation for 'simpleHapplet' for an example.
--
-- It is also possible to define a Happlets where multiple Happlet windows are placed on screen when
-- the program is executed. To do this, the task of initializing the 'Happlets.Model.GUI.Happlet'
-- container, creating a new window, and attaching a 'Happlets.Model.GUI.Happlet' container to the
-- window, are all separate initialization steps which are evaluated in the 'Initialize' monad.
--
-- The 'happlet' function takes a back-end 'Happlets.Provider.Provider' and then evaluates an
-- 'Initialize' function, which behaves a lot like an imparative program in which you set parameter
-- variables and then call functions which make inspect the parameter variables.
--
-- In the 'Initialize' function, you can use the various @'Control.Lens.Lens''
-- 'Happlets.Config.Config'@ lenses defined in the "Happlets.Config" module with the
-- @'(Control.Lens..=')@ operator to set various configuration settings in a stateful way, much like
-- setting environment variables.
--
-- After setting the 'Happlets.Config.Config' paramters, you must:
--
-- 1. call 'newProvider' to create a new provider,
-- 2. call 'newHapplet' to create a new 'Happlets.Model.GUI.Happlet' container which contains your
--    document object model.
-- 3. call 'attachProvider' to attach the 'Happlets.Model.GUI.Happlet' to the provider, passing an
--    initializing 'Happlets.Model.GUI.GUI' function to setup the event handlers.
-- 4. if necessary, call 'deleteProvider'.
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
--     pro <- newProvider
--     ---
--     --- create a new 'Happlets.Model.GUI.Happlet' to contain the document object model ---
--     ---
--     happ <- 'newHapplet' MyDocumentModel
--         { myFoo = emptyFoo
--         , myBar = emptyBar
--         }
--     ---
--     --- attaching the happlet container to the window and initializing the event handlers ---
--     ---
--     'attachProvider' pro happ initMyHapplet
-- @
--
-- The initializing function passed to 'attachProvider' takes a 'Happlets.Draw.SampCoord.PixSize',
-- so if your document model contains information sensitive to the geometry of the window, you can
-- initialize your document model using the given window size.
--
-- The document model stored in the 'Happlets.Model.GUI.Happlet' container can be any data type at
-- all. Haskell's static type checker ensures that the 'GUI' function you use to initialize the
-- event handlers must match the content of the 'Happlets.Model.GUI.Happlet' container. As your
-- 'Happlets.Model.GUI.GUI' program begins receiving events from the operating system's window
-- manager, the document model is updated using the ordinary 'Control.Monad.State.Class.MonadState'
-- API functions like 'Control.Monad.State.Class.get', 'Control.Monad.State.Class.put',
-- 'Control.Monad.State.Class.modify', as well as the 'Control.Lens.Lens'' functions
-- 'Control.Lens.use', 'Control.Lens.assign' and it's infix form ('Control.Lens..='), and
-- 'Control.Lens.modifying' and it's infix form ('Control.Lens.%=').
--
-- Please also refer to the "Happlets.Model.GUI" documentation on the 'Happlets.Model.GUI.GUI'
-- function type which is the Happlets equivalent of the @IO@ function type. There you will find
-- instructions on how to install event handlers.
module Happlets.Model.Initialize where

import           Happlets.Config
import           Happlets.View.SampCoord
import           Happlets.Model.GUI
import           Happlets.Provider

import           Control.Monad.Reader

----------------------------------------------------------------------------------------------------

-- | This function carries around with it a reference to a 'Happlets.Provider.Provider' and a
-- 'Happlets.Config.Config' data structure.
-- 
-- The real use of this function type is provided by the various classes that can be instantiated by
-- the back-end provider.
newtype Initialize provider a
  = Initialize { unwrapGUI :: ReaderT (Provider provider) (StateT Config IO) a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadReader (Provider provider) (Initialize provider) where
  ask = Initialize ask
  local f = Initialize . local f . unwrapGUI

instance MonadState Config (Initialize provider) where
  state = Initialize . state

-- | This function is an all-in-one setup function that lets you create a
-- 'Happlets.Model.GUI.Happlet' with a single function call. Pass the back-end
-- 'Happlets.Provider.Provider', a stateful function for updating the default
-- 'Happlets.Config.Config', an initial @model@, and an initial 'Happlets.Model.GUI.GUI' function
-- for installing the 'Happlets.Model.GUI.Happlet' event handlers. This function will automatically
-- call 'newHapplet', 'newProvider', 'attachProvider', 'happlet', and then
-- return when the GUI event loop halts. Here is an example of how you might use it:
--
-- @
-- import Happlets
--
-- -- Here you define your document model data type, which will be updated by the 'Happlets.Model.GUI.GUI'
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
--         'Happlets.Model.GUI.modifyModel' $ \ mydoc ->
--             mydoc{ myDocWinSize = winSiz } --  <-- here you set the window size
--
--         'Happlets.Model.GUI.mouseEvents' 'Happlets.Model.GUI.MouseButton' $ \ mousEvt -> do
--             ---
--             --- some code for handling mouse events ...
--             ---
--
--         'Happlets.Model.GUI.keyboardEvents' $ \ keyEvt -> do
--             ---
--             --- some code for handling key events ...
--             ---
--     )
-- @
simpleHapplet
  :: Provider provider -- ^ pass the Happlets back-end provider
  -> StateT Config IO ()
      -- ^ Use lens functions like @('Control.Lens..=')@ and @('Control.Lens.%=')@ to define this
      -- function.
  -> model -- ^ pass the initial model
  -> (PixSize -> GUI provider model ())
      -- ^ Pass the function which installs the event handlers.
  -> IO ()
simpleHapplet provider updcfg model init = do
  doInitializeGUI provider
  hap <- makeHapplet model
  config <- execStateT updcfg (defaultConfig provider)
  win <- doProviderNew provider config
  doProviderAttach provider True win hap init
  doGUIEventLoopLaunch provider config
  doProviderDelete provider win

-- | Run the 'Initialize' function with the given back-end 'Happlets.Happlet.Provider'.
happlet :: Provider provider -> Initialize provider a -> IO a
happlet provider (Initialize f) = do
  doInitializeGUI provider
  (a, config) <- runStateT (runReaderT f provider) (defaultConfig provider)
  liftIO $ doGUIEventLoopLaunch provider config
  return a

-- | Create a new @provider@. The @provider@ itself only contains stateful information relevant to
-- the 'Happlets.Model.GUI.GUI' back-end 'Happlets.Provider.Provider', it does not do anything on
-- it's own. This function does not place a window on the screen, only the 'attachProvider' function
-- can do that.
newProvider :: Initialize provider (ProviderStateRef provider)
newProvider = liftIO =<< asks doProviderNew <*> get

-- | Create a new 'Happlets.Model.GUI.Happlet', which contains a document object model of any
-- type. When you call 'attachProvider' you must supply this 'Happlets.Model.GUI.Happlet' container
-- along with a 'Happlets.Model.GUI.GUI' function that installs all the necessary event handlers
-- into the @provider@. As the @provider@ begins reciving events from the operating system's window
-- manager, these event handlers are evaluated, and the content of this 'Happlets.Model.GUI.Happlet'
-- can be updated using the 'Control.Monad.State.Class.MonadState' functions like
-- 'Control.Monad.State.Class.get', 'Control.Monad.State.Class.put',
-- 'Control.Monad.State.Class.modify', as well as the 'Control.Lens.Lens'' functions
-- 'Control.Lens.use', 'Control.Lens.assign' and it's infix form ('Control.Lens..='), and
-- 'Control.Lens.modifying' and it's infix form ('Control.Lens.%=').
newHapplet :: model -> Initialize provider (Happlet model)
newHapplet = liftIO . makeHapplet

-- | This function evaluates the given 'Happlets.Model.GUI.GUI' function to install
-- 'Happlets.Model.GUI.GUI' event handlers into the @provider@ that can modify the content of the
-- 'Happlets.Model.GUI.Happlet' container in response to events received from the operating system's
-- window manager.
--
-- Note that it is possible to detach a 'Happlets.Model.GUI.Happlet' container from a @provider@ and
-- re-attach the @provider@ to an entirely different type of 'Happlets.Model.GUI.Happlet' container
-- using the 'Happlets.Model.GUI.providerChangeHapplet' function as the program is running and
-- receiving events. This allows a single @provider@ to switch between multiple
-- 'Happlets.Model.GUI.Happlet's during program execution. Please refer to the
-- 'Happlets.Model.GUI.providerChangeHapplet' function documentation for information about how to do
-- this.
attachProvider
  :: Bool -- ^ make visible immediately?
  -> (ProviderStateRef provider)
      -- ^ the provider function that will actually realize the window
  -> Happlet model
      -- ^ the happlet to attach
  -> (PixSize -> GUI provider model ())
      -- ^ the GUI initializer that will install event handlers
  -> Initialize provider ()
attachProvider vis win happ init = liftIO =<<
  asks doProviderAttach <*> pure vis <*> pure win <*> pure happ <*> pure init

---- | This function launches the GUI event loop. This function is called automatically by the
---- functions 'happlet' and 'simpleHapplet', so it should generally not be called. This function is
---- exported for those who wish to 
--launchGUIEventLoop :: Initialize provider ()
--launchGUIEventLoop = get >>= \ config -> asks doGUIEventLoopLaunch >>= liftIO . ($ config)

---- | Delete a provider. It almost never necessary to delete a Happlet window because usually once the
---- 'Initialize' function has completed evaluating, there ought not be anything after the 'happlet'
---- function in the @main@ function, and so the executable program will quit, signaling to the
---- operating system window manager to delete all windows associated with this program. However this
---- function exists for the purpose of explicitly deleting windows. Be sure this function is called
---- /after/ evaluating the 'launchGUIEventLoop' function, otherwise the window will be deleted before
---- the event loop begins.
--deleteProvider :: window -> Initialize window ()
--deleteProvider win = liftIO =<< asks doWindowDelete <*> pure win
