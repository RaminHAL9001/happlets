-- | To begin programming your 'Happlet', import the back-end 'Happlets.Provider.Provider' you want
-- to use, for example @import Happlets.Provider.Gtk2@. The functions in this module provide a
-- convenient interface for evaluating the functions provided by the 'Happlets.Provider.Provider',
-- making it easy to setup your application from within the "main" function of your executable
-- program.
--
-- If your Happlet is just a simple, single-window happlet, then use 'simpleHapplet' to initialize
-- your Happlet. The 'simpleHapplet' function takes a 'Happlets.Provider.Provider', a function for
-- setting the 'Happlets.Initialize.HappletInitConfig' parameters, an initial value for the document
-- model data, and an initializing 'Happlets.Model.GUI.GUI' function for installing event
-- handlers. See the documentation for 'simpleHapplet' for an example.
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
-- 'Happlets.Initialize.HappletInitConfig'@ lenses defined in the "Happlets.Initialize" module with
-- the @'(Control.Lens..=')@ operator to set various configuration settings in a stateful way, much
-- like setting environment variables.
--
-- After setting the 'Happlets.Initialize.InitConfig' paramters, you must:
--
-- 1. call 'newProvider' to create a new provider,
--
-- 2. call 'newModel' to create a new 'Happlets.Model.GUI.Happlet' container which contains your
--    document object model.
--
-- 3. call 'attachWindow' to attach the 'Happlets.Model.GUI.Happlet' to the provider, passing an
--    initializing 'Happlets.Model.GUI.GUI' function to setup the event handlers.
--
-- 4. if necessary, call 'deleteProvider'.
--
-- @
-- main :: IO ()
-- main = 'happlet' gtk2Provider $ do
--     ---
--     --- setting the 'Happlets.Initialize.InitConfig' paramters ---
--     ---
--     'Happlets.Initialize.registeredAppName' 'Control.Lens..=' "My First Happlet!"
--     'Happlets.Initialize.initWindowTitleBar' .= "Hello World Happlet"
--     'Happlets.Initialize.recommendWindowSize' 'Control.Lens..=' (640, 480)
--     ---
--     --- create a new 'Happlets.Model.GUI.Happlet' to contain the document object model ---
--     ---
--     happ <- 'newModel' MyDocumentModel
--         { myFoo = emptyFoo
--         , myBar = emptyBar
--         }
--     ---
--     --- attaching the happlet container to the window and initializing the event handlers ---
--     ---
--     'attachWindow' happ initMyHapplet
-- @
--
-- The initializing function passed to 'attachWindow' takes a 'Happlets.Draw.SampCoord.PixSize', so
-- if your document model contains information sensitive to the geometry of the window, you can
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
module Happlets.Initialize
  ( Provider(..),

    -- ** Initializing
    Initialize, happlet, newHapplet, newHappletIO, attachWindow,

    -- ** Parameters Configurable at Initialization Time
    --
    -- Methods and data types for working with the initial Happlet configuration. Be careful not to
    -- confuse the 'HappletInitConfig' data type with the
    -- 'Happlets.Provider.ConfigState.ConfigState'.  'HappletInitConfig' settings are can only
    -- changed before the Happlet is initialized and lanched, whereas the
    -- 'Happlets.Provider.ConfigState.ConfigState' settings can be changed while the Happlet program
    -- is running.
    InitConfig, StrictFilePath, makeInitConfig,
    sanitizeName, initConfigFilePath,
    registeredAppName, initWindowTitleBar, initBackgroundTransparency, initBackgroundGreyValue,
    recommendWindowPosition, recommendWindowSize, animationFrameRate,
    deleteWindowOnClose, quitOnWindowClose, initDecorateWindow, theActualLogReporter,
    setMinLogReportLevel, setLogReportWriter,
  ) where

import           Happlets.Logging
import           Happlets.Model.GUI
import           Happlets.View.Types2D (PixSize)

import           Control.Arrow
import           Control.Concurrent.MVar
import           Control.Exception
import           Control.Lens
import           Control.Monad.Reader

import           Data.Char (isPrint)
import           Data.List (groupBy)
import qualified Data.Text as Strict

import           System.IO (hPutStrLn, stderr)

----------------------------------------------------------------------------------------------------

-- | This data structure contains a set of "callback" functions that a Happlet back-end system
-- provider must provide so that the 'happlet' function can call the necessary low-level
-- initializers for the GUI.
--
-- The @provider@ is a data type containing all information necessary for a high-level 'Happlet' to
-- interface with the low-level back-end provider. The @provider@ is associated with a @provider@
-- handle type (usually a wrapper around a 'Control.Concurrent.MVar') and an @image@ type which
-- store image buffers. The @provider@ and @image@ types are functionally dependent on the
-- @provider@ type so it is only necessary to specify the @provider@ type to the type checker in
-- ambiguous situations where the type variables cannot be resolved.
--
-- Furthermore, the @provider@ type may provided different types of @provider@ which display
-- different types of @image@. One example of how this might be used is for a Gtk+ back-end might
-- provide a single @provider@ type but create two different instances of @provider@, one with
-- Cairo-based @image@ buffers, and one with OpenGL-based @image@ buffers.
data Provider provider
  = Provider
    { -- | The default 'Happlet.Initialize.InitConfig' parameters best suited for this partuclar
      -- back-end. The 'happlet' function will evaluate this function first, and then evaluate the
      -- 'Initialize' callback function passed to it by end users of this library.
      defaultConfig :: Initialize provider ()

      -- | This is the pre-initialization function that should run before performing any GUI-related
      -- action at all.
    , doInitializeGUI :: IO ()

      -- | This function launches the event loop for the given @provider@ and does not return until
      -- the GUI application's event handler thread has quit. In the case of Gtk+, since there can
      -- only be one event loop per application, calling this function on any window handle will
      -- launch the event loop for __all__ window handles.
    , doGUIEventLoopLaunch :: InitConfig -> IO ()

      -- | This function creates a @provider@ without installing any event handlers, and without
      -- making the window visible. The @provider@ craeted can then have 'Happlet's attached to it
      -- using 'doWindowAttach'. In order to return a 'ProviderStateLock', it is necessary to
      -- evaluate 'initProviderState'.
    , doProviderNew :: InitConfig -> IO (ProviderStateLock provider)

      -- | This function asks the operating system or desktop environment to delete the window
      -- associated with the given @provider@. If the 'Happlets.Initailze.quitOnWindowClose'
      -- configuration parameter is 'Prelude.True', calling this function should also quit the whole
      -- application.
    , doProviderDelete :: ProviderStateLock provider -> IO ()

      -- | This function deletes all event handlers currently installed into the @provider@ and
      -- evaluate a new 'Happlets.GUI.GUI' function which can install new event handlers.
      --
      -- This function should only install the given initializing event handler and then immediately
      -- return, it should not otherwise launch any threads, update the 'Happlet' model, or make any
      -- synchronous calls.
    , doProviderAttach
        :: forall model
        .  Bool -- ^ whether or not to make the window visible
        -> ProviderStateLock provider
            -- ^ the window to which the 'Happlets.Happlet.Happlet' will be attached
        -> Happlet model -- ^ the 'Happlets.Happlet.Happlet' to attach.
        -> (PixSize -> GUI provider model ())
        -> IO ()
    }

----------------------------------------------------------------------------------------------------

-- | This function carries around with it a reference to a 'Happlets.Provider.Provider' and a
-- 'Happlets.Initialize.InitConfig' data structure.
-- 
-- The real use of this function type is provided by the various classes that can be instantiated by
-- the back-end provider.
newtype Initialize provider a
  = Initialize (ReaderT (Provider provider) (StateT InitState IO) a)
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadState InitConfig (Initialize provider) where
  state f =
    Initialize $
    state $ \ init ->
    let (a, config) = f $ theInitConfig init in
    (a, init{ theInitConfig = config })

instance CanWriteReports (Initialize provider) where
  report msgLevel msg =
    getUpdateReporter >>=
    liftIO . ($ msg) . ($ msgLevel)

data InitState
  = InitState
    { theInitConfig :: !InitConfig
    , theInitLogReporter ::
        !((Strict.Text -> IO ()) ->
           ReportLevel ->
           ReportLevel ->
           Strict.Text ->
           IO ()
         )
    }

getUpdateReporter :: Initialize provider (LogReporter IO)
getUpdateReporter = Initialize $ do
  InitState{theInitConfig=config,theInitLogReporter=report} <- get
  return $ report (theGlobalLogReporter config) (theConfigMaxLogLevel config)

-- not for export
asksInit :: (Provider provider -> a) -> Initialize provider a
asksInit = Initialize . asks

-- | Run the 'Initialize' function with the given back-end 'Happlets.Happlet.Provider'.
happlet :: Provider provider -> Initialize provider a -> IO a
happlet provider userInit = do
  let (Initialize init) = defaultConfig provider >> userInit
  logLock <- newMVar ()
  config0 <- makeInitConfig
  doInitializeGUI provider
  (a, init) <- runStateT
    (runReaderT init provider) $
    InitState
    { theInitConfig = config0
    , theInitLogReporter   = \ print minLevel msgLevel msg ->
        withMVar logLock $ \ () ->
        when (msgLevel >= minLevel) $
        print msg >>=
        evaluate
    }
  let config =
        (theInitConfig init)
        { theActualLogReporter =
          theInitLogReporter init (theGlobalLogReporter config) (theConfigMaxLogLevel config)
        }
  liftIO $ doGUIEventLoopLaunch provider config
  return a

-- | Create a new 'Happlets.Model.GUI.Happlet', which contains a document object model of any
-- type. When you call 'attachWindow' you must supply this 'Happlets.Model.GUI.Happlet' container
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

-- | Same as 'newHapplet', but initializes the @model@ by evaluating an @IO@ function.
newHappletIO :: IO model -> Initialize provider (Happlet model)
newHappletIO = liftIO . (>>= makeHapplet)

-- | This function evaluates the "main" 'Happlets.Model.GUI.GUI' function with a Happlet system
-- provider.
--
-- Note that it is possible to detach a 'Happlets.Model.GUI.Happlet' container from a @provider@ and
-- re-attach the @provider@ to an entirely different type of 'Happlets.Model.GUI.Happlet' container
-- using the 'Happlets.Model.GUI.changeRootHapplet' function as the program is running and receiving
-- events. This allows a single @provider@ to switch between multiple 'Happlets.Model.GUI.Happlet's
-- during program execution. Please refer to the 'Happlets.Model.GUI.changeRootHapplet' function
-- documentation for information about how to do this.
attachWindow
  :: Bool -- ^ make visible immediately?
  -> Happlet model
      -- ^ the happlet to attach
  -> (PixSize -> GUI provider model ())
      -- ^ the GUI initializer that will install event handlers
  -> Initialize provider ()
attachWindow vis happ init = do
  window <- asksInit doProviderNew <*> get >>= liftIO
  attach <- asksInit doProviderAttach
  liftIO $ attach vis window happ init

----------------------------------------------------------------------------------------------------

type StrictFilePath = Strict.Text

-- | This is the data structure used to configure various parameters in the Happlet back-end
-- provider, as well as a few defaults that end users of this library may want to set when they
-- first initialize their app, such as the app's window size and position, or the default background
-- color for the canvas.
data InitConfig
  = InitConfig
    { theConfigMaxLogLevel       :: ReportLevel
    , theGlobalLogReporter       :: Strict.Text -> IO ()
    , theActualLogReporter       :: LogReporter IO
      -- ^ This function is the 'LogReporter' that was setup by the 'Initialze' callback that was
      -- given to the 'happlet' function. This is the only field that is read-only to the
      -- 'Initialize' function.
    , theInitConfigFilePath      :: StrictFilePath
    , theRegisteredAppName       :: Strict.Text
    , theWindowTitleBar          :: Strict.Text
    , theBackgroundTransparency  :: (Maybe Double)
    , theBackgroundGreyValue     :: Double
    , theRecommendWindowPosition :: (Int, Int)
    , theRecommendWindowSize     :: (Int, Int)
    , theAnimationFrameRate      :: Double
    , willDecorateWindow         :: Bool
    , willQuitOnWindowClose      :: Bool
    , willDeleteWindowOnClose    :: Bool
    }

makeInitConfig :: IO InitConfig
makeInitConfig = return config where
  config =
    InitConfig
    { theConfigMaxLogLevel = INFO
    , theGlobalLogReporter = hPutStrLn stderr . Strict.unpack
    , theActualLogReporter = const $ theGlobalLogReporter config
    , theInitConfigFilePath = ""
    , theRegisteredAppName = "Happlet"
    , theWindowTitleBar = "Happlet"
    , theBackgroundTransparency = Nothing
    , theBackgroundGreyValue = 1.0
    , theRecommendWindowPosition = (0, 0)
    , theRecommendWindowSize = (768, 512)
    , theAnimationFrameRate = 24.0
    , willDecorateWindow = True
    , willQuitOnWindowClose = True
    , willDeleteWindowOnClose = True
    }

-- | A string sanitizer for applied to strings assigned to 'registeredAppName' and 'initWindowTitleBar'.
sanitizeName :: Strict.Text -> Strict.Text
sanitizeName = let sp = ' ' in Strict.unpack >>>
  takeWhile (\ c -> c /= '\n' && c /= '\r')  >>> -- Take only the first line of text.
  fmap (\ c -> if c == '\t' then sp else c)  >>> -- Convert tabs to single-spaces.
  filter isPrint                             >>> -- Filter out non-printing characters.
  dropWhile (== sp)                          >>> -- Cut out leading spaces.
  groupBy (\ a b -> a == sp && b == sp)      >>> -- Group together spaces.
  ( let loop stk = \ case
          []           -> stk ""
          [' ':_]      -> stk ""                     -- Cut out trailing spaces.
          (' ':_):more -> loop (stk . (sp  : )) more -- Replace multi spaces with single spaces.
          txt    :more -> loop (stk . (txt ++)) more
    in loop id )                             >>> 
  Strict.pack

-- | This function set's the minimum logging level that will be used to determine whether for calls
-- to 'report' actually display a message in the log. The 'ReportLevel' value passed to 'report'
-- must be larger than the minimum value set here in order for the message to actually appear in the
-- log.
setMinLogReportLevel :: ReportLevel -> Initialize provider ()
setMinLogReportLevel level = do
  configMinLogLevel .= level
  getUpdateReporter >>= assign actualLogReporter

-- | This function set's the minimum logging level that will be used to determine whether for calls
-- to 'report' actually display a message in the log. The 'ReportLevel' value passed to 'report'
-- must be larger than the minimum value set here in order for the message to actually appear in the
-- log.
-- 
-- This function need not provide any locking mechanism or checking of the 'ReportLevel', those
-- mechanisms are already built-in and will call the function you set here. By default, this
-- function writes messages to 'stderr' using 'hPutStrLn'.
setLogReportWriter :: LogReporter IO -> Initialize provider ()
setLogReportWriter reporter = do
  actualLogReporter .= reporter
  getUpdateReporter >>= assign actualLogReporter

-- | Reports the file path from which this config file was loaded, or a null string if this has not
-- been set.
initConfigFilePath :: Lens' InitConfig StrictFilePath
initConfigFilePath = lens theInitConfigFilePath $ \ a b -> a{ theInitConfigFilePath = b }

-- Not for export: must be set by 'setMaxLogReportLevel'
--
-- Configure the minimum 'ReportLevel' to accept from the 'report' function. If messages passed to
-- the 'report' function are lower than this minimum level, they are not reported.
configMinLogLevel :: Lens' InitConfig ReportLevel
configMinLogLevel = lens theConfigMaxLogLevel $ \ a b -> a{ theConfigMaxLogLevel = b }

---- Not for export: must be set by 'setLogReportWriter'
--globalLogReporter :: Lens' InitConfig (Strict.Text -> IO ())
--globalLogReporter = lens theGlobalLogReporter $ \ a b -> a{ theGlobalLogReporter = b }

-- Not for export: must be read-only
actualLogReporter :: Lens' InitConfig (LogReporter IO)
actualLogReporter = lens theActualLogReporter $ \ a b -> a{ theActualLogReporter = b }

-- | Gtk+ apps can register your GHCi/Happlet process with the window manager under a specific name,
-- which you can specify here. Using this lens to set a null name will not result in an error but
-- the update will not occur.
registeredAppName :: Lens' InitConfig Strict.Text
registeredAppName = lens theRegisteredAppName $ \ a -> sanitizeName >>> \ b ->
  if Strict.null b then a else a{ theRegisteredAppName = sanitizeName b }

-- | Most window managers depict a title bar above every window that displays a descriptive name of
-- the content of that window. Specify that name here. Using this lens to set a title name will not
-- result in an error but the update will not occur.
initWindowTitleBar :: Lens' InitConfig Strict.Text
initWindowTitleBar = lens theWindowTitleBar $ \ a -> sanitizeName >>> \ b ->
  if Strict.null b then a else a{ theWindowTitleBar = sanitizeName b }

-- | When the window is created, setting this parameter to a non-'Prelude.Nothing' value determines
-- whether an image buffer in memory is allocated with enough memory for an alpha channel, so that
-- the window background can be made partially or fully transparent, and also determines what the
-- default transparency value is, with 0.0 being fully transparent, and 1.0 being fully
-- opaque. However not all window managers will allow for windows with alpha channels, so this may
-- not work depending on how the GUI of your opreating system has been configured.
initBackgroundTransparency :: Lens' InitConfig (Maybe Double)
initBackgroundTransparency = lens theBackgroundTransparency $ \ a b -> a{ theBackgroundTransparency=b }

-- | The default background "color", actually a gray value since no one in their right mind would
-- choose a non-grey solid color background for their applet. 0 is black, 1 is white, and everything
-- in between 0 and 1 are valid grey values.
initBackgroundGreyValue :: Lens' InitConfig Double
initBackgroundGreyValue = lens theBackgroundGreyValue $ \ a b -> a{ theBackgroundGreyValue=b }

-- | Recommend to the window manager where to place the window. Pass @(0,0)@ to let the window
-- manager choose.
recommendWindowPosition :: Lens' InitConfig (Int, Int)
recommendWindowPosition = lens theRecommendWindowPosition $ \ a b -> a{ theRecommendWindowPosition=b }

-- | Recommend to the window manager how large to make the window. Pass @(0,0)@ to let the
-- window manager choose.
recommendWindowSize :: Lens' InitConfig (Int, Int)
recommendWindowSize = lens theRecommendWindowSize $ \ a b -> a{ theRecommendWindowSize=b }

-- | Recommend the frame rate to use when installing animation event handlers.
animationFrameRate :: Lens' InitConfig Double
animationFrameRate = lens theAnimationFrameRate $ \ a b -> a{ theAnimationFrameRate=b }

-- | For window managers that allow windows to be created without a titlebar, close\/minimize\/hide
-- buttons, or other visible controls (on Linux systems, "reparenting" window managers will do
-- this), setting this value to 'Prelude.False' will tell the window manager to not draw these
-- decorations. Non-reparenting window managers like Xmonad are unable to draw a title bar, in which
-- case this parameter will have no effect, the Happlet window will behave as if this parameter is
-- always set to 'Prelude.True'. Other reparenting window managers may simply ignore this option, in
-- which case the Happlet window will behave as if this parameters is always set to 'Prelude.False'.
initDecorateWindow :: Lens' InitConfig Bool
initDecorateWindow = lens willDecorateWindow $ \ a b -> a{ willDecorateWindow = b }

-- | If the Happlet window is closed, halt the GUI event loop, returning control to the @main@
-- function. If the 'gtkLaunchEventLoop' is the final function called in @main@ the Haskell program
-- will end. If you would like your GUI application to have multiple windows open, this value should
-- be set to 'False'. By default, this value should be set to 'True'.
quitOnWindowClose :: Lens' InitConfig Bool
quitOnWindowClose = lens willQuitOnWindowClose $ \ a b -> a{ willQuitOnWindowClose = b }

-- | If the Happlet window is closed, the image buffer allocated for it should be
-- deleted. Otherwise, the Happlet window is simply made invisible when it is closed, and the image
-- buffer is not deleted. Of course, if 'quitOnWindowClose' is set to 'Prelude.True', the entire
-- program may quit even if this parameter is set to 'Prelude.False'.
deleteWindowOnClose :: Lens' InitConfig Bool
deleteWindowOnClose = lens willDeleteWindowOnClose $ \ a b -> a{ willDeleteWindowOnClose = b }
