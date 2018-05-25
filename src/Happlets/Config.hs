-- | Methods and data types for working with the configuration. The Happlets configuration was
-- originally designed to be loaded from an INI file, but this functionality has since been moved
-- into a separate package. However many of the primitives for loading a configuration file from
-- disk are still provided here so that they are available universally to all Happlets back-end
-- providers.
module Happlets.Config
  ( Config(..),
    ConfigException(..), StrictFilePath,
    sanitizeName, configErrorsOnLoad, clearConfigErrors, configFilePath,
    registeredAppName, windowTitleBar, backgroundTransparency, backgroundGreyValue,
    recommendWindowPosition, recommendWindowSize, animationFrameRate,
    deleteWindowOnClose, quitOnWindowClose, decorateWindow,
  )
  where

import           Control.Arrow
import           Control.Exception
import           Control.Lens

import           Data.Char (isPrint)
import           Data.List (groupBy)
import qualified Data.Text as Strict

----------------------------------------------------------------------------------------------------

type StrictFilePath = Strict.Text

data ConfigException
  = ConfigErrorIO           StrictFilePath IOException
  | ConfigErrorIniParseFail StrictFilePath Strict.Text
  | ConfigErrorIniStructure StrictFilePath [Strict.Text]
  | ConfigError             Strict.Text
  | ConfigMultipleErrors    Strict.Text [ConfigException]
  deriving (Eq, Show)

instance Exception ConfigException where {}

----------------------------------------------------------------------------------------------------

-- | This is the data structure used to configure various parameters in the Happlet back-end
-- provider.
data Config
  = Config
    { theConfigErrorsOnLoad      :: [ConfigException]
    , theConfigFilePath          :: StrictFilePath
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
  deriving (Eq, Show)

-- | A string sanitizer for applied to strings assigned to 'registeredAppName' and 'windowTitleBar'.
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

-- | This is a list of errors that occurred while trying to load this configuration from the
-- filesystem.
configErrorsOnLoad :: Config -> [ConfigException]
configErrorsOnLoad = theConfigErrorsOnLoad

-- | The configuration errors are stored into the 'Config' data structure. This function erases the
-- errors.
clearConfigErrors :: Config -> Config
clearConfigErrors c = c{ theConfigErrorsOnLoad = [] }

-- | Reports the file path from which this config file was loaded, or a null string if this has not
-- been set.
configFilePath :: Config -> StrictFilePath
configFilePath = theConfigFilePath

-- | Gtk+ apps can register your GHCi/Happlet process with the window manager under a specific name,
-- which you can specify here. Using this lens to set a null name will not result in an error but
-- the update will not occur.
registeredAppName :: Lens' Config Strict.Text
registeredAppName = lens theRegisteredAppName $ \ b -> sanitizeName >>> \ a ->
  if Strict.null a then b else b{ theRegisteredAppName = sanitizeName a }

-- | Most window managers depict a title bar above every window that displays a descriptive name of
-- the content of that window. Specify that name here. Using this lens to set a title name will not
-- result in an error but the update will not occur.
windowTitleBar :: Lens' Config Strict.Text
windowTitleBar = lens theWindowTitleBar $ \ b -> sanitizeName >>> \ a ->
  if Strict.null a then b else b{ theWindowTitleBar = sanitizeName a }

-- | When the window is created, setting this parameter to a non-'Prelude.Nothing' value determines
-- whether an image buffer in memory is allocated with enough memory for an alpha channel, so that
-- the window background can be made partially or fully transparent, and also determines what the
-- default transparency value is, with 0.0 being fully transparent, and 1.0 being fully
-- opaque. However not all window managers will allow for windows with alpha channels, so this may
-- not work depending on how the GUI of your opreating system has been configured.
backgroundTransparency :: Lens' Config (Maybe Double)
backgroundTransparency = lens theBackgroundTransparency $ \ b a -> b{ theBackgroundTransparency=a }

-- | The default background "color", actually a gray value since no one in their right mind would
-- choose a non-grey solid color background for their applet. 0 is black, 1 is white, and everything
-- in between 0 and 1 are valid grey values.
backgroundGreyValue :: Lens' Config Double
backgroundGreyValue = lens theBackgroundGreyValue $ \ b a -> b{ theBackgroundGreyValue=a }

-- | Recommend to the window manager where to place the window. Pass @(0,0)@ to let the window
-- manager choose.
recommendWindowPosition :: Lens' Config (Int, Int)
recommendWindowPosition = lens theRecommendWindowPosition $ \ b a -> b{ theRecommendWindowPosition=a }

-- | Recommend to the window manager how large to make the window. Pass @(0,0)@ to let the
-- window manager choose.
recommendWindowSize :: Lens' Config (Int, Int)
recommendWindowSize = lens theRecommendWindowSize $ \ b a -> b{ theRecommendWindowSize=a }

-- | Recommend the frame rate to use when installing animation event handlers.
animationFrameRate :: Lens' Config Double
animationFrameRate = lens theAnimationFrameRate $ \ b a -> b{ theAnimationFrameRate=a }

-- | For window managers that allow windows to be created without a titlebar, close\/minimize\/hide
-- buttons, or other visible controls (on Linux systems, "reparenting" window managers will do
-- this), setting this value to 'Prelude.False' will tell the window manager to not draw these
-- decorations. Non-reparenting window managers like Xmonad are unable to draw a title bar, in which
-- case this parameter will have no effect, the Happlet window will behave as if this parameter is
-- always set to 'Prelude.True'. Other reparenting window managers may simply ignore this option, in
-- which case the Happlet window will behave as if this parameters is always set to 'Prelude.False'.
decorateWindow :: Lens' Config Bool
decorateWindow = lens willDecorateWindow $ \ b a -> b{ willDecorateWindow = a }

-- | If the Happlet window is closed, halt the Gtk+ event loop, returning control to the @main@
-- function. If the 'gtkLaunchEventLoop' is the final function called in @main@ the Haskell program
-- will end.
quitOnWindowClose :: Lens' Config Bool
quitOnWindowClose = lens willQuitOnWindowClose $ \ b a -> b{ willQuitOnWindowClose = a }

-- | If the Happlet window is closed, the image buffer allocated for it should be
-- deleted. Otherwise, the Happlet window is simply made invisible when it is closed, and the image
-- buffer is not deleted. Of course, if 'quitOnWindowClose' is set to 'Prelude.True', the entire
-- program may quit even if this parameter is set to 'Prelude.False'.
deleteWindowOnClose :: Lens' Config Bool
deleteWindowOnClose = lens willDeleteWindowOnClose $ \ b a -> b{ willDeleteWindowOnClose = a }
