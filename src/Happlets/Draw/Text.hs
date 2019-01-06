-- | For Happlet back-end developers, it is only necessary to implement the 'RenderText' type class.
--
-- This module provides a minimal extended API for rendering text into a Happlet window as an
-- old-fashioned "dumb terminal" would display fonts, e.g. the DEC VT-100 terminal protocol still
-- used today on modern Linux systems.
--
-- The most important function type is the 'ScreenPrinter'. Construct a 'ScreenPrinter' from a
-- string using 'displayString' or 'displayChar'. You can also modify the font style using the
-- 'fontStyle' function. If you have set the @-XOverloadedStrings@ Haskell compiler flag, simply
-- writing a string literal will construct a 'ScreenPrinter' by way of the 'Data.String.fromString'
-- function. For example:
--
-- @
-- 'Happlets.GUI.onCanvas' $ do
--     'screenPrinter' $ do
--         'textCursor' . 'gridRow'
--         'displayString' "This is some text printed in the default font.\\n"
--         'fontStyle' (do 'fontForeColor' 'Control.Lens..=' 'Happlets.Draw.Color.blue'; 'fontBold' 'Control.Lens..=' 'Prelude.True';) $ do
--             "This text is written with a bold blue font of the default size.\\n"
--             "Using -XOverloadedStrings, you don't need to use \''displayString'\' at all.\\n"
--             'displayString' "Or you can use it for clarity.\\n"
--         "This text is back to the default font.\\n"
--
-- 'Happlets.GUI.display' $ do
--     'displayString' "The \''ScreenPrinter'\' type instantiates the \''display'\' function.\\n"
-- @
--
-- The 'ScreenPrinter' function type lifts a @render@ function type that /should/ be defined by the
-- Happlets back-end provider to be the same @render@ type as that of the
-- 'Happlets.GUI.HappletWindow' class. In order to display text in a Happlet window, evaluate a
-- 'ScreenPrinter' using the 'screenPrinter' function, which produces a @render@ function. Then
-- use either the 'Happlets.GUI.onCanvas' or 'Happlets.GUI.onOSBuffer' function to evaluate the
-- @render@ function to a 'GUI' function that can be evaluate from within any Happlet event handler.
--
-- It is also possible to display text with the 'Happlets.GUI.display' function, however you cannot
-- pass overloaded strings to the 'display' function because Haskell's type inference will not be
-- able to deduce that the type you intend for 'display' to evaluate is of the 'ScreenPrinter' type
-- from a polymorphic string literal alone.
--
-- The functions and data types in this module model a virtual dumb terminal device, all font styles
-- are assumed to be monospace fonts with a limited range of font sizes. The font sizes are integer
-- multiples of a minimum text grid size. The way this virtual device basically works is that the
-- visible window contains a layer of text that is always on top of the graphics layer. The window
-- is sub-divided into rows and columns, and the cursor (by default) advances from left to right,
-- and on a carriage return advnaces downward. The text grid increases and decreases with the window
-- size, but the cells of the text grid are always a value of 03 by 03.
--
-- Characters are placed into the grid, and each character advances a cursor. Characters can be
-- half-width which advances the column count by 1 times the 'fontSizeMultiplier' or full-width,
-- which advances the column count by 2 times the 'fontWidthMultiplier'. When advancing rows, the
-- default behavior is to advance downward by 2 times the 'fontWidthMultiplier'.
--
-- It is also possible to set character advance modes for right-to-left (used by Arabic and Persian
-- languages) or top-to-botton settings (used by Chinese and Japanese languages). Even more
-- complicated rules, like for ancient Hebrew, can be defined.
--
-- The default text mode is 'normalSize' black text on white
module Happlets.Draw.Text
  ( ScreenPrinter(..), screenPrinter, fontStyle,
    textCursor, cursorCharRule, renderOffset,
    HasTextGridLocation(..),
    -- * Font Styles
    FontStyle(..), FontSize, IsUnderlined(..), IsStriken(..), defaultFontStyle,
    fontForeColor, fontBackColor, fontSize, fontBold, fontItalic, fontUnderline, fontStriken,
    -- * Cursor Advance Mechanism
    CursorCharRule, cursorCharRuleLangC, cursorCharRuleLangDOS,
    -- * Text Grid Location
    TextGridRow(..), rowInt, TextGridColumn(..), columnInt,
    TextGridLocation(..), TextGridSize, textGridLocation,
    -- * Character Printing Machine
    RenderText(..), displayChar, displayString,
    ScreenPrinterState(..), screenPrinterState, runScreenPrinter,
    module Data.Char,
    module Data.Char.WCWidth
  ) where

import           Happlets.Draw.Color
import           Happlets.Draw.Types2D

import           Control.Applicative
import           Control.Lens
import           Control.Monad.State

import           Data.Char
import           Data.Char.WCWidth
import           Data.String

----------------------------------------------------------------------------------------------------

-- | Not all font sizes may be possible, depending on the 'Happlets.Initializ.Provider'. Happlet
-- programmers specify their requested 'FontSize' to the 'theFontSize' field of the 'FontStyle' data
-- structure, the provider will accept any 'FontSize' and set the actual font size nearest to the
-- requested size that is possible, returning the possible value.
type FontSize = Float

-- | A value indicating whether the text should be underlined.
data IsUnderlined = NoUnderline | Underline | DoubleUnderline deriving (Eq, Ord, Show)

-- | A value indicating whether the text should have a strike-through line.
data IsStriken = NotStriken | Striken | DoubleStriken deriving (Eq, Ord, Show)

----------------------------------------------------------------------------------------------------

data FontStyle
  = FontStyle
    { theFontForeColor :: !Color
    , theFontBackColor :: !Color
    , theFontSize      :: !FontSize
    , theFontBold      :: !Bool
    , theFontItalic    :: !Bool
    , theFontUnderline :: !IsUnderlined
    , theFontStriken   :: !IsStriken
    }
  deriving Eq

defaultFontStyle :: FontStyle
defaultFontStyle = FontStyle
  { theFontForeColor = black
  , theFontBackColor = white
  , theFontSize      = 16.0
  , theFontBold      = False
  , theFontItalic    = False
  , theFontUnderline = NoUnderline
  , theFontStriken   = NotStriken
  }

fontForeColor     :: Lens' FontStyle Color
fontForeColor = lens theFontForeColor $ \ a b -> a{ theFontForeColor = b }

fontBackColor     :: Lens' FontStyle Color
fontBackColor = lens theFontBackColor $ \ a b -> a{ theFontBackColor = b }

fontSize      :: Lens' FontStyle FontSize
fontSize = lens theFontSize $ \ a b -> a{ theFontSize = b }

fontBold      :: Lens' FontStyle Bool
fontBold = lens theFontBold $ \ a b -> a{ theFontBold = b }

fontItalic    :: Lens' FontStyle Bool
fontItalic = lens theFontItalic $ \ a b -> a{ theFontItalic = b }

fontUnderline :: Lens' FontStyle IsUnderlined
fontUnderline = lens theFontUnderline $ \ a b -> a{ theFontUnderline = b }

fontStriken   :: Lens' FontStyle IsStriken
fontStriken = lens theFontStriken $ \ a b -> a{ theFontStriken = b }

----------------------------------------------------------------------------------------------------

-- | Positive 1 indicates the left-most column, negative 1 indicates the right-most column. Zero
-- values indicate the cursor is not visible.
newtype TextGridRow = TextGridRow Int
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Num)

-- | Positive 1 indicates the top-most column, negative 1 indicates the bottom-most column. Zero
-- values indicate the cursor is not visible.
newtype TextGridColumn = TextGridColumn Int
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Num)

-- | A synonym for 'TextGridColumn', used in contexts where the rectangular area, rather than text
-- location, is the value being used.
type TextGridSize = TextGridLocation

-- | This is a screen cursor value containing both a 'CursorRow' and a 'CursorColumn'.
data TextGridLocation = TextGridLocation !TextGridRow !TextGridColumn
  deriving (Eq, Ord)

class HasTextGridLocation a where
  gridRow    :: Lens' a TextGridRow
  gridColumn :: Lens' a TextGridColumn

instance HasTextGridLocation TextGridLocation where
  gridRow    = lens (\ (TextGridLocation   a _) -> a)
                    (\ (TextGridLocation _ b) a -> TextGridLocation a b)
  gridColumn = lens (\ (TextGridLocation   _ b) -> b)
                    (\ (TextGridLocation a _) b -> TextGridLocation a b)

textGridLocation :: TextGridLocation
textGridLocation = TextGridLocation 0 0

rowInt :: Iso' TextGridRow Int
rowInt = iso (\ (TextGridRow i) -> i) TextGridRow

columnInt :: Iso' TextGridColumn Int
columnInt = iso (\ (TextGridColumn i) -> i) TextGridColumn

----------------------------------------------------------------------------------------------------

-- | This is a data type used to maintain the current state of the 'ScreenPrinter' function
-- type. You can access the elements from within a 'ScreenPrinter' function using the associated
-- lenes. For example, to set the font to be bold:
--
-- @
-- do  'printerFontStyle' . 'fontBold' .= True
-- @
--
-- To get the current row and column:
--
-- @
-- do  row <- use 'gridRow'
--     col <- use 'gridColumn'
--     
-- @
data ScreenPrinterState
  = ScreenPrinterState
    { theTextCursor       :: !TextGridLocation
    , theRenderOffset     :: !(V2 Double)
    , theCursorCharRule   :: CursorCharRule
    }

screenPrinterState :: ScreenPrinterState
screenPrinterState = ScreenPrinterState
  { theTextCursor       = textGridLocation
  , theRenderOffset     = V2 (0.0) (0.0)
  , theCursorCharRule   = cursorCharRuleLangC
  }

-- | The current 'gridRow' and 'gridColumn' of the text cursor.
textCursor :: Lens' ScreenPrinterState TextGridLocation
textCursor = lens theTextCursor $ \ a b -> a{ theTextCursor = b }

instance HasTextGridLocation ScreenPrinterState where
  gridColumn = textCursor . gridColumn
  gridRow    = textCursor . gridRow

-- | Determines how far from the left and top of the screen a character will be positioned when it
-- is rendered at the cursor location @('gridRow' 'Control.Lens..=' 1)@ @('gridColumn'
-- 'Cursor.Lens..=' 1)@.
renderOffset :: Lens' ScreenPrinterState (V2 Double)
renderOffset = lens theRenderOffset $ \ a b -> a{ theRenderOffset = b }

-- | This function is called to advance the cursor every time the 'displayChar' function is called.
cursorCharRule :: Lens' ScreenPrinterState CursorCharRule
cursorCharRule = lens theCursorCharRule $ \ a b -> a{ theCursorCharRule = b }

----------------------------------------------------------------------------------------------------

-- | This is a function that inspects a single character and then updates the 'ScreenPrinterState'
type CursorCharRule = ScreenPrinterState -> Char -> TextGridLocation -> TextGridLocation

-- not for export
cursorCharWCWidth :: CursorCharRule
cursorCharWCWidth _st c = execState $ do
  let w = wcwidth c
  unless (w <= 0) $ gridColumn += TextGridColumn w

-- | This is the default text cursor behavior. If the given character is @('\\LF')@, the
-- 'gridColumn' is set to 1 and the 'gridRow' is incremented. For all other characters, the
-- 'Data.Char.WCWidth.wcwidth' function is used to obtain a character width value, and if the
-- character width value is @(-1)@ then the cursor is not advanced, otherwise the cursor is advanced
-- by the character width value.
--
-- The name of this function is similar to the UNIX locale environment variable setting @LANG=C@, as
-- in the locale is set to the C programming language, rather than the language for a specific
-- country, meaning C (the language of the computer) is the default locale.
cursorCharRuleLangC :: CursorCharRule
cursorCharRuleLangC st c = execState $ case c of
  '\n'-> gridRow += TextGridRow 2 >> gridColumn .= 0
  c   -> modify $ cursorCharWCWidth st c

-- | Similar to 'cursorCharLangC' with one difference: the new line character @('\\LF')@ simply
-- increments the 'cursorRow' but does not set the 'cursorColumn' to 1, while the carriage return
-- character @('\\CR')@ sets the 'cursorColumn' to 1 but does not increment the 'cursorRow',
-- therefore both the carriage return and line feed @('\\CR\\LF')@ are both necessary to send the
-- cursor to the next line.
cursorCharRuleLangDOS :: CursorCharRule
cursorCharRuleLangDOS st c = case c of
  '\n' -> gridRow +~ 1
  '\r' -> gridColumn .~ 0
  c    -> cursorCharWCWidth st c

----------------------------------------------------------------------------------------------------

newtype ScreenPrinter render a
  = ScreenPrinter { unwrapScreenPrinter :: StateT ScreenPrinterState render a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance Monad render => MonadState ScreenPrinterState (ScreenPrinter render) where
  state = ScreenPrinter . state

instance MonadTrans ScreenPrinter where
  lift = ScreenPrinter . lift

instance RenderText render => IsString (ScreenPrinter render ()) where
  fromString = void . displayString

-- | Instantiate this class into a monadic @render@ing function by
-- 'Control.Monad.Trans.Class.lift'ing the @render@ function type into the 'ScreenPrinter' type. Use
-- the 'Control.Monad.State.get' or 'Control.Lens.use' functions to obtain the 'fontStyle' and
-- 'gridRow' and 'gridColumn' values, and render these value according to the rules defined in
-- comments in this module.
class Monad render => RenderText render where
  getGridCellSize :: render (Size2D SampCoord)

  -- | Get the size of the current window in terms of
  -- @(windowWidth\/cellSize, windowHeight\/cellSize)@ 
  getWindowTextGridSize :: render TextGridSize

  -- | Render a single character to the window buffer according to the 'ScreenPrinterState',
  -- including the row and column position.
  screenPrintCharNoAdvance  :: ScreenPrinterState -> Char -> render (Rect2D SampCoord)

  -- | Preserve a single 'ScreenPrinterState' in the @render@ state. This function is called by the
  -- 'screenPrinter' function to retrieve the state from the @render@ evaluation context.
  saveScreenPrinterState :: ScreenPrinterState -> render ()

  -- | Produce a copy of the laste 'ScreenPrinterState' that was called with
  -- 'saveScreenPrinterState', or return 'screenPrinterState' if 'saveScreenPrinterState' was never
  -- called. This function is called by the 'screenPrinter' function to store the screen printer
  -- state with the given @render@ evaluation context.
  recallSavedScreenPrinterState :: render ScreenPrinterState

  -- | Convert a 'GridTextLocation' to a 'Happlets.Types2D.Point2D'. 
  gridLocationOfPoint :: Point2D SampCoord -> render TextGridLocation

  -- | Get the pixel size of a character given the current 'fontStyle'. This value is entirely
  -- dependent on just three values: the result of 'getGridSizeOfWindow' and the current 'FontSize',
  -- and the width of the character returned by 'Data.Char.WCWidth.wcwidth'.
  getPixSizeOfChar :: FontSize -> Char -> render (Size2D SampCoord)

  -- | Set the 'FontStyle' used by the 'ScreenPrinter'. You can pass in any 'FontStyle' value, but
  -- not all 'FontStyle' values may be possible, for example the requested font size might not be
  -- available or a bold-italic monospaced font may not be available. The actual 'FontStyle' value
  -- that is set is returned, which is the nearest possible 'FontStyle' to the requested
  -- 'FontStyle.'
  setPrinterFontStyle :: FontStyle -> ScreenPrinter render FontStyle

  -- | Get the 'FontStyle' that was most recently set by the 'setPrinterFontStyle' function. This is
  -- the 'FontStyle' that was nearest to the requested 'FontStyle' that was possible to set, and the
  -- same value returned by the 'setPrinterFontStyle' function.
  getPrinterFontStyle :: ScreenPrinter render FontStyle

-- | Evaluate the 'Control.Monad.State.Class.MonadState' 'ScreenPrinter'. Use this function within a
-- @do@ block passed to the 'Happlets.GUI.onCanvas' or 'Happlets.GUI.onOSBuffer' function in order
-- to to place text on screen.
screenPrinter :: RenderText render => ScreenPrinter render a -> render a
screenPrinter f = do
  (a, st) <- recallSavedScreenPrinterState >>= runScreenPrinter f
  saveScreenPrinterState st
  return a

-- | Wrapper around the 'Control.Monad.State.runStateT' function for evaluating a 'ScreenPrinter'
-- function.
runScreenPrinter :: ScreenPrinter render a -> ScreenPrinterState -> render (a, ScreenPrinterState)
runScreenPrinter = runStateT . unwrapScreenPrinter

-- | This function renders a single character, then advances the 'gridColumn' or 'gridRow'.
displayChar :: RenderText render => Char -> ScreenPrinter render (Rect2D SampCoord)
displayChar c = get >>= \ st -> 
  (lift $ screenPrintCharNoAdvance st c) <*
  (textCursor %= theCursorCharRule st st c)

-- | Render a string by calling 'displayChar' repeatedly.
displayString :: RenderText render => String -> ScreenPrinter render (Maybe (Rect2D SampCoord))
displayString = fmap (foldl f Nothing . fmap Just) . mapM displayChar where
  f a b = rect2DMinBoundOf <$> a <*> b <|> a <|> b

-- | This function takes a function which temporarily changes the font style used by the
-- printer. The function used to modify the 'printerFontStyle' is a pure 'Control.Monad.State.State'
-- function, meaning you can make use of the 'Control.Lens.Lens'es like @('Control.Lens..=')@ to set
-- the font style. For example:
--
-- @
-- "This text is printed in the default font style.\\n"
-- 'fontStyle' (do 'fontForeColor' 'Control.Lens..=' 'Happlets.Draw.Color.blue'; 'fontBold' 'Control.Lens..=' True) $ do
--     "This text is bold and blue.\\n"
-- "This text is printed in the default font style.\\n"
-- @
--
-- This function is usually makes use of 'setPrinterFontStyle' and 'getPrinterFontStyle', but it
-- tends to be much more convenient to use this function instead.
fontStyle
  :: RenderText render
  => State FontStyle () -> ScreenPrinter render a -> ScreenPrinter render a
fontStyle changeFont print = do
  oldStyle <- getPrinterFontStyle
  setPrinterFontStyle $ execState changeFont oldStyle
  print <* setPrinterFontStyle oldStyle
