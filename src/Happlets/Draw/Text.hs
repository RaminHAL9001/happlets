-- | For Happlet back-end developers, it is only necessary to implement the 'RenderText' type class.
--
-- This module provides a minimal extended API for rendering text into a Happlet window as an
-- old-fashioned "dumb terminal" would display fonts, e.g. the DEC VT-100 terminal protocol still
-- used today on modern Linux systems. The basic idea is that the visible window contains a layer of
-- text that is always on top of the graphics layer. The window is sub-divided into rows and
-- columns, and the cursor (by default) advances from left to right, and on a carriage return
-- advnaces downward. The text grid increases and decreases with the window size, but the cells of
-- the text grid are always a value of 03 by 03.
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
  (-- * Character Printing Machine
    RenderText(..), printChar, printString,
    ScreenPrinter(..), ScreenPrinterState(..), mkScreenPrinter, runScreenPrinter,
    printStyle, gridColumn, gridRow, cursorCharRule, renderOffset,
     -- * Font Styles
    FontStyle(..), mkFontStyle, fontColor, fontSize, fontBold, fontItalic, fontUnderline, fontStriken,
    FontSize(..), normalSize, doubleSize, smallSize, mediumSize, largestSize, fontSizeMultiple,
    IsUnderlined(..), IsStriken(..),
    -- * Text Grid Location
    TextGridRow(..), rowInt, TextGridColumn(..), columnInt,
    TextGridLocation(..), mkTextGridLocation,
    -- * Cursor Advance Mechanism
    CursorCharRule, cursorCharRuleLangC, cursorCharRuleLangDOS,
    module Data.Char,
    module Data.Char.WCWidth
  ) where

import           Happlets.Draw.Color

import           Control.Lens
import           Control.Monad.State

import           Data.Char
import           Data.Char.WCWidth

import           Linear.V2

----------------------------------------------------------------------------------------------------

-- | Support a limited set of font sizes. The numbers 6, 12, 16, 18, and 30 do not necessarily
-- indicate the pixel height of the font used. The Happlets back-end provider needs to provide a
-- single default monospace font and choose reasonable default sizes to correspond to these
-- 'FontSize' values.
data FontSize = FontSize06 | FontSize12 | FontSize18 | FontSize24 | FontSize30
  deriving (Eq, Ord, Show, Bounded)

-- | Font widths are defined as multiples of 3, starting with 6. This function returns the multiple,
-- so 'FontSize06' returns 1, 'fontSize30' returns 5.
fontSizeMultiple :: FontSize -> Int
fontSizeMultiple = \ case
  FontSize06 -> 1
  FontSize12 -> 2
  FontSize18 -> 3
  FontSize24 -> 4
  FontSize30 -> 5

-- | Synonym for 'FontSize06'
smallSize :: FontSize
smallSize = FontSize06

-- | Synonym for 'FontSize12'
normalSize :: FontSize
normalSize = FontSize12

-- | Synonym for 'FontSize18'
mediumSize :: FontSize
mediumSize = FontSize18

-- | Synonym for 'FontSize24'
doubleSize :: FontSize
doubleSize = FontSize24

-- | Synonym for 'FontSize30'
largestSize :: FontSize
largestSize = FontSize30

----------------------------------------------------------------------------------------------------

-- | A value indicating whether the text should be underlined.
data IsUnderlined = NoUnderline | Underline | DoubleUnderline deriving (Eq, Ord, Show)

-- | A value indicating whether the text should have a strike-through line.
data IsStriken = NotStriken | Striken | DoubleStriken deriving (Eq, Ord, Show)

----------------------------------------------------------------------------------------------------

data FontStyle
  = FontStyle
    { theFontColor     :: !PackedRGBA32
    , theFontSize      :: !FontSize
    , theFontBold      :: !Bool
    , theFontItalic    :: !Bool
    , theFontUnderline :: !IsUnderlined
    , theFontStriken   :: !IsStriken
    }

mkFontStyle :: FontStyle
mkFontStyle = FontStyle
  { theFontColor     = black
  , theFontSize      = normalSize
  , theFontBold      = False
  , theFontItalic    = False
  , theFontUnderline = NoUnderline
  , theFontStriken   = NotStriken
  }

fontColor     :: Lens' FontStyle PackedRGBA32
fontColor = lens theFontColor $ \ a b -> a{ theFontColor = b }

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

mkTextGridLocation :: TextGridLocation
mkTextGridLocation = TextGridLocation 1 1

rowInt    :: Iso' TextGridRow Int
rowInt    = iso (\ (TextGridRow i) -> i) TextGridRow

columnInt :: Iso' TextGridColumn Int
columnInt = iso (\ (TextGridColumn i) -> i) TextGridColumn

----------------------------------------------------------------------------------------------------

-- | This is a data type used to maintain the current state of the 'ScreenPrinter' function
-- type. You can access the elements from within a 'ScreenPrinter' function using the associated
-- lenes. For example, to set the font to be bold:
--
-- @
-- do  'printStyle' . 'fontBold' .= True
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
    { thePrintStyle     :: !FontStyle
    , theTextCursor     :: !TextGridLocation
    , theRenderOffset   :: !(V2 Double)
    , theCursorCharRule :: CursorCharRule
    }

mkScreenPrinter :: ScreenPrinterState
mkScreenPrinter = ScreenPrinterState
  { thePrintStyle     = mkFontStyle
  , theTextCursor     = mkTextGridLocation
  , theRenderOffset   = V2 (0.0) (0.0)
  , theCursorCharRule = cursorCharRuleLangC
  }

-- | The current font style.
printStyle    :: Lens' ScreenPrinterState FontStyle
printStyle    = lens thePrintStyle $ \ a b -> a{ thePrintStyle = b }

-- | The current 'gridRow' and 'gridColumn' of the text cursor.
textCursor    :: Lens' ScreenPrinterState TextGridLocation
textCursor    = lens theTextCursor $ \ a b -> a{ theTextCursor = b }

instance HasTextGridLocation ScreenPrinterState where
  gridColumn = textCursor . gridColumn
  gridRow    = textCursor . gridRow

-- | Determines how far from the left and top of the screen a character will be positioned when it
-- is rendered at the cursor location @('gridRow' 'Control.Lens..=' 1)@ @('gridColumn'
-- 'Cursor.Lens..=' 1)@.
renderOffset   :: Lens' ScreenPrinterState (V2 Double)
renderOffset   = lens theRenderOffset $ \ a b -> a{ theRenderOffset = b }

-- | This function is called to advance the cursor every time the 'printChar' function is called.
cursorCharRule :: Lens' ScreenPrinterState CursorCharRule
cursorCharRule = lens theCursorCharRule $ \ a b -> a{ theCursorCharRule = b }

----------------------------------------------------------------------------------------------------

-- | This is a function that inspects a single character and then updates the 'ScreenPrinterState'
type CursorCharRule = Char -> TextGridLocation -> TextGridLocation

-- not for export
cursorCharWCWidth :: CursorCharRule
cursorCharWCWidth c = let w = wcwidth c in if w <= 0 then id else
  gridColumn %~ (+ (TextGridColumn w))

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
cursorCharRuleLangC c = case c of
  '\n'-> (gridRow %~ (+ 1)) . (gridColumn .~ 1)
  c   -> cursorCharWCWidth c

-- | Similar to 'cursorCharLangC' with one difference: the new linr character @('\\LF')@ simply
-- increments the 'cursorRow' but does not set the 'cursorColumn' to 1, while the carriage return
-- character @('\\CR')@ sets the 'cursorColumn' to 1 but does not increment the 'cursorRow',
-- therefore both the carriage return and line feed @('\\CR\\LF')@ are both necessary to send the
-- cursor to the next line.
cursorCharRuleLangDOS :: CursorCharRule
cursorCharRuleLangDOS c = case c of
  '\n' -> gridRow %~ (+ 1)
  '\r' -> gridColumn .~ 1
  c    -> cursorCharWCWidth c

----------------------------------------------------------------------------------------------------

newtype ScreenPrinter render a
  = ScreenPrinter { unwrapScreenPrinter :: StateT ScreenPrinterState render a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance Monad render => MonadState ScreenPrinterState (ScreenPrinter render) where
  state = ScreenPrinter . state

instance MonadTrans ScreenPrinter where
  lift = ScreenPrinter . lift

-- | Instantiate this class into a monadic @render@ing function by
-- 'Control.Monad.Trans.Class.lift'ing the @render@ function type into the 'ScreenPrinter' type. Use
-- the 'Control.Monad.State.get' or 'Control.Lens.use' functions to obtain the 'fontStyle' and
-- 'gridRow' and 'gridColumn' values, and render these value according to the rules defined in
-- comments in this module.
class Monad render => RenderText render where
  -- | A grid cell is equal to the width of the 'smallSize'd monospaced font for an ASCII
  -- character. Monospaced means the width of an @w@ character is the same as the width of an @i@
  -- character. It is also assumed that the height of a monospace font is always double its width.
  -- If (for example) your @render@ function type is defined such that the 'FontSize06' is indeed a
  -- font with a height of 6 pixels (thought it doesn't necessarily have to be 6 pixels), the value
  -- returned by 'getGridCellSize' should be @3.0@, because the height of the 'smallSize'd font is 6
  -- and the width is half the height. This function always returns the same value, regardless of
  -- the 'FontSize' currently set in the 'ScreenPrinterState'.
  getGridCellSize :: render Double

  -- | Get the size of the current window in terms of rows and columns. Always round up if the
  -- window size extends to include a partial row or column.
  getGridSizeOfWindow :: render TextGridLocation

  -- | Get the pixel size of a character given the current 'fontStyle'.
  getPixSizeOfChar :: ScreenPrinterState -> Char -> render (V2 Double)

  -- | Render a single character to the window buffer according to the 'ScreenPrinterState',
  -- including the row and column position.
  screenPrintCharNoAdvance  :: ScreenPrinterState -> Char -> render ()

runScreenPrinter :: ScreenPrinter render a -> ScreenPrinterState -> render (a, ScreenPrinterState)
runScreenPrinter = runStateT . unwrapScreenPrinter

-- | This function renders a single character, then advances the 'gridColumn' or 'gridRow'.
printChar :: RenderText render => Char -> ScreenPrinter render ()
printChar c = do
  st <- get
  lift $ screenPrintCharNoAdvance st c
  gets theCursorCharRule >>= modifying textCursor . ($ c)

-- | Render a string by repeatedly calling 'printChar' repeatedly.
printString :: RenderText render => String -> ScreenPrinter render ()
printString = mapM_ printChar
