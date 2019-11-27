-- | For Happlet back-end developers, it is only necessary to implement the 'RenderText' type class.
--
-- This module provides a minimal extended API for rendering text into a Happlet window as an
-- old-fashioned "dumb terminal" would display fonts, e.g. the DEC VT-100 terminal protocol still --
-- used today on POSIX-compatible systems. The purpose of the Happlets Text API is to provide a
-- means to display a matrix of characters for a variety of command-line like applications, while
-- still providing some of the conveniences of a widget-based GUI application.
-- 
-- The text matrix must display a monospace font, with the assumption that all characters are either
-- half-width or full-width. Ligatures should be disallowed unless the font in question can
-- guarantee that the width of every glyph is an integer multiple of the width of all half-width
-- glyphs.
-- 
-- The minimum API defined in this module provides functionality for the following:
-- 
-- * Set the font size.
-- 
-- * Set bold/italic fonts.
-- 
-- * Set the font foreground and background color.
-- 
-- * Evaluate a canvas monad that draws a character or string to an
--   arbitrary point within an image.
-- 
-- * Obtain a PixCoord rectangle for a text string, even without
--   displaying the string.
-- 
-- * Obtain a relative row/column size for a text string, even without
--   displaying the string.
-- 
-- * Obtain a row/column for a mouse PixCoord.
-- 
-- * Obtain the row/column size of an arbitrary rectangle.
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
--         'textCursor' . 'gridRow' 'Control.Lens..=' 2
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
-- Happlets 'Happlets.Initialize.Provider' to be the same @render@ type as that of the
-- 'Happlets.GUI.HappletWindow' class. In order to display text in a Happlet window, evaluate a
-- 'ScreenPrinter' using the 'screenPrinter' function, which produces a @render@ function. Then use
-- either the 'Happlets.GUI.onCanvas' or 'Happlets.GUI.onOSBuffer' function to evaluate the @render@
-- function to a 'GUI' function that can be evaluate from within any Happlet event handler.
--
-- It is also possible to display text with the 'Happlets.GUI.display' function, however you cannot
-- pass overloaded strings to the 'display' function because Haskell's type inference will not be
-- able to deduce that the type you intend for 'display' to evaluate is of the 'ScreenPrinter' type
-- from a polymorphic string literal alone.
--
-- Characters are placed into the grid, and each character advances a cursor. Characters may be of
-- varying width but the cursor always advances by an integer multiple of the 'UnitGridSize'.
--
-- The 'CursorAdvanceRules' APIs make it possible to define character advance modes for
-- right-to-left (used by Arabic and Persian languages) or top-to-botton settings (used by Chinese
-- and Japanese languages). Even more complicated rules, like for ancient Hebrew, can be
-- defined. The default cursor behavior is 'cursorAdvanceRuleLangC'.
--
-- The functions and data types in this module model a virtual dumb terminal device, all font styles
-- are assumed to be monospace fonts with a limited range of font sizes, although any font face may
-- be configured by the operating system. There are no (and __never will be any__) APIs for working
-- with a variety of font faces, as listing faces is a known source of entropy for identifying and
-- tracking clients over a network. All font configurations must be performed by the operating
-- system, and the Happlet 'Happlets.Initialize.Provider' must always select the default monospace
-- font.
module Happlets.Draw.Text
  ( ScreenPrinter(..),
    screenPrinter, fontStyle, textCursor, renderOffset,
    asGridSize, pixToGridSize,
    displayPrintable, displayChar, displayString, displayStringEvalBoxes,
    gridLocationOfMouse,
    HasTextGridLocation(..),
    -- * Font Styles
    withFontStyle, updateFontStyle,
    FontStyle(..), FontSize, IsUnderlined(..), IsStriken(..), defaultFontStyle, fontName,
    fontForeColor, fontBackColor, fontSize, fontBold, fontItalic, fontUnderline, fontStriken,
    -- * Text Grid Location
    UnitGridSize, TextBoundingBox, withGridLocation, gridBoundingBox,
    TextGridRow(..), rowInt, TextGridColumn(..), columnInt,
    TextGridLocation(..), TextGridSize, textGridLocation,
    sumGridLocation, diffGridLocation, positiveGridLocation,
    -- * Character Printing Machine
    ScreenPrinterState(..), screenPrinterState, runScreenPrinter,
    RenderText(..), spanPrintable, printableChar, PrintableString, unwrapPrintable,
    module Data.Char,
  ) where

import           Happlets.Draw.Color
import           Happlets.Draw.Types2D

import           Control.Arrow      ((>>>), (&&&), first)
import           Control.Lens
import           Control.Monad.State

import           Data.Char
import           Data.String
import qualified Data.Text as Strict

----------------------------------------------------------------------------------------------------

-- | Not all font sizes may be possible, depending on the 'Happlets.Initializ.Provider'. Happlet
-- programmers specify their requested 'FontSize' to the 'theFontSize' field of the 'FontStyle' data
-- structure, the provider will accept any 'FontSize' and set the actual font size nearest to the
-- requested size that is possible, returning the possible value.
type FontSize = Double

-- | A value indicating whether the text should be underlined.
data IsUnderlined = NoUnderline | Underline | DoubleUnderline deriving (Eq, Ord, Show)

-- | A value indicating whether the text should have a strike-through line.
data IsStriken = NotStriken | Striken | DoubleStriken deriving (Eq, Ord, Show)

----------------------------------------------------------------------------------------------------

-- | This is the data structure containing the style information for 'ScreenPrinter' functions. All
-- of these fields are accessible with lenses, so you can perform stateful updates to the
-- 'ScreenPrinter's rendering context using the @('Control.Lens..=')@ family of operators. For
-- example:
--
-- @
-- writeHello :: 'ScreenPrinter' ()
-- writeHello = do
--     'fontForeColor' .= 'white'
--     'fontBackColor' .= 'black'
--     'fontSize'      .= 48.0
--     'fontItalic'    .= 'True'
--     "Hello, world!"               -- Don't forget the "OverloadedStrings" compiler flag.
-- @
--
-- You can also use the 'withFontStyle' function to define a kind of markup, in which a sub
-- 'ScreenPrinter' within a 'ScreenPrinter' that uses modified 'FontStyle' parameters for the
-- evaluation of the sub 'ScreenPrinter' is evaluated.
data FontStyle
  = FontStyle
    { theFontForeColor :: !Color
    , theFontBackColor :: !Color
    , theFontName      :: !Strict.Text
    , theFontSize      :: !FontSize
    , theFontBold      :: !Bool
    , theFontItalic    :: !Bool
    , theFontUnderline :: !IsUnderlined
    , theFontStriken   :: !IsStriken
    }
  deriving (Eq, Show)

-- | This is the default font style set by all 'Happlets.Initialize.Provider's.
defaultFontStyle :: FontStyle
defaultFontStyle = FontStyle
  { theFontForeColor = black
  , theFontBackColor = white
  , theFontName      = "Monospace"
  , theFontSize      = 16.0
  , theFontBold      = False
  , theFontItalic    = False
  , theFontUnderline = NoUnderline
  , theFontStriken   = NotStriken
  }

-- | The requested font name for the built-in text console. It must be a monospaced font. This font
-- will be set only if available, otherwise the system default font will be used. There is no way to
-- programmatically determine whether the requested font has been set or not. This feature is merely
-- a convenience, Happlets implementations are not required to support it.
fontName :: Lens' FontStyle Strict.Text
fontName = lens theFontName $ \ a b -> a{ theFontName = b }

-- | The foreground 'Color' of the font.
fontForeColor     :: Lens' FontStyle Color
fontForeColor = lens theFontForeColor $ \ a b -> a{ theFontForeColor = b }

-- | The background 'Color' of the font.
fontBackColor     :: Lens' FontStyle Color
fontBackColor = lens theFontBackColor $ \ a b -> a{ theFontBackColor = b }

-- | The size of the font.
fontSize      :: Lens' FontStyle FontSize
fontSize = lens theFontSize $ \ a b -> a{ theFontSize = b }

-- | Whether or not the font should be bold -- not all fonts provide bold faces for their default
-- monospace typeface.
fontBold      :: Lens' FontStyle Bool
fontBold = lens theFontBold $ \ a b -> a{ theFontBold = b }

-- | Whether or not the font should be italic -- not all fonts provide italic faces for their
-- default monospace typeface.
fontItalic    :: Lens' FontStyle Bool
fontItalic = lens theFontItalic $ \ a b -> a{ theFontItalic = b }

-- | Whether or not the text should be underlined.
fontUnderline :: Lens' FontStyle IsUnderlined
fontUnderline = lens theFontUnderline $ \ a b -> a{ theFontUnderline = b }

-- | Whether or not the text should be striken-through with a line down the middle.
fontStriken   :: Lens' FontStyle IsStriken
fontStriken = lens theFontStriken $ \ a b -> a{ theFontStriken = b }

------------------------------------------------------------------------------------------------------

-- | Positive 1 indicates the left-most column, negative 1 indicates the right-most column. Zero
-- values indicate the cursor is not visible.
newtype TextGridRow = TextGridRow Int
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Num, Real, Integral)

-- | Positive 1 indicates the top-most column, negative 1 indicates the bottom-most column. Zero
-- values indicate the cursor is not visible.
newtype TextGridColumn = TextGridColumn Int
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Num, Real, Integral)

-- | This is a screen cursor value containing both a 'CursorRow' and a 'CursorColumn'.
data TextGridLocation = TextGridLocation !TextGridRow !TextGridColumn
  deriving (Eq, Ord)

-- | A synonym for 'TextGridColumn', used in contexts where the rectangular area, rather than text
-- location, is the value being used.
type TextGridSize = TextGridLocation

-- | The text grid has a unit size given as a 2-sized vector of floating-point numbers.
type UnitGridSize    = Size2D Double

-- | When printing text to the screen, the Happlet 'Happlet.Initialize.Provider' must return the
-- bounding box of the text.
type TextBoundingBox = Rect2D Double

-- | A class of lenses, allowing for multiple data types to provide 'gridRow' and 'gridColumn'
-- information.
class HasTextGridLocation a where
  gridRow    :: Lens' a TextGridRow
  gridColumn :: Lens' a TextGridColumn

instance HasTextGridLocation TextGridLocation where
  gridRow    = lens (\ (TextGridLocation   a _) -> a)
                    (\ (TextGridLocation _ b) a -> TextGridLocation a b)
  gridColumn = lens (\ (TextGridLocation   _ b) -> b)
                    (\ (TextGridLocation a _) b -> TextGridLocation a b)

-- | Compute the difference between two 'TextGridLocation's. Works just like vector arithmetic.
diffGridLocation :: TextGridLocation -> TextGridLocation -> TextGridLocation
diffGridLocation (TextGridLocation x0 y0) (TextGridLocation x1 y1) =
  TextGridLocation (x0 - x1) (y0 - y1)

sumGridLocation :: TextGridLocation -> TextGridLocation -> TextGridLocation
sumGridLocation (TextGridLocation x0 y0) (TextGridLocation x1 y1) =
  TextGridLocation (x0 + x1) (y0 + y1)

-- | Returns 'True' if both components are positive. This is used to test if a location lies within
-- a 'TextGridSize' bounding box when used with 'diffGridLocation'.
positiveGridLocation :: TextGridLocation -> Bool
positiveGridLocation (TextGridLocation row col) = row >= 0 && col >= 0

-- | The initial 'TextGridLocation' which is set to @(0, 0)@. This value is intended to be used with
-- lenses.
textGridLocation :: TextGridLocation
textGridLocation = TextGridLocation 0 0

-- | A lens that can obtain the integer within the 'TextGridRow', although this is almost never
-- necessary since 'TextGridRow' instantiates the 'Num' class and can be initialized with an integer
-- literal.
rowInt :: Iso' TextGridRow Int
rowInt = iso (\ (TextGridRow i) -> i) TextGridRow

-- | A lens that can obtain the integer within the 'TextGridColumn', although this is almost never
-- necessary since 'TextGridColumn' instantiates the 'Num' class and can be initialized with an
-- integer literal.
columnInt :: Iso' TextGridColumn Int
columnInt = iso (\ (TextGridColumn i) -> i) TextGridColumn

-- | Compute the minimum 'TextGridSize' that contains a 'TextBoundingBox'. This divides the given
-- 'TextBoundingBox' by the 'UnitGridSize' and rounds up (using 'Prelude.ceiling') to the nearest
-- integer value.
gridBoundingBox :: UnitGridSize -> TextBoundingBox -> TextGridSize
gridBoundingBox (V2 uW uH) = rect2DSize >>> \ (V2 tW tH) ->
  TextGridLocation (TextGridRow $ ceiling $ tH / uH) (TextGridColumn $ ceiling $ tW / uW)

----------------------------------------------------------------------------------------------------

-- | Instantiate this class into a monadic @render@ing function by
-- 'Control.Monad.Trans.Class.lift'ing the @render@ function type into the 'ScreenPrinter' type. Use
-- the 'Control.Monad.State.get' or 'Control.Lens.use' functions to obtain the 'fontStyle' and
-- 'gridRow' and 'gridColumn' values, and render these value according to the rules defined in
-- comments in this module.
--
-- The 'ScreenPrinter' function type instantiates 'RenderText' so that you can use all of these
-- functions within the 'ScreenPrinter' without having to use the 'Control.Monad.Trans.Class.lift'
-- function.
class Monad render => RenderText render where
  -- | Preserve a single 'ScreenPrinterState' in the @render@ state. This function is called by the
  -- 'screenPrinter' function to retrieve the state from the @render@ evaluation context.
  setScreenPrinterState :: ScreenPrinterState -> render ()

  -- | Produce a copy of the last 'ScreenPrinterState' that was called with
  -- 'setScreenPrinterState', or return 'screenPrinterState' if 'setScreenPrinterState' was never
  -- called. This function is called by the 'screenPrinter' function to store the screen printer
  -- state with the given @render@ evaluation context.
  getScreenPrinterState :: render ScreenPrinterState

  -- | Set the 'FontStyle' used by the 'ScreenPrinter'. You can pass in any 'FontStyle' value, but
  -- not all 'FontStyle' values may be possible, for example the requested font size might not be
  -- available or a bold-italic monospaced font may not be available. The actual 'FontStyle' value
  -- that is set is returned, which is the nearest possible 'FontStyle' to the requested
  -- 'FontStyle.'
  setRendererFontStyle :: FontStyle -> render FontStyle

  -- | Get the 'FontStyle' that was most recently set by the 'setPrinterFontStyle' function. This is
  -- the 'FontStyle' that was nearest to the requested 'FontStyle' that was possible to set, and the
  -- same value returned by the 'setPrinterFontStyle' function.
  getRendererFontStyle :: render FontStyle

  -- | Obtain 'UnitGridSize' value, which is a rectangle boundary that can contain hold a single
  -- half-width character of the current system font.
  getGridCellSize :: render UnitGridSize

  -- | Get the size of the current window in terms of @(cailing (windowWidth/cellSize), ceiling
  -- (windowHeight/cellSize))@. This means the maximum number of grid cells visible, even a grid
  -- cell has only a single pixel visible in the window, it is counted in the size.
  getWindowTextGridSize :: render TextGridSize

  -- | Render a single character to the window buffer according to the 'ScreenPrinterState',
  -- including the row and column position, without modifying the 'ScreenPrinterState'. This
  -- function may return 'Prelude.Nothing' if the character is non-printable ('Data.Char.isPrint'
  -- evaluates the character as 'Prelude.False').
  screenPrintCharNoAdvance :: Char -> render (Maybe TextBoundingBox)

  -- | This function must render the characters according to the given 'ScreenPrinterState' and blit
  -- them to the display image buffer without modifying the 'ScreenPrinterState'.
  --
  -- The bounding box of the printed 'Prelude.String' is returned, along with the portion of the
  -- input string that was not printed. The returned bounding box need not align with the text grid,
  -- but it is expected that the 'ScreenPrinterState' can be updated with a new 'GridLocation'
  -- according to the returned bounding box.
  --
  -- If the given string is empty, this function shall return 'Prelude.Nothing', along with an empty
  -- string.
  screenPrintNoAdvance :: PrintableString -> render (Maybe TextBoundingBox)

  -- | Get the 'TextBoundingBox' size of a character without changing anything visible on the
  -- display.
  getCharBoundingBox :: Char -> render (Maybe TextBoundingBox)

  -- | Get the 'TextBoundingBox' size of a string without changing anything visible on the display.
  getStringBoundingBox :: PrintableString -> render (Maybe TextBoundingBox)

  -- | Convert a 'Happlets.Types2D.Point2D' to a 'GridTextLocation'. This is useful for computing
  -- the grid locaiton of where a mouse event occurs.
  gridLocationOfPoint :: Point2D Double -> render TextGridLocation

  -- | Convert a 'GridTextLocation' to a 'Happlets.Types2D.Point2D'. This is useful for determining
  -- where in the drawable canvas a text point will be drawn if the cursor is at a particular
  -- 'TextGridLocation'.
  gridLocationToPoint :: TextGridLocation -> render (Point2D Double)

-- | A convenient function to call 'gridLocationOfPoint' using a 'PixCoord' value. This function
-- simply coverts the 'PixCoord' to a @'Point2D' 'Double'@. This function is more useful than
-- 'gridLocationOfPoint' when programming mouse event handlers which receive mouse event locations
-- as 'PixCoords'.
gridLocationOfMouse :: RenderText render => PixCoord -> render TextGridLocation
gridLocationOfMouse = gridLocationOfPoint . fmap realToFrac

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
    { theTextCursor   :: !TextGridLocation
    , theFontStyle    :: !FontStyle
    , theRenderOffset :: !(V2 Double)
    , theTabWidth     :: !TextGridColumn
    , theTabStops     :: [TextGridColumn]
    }

instance HasTextGridLocation ScreenPrinterState where
  gridColumn = textCursor . gridColumn
  gridRow    = textCursor . gridRow

instance RenderText render => RenderText (ScreenPrinter render) where
  getGridCellSize = lift getGridCellSize
  getWindowTextGridSize    = lift getWindowTextGridSize
  screenPrintCharNoAdvance = lift . screenPrintCharNoAdvance
  screenPrintNoAdvance     = lift . screenPrintNoAdvance
  setScreenPrinterState    = lift . setScreenPrinterState
  getScreenPrinterState    = lift getScreenPrinterState
  gridLocationOfPoint      = lift . gridLocationOfPoint
  gridLocationToPoint      = lift . gridLocationToPoint
  getCharBoundingBox       = lift . getCharBoundingBox
  getStringBoundingBox     = lift . getStringBoundingBox
  setRendererFontStyle     = lift . setRendererFontStyle
  getRendererFontStyle     = lift getRendererFontStyle

screenPrinterState :: ScreenPrinterState
screenPrinterState = ScreenPrinterState
  { theTextCursor   = textGridLocation
  , theFontStyle    = defaultFontStyle
  , theRenderOffset = V2 (0.0) (0.0)
  , theTabWidth     = 4
  , theTabStops     = []
  }

-- | The current 'gridRow' and 'gridColumn' of the text cursor.
textCursor :: Lens' ScreenPrinterState TextGridLocation
textCursor = lens theTextCursor $ \ a b -> a{ theTextCursor = b }

-- | The text style set for this printer.
fontStyle :: Lens' ScreenPrinterState FontStyle
fontStyle = lens theFontStyle $ \ a b -> a{ theFontStyle = b }

-- | Determines how far from the left and top of the screen a character will be positioned when it
-- is rendered at the cursor location @('gridRow' 'Control.Lens..=' 1)@ @('gridColumn'
-- 'Cursor.Lens..=' 1)@.
renderOffset :: Lens' ScreenPrinterState (V2 Double)
renderOffset = lens theRenderOffset $ \ a b -> a{ theRenderOffset = b }

-- | This field is used by 'displayString', decides which 'gridColumn' to which the cursor will
-- advance when a tab @\'\\t\'@ character is encountered.
tabStops :: Lens' ScreenPrinterState [TextGridColumn]
tabStops = lens theTabStops $ \ a b -> a{ theTabStops = b }

-- | This field is used by 'displayString', decides which 'gridColumn' to which the cursor will
-- advance when a tab @\'\\t\'@ character is encountered and there are no 'tabStops' set. This
-- 'gridColumn' will be advanced to the nearest integer multiple of this value. A value of 1 treats
-- tabs as being identical to the space character unless there are 'tabStops' set, a value of 0 or
-- less will cause tabs to be ignored unless there are 'tabStops' set.
tabWidth :: Lens' ScreenPrinterState TextGridColumn
tabWidth = lens theTabWidth $ \ a b -> a{ theTabWidth = b }

-- | This type can only be constructed by 'spanPrintable' or 'printableChar', and stores a 'String'
-- that is guaranteed to only contain printable characters i.e. characters that can pass through a
-- 'Data.List.filter' of characters for which 'Data.Char.isPrint' is true.
newtype PrintableString = PrintableString{ unwrapPrintable :: String }
  deriving (Eq, Ord)

-- | This function must be used by "screenPrintStringNoAdvance" to sanitize strings passed to it
-- before rendering. This function drops all @'not' . 'isPrint'@ characters from the 'Prelude.head'
-- of the string, then takes only as many consecutive 'isPrint' characters afterward, dropping any
-- non-printing characters after the first run of printing characters. This means if you have any
-- tabs, newlines, line breaks, or anything that isn't printable in the given 'Prelude.String', only
-- the characters up to those non-printing characters are taken.
spanPrintable :: String -> (PrintableString, String)
spanPrintable = first PrintableString . span isPrint . dropWhile (not . isPrint)

-- | Constructs a 'PrintableString' containing a single 'Char' value only if it is printable, that
-- is, if the 'isPrint' of of the 'Char' value is 'True'. Evaluates to 'Nothing' if the 'Char' is
-- not printable.
printableChar :: Char -> Maybe PrintableString
printableChar c = if isPrint c then Just $ PrintableString [c] else Nothing

-- | Given a 'TextBoundingBox' (a value typically returned by 'displayString' or 'displayChar'),
-- evaluate 'gridCellSize' with 'gridBoundingBox' to obtain a 'TextGridSize' value that bounds the
-- given 'TextBoundingBox'. The value resulting from this function can be used to advance the
-- cursor.
asGridSize :: RenderText render => TextBoundingBox -> render TextGridSize
asGridSize box = flip gridBoundingBox box <$> getGridCellSize

-- | Takes a value of type 'PixSize' and computes the current 'TextGridSize' value that fits within
-- that 'PixSize'. The 'PixSize' value is usually provided by the 'Happlets.GUI.getWindowSize'
-- function, or passed as an argument of type 'Happlets.GUI.NewPixSize' to the redraw continuation
-- function set by 'Happlets.GUI.resizeEvents',
pixToGridSize
  :: RenderText render
  => PixSize -- ^ The size of the GUI window.
  -> render TextGridSize
pixToGridSize winSize = asGridSize $ fromIntegral <$> (rect2D & rect2DHead .~ winSize)

-- | Evaluate the 'Control.Monad.State.Class.MonadState' 'ScreenPrinter'. Use this function within a
-- @do@ block passed to the 'Happlets.GUI.onCanvas' or 'Happlets.GUI.onOSBuffer' function in order
-- to to place text on screen.
screenPrinter :: RenderText render => ScreenPrinter render a -> render a
screenPrinter f = do
  (a, st) <- getScreenPrinterState >>= runScreenPrinter f
  setScreenPrinterState st
  return a

-- | Wrapper around the 'Control.Monad.State.runStateT' function for evaluating a 'ScreenPrinter'
-- function. Usually it is better to use 'screenPrinter', which evaluates this function and
-- automatically retrieving the 'ScreenPrinterState' from the 'getScreenPrinterState' function.
runScreenPrinter :: ScreenPrinter render a -> ScreenPrinterState -> render (a, ScreenPrinterState)
runScreenPrinter = runStateT . unwrapScreenPrinter

-- not for export
displayInput
  :: RenderText render
  => (input -> ScreenPrinter render (Maybe TextBoundingBox))
  -> input -> ScreenPrinter render (Maybe TextBoundingBox)
displayInput display input = do
  st <- get
  setScreenPrinterState st
  display input >>= \ case
    Nothing   -> return Nothing
    Just bnds -> do
      spanSize <- asGridSize bnds
      textCursor . gridColumn += spanSize ^. gridColumn
      return (Just bnds)

-- | After setting the 'fontStyle' with lens operators like @('Control.Lens..=')@, the state is
-- modified but the changed state has not yet been applied. Use this function to apply the updated
-- font style state. Note that the 'withFontStyle' function does this automatically, so it is
-- usually better to use that unless performance is a concern.
updateFontStyle :: RenderText render => ScreenPrinter render FontStyle
updateFontStyle = use fontStyle >>= setRendererFontStyle

-- | This function renders a single character, then advances the 'gridColumn' or 'gridRow'.
displayChar :: RenderText render => Char -> ScreenPrinter render (Maybe TextBoundingBox)
displayChar c = if isPrint c then displayInput screenPrintCharNoAdvance c else pure Nothing

-- | Displays a 'PrintableString'.
--
-- This function works by rendering a string by calling 'screenPrintNoAdvance', and then advancing
-- the cursor according to the size of the 'TextBoundingBox' returned by that function. This
-- function will halt after the first newline character, returning the first newline character and
-- all characters after it that were ignored.
displayPrintable
  :: RenderText render
  => PrintableString
  -> ScreenPrinter render (Maybe TextBoundingBox)
displayPrintable = displayInput screenPrintNoAdvance

-- | Prints a string with line breaks. This function will display strings in a way that is most
-- intuitive to programmers who are familiar with command line interfaces.
--
-- This is a convenience function that breaks a given input 'String' up into 'PrintableString's,
-- increments the ('textCursor' . 'gridColumn') field every time an @\'\\n\'@ character is
-- encountered, sets the ('textCursor' . 'gridRow') to zero every time a @\'\\r\'@ character is
-- encountered, and advances the @('textCursor' . 'gridColumn')@ field to the next 'tabStops'
-- position every time a @\'\\t\'@ character is encountered.
--
-- All other non-printing characters are ignored, so VT-100 color changing codes or cursor
-- positioning codes are not obeyed. (With a bit of help, we would like to provide some simple
-- VT-100 protocol color and cursor positioning functionality in the future.)
displayString :: RenderText render => String -> ScreenPrinter render (Maybe TextBoundingBox)
displayString = displayStringEvalBoxes (const $ return ())

-- | This function is very similar to 'displayString', but it takes a continuation function called
-- the @evalBoxes@ function, a function of type @('TextBoundingBox' -> render ())@, which is
-- evaluated every time a block of text is rendered by 'displayPrintable'. The 'TextBoundingBox'
-- passed to @evalBoxes@ is the value returned by 'displayPrintable'.
--
-- This function allows you more fine-grained control of which parts of the screen might be
-- refreshed. The 'TextBoundingBox' returned by this functionq is the 'mconcat'enation of all of the
-- 'TextBoundingBox'es passed to the 'displayPrintable' function, which is the smallest box that
-- encompases all of those boxes.
displayStringEvalBoxes
  :: RenderText render
  => (TextBoundingBox -> render ()) -- ^ the @evalBoxes@ function
  -> String -> ScreenPrinter render (Maybe TextBoundingBox)
displayStringEvalBoxes withBox str = use tabStops >>= loop Nothing str where
  loop bounding str stops = do
    (prn, str) <- pure $ spanPrintable str
    box <- displayPrintable prn
    maybe (pure ()) (lift . withBox) box
    bounding <- pure $ bounding <> box
    case str of
      ""    -> return bounding
      '\ESC':str -> do
        -- TODO: handle VT-100 escape codes.
        loop bounding str stops
      c:str -> do
        stops <- case c of
          '\n' -> do
            textCursor %= (gridColumn .~ 0) . (gridRow +~ 1)
            use tabStops -- replenish the list of tab stops
          '\t' -> do
            col <- use $ textCursor . gridColumn
            case dropWhile (<= col) stops of
              []      -> do
                w <- use tabWidth
                textCursor . gridColumn += case w of
                  w | w <= 0 -> 0
                  w | w == 1 -> 1
                  w          -> w - mod col w
                return []
              s:stops -> (textCursor . gridColumn .= s) >> return stops
          _    -> return stops
        loop bounding str stops

-- | This function takes a continuation which temporarily changes the font style used by the
-- printer, which acts as a sort-of markup function. The function used to modify the 'fontStyle' is
-- a pure 'Control.Monad.State.State' function, meaning you can make use of the
-- 'Control.Lens.Lens'es like @('Control.Lens..=')@ to set the font style. For example:
--
-- @
-- "This text is printed in the default font style.\\n"
-- 'withFontStyle' (do 'fontForeColor' 'Control.Lens..=' 'Happlets.Draw.Color.blue'; 'fontBold' 'Control.Lens..=' True) $ do
--     "This text is bold and blue.\\n"
-- "This text is printed in the default font style.\\n"
-- @
--
-- This function is usually makes use of 'setPrinterFontStyle' and 'getPrinterFontStyle', but it
-- tends to be much more convenient to use this function instead.
withFontStyle
  :: RenderText render
  => State FontStyle () -> ScreenPrinter render a -> ScreenPrinter render a
withFontStyle changeFont print = do
  oldStyle <- lift getRendererFontStyle
  lift $ setRendererFontStyle $ execState changeFont oldStyle
  print <* lift (setRendererFontStyle oldStyle)

-- | This function takes a continuation and a function which temporarily changes the location of the
-- cursor, then evaluates the continuation, then restores the prior cursor position, then returns
-- the result of the continuation's evaluation.
withGridLocation
  :: RenderText render
  => (TextGridLocation -> TextGridLocation)
  -> ScreenPrinter render a -> ScreenPrinter render a
withGridLocation move f =
  state ((^. textCursor) &&& (textCursor %~ move)) >>= (f <*) . (textCursor .=)
