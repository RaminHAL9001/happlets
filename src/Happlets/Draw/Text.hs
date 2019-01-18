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
    asGridSize, displayChar, displayString, gridLocationOfMouse,
    HasTextGridLocation(..),
    -- * Font Styles
    withFontStyle, updateFontStyle,
    FontStyle(..), FontSize, IsUnderlined(..), IsStriken(..), defaultFontStyle,
    fontForeColor, fontBackColor, fontSize, fontBold, fontItalic, fontUnderline, fontStriken,
    -- * Text Grid Location
    UnitGridSize, TextBoundingBox, gridBoundingBox,
    TextGridRow(..), rowInt, TextGridColumn(..), columnInt,
    TextGridLocation(..), TextGridSize, textGridLocation,
    -- * Cursor Advance Mechanism
    CursorAdvanceRules,
    cursorAdvanceRules, defaultCursorAdvanceRules, cursorAdvanceRuleLangC, cursorCharRuleLangDOS,
    -- * Character Printing Machine
    ScreenPrinterState(..), screenPrinterState, runScreenPrinter,
    CursorAdvanceInput(..),
    RenderText(..), spanPrintable,
    module Data.Char,
  ) where

import           Happlets.Draw.Color
import           Happlets.Draw.Types2D

import           Control.Category      ((>>>))
import           Control.Lens
import           Control.Monad.State

import           Data.Char
import           Data.String

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
  , theFontSize      = 16.0
  , theFontBold      = False
  , theFontItalic    = False
  , theFontUnderline = NoUnderline
  , theFontStriken   = NotStriken
  }

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
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Num)

-- | Positive 1 indicates the top-most column, negative 1 indicates the bottom-most column. Zero
-- values indicate the cursor is not visible.
newtype TextGridColumn = TextGridColumn Int
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Num)

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
  saveScreenPrinterState :: ScreenPrinterState -> render ()

  -- | Produce a copy of the last 'ScreenPrinterState' that was called with
  -- 'saveScreenPrinterState', or return 'screenPrinterState' if 'saveScreenPrinterState' was never
  -- called. This function is called by the 'screenPrinter' function to store the screen printer
  -- state with the given @render@ evaluation context.
  recallSavedScreenPrinterState :: render ScreenPrinterState

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

  -- | This function __must__ pass the given 'Prelude.String' to the 'takePrintable' function and
  -- then render the characters according to the given 'ScreenPrinterState' without modifying the
  -- 'ScreenPrinterState'. The bounding box of the printed 'Prelude.String' is returned. The
  -- returned bounding box need not align with the text grid, but it is expected that the
  -- 'ScreenPrinterState' can be updated with a new 'GridLocation' according to the returned
  -- bounding box.
  --
  -- If the given string is empty, this function shall return 'Prelude.Nothing'.
  screenPrintNoAdvance :: String -> render (Maybe TextBoundingBox)

  -- | Get the 'TextBoundingBox' size of a character without changing anything visible on the
  -- display.
  getCharBoundingBox :: Char -> render (Maybe TextBoundingBox)

  -- | Get the 'TextBoundingBox' size of a string without changing anything visible on the display.
  getStringBoundingBox :: String -> render (Maybe TextBoundingBox)

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
    { theTextCursor         :: !TextGridLocation
    , theFontStyle   :: !FontStyle
    , theRenderOffset       :: !(V2 Double)
    , theCursorAdvanceRules :: CursorAdvanceRules
    }

-- | This function type is used by 'CursorAdvanceRules'.
data CursorAdvanceInput
  = CursorAdvanceChar   Char
  | CursorAdvanceString String
  deriving (Eq, Ord, Show)

-- | This function takes either a 'Prelude.String' or a 'Prelude.Char', along with a 'TextGridSize'
-- which approximates the bounding box for the given 'Prelude.String' or 'Prelude.Char'. This
-- function must then compute how to advance the text cursor based on these two inputs. Functions of
-- this type can be constructed for any possible use, and can be modifed at any time using the
-- 'cursorAdvanceRules' lens.
type CursorAdvanceRules = CursorAdvanceInput -> TextGridSize -> TextGridLocation -> TextGridLocation

instance HasTextGridLocation ScreenPrinterState where
  gridColumn = textCursor . gridColumn
  gridRow    = textCursor . gridRow

instance RenderText render => RenderText (ScreenPrinter render) where
  getGridCellSize = lift getGridCellSize
  getWindowTextGridSize         = lift getWindowTextGridSize
  screenPrintCharNoAdvance      = lift . screenPrintCharNoAdvance
  screenPrintNoAdvance          = lift . screenPrintNoAdvance
  saveScreenPrinterState        = lift . saveScreenPrinterState
  recallSavedScreenPrinterState = lift recallSavedScreenPrinterState
  gridLocationOfPoint           = lift . gridLocationOfPoint
  gridLocationToPoint           = lift . gridLocationToPoint
  getCharBoundingBox            = lift . getCharBoundingBox
  getStringBoundingBox          = lift . getStringBoundingBox
  setRendererFontStyle          = lift . setRendererFontStyle
  getRendererFontStyle          = lift getRendererFontStyle

-- | The 'defaultCursorAdvanceRules' are set to 'cursorAdvanceRuleLangC'.
defaultCursorAdvanceRules :: CursorAdvanceRules
defaultCursorAdvanceRules = cursorAdvanceRuleLangC

-- | This is the default text cursor behavior. If the given character is @('\\LF')@, the
-- 'gridColumn' is set to 0 and the 'gridRow' is incremented. For all other characters, the
-- 'Data.Char.WCWidth.wcwidth' function is used to obtain a character width value, and if the
-- character width value is @(-1)@ then the cursor is not advanced, otherwise the cursor is advanced
-- by the character width value.
--
-- The name of this function is similar to the UNIX locale environment variable setting @LANG=C@, as
-- in the locale is set to the C programming language, rather than the language for a specific
cursorAdvanceRuleLangC :: CursorAdvanceRules
cursorAdvanceRuleLangC input (TextGridLocation row col) = case input of
  CursorAdvanceChar   c -> case c of
    '\n'                  -> (gridRow +~ row) . (gridColumn .~ 0)
    c | not (isPrint c)   -> id
    _                     -> gridColumn +~ col
  CursorAdvanceString{} -> gridColumn +~ col

-- | Similar to 'cursorCharLangC' with one difference: the new line character @('\\LF')@ simply
-- increments the 'cursorRow' but does not set the 'cursorColumn' to 0, while the carriage return
-- character @('\\CR')@ sets the 'cursorColumn' to 1 but does not increment the 'cursorRow',
-- therefore both the carriage return and line feed @('\\CR\\LF')@ are both necessary to send the
-- cursor to the next line.
cursorCharRuleLangDOS :: CursorAdvanceRules
cursorCharRuleLangDOS input (TextGridLocation _ col) = case input of
  CursorAdvanceChar   c -> case c of
    '\n'                  -> gridRow    +~ 1
    '\r'                  -> gridColumn .~ 0
    c | not (isPrint c)   -> id
    _                     -> gridColumn +~ col
  CursorAdvanceString{} -> gridColumn +~ col

screenPrinterState :: ScreenPrinterState
screenPrinterState = ScreenPrinterState
  { theTextCursor         = textGridLocation
  , theFontStyle   = defaultFontStyle
  , theRenderOffset       = V2 (0.0) (0.0)
  , theCursorAdvanceRules = defaultCursorAdvanceRules
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

-- | This is a function used to make decisions on how to move the cursor around as characters and
-- strings are printed. The default behavior is 'defaultCursorAdvanceRules', but you can make your
-- own rules (as long as the rules are expressed as a function of the type 'CursorAdvanceRules'),
-- and change the rules at any time within any function that instantiates the 'TextRender' type class.
-- You change the rules using this very lens.
--
-- One example of how you may want to change the cursor advance rules would be if you are
-- implementing a Chinese/Japanese language display where the characters are printed in columns from
-- top to bottom, and columns process from the right of the screen to the left. This behavior can be
-- defined as a rule and switched with this lens before calling 'displayString' on a string
-- containing many Chinese language characters. You can then switch back to the default rules using:
-- @'cursorAdvanceRules' 'Control.Lens..=' 'defaultCursorAdvanceRules'@.
cursorAdvanceRules :: Lens' ScreenPrinterState CursorAdvanceRules
cursorAdvanceRules = lens theCursorAdvanceRules $ \ a b -> a{ theCursorAdvanceRules = b }

-- | This function must be used by "screenPrintStringNoAdvance" to sanitize strings passed to it
-- before rendering. This function drops all @'not' . 'isPrint'@ characters from the 'Prelude.head'
-- of the string, then takes only as many consecutive 'isPrint' characters afterward, dropping any
-- non-printing characters after the first run of printing characters. This means if you have any
-- tabs, newlines, line breaks, or anything that isn't printable in the given 'Prelude.String', only
-- the characters up to those non-printing characters are taken.
spanPrintable :: String -> (String, String)
spanPrintable = span isPrint . dropWhile (not . isPrint)

-- | Given a 'TextBoundingBox' (a value typically returned by 'displayString' or 'displayChar'),
-- evaluate 'gridCellSize' with 'gridBoundingBox' to obtain a 'TextGridSize' value that bounds the
-- given 'TextBoundingBox'. The value resulting from this function can be used to advance the
-- cursor.
asGridSize :: RenderText render => TextBoundingBox -> render TextGridSize
asGridSize box = flip gridBoundingBox box <$> getGridCellSize

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

-- not for export
displayInput
  :: RenderText render
  => (input -> CursorAdvanceInput)
  -> (input -> ScreenPrinter render (Maybe TextBoundingBox))
  -> input -> ScreenPrinter render (Maybe TextBoundingBox)
displayInput constr display input = display input >>= \ case
  Nothing   -> return Nothing
  Just bnds -> do
    grid <- asGridSize bnds
    st   <- lift recallSavedScreenPrinterState
    textCursor %= theCursorAdvanceRules st (constr input) grid
    return $ Just bnds

-- | After setting the 'fontStyle' with lens operators like @('Control.Lens..=')@, the state is
-- modified but the changed state has not yet been applied. Use this function to apply the updated
-- font style state. Note that the 'withFontStyle' function does this automatically, so it is
-- usually better to use that unless performance is a concern.
updateFontStyle :: RenderText render => ScreenPrinter render FontStyle
updateFontStyle = use fontStyle >>= setRendererFontStyle

-- | This function renders a single character, then advances the 'gridColumn' or 'gridRow'.
displayChar :: RenderText render => Char -> ScreenPrinter render (Maybe TextBoundingBox)
displayChar = displayInput CursorAdvanceChar screenPrintCharNoAdvance

-- | Render a string by calling 'displayChar' repeatedly.
displayString :: RenderText render => String -> ScreenPrinter render (Maybe TextBoundingBox)
displayString = displayInput CursorAdvanceString screenPrintNoAdvance

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
