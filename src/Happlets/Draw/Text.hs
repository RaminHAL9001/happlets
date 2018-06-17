-- | This module provides an extended API for rendering text into a Happlet window.
module Happlets.Draw.Text
  ( SelectFont, FontExtents(..), TextExtents2D(..), HappletsDrawText(..),
  ) where

import           Happlets.Draw.Types2D
import           Happlets.Draw.Color

import           Control.Monad.IO.Class

import qualified Data.Text as Strict

----------------------------------------------------------------------------------------------------

-- | Fonts should be selected with descriptive strings like "Monospace" or "Arial Italic" or
-- "Helvetica Bold". Font sizes are passed as a 'Happlets.Draw.Types2D.RealApprox' parameter.
type SelectFont = Strict.Text

-- | Contains information about how to align a font in a window. If you construct a command line
-- interface with a monospace font, it should be possible to use the 'fontExtentsXadvance' and
-- 'fontExtentsYadvance' to compute which text character in the window is under the mouse if a mouse
-- coordinate is given.
data FontExtents font
  = FontExtents
    { fontExtentsIdentifier  :: font
    , fontExtentsAscent      :: RealApprox
    , fontExtentsDescent     :: RealApprox
    , fontExtentsHeight      :: RealApprox
    , fontExtentsMaxXadvance :: RealApprox
    , fontExtentsMaxYadvance :: RealApprox
    }
  deriving (Eq, Ord, Show, Read)

-- | A data structure containing informaion on rendered text.
data TextExtents2D
  = TextExtents2D
    { textFontSelector :: !SelectFont
      -- ^ A valid font name.
    , textStartPoint   :: !(Point2D RealApprox)
      -- ^ The point at which to draw the text in order for the 'textBoundingBox' below is to be
      -- realized.
    , textBoundingBox  :: !(Rect2D RealApprox)
      -- ^ The minimum bounding box that contains the visual representation of the text.
    , textToDraw       :: !Strict.Text
    }
  deriving Eq

-- | This class must instantiate 'Control.Monad.Monad' and 'Control.Monad.IO.Class.MonadIO' because
-- it is expected to perform stateful updates to the system, namely changing what graphics are
-- displayed in a Happlet window.
--
-- The @render@ data type instantiated here is expected to be the the same as the @render@ type
-- instantiated into the 'Happlets.GUI.HappletWindow' type class, which should allow Happlet
-- programmers to evaluate all of these functions using the 'Happlets.GUI.onCanvas' function and
-- perform the expected update on the Happlet window.
class (Functor render, Applicative render, Monad render, MonadIO render, Show font)
   => HappletsDrawText render font | render -> font where
  -- | Obtain a list of fonts available to this @canvas@.
  listFonts :: render [font]

  -- | Select a font using a string descriptor of the font. If the selection is successful, return
  -- the new 'FontExtents' information. If the font could not be selected, returns
  -- 'Prelude.Nothing'.
  selectFont :: SelectFont -> render (Maybe (FontExtents font))

  -- | Obtain general information about the selected font.
  fontExtents :: render (FontExtents font)

  -- | Compute the 'TextExtents2D' data structure for this canvas for a given font and text.
  textExtents :: Point2D RealApprox -> Strict.Text -> render TextExtents2D

  -- | Draw text on the canvas. The 'LineColor' selects the text color, and the 'FillColor' selects
  -- the background color.
  drawText :: FillColor -> LineColor -> Point2D RealApprox -> Strict.Text -> render TextExtents2D
