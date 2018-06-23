-- | This module provides a basic color data type that can be used to build apps without needing to
-- include additional packages into your project. This module is not imported automatically when you
-- import the "Happlets" module, so if you would prefer to use a better alternative color data type
-- it is easier to do so. This module can be imported with the "Happlets.Draw" module.
module Happlets.Draw.Color
  ( PackedRGBA32, FillColor, LineColor,
    get32BitsRGBA, set32BitsRGBA, word32RGBA,
    get32BitsARGB, set32BitsARGB, word32ARGB,
    unpackRGBA32Color, packRGBA32Color,
    packRGBA32, unpackRGBA32,
    packRGBA32Root2, unpackRGBA32Pow2,
    quantizedColor, quantizeColorChannel, unquantizeColorChannel,
    -- * British Spellings
    FillColour, LineColour,
    unpackRGBA32Colour, packRGBA32Colour, quantizeColourChannel, unquantizeColourChannel,
    -- * Fundamental Colors and Shades,
    red, maroon, lime, green, blue, navy,
    cyan, aqua, teal, magenta, fuchsia, purple, yellow, olive,
    chartreuse, violet, orange, rose, spring, azure,
    white, black, grey, gray, light, dark,
    -- * Color Channels
    chanR, chanG, chanB, alphaChannel,
  ) where

import           Control.Arrow
import           Control.Lens

import           Data.Bits
import           Data.Char
import           Data.Word

import           Numeric

----------------------------------------------------------------------------------------------------

-- | For using a type name that describes how the 'PackedRGBA32' value is being used for.
type FillColor = PackedRGBA32

-- | For using a type name that describes how the 'PackedRGBA32' value is being used for.
type LineColor = PackedRGBA32

newtype PackedRGBA32 = PackedRGBA32 Word32
  deriving Eq

instance Show PackedRGBA32 where
  showsPrec _ (PackedRGBA32 w) = (++)
    ( case w of
        w | w <= 0xF -> "#0000000"
        w | w <= 0xFF -> "#000000"
        w | w <= 0xFFF -> "#00000"
        w | w <= 0xFFFF -> "#0000"
        w | w <= 0xFFFFF -> "#000"
        w | w <= 0xFFFFFF -> "#00"
        w | w <= 0xFFFFFFF -> "#0"
        _                   -> "#"
    ) . showHex w

instance Read PackedRGBA32 where
  readsPrec _ str = case str of
    '#' : str -> do
      (a, str) <- readHex str
      let done str = (PackedRGBA32 a, str)
      case str of
        ""                -> [done ""]
        s:str | isSpace s -> [done $ dropWhile isSpace str]
        _                 -> []
    _         -> []

-- | This function evaluates 'unpackRGBA32' and then performs a 'unquantizeColorChannel'
-- computation on each channel, including the alpha channel.
unpackRGBA32Color :: PackedRGBA32 -> (Double, Double, Double, Double)
unpackRGBA32Color p =
  let (r,g,b,a) = unpackRGBA32 p
      un = unquantizeColorChannel
  in (un r, un g, un b, un a)

-- | This function evaluates 'qunatizeColorChannel' on each given 'Prelude.Double'-precision color
-- channel value, then evaluates 'packRGBA32' on the four resulting quantized values.
packRGBA32Color :: Double -> Double -> Double -> Double -> PackedRGBA32
packRGBA32Color r g b a = packRGBA32 (q r) (q g) (q b) (q a) where
  q = quantizeColorChannel

-- | This function performs no transformation on the given 'Data.Word.Word8' values, they are simply
-- bit-packed into a 'Data.Word.Word32' data value by bit-shifting and the bitwise-OR operator.
packRGBA32 :: Word8 -> Word8 -> Word8 -> Word8 -> PackedRGBA32
packRGBA32 r g b a = PackedRGBA32 $! sh r 24 .|. sh g 16 .|. sh b 8 .|. sh a 0 where
  sh c s = shift (fromIntegral c) s

-- | Perform an integral square root on each component value and then call 'packRGBA32'.
packRGBA32Root2 :: Word16 -> Word16 -> Word16 -> Word16 -> PackedRGBA32
packRGBA32Root2 r g b a = packRGBA32 (root r) (root g) (root b) (root a) where
  root = round . sqrt . (realToFrac :: Word16 -> Float)

-- | This is the inverse operation of 'packRGBA32'.
unpackRGBA32 :: PackedRGBA32 -> (Word8, Word8, Word8, Word8)
unpackRGBA32 (PackedRGBA32 w) = (unsh 24, unsh 16, unsh 8, unsh 0) where
  unsh s = fromIntegral $! 0x000000FF .&. shift w (negate s)

-- | Similar to 'unpackRGBA32', but returns 'Data.Word.Word16' values, and raises each returned
-- value to the power of 2.
unpackRGBA32Pow2 :: PackedRGBA32 -> (Word16, Word16, Word16, Word16)
unpackRGBA32Pow2 = unpackRGBA32 >>> \ (r, g, b, a) -> (un r, un g, un b, un a) where
  un = fromIntegral >>> \ x -> x * x

-- | This function takes a linear color value expressed as a 'Prelude-Double'-precision
-- floating-point value, clamps it to a value between 0 and 1, then computes the square-root on this
-- value, then compresses the square-root to a 'Data.Word.Word8' value (because the human eye is
-- more sensitive to changes in brigher colors than in darker colors). Use this function to convert
-- a linear double-precision color channel intensity value to a value that can be stored efficiently
-- in memory or some other digital computing medium.
quantizeColorChannel :: Double -> Word8
quantizeColorChannel =
  round . (* (realToFrac (maxBound::Word8) :: Double)) . sqrt . min 1.0 . max 0.0

-- | This is the inverse operation of 'quantizeColorChannel', the 'Data.Word.Word8' value is
-- converted to a 'Prelude.Double'-precision floating point value between the numbers 0 and 1, then
-- returns the square of this value.
unquantizeColorChannel :: Word8 -> Double
unquantizeColorChannel = (** 2.0) . (/ (realToFrac (maxBound :: Word8) :: Double)) . realToFrac

-- | An 'Control.Lens.Iso'morphism for 'quantizeColorChannel' and 'unquantizeColorChannel'.
quantizedColor :: Iso' Double Word8
quantizedColor = iso quantizeColorChannel unquantizeColorChannel

-- | Obtain the bits as a 'Data.Word.Word32' value formatted with each octet in the
-- Red-Green-Blue-Alpha format (in that order, from biggest to littlest significant bits). This is
-- similar to 'get32BitsARGB' but the resultant 'Data.Word.Word32' value is bitwise rotated -8
-- steps.
get32BitsRGBA :: PackedRGBA32 -> Word32
get32BitsRGBA (PackedRGBA32 w) = w

-- | The inverse operation of 'packed32BitsRGBA'.
set32BitsRGBA :: Word32 -> PackedRGBA32
set32BitsRGBA = PackedRGBA32

-- | An 'Control.Lens.Iso'morphism wrapping 'get32BitsRGBA' and 'set32BitsRGBA'.
word32RGBA :: Iso' PackedRGBA32 Word32
word32RGBA = iso get32BitsRGBA set32BitsRGBA

-- | Obtain the bits as a 'Data.Word.Word32' value formatted with each octet in the
-- Alpha-Red-Green-Blue format (in that order, from biggest to littlest significant bit). This is
-- similar to 'get32BitsRGBA' but the resultant 'Data.Word.Word32' value is bitwise rotated +8
-- steps.
get32BitsARGB :: PackedRGBA32 -> Word32
get32BitsARGB (PackedRGBA32 w) = rotate w (-8)

-- | The inverse operation of 'get32BitsARGB'.
set32BitsARGB :: Word32 -> PackedRGBA32
set32BitsARGB = PackedRGBA32 . flip rotate 8

-- | An 'Control.Lens.Iso'morphism wrapping 'get32BitsARGB' and 'set32BitsRGBA'.
word32ARGB :: Iso' PackedRGBA32 Word32
word32ARGB = iso get32BitsARGB set32BitsARGB

----------------------------------------------------------------------------------------------------

-- | British spelling of 'FillColor'
type FillColour = FillColor

-- | British spelling of 'LineColor'
type LineColour = LineColor

-- | British spelling of 'unpackRGBA32Color'
unpackRGBA32Colour :: PackedRGBA32 -> (Double, Double, Double, Double)
unpackRGBA32Colour = unpackRGBA32Color

-- | British spelling of 'packRGBA32Color'
packRGBA32Colour :: Double -> Double -> Double -> Double -> PackedRGBA32
packRGBA32Colour = packRGBA32Color

-- | British spelling of 'quantizeColorChannel'
quantizeColourChannel :: Double -> Word8
quantizeColourChannel = quantizeColorChannel

-- | British spelling of 'unquantizeColorChannel'
unquantizeColourChannel :: Word8 -> Double
unquantizeColourChannel = unquantizeColorChannel

----------------------------------------------------------------------------------------------------

nameColor :: Word32 -> PackedRGBA32
nameColor w = PackedRGBA32 $ rotate w 8 .|. 0x0000FF

-- | #FF0000
red :: PackedRGBA32
red = nameColor 0xFF0000

-- | #7F0000
maroon :: PackedRGBA32
maroon = nameColor 0x7F0000

-- | #00FF00 
lime :: PackedRGBA32
lime = nameColor 0x00FF00

-- | #007F00
green :: PackedRGBA32
green = nameColor 0x007F00

-- | #0000FF
blue :: PackedRGBA32
blue = nameColor 0x0000FF

-- | #00007F
navy :: PackedRGBA32
navy = nameColor 0x00007F

-- | #00FFFF (same as 'aqua')
cyan :: PackedRGBA32
cyan = nameColor 0x00FFFF

-- | #00FFFF (same as 'cyan')
aqua :: PackedRGBA32
aqua = cyan

-- | #FF00FF (same as 'fuchsia')
magenta :: PackedRGBA32
magenta = nameColor 0xFF00FF

-- | #FF00FF (same as 'magenta')
fuchsia :: PackedRGBA32
fuchsia = magenta

-- | #FFFF00
yellow :: PackedRGBA32
yellow = nameColor 0xFFFF00

-- | #7FFF00FF
chartreuse :: PackedRGBA32
chartreuse = nameColor 0x7FFF00FF

-- | #FF7F00FF
orange :: PackedRGBA32
orange = nameColor 0xFF7F00FF

-- | #7F00FF
violet :: PackedRGBA32
violet = nameColor 0x7F00FF

-- | #FF007F
rose :: PackedRGBA32
rose = nameColor 0xFF007F

-- | #007FFF
azure :: PackedRGBA32
azure = nameColor 0x007FFF

-- | #00FF7F
spring :: PackedRGBA32
spring = nameColor 0x00FF7F

-- | #007F7F
teal :: PackedRGBA32
teal = nameColor 0x007F7F

-- | #7F7F00
purple :: PackedRGBA32
purple = nameColor 0x7F7F00

-- | #7F7F00
olive :: PackedRGBA32
olive = nameColor 0x7F7F00

-- | #FFFFFF
white :: PackedRGBA32
white = nameColor 0xFFFFFF

-- | #000000
black :: PackedRGBA32
black = nameColor 0x000000

-- | #7F7F7F (same as 'grey')
gray :: PackedRGBA32
gray = nameColor 0x7F7F7F

-- | #7F7F7F (same as 'gray')
grey :: PackedRGBA32
grey = grey

----------------------------------------------------------------------------------------------------

bitShiftColorChannelLens :: Int -> Lens' PackedRGBA32 Double
bitShiftColorChannelLens i = lens
  (\ (PackedRGBA32 c)   ->
     unquantizeColorChannel $ fromIntegral $ shift (shift 0xFF i .&. c) (negate i)
  )
  (\ (PackedRGBA32 c) a -> PackedRGBA32 $ (c .&. xor 0xFFFFFFFF (shift 0xFF i)) .|.
    shift (fromIntegral $ quantizeColorChannel a) i
  )

-- | A lens to change the linear (unquantized) alpha channel value without needing to unpack then
-- repack the 'PackedRGBA32' value.
alphaChannel :: Lens' PackedRGBA32 Double
alphaChannel = bitShiftColorChannelLens 0

-- | A lens to change the linear (unquantized) red channel value without needing to unpack then
-- repack the 'PackedRGBA32' value.
chanR :: Lens' PackedRGBA32 Double
chanR = bitShiftColorChannelLens 24

-- | A lens to change the linear (unquantized) green channel value without needing to unpack then
-- repack the 'PackedRGBA32' value.
chanG :: Lens' PackedRGBA32 Double
chanG = bitShiftColorChannelLens 16

-- | A lens to change the linear (unquantized) blue channel value without needing to unpack then
-- repack the 'PackedRGBA32' value.
chanB :: Lens' PackedRGBA32 Double
chanB = bitShiftColorChannelLens 8

-- | Shift the color toward white. For example @('light' 'red')@ may look like a "pink"
-- color. Please be aware that this is not necessarily the inverse function of 'dark' due to loss of
-- precision when packing the color channels.
light :: Double -> PackedRGBA32 -> PackedRGBA32
light = min 1.0 . max 0.0 >>> \ p ->
  unpackRGBA32Color >>> \ (r,g,b,a) ->
  packRGBA32Color (p * (1 - r) + r) (p * (1 - g) + g) (p * (1 - b) + b) a

-- | Shift the color toward black. For example @('dark' 'blue')@ may look like a "navy" color.
-- Please be aware that this is not necessarily the inverse function of 'light' due to loss of
-- precision when packing the color channels.
dark :: Double -> PackedRGBA32 -> PackedRGBA32
dark = min 1.0 . max 0.0 >>> \ p ->
  unpackRGBA32Color >>> \ (r,g,b,a) ->
  packRGBA32Color (p * r) (p * g) (p * b) a
