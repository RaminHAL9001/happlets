-- | This module provides a basic color data type that can be used to build apps without needing to
-- include additional packages into your project. This module is not imported automatically when you
-- import the "Happlets" module, so if you would prefer to use a better alternative color data type
-- it is easier to do so. This module can be imported with the "Happlets.Draw" module.
module Happlets.Draw.Color
  ( PackedRGBA32(..), FillColor, LineColor,
    unpackRGBA32Color, packRGBA32Color,
    packRGBA32, unpackRGBA32,
    packRGBA32Root2, unpackRGBA32Pow2,
    quantizeColorChannel, unquantizeColorChannel,
    -- * British Spellings
    FillColour, LineColour,
    unpackRGBA32Colour, packRGBA32Colour, quantizeColourChannel, unquantizeColourChannel,
    -- * Fundamental Colors and Shades,
    red, green, blue, cyan, magenta, yellow, white, black, grey, gray, light, dark,
    -- * Color Channels
    redChannel, greenChannel, blueChannel, alphaChannel,
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

newtype PackedRGBA32 = PackedRGBA32{ unwrapPackedRGBA32 :: Word32 }
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

-- |
red :: PackedRGBA32
red = PackedRGBA32 0xFF0000FF

-- | 
green :: PackedRGBA32
green = PackedRGBA32 0x00FF00FF

-- |
blue :: PackedRGBA32
blue = PackedRGBA32 0x0000FFFF

-- |
cyan :: PackedRGBA32
cyan = PackedRGBA32 0x00FFFFFF

-- |
magenta :: PackedRGBA32
magenta = PackedRGBA32 0xFF00FFFF

-- |
yellow :: PackedRGBA32
yellow = PackedRGBA32 0xFFFF00FF

-- |
white :: PackedRGBA32
white = PackedRGBA32 0xFFFFFFFF

-- |
black :: PackedRGBA32
black = PackedRGBA32 0x000000FF

-- |
grey :: PackedRGBA32
grey = PackedRGBA32 0x808080FF

-- |
gray :: PackedRGBA32
gray = grey

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
redChannel :: Lens' PackedRGBA32 Double
redChannel = bitShiftColorChannelLens 24

-- | A lens to change the linear (unquantized) green channel value without needing to unpack then
-- repack the 'PackedRGBA32' value.
greenChannel :: Lens' PackedRGBA32 Double
greenChannel = bitShiftColorChannelLens 16

-- | A lens to change the linear (unquantized) blue channel value without needing to unpack then
-- repack the 'PackedRGBA32' value.
blueChannel :: Lens' PackedRGBA32 Double
blueChannel = bitShiftColorChannelLens 8

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
