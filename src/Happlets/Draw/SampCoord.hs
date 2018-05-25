-- | This is a primitive data type for representing screen sizes and points on a screen in discrete
-- using 'Data.Int.Int32' values.
module Happlets.Draw.SampCoord where

import           Data.Int

import           Linear.V2

----------------------------------------------------------------------------------------------------

-- | A sample coordinate: this is a value that is used to count some integral number of units along
-- a 1-dimensional axis of some scan line, for example pixels along the edge of a view screen or
-- canvas. Alone, these elements can represent the amount of time (measured in the units of time
-- samples) of an audio signal. A 'Linear.V2.V2' pair's of 'PixCoords' can specify a 2D point on a
-- screen or canvas, 'Linear.V3.V3' triples of 'PixCoords' can specify a 3D point in a voxel space.
newtype SampCoord = SampCoord Int32
  deriving (Eq, Ord, Show, Read, Enum, Num, Integral, Real)

-- | A pair of 'SampCoord' indicating the location of a pixel in a raster image.
type PixCoord = V2 SampCoord

-- | A pair of 'SampCoord' values indicating the size of a rectangle of pixels.
type PixSize = V2 SampCoord

sampCoord :: Integral i => i -> SampCoord
sampCoord = SampCoord . fromIntegral

-- | This is a simple type tag used to indicate the difference between a source and target image,
-- for operations like blitting.
type Source image = image

-- | This is a simple type tag used to indicate the difference between a source and target image,
-- for operations like blitting.
type Target image = image
