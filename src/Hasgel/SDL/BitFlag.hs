-- | This module just declares BitFlag type class.
module Hasgel.SDL.BitFlag (
  BitFlag(..)
) where

import Data.Bits (Bits, (.&.), (.|.))
import Data.Foldable (foldl')
import Data.Maybe (mapMaybe)

-- | Type class for various BitFlag types.
-- | Minimum complete definition is marshalBitFlag and unmarshalBitFlag
class (Bounded f, Enum f) => BitFlag f where
  -- | Convert a BitFlag to numerical representation.
  marshalBitFlag :: (BitFlag f, Num a, Eq a) => f -> a
  -- | Convert from numerical representation to BitFlag.
  unmarshalBitFlag :: (Num a, Eq a, BitFlag f) => a -> Maybe f
  -- | Converts multiple flags to numerical representation using bitwise 'or'.
  createBitFlags :: (BitFlag f, Num a, Bits a) => [f] -> a
  createBitFlags = foldl' (.|.) 0 . fmap marshalBitFlag
  -- | Converts numerical representation of flags back to list of BitFlag.
  fromBitFlags :: (Num a, Bits a, BitFlag f) => a -> [f]
  fromBitFlags = fromBitFlags' [minBound..]

fromBitFlags' :: (Num a, Bits a, BitFlag f) => [f] -> a -> [f]
fromBitFlags' allBitFlags bits = mapMaybe unmarshalBitFlag flags
  where flags = filter (\b -> b == b .&. bits) rawBitFlags
        rawBitFlags = map marshalBitFlag allBitFlags
