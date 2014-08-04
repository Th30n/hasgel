-- | This module just declares BitFlag type class.
module Hasgel.SDL.BitFlag (
  BitFlag(..)
) where

import Data.Bits (Bits, (.|.), (.&.))
import Data.List (foldl')
import Data.Maybe (mapMaybe)

-- | Type class for various BitFlag types.
-- Minimum complete definition is marshalBitFlag and unmarshalBitFlag
class (Bounded f, Enum f) => BitFlag f where
  -- | Convert a BitFlag to 32bit representation.
  marshalBitFlag :: (BitFlag f, Num a) => f -> a
  -- | Convert from 32bit representation to BitFlag.
  unmarshalBitFlag :: (Num a, Eq a, BitFlag f) => a -> Maybe f
  -- | Converts multiple flags to 32bit representation using bitwise 'or'.
  createBitFlags :: (Num a, Bits a, BitFlag f) => [f] -> a
  createBitFlags = foldl' (.|.) 0 . map marshalBitFlag
  -- | Converts 32bit representation of flags back to list of BitFlag.
  fromBitFlags :: (Num a, Bits a, BitFlag f) => a -> [f]
  fromBitFlags = fromBitBitFlags' [minBound..]

fromBitBitFlags' :: (Num a, Bits a, BitFlag f) => [f] -> a -> [f]
fromBitBitFlags' allBitFlags bits = mapMaybe unmarshalBitFlag flags
  where flags = filter ((`elem` rawBitFlags) . (.&. bits)) rawBitFlags
        rawBitFlags = map marshalBitFlag allBitFlags
