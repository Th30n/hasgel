module Hasgel.Game.Movement (
  maxSpeed, tryMove
) where


import Data.List (findIndex)
import Data.Maybe (fromMaybe, isNothing)

import Control.Lens ((^.))
import qualified Linear as L

import Hasgel.Transform (Transform (..), translate)

-- | Maximum speed of movement allowed when checking collisions.
maxSpeed :: Float
maxSpeed = 0.25

-- | Returns the new position and the index of the colliding object when moving
-- for the given velocity.
tryMove :: Transform -> L.V3 Float -> [Transform] -> (Transform, Maybe Int)
tryMove mobj speed [] = (translate mobj speed, Nothing)
tryMove mobj speed blockers =
  case clampSpeed of
    -- Speed is below maximum, move to destination.
    (speed', Nothing) -> tryMove' speed'
    -- Speed is above maximum, move incrementally.
    (speed', Just remSpeed) ->
      case tryMove' speed' of
        -- No collision, continue moving.
        (mobj', Nothing) -> tryMove mobj' remSpeed blockers
        -- Collision, return the result.
        colRes -> colRes
  where clampSpeed = let L.V3 (x, mx) (y, my) (z, mz) = clampComp <$> speed
                         -- Remainder speed if any.
                         rv = if all isNothing [mx, my, mz]
                              then Nothing
                              else let [rx, ry, rz] = fromMaybe 0 <$> [mx, my, mz]
                                   in Just $ L.V3 rx ry rz
                     in (L.V3 x y z, rv)
        clampComp c
          | c < 0, c < -maxSpeed = (-maxSpeed, Just $ maxSpeed + c)
          | c > 0, c > maxSpeed = (maxSpeed, Just $ c - maxSpeed)
          | otherwise = (c, Nothing)
        tryMove' dv
          | Just i <- checkPosition (translate mobj dv) blockers = (mobj, Just i)
          | otherwise = (translate mobj dv, Nothing)

-- | Returns the index of the colliding object.
checkPosition :: Transform -> [Transform] -> Maybe Int
checkPosition mobj = findIndex (bvOverlaps mobj)

-- | Returns True if the axis aligned bounding volumes of given transforms
-- overlap.
bvOverlaps :: Transform -> Transform -> Bool
bvOverlaps a b
  | aMinY > bMaxY || aMaxY < bMinY = False
  | aMinX > bMaxX || aMaxX < bMinX = False
  | aMinZ > bMaxZ || aMaxZ < bMinZ = False
  | otherwise = True
  where bounds t c = let scale = transformScale t ^. c
                         pos = transformPosition t ^. c
                     in (pos - scale, pos + scale)
        [(aMinX, aMaxX), (aMinY, aMaxY), (aMinZ, aMaxZ)] =
          bounds a <$> [L._x, L._y, L._z]
        [(bMinX, bMaxX), (bMinY, bMaxY), (bMinZ, bMaxZ)] =
          bounds b <$> [L._x, L._y, L._z]
