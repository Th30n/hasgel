module Hasgel.Transform (
  Transform(..), defaultTransform, deg2Rad,
  transform2M44, transformRotationM44, translate, rotate
) where

import Control.Lens ((.~))
import Linear ((!*!))
import qualified Linear as L

data Transform = Transform
  {
    transformRotation :: L.Quaternion Float
  , transformScale :: L.V3 Float
  , transformPosition :: L.V3 Float
  } deriving (Show, Eq)

deg2Rad :: Floating a => a -> a
deg2Rad = ((pi / 180) *)

-- | Transform with default scale, no rotation and positioned at origin.
defaultTransform :: Transform
defaultTransform = Transform (L.unit L._e) (L.V3 1 1 1) L.zero

transform2M44 :: Transform -> L.M44 Float
transform2M44 transform =
  let trans = L.translation .~ transformPosition transform $ L.identity
      scale = L.scaled $ transformScale transform
      rot = L.fromQuaternion $ transformRotation transform
  in trans !*! L.m33_to_m44 rot !*! L.m33_to_m44 scale

transformRotationM44 :: Transform -> L.M44 Float
transformRotationM44 = L.m33_to_m44 . L.fromQuaternion . transformRotation

translate :: Transform -> L.V3 Float -> Transform
translate transform dv = let pos = transformPosition transform
                         in transform { transformPosition = pos + dv }

-- | Rotate in local coordinate system.
rotate :: Transform -> L.V3 Float -> Transform
rotate transform (L.V3 x y z) =
  let rot = transformRotation transform
      [right, up, forward] = L.basis
      xRot = L.axisAngle right $ deg2Rad x
      yRot = L.axisAngle up $ deg2Rad y
      zRot = L.axisAngle forward $ deg2Rad z
  in transform { transformRotation = rot * xRot * yRot * zRot }
