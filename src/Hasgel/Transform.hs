module Hasgel.Transform (
  Transform(..), Space(..), defaultTransform, deg2Rad,
  transform2M44, transformRotationM44, transformForward, transformBack,
  translate, rotate, rotateLocal, rotateWorld
) where

import Control.Lens ((.~))
import Linear ((!*!))
import qualified Linear as L

data Transform = Transform
  { transformRotation :: L.Quaternion Float
  , transformScale :: L.V3 Float
  , transformPosition :: L.V3 Float
  } deriving (Show, Eq)

data Space = SpaceLocal | SpaceWorld deriving (Show, Eq)

forwardV3 :: L.V3 Float
forwardV3 = L.V3 0 0 1

backV3 :: L.V3 Float
backV3 = -forwardV3

rightV3 :: L.V3 Float
rightV3 = L.V3 1 0 0

upV3 :: L.V3 Float
upV3 = L.V3 0 1 0

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

rotate :: Space -> Transform -> L.V3 Float -> Transform
rotate relativeTo transform (L.V3 x y z) =
  let rot = transformRotation transform
      xRot = L.axisAngle rightV3 $ deg2Rad x
      yRot = L.axisAngle upV3 $ deg2Rad y
      zRot = L.axisAngle forwardV3 $ deg2Rad z
      rot' = xRot * yRot * zRot
      newRot = case relativeTo of
                 SpaceLocal -> rot' * rot
                 SpaceWorld -> rot * rot'
  in transform { transformRotation = newRot }

-- | Rotate in local coordinate system.
rotateLocal :: Transform -> L.V3 Float -> Transform
rotateLocal = rotate SpaceLocal

rotateWorld :: Transform -> L.V3 Float -> Transform
rotateWorld = rotate SpaceWorld

-- | Rotate the direction vector from local to world space.
transformDirection :: Transform -> L.V3 Float -> L.V3 Float
transformDirection transform = L.normalize . L.rotate (transformRotation transform)

-- | Return the forward vector of given transform in world space.
transformForward :: Transform -> L.V3 Float
transformForward = flip transformDirection forwardV3

transformBack :: Transform -> L.V3 Float
transformBack = flip transformDirection backV3
