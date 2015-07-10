module Hasgel.Transform (
  Transform(..), transform2M44
) where

import Control.Lens ((.~))
import Linear ((!*!))
import qualified Linear as L

transform2M44 :: Transform -> L.M44 Float
transform2M44 transform =
  let trans = L.translation .~ transformPosition transform $ L.identity
      rot = L.fromQuaternion $ transformRotation transform
  in trans !*! L.m33_to_m44 rot

data Transform = Transform
  {
    transformRotation :: L.Quaternion Float
  , transformPosition :: L.V3 Float
  }