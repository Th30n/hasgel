module Hasgel.Mesh (
  Face(..), Mesh(..), cube
) where

import Data.Word (Word16)
import qualified Linear as L

data Mesh = Mesh
  { meshVertices :: [L.V3 Float]
  , meshFaces :: [Face]
  , meshUvs :: Maybe [L.V2 Float]
  , meshNormals :: Maybe [L.V3 Float]
  }

data Face = Face
  { faceVertexIx :: [Word16]
  , faceUvIx :: Maybe [Word16]
  , faceNormalIx :: Maybe [Word16]
  }

cube :: Mesh
cube = Mesh {
  meshVertices = [ L.V3 (-1) (-1)   1,
                   L.V3 (-1) (-1) (-1),
                   L.V3   1  (-1) (-1),
                   L.V3   1  (-1)   1,
                   L.V3 (-1)   1    1,
                   L.V3 (-1)   1  (-1),
                   L.V3   1    1  (-1),
                   L.V3   1    1    1
                 ]
  , meshFaces = [ Face [2, 3, 4] Nothing $ Just [1, 1, 1],
                  Face [8, 7, 6] Nothing $ Just [2, 2, 2],
                  Face [5, 6, 2] Nothing $ Just [3, 3, 3],
                  Face [6, 7, 3] Nothing $ Just [4, 4, 4],
                  Face [3, 7, 8] Nothing $ Just [5, 5, 5],
                  Face [1, 4, 8] Nothing $ Just [6, 6, 6],
                  Face [1, 2, 4] Nothing $ Just [1, 1, 1],
                  Face [5, 8, 6] Nothing $ Just [2, 2, 2],
                  Face [1, 5, 2] Nothing $ Just [3, 3, 3],
                  Face [2, 6, 3] Nothing $ Just [4, 4, 4],
                  Face [4, 3, 8] Nothing $ Just [5, 5, 5],
                  Face [5, 1, 8] Nothing $ Just [6, 6, 6]
                ]
  , meshUvs = Nothing
  , meshNormals = Just [ L.V3   0 (-1)  0,
                         L.V3   0   1   0,
                         L.V3 (-1)  0   0,
                         L.V3   0   0 (-1),
                         L.V3   1   0   0,
                         L.V3   0   0   1
                       ]
  }
