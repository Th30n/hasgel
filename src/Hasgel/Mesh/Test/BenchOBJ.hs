module Main where

import Control.DeepSeq (NFData (..))
import Criterion.Main

import Hasgel.Mesh

instance NFData Mesh where
  rnf mesh = rnf (meshVertices mesh) `seq`
             rnf (meshNormals mesh) `seq`
             rnf (meshUvs mesh) `seq`
             rnf (meshFaces mesh)

instance NFData Face where
  rnf face = rnf (faceVertexIx face) `seq`
             rnf (faceUvIx face) `seq`
             rnf (faceNormalIx face)

main :: IO ()
main = defaultMain [bgroup "mesh loading"
                    [bench "ring.obj" $ nfIO $ loadObj "ring.obj",
                     bench "ring.hmd" $ nfIO $ loadHmd "ring.hmd"]]
