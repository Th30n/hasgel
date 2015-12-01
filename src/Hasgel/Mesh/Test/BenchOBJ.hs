module Main where

import Control.DeepSeq (NFData (..))
import Criterion.Main

import Hasgel.Mesh.OBJ

instance NFData OBJ where
  rnf obj = rnf (objVertices obj) `seq`
            rnf (objNormals obj) `seq`
            rnf (objUvs obj) `seq`
            rnf (objFaces obj)

main :: IO ()
main = defaultMain [bench "ring.obj" $ nfIO $ parseFile "ring.obj"]
