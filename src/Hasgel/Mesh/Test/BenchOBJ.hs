module Main where

import Criterion.Main

import Hasgel.Mesh
import Hasgel.Mesh.OBJ (OBJ (..))
import qualified Hasgel.Mesh.OBJ as OBJ

loadRingObj :: IO OBJ
loadRingObj = do
  Right obj <- OBJ.parseFile "ring.obj"
  pure obj

main :: IO ()
main = defaultMain [bgroup "mesh loading"
                    [bench "ring.obj" $ nfIO $ OBJ.parseFile "ring.obj",
                     bench "ring.hmd" $ nfIO $ loadHmd "ring.hmd"],
                    env loadRingObj $ \obj ->
                     bgroup "mesh conversion"
                     [bench "obj2Mesh" $ nf obj2Mesh obj]
                   ]
