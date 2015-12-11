module Main where

import Criterion.Main

import Hasgel.Mesh

main :: IO ()
main = defaultMain [bgroup "mesh loading"
                    [bench "ring.obj" $ nfIO $ loadObj "ring.obj",
                     bench "ring.hmd" $ nfIO $ loadHmd "ring.hmd"]]
