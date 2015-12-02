module Main where

import Data.Binary
import System.Environment (getArgs)

import Hasgel.Mesh.OBJ

instance Binary OBJ where
  put obj = do
    put "HMD"
    put $ objVertices obj
    put $ objNormals obj
    put $ objUvs obj
    put $ objFaces obj

  get = do
    magic <- get
    case magic of
      "HMD" -> do
        verts <- get
        norms <- get
        uvs <- get
        faces <- get
        pure $! OBJ verts norms uvs faces
      _ -> fail "Invalid file format"

usage :: String
usage = "usage: obj2hmd input.obj output.hmd"

main :: IO ()
main = do
  args <- getArgs
  case args of
    (infile:outfile:[]) -> convert infile outfile
    _ -> putStrLn usage

convert :: FilePath -> FilePath -> IO ()
convert infile outfile = do
  parsed <- parseFile infile
  case parsed of
    Left err -> putStrLn err
    Right obj -> encodeFile outfile obj
