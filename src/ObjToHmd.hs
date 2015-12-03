module Main where

import Data.Binary
import System.Environment (getArgs)

import Hasgel.Mesh

usage :: String
usage = "usage: obj2hmd input.obj output.hmd"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [infile, outfile] -> convert infile outfile
    _ -> putStrLn usage

convert :: FilePath -> FilePath -> IO ()
convert infile outfile = do
  parsed <- loadObj infile
  case parsed of
    Left err -> putStrLn err
    Right obj -> encodeFile outfile  obj
