module Hasgel.Args (
  DemoState(..), Args(..), getArgs
) where

import qualified System.Environment

data DemoState = Record FilePath | Playback FilePath | NoDemo deriving (Eq, Show)

data Args = Args
  { argsDemo :: DemoState
  , argsNormals :: !Bool
  , argsMsaa :: !Bool
  , argsGamma :: Maybe Float
  }

defaultArgs :: Args
defaultArgs = Args { argsDemo = NoDemo, argsNormals = False, argsMsaa = False,
                     argsGamma = Nothing }

getArgs :: IO Args
getArgs = parseArgs <$> System.Environment.getArgs

parseArgs :: [String] -> Args
parseArgs = go defaultArgs
  where go args [] = args
        go args ("-record":fp:as) = go args { argsDemo = Record fp } as
        go args ("-playdemo":fp:as) = go args { argsDemo = Playback fp } as
        go args ("-normals":as) = go args { argsNormals = True } as
        go args ("-msaa":as) = go args { argsMsaa = True } as
        go args ("-gamma":gamma:as) = go args { argsGamma = Just $ read gamma } as
        go args _ = args
