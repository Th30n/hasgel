module Hasgel.Configuration (
  Configuration (..), defaultCfg
) where

data Configuration = Configuration
  { cfgFullscreen :: Bool
  } deriving (Show)

defaultCfg :: Configuration
defaultCfg = Configuration {
  cfgFullscreen = False
  }
