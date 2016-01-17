{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Hasgel.World (
  World(..), Configuration (..), Loop(..), DemoBuffer(..),
  Console(..), ConsoleCmd (..), runConsole,
  createWorld, execDefaultCfg, handleConsoleEvent
) where

import Control.Monad (foldM)
import Data.Char (toLower)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import System.IO.Error (tryIOError)
import Text.Printf (printf)

import Data.ByteString.Builder

import qualified SDL

import Hasgel.Display (Display (..))
import qualified Hasgel.FrameTimer as FT
import Hasgel.Game (GameState, PlayerCmd)
import Hasgel.Input
import Hasgel.Rendering (Camera, defaultCamera)
import Hasgel.Resources (HasResources (..), Resources (..))
import Hasgel.Simulation (HasSimulation (..), Simulation, Time (..), simulation)

data Loop = Continue | Quit deriving (Eq, Show)

newtype DemoBuffer = DemoBuffer Builder deriving (Monoid)

type CommandArg = String
type CommandName = String

data Command =
  FullscreenCmd Bool
  | ExecCmd FilePath
  | QuitCmd
  | EchoCmd String
  deriving (Show)

type Commands = Map CommandName ([CommandArg] -> Either String Command)

data World = World
  { worldLoopState :: Loop
  , worldDisplay :: Display
  , worldTime :: Time
  , worldResources :: Resources
  , worldFrameTimer :: FT.FrameTimer
  , worldSimulation :: Simulation GameState
  , worldPlayerCmds :: Set PlayerCmd
  , worldPaused :: Bool
  , worldCamera :: Camera
  , worldDemoBuffer :: DemoBuffer
  , worldConfiguration :: Configuration
  , worldConsole :: Console
  , worldShowConsole :: Bool
  }

instance HasResources World where
  getResources = worldResources
  setResources w res = w { worldResources = res }

instance FT.HasFrameTimer World where
  getFrameTimer = worldFrameTimer
  setFrameTimer w ft = w { worldFrameTimer = ft }

instance HasSimulation World GameState where
  getSimulation = worldSimulation
  setSimulation w sim = w { worldSimulation = sim }

data Configuration = Configuration
  { cfgFullscreen :: Bool
  } deriving (Show)

data Console = Console
  { conHistory :: [String] -- ^ Executed commands and outputs.
  , conCurrent :: String -- ^ Current, unexecuted command.
  } deriving (Show)

data ConsoleCmd = Insert Char | DeleteChar | DeleteLine | Confirm deriving (Show)

defaultCfg :: Configuration
defaultCfg = Configuration {
  cfgFullscreen = False
  }

createWorld :: Display -> Resources -> DemoBuffer -> GameState -> IO World
createWorld disp res demoBuffer gs = do
  time <- SDL.ticks
  let [q1, q2, q3, q4] = timeQueries res
  ft <- FT.createFrameTimer ((q1, q2), (q3, q4))
  return World { worldLoopState = Continue,
                 worldDisplay = disp,
                 worldTime = Time time 0,
                 worldResources = res, worldFrameTimer = ft,
                 worldSimulation = simulation gs,
                 worldPlayerCmds = Set.empty,
                 worldPaused = False,
                 worldCamera = defaultCamera,
                 worldDemoBuffer = demoBuffer,
                 worldConfiguration = defaultCfg,
                 worldConsole = emptyConsole,
                 worldShowConsole = False }

execDefaultCfg :: World -> IO World
execDefaultCfg = runCommand  "exec default.cfg"

runCommand :: String -> World -> IO World
runCommand line world =
  case parseCommand line of
    Left err -> pure $ printConsole err world
    Right cmd -> evalCommand cmd world

commands :: Commands
commands = Map.fromList [
  ("vid_fullscreen", fullscreenCmd),
  ("exec", execCmd),
  ("quit", quitCmd),
  ("echo", echoCmd)]

evalCommand :: Command -> World -> IO World
evalCommand (FullscreenCmd f) world = do
  let d = worldDisplay world
      cfg = worldConfiguration world
  if f == cfgFullscreen cfg
    then pure world
    else do
    SDL.setWindowMode (getWindow d) $ if f then SDL.Fullscreen else SDL.Windowed
    pure world { worldConfiguration = cfg { cfgFullscreen = f } }
evalCommand (ExecCmd fp) world = do
  res <- tryIOError $ do
    cmdLines <- lines <$> readFile fp
    foldM (flip runCommand) world cmdLines
  case res of
    Right w -> pure w
    Left err -> pure $ printConsole ("exec " ++ show err) world
evalCommand QuitCmd world = pure world { worldLoopState = Quit }
evalCommand (EchoCmd args) world = pure $ printConsole args world

printConsole :: String -> World -> World
printConsole msg w = let con = worldConsole w
                     in w { worldConsole = printConsole' msg con }

printConsole' :: String -> Console -> Console
printConsole' msg con = con { conHistory = msg : conHistory con }

fullscreenCmd :: [CommandArg] -> Either String Command
fullscreenCmd ["1"] = Right $ FullscreenCmd True
fullscreenCmd ["0"] = Right $ FullscreenCmd False
fullscreenCmd [arg] = Left $ printf "Expected 1 or 0, got '%s'" arg
fullscreenCmd _ = Left "Expected 1 argument"

execCmd :: [CommandArg] -> Either String Command
execCmd [fp] = Right $ ExecCmd fp
execCmd _ = Left $ printf "Expected 1 argument"

quitCmd :: [CommandArg] -> Either String Command
quitCmd [] = Right QuitCmd
quitCmd _ = Left $ printf "Expected 0 arguments"

echoCmd :: [CommandArg] -> Either String Command
echoCmd = Right . EchoCmd . unwords

parseCommand :: String -> Either String Command
parseCommand line =
  let cmd:args = words line
  in case Map.lookup (map toLower cmd) commands of
       Nothing -> Left $ printf "Command '%s' does not exist" cmd
       Just c -> c args

runConsole :: ConsoleCmd -> World -> IO World
runConsole Confirm w =
  let cmd = conCurrent $ worldConsole w
      world = runConsole' Confirm w
  in runCommand cmd world
runConsole cmd w = pure $ runConsole' cmd w

runConsole' :: ConsoleCmd -> World -> World
runConsole' cmd w =
  let con = worldConsole w
  in w { worldConsole = editConsole cmd con }

emptyConsole :: Console
emptyConsole = Console { conHistory = [], conCurrent = "" }

editConsole :: ConsoleCmd -> Console -> Console
editConsole (Insert c) con = con { conCurrent = conCurrent con ++ [c] }
editConsole DeleteChar con
  | null $ conCurrent con = con
  | otherwise = con { conCurrent = init $ conCurrent con }
editConsole DeleteLine con = con { conCurrent = "" }
editConsole Confirm con = con { conHistory = conCurrent con : conHistory con,
                                conCurrent = "" }

handleConsoleEvent :: [KeyMod] -> KeyboardKey -> Maybe ConsoleCmd
handleConsoleEvent keyMod key | Just c <- key2Char keyMod key = Just $ Insert c
handleConsoleEvent _ KeyReturn = Just Confirm
handleConsoleEvent _ KeyBackspace = Just DeleteChar
handleConsoleEvent _ KeyEsc = Just DeleteLine
handleConsoleEvent _ _ = Nothing
