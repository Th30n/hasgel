{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main ( main ) where

import Control.Exception (bracket)
import Control.Monad.State
import Control.Monad.Trans.Control (MonadBaseControl (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Foreign (nullPtr)
import System.Environment (getArgs)
import System.IO (IOMode (..), hPrint, withFile)
import Text.Printf (printf)

import Graphics.GL.Core45
import qualified Linear as L
import qualified SDL

import Hasgel.Display
import qualified Hasgel.FrameTimer as FT
import Hasgel.Game (GameState (..), PlayerCmd (..), Ticcmd, addTiccmd,
                    buildTiccmd, gameState, ticGame)
import Hasgel.GL
import Hasgel.Input (InputEvent (..), KeyboardKey (..), getEvents)
import Hasgel.Mesh (Mesh (..), meshVertexIx)
import Hasgel.Rendering
import Hasgel.Resources
import qualified Hasgel.SDL as MySDL
import Hasgel.Simulation (HasSimulation (..), Milliseconds, Simulation (..),
                          Time (..), simulate, simulation)

ortho :: L.M44 Float
ortho = L.ortho (-10) 10 (-10) 10 (-10) 10

instance HasSimulation World GameState where
  getSimulation = worldSimulation
  setSimulation w sim = w { worldSimulation = sim }

runTics :: MonadState World m => Milliseconds -> m ()
runTics dt = do
  sim <- gets getSimulation
  demo <- gets worldDemoState
  cmds <- gets worldPlayerCmds
  modify $ \w -> setSimulation w $ simulate sim dt $ updateGame demo cmds

updateGame :: DemoState -> Set PlayerCmd -> Time -> GameState -> GameState
updateGame (Playback _) _ time gs = ticGame time gs
updateGame _ cmds time gs = ticGame time $ addTiccmd gs $ buildTiccmd cmds time

genIndexBuffer :: Mesh -> IO Buffer
genIndexBuffer mesh = do
  let ixs = meshVertexIx mesh
  buf <- gen
  bindBuffer GL_ELEMENT_ARRAY_BUFFER buf $
    bufferData ixs GL_STATIC_DRAW
  pure buf

parseArgs :: [String] -> DemoState
parseArgs ("-record":fp:_) = Record fp
parseArgs ("-playdemo":fp:_) = Playback fp
parseArgs _ = NoDemo

main :: IO ()
main =
  MySDL.withInit [MySDL.InitVideo] . withDisplay $ \d -> do
    glViewport 0 0 800 600
    glActiveTexture GL_TEXTURE0
    withResources $ \res -> do
      vao <- gen :: IO VertexArray
      glBindVertexArray $ object vao
      buf <- gen
      bindBuffer GL_ARRAY_BUFFER buf $
        bufferData (meshVertices (resMesh res)) GL_STATIC_DRAW
      glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE 0 nullPtr
      glEnableVertexAttribArray 0
      normalBuf <- gen
      bindBuffer GL_ARRAY_BUFFER normalBuf $
        bufferData (meshNormals (resMesh res)) GL_STATIC_DRAW
      glVertexAttribPointer 1 3 GL_FLOAT GL_FALSE 0 nullPtr
      glEnableVertexAttribArray 1
      indexBuf <- genIndexBuffer $ resMesh res
      glEnable GL_DEPTH_TEST
      args <- getArgs
      let demo = parseArgs args
      ticcmds <- case demo of
                   Playback fp -> readDemo fp
                   _ -> return []
      void . execStateT loop =<< createWorld d res (gameState ticcmds) demo
      delete indexBuf
      delete buf
      delete vao

withDisplay :: (Display -> IO a) -> IO a
withDisplay = bracket createDisplay destroyDisplay

data Loop = Continue | Quit deriving (Eq, Show)

data World = World
  { loopState :: Loop
  , display :: Display
  , worldTime :: Time
  , resources :: Resources
  , worldFrameTimer :: FT.FrameTimer
  , worldSimulation :: Simulation GameState
  , worldPlayerCmds :: Set PlayerCmd
  , worldDemoState :: DemoState
  }

data DemoState = Record FilePath | Playback FilePath | NoDemo deriving (Eq, Show)

instance HasResources World where
  getResources = resources
  setResources w res = w { resources = res }

instance FT.HasFrameTimer World where
  getFrameTimer = worldFrameTimer
  setFrameTimer w ft = w { worldFrameTimer = ft }

createWorld :: Display -> Resources -> GameState -> DemoState -> IO World
createWorld disp res gs demo = do
  time <- SDL.ticks
  let [q1, q2, q3, q4] = timeQueries res
      ft = FT.createFrameTimer ((q1, q2), (q3, q4)) time
  return World { loopState = Continue, display = disp, worldTime = Time time 0,
                 resources = res, worldFrameTimer = ft,
                 worldSimulation = simulation gs,
                 worldPlayerCmds = Set.empty,
                 worldDemoState = demo }

loop :: (MonadIO m, MonadBaseControl IO m, MonadState World m) => m ()
loop = do
  ls <- gets loopState
  when (ls /= Quit) $ do
    liftIO getEvents >>= mapM_ handleEvent
    gets (timeDelta . worldTime) >>= runTics
    demoState <- gets worldDemoState
    case demoState of
      Playback _ -> checkDemoEnd
      Record fp -> recordDemo fp
      _ -> return ()
    clearOldTiccmds
    disp <- gets display
    renderDisplay disp . FT.withFrameTimer $ do
      clearBufferfv GL_COLOR 0 [0, 0, 0, 1]
      clearDepthBuffer 1
      renderPlayer
      renderPlayerShots
      renderInvaders
      axisRenderer
      throwError
    displayFrameRate
    updateTime
    loop

checkDemoEnd :: MonadState World m => m ()
checkDemoEnd = do
  ticcmds <- gets $ gTiccmds . simState . getSimulation
  when (null ticcmds) $ modify $ \w -> w { loopState = Quit }

readDemo :: FilePath -> IO [Ticcmd]
readDemo fp = concatMap read . lines <$> readFile fp

recordDemo :: (MonadIO m, MonadState World m) => FilePath -> m ()
recordDemo fp = do
  sim <- gets getSimulation
  let ticcmds = reverse . gOldTiccmds $ simState sim
  liftIO . withFile fp AppendMode $ \fh -> hPrint fh ticcmds

clearOldTiccmds :: (MonadState World m) => m ()
clearOldTiccmds = do
  sim <- gets getSimulation
  let sim' = (\gs -> gs { gOldTiccmds = [] }) <$> sim
  modify $ \w -> setSimulation w sim'

-- | Interval for updating the frame timer information.
timerInterval :: Milliseconds
timerInterval = 500

titleFormat :: String
titleFormat = "hasgel  CPU: %.2fms  GPU: %.2fms"

displayFrameRate :: (MonadIO m, MonadState World m) => m ()
displayFrameRate = do
  ft <- gets worldFrameTimer
  time <- gets $ timeCurrent . worldTime
  let startTime = FT.timerStart ft
  when (time >= timerInterval + startTime) $ do
    let cpuTime = FT.getCPUTime ft time
        gpuTime = FT.getGPUTime ft
        title = printf titleFormat cpuTime gpuTime
    win <- gets (getWindow . display)
    MySDL.setWindowTitle win title
    modify . flip FT.setFrameTimer $ FT.resetFrameTimer ft time

updateTime :: (MonadIO m, MonadState World m) => m ()
updateTime = do
  newTime <- SDL.ticks
  time <- gets worldTime
  let oldTime = timeCurrent time
  modify $ \w -> w { worldTime = time { timeCurrent = newTime,
                                        timeDelta = newTime - oldTime } }

handleEvent :: (MonadIO m, MonadState World m) => InputEvent -> m ()
handleEvent QuitEvent = modify $ \w -> w { loopState = Quit }
handleEvent (KeyPressedEvent KeyLeft) = modify $ insertCmd MoveLeft
handleEvent (KeyReleasedEvent KeyLeft) = modify $ deleteCmd MoveLeft
handleEvent (KeyPressedEvent KeyRight) = modify $ insertCmd MoveRight
handleEvent (KeyReleasedEvent KeyRight) = modify $ deleteCmd MoveRight
handleEvent (KeyPressedEvent KeySpace) = modify $ insertCmd Shoot
handleEvent (KeyReleasedEvent KeySpace) = modify $ deleteCmd Shoot
handleEvent (KeyPressedEvent key) = liftIO $ printf "Pressed %s\n" (show key)
handleEvent (KeyReleasedEvent key) = liftIO $ printf "Released %s\n" (show key)
handleEvent _ = pure ()

insertCmd :: PlayerCmd -> World -> World
insertCmd cmd = modifyCmds (Set.insert cmd)

deleteCmd :: PlayerCmd -> World -> World
deleteCmd cmd = modifyCmds (Set.delete cmd)

modifyCmds :: (Set PlayerCmd -> Set PlayerCmd) -> World -> World
modifyCmds f w = w { worldPlayerCmds = f (worldPlayerCmds w) }
