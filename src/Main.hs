{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main ( main ) where

import Control.Exception (bracket)
import Control.Monad.State
import Control.Monad.Trans.Control (MonadBaseControl (..))
import Data.Int (Int32)
import Data.Set (Set)
import qualified Data.Set as Set
import Foreign (nullPtr)
import System.Environment (getArgs)
import System.IO (IOMode (..), hPrint, withFile)
import Text.Printf (printf)

import Graphics.GL.Core45
import Linear ((*^))
import qualified Linear as L
import qualified SDL

import Hasgel.Display
import qualified Hasgel.FrameTimer as FT
import Hasgel.Game (GameState (..), PlayerCmd (..), Ticcmd, addTiccmd,
                    buildTiccmd, gameState, ticGame)
import Hasgel.GL
import Hasgel.Input
import Hasgel.Mesh (Mesh (..), meshVertexIx)
import Hasgel.Rendering
import Hasgel.Resources
import qualified Hasgel.SDL as MySDL
import Hasgel.Simulation (HasSimulation (..), Milliseconds, Simulation (..),
                          Time (..), millis2Sec, simulate, simulation)
import Hasgel.Transform (rotateLocal, rotateWorld, translate)

data CameraMovement =
  MoveForward
  | MoveBack
  deriving (Show, Eq)

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
  bindBuffer ElementArrayBuffer buf $
    bufferData ixs StaticDraw
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
      bindVertexArray $ resVao res
      buf <- gen
      bindBuffer ArrayBuffer buf $
        bufferData (meshVertices (resMesh res)) StaticDraw
      vertexAttribPointer 0 3 GL_FLOAT GL_FALSE 0 nullPtr
      normalBuf <- gen
      bindBuffer ArrayBuffer normalBuf $
        bufferData (meshNormals (resMesh res)) StaticDraw
      vertexAttribPointer 1 3 GL_FLOAT GL_FALSE 0 nullPtr
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

withDisplay :: (Display -> IO a) -> IO a
withDisplay = bracket createDisplay destroyDisplay

data Loop = Continue | Quit deriving (Eq, Show)

data World = World
  { worldLoopState :: Loop
  , worldDisplay :: Display
  , worldTime :: Time
  , worldResources :: Resources
  , worldFrameTimer :: FT.FrameTimer
  , worldSimulation :: Simulation GameState
  , worldPlayerCmds :: Set PlayerCmd
  , worldDemoState :: DemoState
  , worldPaused :: Bool
  , worldCamera :: Camera
  }

data DemoState = Record FilePath | Playback FilePath | NoDemo deriving (Eq, Show)

instance HasResources World where
  getResources = worldResources
  setResources w res = w { worldResources = res }

instance FT.HasFrameTimer World where
  getFrameTimer = worldFrameTimer
  setFrameTimer w ft = w { worldFrameTimer = ft }

createWorld :: Display -> Resources -> GameState -> DemoState -> IO World
createWorld disp res gs demo = do
  time <- SDL.ticks
  let [q1, q2, q3, q4] = timeQueries res
      ft = FT.createFrameTimer ((q1, q2), (q3, q4)) time
  return World { worldLoopState = Continue,
                 worldDisplay = disp,
                 worldTime = Time time 0,
                 worldResources = res, worldFrameTimer = ft,
                 worldSimulation = simulation gs,
                 worldPlayerCmds = Set.empty,
                 worldDemoState = demo,
                 worldPaused = False,
                 worldCamera = defaultCamera }

loop :: (MonadIO m, MonadBaseControl IO m, MonadState World m) => m ()
loop = do
  ls <- gets worldLoopState
  when (ls /= Quit) $ do
    liftIO getEvents >>= mapM_ handleEvent
    paused <- gets worldPaused
    unless paused $ gets (timeDelta . worldTime) >>= runTics
    demoState <- gets worldDemoState
    case demoState of
      Playback _ -> checkDemoEnd
      Record fp -> recordDemo fp
      _ -> return ()
    clearOldTiccmds
    disp <- gets worldDisplay
    renderDisplay disp . FT.withFrameTimer $ do
      clearBufferfv GL_COLOR 0 [0, 0, 0, 1]
      clearDepthBuffer 1
      camera <- gets worldCamera
      mapM_ ($ camera) [renderPlayer,
                        renderPlayerShots,
                        renderInvaders,
                        axisRenderer]
      glViewport 0 0 100 100
      renderCameraOrientation camera
      glViewport 0 0 800 600
      throwError
    displayFrameRate
    updateTime
    loop

checkDemoEnd :: MonadState World m => m ()
checkDemoEnd = do
  ticcmds <- gets $ gTiccmds . simState . getSimulation
  when (null ticcmds) $ modify $ \w -> w { worldLoopState = Quit }

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
    win <- gets (getWindow . worldDisplay)
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
handleEvent QuitEvent = modify $ \w -> w { worldLoopState = Quit }
handleEvent (KeyPressedEvent KeyLeft) = modify $ insertCmd MoveLeft
handleEvent (KeyReleasedEvent KeyLeft) = modify $ deleteCmd MoveLeft
handleEvent (KeyPressedEvent KeyRight) = modify $ insertCmd MoveRight
handleEvent (KeyReleasedEvent KeyRight) = modify $ deleteCmd MoveRight
handleEvent (KeyPressedEvent KeySpace) = modify $ insertCmd Shoot
handleEvent (KeyReleasedEvent KeySpace) = modify $ deleteCmd Shoot
handleEvent (KeyPressedEvent KeyP) = modify pausePressed
handleEvent (KeyPressedEvent KeyW) = modify $ moveCamera MoveForward
handleEvent (KeyPressedEvent KeyS) = modify $ moveCamera MoveBack
handleEvent (KeyPressedEvent key) = liftIO $ printf "Pressed %s\n" (show key)
handleEvent (KeyReleasedEvent key) = liftIO $ printf "Released %s\n" (show key)
handleEvent (MouseMotionEvent motion mouseButtons) =
  when (ButtonLeft `elem` mouseButtons) $ modify (rotateCamera motion)
handleEvent _ = pure ()

insertCmd :: PlayerCmd -> World -> World
insertCmd cmd w | worldPaused w = w
                | otherwise = modifyCmds (Set.insert cmd) w

deleteCmd :: PlayerCmd -> World -> World
deleteCmd cmd = modifyCmds (Set.delete cmd)

modifyCmds :: (Set PlayerCmd -> Set PlayerCmd) -> World -> World
modifyCmds f w = w { worldPlayerCmds = f (worldPlayerCmds w) }

pausePressed :: World -> World
pausePressed w = w { worldPaused = not $ worldPaused w }

moveCamera :: CameraMovement -> World -> World
moveCamera dir world =
  let camera = worldCamera world
      viewDir = case dir of
                  MoveForward -> viewForward
                  MoveBack -> viewBack
      moveSpeed = 20
      dt = millis2Sec . timeDelta $ worldTime world
      transform = cameraTransform camera
      velocity = moveSpeed * dt * viewDir camera
      transform' = translate transform velocity
  in world { worldCamera = camera { cameraTransform = transform' } }

-- | Rotate the camera as in first person.
rotateCamera :: L.V2 Int32 -> World -> World
rotateCamera motion world =
  let sens = -10
      dt = millis2Sec . timeDelta $ worldTime world
      L.V2 x y = sens * dt *^ fmap fromIntegral motion
      camera = worldCamera world
      -- | Horizontal rotation is locked to world vertical axis.
      horRot = rotateWorld (cameraTransform camera) $ L.V3 0 x 0
      -- | Vertical rotation is relative to local horizontal axis.
      vertRot = rotateLocal horRot $ L.V3 y 0 0
  in world { worldCamera = camera { cameraTransform = vertRot } }
