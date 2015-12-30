{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Main ( main ) where

import Control.Exception (bracket)
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Control (MonadBaseControl (..))
import Data.Int (Int32)
import Data.Set (Set)
import qualified Data.Set as Set
import System.IO (IOMode (..), hPrint, withFile)
import Text.Printf (printf)

import Graphics.GL.Core45
import Linear ((*^))
import qualified Linear as L
import qualified SDL

import Hasgel.Args
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
                          Time (..), Update, millis2Sec, simulate, simulation)
import Hasgel.Transform (rotateLocal, rotateWorld, translate)

data CameraMovement =
  MoveForward
  | MoveBack
  | MoveLeft
  | MoveRight
  | MoveDown
  | MoveUp
  deriving (Show, Eq)

data Loop = Continue | Quit deriving (Eq, Show)

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

runTics :: MonadState World m => DemoState -> Milliseconds -> m ()
runTics demo dt = do
  sim <- gets getSimulation
  cmds <- gets worldPlayerCmds
  modify $ \w -> setSimulation w $ simulate sim dt $ updateGame demo cmds

updateGame :: DemoState -> Set PlayerCmd -> Update GameState
updateGame (Playback _) _ time gs = ticGame time gs
updateGame _ cmds time gs = ticGame time $ addTiccmd gs $ buildTiccmd cmds time

main :: IO ()
main =
  MySDL.withInit [MySDL.InitVideo] . withDisplay $ \d -> do
    printGLInfo
    glViewport 0 0 800 600
    glEnable GL_CULL_FACE
    glActiveTexture GL_TEXTURE0
    withResources $ \res -> do
      bufs <- setVertexArray (resVao res) $ do
        let mesh = resMesh res
        attrib $ meshVertices mesh
        attrib $ meshNormals mesh
        attrib $ meshUvs mesh
        indexBuffer $ meshVertexIx mesh
      glEnable GL_DEPTH_TEST
      args <- getArgs
      ticcmds <- case argsDemo args of
                   Playback fp -> readDemo fp
                   _ -> return []
      world <- createWorld d res (gameState ticcmds)
      void $ execStateT (runReaderT loop args) world
      deletes bufs

withDisplay :: (Display -> IO a) -> IO a
withDisplay = bracket createDisplay destroyDisplay

printGLInfo :: IO ()
printGLInfo = do
    printf "GL Vendor: %s\n" =<< getParam Vendor
    printf "GL Renderer: %s\n" =<< getParam Renderer
    printf "GL Version: %s\n" =<< getParam Version
    printf "GLSL Version: %s\n" =<< getParam ShadingLanguageVersion

createWorld :: Display -> Resources -> GameState -> IO World
createWorld disp res gs = do
  time <- SDL.ticks
  let [q1, q2, q3, q4] = timeQueries res
      ft = FT.createFrameTimer ((q1, q2), (q3, q4)) time
  return World { worldLoopState = Continue,
                 worldDisplay = disp,
                 worldTime = Time time 0,
                 worldResources = res, worldFrameTimer = ft,
                 worldSimulation = simulation gs,
                 worldPlayerCmds = Set.empty,
                 worldPaused = False,
                 worldCamera = defaultCamera }

loop :: (MonadIO m, MonadBaseControl IO m,
         MonadState World m, MonadReader Args m) => m ()
loop = do
  ls <- gets worldLoopState
  when (ls /= Quit) $ do
    liftIO getEvents >>= mapM_ handleEvent
    paused <- gets worldPaused
    demoState <- asks argsDemo
    unless paused $ gets (timeDelta . worldTime) >>= runTics demoState
    case demoState of
      Playback _ -> checkDemoEnd
      Record fp -> liftIO . recordDemo fp =<< gets getSimulation
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

recordDemo :: FilePath -> Simulation GameState -> IO ()
recordDemo fp sim = do
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
handleEvent (KeyPressedEvent KeyLeft) = modify $ insertCmd Hasgel.Game.MoveLeft
handleEvent (KeyReleasedEvent KeyLeft) = modify $ deleteCmd Hasgel.Game.MoveLeft
handleEvent (KeyPressedEvent KeyRight) = modify $ insertCmd Hasgel.Game.MoveRight
handleEvent (KeyReleasedEvent KeyRight) = modify $ deleteCmd Hasgel.Game.MoveRight
handleEvent (KeyPressedEvent KeySpace) = modify $ insertCmd Shoot
handleEvent (KeyReleasedEvent KeySpace) = modify $ deleteCmd Shoot
handleEvent (KeyPressedEvent KeyP) = modify pausePressed
handleEvent (KeyPressedEvent KeyW) = modify $ moveCamera MoveForward
handleEvent (KeyPressedEvent KeyS) = modify $ moveCamera MoveBack
handleEvent (KeyPressedEvent KeyA) = modify $ moveCamera Main.MoveLeft
handleEvent (KeyPressedEvent KeyD) = modify $ moveCamera Main.MoveRight
handleEvent (KeyPressedEvent KeyQ) = modify $ moveCamera MoveDown
handleEvent (KeyPressedEvent KeyE) = modify $ moveCamera MoveUp
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

-- | Move the camera as in first person.
moveCamera :: CameraMovement -> World -> World
moveCamera dir world =
  let camera = worldCamera world
      viewDir = case dir of
                  MoveForward -> viewForward
                  MoveBack -> viewBack
                  Main.MoveLeft -> viewLeft
                  Main.MoveRight -> viewRight
                  MoveDown -> viewDown
                  MoveUp -> viewUp
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
