{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main ( main ) where

import Control.Exception (bracket, bracket_)
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Control (MonadBaseControl (..))
import Data.Int (Int32)
import Data.IntMap ((!))
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set
import System.IO
import System.Random (mkStdGen, randomIO)
import Text.Printf (printf)

import qualified Data.Binary as B
import Data.Binary.Get (ByteOffset)
import qualified Data.Binary.Get as B
import Data.ByteString.Builder
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
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
import Hasgel.Rendering
import Hasgel.Resources
import Hasgel.Simulation (HasSimulation (..), Milliseconds, Simulation (..),
                          Time (..), Update, millis2Sec, simulate)
import Hasgel.Transform (rotateLocal, rotateWorld, translate)
import Hasgel.World

data CameraMovement =
  MoveForward
  | MoveBack
  | MoveLeft
  | MoveRight
  | MoveDown
  | MoveUp
  deriving (Show, Eq)

type RandomSeed = Int

runTics :: MonadState World m => DemoState -> Milliseconds -> m ()
runTics demo dt = do
  sim <- gets getSimulation
  cmds <- gets worldPlayerCmds
  modify $ \w -> setSimulation w $ simulate sim dt $ updateGame demo cmds

updateGame :: DemoState -> Set PlayerCmd -> Update GameState
updateGame (Playback _) _ time gs = ticGame time gs
updateGame _ cmds time gs = ticGame time $ addTiccmd gs $ buildTiccmd cmds

main :: IO ()
main = withSDL [SDL.InitVideo] . withDisplay $ \d -> do
    printGLInfo
    glViewport 0 0 800 600
    glEnable GL_CULL_FACE
    withResources $ \res -> do
      args <- getArgs
      (seed, ticcmds) <- case argsDemo args of
                           Playback fp -> readDemo fp
                           _ -> (\s -> (s, [])) <$> randomIO
      let demoBuffer = if isDemoRecording (argsDemo args)
                       then beginRecording seed
                       else mempty
          game = gameState ticcmds (mkStdGen seed)
      world <- execDefaultCfg =<< createWorld d res demoBuffer game
      void $ execStateT (runReaderT loop args) world

withSDL :: Foldable f => f SDL.InitFlag -> IO a -> IO a
withSDL flags = bracket_ (SDL.initialize flags) SDL.quit

withDisplay :: (Display -> IO a) -> IO a
withDisplay = bracket createDisplay destroyDisplay

printGLInfo :: IO ()
printGLInfo = do
    printf "GL Vendor: %s\n" =<< getParam Vendor
    printf "GL Renderer: %s\n" =<< getParam Renderer
    printf "GL Version: %s\n" =<< getParam Version
    printf "GLSL Version: %s\n" =<< getParam ShadingLanguageVersion

loop :: (MonadIO m, MonadBaseControl IO m,
         MonadState World m, MonadReader Args m) => m ()
loop = do
  ls <- gets worldLoopState
  demoState <- asks argsDemo
  when (ls == Quit && isDemoRecording demoState) $ do
    let Record fp = demoState
    liftIO . writeDemo fp =<< gets worldDemoBuffer
  unless (ls == Quit) $ do
    (events, keyMod) <- liftIO getEvents
    mapM_ (handleEvent keyMod) events
    paused <- gets worldPaused
    unless paused $ gets (timeDelta . worldTime) >>= runTics demoState
    case demoState of
      Playback _ -> checkDemoEnd
      Record _ -> recordDemo
      _ -> pure ()
    clearOldTiccmds
    disp <- gets worldDisplay
    renderDisplay disp . FT.withFrameTimer $ do
      fbo <- gets $ resFbo . getResources
      void . bindFramebuffer FramebufferTarget fbo $ do
        glEnable GL_DEPTH_TEST
        clearBufferfv GL_COLOR 0 [0, 0, 0, 1]
        clearDepthBuffer 1
        camera <- lift $ gets worldCamera
        lift $ mapM_ ($ camera) [renderPlayer,
                                 renderShots,
                                 renderInvaders,
                                 axisRenderer]
        glViewport 0 0 100 100
        lift $ renderCameraOrientation camera
        glDisable GL_DEPTH_TEST
        glViewport 0 0 800 600
      -- Skip clearing draw buffer and depth testing for fullscreen quad.
      glDisable GL_DEPTH_TEST
      fboTex <- gets $ (! 0) . fbColorTextures . resFbo . getResources
      gamma <- fromMaybe defaultGamma <$> asks argsGamma
      renderGamma gamma fboTex
      -- Text rendering, ignore depth buffer.
      displayStats
      gets worldShowConsole >>= (`when` displayConsole)
      throwError
    resetFrameTimer
    updateTime
    loop

isDemoRecording :: DemoState -> Bool
isDemoRecording (Record _) = True
isDemoRecording _ = False

-- | Check whether there are no more ticcmds and quit the loop.
checkDemoEnd :: MonadState World m => m ()
checkDemoEnd = do
  ticcmds <- gets $ gTiccmds . simState . getSimulation
  when (null ticcmds) $ modify $ \w -> w { worldLoopState = Quit }

-- | Read the demo information from the file.
readDemo :: FilePath -> IO (RandomSeed, [Ticcmd])
readDemo fp = do
  bytes <- BS.readFile fp
  res <- pure $ do
    (remaining, _, seed) <- B.runGetOrFail B.get bytes
    ticcmds <- readTiccmds remaining
    pure (seed, ticcmds)
  case res of
    Left (_, _, msg) -> error $ printf "readDemo: %s" msg
    Right r -> pure r

readTiccmds :: ByteString -> Either (ByteString, ByteOffset, String) [Ticcmd]
readTiccmds bytes
  | BS.null bytes = pure []
  | otherwise = do
      (remaining, _, cmd) <- B.runGetOrFail B.get bytes
      (cmd:) <$> readTiccmds remaining

beginRecording :: RandomSeed -> DemoBuffer
beginRecording = DemoBuffer . lazyByteString . B.encode

recordDemo :: MonadState World m => m ()
recordDemo = modify $ \w ->
  let sim = worldSimulation w
      buf' = writeDemoTiccmds sim $ worldDemoBuffer w
  in w { worldDemoBuffer = buf' }

writeDemoTiccmds :: Simulation GameState -> DemoBuffer -> DemoBuffer
writeDemoTiccmds sim buffer =
  let ticcmds = reverse . gOldTiccmds $ simState sim
      bytes = map (DemoBuffer . lazyByteString . B.encode) ticcmds
  in buffer <> mconcat bytes

writeDemo :: FilePath -> DemoBuffer -> IO ()
writeDemo fp (DemoBuffer buffer) =
  withBinaryFile fp WriteMode $ \fh -> do
    hSetBuffering fh $ BlockBuffering Nothing
    hPutBuilder fh buffer
    printf "Wrote demo to %s\n" fp

clearOldTiccmds :: (MonadState World m) => m ()
clearOldTiccmds = do
  sim <- gets getSimulation
  let sim' = (\gs -> gs { gOldTiccmds = [] }) <$> sim
  modify $ \w -> setSimulation w sim'

-- | Interval in ms for reseting the frame timer information.
timerInterval :: Double
timerInterval = 500

displayConsole :: (MonadBaseControl IO m, MonadState World m) => m ()
displayConsole = do
  conText <- (:) <$> conCurrent <*> conHistory <$> gets worldConsole
  let x = 10
      y = 600 * 0.9
  forM_ (zip [0..] conText) $ \(i, line) ->
    renderString x (y + i * 16) $ printf "> %s" line

displayStats :: (MonadBaseControl IO m, MonadState World m) => m ()
displayStats = do
  ft <- gets worldFrameTimer
  let cpuAvg = FT.getCPUAvg ft
      cpuMax = FT.getCPUMax ft
      gpuTime = FT.getGPUMax ft
      cpuStats s = printf "CPU: %.2fms %.2fFPS (%s)" s (1e3 / s)
      gpuStats = printf "GPU: %.2fms %.2fFPS (max)" gpuTime (1e3 / gpuTime)
      x = 800 * 0.7
      y = 600 * 0.92
  renderString x y $ cpuStats cpuAvg "avg."
  renderString x (y + 16) $ cpuStats cpuMax "max"
  renderString x (y + 32) gpuStats

-- | Reset the frame timer every 'timerInterval'.
resetFrameTimer :: (MonadIO m, MonadState World m) => m ()
resetFrameTimer = do
  ft <- gets worldFrameTimer
  time <- FT.now
  let startTime = FT.getTimerStart ft
  when (time >= timerInterval + startTime) $
    modify . flip FT.setFrameTimer $ FT.resetFrameTimer ft

updateTime :: (MonadIO m, MonadState World m) => m ()
updateTime = do
  newTime <- SDL.ticks
  time <- gets worldTime
  let oldTime = timeCurrent time
  modify $ \w -> w { worldTime = time { timeCurrent = newTime,
                                        timeDelta = newTime - oldTime } }

handleEvent :: (MonadIO m, MonadState World m) => [KeyMod] -> InputEvent -> m ()
handleEvent _ QuitEvent = modify $ \w -> w { worldLoopState = Quit }
handleEvent _ (KeyPressedEvent KeyTilde) = toggleConsole
handleEvent keyMod ev@(KeyPressedEvent key) = do
  showConsole <- gets worldShowConsole
  if showConsole
    then case handleConsoleEvent keyMod key of
           Just cmd -> get >>= liftIO . runConsole cmd >>= put
           Nothing -> handleGameEvent ev
    else handleGameEvent ev
handleEvent _ ev = handleGameEvent ev

handleGameEvent :: (MonadIO m, MonadState World m) => InputEvent -> m ()
handleGameEvent (KeyPressedEvent KeyLeft) =
  modify $ insertCmd Hasgel.Game.MoveLeft
handleGameEvent (KeyReleasedEvent KeyLeft) =
  modify $ deleteCmd Hasgel.Game.MoveLeft
handleGameEvent (KeyPressedEvent KeyRight) =
  modify $ insertCmd Hasgel.Game.MoveRight
handleGameEvent (KeyReleasedEvent KeyRight) =
  modify $ deleteCmd Hasgel.Game.MoveRight
handleGameEvent (KeyPressedEvent KeySpace) = modify $ insertCmd Shoot
handleGameEvent (KeyReleasedEvent KeySpace) = modify $ deleteCmd Shoot
handleGameEvent (KeyPressedEvent KeyP) = modify pausePressed
handleGameEvent (KeyPressedEvent KeyW) = modify $ moveCamera MoveForward
handleGameEvent (KeyPressedEvent KeyS) = modify $ moveCamera MoveBack
handleGameEvent (KeyPressedEvent KeyA) = modify $ moveCamera Main.MoveLeft
handleGameEvent (KeyPressedEvent KeyD) = modify $ moveCamera Main.MoveRight
handleGameEvent (KeyPressedEvent KeyQ) = modify $ moveCamera MoveDown
handleGameEvent (KeyPressedEvent KeyE) = modify $ moveCamera MoveUp
handleGameEvent (KeyPressedEvent key@(KeyUnknown _)) =
  liftIO $ printf "Pressed %s\n" (show key)
handleGameEvent (MouseMotionEvent motion mouseButtons) =
  when (ButtonLeft `elem` mouseButtons) $ modify (rotateCamera motion)
handleGameEvent _ = pure ()

toggleConsole :: MonadState World m => m ()
toggleConsole = modify $ \w -> w { worldShowConsole = not $ worldShowConsole w }

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
