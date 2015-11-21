{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main ( main ) where

import Control.Exception (bracket)
import Control.Monad.State
import Control.Monad.Trans.Control (MonadBaseControl (..))
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import System.Environment (getArgs)
import System.IO (IOMode (..), hPrint, withFile)

import Foreign (nullPtr)
import Graphics.GL.Core45
import qualified Linear as L
import qualified SDL
import Text.Printf (printf)

import Hasgel.Display
import qualified Hasgel.FrameTimer as FT
import Hasgel.Game (GameState (..), PlayerCmd (..), Ticcmd, addTiccmd,
                    buildTiccmd, gameState, ticPlayer)
import Hasgel.GL
import Hasgel.Input (InputEvent (..), KeyboardKey (..), getEvents)
import Hasgel.Mesh (Face (..), Mesh (..), cube)
import qualified Hasgel.Resources as Res
import qualified Hasgel.SDL as MySDL
import Hasgel.Simulation (Milliseconds, Simulation (..), Time (..), simulate,
                          simulation)
import Hasgel.Transform (Transform (..), transform2M44)

ortho :: L.M44 Float
ortho = L.ortho (-10) 10 (-10) 10 (-10) 10

deg2Rad :: Floating a => a -> a
deg2Rad = ((pi / 180) *)

persp :: L.M44 Float
persp = L.perspective fovy ar n f
        where fovy = deg2Rad 60
              ar = 800 / 600
              n = 0.1
              f = 100

camera :: L.M44 Float
camera = L.lookAt eye center up
  where eye = L.V3 0 0 1
        center = L.V3 0 0 0
        up = L.V3 0 1 0

uniformProjection :: MonadIO m => Program -> m ()
uniformProjection prog = do
  mbLoc <- getUniformLocation prog "proj"
  case mbLoc of
    Just loc -> useProgram prog >> uniform loc (persp L.!*! camera)
    _ -> pure ()

updateModelTransform :: Transform -> Time -> Transform
updateModelTransform prev time =
  let current = fromIntegral (timeCurrent time) * 1E-3
      angle = 10 * current
      rot = L.axisAngle (L.V3 1 1 0) $ deg2Rad angle
  in prev { transformRotation = rot }

class HasSimulation a b where
  getSimulation :: HasSimulation a b => a -> Simulation b
  setSimulation :: HasSimulation a b => a -> Simulation b -> a

instance HasSimulation World GameState where
  getSimulation = worldSimulation
  setSimulation w sim = w { worldSimulation = sim }

updateGame :: MonadState World m => Milliseconds -> m ()
updateGame dt = do
  sim <- gets getSimulation
  demo <- gets worldDemoState
  cmds <- gets worldPlayerCmds
  modify $ \w -> setSimulation w $ simulate sim dt $ updatePlayer demo cmds

updatePlayer :: DemoState -> Set PlayerCmd -> Time -> GameState -> GameState
updatePlayer Playback _ _ gs = ticPlayer gs
updatePlayer _ cmds time gs = ticPlayer $ addTiccmd gs $ buildTiccmd cmds time

setModelTransform :: MonadIO m => Program -> L.M44 Float -> m ()
setModelTransform prog model = do
  Just loc <- getUniformLocation prog "model"
  useProgram prog >> uniform loc model

genIndexBuffer :: Mesh -> IO Buffer
genIndexBuffer mesh = do
  let ixs = concatMap (map (\x -> x - 1) . faceVertexIx) $ meshFaces mesh
  buf <- gen
  glBindBuffer GL_ELEMENT_ARRAY_BUFFER $ object buf
  bufferData GL_ELEMENT_ARRAY_BUFFER ixs GL_STATIC_DRAW
  pure buf

parseArgs :: [String] -> (DemoState, Maybe FilePath)
parseArgs [] = (NoDemo, Nothing)
parseArgs ["-record", fp] = (Record fp, Nothing)
parseArgs ["-playdemo", fp] = (Playback, Just fp)
parseArgs _ = (NoDemo, Nothing)

main :: IO ()
main =
  MySDL.withInit [MySDL.InitVideo] . withDisplay $ \d -> do
    glViewport 0 0 800 600
    glActiveTexture GL_TEXTURE0
    withResources $ \res -> do
      vao <- gen :: IO VertexArray
      glBindVertexArray $ object vao
      buf <- gen :: IO Buffer
      glBindBuffer GL_ARRAY_BUFFER $ object buf
      bufferData GL_ARRAY_BUFFER (meshVertices cube) GL_STATIC_DRAW
      glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE 0 nullPtr
      glEnableVertexAttribArray 0
      indexBuf <- genIndexBuffer cube
      glEnable GL_DEPTH_TEST
      args <- getArgs
      let (demo, mbFile) = parseArgs args
      world <- case demo of
        r@(Record _) -> createWorld d res (gameState []) r
        Playback -> do
          demoCmds <- readDemo $ fromMaybe "demo.hdm" mbFile
          createWorld d res (gameState demoCmds) Playback
        _ -> createWorld d res (gameState []) NoDemo
      void $ execStateT loop world
      delete indexBuf
      delete buf
      delete vao

withDisplay :: (Display -> IO a) -> IO a
withDisplay = bracket createDisplay destroyDisplay

mainProgramDesc :: Res.ProgramDesc
mainProgramDesc = [("shaders/basic.vert", VertexShader),
                   ("shaders/basic.frag", FragmentShader)]

axisProgramDesc :: Res.ProgramDesc
axisProgramDesc = [("shaders/axis.vert", VertexShader),
                   ("shaders/axis.geom", GeometryShader),
                   ("shaders/axis.frag", FragmentShader)]

data Resources = Resources
  { texture :: Texture
  , timeQueries :: [Query]
  , resPrograms :: Res.Programs
  }

instance Res.HasPrograms Resources where
  getPrograms = resPrograms
  setPrograms res programs = res { resPrograms = programs }

withResources :: (Resources -> IO a) -> IO a
withResources = bracket loadResources freeResources

loadResources :: IO Resources
loadResources = do
    tex <- loadTexture "share/gfx/checker.bmp"
    qs <- gens 4
    pure $ Resources tex qs Res.emptyPrograms

freeResources :: Resources -> IO ()
freeResources res = do
  delete $ texture res
  deletes $ timeQueries res
  void . Res.freePrograms $ resPrograms res

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

data DemoState = Record FilePath | Playback | NoDemo deriving (Eq, Show)

worldModelTransform :: World -> Transform
worldModelTransform = gPlayerTransform . simState . worldSimulation

instance Res.HasPrograms World where
  getPrograms = resPrograms . resources
  setPrograms w programs = let res = resources w
                           in w { resources = res { resPrograms = programs } }

instance FT.HasFrameTimer World where
  getFrameTimer = worldFrameTimer
  setFrameTimer w ft = w { worldFrameTimer = ft }

type Renderer = IO ()

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
    gets (timeDelta . worldTime) >>= updateGame
    demoState <- gets worldDemoState
    case demoState of
      Playback -> checkDemoEnd
      Record fp -> recordDemo fp
      _ -> return ()
    clearOldTiccmds
    renderActions <- sequence [cubeRenderer, axisRenderer]
    disp <- gets display
    renderDisplay disp . FT.withFrameTimer $ do
      liftIO $ sequence_ renderActions
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

axisRenderer :: (MonadBaseControl IO m, MonadState World m) => m Renderer
axisRenderer = do
  w <- get
  axisProgram <- Res.loadProgram axisProgramDesc
  pure $ do
    useProgram axisProgram
    Just mvpLoc <- getUniformLocation axisProgram "mvp"
    let model = transform2M44 $ worldModelTransform w
        mvp = persp L.!*! camera L.!*! model
    uniform mvpLoc mvp
    glDrawArrays GL_POINTS 0 1

cubeRenderer :: (MonadBaseControl IO m, MonadState World m) => m Renderer
cubeRenderer = do
  mainProg <- Res.loadProgram mainProgramDesc
  w <- get
  pure $ do
    useProgram mainProg
    uniformProjection mainProg
    let vertexCount = 3 * length (meshFaces cube)
        model = transform2M44 $ worldModelTransform w
    clearBufferfv GL_COLOR 0 [0, 0, 0, 1]
    clearDepthBuffer 1
    setModelTransform mainProg model
    drawElements GL_TRIANGLES vertexCount GL_UNSIGNED_SHORT nullPtr

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
handleEvent (KeyPressedEvent KeyRight) = modify $ insertCmd MoveRight
handleEvent (KeyPressedEvent key) = liftIO $ printf "Pressed %s\n" (show key)
handleEvent (KeyReleasedEvent KeyLeft) = modify $ deleteCmd MoveLeft
handleEvent (KeyReleasedEvent KeyRight) = modify $ deleteCmd MoveRight
handleEvent (KeyReleasedEvent key) = liftIO $ printf "Released %s\n" (show key)
handleEvent _ = pure ()

insertCmd :: PlayerCmd -> World -> World
insertCmd cmd = modifyCmds (Set.insert cmd)

deleteCmd :: PlayerCmd -> World -> World
deleteCmd cmd = modifyCmds (Set.delete cmd)

modifyCmds :: (Set PlayerCmd -> Set PlayerCmd) -> World -> World
modifyCmds f w = w { worldPlayerCmds = f (worldPlayerCmds w) }

loadTexture :: FilePath -> IO Texture
loadTexture file = do
  s <- MySDL.loadBMP file
  tex <- gen
  glBindTexture GL_TEXTURE_2D $ object tex
  let w = fromIntegral $ MySDL.surfaceW s
      h = fromIntegral $ MySDL.surfaceH s
      pixels = MySDL.surfacePixels s
  glTexImage2D GL_TEXTURE_2D 0 GL_RGB w h 0 GL_BGR GL_UNSIGNED_BYTE pixels
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST
  MySDL.freeSurface s
  pure tex
