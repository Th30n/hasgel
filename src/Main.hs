{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main ( main ) where

import Control.Exception (bracket)
import Control.Monad.State
import Control.Monad.Trans.Control (MonadBaseControl (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word (Word32)
import Foreign (nullPtr)
import Graphics.GL.Core45
import qualified Linear as L
import qualified SDL
import Text.Printf (printf)

import Hasgel.Display
import qualified Hasgel.FrameTimer as FT
import Hasgel.GL
import Hasgel.Input (InputEvent (..), KeyboardKey (..), getEvents)
import Hasgel.Mesh (Face (..), Mesh (..), cube)
import qualified Hasgel.Resources as Res
import qualified Hasgel.SDL as MySDL
import Hasgel.Transform (Transform (..), transform2M44, translate)

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

-- | Time step which is simulated.
simulationStep :: Milliseconds
simulationStep = 20

-- | Maximum number of frames that will be simulated between rendering.
maxFrameSkip :: Int
maxFrameSkip = 10

class HasSimulation a where
  getSimulation :: HasSimulation a => a -> Simulation
  setSimulation :: HasSimulation a => a -> Simulation -> a

instance HasSimulation World where
  getSimulation = worldSimulation
  setSimulation w sim = w { worldSimulation = sim }

-- | Update the simulation time and frame count.
updateSimulation :: Simulation -> Milliseconds -> Simulation
updateSimulation sim !dt =
  let time = simTime sim
      currentTime = dt + timeCurrent time
      acc = simAccumulatedTime sim
      frames = simFrame sim
  in sim { simTime = time { timeCurrent = currentTime },
           simAccumulatedTime = acc - dt,
           simFrame = frames + 1 }

-- | Run the simulation update for given time step until the simulation time
-- is exhausted or 'maxFrameSkip' reached. Update simulates 'simulationStep'
-- amount of time, if given time step is less than that there may be no update.
simulate :: MonadState World m => Milliseconds -> m ()
simulate dt = do
  acc <- gets $ (dt +) . simAccumulatedTime . getSimulation
  setAccum acc
  simulate' acc 1
  where setAccum a = do
          sim <- gets getSimulation
          modify $ flip setSimulation sim { simAccumulatedTime = a }
        simulate' acc i | acc < simulationStep || i >= maxFrameSkip = pure ()
                        | otherwise = do
                            model <- gets worldModelTransform
                            sim <- gets getSimulation
                            cmds <- gets worldPlayerCmds
                            let time = simTime sim
                                model' = updatePlayer model time cmds
                                acc' = acc - simulationStep
                            modify $ \w -> w {
                              worldModelTransform = model',
                              worldSimulation =
                                updateSimulation sim simulationStep }
                            simulate' acc' $ i + 1

millis2Sec :: Fractional a => Milliseconds -> a
millis2Sec = (0.001 *) . fromIntegral

updatePlayer :: Transform -> Time -> Set PlayerCmd -> Transform
updatePlayer prev time cmds = foldl cmdPlayer prev cmds
  where playerSpeed = 4 * millis2Sec (timeDelta time)
        cmdPlayer trans MoveLeft = movePlayer trans (-playerSpeed)
        cmdPlayer trans MoveRight = movePlayer trans playerSpeed
        cmdPlayer trans _ = trans

movePlayer :: Transform -> Float -> Transform
movePlayer prev dx = translate prev $ L.V3 dx 0 0

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

main :: IO ()
main =
  MySDL.withInit [MySDL.InitVideo] . withDisplay $ \d -> do
    glViewport 0 0 800 600
    glActiveTexture GL_TEXTURE0
    bracket loadResources freeResources $ \res -> do
      vao <- gen :: IO VertexArray
      glBindVertexArray $ object vao
      buf <- gen :: IO Buffer
      glBindBuffer GL_ARRAY_BUFFER $ object buf
      bufferData GL_ARRAY_BUFFER (meshVertices cube) GL_STATIC_DRAW
      glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE 0 nullPtr
      glEnableVertexAttribArray 0
      indexBuf <- genIndexBuffer cube
      glEnable GL_DEPTH_TEST
      current <- SDL.ticks
      let [q1, q2, q3, q4] = timeQueries res
          ft = FT.createFrameTimer ((q1, q2), (q3, q4)) current
      void . execStateT loop $ createWorld current d res ft
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

type Milliseconds = Word32

data Loop = Continue | Quit deriving (Eq, Show)

data World = World
  { loopState :: Loop
  , display :: Display
  , worldTime :: Time
  , resources :: Resources
  , worldFrameTimer :: FT.FrameTimer
  , worldModelTransform :: Transform
  , worldSimulation :: Simulation
  , worldPlayerCmds :: Set PlayerCmd
  }

instance Res.HasPrograms World where
  getPrograms = resPrograms . resources
  setPrograms w programs = let res = resources w
                           in w { resources = res { resPrograms = programs } }

instance FT.HasFrameTimer World where
  getFrameTimer = worldFrameTimer
  setFrameTimer w ft = w { worldFrameTimer = ft }

data Time = Time
  { timeCurrent :: Milliseconds
  , timeDelta :: Milliseconds
  }

-- | Stores time and frame details of the simulation.
data Simulation = Simulation
  { simTime :: !Time -- ^ Total elapsed time since the start.
  , simAccumulatedTime :: !Milliseconds -- ^ Accumulated unsimulated time.
  , simFrame :: !Int -- ^ Total number of simulated frames.
  }

type Renderer = IO ()

simulation :: Simulation
simulation = Simulation { simTime = Time 0 simulationStep,
                          simAccumulatedTime = 0,
                          simFrame = 0 }

createWorld :: Milliseconds -> Display -> Resources -> FT.FrameTimer -> World
createWorld time disp res ft =
  World { loopState = Continue, display = disp, worldTime = Time time 0,
          resources = res, worldFrameTimer = ft,
          worldModelTransform = Transform L.zero (L.V3 0.5 0.5 0.5) (L.V3 0 0 (-5)),
          worldSimulation = simulation, worldPlayerCmds = Set.empty }

loop :: (MonadIO m, MonadBaseControl IO m, MonadState World m) => m ()
loop = do
  ls <- gets loopState
  when (ls /= Quit) $ do
    liftIO getEvents >>= mapM_ handleEvent
    gets (timeDelta . worldTime) >>= simulate
    w <- get
    renderActions <- sequence [cubeRenderer, axisRenderer]
    renderDisplay (display w) . FT.withFrameTimer $ do
      liftIO $ sequence_ renderActions
      throwError
    displayFrameRate
    updateTime
    loop

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

-- | Player control commands.
data PlayerCmd =
  MoveLeft
  | MoveRight
  | Shoot
  deriving (Show, Ord, Eq)

insertCmd :: PlayerCmd -> World -> World
insertCmd cmd w = let cmds = worldPlayerCmds w
                  in w { worldPlayerCmds = Set.insert cmd cmds }

deleteCmd :: PlayerCmd -> World -> World
deleteCmd cmd w = let cmds = worldPlayerCmds w
                  in w { worldPlayerCmds = Set.delete cmd cmds }

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
