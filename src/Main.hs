{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main ( main ) where

import Control.Exception (bracket)
import Control.Lens ((.~))
import Control.Monad.State
import Data.Word (Word16, Word32)
import Foreign (castPtr, nullPtr, with)
import Graphics.GL.Core45
import qualified Graphics.UI.SDL as SDL
import Linear ((!*!))
import qualified Linear as L
import Text.Printf (printf)

import Hasgel.Display
import qualified Hasgel.FrameTimer as FT
import Hasgel.GL
import qualified Hasgel.SDL.Basic as MySDL
import qualified Hasgel.SDL.Events as MySDL
import qualified Hasgel.SDL.Video as MySDL

ortho :: L.M44 Float
ortho = L.ortho (-2) 2 (-2) 2 (-2) 2

deg2Rad :: Floating a => a -> a
deg2Rad = ((pi / 180) *)

persp :: L.M44 Float
persp = L.perspective fovy ar n f
        where fovy = deg2Rad 75
              ar = 800 / 600
              n = 0.1
              f = 100

uniformProjection :: MonadIO m => Program -> m ()
uniformProjection prog = do
  useProgram prog
  Just loc <- getUniformLocation prog "proj"
  liftIO . with persp $ uniformMatrix4f loc 1 GL_TRUE . castPtr

updateModelTransform :: L.M44 Float -> Time -> L.M44 Float
updateModelTransform prev time =
  let trans = L.translation .~ L.V3 0 0 (-5) $ L.identity
      current = fromIntegral (timeCurrent time) * 1E-3
      angle = 10 * current
      rot = L.fromQuaternion . L.axisAngle (L.V3 1 1 0) $ deg2Rad angle
  in trans !*! L.m33_to_m44 rot

simulationStep :: Milliseconds
simulationStep = 20

maxFrameSkip :: Int
maxFrameSkip = 5

simulate :: MonadState World m => m ()
simulate = do
  acc <- (+) <$> timeDelta <*> timeAccumulator <$> gets worldTime
  setAccum acc
  simulate' acc 1
  where setAccum a = do
          time <- gets worldTime
          modify $ \w -> w { worldTime = time { timeAccumulator = a } }
        simulate' acc i | acc < simulationStep || i >= maxFrameSkip = pure ()
                        | otherwise = do
                            model <- gets worldModelTransform
                            time <- gets worldTime
                            let model' = updateModelTransform model time
                                acc' = acc - simulationStep
                            modify $ \w -> w { worldModelTransform = model' }
                            setAccum acc'
                            simulate' acc' $ i + 1

setModelTransform :: MonadIO m => Program -> L.M44 Float -> m ()
setModelTransform prog model = do
  useProgram prog
  Just loc <- getUniformLocation prog "model"
  liftIO . with model $ uniformMatrix4f loc 1 GL_TRUE . castPtr

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
      uniformProjection $ mainProgram res
      glEnable GL_DEPTH_TEST
      current <- SDL.getTicks
      let [q1, q2, q3, q4] = timeQueries res
          ft = FT.createFrameTimer ((q1, q2), (q3, q4)) current
      void . execStateT loop $ createWorld current d res ft
      delete indexBuf
      delete buf
      delete vao

genIndexBuffer :: MonadIO m => Mesh -> m Buffer
genIndexBuffer mesh = do
  let ixs = concatMap (map (\x -> x - 1) . faceVertexIx) $ meshFaces mesh
  buf <- gen
  glBindBuffer GL_ELEMENT_ARRAY_BUFFER $ object buf
  bufferData GL_ELEMENT_ARRAY_BUFFER ixs GL_STATIC_DRAW
  pure buf

withDisplay :: (Display -> IO a) -> IO a
withDisplay = bracket createDisplay destroyDisplay

data Resources = Resources
  { texture :: Texture
  , mainProgram :: Program
  , axisProgram :: Program
  , timeQueries :: [Query]
  }

loadResources :: MonadIO m => m Resources
loadResources = do
    tex <- loadTexture "share/gfx/checker.bmp"
    program <- compileProgram [("shaders/basic.vert", VertexShader),
                               ("shaders/basic.frag", FragmentShader)]
    axis <- compileProgram [("shaders/axis.vert", VertexShader),
                            ("shaders/axis.frag", FragmentShader)]
    qs <- gens 4
    pure $ Resources tex program axis qs

freeResources :: MonadIO m => Resources -> m ()
freeResources res = do
  delete $ texture res
  delete $ mainProgram res
  delete $ axisProgram res
  deletes $ timeQueries res

type Milliseconds = Word32

data Loop = Continue | Quit deriving (Eq, Show)

data World = World
  { loopState :: Loop
  , display :: Display
  , worldTime :: Time
  , resources :: Resources
  , worldFrameTimer :: FT.FrameTimer
  , worldModelTransform :: L.M44 Float
  }

data Time = Time
  { timeCurrent :: Milliseconds
  , timeDelta :: Milliseconds
  , timeAccumulator :: Milliseconds
  }

createWorld :: Milliseconds -> Display -> Resources -> FT.FrameTimer -> World
createWorld time disp res ft = World { loopState = Continue, display = disp,
                                       worldTime = Time time 0 0,
                                       resources = res, worldFrameTimer = ft,
                                       worldModelTransform = L.identity }

loop :: (MonadIO m, MonadState World m) => m ()
loop = do
  ls <- gets loopState
  when (ls /= Quit) $ do
    getEvents >>= mapM_ handleEvent
    simulate
    w <- get
    renderDisplay (display w) . FT.withFrameTimer $ do
      let res = resources w
      useProgram $ mainProgram res
      let current = fromIntegral (timeCurrent $ worldTime w) / 1000
          r = 0.5 + 0.5 * sin current
          g = 0.5 + 0.5 * cos current
          vertexCount = 3 * length (meshFaces cube)
          model = worldModelTransform w
      clearBufferfv GL_COLOR 0 [r, g, 0, 1]
      clearDepthBuffer 1
      setModelTransform (mainProgram res) model
      drawElements GL_TRIANGLES vertexCount GL_UNSIGNED_SHORT nullPtr
      useProgram $ axisProgram res
      glDrawArrays GL_LINES 0 6
      throwError
    displayFrameRate
    updateTime
    loop

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

instance FT.HasFrameTimer World where
  getFrameTimer = worldFrameTimer
  setFrameTimer w ft = w { worldFrameTimer = ft }

getEvents :: MonadIO m => m [MySDL.Event]
getEvents = MySDL.pollEvent >>= collect
  where collect :: MonadIO m => Maybe MySDL.Event -> m [MySDL.Event]
        collect Nothing = pure []
        collect (Just e) = (e:) <$> getEvents

updateTime :: (MonadIO m, MonadState World m) => m ()
updateTime = do
  newTime <- SDL.getTicks
  time <- gets worldTime
  let oldTime = timeCurrent time
  modify $ \w -> w { worldTime = time { timeCurrent = newTime,
                                        timeDelta = newTime - oldTime } }

handleEvent :: MonadState World m => MySDL.Event -> m ()
handleEvent (MySDL.QuitEvent _ _) = modify $ \w -> w { loopState = Quit }
handleEvent _ = pure ()

compileProgram :: MonadIO m => [(FilePath, ShaderType)] -> m Program
compileProgram files = do
  shaders <- mapM readShader files
  program <- linkProgram shaders
  deletes shaders
  pure program
  where readShader (file, shaderType) = do
          src <- liftIO $ readFile file
          compileShader src shaderType

loadTexture :: MonadIO m => FilePath -> m Texture
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

data Mesh = Mesh
  { meshVertices :: [L.V3 Float]
  , meshFaces :: [Face]
  , meshUvs :: Maybe [L.V2 Float]
  , meshNormals :: Maybe [L.V3 Float]
  }

data Face = Face
  { faceVertexIx :: [Word16]
  , faceUvIx :: Maybe [Word16]
  , faceNormalIx :: Maybe [Word16]
  }

cube :: Mesh
cube = Mesh {
  meshVertices = [ L.V3 (-1) (-1)   1,
                   L.V3 (-1) (-1) (-1),
                   L.V3   1  (-1) (-1),
                   L.V3   1  (-1)   1,
                   L.V3 (-1)   1    1,
                   L.V3 (-1)   1  (-1),
                   L.V3   1    1  (-1),
                   L.V3   1    1    1
                 ]
  , meshFaces = [ Face [2, 3, 4] Nothing $ Just [1, 1, 1],
                  Face [8, 7, 6] Nothing $ Just [2, 2, 2],
                  Face [5, 6, 2] Nothing $ Just [3, 3, 3],
                  Face [6, 7, 3] Nothing $ Just [4, 4, 4],
                  Face [3, 7, 8] Nothing $ Just [5, 5, 5],
                  Face [1, 4, 8] Nothing $ Just [6, 6, 6],
                  Face [1, 2, 4] Nothing $ Just [1, 1, 1],
                  Face [5, 8, 6] Nothing $ Just [2, 2, 2],
                  Face [1, 5, 2] Nothing $ Just [3, 3, 3],
                  Face [2, 6, 3] Nothing $ Just [4, 4, 4],
                  Face [4, 3, 8] Nothing $ Just [5, 5, 5],
                  Face [5, 1, 8] Nothing $ Just [6, 6, 6]
                ]
  , meshUvs = Nothing
  , meshNormals = Just [ L.V3   0 (-1)  0,
                         L.V3   0   1   0,
                         L.V3 (-1)  0   0,
                         L.V3   0   0 (-1),
                         L.V3   1   0   0,
                         L.V3   0   0   1
                       ]
  }
