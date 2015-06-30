{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main ( main ) where

import Control.Exception (bracket)
import Control.Lens ((.~))
import Control.Monad.State
import Data.Monoid ((<>))
import Data.Word (Word16, Word32)
import Foreign (castPtr, nullPtr, with)
import Graphics.GL.Core45
import qualified Graphics.UI.SDL as SDL
import Linear ((!*!))
import qualified Linear as L
import Text.Printf (printf)

import Hasgel.Display
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

setModelTransform :: MonadIO m => Program -> Float -> m ()
setModelTransform prog angle = do
  useProgram prog
  Just loc <- getUniformLocation prog "model"
  let trans = L.translation .~ L.V3 0 0 (-5) $ L.identity
      rot = L.fromQuaternion . L.axisAngle (L.V3 1 1 0) $ deg2Rad angle
      model = trans !*! L.m33_to_m44 rot
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
      let (q1:q2:_) = timeQueries res
          ft = createFrameTimer (q1, q2) current
      void $ execStateT loop World { loopState = Continue, display = d,
                                     currentTime = current, resources = res,
                                     worldFrameTimer = ft }
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
    qs <- gens 2
    pure $ Resources tex program axis qs

freeResources :: MonadIO m => Resources -> m ()
freeResources res = do
  delete $ texture res
  delete $ mainProgram res
  delete $ axisProgram res
  deletes $ timeQueries res

type Milliseconds = Word32

data LoopState = Continue | Quit deriving (Eq, Show)

data WorldState = World
  { loopState :: LoopState
  , display :: Display
  , currentTime :: Milliseconds
  , resources :: Resources
  , worldFrameTimer :: FrameTimer
  }

loop :: (MonadIO m, MonadState WorldState m) => m ()
loop = do
  ls <- gets loopState
  when (ls /= Quit) $ do
    getEvents >>= mapM_ handleEvent
    w <- get
    let res = resources w
        ft = worldFrameTimer w
    withFrameTimer ft $ liftIO . renderDisplay (display w) $ do
      useProgram $ mainProgram res
      let current = fromIntegral (currentTime w) / 1000
          r = 0.5 + 0.5 * sin current
          g = 0.5 + 0.5 * cos current
          vertexCount = 3 * length (meshFaces cube)
      clearBufferfv GL_COLOR 0 [r, g, 0, 1]
      clearDepthBuffer 1
      setModelTransform (mainProgram res) $ current * 10
      drawElements GL_TRIANGLES vertexCount GL_UNSIGNED_SHORT nullPtr
      useProgram $ axisProgram res
      glDrawArrays GL_LINES 0 6
      throwError
    displayFrameRate
    updateTime
    loop

displayFrameRate :: (MonadIO m, MonadState WorldState m) => m ()
displayFrameRate = do
  ft <- gets worldFrameTimer >>= updateFrameTimer
  modify $ \w -> w { worldFrameTimer = ft }
  time <- gets currentTime
  when (time >= 500 + timerStart ft) $ do
    let ms = printf "\tGPU: %.2fms" $ getFrameTime ft
    win <- gets (getWindow . display)
    MySDL.setWindowTitle win $ "hasgel " <> ms
    modify $ \w -> w { worldFrameTimer = resetFrameTimer ft time }

data FrameTimer = FrameTimer
  { timerQueries :: (Query, Query)
  , timerFrames :: Int
  , timerAccum :: Double
  , timerStart :: Milliseconds
  } deriving (Show)

withFrameTimer :: MonadIO m => FrameTimer -> m a -> m a
withFrameTimer (FrameTimer qs frames _ _) action = do
  let q = if odd frames then fst qs else snd qs
  withQuery GL_TIME_ELAPSED q action

createFrameTimer :: (Query, Query) -> Milliseconds -> FrameTimer
createFrameTimer qs start = FrameTimer qs 0 0 start

updateFrameTimer :: MonadIO m => FrameTimer -> m FrameTimer
updateFrameTimer ft@(FrameTimer _ 0 _ _) = pure ft { timerFrames = 1 }
updateFrameTimer (FrameTimer qs frames acc time) = do
  let q = if even frames then fst qs else snd qs
  ms <- (*1E-6) . fromIntegral <$> getQueryResult q
  pure $ FrameTimer qs (frames + 1) (acc + ms) time

resetFrameTimer :: FrameTimer -> Milliseconds -> FrameTimer
resetFrameTimer (FrameTimer qs@(q1, q2) frames _ _) start =
  let qs' = if odd frames then qs else (q2, q1)
  in FrameTimer qs' 1 0 start

getFrameTime :: FrameTimer -> Double
getFrameTime (FrameTimer _ frames acc _) = acc / fromIntegral frames

getEvents :: MonadIO m => m [MySDL.Event]
getEvents = MySDL.pollEvent >>= collect
  where collect :: MonadIO m => Maybe MySDL.Event -> m [MySDL.Event]
        collect Nothing = pure []
        collect (Just e) = (e:) <$> getEvents

updateTime :: (MonadIO m, MonadState WorldState m) => m ()
updateTime = SDL.getTicks >>= \t -> modify $ \w -> w { currentTime = t }

handleEvent :: MonadState WorldState m => MySDL.Event -> m ()
handleEvent (MySDL.QuitEvent _ _) = modify $ \w ->  w { loopState = Quit }
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
