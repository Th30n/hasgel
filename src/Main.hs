{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main ( main ) where

import Control.Exception (bracket)
import Control.Monad.State

import Data.Word (Word32)
import Foreign (nullPtr)
import Graphics.GL.Core45
import qualified Graphics.UI.SDL as SDL
import qualified Linear as L

import Hasgel.Display
import Hasgel.GL
import qualified Hasgel.SDL.Basic as MySDL
import qualified Hasgel.SDL.Events as MySDL
import qualified Hasgel.SDL.Video as MySDL

triangle :: [L.V4 Float]
triangle = [ L.V4 0.25 (-0.25) 0.5 1.0,
             L.V4 (-0.25) (-0.25) 0.5 1.0,
             L.V4 0.25 0.25 0.5 1.0
           ]

main :: IO ()
main =
  MySDL.withInit [MySDL.InitVideo] . withDisplay $ \d -> do
    glActiveTexture GL_TEXTURE0
    bracket loadResources freeResources $ \res -> do
      vao <- gen :: IO VertexArray
      glBindVertexArray $ object vao
      buf <- gen :: IO Buffer
      glBindBuffer GL_ARRAY_BUFFER $ object buf
      bufferData GL_ARRAY_BUFFER triangle GL_STATIC_DRAW
      glVertexAttribPointer 0 4 GL_FLOAT GL_FALSE 0 nullPtr
      glEnableVertexAttribArray 0
      current <- SDL.getTicks
      void $ execStateT loop World { loopState = Continue, display = d,
                                     currentTime = current, resources = res }
      delete buf
      delete vao

withDisplay :: (Display -> IO a) -> IO a
withDisplay = bracket createDisplay destroyDisplay

data Resources = Resources
  { texture :: Texture
  , mainProgram :: Program
  , axisProgram :: Program
  }

loadResources :: MonadIO m => m Resources
loadResources = do
    tex <- loadTexture "share/gfx/checker.bmp"
    program <- compileProgram [("shaders/basic.vert", VertexShader),
                               ("shaders/basic.frag", FragmentShader)]
    axis <- compileProgram [("shaders/axis.vert", VertexShader),
                            ("shaders/axis.frag", FragmentShader)]
    pure $ Resources tex program axis

freeResources :: MonadIO m => Resources -> m ()
freeResources res = do
  delete $ texture res
  delete $ mainProgram res
  delete $ axisProgram res

data LoopState = Continue | Quit deriving (Eq, Show)

data WorldState = World
  { loopState :: LoopState
  , display :: Display
  , currentTime :: Word32 -- ^ Time in milliseconds
  , resources :: Resources
  }

loop :: (MonadIO m, MonadState WorldState m) => m ()
loop = do
  ls <- gets loopState
  when (ls /= Quit) $ do
    event <- MySDL.pollEvent
    mapM_ handleEvent event
    w <- get
    liftIO . renderDisplay (display w) $ do
      let res = resources w
      useProgram $ mainProgram res
      let current = fromIntegral (currentTime w) / 1000
          r = 0.5 + 0.5 * sin current
          g = 0.5 + 0.5 * cos current
      clearBufferfv GL_COLOR 0 [r, g, 0, 1]
      vertexAttrib4f (Index 1) (0.5 * sin current) (0.6 * cos current) 0 0
      glDrawArrays GL_TRIANGLES 0 3
      useProgram $ axisProgram res
      glDrawArrays GL_LINES 0 6
      throwError
    updateTime
    loop

updateTime :: (MonadIO m, MonadState WorldState m) => m ()
updateTime = SDL.getTicks >>= \t -> modify $ \w -> w { currentTime = t }

handleEvent :: MonadState WorldState m => MySDL.Event -> m ()
handleEvent (MySDL.QuitEvent _ _) = modify $ \w ->  w { loopState = Quit }
handleEvent _ = return ()

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
  { meshVertices :: [Float]
  , meshFaceIndexes :: [(Int, Maybe Int, Maybe Int)]
  , meshUvs :: Maybe [Float]
  , meshNormals :: Maybe [Float]
  }

cube :: Mesh
cube = Mesh{
  meshVertices = [ -1, -1,  1,
                   -1, -1, -1,
                    1, -1, -1,
                    1, -1,  1,
                   -1,  1,  1,
                   -1,  1, -1,
                    1,  1, -1,
                    1,  1,  1
                 ]
  , meshFaceIndexes = [
      (2, Nothing, Just 1), (3, Nothing, Just 1), (4, Nothing, Just 1),
      (8, Nothing, Just 2), (7, Nothing, Just 2), (6, Nothing, Just 2),
      (5, Nothing, Just 3), (6, Nothing, Just 3), (2, Nothing, Just 3),
      (6, Nothing, Just 4), (7, Nothing, Just 4), (3, Nothing, Just 4),
      (3, Nothing, Just 5), (7, Nothing, Just 5), (8, Nothing, Just 5),
      (1, Nothing, Just 6), (4, Nothing, Just 6), (8, Nothing, Just 6),
      (1, Nothing, Just 1), (2, Nothing, Just 1), (4, Nothing, Just 1),
      (5, Nothing, Just 2), (8, Nothing, Just 2), (6, Nothing, Just 2),
      (1, Nothing, Just 3), (5, Nothing, Just 3), (2, Nothing, Just 3),
      (2, Nothing, Just 4), (6, Nothing, Just 4), (3, Nothing, Just 4),
      (4, Nothing, Just 5), (3, Nothing, Just 5), (8, Nothing, Just 5),
      (5, Nothing, Just 6), (1, Nothing, Just 6), (8, Nothing, Just 6)
      ]
  , meshUvs = Nothing
  , meshNormals = Just [ 0, -1, 0,
                         0,  1, 0,
                         -1, 0, 0,
                         0, 0, -1,
                         1, 0, 0,
                         0, 0, 1
                       ]
  }
