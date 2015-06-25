{-# LANGUAGE FlexibleContexts #-}
module Main ( main ) where

import Control.Exception (bracket)
import Control.Monad.State

import Data.Word (Word32)
import Foreign.Marshal.Array (allocaArray, peekArray, withArray)
import Graphics.GL.Core45
import Graphics.GL.Types
import qualified Graphics.UI.SDL as SDL
import Prelude

import Hasgel.Display
import Hasgel.GL
import qualified Hasgel.SDL.Basic as MySDL
import qualified Hasgel.SDL.Events as MySDL
import qualified Hasgel.SDL.Video as MySDL

main :: IO ()
main =
  MySDL.withInit [MySDL.InitVideo] . withDisplay $ \d -> do
    glActiveTexture GL_TEXTURE0
    tex <- loadTexture "share/gfx/checker.bmp"
    Program program <- compileShaders
    allocaArray 1 $ \vaoPtr -> do
      glGenVertexArrays 1 vaoPtr
      vao <- peekArray 1 vaoPtr
      glBindVertexArray $ head vao
      glUseProgram program
      current <- SDL.getTicks
      void $ execStateT loop World { loopState = Continue, display = d,
                                     currentTime = current }
      glDeleteVertexArrays 1 vaoPtr
    glDeleteProgram program

withDisplay :: (Display -> IO a) -> IO a
withDisplay = bracket createDisplay destroyDisplay

data LoopState = Continue | Quit deriving (Eq, Show)

data WorldState = World
  { loopState :: LoopState
  , display :: Display
  , currentTime :: Word32 -- ^ Time in milliseconds
  }

loop :: (MonadIO m, MonadState WorldState m) => m ()
loop = do
  ls <- gets loopState
  when (ls /= Quit) $ do
    event <- MySDL.pollEvent
    mapM_ handleEvent event
    w <- get
    liftIO . renderDisplay (display w) $ do
      let current = fromIntegral (currentTime w) / 1000
      let r = 0.5 + 0.5 * sin current
      let g = 0.5 + 0.5 * cos current
      withArray [r, g, 0.0, 1.0] $ \color ->
        glClearBufferfv GL_COLOR 0 color
      withArray [0.5 * sin current, 0.6 * cos current, 0.0, 0.0] $ \attrib ->
        glVertexAttrib4fv 0 attrib
      glDrawArrays GL_TRIANGLES 0 3
      throwError
    updateTime
    loop

updateTime :: (MonadIO m, MonadState WorldState m) => m ()
updateTime = SDL.getTicks >>= \t -> modify $ \w -> w { currentTime = t }

handleEvent :: MonadState WorldState m => MySDL.Event -> m ()
handleEvent (MySDL.QuitEvent _ _) = modify $ \w ->  w { loopState = Quit }
handleEvent _ = return ()

compileShaders :: MonadIO m => m Program
compileShaders = do
  vsSrc <- liftIO $ readFile "shaders/basic.vert"
  fsSrc <- liftIO $ readFile "shaders/basic.frag"
  vs <- compileShader vsSrc VertexShader
  fs <- compileShader fsSrc FragmentShader
  program <- linkProgram [vs, fs]
  glDeleteShader $ (\(Shader sh) -> sh) vs
  glDeleteShader $ (\(Shader sh) -> sh) fs
  return program

type Texture = GLuint

loadTexture :: MonadIO m => FilePath -> m Texture
loadTexture file = do
  s <- MySDL.loadBMP file
  liftIO . allocaArray 1 $ \texPtr -> do
    glGenTextures 1 texPtr
    tex <- peekArray 1 texPtr
    glBindTexture GL_TEXTURE_2D $ head tex
    let w = fromIntegral $ MySDL.surfaceW s
        h = fromIntegral $ MySDL.surfaceH s
        pixels = MySDL.surfacePixels s
    glTexImage2D GL_TEXTURE_2D 0 GL_RGB w h 0 GL_BGR GL_UNSIGNED_BYTE pixels
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST
    MySDL.freeSurface s
    pure $ head tex
