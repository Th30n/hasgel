module Main ( main ) where

import Control.Error
import Control.Monad (when)
import Control.Monad.IO.Class
import Data.Word (Word32)
import Foreign.Marshal.Array (allocaArray, peekArray, withArray)
import Graphics.GL.Core45
import qualified Graphics.UI.SDL as SDL
import Prelude

import Hasgel.Display
import Hasgel.GL
import qualified Hasgel.SDL.Basic as MySDL
import qualified Hasgel.SDL.Events as MySDL

main :: IO ()
main =
  MySDL.withInit [MySDL.InitVideo] $ runScript createDisplay >>=
    (\d -> do
      Program program <- runScript compileShaders
      allocaArray 1 $ \vaoPtr -> do
        glGenVertexArrays 1 vaoPtr
        vao <- peekArray 1 vaoPtr
        glBindVertexArray $ head vao
        glUseProgram program
        current <- SDL.getTicks
        loop World { loopState = Continue, display = d,
                     currentTime = current }
        glDeleteVertexArrays 1 vaoPtr
      glDeleteProgram program
      destroyDisplay d)

data LoopState = Continue | Quit

data WorldState a b = World {
  loopState :: LoopState,
  display :: Display a b,
  currentTime :: Word32 -- ^ Time in milliseconds
}

loop :: MonadIO m => WorldState a b -> m ()
loop curw = case loopState curw of
  Continue -> do
    event <- liftIO MySDL.pollEvent
    let w = maybe curw (handleEvent curw) event
    liftIO . renderDisplay (display w) $ do
      let current = fromIntegral (currentTime w) / 1000
      let r = 0.5 + 0.5 * sin current
      let g = 0.5 + 0.5 * cos current
      withArray [r, g, 0.0, 1.0] $ \color ->
        glClearBufferfv GL_COLOR 0 color
      withArray [0.5 * sin current, 0.6 * cos current, 0.0, 0.0] $ \attrib ->
        glVertexAttrib4fv 0 attrib
      glDrawArrays GL_TRIANGLES 0 3
      errFlag <- Hasgel.GL.getError
      when (errFlag /= NoError) . fail $ show errFlag
    current <- SDL.getTicks
    loop w { currentTime = current}
  Quit -> return ()

handleEvent :: WorldState a b -> MySDL.Event -> WorldState a b
handleEvent w (MySDL.QuitEvent _ _) = w { loopState = Quit }
handleEvent w _ = w

compileShaders :: Script Program
compileShaders = do
  vsSrc <- scriptIO $ readFile "shaders/basic.vert"
  fsSrc <- scriptIO $ readFile "shaders/basic.frag"
  vs <- compileShader vsSrc VertexShader
  fs <- compileShader fsSrc FragmentShader
  program <- linkProgram [vs, fs]
  glDeleteShader $ (\(Shader sh) -> sh) vs
  glDeleteShader $ (\(Shader sh) -> sh) fs
  return program
