module Main ( main ) where

import Control.Error
import Data.Word (Word32)
import Foreign.Marshal.Array (allocaArray, peekArray, withArray)
import Graphics.GL.Core45
import Graphics.UI.SDL as SDL
import Prelude

import Hasgel.Display
import Hasgel.GL
import Hasgel.SDL.Basic as MySDL
import Hasgel.SDL.Events as MySDL

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

loop :: WorldState a b -> IO ()
loop curw = case loopState curw of
  Continue -> do
    event <- MySDL.pollEvent
    let w = maybe curw (handleEvent curw) event
    renderDisplay (display w) $ do
      let current = fromIntegral (currentTime w) / 1000
      let r = 0.5 + 0.5 * sin current
      let g = 0.5 + 0.5 * cos current
      withArray [r, g, 0.0, 1.0] $ \color ->
        glClearBufferfv GL_COLOR 0 color
      glPointSize 40.0
      glDrawArrays GL_TRIANGLES 0 3
    current <- SDL.getTicks
    loop w { currentTime = current}
  Quit -> return ()

handleEvent :: WorldState a b -> MySDL.Event -> WorldState a b
handleEvent w (MySDL.QuitEvent _ _) = w { loopState = Quit }
handleEvent w _ = w

compileShaders :: Script Program
compileShaders = do
  let vsSrc = "#version 430 core\n" ++
        "void main(void)\n" ++
        "{\n" ++
            "const vec4 vertices[3] = vec4[3](vec4(0.25, -0.25, 0.5, 1.0),\n" ++
            "                                 vec4(-0.25, -0.25, 0.5, 1.0),\n" ++
            "                                 vec4(0.25, 0.25, 0.5, 1.0));\n" ++
            "gl_Position = vertices[gl_VertexID];\n" ++
        "}\n"
  let fsSrc = "#version 430 core\n" ++
        "out vec4 color;\n" ++
        "void main(void)\n" ++
        "{\n" ++
            "color = vec4(0.0, 0.8, 1.0, 1.0);\n" ++
        "}\n"
  vs <- compileShader vsSrc VertexShader
  fs <- compileShader fsSrc FragmentShader
  program <- linkProgram [vs, fs]
  glDeleteShader $ (\(Shader sh) -> sh) vs
  glDeleteShader $ (\(Shader sh) -> sh) fs
  return program
