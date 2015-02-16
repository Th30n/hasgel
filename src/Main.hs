module Main ( main ) where

import Control.Error
import Control.Monad (when)
import Data.Word (Word32)
import Foreign.C.String (withCAString)
import Foreign.Marshal.Array (allocaArray, peekArray, withArray)
import Foreign.Ptr (nullPtr)
import Graphics.GL.Core45
import Graphics.GL.Types
import Graphics.UI.SDL as SDL
import Prelude

import Hasgel.Display
import Hasgel.SDL.Basic as MySDL
import Hasgel.SDL.Events as MySDL

main :: IO ()
main =
  MySDL.withInit [MySDL.InitVideo] $ runScript createDisplay >>=
    (\d -> do
      program <- runScript compileShaders
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
      glDrawArrays GL_POINTS 0 1
    current <- SDL.getTicks
    loop w { currentTime = current}
  Quit -> return ()

handleEvent :: WorldState a b -> MySDL.Event -> WorldState a b
handleEvent w (MySDL.QuitEvent _ _) = w { loopState = Quit }
handleEvent w _ = w

compileShaders :: Script GLuint
compileShaders = do
  let vsSrc = "#version 430 core\n" ++
        "void main(void)\n" ++
        "{\n" ++
            "gl_Position = vec4(0.0, 0.0, 0.5, 1.0);\n" ++
        "}\n"
  let fsSrc = "#version 430 core\n" ++
        "out vec4 color;\n" ++
        "void main(void)\n" ++
        "{\n" ++
            "color = vec4(0.0, 0.8, 1.0, 1.0);\n" ++
        "}\n"
  vs <- createShader vsSrc VertexShader
  fs <- createShader fsSrc FragmentShader
  program <- glCreateProgram
  when (program == 0) $ throwT "Error creating program."
  glAttachShader program vs
  glAttachShader program fs
  glLinkProgram program
  glDeleteShader vs
  glDeleteShader fs
  return program

-- | Shader object.
type Shader = GLuint

data ShaderType =
  ComputeShader
  | VertexShader
  | TessControlShader
  | TessEvaluationShader
  | GeometryShader
  | FragmentShader
  deriving (Show)

-- | Transform from 'ShaderType' to raw 'GLenum' representation.
marshalShaderType :: ShaderType -> GLenum
marshalShaderType ComputeShader = GL_COMPUTE_SHADER
marshalShaderType VertexShader = GL_VERTEX_SHADER
marshalShaderType TessControlShader = GL_TESS_CONTROL_SHADER
marshalShaderType TessEvaluationShader = GL_TESS_EVALUATION_SHADER
marshalShaderType GeometryShader = GL_GEOMETRY_SHADER
marshalShaderType FragmentShader = GL_FRAGMENT_SHADER

-- | Creates shader object of given type and compiles it with given source.
-- Shader object must be deleted after use.
-- Returns error message on failure.
createShader :: String -> ShaderType -> Script Shader
createShader source shaderType = do
  shader <- glCreateShader $ marshalShaderType shaderType
  when (shader == 0) .
    throwT $ "Error creating shader of type " ++ show shaderType
  scriptIO . withCAString source $ \str ->
    withArray [str] $ \srcArray -> glShaderSource shader 1 srcArray nullPtr
  glCompileShader shader
  return shader
