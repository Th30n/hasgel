module Hasgel.GL (
  Shader(..), Program(..), ShaderType(..),
  compileShader, linkProgram
) where

import Control.Error
import Control.Monad (when)
import Foreign.C.String (withCAString)
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr (nullPtr)
import Graphics.GL.Core45
import Graphics.GL.Types

-- | Shader object.
newtype Shader = Shader GLuint deriving (Show)

-- | Program object.
newtype Program = Program GLuint deriving (Show)

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
compileShader :: String -> ShaderType -> Script Shader
compileShader source shaderType = do
  shader <- glCreateShader $ marshalShaderType shaderType
  when (shader == 0) .
    throwT $ "Error creating shader of type " ++ show shaderType
  scriptIO . withCAString source $ \str ->
    withArray [str] $ \srcArray -> glShaderSource shader 1 srcArray nullPtr
  glCompileShader shader
  return $ Shader shader

-- | Creates a program object and links it with compiled shader objects.
-- Returns error message on failure.
linkProgram :: [Shader] -> Script Program
linkProgram ss = do
  program <- glCreateProgram
  when (program == 0) $ throwT "Error creating program."
  mapM_ (\(Shader sh) -> glAttachShader program sh) ss
  glLinkProgram program
  return $ Program program

