module Hasgel.GL (
  Shader(..), Program(..), ShaderType(..), ShaderError(..), CompileError(..),
  ProgramError(..), LinkError(..), ErrorFlag(..),
  compileShader, linkProgram, getError, throwError, getShaderiv,
  getShaderInfoLog, getProgramiv, getProgramInfoLog
) where

import Control.Exception (Exception, throwIO)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Typeable (Typeable)
import Foreign.C.String (peekCString, withCAString)
import Foreign.Marshal (alloca, allocaArray, withArray)
import Foreign.Ptr (nullPtr)
import Foreign.Storable (peek)
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

newtype ShaderError = ShaderError String deriving (Show, Typeable)
instance Exception ShaderError

newtype CompileError = CompileError String deriving (Show, Typeable)
instance Exception CompileError

newtype ProgramError = ProgramError String deriving (Show, Typeable)
instance Exception ProgramError

newtype LinkError = LinkError String deriving (Show, Typeable)
instance Exception LinkError

-- | Creates shader object of given type and compiles it with given source.
-- Shader object must be deleted after use.
-- Throws 'CompileError' or 'ShaderError' on failure.
compileShader :: MonadIO m => String -> ShaderType -> m Shader
compileShader source shaderType = do
  shader <- glCreateShader $ marshalShaderType shaderType
  when (shader == 0) $ liftIO .
    throwIO . ShaderError $ "Error creating shader of type " <> show shaderType
  liftIO . withCAString source $ \str ->
    withArray [str] $ \srcArray -> glShaderSource shader 1 srcArray nullPtr
  glCompileShader shader
  status <- getShaderiv (Shader shader) GL_COMPILE_STATUS
  when (status == GL_FALSE) $ do
    compileLog <- getShaderInfoLog $ Shader shader
    glDeleteShader shader
    liftIO . throwIO $ CompileError compileLog
  return $ Shader shader

-- | Returns the information log for a shader object.
getShaderInfoLog :: MonadIO m => Shader -> m String
getShaderInfoLog s@(Shader shader) = do
  logSize <- getShaderiv s GL_INFO_LOG_LENGTH
  liftIO . allocaArray (fromIntegral logSize) $ \infoLog -> do
    glGetShaderInfoLog shader logSize nullPtr infoLog
    peekCString infoLog

-- | Returns a parameter from a shader object.
getShaderiv :: MonadIO m => Shader -> GLenum -> m GLint
getShaderiv (Shader shader) pname =
  liftIO . alloca $ \params -> do
    glGetShaderiv shader pname params
    peek params

-- | Creates a program object and links it with compiled shader objects.
-- Throws 'ProgramError' or 'LinkError' on failure.
linkProgram :: MonadIO m => [Shader] -> m Program
linkProgram ss = do
  program <- glCreateProgram
  when (program == 0) . liftIO . throwIO $ ProgramError "Error creating program."
  mapM_ (\(Shader sh) -> glAttachShader program sh) ss
  glLinkProgram program
  status <- getProgramiv (Program program) GL_LINK_STATUS
  when (status == GL_FALSE) $ do
    linkLog <- getProgramInfoLog $ Program program
    glDeleteProgram program
    liftIO . throwIO $ LinkError linkLog
  return $ Program program

-- | Returns the information log for a program object.
getProgramInfoLog :: MonadIO m => Program -> m String
getProgramInfoLog p@(Program program) = do
  logSize <- getProgramiv p GL_INFO_LOG_LENGTH
  liftIO . allocaArray (fromIntegral logSize) $ \infoLog -> do
    glGetProgramInfoLog program logSize nullPtr infoLog
    peekCString infoLog

-- | Returns a parameter from a program object.
getProgramiv :: MonadIO m => Program -> GLenum -> m GLint
getProgramiv (Program program) pname =
  liftIO . alloca $ \params -> do
    glGetProgramiv program pname params
    peek params

-- | Enumeration of possible error codes in OpenGL.
data ErrorFlag =
  NoError
  | InvalidEnum
  | InvalidValue
  | InvalidOperation
  | InvalidFramebufferOperation
  | OutOfMemory
  | StackUnderflow
  | StackOverflow
  deriving (Show, Eq, Typeable)
instance Exception ErrorFlag

-- | Converts from numerical representation to 'ErrorFlag'
unmarshalErrorFlag :: GLenum -> Maybe ErrorFlag
unmarshalErrorFlag val
  | val == GL_NO_ERROR = Just NoError
  | val == GL_INVALID_ENUM = Just InvalidEnum
  | val == GL_INVALID_OPERATION = Just InvalidOperation
  | val == GL_INVALID_FRAMEBUFFER_OPERATION = Just InvalidFramebufferOperation
  | val == GL_OUT_OF_MEMORY = Just OutOfMemory
  | val == GL_STACK_UNDERFLOW = Just StackUnderflow
  | val == GL_STACK_OVERFLOW = Just StackOverflow
  | otherwise = Nothing

-- | Returns 'ErrorFlag'.
getError :: MonadIO m => m ErrorFlag
getError = (fromMaybe NoError . unmarshalErrorFlag) <$> glGetError

-- | Throws an 'ErrorFlag' on error.
throwError :: MonadIO m => m ()
throwError = getError >>= \e -> when (e /= NoError) $ liftIO $ throwIO e
