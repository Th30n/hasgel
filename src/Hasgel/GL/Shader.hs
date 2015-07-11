module Hasgel.GL.Shader (
  Shader(..), ShaderException(..), ShaderType(..),
  compileShader, recompileShader
) where

import Control.Exception (Exception (..), onException, throwIO)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Monoid ((<>))
import Data.Typeable (Typeable)
import Foreign (alloca, allocaArray, nullPtr, peek, withArray)
import Foreign.C.String (peekCString, withCAString)

import Graphics.GL.Core45
import Graphics.GL.Types

import Hasgel.GL.Object

-- | Shader object.
newtype Shader = Shader GLuint deriving (Show)

instance Object Shader where
  delete = glDeleteShader . object
  object (Shader obj) = obj

data ShaderType =
  ComputeShader
  | VertexShader
  | TessControlShader
  | TessEvaluationShader
  | GeometryShader
  | FragmentShader
  deriving (Show, Ord, Eq)

-- | Transform from 'ShaderType' to raw 'GLenum' representation.
marshalShaderType :: ShaderType -> GLenum
marshalShaderType ComputeShader = GL_COMPUTE_SHADER
marshalShaderType VertexShader = GL_VERTEX_SHADER
marshalShaderType TessControlShader = GL_TESS_CONTROL_SHADER
marshalShaderType TessEvaluationShader = GL_TESS_EVALUATION_SHADER
marshalShaderType GeometryShader = GL_GEOMETRY_SHADER
marshalShaderType FragmentShader = GL_FRAGMENT_SHADER

data ShaderException =
  CreationError String |
  CompileError String |
  LinkError String
  deriving (Eq, Show, Typeable)

instance Exception ShaderException where
  displayException (CreationError msg) = "Creation error: " <> msg
  displayException (CompileError msg) = "Compile error: " <> msg
  displayException (LinkError msg) = "Link error: " <> msg

createShader :: MonadIO m => ShaderType -> m Shader
createShader shaderType = do
  shader <- glCreateShader $ marshalShaderType shaderType
  when (shader == 0) $ liftIO . throwIO .
    CreationError $ "Error creating shader of type " <> show shaderType
  pure $ Shader shader

shaderSource :: MonadIO m => Shader -> String -> m ()
shaderSource shader source =
  liftIO . withCAString source $ \str ->
    withArray [str] $ \srcArray ->
      glShaderSource (object shader) 1 srcArray nullPtr

-- | Creates shader object of given type and compiles it with given source.
-- Shader object must be deleted after use.
-- Throws 'CompileError' or 'CreationError' on failure.
compileShader :: MonadIO m => String -> ShaderType -> m Shader
compileShader source shaderType = do
  shader <- createShader shaderType
  liftIO . onException (recompileShader shader source) $ delete shader

recompileShader :: MonadIO m => Shader -> String -> m Shader
recompileShader shader source = do
  shaderSource shader source
  glCompileShader $ object shader
  status <- getShaderCompileStatus shader
  case status of
    Right _ -> pure shader
    Left compileLog -> liftIO . throwIO $ CompileError compileLog

getShaderCompileStatus :: MonadIO m => Shader -> m (Either String ())
getShaderCompileStatus shader = do
  status <- getShaderiv shader GL_COMPILE_STATUS
  case status of
    GL_TRUE -> pure $ Right ()
    _ -> Left <$> getShaderInfoLog shader

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

