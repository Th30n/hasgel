module Hasgel.GL (
  Shader(..), Program(..), ShaderType(..),
  ShaderException(..), GLError(..),
  Object(..), Gen(..), VertexArray, Texture,
  compileShader, linkProgram, getError, throwError, getShaderiv,
  getShaderInfoLog, getProgramiv, getProgramInfoLog, useProgram
) where

import Control.Exception (Exception, throwIO)
import Control.Monad (replicateM, when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Typeable (Typeable)
import Foreign.C.String (peekCString, withCAString)
import Foreign.Marshal (alloca, allocaArray, peekArray, with, withArray,
                        withArrayLen)
import Foreign.Ptr (nullPtr)
import Foreign.Storable (peek)
import Graphics.GL.Core45
import Graphics.GL.Types

-- | Shader object.
newtype Shader = Shader GLuint deriving (Show)

instance Object Shader where
  delete = glDeleteShader . object
  object (Shader obj) = obj

-- | Program object.
newtype Program = Program GLuint deriving (Show)

instance Object Program where
  delete = glDeleteProgram . object
  object (Program obj) = obj

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

data ShaderException =
  CreationError String |
  CompileError String |
  LinkError String
  deriving (Eq, Show, Typeable)

instance Exception ShaderException

createShader :: MonadIO m => ShaderType -> m Shader
createShader shaderType = do
  shader <- glCreateShader $ marshalShaderType shaderType
  when (shader == 0) $ liftIO . throwIO .
    CreationError $ "Error creating shader of type " <> show shaderType
  pure $ Shader shader

-- | Creates shader object of given type and compiles it with given source.
-- Shader object must be deleted after use.
-- Throws 'CompileError' or 'CreationError' on failure.
compileShader :: MonadIO m => String -> ShaderType -> m Shader
compileShader source shaderType = do
  shader <- createShader shaderType
  liftIO . withCAString source $ \str ->
    withArray [str] $ \srcArray ->
      glShaderSource (object shader) 1 srcArray nullPtr
  glCompileShader $ object shader
  status <- getShaderiv shader GL_COMPILE_STATUS
  when (status == GL_FALSE) $ do
    compileLog <- getShaderInfoLog shader
    delete shader
    liftIO . throwIO $ CompileError compileLog
  pure shader

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

createProgram :: MonadIO m => m Program
createProgram = do
  program <- glCreateProgram
  when (program == 0) . liftIO . throwIO $
    CreationError "Error creating program."
  pure $ Program program

attachShader :: MonadIO m => Program -> Shader -> m ()
attachShader (Program prog) (Shader sh) = glAttachShader prog sh

-- | Creates a program object and links it with compiled shader objects.
-- Throws 'ProgramError' or 'LinkError' on failure.
linkProgram :: MonadIO m => [Shader] -> m Program
linkProgram ss = do
  program <- createProgram
  mapM_ (attachShader program) ss
  glLinkProgram $ object program
  status <- getProgramiv program GL_LINK_STATUS
  when (status == GL_FALSE) $ do
    linkLog <- getProgramInfoLog program
    delete program
    liftIO . throwIO $ LinkError linkLog
  pure program

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

useProgram :: MonadIO m => Program -> m ()
useProgram (Program program) = glUseProgram program

-- | Enumeration of possible error codes in OpenGL.
data GLError =
  NoError
  | InvalidEnum
  | InvalidValue
  | InvalidOperation
  | InvalidFramebufferOperation
  | OutOfMemory
  | StackUnderflow
  | StackOverflow
  deriving (Show, Eq, Typeable)
instance Exception GLError

-- | Converts from numerical representation to 'GLError'
unmarshalGLError :: GLenum -> Maybe GLError
unmarshalGLError val
  | val == GL_NO_ERROR = Just NoError
  | val == GL_INVALID_ENUM = Just InvalidEnum
  | val == GL_INVALID_OPERATION = Just InvalidOperation
  | val == GL_INVALID_FRAMEBUFFER_OPERATION = Just InvalidFramebufferOperation
  | val == GL_OUT_OF_MEMORY = Just OutOfMemory
  | val == GL_STACK_UNDERFLOW = Just StackUnderflow
  | val == GL_STACK_OVERFLOW = Just StackOverflow
  | otherwise = Nothing

-- | Returns 'GLError'.
getError :: MonadIO m => m GLError
getError = (fromMaybe NoError . unmarshalGLError) <$> glGetError

-- | Throws an 'GLError' on error.
throwError :: MonadIO m => m ()
throwError = getError >>= \e -> when (e /= NoError) $ liftIO $ throwIO e

class Object a where
  {-# MINIMAL (delete | deletes), object #-}
  delete :: (Object a, MonadIO m) => a -> m ()
  delete = deletes . pure
  deletes :: (Object a, MonadIO m) => [a] -> m ()
  deletes = mapM_ delete
  object :: Object a => a -> GLuint

class Object a => Gen a where
  {-# MINIMAL gen | gens #-}
  gen :: (Object a, MonadIO m) => m a
  gen = head <$> gens 1
  gens :: (Object a, MonadIO m) => Int -> m [a]
  gens n = replicateM n gen

newtype VertexArray = VertexArray GLuint deriving (Show)

instance Object VertexArray where
  delete (VertexArray obj) = liftIO . with obj $ glDeleteVertexArrays 1
  object (VertexArray obj) = obj

instance Gen VertexArray where
  gen = liftIO . alloca $ \vaoPtr -> do
    glGenVertexArrays 1 vaoPtr
    VertexArray . head <$> peekArray 1 vaoPtr

newtype Texture = Texture GLuint deriving (Show)

instance Object Texture where
  deletes texs = liftIO . withArrayLen (object <$> texs) $ \n ptr ->
    glDeleteTextures (fromIntegral n) ptr
  object (Texture obj) = obj

instance Gen Texture where
  gens n = liftIO . allocaArray n $ \texPtr -> do
    glGenTextures (fromIntegral n) texPtr
    map Texture <$> peekArray n texPtr
