{-# LANGUAGE ScopedTypeVariables #-}

module Hasgel.GL (
  module Hasgel.GL.Object,
  module Hasgel.GL.Shader,
  Program(..), GLError(..),
  VertexArray, Texture, Index(..), Buffer, BufferData(..),
  Query,
  linkProgram, getError, throwError,
  getProgramiv, getProgramInfoLog, useProgram,
  vertexAttrib4f, clearBufferfv, clearDepthBuffer, bufferData,
  getUniformLocation, uniform4f, uniformMatrix4f, drawElements,
  beginQuery, endQuery, getQueryResult
) where

import Control.Exception (Exception, throwIO)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable)
import Data.Word (Word32)
import Foreign (Ptr, Storable (..), alloca, allocaArray, castPtr, nullPtr, peek,
                peekArray, with, withArray, withArrayLen)
import Foreign.C (peekCString, withCAString)
import Graphics.GL.Core45
import Graphics.GL.Types

import Hasgel.GL.Object
import Hasgel.GL.Shader

-- | Program object.
newtype Program = Program GLuint deriving (Show)

instance Object Program where
  delete = glDeleteProgram . object
  object (Program obj) = obj

instance Gen Program where
  gen = createProgram

createProgram :: MonadIO m => m Program
createProgram = do
  program <- glCreateProgram
  when (program == 0) . liftIO . throwIO $
    CreationError "Error creating program."
  pure $ Program program

attachShader :: MonadIO m => Program -> Shader -> m ()
attachShader (Program prog) (Shader sh) = glAttachShader prog sh

-- | Creates a program object and links it with compiled shader objects.
-- Throws 'CreationError' or 'LinkError' on failure.
linkProgram :: MonadIO m => [Shader] -> m Program
linkProgram ss = do
  program <- gen
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

newtype Index = Index GLuint deriving (Show)

vertexAttrib4f :: MonadIO m => Index ->
                  GLfloat -> GLfloat -> GLfloat -> GLfloat -> m ()
vertexAttrib4f (Index index) = glVertexAttrib4f index

type ClearBuffer = GLenum
type DrawBuffer = GLint

clearBufferfv :: MonadIO m => ClearBuffer -> DrawBuffer -> [GLfloat] -> m ()
clearBufferfv buffer drawBuffer values =
  liftIO . withArray values $ glClearBufferfv buffer drawBuffer

clearDepthBuffer :: MonadIO m => GLfloat -> m ()
clearDepthBuffer value = liftIO . with value $ glClearBufferfv GL_DEPTH 0

newtype Buffer = Buffer GLuint deriving (Show)

instance Object Buffer where
  object (Buffer obj) = obj
  delete (Buffer obj) = liftIO . with obj $ glDeleteBuffers 1

instance Gen Buffer where
  gens n = liftIO . allocaArray n $ \ptr -> do
    glGenBuffers (fromIntegral n) ptr
    map Buffer <$> peekArray n ptr

type BufferTarget = GLenum
type BufferUsage = GLenum

class BufferData a where
  withDataPtr :: (BufferData a, MonadIO m) => a -> (Ptr () -> IO b) -> m b
  sizeOfData :: BufferData a => a -> Int

instance Storable a => BufferData [a] where
  withDataPtr vs action = liftIO . withArray vs $ action . castPtr
  sizeOfData vs = length vs * sizeOf (undefined :: a)

bufferData :: (MonadIO m, BufferData a) =>
              BufferTarget -> a -> BufferUsage -> m ()
bufferData target values usage =
  let bytes = fromIntegral $ sizeOfData values
  in withDataPtr values $ \ptr -> glBufferData target bytes ptr usage


newtype UniformLocation = UniformLocation GLint

getUniformLocation :: MonadIO m => Program -> String -> m (Maybe UniformLocation)
getUniformLocation program name =
  liftIO . withCAString name $ \ptr -> do
    loc <- glGetUniformLocation (object program) ptr
    pure $ if loc == -1 then Nothing else Just $ UniformLocation loc

uniform4f :: MonadIO m => UniformLocation ->
             GLfloat -> GLfloat -> GLfloat -> GLfloat -> m ()
uniform4f (UniformLocation loc) = glUniform4f loc

uniformMatrix4f :: MonadIO m =>
                   UniformLocation -> GLsizei -> GLboolean -> Ptr GLfloat -> m ()
uniformMatrix4f (UniformLocation loc) = glUniformMatrix4fv loc

type DrawMode = GLenum
type DataType = GLenum

drawElements :: (Integral a, MonadIO m) =>
                DrawMode -> a -> DataType -> Ptr () -> m ()
drawElements mode count = glDrawElements mode (fromIntegral count)

newtype Query = Query GLuint

instance Object Query where
  object (Query q) = q
  deletes qs = liftIO . withArrayLen (object <$> qs) $ \n ptr ->
    glDeleteQueries (fromIntegral n) ptr

instance Gen Query where
  gens n = liftIO . allocaArray n $ \ptr -> do
    glGenQueries (fromIntegral n) ptr
    map Query <$> peekArray n ptr

type QueryTarget = GLenum

beginQuery :: MonadIO m => QueryTarget -> Query -> m ()
beginQuery target = glBeginQuery target . object

endQuery :: MonadIO m => QueryTarget -> m ()
endQuery = glEndQuery

getQueryResult :: MonadIO m => Query -> m Word32
getQueryResult (Query q) = liftIO . alloca $ \ptr -> do
  glGetQueryObjectuiv q GL_QUERY_RESULT ptr
  peek ptr
