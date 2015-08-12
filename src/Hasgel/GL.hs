{-# LANGUAGE ScopedTypeVariables #-}

module Hasgel.GL (
  module GL,
  VertexArray, Texture, Index(..), Buffer, BufferData(..),
  Query, GLError(..),
  getError, throwError,
  vertexAttrib4f, vertexAttrib3f,
  clearBufferfv, clearDepthBuffer, bufferData,
  drawElements,
  beginQuery, endQuery, getQueryResult, withQuery, queryCounter
) where

import Control.Exception (Exception, throwIO)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable)
import Data.Word (Word64)
import Foreign (Ptr, Storable (..), alloca, allocaArray, castPtr, peek,
                peekArray, with, withArray, withArrayLen)
import Graphics.GL.Core45
import Graphics.GL.Types

import Hasgel.GL.Object as GL
import Hasgel.GL.Program as GL
import Hasgel.GL.Shader as GL
import Hasgel.GL.Uniform as GL

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

newtype VertexArray = VertexArray GLuint deriving (Show)

instance Object VertexArray where
  deletes vaos = liftIO . withArrayLen (object <$> vaos) $ \n ptr ->
    glDeleteVertexArrays (fromIntegral n) ptr
  object (VertexArray obj) = obj

instance Gen VertexArray where
  gens n = liftIO . allocaArray n $ \vaoPtr -> do
    glGenVertexArrays (fromIntegral n) vaoPtr
    map VertexArray <$> peekArray n vaoPtr

newtype Texture = Texture GLuint deriving (Show)

instance Object Texture where
  deletes texs = liftIO . withArrayLen (object <$> texs) $ \n ptr ->
    glDeleteTextures (fromIntegral n) ptr
  object (Texture obj) = obj

instance Gen Texture where
  gens n = liftIO . allocaArray n $ \texPtr -> do
    glGenTextures (fromIntegral n) texPtr
    map Texture <$> peekArray n texPtr

newtype Buffer = Buffer GLuint deriving (Show)

instance Object Buffer where
  object (Buffer obj) = obj
  deletes bufs = liftIO . withArrayLen (object <$> bufs) $ \n ptr ->
    glDeleteBuffers (fromIntegral n) ptr

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

newtype Index = Index GLuint deriving (Show)

newtype Query = Query GLuint deriving (Show)

instance Object Query where
  object (Query q) = q
  deletes qs = liftIO . withArrayLen (object <$> qs) $ \n ptr ->
    glDeleteQueries (fromIntegral n) ptr

instance Gen Query where
  gens n = liftIO . allocaArray n $ \ptr -> do
    glGenQueries (fromIntegral n) ptr
    map Query <$> peekArray n ptr

type QueryTarget = GLenum
type ClearBuffer = GLenum
type DrawMode = GLenum
type DataType = GLenum

type DrawBuffer = GLint

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

vertexAttrib4f :: MonadIO m => Index ->
                  GLfloat -> GLfloat -> GLfloat -> GLfloat -> m ()
vertexAttrib4f (Index index) = glVertexAttrib4f index

vertexAttrib3f :: MonadIO m => Index -> GLfloat -> GLfloat -> GLfloat -> m ()
vertexAttrib3f (Index index) = glVertexAttrib3f index

clearBufferfv :: MonadIO m => ClearBuffer -> DrawBuffer -> [GLfloat] -> m ()
clearBufferfv buffer drawBuffer values =
  liftIO . withArray values $ glClearBufferfv buffer drawBuffer

clearDepthBuffer :: MonadIO m => GLfloat -> m ()
clearDepthBuffer value = liftIO . with value $ glClearBufferfv GL_DEPTH 0

bufferData :: (MonadIO m, BufferData a) =>
              BufferTarget -> a -> BufferUsage -> m ()
bufferData target values usage =
  let bytes = fromIntegral $ sizeOfData values
  in withDataPtr values $ \ptr -> glBufferData target bytes ptr usage

drawElements :: (Integral a, MonadIO m) =>
                DrawMode -> a -> DataType -> Ptr () -> m ()
drawElements mode count = glDrawElements mode (fromIntegral count)

beginQuery :: MonadIO m => QueryTarget -> Query -> m ()
beginQuery target = glBeginQuery target . object

endQuery :: MonadIO m => QueryTarget -> m ()
endQuery = glEndQuery

getQueryResult :: MonadIO m => Query -> m Word64
getQueryResult (Query q) = liftIO . alloca $ \ptr -> do
  glGetQueryObjectui64v q GL_QUERY_RESULT ptr
  peek ptr

withQuery :: MonadIO m => QueryTarget -> Query -> m a -> m a
withQuery target q action = do
  beginQuery target q
  r <- action
  endQuery target
  pure r

queryCounter :: MonadIO m => Query -> m ()
queryCounter q = glQueryCounter (object q) GL_TIMESTAMP
