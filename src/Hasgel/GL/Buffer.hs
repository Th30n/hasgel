{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Hasgel.GL.Buffer (
  Buffer, BufferData(..), BufferTarget(..), BufferUsage(..), BoundBuffer,
  bindBuffer, bufferData,
  clearBufferfv, clearDepthBuffer
) where

import Control.Monad.Reader
import Foreign (Ptr, Storable (..), allocaArray, castPtr, peekArray, with,
                withArray, withArrayLen)

import Graphics.GL.Core45
import Graphics.GL.Types

import Hasgel.GL.Object

newtype Buffer = Buffer GLuint deriving (Show)

instance Object Buffer where
  object (Buffer obj) = obj
  deletes bufs = liftIO . withArrayLen (object <$> bufs) $ \n ptr ->
    glDeleteBuffers (fromIntegral n) ptr

instance Gen Buffer where
  gens n = liftIO . allocaArray n $ \ptr -> do
    glGenBuffers (fromIntegral n) ptr
    map Buffer <$> peekArray n ptr

data BufferTarget =
  ArrayBuffer
  | ElementArrayBuffer
  deriving (Show, Ord, Eq)

marshalBufferTarget :: BufferTarget -> GLenum
marshalBufferTarget ArrayBuffer = GL_ARRAY_BUFFER
marshalBufferTarget ElementArrayBuffer = GL_ELEMENT_ARRAY_BUFFER

data BufferUsage =
  StaticDraw
  deriving (Show, Ord, Eq)

marshalBufferUsage :: BufferUsage -> GLenum
marshalBufferUsage StaticDraw = GL_STATIC_DRAW

class BufferData a where
  withDataPtr :: (BufferData a, MonadIO m) => a -> (Ptr () -> IO b) -> m b
  sizeOfData :: BufferData a => a -> Int

instance Storable a => BufferData [a] where
  withDataPtr vs action = liftIO . withArray vs $ action . castPtr
  sizeOfData vs = length vs * sizeOf (undefined :: a)

type DrawBuffer = GLint
type ClearBuffer = GLenum

newtype BoundBuffer a =
  BoundBuffer { withBoundBuffer :: ReaderT BufferTarget IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

bindBuffer :: BufferTarget -> Buffer -> BoundBuffer a -> IO a
bindBuffer target buffer actions = do
  glBindBuffer (marshalBufferTarget target) $ object buffer
  runReaderT (withBoundBuffer actions) target

bufferData :: BufferData a => a -> BufferUsage -> BoundBuffer ()
bufferData values usage = BoundBuffer $ ask >>= \target ->
  let bytes = fromIntegral $ sizeOfData values
  in withDataPtr values $ \ptr ->
  glBufferData (marshalBufferTarget target) bytes ptr (marshalBufferUsage usage)

clearBufferfv :: MonadIO m => ClearBuffer -> DrawBuffer -> [GLfloat] -> m ()
clearBufferfv buffer drawBuffer values =
  liftIO . withArray values $ glClearBufferfv buffer drawBuffer

clearDepthBuffer :: MonadIO m => GLfloat -> m ()
clearDepthBuffer value = liftIO . with value $ glClearBufferfv GL_DEPTH 0
