{-# LANGUAGE ScopedTypeVariables #-}

module Hasgel.GL.Buffer (
  Buffer, BufferData(..), BufferTarget(..), BufferUsage(..), BoundBuffer,
  bindBuffer, bufferData,
  clearBufferfv, clearDepthBuffer
) where

import Control.Monad.IO.Class (MonadIO (..))
import Foreign (Ptr, Storable (..), castPtr, with, withArray)

import Graphics.GL.Core45
import Graphics.GL.Types

import Hasgel.GL.Object

newtype Buffer = Buffer GLuint deriving (Show)

instance Object Buffer where
  object (Buffer obj) = obj
  deletes = deletesWith glDeleteBuffers

instance Gen Buffer where
  gens = gensWith glGenBuffers Buffer

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

type BoundBuffer = WithUse BufferTarget

bindBuffer :: BufferTarget -> Buffer -> BoundBuffer a -> IO a
bindBuffer target buffer actions = do
  glBindBuffer (marshalBufferTarget target) $ object buffer
  runWithUse actions target

bufferData :: BufferData a => a -> BufferUsage -> BoundBuffer ()
bufferData values usage = askUse >>= \target ->
  let bytes = fromIntegral $ sizeOfData values
  in withDataPtr values $ \ptr ->
  glBufferData (marshalBufferTarget target) bytes ptr (marshalBufferUsage usage)

clearBufferfv :: MonadIO m => ClearBuffer -> DrawBuffer -> [GLfloat] -> m ()
clearBufferfv buffer drawBuffer values =
  liftIO . withArray values $ glClearBufferfv buffer drawBuffer

clearDepthBuffer :: MonadIO m => GLfloat -> m ()
clearDepthBuffer value = liftIO . with value $ glClearBufferfv GL_DEPTH 0
