{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hasgel.GL.Attribute (
  Index(..), Layout, VA, layout,
  setVertexArray, attrib, attrib3f, indexBuffer,
  vertexAttrib3f
) where

import Control.Arrow (first, second)
import Foreign (Ptr, nullPtr)
import Foreign.Storable (Storable)

import Control.Monad.State

import Graphics.GL.Core45
import Graphics.GL.Types
import qualified Linear as L

import Hasgel.GL.Buffer
import Hasgel.GL.Object

newtype Index = Index GLuint deriving (Show)

type Components = GLint
type Type = GLenum
type Normalize = GLboolean
type Stride = GLsizei
type Offset = Ptr ()

data Layout = Layout Components Type Normalize Stride Offset

class Attribute a where
  components :: a -> Components
  glType :: a -> Type

instance Attribute (L.V3 Float) where
  components = const 3
  glType = const GL_FLOAT

instance Attribute (L.V2 Float) where
  components = const 2
  glType = const GL_FLOAT

newtype VA a = VA { unVA :: StateT (Int, [Buffer]) IO a }
             deriving (Functor, Applicative, Monad, MonadIO)

-- | Set the vertex array state through monadic 'VA' interface.
-- Returns the list of buffers that were created in order to fill generic
-- vertex attributes.
setVertexArray :: VertexArray -> VA a -> IO [Buffer]
setVertexArray vao actions = do
  bindVertexArray vao
  snd <$> execStateT (unVA actions) (0, [])

-- | Fill and enable the next available vertex attribute.
attrib :: (Storable a, Attribute a) => [a] -> VA ()
attrib attr = VA $ do
  (ix, bufs) <- get
  buf <- gen
  liftIO . bindBuffer ArrayBuffer buf $
    bufferData attr StaticDraw
  vertexAttribPointer (Index (fromIntegral ix)) $
    layout (head attr) GL_FALSE 0 nullPtr
  put (ix + 1, buf:bufs)

attrib3f :: GLfloat -> GLfloat -> GLfloat -> VA ()
attrib3f a b c = VA $ do
  ix  <- gets fst
  glDisableVertexAttribArray (fromIntegral ix)
  glVertexAttrib3f (fromIntegral ix) a b c
  modify $ first (+1)

-- | Set the indexes for element array buffer.
indexBuffer :: BufferData a => a -> VA ()
indexBuffer ixs = VA $ do
  buf <- gen
  liftIO . bindBuffer ElementArrayBuffer buf $
    bufferData ixs StaticDraw
  modify $ second (buf:)

layout :: Attribute a => a -> Normalize -> Stride -> Offset -> Layout
layout attr = Layout (components attr) (glType attr)

vertexAttrib4f :: MonadIO m => Index ->
                  GLfloat -> GLfloat -> GLfloat -> GLfloat -> m ()
vertexAttrib4f (Index index) = glVertexAttrib4f index

vertexAttrib3f :: MonadIO m => Index -> GLfloat -> GLfloat -> GLfloat -> m ()
vertexAttrib3f (Index index) = glVertexAttrib3f index

vertexAttribPointer :: MonadIO m => Index -> Layout -> m ()
vertexAttribPointer (Index index) (Layout size typ normalize stride offset) = do
  glEnableVertexAttribArray index
  glVertexAttribPointer index size typ normalize stride offset
