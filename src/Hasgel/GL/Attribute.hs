{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hasgel.GL.Attribute (
  Index(..), Layout, VertexArray, VertexArrayState, layout,
  bindVertexArray, setVertexArray, attrib, attrib3f, indexBuffer,
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

-- | A vertex array object. Owns buffers associated with its state.
data VertexArray = VertexArray
  { vaObject :: GLuint
  , vaBuffers :: [Buffer]
  } deriving (Show)

instance Object VertexArray where
  deletes vaos = do
    deletesWith glDeleteVertexArrays vaos
    sequence_ $ mapM_ delete <$> map vaBuffers vaos
  object = vaObject

instance Gen VertexArray where
  gens = gensWith glGenVertexArrays vertexArray

newtype VertexArrayState m a =
  VertexArrayState { unVA :: StateT (Int, VertexArray) m a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans VertexArrayState where
  lift = VertexArrayState . lift

vertexArray :: GLuint -> VertexArray
vertexArray obj = VertexArray obj []

bindVertexArray :: MonadIO m => VertexArray -> m ()
bindVertexArray = glBindVertexArray . object

-- | Set the vertex array state through monadic interface.
-- Return the modified vertex array.
setVertexArray :: MonadIO m => VertexArray -> VertexArrayState m a -> m VertexArray
setVertexArray vao actions = do
  bindVertexArray vao
  snd <$> execStateT (unVA actions) (0, vao)

-- | Fill and enable the next available vertex attribute.
-- A buffer is created to store the vertex data.
attrib :: (Storable a, Attribute a, MonadIO m) => [a] -> VertexArrayState m ()
attrib attr = VertexArrayState $ do
  (ix, vao) <- get
  buf <- gen
  bindBuffer ArrayBuffer buf $
    bufferData attr StaticDraw
  vertexAttribPointer (Index (fromIntegral ix)) $
    layout (head attr) GL_FALSE 0 nullPtr
  let bufs = vaBuffers vao
  put (ix + 1, vao { vaBuffers = buf:bufs })

-- | Set the current vertex attribute. This overrides any set vertex array.
attrib3f :: MonadIO m => GLfloat -> GLfloat -> GLfloat -> VertexArrayState m ()
attrib3f a b c = VertexArrayState $ do
  ix  <- gets fst
  glDisableVertexAttribArray (fromIntegral ix)
  glVertexAttrib3f (fromIntegral ix) a b c
  modify $ first (+1)

-- | Set the indexes for element array buffer.
indexBuffer :: (MonadIO m, BufferData a) => a -> VertexArrayState m ()
indexBuffer ixs = VertexArrayState $ do
  buf <- gen
  bindBuffer ElementArrayBuffer buf $
    bufferData ixs StaticDraw
  modify . second $ \vao -> vao { vaBuffers = buf : vaBuffers vao }

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
