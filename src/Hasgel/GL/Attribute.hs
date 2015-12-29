{-# LANGUAGE FlexibleInstances #-}

module Hasgel.GL.Attribute (
  Index(..), Layout, layout, vertexAttrib3f,
  vertexAttribPointer
) where

import Foreign (Ptr)

import Control.Monad.IO.Class (MonadIO (..))

import Graphics.GL.Core45
import Graphics.GL.Types
import qualified Linear as L

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
