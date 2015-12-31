module Hasgel.GL.Framebuffer (
  Framebuffer, FramebufferTarget(..), Renderbuffer, Texture,
  bindFramebuffer, framebufferTexture2D, framebufferDepth, drawBuffers
) where

import Control.Monad.IO.Class (MonadIO (..))
import Foreign (withArrayLen)

import Graphics.GL.Core45
import Graphics.GL.Types

import Hasgel.GL.Object

newtype Framebuffer = Framebuffer GLuint deriving (Show)

instance Object Framebuffer where
  object (Framebuffer obj) = obj
  deletes = deletesWith glDeleteFramebuffers

instance Gen Framebuffer where
  gens = gensWith glGenFramebuffers Framebuffer

data FramebufferTarget =
  FramebufferTarget
  | DrawFramebuffer
  | ReadFramebuffer
  deriving (Show, Ord, Eq)

marshalFramebufferTarget :: FramebufferTarget -> GLenum
marshalFramebufferTarget FramebufferTarget = GL_FRAMEBUFFER
marshalFramebufferTarget DrawFramebuffer = GL_DRAW_FRAMEBUFFER
marshalFramebufferTarget ReadFramebuffer = GL_READ_FRAMEBUFFER

newtype Renderbuffer = Renderbuffer GLuint deriving (Show)

instance Object Renderbuffer where
  object (Renderbuffer obj) = obj
  deletes = deletesWith  glDeleteRenderbuffers

instance Gen Renderbuffer where
  gens = gensWith glGenRenderbuffers Renderbuffer

newtype Texture = Texture GLuint deriving (Show)

instance Object Texture where
  deletes = deletesWith glDeleteTextures
  object (Texture obj) = obj

instance Gen Texture where
  gens = gensWith glGenTextures Texture

type BoundFramebuffer = WithUse FramebufferTarget

-- | Runs actions with bound framebuffer. Framebuffer binding reverts to
-- default at the end.
bindFramebuffer :: FramebufferTarget -> Framebuffer -> BoundFramebuffer a -> IO a
bindFramebuffer target buffer actions = do
  glBindFramebuffer (marshalFramebufferTarget target) $ object buffer
  res <- runWithUse actions target
  glBindFramebuffer (marshalFramebufferTarget target) 0
  pure res

type Attachment = GLenum
type TexTarget = GLenum
type Level = GLint

framebufferTexture2D :: Attachment -> TexTarget -> Texture -> Level ->
                        BoundFramebuffer ()
framebufferTexture2D attachment textarget texture level = do
  target <- marshalFramebufferTarget <$> askUse
  glFramebufferTexture2D target attachment textarget (object texture) level

framebufferDepth :: GLsizei -> GLsizei -> Renderbuffer -> BoundFramebuffer ()
framebufferDepth w h renderbuffer = do
  bindRenderbuffer renderbuffer
  glRenderbufferStorage GL_RENDERBUFFER GL_DEPTH_COMPONENT w h
  target <- marshalFramebufferTarget <$> askUse
  glFramebufferRenderbuffer target GL_DEPTH_ATTACHMENT
                            GL_RENDERBUFFER (object renderbuffer)

bindRenderbuffer :: MonadIO m => Renderbuffer -> m ()
bindRenderbuffer = glBindRenderbuffer GL_RENDERBUFFER . object

drawBuffers :: MonadIO m => [GLenum] -> m ()
drawBuffers buffers = liftIO . withArrayLen buffers $ \n ptr ->
    glDrawBuffers (fromIntegral n) ptr
