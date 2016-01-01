{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hasgel.GL.Framebuffer (
  Framebuffer(fbColorTextures), FramebufferTarget(..),
  Attachment(..), Renderbuffer, Texture,
  bindFramebuffer, framebufferTexture, framebufferDepth, drawBuffers
) where

import Control.Arrow (second)
import Control.Monad.State
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Word (Word32)
import Foreign (withArrayLen)

import Graphics.GL.Core45
import Graphics.GL.Types

import Hasgel.GL.Object

-- | Represents a framebuffer object. Owns texture objects attached as color
-- attachments and attached renderbuffers.
data Framebuffer = Framebuffer
  { fbObject :: GLuint
  , fbColorTextures :: IntMap Texture
  , fbRenderbuffers :: [Renderbuffer]
  } deriving (Show)

instance Object Framebuffer where
  object = fbObject
  deletes fbos = do
    deletesWith glDeleteFramebuffers fbos
    sequence_ $ mapM_ delete <$> map fbColorTextures fbos
    sequence_ $ mapM_ delete <$> map fbRenderbuffers fbos

instance Gen Framebuffer where
  gens = gensWith glGenFramebuffers framebuffer

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

newtype BoundFramebuffer m a = BoundFramebuffer
  { withBoundFramebuffer :: StateT (FramebufferTarget, Framebuffer) m a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans BoundFramebuffer where
  lift = BoundFramebuffer . lift

framebuffer :: GLuint -> Framebuffer
framebuffer obj = Framebuffer { fbObject = obj, fbColorTextures = IM.empty,
                                fbRenderbuffers = [] }

-- | Run actions with bound framebuffer. Return the result and modified
-- framebuffer. Draw buffers for all color attachments are automatically set.
-- Framebuffer binding reverts to default at the end.
bindFramebuffer :: MonadIO m => FramebufferTarget -> Framebuffer ->
                   BoundFramebuffer m a -> m Framebuffer
bindFramebuffer target buffer actions = do
  glBindFramebuffer (marshalFramebufferTarget target) $ object buffer
  (_, buffer') <- execStateT (withBoundFramebuffer actions) (target, buffer)
  let colors = IM.keys $ fbColorTextures buffer'
  drawBuffers $ map (\i -> GL_COLOR_ATTACHMENT0 + fromIntegral i) colors
  glBindFramebuffer (marshalFramebufferTarget target) 0
  pure buffer'

data Attachment = ColorAttachment Word32 deriving (Show, Eq, Ord)

marshalAttachment :: Attachment -> GLenum
marshalAttachment (ColorAttachment i) = GL_COLOR_ATTACHMENT0 + i

type Level = GLint

-- | Attach a texture to framebuffer. The framebuffer takes ownership of the
-- texture object and will delete it. Color attachment is automatically set to
-- draw into.
framebufferTexture :: MonadIO m => Attachment -> Texture -> Level ->
                      BoundFramebuffer m ()
framebufferTexture attachment texture level = do
  target <- marshalFramebufferTarget <$> BoundFramebuffer (gets fst)
  let att = marshalAttachment attachment
  glFramebufferTexture target att (object texture) level
  case attachment of
    ColorAttachment i -> BoundFramebuffer . modify . second $ \fbo ->
                           let texs = fbColorTextures fbo
                               texs' = IM.insert (fromIntegral i) texture texs
                           in fbo { fbColorTextures = texs' }

-- | Attach a renderbuffer as depth attachment. The framebuffer takes ownership of
-- the renderbuffer and will delete it.
framebufferDepth :: MonadIO m => GLsizei -> GLsizei -> BoundFramebuffer m ()
framebufferDepth w h = do
  renderbuffer <- gen
  bindRenderbuffer renderbuffer
  glRenderbufferStorage GL_RENDERBUFFER GL_DEPTH_COMPONENT w h
  target <- marshalFramebufferTarget <$> BoundFramebuffer (gets fst)
  glFramebufferRenderbuffer target GL_DEPTH_ATTACHMENT
                            GL_RENDERBUFFER (object renderbuffer)
  BoundFramebuffer . modify . second $ \fb ->
    fb { fbRenderbuffers = renderbuffer : fbRenderbuffers fb }

bindRenderbuffer :: MonadIO m => Renderbuffer -> m ()
bindRenderbuffer = glBindRenderbuffer GL_RENDERBUFFER . object

drawBuffers :: MonadIO m => [GLenum] -> m ()
drawBuffers buffers = liftIO . withArrayLen buffers $ \n ptr ->
    glDrawBuffers (fromIntegral n) ptr
