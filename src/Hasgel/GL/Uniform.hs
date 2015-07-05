{-# LANGUAGE FlexibleInstances #-}

module Hasgel.GL.Uniform (
  UniformData(..), getUniformLocation
) where

import Control.Monad.IO.Class (MonadIO (..))
import Foreign (Ptr, castPtr, withArrayLen)
import Foreign.C (withCAString)
import Graphics.GL.Core45
import Graphics.GL.Types
import qualified Linear as L

import Hasgel.GL.Object
import Hasgel.GL.Program

newtype UniformLocation = UniformLocation GLint

class UniformData a where
  {-# MINIMAL uniformv #-}
  uniformv :: (UniformData a, MonadIO m) => UniformLocation -> [a] -> m ()
  uniform :: (UniformData a, MonadIO m) => UniformLocation -> a -> m ()
  uniform loc x = uniformv loc [x]

instance UniformData (L.M44 Float) where
  uniformv loc ms = liftIO . withArrayLen ms $ \n ->
    uniformMatrix4f loc (fromIntegral n) GL_TRUE . castPtr

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
