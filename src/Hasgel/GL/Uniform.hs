{-# LANGUAGE FlexibleInstances #-}

module Hasgel.GL.Uniform (
  UniformData(..), getUniformLocation
) where

import Control.Monad.IO.Class (MonadIO (..))
import Foreign (castPtr, withArrayLen)
import Foreign.C (withCAString)
import Graphics.GL.Core45
import Graphics.GL.Types
import qualified Linear as L

import Hasgel.GL.Object
import Hasgel.GL.Program

newtype UniformLocation = UniformLocation { unwrapLocation :: GLint }

class UniformData a where
  {-# MINIMAL uniformv #-}
  uniformv :: (UniformData a, MonadIO m) => UniformLocation -> [a] -> m ()
  uniform :: (UniformData a, MonadIO m) => UniformLocation -> a -> m ()
  uniform loc x = uniformv loc [x]

instance UniformData (L.M44 Float) where
  uniformv loc ms = liftIO . withArrayLen ms $ \n ->
    glUniformMatrix4fv (unwrapLocation loc) (fromIntegral n) GL_TRUE . castPtr

instance UniformData Float where
  uniformv loc fs = liftIO . withArrayLen fs $ \n ->
    glUniform1fv (unwrapLocation loc) (fromIntegral n) . castPtr

getUniformLocation :: MonadIO m => Program -> String -> m (Maybe UniformLocation)
getUniformLocation program name =
  liftIO . withCAString name $ \ptr -> do
    loc <- glGetUniformLocation (object program) ptr
    pure $ if loc == -1 then Nothing else Just $ UniformLocation loc
