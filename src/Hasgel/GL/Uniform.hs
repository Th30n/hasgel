{-# LANGUAGE FlexibleInstances #-}

module Hasgel.GL.Uniform (
  UniformData(..), uniformByName, getUniformLocation
) where

import Control.Monad.IO.Class (MonadIO (..))
import Foreign (castPtr, withArrayLen)
import Foreign.C (withCAString)
import Graphics.GL.Core45
import Graphics.GL.Types
import qualified Linear as L
import Text.Printf (printf)

import Hasgel.GL.Object
import Hasgel.GL.Program

newtype UniformLocation = UniformLocation { unwrapLocation :: GLint }

class UniformData a where
  {-# MINIMAL uniformv #-}
  uniformv :: (MonadIO m, UniformData a) =>
              UniformLocation -> [a] -> UsedProgram m ()
  uniform :: (MonadIO m, UniformData a) =>
             UniformLocation -> a -> UsedProgram m ()
  uniform loc x = uniformv loc [x]

instance UniformData (L.M44 Float) where
  uniformv loc ms = liftIO . withArrayLen ms $ \n ->
    glUniformMatrix4fv (unwrapLocation loc) (fromIntegral n) GL_TRUE . castPtr

instance UniformData (L.M33 Float) where
  uniformv loc ms = liftIO . withArrayLen ms $ \n ->
    glUniformMatrix3fv (unwrapLocation loc) (fromIntegral n) GL_TRUE . castPtr

instance UniformData Float where
  uniformv loc fs = liftIO . withArrayLen fs $ \n ->
    glUniform1fv (unwrapLocation loc) (fromIntegral n) . castPtr

uniformByName :: (MonadIO m, UniformData a) => String -> a -> UsedProgram m ()
uniformByName name v = do
  mbLoc <- getUniformLocation name
  case mbLoc of
    Just loc -> uniform loc v
    Nothing -> liftIO $ printf "Missing uniform '%s'\n" name

getUniformLocation :: MonadIO m => String -> UsedProgram m (Maybe UniformLocation)
getUniformLocation name = do
  program <- getUsedProgram
  liftIO . withCAString name $ \ptr -> do
    loc <- glGetUniformLocation (object program) ptr
    pure $ if loc == -1 then Nothing else Just $ UniformLocation loc
