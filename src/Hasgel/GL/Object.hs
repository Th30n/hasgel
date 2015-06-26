module Hasgel.GL.Object (
  Object(..), Gen(..)
) where

import Control.Monad (replicateM)
import Control.Monad.IO.Class (MonadIO)
import Graphics.GL.Types (GLuint)

class Object a where
  {-# MINIMAL (delete | deletes), object #-}
  delete :: (Object a, MonadIO m) => a -> m ()
  delete = deletes . pure
  deletes :: (Object a, MonadIO m) => [a] -> m ()
  deletes = mapM_ delete
  object :: Object a => a -> GLuint

class Object a => Gen a where
  {-# MINIMAL gen | gens #-}
  gen :: (Object a, MonadIO m) => m a
  gen = head <$> gens 1
  gens :: (Object a, MonadIO m) => Int -> m [a]
  gens n = replicateM n gen

