{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hasgel.GL.Object (
  Object(..), Gen(..), WithUse, runWithUse, askUse,
  gensWith, deletesWith
) where

import Control.Monad.Reader
import Foreign (Ptr, allocaArray, peekArray, withArrayLen)

import Graphics.GL.Types

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

newtype WithUse b m a =
  WithUse { withUse :: ReaderT b m a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans (WithUse b) where
  lift = WithUse . lift

runWithUse :: MonadIO m => WithUse b m a -> b -> m a
runWithUse actions = runReaderT (withUse actions)

askUse :: Monad m => WithUse b m b
askUse = WithUse ask

type GensFun = GLsizei -> Ptr GLuint -> IO ()
type DeletesFun = GLsizei -> Ptr GLuint -> IO ()

gensWith :: (MonadIO m, Gen a) => GensFun -> (GLuint -> a) -> Int -> m [a]
gensWith g wrap n = liftIO . allocaArray n $ \ptr -> do
    g (fromIntegral n) ptr
    map wrap <$> peekArray n ptr

deletesWith :: (MonadIO m, Object a) => DeletesFun -> [a] -> m ()
deletesWith del bufs = liftIO . withArrayLen (object <$> bufs) $ \n ptr ->
    del (fromIntegral n) ptr
