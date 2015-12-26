module Hasgel.GL.Program (
  Program, UsedProgram, useProgram, getUsedProgram, linkProgram
) where

import Control.Exception (throwIO)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (..))
import Foreign (Storable (..), alloca, allocaArray, nullPtr, peek)
import Foreign.C (peekCString)
import Graphics.GL.Core45
import Graphics.GL.Types

import Hasgel.GL.Object
import Hasgel.GL.Shader

-- | Program object.
newtype Program = Program GLuint deriving (Show)

instance Object Program where
  delete = glDeleteProgram . object
  object (Program obj) = obj

instance Gen Program where
  gen = createProgram

-- | XXX: Duplicated from BoundBuffer
newtype UsedProgram a = UsedProgram { withUsedProgram :: Program -> IO a }

instance Functor UsedProgram where
  fmap f (UsedProgram b) = UsedProgram $ fmap f . b

instance Applicative UsedProgram where
  pure a = UsedProgram $ \_ -> pure a

  (UsedProgram f) <*> (UsedProgram a) =
    UsedProgram $ \program -> f program <*> a program

instance Monad UsedProgram where
  (UsedProgram a) >>= f = UsedProgram $ \program -> do
    a' <- a program
    withUsedProgram (f a') program

instance MonadIO UsedProgram where
  liftIO action = UsedProgram $ const action

useProgram :: MonadIO m => Program -> UsedProgram a -> m a
useProgram program actions = do
  glUseProgram $ object program
  liftIO $ withUsedProgram actions program

getUsedProgram :: UsedProgram Program
getUsedProgram = UsedProgram pure

createProgram :: MonadIO m => m Program
createProgram = do
  program <- glCreateProgram
  when (program == 0) . liftIO . throwIO $
    CreationError "Error creating program."
  pure $ Program program

attachShader :: MonadIO m => Program -> Shader -> m ()
attachShader (Program prog) (Shader sh) = glAttachShader prog sh

-- | Creates a program object and links it with compiled shader objects.
-- Throws 'CreationError' or 'LinkError' on failure.
linkProgram :: MonadIO m => [Shader] -> m Program
linkProgram ss = do
  program <- gen
  mapM_ (attachShader program) ss
  glLinkProgram $ object program
  status <- getProgramiv program GL_LINK_STATUS
  when (status == GL_FALSE) $ do
    linkLog <- getProgramInfoLog program
    delete program
    liftIO . throwIO $ LinkError linkLog
  pure program

-- | Returns the information log for a program object.
getProgramInfoLog :: MonadIO m => Program -> m String
getProgramInfoLog p@(Program program) = do
  logSize <- getProgramiv p GL_INFO_LOG_LENGTH
  liftIO . allocaArray (fromIntegral logSize) $ \infoLog -> do
    glGetProgramInfoLog program logSize nullPtr infoLog
    peekCString infoLog

-- | Returns a parameter from a program object.
getProgramiv :: MonadIO m => Program -> GLenum -> m GLint
getProgramiv (Program program) pname =
  liftIO . alloca $ \params -> do
    glGetProgramiv program pname params
    peek params
