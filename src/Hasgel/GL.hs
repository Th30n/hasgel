module Hasgel.GL (
  module GL,
  Texture, Query, GLError(..),
  getError, throwError,
  drawElements,
  beginQuery, endQuery, getQueryResult, withQuery, queryCounter
) where

import Control.Exception (Exception, throwIO)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable)
import Data.Word (Word64)
import Foreign (Ptr, Storable (..), alloca, peek)
import Graphics.GL.Core45
import Graphics.GL.Types

import Hasgel.GL.Attribute as GL
import Hasgel.GL.Buffer as GL
import Hasgel.GL.Object as GL
import Hasgel.GL.Param as GL
import Hasgel.GL.Program as GL
import Hasgel.GL.Shader as GL
import Hasgel.GL.Uniform as GL

-- | Enumeration of possible error codes in OpenGL.
data GLError =
  NoError
  | InvalidEnum
  | InvalidValue
  | InvalidOperation
  | InvalidFramebufferOperation
  | OutOfMemory
  | StackUnderflow
  | StackOverflow
  deriving (Show, Eq, Typeable)

instance Exception GLError

newtype Texture = Texture GLuint deriving (Show)

instance Object Texture where
  deletes = deletesWith glDeleteTextures
  object (Texture obj) = obj

instance Gen Texture where
  gens = gensWith glGenTextures Texture

newtype Query = Query GLuint deriving (Show)

instance Object Query where
  object (Query q) = q
  deletes = deletesWith glDeleteQueries

instance Gen Query where
  gens = gensWith glGenQueries Query

type QueryTarget = GLenum
type DrawMode = GLenum
type DataType = GLenum

-- | Converts from numerical representation to 'GLError'
unmarshalGLError :: GLenum -> Maybe GLError
unmarshalGLError val
  | val == GL_NO_ERROR = Just NoError
  | val == GL_INVALID_ENUM = Just InvalidEnum
  | val == GL_INVALID_OPERATION = Just InvalidOperation
  | val == GL_INVALID_FRAMEBUFFER_OPERATION = Just InvalidFramebufferOperation
  | val == GL_OUT_OF_MEMORY = Just OutOfMemory
  | val == GL_STACK_UNDERFLOW = Just StackUnderflow
  | val == GL_STACK_OVERFLOW = Just StackOverflow
  | otherwise = Nothing

-- | Returns 'GLError'.
getError :: MonadIO m => m GLError
getError = (fromMaybe NoError . unmarshalGLError) <$> glGetError

-- | Throws an 'GLError' on error.
throwError :: MonadIO m => m ()
throwError = getError >>= \e -> when (e /= NoError) $ liftIO $ throwIO e

drawElements :: (Integral a, MonadIO m) =>
                DrawMode -> a -> DataType -> Ptr () -> m ()
drawElements mode count = glDrawElements mode (fromIntegral count)

beginQuery :: MonadIO m => QueryTarget -> Query -> m ()
beginQuery target = glBeginQuery target . object

endQuery :: MonadIO m => QueryTarget -> m ()
endQuery = glEndQuery

getQueryResult :: MonadIO m => Query -> m Word64
getQueryResult (Query q) = liftIO . alloca $ \ptr -> do
  glGetQueryObjectui64v q GL_QUERY_RESULT ptr
  peek ptr

withQuery :: MonadIO m => QueryTarget -> Query -> m a -> m a
withQuery target q action = do
  beginQuery target q
  r <- action
  endQuery target
  pure r

queryCounter :: MonadIO m => Query -> m ()
queryCounter q = glQueryCounter (object q) GL_TIMESTAMP
