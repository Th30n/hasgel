module Hasgel.Resources (
  Shaders, HasShaders(..), emptyShaders, loadShader, freeShaders
) where

import qualified Data.Map.Strict as M
import System.Directory (getModificationTime)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State (MonadState(..), gets, modify)
import Data.Time(UTCTime)

import qualified Hasgel.GL as GL

type ModificationTime = UTCTime

newtype Shaders = Shaders
  { shadersMap :: M.Map FilePath (GL.Shader, ModificationTime) }
  deriving (Show)

class HasShaders s where
  getShaders :: s -> Shaders
  setShaders :: s -> Shaders -> s

emptyShaders :: Shaders
emptyShaders = Shaders M.empty

loadShader :: (HasShaders s, MonadIO m, MonadState s m) =>
              FilePath -> GL.ShaderType  -> m GL.Shader
loadShader file shaderType = do
  shaders <- gets getShaders
  let mbShader = M.lookup file $ shadersMap shaders
  case mbShader of
    Nothing -> insertShader file shaderType
    Just (shader, modTime) -> reloadShader file shaderType modTime shader

insertShader :: (HasShaders s, MonadIO m, MonadState s m) =>
                FilePath -> GL.ShaderType -> m GL.Shader
insertShader file shaderType = do
  src <- liftIO $ readFile file
  shader <- GL.compileShader src shaderType
  shaders <- gets getShaders
  modTime <- liftIO $ getModificationTime file
  let shadersMap' = M.insert file (shader, modTime) $ shadersMap shaders
      shaders' = shaders { shadersMap = shadersMap' }
  modify $ flip setShaders shaders'
  pure shader

freeShaders :: (HasShaders s, MonadIO m, MonadState s m) => m ()
freeShaders = do
  mapM_ (GL.delete . fst) . shadersMap =<< gets getShaders
  liftIO $ putStrLn "freeing shaders"
  modify . flip setShaders $ Shaders M.empty

reloadShader :: (HasShaders s, MonadIO m, MonadState s m) =>
                FilePath -> GL.ShaderType -> ModificationTime -> GL.Shader ->
                m GL.Shader
reloadShader file shaderType lastModTime lastShader = do
  newModTime <- liftIO $ getModificationTime file
  if newModTime <= lastModTime
    then liftIO (putStrLn "Old shader") >> pure lastShader
    else do liftIO (putStrLn "New shader")
            GL.delete lastShader
            insertShader file shaderType
