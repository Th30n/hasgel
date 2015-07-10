module Hasgel.Resources (
  Shaders, HasShaders(..), HasPrograms(..), Programs, ProgramDesc, ShaderDesc,
  emptyShaders, loadShader, freeShaders,
  emptyPrograms, loadProgram, freePrograms
) where

import qualified Data.Map.Strict as M
import System.Directory (getModificationTime)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State (MonadState(..), gets, modify)
import Data.Time(UTCTime)

import qualified Hasgel.GL as GL

type ModificationTime = UTCTime
type ShaderDesc = (FilePath, GL.ShaderType)
type ProgramDesc = [ShaderDesc]

newtype Shaders = Shaders
  { shadersMap :: M.Map FilePath (GL.Shader, ModificationTime) }
  deriving (Show)

newtype Programs = Programs
  { programsMap :: M.Map [ShaderDesc] (GL.Program, ModificationTime) }
  deriving (Show)

class HasShaders s where
  getShaders :: s -> Shaders
  setShaders :: s -> Shaders -> s

class HasShaders s => HasPrograms s where
  getPrograms :: s -> Programs
  setPrograms :: s -> Programs -> s

emptyShaders :: Shaders
emptyShaders = Shaders M.empty

loadShader :: (HasShaders s, MonadIO m, MonadState s m) =>
              ShaderDesc -> m GL.Shader
loadShader desc@(file, _) = do
  shaders <- gets getShaders
  let mbShader = M.lookup file $ shadersMap shaders
  case mbShader of
    Nothing -> insertShader desc
    Just (shader, modTime) -> reloadShader desc modTime shader

insertShader :: (HasShaders s, MonadIO m, MonadState s m) =>
                ShaderDesc -> m GL.Shader
insertShader (file, shaderType) = do
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
  modify . flip setShaders $ Shaders M.empty

reloadShader :: (HasShaders s, MonadIO m, MonadState s m) =>
                ShaderDesc -> ModificationTime -> GL.Shader -> m GL.Shader
reloadShader desc@(file, _) lastModTime lastShader = do
  newModTime <- liftIO $ getModificationTime file
  if newModTime <= lastModTime
    then pure lastShader
    else do GL.delete lastShader
            insertShader desc

emptyPrograms :: Programs
emptyPrograms = Programs M.empty

loadProgram :: (HasPrograms s, MonadIO m, MonadState s m) =>
               ProgramDesc -> m GL.Program
loadProgram files = do
  programs <- gets getPrograms
  let mbProgram = M.lookup files $ programsMap programs
  case mbProgram of
    Nothing -> insertProgram files
    Just (program, modTime) -> reloadProgram files modTime program

insertProgram :: (HasPrograms s, MonadIO m, MonadState s m) =>
                 ProgramDesc -> m GL.Program
insertProgram files = do
  loadedShaders <- mapM loadShader files
  program <- GL.linkProgram loadedShaders
  modTime <- liftIO $ latestModificationTime files
  programs <- gets getPrograms
  let programsMap' = M.insert files (program, modTime) $ programsMap programs
      programs' = programs { programsMap = programsMap' }
  modify $ flip setPrograms programs'
  pure program
  where latestModificationTime =
          fmap maximum . mapM (getModificationTime . fst)

freePrograms :: (HasPrograms s, MonadIO m, MonadState s m) => m ()
freePrograms = do
  mapM_ (GL.delete . fst) . programsMap =<< gets getPrograms
  modify . flip setPrograms $ Programs M.empty

reloadProgram :: (HasPrograms s, MonadIO m, MonadState s m) =>
                 ProgramDesc -> ModificationTime -> GL.Program -> m GL.Program
reloadProgram desc lastModTime lastProgram = do
  newModTime <- liftIO $ maximum <$> mapM (getModificationTime . fst) desc
  if newModTime <= lastModTime
    then pure lastProgram
    else do GL.delete lastProgram
            insertProgram desc
