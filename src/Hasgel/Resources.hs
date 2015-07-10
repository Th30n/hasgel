{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Hasgel.Resources (
  HasPrograms(..), Programs, ProgramDesc, ShaderDesc,
  emptyPrograms, loadProgram, freePrograms
) where

import Control.Exception (Exception (..))
import Control.Exception.Lifted (catch)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.State (MonadState (..), gets, modify)
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.Map.Strict as M
import Data.Time (UTCTime)
import System.Directory (getModificationTime)

import qualified Hasgel.GL as GL

type ModificationTime = UTCTime
type ShaderDesc = (FilePath, GL.ShaderType)
type ProgramDesc = [ShaderDesc]

data Programs = Programs
  { programsMap :: M.Map [ShaderDesc] (GL.Program, ModificationTime)
  , shadersMap :: M.Map FilePath GL.Shader }
  deriving (Show)

class HasPrograms s where
  getPrograms :: s -> Programs
  setPrograms :: s -> Programs -> s

loadShader :: (HasPrograms s, MonadIO m, MonadBaseControl IO m, MonadState s m) =>
              ShaderDesc -> m GL.Shader
loadShader desc@(file, _) = do
  programs <- gets getPrograms
  let mbShader = M.lookup file $ shadersMap programs
  case mbShader of
    Nothing -> insertShader desc
    Just shader -> reloadShader desc shader

insertShader :: (HasPrograms s, MonadIO m, MonadBaseControl IO m,
                 MonadState s m) => ShaderDesc -> m GL.Shader
insertShader (file, shaderType) = do
  src <- liftIO $ readFile file
  shader <- GL.compileShader src shaderType
  programs <- gets getPrograms
  let shadersMap' = M.insert file shader $ shadersMap programs
      programs' = programs { shadersMap = shadersMap' }
  modify $ flip setPrograms programs'
  pure shader

reloadShader :: (HasPrograms s, MonadIO m, MonadBaseControl IO m,
                 MonadState s m) => ShaderDesc -> GL.Shader -> m GL.Shader
reloadShader desc@(file, _) lastShader = do
  src <- liftIO $ readFile file
  GL.recompileShader lastShader src

emptyPrograms :: Programs
emptyPrograms = Programs M.empty M.empty

loadProgram :: (HasPrograms s, MonadIO m, MonadBaseControl IO m,
                MonadState s m) => ProgramDesc -> m GL.Program
loadProgram files = do
  programs <- gets getPrograms
  let mbProgram = M.lookup files $ programsMap programs
  case mbProgram of
    Nothing -> insertProgram files
    Just (program, modTime) -> reloadProgram files modTime program

insertProgram :: (HasPrograms s, MonadIO m, MonadBaseControl IO m,
                  MonadState s m) => ProgramDesc -> m GL.Program
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

freePrograms :: MonadIO m => Programs -> m Programs
freePrograms programs = do
  liftIO $ putStrLn "Freeing shader programs"
  mapM_ GL.delete $ shadersMap programs
  mapM_ (GL.delete . fst) $ programsMap programs
  pure $ Programs M.empty M.empty

reloadProgram :: (HasPrograms s, MonadIO m, MonadBaseControl IO m,
                  MonadState s m) =>
                 ProgramDesc -> ModificationTime -> GL.Program -> m GL.Program
reloadProgram desc lastModTime lastProgram = do
  newModTime <- liftIO $ maximum <$> mapM (getModificationTime . fst) desc
  if newModTime <= lastModTime
    then pure lastProgram
    else do liftIO $ putStrLn "Reloading shader program"
            reloadProgram' `catch` \(e :: GL.ShaderException)  -> do
              liftIO . putStrLn $ displayException e
              programs <- gets getPrograms
              let programsMap' = M.insert desc (lastProgram, newModTime) $
                                 programsMap programs
                  programs' = programs { programsMap = programsMap' }
              modify $ flip setPrograms programs'
              pure lastProgram
  where reloadProgram' = do
          prog <- insertProgram desc
          GL.delete lastProgram
          pure prog

