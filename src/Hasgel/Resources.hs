{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Hasgel.Resources (
  HasPrograms(..), Programs, ProgramDesc, ShaderDesc,
  emptyPrograms, loadProgram, freePrograms
) where

import Control.Exception (Exception (..))
import Control.Exception.Lifted (catch, catchJust)
import Control.Monad.Base (MonadBase (..))
import Control.Monad.State (MonadState (..), gets, modify)
import Control.Monad.Trans.Control (MonadBaseControl (..))
import qualified Data.Map.Strict as M
import Data.Time (UTCTime)
import System.Directory (getModificationTime)
import System.IO (hPutStrLn, stderr)
import System.IO.Error (isDoesNotExistError, isPermissionError)

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

loadShader :: (HasPrograms s, MonadBaseControl IO m, MonadState s m) =>
              ShaderDesc -> m GL.Shader
loadShader desc@(file, _) = do
  mbShader <- M.lookup file . shadersMap <$> gets getPrograms
  case mbShader of
    Nothing -> insertShader desc
    Just shader -> liftBase $ reloadShader desc shader

insertShader :: (HasPrograms s, MonadBaseControl IO m, MonadState s m) =>
                ShaderDesc -> m GL.Shader
insertShader (file, shaderType) = do
  shader <- liftBase $ flip GL.compileShader shaderType =<< readFile file
  programs <- gets getPrograms
  let shadersMap' = M.insert file shader $ shadersMap programs
      programs' = programs { shadersMap = shadersMap' }
  modify $ flip setPrograms programs'
  pure shader

reloadShader :: ShaderDesc -> GL.Shader -> IO GL.Shader
reloadShader (file, _) lastShader =
  readFile file >>= GL.recompileShader lastShader

emptyPrograms :: Programs
emptyPrograms = Programs M.empty M.empty

loadProgram :: (HasPrograms s, MonadBaseControl IO m, MonadState s m) =>
               ProgramDesc -> m GL.Program
loadProgram files = do
  mbProgram <- M.lookup files . programsMap <$> gets getPrograms
  case mbProgram of
    Nothing -> insertProgram files
    Just (program, modTime) -> reloadProgram files modTime program

insertProgram :: (HasPrograms s, MonadBaseControl IO m, MonadState s m) =>
                 ProgramDesc -> m GL.Program
insertProgram files = do
  program <- mapM loadShader files >>= liftBase . GL.linkProgram
  modTime <- liftBase $ latestModificationTime files
  programs <- gets getPrograms
  let programsMap' = M.insert files (program, modTime) $ programsMap programs
      programs' = programs { programsMap = programsMap' }
  modify $ flip setPrograms programs'
  pure program
  where latestModificationTime =
          fmap maximum . mapM (getModificationTime . fst)

freePrograms :: Programs -> IO Programs
freePrograms programs = do
  putStrLn "Freeing shader programs"
  mapM_ GL.delete $ shadersMap programs
  mapM_ (GL.delete . fst) $ programsMap programs
  pure $ Programs M.empty M.empty

reloadProgram :: (HasPrograms s, MonadBaseControl IO m, MonadState s m) =>
                 ProgramDesc -> ModificationTime -> GL.Program -> m GL.Program
reloadProgram desc lastModTime lastProgram =
  handleFileError lastProgram $ do
    newModTime <- liftBase $ maximum <$> mapM (getModificationTime . fst) desc
    if newModTime <= lastModTime
      then pure lastProgram
      else do liftBase $ putStrLn "Reloading shader program"
              reloadProgram' `catch` \(e :: GL.ShaderException)  -> do
                liftBase . hPutStrLn stderr $ displayException e
                programs <- gets getPrograms
                let programsMap' = M.insert desc (lastProgram, newModTime) $
                                   programsMap programs
                    programs' = programs { programsMap = programsMap' }
                modify $ flip setPrograms programs'
                pure lastProgram
      where reloadProgram' = do
              prog <- insertProgram desc
              liftBase $ GL.delete lastProgram
              pure prog

handleFileError :: MonadBaseControl IO m => a -> m a -> m a
handleFileError def action =
  catchJust (\e -> if isDoesNotExistError e || isPermissionError e
                   then Just e else Nothing)
    action
    (\e -> do liftBase . hPutStrLn stderr $ displayException e
              pure def)
