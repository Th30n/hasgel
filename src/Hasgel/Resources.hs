{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}

module Hasgel.Resources (
  Resources(..), HasResources(..),
  Programs, ProgramDesc, ShaderDesc,
  withResources, emptyPrograms, loadProgram, freePrograms
) where

import Control.Exception (Exception (..), bracket)
import Control.Exception.Lifted (catch, catchJust)
import Control.Monad (void)
import Control.Monad.Base (MonadBase (..))
import Control.Monad.State (MonadState (..), gets, modify, runStateT)
import Control.Monad.Trans.Control (MonadBaseControl (..))
import qualified Data.Map.Strict as M
import Data.Time (UTCTime)
import System.Directory (getModificationTime)
import System.IO (hPutStrLn, stderr)
import System.IO.Error (isDoesNotExistError, isPermissionError)

import Graphics.GL.Core45

import qualified Hasgel.GL as GL
import Hasgel.Mesh (Mesh, cube, loadHmd)
import qualified Hasgel.SDL as SDL

type ModificationTime = UTCTime
type ShaderDesc = (FilePath, GL.ShaderType)
type ProgramDesc = [ShaderDesc]

data Resources = Resources
  { texture :: GL.Texture
  , timeQueries :: [GL.Query]
  , resPrograms :: Programs
  , resMesh :: Mesh
  , resVao :: GL.VertexArray
  , resAxisVao :: GL.VertexArray
  }

data Programs = Programs
  { programsMap :: M.Map [ShaderDesc] (GL.Program, ModificationTime)
  , shadersMap :: M.Map FilePath GL.Shader }
  deriving (Show)

newtype WrappedResources a = WrapResources { unwrapResources :: a }

class HasResources s where
  getResources :: s -> Resources
  setResources :: s -> Resources -> s

class HasPrograms s where
  getPrograms :: s -> Programs
  setPrograms :: s -> Programs -> s

instance HasPrograms Resources where
  getPrograms = resPrograms
  setPrograms res programs = res { resPrograms = programs }

instance HasResources s => HasPrograms (WrappedResources s) where
  getPrograms = resPrograms . getResources . unwrapResources
  setPrograms s programs =
    let res = getResources $ unwrapResources s
        res' = res { resPrograms = programs }
    in WrapResources $ setResources (unwrapResources s) res'

withResources :: (Resources -> IO a) -> IO a
withResources = bracket loadResources freeResources

loadResources :: IO Resources
loadResources = do
    tex <- loadTexture "share/models/player-ship-diffuse.bmp"
    qs <- GL.gens 4
    eitherMesh <- loadHmd "share/models/player-ship.hmd"
    vao <- GL.gen
    axisVao <- GL.gen
    mesh <- case eitherMesh of
              Left err -> putStrLn err >> pure cube
              Right m -> pure m
    pure $ Resources tex qs emptyPrograms mesh vao axisVao

loadTexture :: FilePath -> IO GL.Texture
loadTexture file = do
  s <- SDL.loadBMP file
  tex <- GL.gen
  glBindTexture GL_TEXTURE_2D $ GL.object tex
  let w = fromIntegral $ SDL.surfaceW s
      h = fromIntegral $ SDL.surfaceH s
      pixels = SDL.surfacePixels s
  glTexImage2D GL_TEXTURE_2D 0 GL_RGB w h 0 GL_BGR GL_UNSIGNED_BYTE pixels
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST
  SDL.freeSurface s
  pure tex

freeResources :: Resources -> IO ()
freeResources res = do
  GL.delete $ texture res
  GL.deletes $ timeQueries res
  GL.delete $ resVao res
  GL.delete $ resAxisVao res
  void . freePrograms $ resPrograms res

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

loadProgram :: (HasResources s, MonadBaseControl IO m, MonadState s m) =>
                ProgramDesc -> m GL.Program
loadProgram files = do
  hasRes <- get
  (a, s) <- runStateT (loadProgram' files) $ WrapResources hasRes
  put $ unwrapResources s
  pure a

loadProgram' :: (HasPrograms s, MonadBaseControl IO m, MonadState s m) =>
               ProgramDesc -> m GL.Program
loadProgram' files = do
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
