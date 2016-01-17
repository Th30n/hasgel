{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}

module Hasgel.Resources (
  Resources(..), HasResources(..),
  Programs, ProgramDesc, ShaderDesc,
  withResources, emptyPrograms, loadProgram, freePrograms,
  getDrawable, putDrawable
) where

import Foreign (nullPtr)
import Control.Exception (Exception (..), bracket)
import Control.Exception.Lifted (catch, catchJust)
import Control.Monad.Base (MonadBase (..))
import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.State
import Control.Monad.Trans.Control (MonadBaseControl (..))
import qualified Data.Map.Strict as M
import Data.Time (UTCTime)
import System.Directory (getModificationTime)
import System.IO (hPutStrLn, stderr)
import System.IO.Error (isDoesNotExistError, isPermissionError)

import qualified SDL
import Graphics.GL.Core45
import qualified Linear as L

import qualified Hasgel.GL as GL
import Hasgel.Drawable
import Hasgel.Mesh (cube, loadHmd)

type ModificationTime = UTCTime
type ShaderDesc = (FilePath, GL.ShaderType)
type ProgramDesc = [ShaderDesc]

data Resources = Resources
  { resTex :: GL.Texture
  , resLaserTex :: GL.Texture
  , resFontTex :: GL.Texture
  , timeQueries :: [GL.Query]
  , resPrograms :: Programs
  , resDrawables :: M.Map String Drawable
  , resFbo :: GL.Framebuffer
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
    laserTex <- loadTexture "share/gfx/laser-shot.bmp"
    fontTex <- loadTexture "share/gfx/font-16.bmp"
    qs <- GL.gens 4
    eitherMesh <- loadHmd "share/models/player-ship.hmd"
    eitherRs <- setPlaneVao
    rsMap <- case eitherRs of
            Left err -> putStrLn err >> pure M.empty
            Right rs -> pure $ M.singleton "plane" rs
    mesh <- case eitherMesh of
              Left err -> putStrLn err >> pure cube
              Right m -> pure m
    rs <- mesh2Drawable mesh
    pointDrawable <- createPointDrawable
    let rsMap' = M.insert "point" pointDrawable $
                 M.insert "player-ship" rs rsMap
    fbo <- createFbo
    pure Resources { resTex = tex, resLaserTex = laserTex, resFontTex = fontTex,
                     timeQueries = qs,
                     resPrograms = emptyPrograms,
                     resDrawables = rsMap', resFbo = fbo }

freeResources :: Resources -> IO ()
freeResources res = do
  GL.delete $ resTex res
  GL.delete $ resLaserTex res
  GL.delete $ resFontTex res
  GL.deletes $ timeQueries res
  mapM_ freeDrawable $ resDrawables res
  GL.delete $ resFbo res
  void . freePrograms $ resPrograms res

setPlaneVao :: IO (Either String Drawable)
setPlaneVao = runExceptT $ do
  mesh <- ExceptT $ loadHmd "share/models/plane.hmd"
  liftIO $ mesh2Drawable mesh

createFbo :: IO GL.Framebuffer
createFbo = do
  colorTex <- GL.gen
  glActiveTexture GL_TEXTURE0
  glBindTexture GL_TEXTURE_2D $ GL.object colorTex
  let w = 800
      h = 600
  glTexImage2D GL_TEXTURE_2D 0 GL_RGBA w h 0 GL_RGBA GL_UNSIGNED_BYTE nullPtr
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR
  fbo <- GL.gen
  GL.bindFramebuffer GL.FramebufferTarget fbo $ do
    GL.framebufferTexture (GL.ColorAttachment 0) colorTex 0
    GL.framebufferDepth w h

loadTexture :: FilePath -> IO GL.Texture
loadTexture file = do
  s <- SDL.loadBMP file
  tex <- GL.gen
  glActiveTexture GL_TEXTURE0
  glBindTexture GL_TEXTURE_2D $ GL.object tex
  L.V2 w h <- fmap fromIntegral <$> SDL.surfaceDimensions s
  pixels <- SDL.surfacePixels s
  glTexImage2D GL_TEXTURE_2D 0 GL_SRGB8 w h 0 GL_BGR GL_UNSIGNED_BYTE pixels
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST
  SDL.freeSurface s
  pure tex

getDrawable :: (HasResources s, MonadState s m) =>
                 String -> m (Maybe Drawable)
getDrawable name = M.lookup name . resDrawables <$> gets getResources

putDrawable :: (HasResources s, MonadState s m) => String -> Drawable -> m ()
putDrawable name rs = do
  rsMap <- gets $ resDrawables . getResources
  -- TODO: Handle overwrites.
  let rsMap' = M.insert name rs rsMap
  res <- gets getResources
  modify $ flip setResources res { resDrawables = rsMap' }

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
