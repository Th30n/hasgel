module Main ( main ) where

import Control.Monad.Except
import Foreign.C.String
import Foreign.Marshal.Alloc (free)
import Foreign.Ptr (nullPtr)
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import Prelude

main :: IO ()
main = do
  SDL.setMainReady
  e <- SDL.init SDL.initFlagEverything
  if e == 0 then do
    res <- runExceptT createWindow
    case res of
      Right w -> do
        cr <- runExceptT $ createContext w
        case cr of
          Right c -> do
            GL.clearColor $= GL.Color4 1.0 0.0 0.0 1.0
            GL.clear [GL.ColorBuffer]
            SDL.glSwapWindow w
            SDL.delay 3000
            SDL.glDeleteContext c
          Left err -> putStrLn err
        SDL.destroyWindow w
      Left err -> putStrLn err
  else getSDLErrorString >>= putStrLn
  SDL.quit

getSDLErrorString :: IO String
getSDLErrorString = SDL.getError >>= (\e -> SDL.clearError >> peekCString e)

createWindow :: ExceptT String IO SDL.Window
createWindow = do
  v <- liftIO $ SDL.glSetAttribute SDL.glAttrContextMajorVersion 3
  when (v /= 0) $ liftIO getSDLErrorString >>= throwError
  v' <- liftIO $ SDL.glSetAttribute SDL.glAttrContextMinorVersion 3
  when (v' /= 0) $ liftIO getSDLErrorString >>= throwError
  cfs <- liftIO $
    SDL.glSetAttribute SDL.glAttrContextFlags SDL.glContextFlagForwardCompatible
  when (cfs /= 0) $ liftIO getSDLErrorString >>= throwError
  prof <- liftIO $
    SDL.glSetAttribute SDL.glAttrContextProfileMask SDL.glProfileCore
  when (prof /= 0) $ liftIO getSDLErrorString >>= throwError
  t <- liftIO $ newCString "hasgel"
  w <- liftIO $ SDL.createWindow t 0 0 800 600 SDL.windowFlagOpenGL
  liftIO $ free t
  when (w == nullPtr) $ liftIO getSDLErrorString >>= throwError
  return w

createContext :: SDL.Window -> ExceptT String IO SDL.GLContext
createContext w = do
  c <- liftIO $ SDL.glCreateContext w
  when (c == nullPtr) $ liftIO getSDLErrorString >>= throwError
  return c
