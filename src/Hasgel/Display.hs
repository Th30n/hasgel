-- | Provides access to rendering display
module Hasgel.Display (
  initDisplay,
  quitDisplay,
  Display,
  renderDisplay,
  destroyDisplay,
  createDisplay,
) where

import Control.Monad.Except
import Foreign.Ptr (nullPtr)
import qualified Graphics.UI.SDL as SDL

import Hasgel.SDL.Basic as MySDL
import Hasgel.SDL.Video as MySDL

-- | Stores resources of a display, and provides functions for rendering
-- and cleaning up.
data Display a b = Display {
  getWindow :: a, -- ^ Get underlying window implementation.
  getContext :: b, -- ^ Get underlying context implementation.
  renderDisplay :: IO () -> IO (), -- ^ Take an IO rendering action and render.
  destroyDisplay :: IO () -- ^ Clean up the resources of this display.
}

-- | Initialize display system.
-- Returns error string if failed.
initDisplay :: ExceptT String IO ()
initDisplay = ExceptT $ MySDL.init [MySDL.InitVideo]

-- | Shut down display system.
quitDisplay :: IO ()
quitDisplay = MySDL.quit

createWindow :: ExceptT String IO MySDL.Window
createWindow = do
  v <- liftIO $ SDL.glSetAttribute SDL.glAttrContextMajorVersion 3
  when (v /= 0) $ liftIO MySDL.getError >>= throwError
  v' <- liftIO $ SDL.glSetAttribute SDL.glAttrContextMinorVersion 3
  when (v' /= 0) $ liftIO MySDL.getError >>= throwError
  cfs <- liftIO $
    SDL.glSetAttribute SDL.glAttrContextFlags SDL.glContextFlagForwardCompatible
  when (cfs /= 0) $ liftIO MySDL.getError >>= throwError
  prof <- liftIO $
    SDL.glSetAttribute SDL.glAttrContextProfileMask SDL.glProfileCore
  when (prof /= 0) $ liftIO MySDL.getError >>= throwError
  let t  = "hasgel"
  let rect = MySDL.WindowRectangle (MySDL.Pos 0) (MySDL.Centered) 800 600
  ExceptT $ MySDL.createWindow t rect [MySDL.WindowOpenGl]

createContext :: SDL.Window -> ExceptT String IO SDL.GLContext
createContext w = do
  c <- liftIO $ SDL.glCreateContext w
  when (c == nullPtr) $ liftIO MySDL.getError >>= throwError
  return c

-- | Creates a display window with context for rendering.
createDisplay :: ExceptT String IO (Display SDL.Window SDL.GLContext)
createDisplay = do
  w <- Hasgel.Display.createWindow
  c <- createContext w
  return Display {
    getWindow = w,
    getContext = c,
    renderDisplay = \renderAction -> do
      renderAction
      SDL.glSwapWindow w,
    destroyDisplay = do
      SDL.glDeleteContext c
      SDL.destroyWindow w
      
  }
