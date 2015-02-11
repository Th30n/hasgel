-- | Provides access to rendering display
module Hasgel.Display (
  initDisplay,
  quitDisplay,
  Display,
  renderDisplay,
  destroyDisplay,
  createDisplay,
) where

import Control.Error
import Control.Monad (when)
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
-- | Returns error string if failed.
initDisplay :: Script ()
initDisplay = MySDL.init [MySDL.InitVideo]

-- | Shut down display system.
quitDisplay :: IO ()
quitDisplay = MySDL.quit

-- | Create window with OpenGl context.
createWindow :: Script MySDL.Window
createWindow = do
  v <- scriptIO $ SDL.glSetAttribute SDL.glAttrContextMajorVersion 3
  when (v /= 0) $ scriptIO MySDL.getError >>= throwT
  v' <- scriptIO $ SDL.glSetAttribute SDL.glAttrContextMinorVersion 3
  when (v' /= 0) $ scriptIO MySDL.getError >>= throwT
  cfs <- scriptIO $
    SDL.glSetAttribute SDL.glAttrContextFlags SDL.glContextFlagForwardCompatible
  when (cfs /= 0) $ scriptIO MySDL.getError >>= throwT
  prof <- scriptIO $
    SDL.glSetAttribute SDL.glAttrContextProfileMask SDL.glProfileCore
  when (prof /= 0) $ scriptIO MySDL.getError >>= throwT
  let t = "hasgel"
  let rect = MySDL.WindowRectangle (MySDL.Pos 0) MySDL.Centered 800 600
  MySDL.createWindow t rect [MySDL.WindowOpenGL]

createContext :: MySDL.Window -> Script MySDL.GLContext
createContext = MySDL.glCreateContext

-- | Creates a display window with context for rendering.
createDisplay :: Script (Display MySDL.Window MySDL.GLContext)
createDisplay = do
  w <- Hasgel.Display.createWindow
  c <- createContext w
  return Display {
    getWindow = w,
    getContext = c,
    renderDisplay = \renderAction -> do
      renderAction
      MySDL.glSwapWindow w,
    destroyDisplay = do
      MySDL.glDeleteContext c
      MySDL.destroyWindow w
  }
