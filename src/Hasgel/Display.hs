-- | Provides access to rendering display
module Hasgel.Display (
  Display,
  renderDisplay,
  destroyDisplay,
  createDisplay,
) where


import Control.Monad.IO.Class (MonadIO (..))

import qualified Hasgel.SDL.Video as MySDL

-- | Stores resources of a display, and provides functions for rendering
-- and cleaning up.
data Display = Display {
  getWindow :: MySDL.Window, -- ^ Get underlying window implementation.
  getContext :: MySDL.GLContext, -- ^ Get underlying context implementation.
  renderDisplay :: IO () -> IO (), -- ^ Take an IO rendering action and render.
  destroyDisplay :: IO () -- ^ Clean up the resources of this display.
}

-- | Create window with OpenGl context.
createWindow :: MonadIO m => m MySDL.Window
createWindow = do
  MySDL.glSetContextVersion 4 3
  MySDL.glSetContextFlags [MySDL.GLForwardCompatible]
  MySDL.glSetContextProfile MySDL.GLCore
  let t = "hasgel"
  let rect = MySDL.WindowRectangle (MySDL.Pos 0) MySDL.Centered 800 600
  MySDL.createWindow t rect [MySDL.WindowOpenGL]

createContext :: MonadIO m => MySDL.Window -> m MySDL.GLContext
createContext = MySDL.glCreateContext

-- | Creates a display window with context for rendering.
createDisplay :: MonadIO m => m Display
createDisplay = do
  w <- createWindow
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
