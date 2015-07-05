-- | Provides access to rendering display
module Hasgel.Display (
  Display(..),
  renderDisplay,
  destroyDisplay,
  createDisplay,
) where


import Control.Monad.IO.Class (MonadIO (..))

import qualified Hasgel.SDL.Video as MySDL

-- | Stores resources of a display, and provides functions for rendering
-- and cleaning up.
data Display = Display
  {
    getWindow :: MySDL.Window -- ^ Get underlying window implementation.
  , getContext :: MySDL.GLContext -- ^ Get underlying context implementation.
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
  Display w <$> createContext w

-- | Take an IO rendering action and render.
renderDisplay :: MonadIO m => Display -> m () -> m ()
renderDisplay d renderAction = renderAction >> MySDL.glSwapWindow (getWindow d)

 -- | Clean up the resources of this display.
destroyDisplay :: MonadIO m => Display -> m ()
destroyDisplay d = do
  MySDL.glDeleteContext $ getContext d
  MySDL.destroyWindow $ getWindow d
