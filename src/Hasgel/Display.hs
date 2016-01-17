{-# LANGUAGE OverloadedStrings #-}

-- | Provides access to rendering display
module Hasgel.Display (
  Display(..),
  renderDisplay,
  destroyDisplay,
  createDisplay,
) where

import Control.Monad.IO.Class (MonadIO (..))

import qualified SDL

-- | Stores resources of a display, and provides functions for rendering
-- and cleaning up.
data Display = Display
  {
    getWindow :: SDL.Window -- ^ Get underlying window implementation.
  , getContext :: SDL.GLContext -- ^ Get underlying context implementation.
  }

-- | Creates a display window with context for rendering.
createDisplay :: MonadIO m => m Display
createDisplay = do
  -- Normal context (does this set forward compatible?)
  let glCfg = SDL.defaultOpenGL { SDL.glProfile = SDL.Core SDL.Normal 4 3 }
      wCfg = SDL.defaultWindow { SDL.windowOpenGL = Just glCfg }
  w <- SDL.createWindow "hasgel" wCfg
  Display w <$> SDL.glCreateContext w
  -- w <- createWindow
  -- Display w <$> createContext w

-- | Take an IO rendering action and render.
renderDisplay :: MonadIO m => Display -> m () -> m ()
renderDisplay d renderAction = renderAction >> SDL.glSwapWindow (getWindow d)

 -- | Clean up the resources of this display.
destroyDisplay :: MonadIO m => Display -> m ()
destroyDisplay d = do
  SDL.glDeleteContext $ getContext d
  SDL.destroyWindow $ getWindow d
