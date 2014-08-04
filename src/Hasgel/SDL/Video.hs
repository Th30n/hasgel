-- | This module corresponds to SDL 2.0 Video category
-- on official documentation wiki.
module Hasgel.SDL.Video (
) where

-- | Defines x or y position of a window.
data WindowPos = Pos CInt | Centered | Undefined deriving (Show)

-- | Defines complete rectangle for the window.
data WindowRectangle = WindowRectangle {
  posX :: WindowPos, -- ^ Position on the X axis.
  posY :: WindowPos, -- ^ Position on the Y axis.
  width :: CInt, -- ^ Width of the window.
  height :: CInt -- ^ Height of the window.
} deriving (Show)

-- | Set of window flags used at creation and for querying window state.
data WindowFlag =
    WindowFullscreen
  -- ^ Fullscreen window. Can be used at window creation.
  | WindowFullScreenDesktop
  -- ^ Fullscreen window at desktop resolution. Can be used at window creation.
  | WindowOpenGl
  -- ^ Window usable with OpenGL context. This just prepares a window for use
  -- with OpenGL. Context needs to be created manually by calling
  -- glCreateContext. Can be used at window creation.
  | WindowShown
  -- ^ Window is visible (default state at creation).
  -- Ignored at window creation.
  | WindowHidden
  -- ^ Window is not visible (overrides WindowShow at creation).
  -- Can be used at creation.
  | WindowBorderless
  -- ^ No window decoration. Can be used at window creation.
  | WindowResizable
  -- ^ Window can be resized. Can be used at window creation.
  | WindowMinimized
  -- ^ Window is minimized. Can be used at window creation.
  | WindowMaximized
  -- ^ Window is maximized. Can be used at window creation.
  | WindowInputGrabbed
  -- ^ Window has grabbed input focus. Can be used at window creation.
  | WindowInputFocus -- ^ Window has input focus.
  | WindowMouseFocus -- ^ Window has mouse focus.
  | WindowForeign -- ^ Window not created by SDL.
  | WindowAllowHighDpi
  -- ^ Window should be created in high-DPI mode if supported. (>= SDL 2.0.1)
  -- Can be used at window creation.
  | WindowMouseCapture
  -- ^ Window has mouse captured (unrelated to InputGrabbed, >= SDL 2.0.4)
  deriving (Show)

createWindow :: 
  String -> WindowRectangle -> [WindowFlag] -> IO (Either Error Window)
createWindow title rect flags = undefined
