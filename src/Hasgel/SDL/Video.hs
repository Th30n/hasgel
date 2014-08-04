-- | This module corresponds to SDL 2.0 Video category
-- on official documentation wiki.
module Hasgel.SDL.Video (
  WindowPos(..), WindowRectangle(..), WindowFlag(..),
  Window, createWindow
) where

import Control.Monad (liftM)
import Control.Exception (bracket)
import Foreign.C.String (newCString)
import Foreign.C.Types (CInt)
import Foreign.Marshal.Alloc (free)
import Foreign.Ptr (nullPtr)

import qualified Graphics.UI.SDL as SDL

import Hasgel.SDL.Basic
import Hasgel.SDL.BitFlag

-- | Defines x or y position of a window.
data WindowPos = Pos CInt | Centered | Undefined deriving (Show)

marshalWindowPos :: Num a => WindowPos -> a
marshalWindowPos x = case x of
  Pos n -> fromIntegral n
  Centered -> SDL.windowPosCentered
  Undefined -> SDL.windowPosUndefined

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
  -- ^ Window is not visible (overrides WindowShown at creation).
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
  
  -- WindowMouseCapture
  -- Window has mouse captured (unrelated to InputGrabbed, >= SDL 2.0.4)
  deriving (Show, Eq, Ord, Bounded, Enum)

instance BitFlag WindowFlag where
  marshalBitFlag = marshalWindowFlag
  unmarshalBitFlag = unmarshalWindowFlag

marshalWindowFlag :: Num a => WindowFlag -> a
marshalWindowFlag x = case x of
  WindowFullscreen -> SDL.windowFlagFullscreen
  WindowFullScreenDesktop -> SDL.windowFlagFullscreenDesktop
  WindowOpenGl -> SDL.windowFlagOpenGL
  WindowShown -> SDL.windowFlagShown
  WindowHidden -> SDL.windowFlagHidden
  WindowBorderless -> SDL.windowFlagBorderless
  WindowResizable -> SDL.windowFlagResizable
  WindowMinimized -> SDL.windowFlagMinimized
  WindowMaximized -> SDL.windowFlagMaximized
  WindowInputGrabbed -> SDL.windowFlagInputGrabbed
  WindowInputFocus -> SDL.windowFlagInputFocus
  WindowMouseFocus -> SDL.windowFlagMouseFocus
  WindowForeign -> SDL.windowFlagForeign
  WindowAllowHighDpi -> SDL.windowFlagAllowHighDPI
  -- Not yet implemented in raw bindings.
  -- WindowMouseCapture -> SDL.windowFlagMouseCapture

unmarshalWindowFlag :: (Num a, Eq a) => a -> Maybe WindowFlag
unmarshalWindowFlag = undefined

type Window = SDL.Window

createWindow :: 
  String -> WindowRectangle -> [WindowFlag] -> IO (Either Error Window)
createWindow title rect flags =
  bracket (newCString title) free $ \t -> do
    let x = marshalWindowPos $ posX rect
    let y = marshalWindowPos $ posY rect
    let bits = createBitFlags flags
    w <- SDL.createWindow t x y (width rect) (height rect) bits
    if w == nullPtr then liftM Left getError
    else return $ Right w
