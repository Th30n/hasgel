-- | This module corresponds to SDL 2.0 Video category
-- on official documentation wiki.
module Hasgel.SDL.Video (
  -- * Data types and enumerations
  WindowPos(..), WindowRectangle(..), WindowFlag(..), Window,
  GLContext, GLContextFlag(..), GLContextProfile(..),
  -- * Functions
  createWindow, destroyWindow, glCreateContext, glDeleteContext,
  glSetContextVersion, glSetContextFlags, glSetContextProfile,
  glSwapWindow
) where


import Control.Monad (when)
import Control.Monad.IO.Class
import Foreign.C.String (withCString)
import Foreign.C.Types (CInt)
import Foreign.Ptr (nullPtr)

import qualified Graphics.UI.SDL as SDL

import Hasgel.SDL.Basic
import Hasgel.SDL.BitFlag

-- | Defines x or y position of a window.
data WindowPos = Pos CInt | Centered | Undefined deriving (Show)

marshalWindowPos :: (Num a, Eq a) => WindowPos -> a
marshalWindowPos x = case x of
  Pos n -> fromIntegral n
  Centered -> SDL.SDL_WINDOWPOS_CENTERED
  Undefined -> SDL.SDL_WINDOWPOS_UNDEFINED

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
  | WindowOpenGL
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

marshalWindowFlag :: (Num a, Eq a) => WindowFlag -> a
marshalWindowFlag x = case x of
  WindowFullscreen -> SDL.SDL_WINDOW_FULLSCREEN
  WindowFullScreenDesktop -> SDL.SDL_WINDOW_FULLSCREEN_DESKTOP
  WindowOpenGL -> SDL.SDL_WINDOW_OPENGL
  WindowShown -> SDL.SDL_WINDOW_SHOWN
  WindowHidden -> SDL.SDL_WINDOW_HIDDEN
  WindowBorderless -> SDL.SDL_WINDOW_BORDERLESS
  WindowResizable -> SDL.SDL_WINDOW_RESIZABLE
  WindowMinimized -> SDL.SDL_WINDOW_MINIMIZED
  WindowMaximized -> SDL.SDL_WINDOW_MAXIMIZED
  WindowInputGrabbed -> SDL.SDL_WINDOW_INPUT_GRABBED
  WindowInputFocus -> SDL.SDL_WINDOW_INPUT_FOCUS
  WindowMouseFocus -> SDL.SDL_WINDOW_MOUSE_FOCUS
  WindowForeign -> SDL.SDL_WINDOW_FOREIGN
  WindowAllowHighDpi -> SDL.SDL_WINDOW_ALLOW_HIGHDPI
  -- Not yet implemented in raw bindings.
  -- WindowMouseCapture -> SDL.windowFlagMouseCapture

unmarshalWindowFlag :: (Num a, Eq a) => a -> Maybe WindowFlag
unmarshalWindowFlag = undefined

type Window = SDL.Window

-- | Creates a window with specified position, dimensions and flags.
--
-- Flags that are can be used during creation are:
-- 'WindowFullscreen', 'WindowFullScreenDesktop', 'WindowOpenGL',
-- 'WindowHidden', 'WindowBorderless', 'WindowResizable', 'WindowMinimized',
-- 'WindowMaximized', 'WindowInputGrabbed', 'WindowAllowHighDpi'.
--
-- On failure returns error message.
createWindow :: MonadIO m => String -> WindowRectangle -> [WindowFlag] -> m Window
createWindow title rect flags = do
  w <- createWindow'
  when (w == nullPtr) $ getError >>= fail
  return w
  where createWindow' = liftIO . withCString title $ \t -> do
          let x = marshalWindowPos $ posX rect
          let y = marshalWindowPos $ posY rect
          let bits = createBitFlags flags
          SDL.createWindow t x y (width rect) (height rect) bits

-- | This function destroys the window.
destroyWindow :: Window -> IO ()
destroyWindow = SDL.destroyWindow

type GLContext = SDL.GLContext

-- | Creates an OpenGL context for specified window and makes it current.
-- The window needs to have been created with 'WindowOpenGL' flag.
--
-- Returns error message on failure.
glCreateContext :: MonadIO m => Window -> m GLContext
glCreateContext w = do
  c <- SDL.glCreateContext w
  when (c == nullPtr) $ getError >>= fail
  return c

-- | Deletes an OpenGL context.
glDeleteContext :: GLContext -> IO ()
glDeleteContext = SDL.glDeleteContext

-- | Sets the OpenGL context major and minor version.
-- | Returns error on failure.
glSetContextVersion :: MonadIO m => CInt -> CInt -> m ()
glSetContextVersion major minor = do
  r <- SDL.glSetAttribute SDL.SDL_GL_CONTEXT_MAJOR_VERSION major
  when (r /= 0) $ getError >>= fail
  r' <- SDL.glSetAttribute SDL.SDL_GL_CONTEXT_MINOR_VERSION minor
  when (r' /= 0) $ getError >>= fail

-- | OpenGL profiles.
data GLContextProfile =
  GLCore
  -- ^ OpenGL core profile - deprecated functions are disallowed.
  | GLCompatibility
  -- ^ OpenGL compatibility profile - deprecated functions are allowed.
  | GLES
  -- ^ OpenGL ES profile - only a subset of the base OpenGL
  -- functionality is available.
  deriving (Show)

-- | Set the OpenGL context profile. Returns error on failure.
glSetContextProfile :: MonadIO m => GLContextProfile -> m ()
glSetContextProfile prof = do
  let prof' = case prof of
                GLCore -> SDL.SDL_GL_CONTEXT_PROFILE_CORE
                GLCompatibility -> SDL.SDL_GL_CONTEXT_PROFILE_COMPATIBILITY
                GLES -> SDL.SDL_GL_CONTEXT_PROFILE_ES
  r <- SDL.glSetAttribute SDL.SDL_GL_CONTEXT_PROFILE_MASK prof'
  when (r /= 0) $ getError >>= fail

-- | OpenGL context flags.
data GLContextFlag =
  GLDebug
  -- ^ This flag is intended to put GL into a debug mode which might offer
  -- better developer insights, possibly at a loss of performance.
  | GLForwardCompatible
  -- ^ Puts GL into forward compatible mode, which means no depracated
  -- functionality will be supported, possibly at a gain of performance.
  -- Applies only to GL 3.0 and later contexts.
  | GLRobustAccess
  -- ^ This flag is intended to require a GL context that supports the
  -- GL_ARB_robustness extension which offers a few APIs that are safer
  -- than the usual defaults.
  | GLResetIsolation
  -- ^ Requires the GL to make promises about what to do in the face of driver
  -- or hardware failure.
  deriving (Show, Eq, Ord, Bounded, Enum)

instance BitFlag GLContextFlag where
  marshalBitFlag GLDebug = SDL.SDL_GL_CONTEXT_DEBUG_FLAG
  marshalBitFlag GLForwardCompatible =
    SDL.SDL_GL_CONTEXT_FORWARD_COMPATIBLE_FLAG
  marshalBitFlag GLRobustAccess = SDL.SDL_GL_CONTEXT_ROBUST_ACCESS_FLAG
  marshalBitFlag GLResetIsolation = SDL.SDL_GL_CONTEXT_RESET_ISOLATION_FLAG

  unmarshalBitFlag =
    error "TODO: Hasgel.SDL.Video.GLContextFlag => unmarshalBitFlag"

-- | Sets OpenGL context flags. Returns error on failure.
glSetContextFlags :: MonadIO m => [GLContextFlag] -> m ()
glSetContextFlags flags = do
  let flags' = createBitFlags flags
  r <- SDL.glSetAttribute SDL.SDL_GL_CONTEXT_FLAGS flags'
  when (r /= 0) $ getError >>= fail

-- | This function is used for updating a window with OpenGL rendering.
glSwapWindow :: Window -> IO ()
glSwapWindow = SDL.glSwapWindow
