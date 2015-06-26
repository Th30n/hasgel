{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | This module corresponds to SDL 2.0 Basics category
-- on official documentation wiki.
module Hasgel.SDL.Basic (
  -- * Initialization and shutdown
  InitFlag(..), withInit,
  init, initSubSystem, quit, quitSubSystem, setMainReady, wasInit,
  -- * Error handling
  SDLException(..),
  clearError, getError
) where


import Control.Exception (Exception, bracket_, throwIO)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Control (MonadBaseControl, liftBaseOp_)
import Data.Monoid ((<>))
import Data.Typeable (Typeable)
import Data.Word (Word32)
import Foreign.C.String (peekCString)
import Foreign.C.Types (CInt)
import Prelude hiding (init)

import qualified Graphics.UI.SDL.Basic as SDL
import qualified Graphics.UI.SDL.Enum as SDL

import Hasgel.SDL.BitFlag

-- | Set of flags for initializing subsystems.
-- Event handling, File and Threading subsystems are initialized by default.
data InitFlag =
    InitTimer -- ^ Timer subsystem.
  | InitAudio -- ^ Audio subsystem.
  | InitVideo -- ^ Video subsystem.
  | InitJoystick -- ^ Joystick subsystem.
  | InitHaptic -- ^ Haptic (force feedback) subsystem.
  | InitGameController -- ^ Controller subsystem.
  | InitEvents -- ^ Events subsystem.
  | InitEverything -- ^ Initalizes all of the above subsystems.
  | InitNoParachute -- ^ Compatibility flag, it is ignored.
  deriving (Show, Eq, Ord, Bounded, Enum)

instance BitFlag InitFlag where
  marshalBitFlag = fromIntegral . marshalInitFlag
  unmarshalBitFlag = unmarshalInitFlag

marshalInitFlag :: InitFlag -> Word32
marshalInitFlag x = case x of
  InitTimer -> SDL.SDL_INIT_TIMER
  InitAudio -> SDL.SDL_INIT_AUDIO
  InitVideo -> SDL.SDL_INIT_VIDEO
  InitJoystick -> SDL.SDL_INIT_JOYSTICK
  InitHaptic -> SDL.SDL_INIT_HAPTIC
  InitGameController -> SDL.SDL_INIT_GAMECONTROLLER
  InitEvents -> SDL.SDL_INIT_EVENTS
  InitEverything -> SDL.SDL_INIT_EVERYTHING
  InitNoParachute -> SDL.SDL_INIT_NOPARACHUTE

unmarshalInitFlag :: (Num a, Eq a) => a -> Maybe InitFlag
unmarshalInitFlag x
  | x == fromIntegral SDL.SDL_INIT_TIMER = Just InitTimer
  | x == fromIntegral SDL.SDL_INIT_AUDIO = Just InitAudio
  | x == fromIntegral SDL.SDL_INIT_VIDEO = Just InitVideo
  | x == fromIntegral SDL.SDL_INIT_JOYSTICK = Just InitJoystick
  | x == fromIntegral SDL.SDL_INIT_HAPTIC = Just InitHaptic
  | x == fromIntegral SDL.SDL_INIT_GAMECONTROLLER = Just InitGameController
  | x == fromIntegral SDL.SDL_INIT_EVENTS = Just InitEvents
  | x == fromIntegral SDL.SDL_INIT_EVERYTHING = Just InitEverything
  | x == fromIntegral SDL.SDL_INIT_NOPARACHUTE = Just InitNoParachute
  | otherwise = Nothing

-- | Initializes SDL, performs the action and quits SDL.
-- Cleanup is performed also in case of an exception.
withInit :: (MonadBaseControl IO m) => [InitFlag] -> m a -> m a
withInit flags = liftBaseOp_ $ bracket_ (init flags) quit

-- | This function is used to initialize the SDL library.
-- It must be called before using any other SDL function.
-- On failure returns Error message otherwise ().
-- If you want to initialize subsystems separately pass an empty InitFlag list
-- and use initSubSystem.
init :: MonadIO m => [InitFlag] -> m ()
init = initWithFun SDL.init

-- | This function is used to initialize specific SDL subsystems.
initSubSystem :: MonadIO m => [InitFlag] -> m ()
initSubSystem = initWithFun SDL.initSubSystem

initWithFun :: MonadIO m => (SDL.InitFlag -> m CInt) -> [InitFlag] -> m ()
initWithFun f flags = do
  r <- f $ createBitFlags flags
  unless (r == 0) $ do
    errMsg <- getError
    liftIO . throwIO . InitializationError $ show r <> " : " <> errMsg

-- | Cleans up all initialized subsystems.
-- It should be called upon all exit conditions even if you have
-- already shut down each initialized subsystem with quitSubSystem.
-- If a system was started by calling that subsystem's initialization
-- function (e.g. videoInit) instead of init or initSubSystem then it must
-- be closed using that subsystem's quit function (e.g. videoQuit) before
-- calling quit.
quit :: MonadIO m => m ()
quit = SDL.quit

-- | Shut down specific SDL subsystems.
quitSubSystem :: MonadIO m => [InitFlag] -> m ()
quitSubSystem = SDL.quitSubSystem . createBitFlags

-- | This function is used to avoid failure of init when not using SDL_main()
-- as an entry point. To ensure the main() function will not be changed it
-- is necessary to define SDL_MAIN_HANDLED before including SDL.h.
-- I'm not really sure how this affects Haskell code. On my ArchLinux SDL
-- works fine without calling this function or providing C defines.
setMainReady :: MonadIO m => m ()
setMainReady = SDL.setMainReady

-- | Takes a list of InitFlag as a mask for querying which subsystems
-- are initialized. Returns a list of InitFlag corresponding to initialized
-- systems satisfying the given mask.
-- Giving an empty list as a mask is equivalent to using InitEverything
-- as a mask.
wasInit :: MonadIO m => [InitFlag] -> m [InitFlag]
wasInit mask = do
  ss <- SDL.wasInit $ createBitFlags mask
  return $ fromBitFlags ss

data SDLException =
  InitializationError String |
  GLError String |
  WindowError String |
  SDLError String
  deriving (Eq, Show, Typeable)

instance Exception SDLException

-- | Returns the error message of last error or empty string if no error.
-- The error message is then cleared from SDL.
-- This message might not be necessary if every call of SDL functions
-- from here will return Left Error upon error.
getError :: MonadIO m => m String
getError = SDL.getError >>= (\e -> clearError >> liftIO (peekCString e))

-- | Clears any previous error message.
-- I don't think this is necessary since getError cleans up after.
clearError :: MonadIO m => m ()
clearError = SDL.clearError
