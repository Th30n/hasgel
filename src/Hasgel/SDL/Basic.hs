module Hasgel.SDL.Basic (
  InitFlag(..),
  init
) where

import Data.Bits ((.|.))
import Data.List (foldl')
import Foreign.C.String (peekCString)
import Prelude hiding (init)

import qualified Graphics.UI.SDL.Basic as SDL
import qualified Graphics.UI.SDL.Enum as SDL

-- | Error message received from SDL library.
type Error = String

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
  deriving (Show, Eq, Ord)

-- | This function is used to initialize the SDL library.
-- It must be called before using any other SDL function.
-- On failure returns Error message otherwise Nothing
-- If you want to initialize subsystems separately pass an empty InitFlag list
-- and use initSubSystem.
init :: [InitFlag] -> IO (Maybe Error)
init flags = do
  let bitFlags = foldl' (.|.) 0 $ map marshalInitFlag flags
  r <- SDL.init bitFlags
  if r == 0 then return Nothing
  else do
    err <- getError
    return . Just $ "SDL_init error: " ++ err

marshalInitFlag :: Num a => InitFlag -> a
marshalInitFlag x = case x of
  InitTimer -> SDL.initFlagTimer
  InitAudio -> SDL.initFlagAudio
  InitVideo -> SDL.initFlagVideo
  InitJoystick -> SDL.initFlagJoystick
  InitHaptic -> SDL.initFlagHaptic
  InitGameController -> SDL.initFlagGameController
  InitEvents -> SDL.initFlagEvents
  InitEverything -> SDL.initFlagEverything
  InitNoParachute -> SDL.initFlagNoParachute

getError :: IO Error
getError = SDL.getError >>= (\e -> SDL.clearError >> peekCString e)
