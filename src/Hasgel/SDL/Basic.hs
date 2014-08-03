-- | This module corresponds to SDL 2.0 Basics category
-- on official documentation wiki.
module Hasgel.SDL.Basic (
  -- * Initialization and shutdown
  InitFlag(..),
  init, initSubSystem, quit, quitSubSystem
) where

import Data.Bits ((.|.))
import Data.List (foldl')
import Data.Word (Word32)
import Foreign.C.String (peekCString)
import Foreign.C.Types (CInt)
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
-- On failure returns Error message otherwise ().
-- If you want to initialize subsystems separately pass an empty InitFlag list
-- and use initSubSystem.
init :: [InitFlag] -> IO (Either Error ())
init = initWithFun SDL.init

-- | This function is used to initialize specific SDL subsystems.
initSubSystem :: [InitFlag] -> IO (Either Error ())
initSubSystem = initWithFun SDL.initSubSystem

initWithFun :: (Word32 -> IO CInt) -> [InitFlag] -> IO (Either Error ())
initWithFun f flags = do
  let bitFlags = createBitFlags flags
  r <- f bitFlags
  if r == 0 then return $ Right ()
  else do
    err <- getError
    return . Left $ "SDL initialization error (" ++ show r ++"): " ++ err

createBitFlags :: [InitFlag] -> Word32
createBitFlags = foldl' (.|.) 0 . map marshalInitFlag

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

-- | Cleans up all initialized subsystems.
-- It should be called upon all exit conditions even if you have
-- already shut down each initialized subsystem with quitSubSystem.
-- If a system was started by calling that subsystem's initialization
-- function (e.g. videoInit) instead of init or initSubSystem then it must
-- be closed using that subsystem's quit function (e.g. videoQuit) before
-- calling quit.
quit :: IO ()
quit = SDL.quit

-- | Shut down specific SDL subsystems.
quitSubSystem :: [InitFlag] -> IO ()
quitSubSystem = SDL.quitSubSystem . createBitFlags

getError :: IO Error
getError = SDL.getError >>= (\e -> SDL.clearError >> peekCString e)
