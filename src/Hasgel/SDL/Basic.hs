-- | This module corresponds to SDL 2.0 Basics category
-- on official documentation wiki.
module Hasgel.SDL.Basic (
  -- * Initialization and shutdown
  InitFlag(..),
  init, initSubSystem, quit, quitSubSystem, setMainReady, wasInit
) where

import Data.Bits ((.|.), (.&.))
import Data.List (foldl')
import Data.Maybe (mapMaybe)
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
  r <- f $ createBitFlags flags
  if r == 0 then return $ Right ()
  else do
    err <- getError
    return . Left $ "SDL initialization error (" ++ show r ++"): " ++ err

createBitFlags :: [InitFlag] -> Word32
createBitFlags = foldl' (.|.) 0 . map marshalInitFlag

-- | Plain numerical SDL flags.
rawInitFlags :: Num a => [a]
rawInitFlags = [SDL.initFlagTimer, SDL.initFlagAudio, SDL.initFlagVideo,
  SDL.initFlagJoystick, SDL.initFlagHaptic, SDL.initFlagGameController,
  SDL.initFlagEvents, SDL.initFlagEverything, SDL.initFlagNoParachute]

marshalToInitFlag :: (Num a, Eq a) => a -> Maybe InitFlag
marshalToInitFlag x
  | x == SDL.initFlagTimer = Just InitTimer
  | x == SDL.initFlagAudio = Just InitAudio
  | x == SDL.initFlagVideo = Just InitVideo
  | x == SDL.initFlagJoystick = Just InitJoystick
  | x == SDL.initFlagHaptic = Just InitHaptic
  | x == SDL.initFlagGameController = Just InitGameController
  | x == SDL.initFlagEvents = Just InitEvents
  | x == SDL.initFlagEverything = Just InitEverything
  | x == SDL.initFlagNoParachute = Just InitNoParachute
  | otherwise = Nothing

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

-- | This function is used to avoid failure of init when not using SDL_main()
-- as an entry point. To ensure the main() function will not be changed it
-- is necessary to define SDL_MAIN_HANDLED before including SDL.h.
-- I'm not really sure how this affects Haskell code. On my ArchLinux SDL
-- works fine without calling this function or providing C defines.
setMainReady :: IO ()
setMainReady = SDL.setMainReady

-- | Takes a list of InitFlag as a mask for querying which subsystems
-- are initialized. Returns a list of InitFlag corresponding to initialized
-- systems satisfying the given mask.
-- Giving an empty list as a mask is equivalent to using InitEverything
-- as a mask.
wasInit :: [InitFlag] -> IO [InitFlag]
wasInit mask = do
  ss <- SDL.wasInit $ createBitFlags mask
  return $ fromBitFlags ss

fromBitFlags :: Word32 -> [InitFlag]
fromBitFlags bits = mapMaybe marshalToInitFlag flags
  where flags = filter ((`elem` rawInitFlags) . (.&. bits)) rawInitFlags

getError :: IO Error
getError = SDL.getError >>= (\e -> SDL.clearError >> peekCString e)
