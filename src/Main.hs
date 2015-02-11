module Main ( main ) where

import Control.Error
import Control.Exception (bracket_)
import Data.Word (Word32)
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.UI.SDL as SDL
import Prelude

import Hasgel.Display
import Hasgel.SDL.Events as MySDL

main :: IO ()
main =
  withInit $ runScript createDisplay >>=
    (\d -> do
      current <- SDL.getTicks
      loop World { loopState = Continue, display = d,
                   currentTime = current }
      destroyDisplay d)

-- | Initializes SDL, performs the action and quits SDL.
-- | Cleanup is performed also in case of an exception.
withInit :: IO a -> IO a
withInit = bracket_ (runScript initDisplay) quitDisplay

data LoopState = Continue | Quit

data WorldState a b = World {
  loopState :: LoopState,
  display :: Display a b,
  currentTime :: Word32 -- ^ Time in milliseconds
}

loop :: WorldState a b -> IO ()
loop curw = case loopState curw of
  Continue -> do
    event <- MySDL.pollEvent
    let w = maybe curw (handleEvent curw) event
    renderDisplay (display w) $ do
      let current = (fromIntegral (currentTime w) / 1000) :: GL.GLclampf
      let r = 0.5 + 0.5 * sin current
      let g = 0.5 + 0.5 * cos current
      GL.clearColor $= GL.Color4 r g 0.0 1.0
      GL.clear [GL.ColorBuffer]
    current <- SDL.getTicks
    loop w { currentTime = current}
  Quit -> return ()

handleEvent :: WorldState a b -> MySDL.Event -> WorldState a b
handleEvent w (MySDL.QuitEvent _ _) = w { loopState = Quit }
handleEvent w _ = w
