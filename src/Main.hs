module Main ( main ) where

import Control.Monad.Except
import Control.Exception (bracket)
import Data.Word (Word32)
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import Graphics.UI.SDL as SDL
import Prelude

import Hasgel.Display
import Hasgel.SDL.Events as MySDL

main :: IO ()
main =
  withInit $
    either putStrLn (\d -> do
        current <- SDL.getTicks
        loop World { loopState = Continue, display = d,
            currentTime = current }
        destroyDisplay d) =<< runExceptT createDisplay

withInit :: IO () -> IO ()
withInit action = bracket (runExceptT initDisplay) (const quitDisplay) $
  either putStrLn (const action)

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
    let w = handleEvents event curw
    renderDisplay (display w) $ do
      let current = (fromIntegral (currentTime w) / 1000) :: GL.GLclampf
      let r = 0.5 + 0.5 * sin current
      let g = 0.5 + 0.5 * cos current
      GL.clearColor $= GL.Color4 r g 0.0 1.0
      GL.clear [GL.ColorBuffer]
    current <- SDL.getTicks
    loop w { currentTime = current}
  Quit -> return ()

handleEvents :: Maybe MySDL.Event -> WorldState a b -> WorldState a b
handleEvents Nothing w = w
handleEvents (Just e) w = case e of
    MySDL.QuitEvent _ _ -> w { loopState = Quit }
    _ -> w
