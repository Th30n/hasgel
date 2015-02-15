module Main ( main ) where

import Control.Error
import Data.Word (Word32)
import Foreign.Marshal.Array (withArray)
import Graphics.GL.Core45
import Graphics.UI.SDL as SDL
import Prelude

import Hasgel.Display
import Hasgel.SDL.Basic as MySDL
import Hasgel.SDL.Events as MySDL

main :: IO ()
main =
  MySDL.withInit [MySDL.InitVideo] $ runScript createDisplay >>=
    (\d -> do
      current <- SDL.getTicks
      loop World { loopState = Continue, display = d,
                   currentTime = current }
      destroyDisplay d)

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
      let current = fromIntegral (currentTime w) / 1000
      let r = 0.5 + 0.5 * sin current
      let g = 0.5 + 0.5 * cos current
      withArray [r, g, 0.0, 1.0] $ \color ->
        glClearBufferfv GL_COLOR 0 color
    current <- SDL.getTicks
    loop w { currentTime = current}
  Quit -> return ()

handleEvent :: WorldState a b -> MySDL.Event -> WorldState a b
handleEvent w (MySDL.QuitEvent _ _) = w { loopState = Quit }
handleEvent w _ = w
