module Main ( main ) where

import Control.Monad.Except
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (peek)
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import Prelude

import Hasgel.Display

main :: IO ()
main = do
  e <- runExceptT initDisplay
  case e of
    Right () -> do
      dr <- runExceptT createDisplay
      case dr of
        Right d -> do
          loop World { loopState = Continue, display = d }
          destroyDisplay d
        Left err -> putStrLn err
    Left err -> putStrLn err
  quitDisplay

data LoopState = Continue | Quit
data WorldState a b = World {
  loopState :: LoopState,
  display :: Display a b
}

loop :: WorldState a b -> IO ()
loop w = case loopState w of
  Continue -> do
    event <- getEvent
    renderDisplay (display w) $ do
      GL.clearColor $= GL.Color4 1.0 0.0 0.0 1.0
      GL.clear [GL.ColorBuffer]
    case event of
      Nothing -> loop w { loopState = Continue }
      Just e -> loop $ if SDL.eventType e == SDL.eventTypeQuit
                then w { loopState = Quit }
                else w { loopState = Continue }
  Quit -> return ()

getEvent :: IO (Maybe SDL.Event)
getEvent = alloca $ \e -> do
      r <- SDL.pollEvent e
      if r == 0 then return Nothing
      else do
        event <- peek e
        return $ Just event

