module Main ( main ) where

import Control.Monad.Except
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import Prelude

import Hasgel.Display
import Hasgel.SDL.Events as MySDL

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
    event <- MySDL.pollEvent
    renderDisplay (display w) $ do
      GL.clearColor $= GL.Color4 1.0 0.0 0.0 1.0
      GL.clear [GL.ColorBuffer]
    case event of
      Nothing -> loop w { loopState = Continue }
      Just e -> loop $ case e of 
                  MySDL.QuitEvent _ _ -> w { loopState = Quit }
                  _ -> w { loopState = Continue }
  Quit -> return ()
