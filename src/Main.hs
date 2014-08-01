module Main ( main ) where

import Control.Monad.Except
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc (free)
import Foreign.Ptr (nullPtr)
import qualified Graphics.UI.SDL as SDL
import Prelude

main :: IO ()
main = do
  SDL.setMainReady
  e <- SDL.init SDL.initFlagEverything
  if e == 0 then do
    putStrLn "SDL_init succesful"
    res <- runExceptT createWindow
    case res of
      Right w -> do
        SDL.delay 3000
        SDL.destroyWindow w
      Left err -> putStrLn err
  else getSDLErrorString >>= putStrLn
  SDL.quit

getSDLErrorString :: IO String
getSDLErrorString = SDL.getError >>= (\e -> SDL.clearError >> peekCString e)

createWindow :: ExceptT String IO SDL.Window
createWindow = do
  t <- liftIO $ newCString "hasgel"
  w <- liftIO $ SDL.createWindow t (CInt 0) (CInt 0)
                      (CInt 800) (CInt 600) SDL.windowFlagOpenGL
  liftIO $ free t
  if w == nullPtr
  then liftIO getSDLErrorString >>= throwError
  else return w
