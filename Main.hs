import Prelude
import qualified Graphics.UI.SDL as SDL
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc (free)

main :: IO ()
main = do
  SDL.setMainReady
  e <- SDL.init SDL.initFlagEverything
  if e == 0 then do
    putStrLn "SDL_init succesful"
    w <- createWindow
    SDL.delay 3000
    SDL.destroyWindow w
  else getSDLErrorString >>= putStrLn
  SDL.quit

getSDLErrorString :: IO String
getSDLErrorString = SDL.getError >>= (\e -> SDL.clearError >> peekCString e)

createWindow :: IO SDL.Window
createWindow = do
  t <- newCString "hasgel"
  w <- SDL.createWindow t (CInt 0) (CInt 0) (CInt 800) (CInt 600) SDL.windowFlagOpenGL
  free t
  return w
