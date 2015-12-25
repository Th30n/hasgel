module Hasgel.Input (
  InputEvent(..),
  KeyboardKey(..),
  MouseButton(..),
  getEvents,
) where

import Data.Int (Int32)

import qualified Linear as L
import qualified SDL

-- | Returns all pending events.
getEvents :: IO [InputEvent]
getEvents = map mapEvent <$> SDL.pollEvents

-- | Input events
data InputEvent =
  QuitEvent
  | KeyPressedEvent KeyboardKey
  | KeyReleasedEvent KeyboardKey
  | MouseMotionEvent (L.V2 Int32) [MouseButton]
  | UnknownEvent

data MouseButton =
  ButtonLeft
  deriving (Eq, Show)

data KeyboardKey =
  KeyLeft
  | KeyRight
  | KeyUp
  | KeyDown
  | KeySpace
  | KeyLeftBracket
  | KeyA
  | KeyD
  | KeyE
  | KeyP
  | KeyQ
  | KeyS
  | KeyW
  | KeyUnknown
  deriving (Eq, Show)

mapEvent :: SDL.Event -> InputEvent
mapEvent (SDL.Event _ SDL.QuitEvent) = QuitEvent
mapEvent (SDL.Event _ (SDL.KeyboardEvent keyData)) =
  case SDL.keyboardEventKeyMotion keyData of
    SDL.Pressed -> KeyPressedEvent . mapSDLKey $ SDL.keyboardEventKeysym keyData
    SDL.Released -> KeyReleasedEvent . mapSDLKey $ SDL.keyboardEventKeysym keyData
mapEvent (SDL.Event _ (SDL.MouseMotionEvent mouseData)) =
  MouseMotionEvent (SDL.mouseMotionEventRelMotion mouseData) heldButtons
  where heldButtons = foldr collect [] $ SDL.mouseMotionEventState mouseData
        collect SDL.ButtonLeft bs = ButtonLeft : bs
        collect _ bs = bs

mapEvent _ = UnknownEvent

mapSDLKey :: SDL.Keysym -> KeyboardKey
mapSDLKey (SDL.Keysym SDL.ScancodeLeft _ _) = KeyLeft
mapSDLKey (SDL.Keysym SDL.ScancodeRight _ _) = KeyRight
mapSDLKey (SDL.Keysym SDL.ScancodeUp _ _) = KeyUp
mapSDLKey (SDL.Keysym SDL.ScancodeDown _ _) = KeyDown
mapSDLKey (SDL.Keysym SDL.ScancodeSpace _ _) = KeySpace
mapSDLKey (SDL.Keysym SDL.ScancodeLeftBracket _ _) = KeyLeftBracket
mapSDLKey (SDL.Keysym SDL.ScancodeA _ _) = KeyA
mapSDLKey (SDL.Keysym SDL.ScancodeD _ _) = KeyD
mapSDLKey (SDL.Keysym SDL.ScancodeE _ _) = KeyE
mapSDLKey (SDL.Keysym SDL.ScancodeP _ _) = KeyP
mapSDLKey (SDL.Keysym SDL.ScancodeQ _ _) = KeyQ
mapSDLKey (SDL.Keysym SDL.ScancodeS _ _) = KeyS
mapSDLKey (SDL.Keysym SDL.ScancodeW _ _) = KeyW
mapSDLKey _ = KeyUnknown
