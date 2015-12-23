module Hasgel.Input (
  InputEvent(..),
  KeyboardKey(..),
  getEvents,
) where

import qualified SDL

-- | Returns all pending events.
getEvents :: IO [InputEvent]
getEvents = map mapEvent <$> SDL.pollEvents

-- | Input events
data InputEvent =
  QuitEvent
  | KeyPressedEvent KeyboardKey
  | KeyReleasedEvent KeyboardKey
  | UnknownEvent

data KeyboardKey =
  KeyLeft
  | KeyRight
  | KeyUp
  | KeyDown
  | KeySpace
  | KeyLeftBracket
  | KeyP
  | KeyUnknown
  deriving (Eq, Show)

mapEvent :: SDL.Event -> InputEvent
mapEvent (SDL.Event _ SDL.QuitEvent) = QuitEvent
mapEvent (SDL.Event _ (SDL.KeyboardEvent keyData)) =
  case SDL.keyboardEventKeyMotion keyData of
    SDL.Pressed -> KeyPressedEvent . mapSDLKey $ SDL.keyboardEventKeysym keyData
    SDL.Released -> KeyReleasedEvent . mapSDLKey $ SDL.keyboardEventKeysym keyData
mapEvent _ = UnknownEvent

mapSDLKey :: SDL.Keysym -> KeyboardKey
mapSDLKey (SDL.Keysym SDL.ScancodeLeft _ _) = KeyLeft
mapSDLKey (SDL.Keysym SDL.ScancodeRight _ _) = KeyRight
mapSDLKey (SDL.Keysym SDL.ScancodeUp _ _) = KeyUp
mapSDLKey (SDL.Keysym SDL.ScancodeDown _ _) = KeyDown
mapSDLKey (SDL.Keysym SDL.ScancodeSpace _ _) = KeySpace
mapSDLKey (SDL.Keysym SDL.ScancodeLeftBracket _ _) = KeyLeftBracket
mapSDLKey (SDL.Keysym SDL.ScancodeP _ _) = KeyP
mapSDLKey _ = KeyUnknown
