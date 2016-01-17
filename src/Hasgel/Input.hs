module Hasgel.Input (
  InputEvent(..),
  KeyboardKey(..),
  MouseButton(..),
  KeyMod (..),
  getEvents, key2Char
) where

import Data.Char (toUpper)
import Data.Foldable (foldl')
import Data.Int (Int32)

import qualified Linear as L
import qualified SDL

-- | Returns all pending events.
getEvents :: IO ([InputEvent], [KeyMod])
getEvents = do
  events <- map mapEvent <$> SDL.pollEvents
  modifiers <- mapModState <$> SDL.getModState
  pure (events, modifiers)

-- | Keyboard modifiers.
data KeyMod =
  ModShift
  deriving (Eq, Show)

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
  KeyLeft | KeyRight | KeyUp | KeyDown
  | KeySpace
  | KeyLeftBracket
  | KeyTilde
  | KeyReturn
  | KeyBackspace
  | KeyMinus
  | KeyEsc
  | KeyPeriod
  | Key1 | Key2 | Key3 | Key4 | Key5 | Key6 | Key7 | Key8 | Key9 | Key0
  | KeyQ | KeyW | KeyE | KeyR | KeyT | KeyY | KeyU | KeyI | KeyO | KeyP
  | KeyA | KeyS | KeyD | KeyF | KeyG | KeyH | KeyJ | KeyK | KeyL
  | KeyZ | KeyX | KeyC | KeyV | KeyB | KeyN | KeyM
  | KeyUnknown String
  deriving (Eq, Show)

mapModState :: SDL.KeyModifier -> [KeyMod]
mapModState keyMod =
  let mapping = [(SDL.keyModifierLeftShift, [ModShift]),
                 (SDL.keyModifierRightShift, [ModShift])]
      go acc (accessor, mods) = if accessor keyMod then mods ++ acc else acc
  in foldl' go [] mapping

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
-- Non letters.
mapSDLKey (SDL.Keysym SDL.ScancodeLeft _ _) = KeyLeft
mapSDLKey (SDL.Keysym SDL.ScancodeRight _ _) = KeyRight
mapSDLKey (SDL.Keysym SDL.ScancodeUp _ _) = KeyUp
mapSDLKey (SDL.Keysym SDL.ScancodeDown _ _) = KeyDown
mapSDLKey (SDL.Keysym SDL.ScancodeSpace _ _) = KeySpace
mapSDLKey (SDL.Keysym SDL.ScancodeLeftBracket _ _) = KeyLeftBracket
mapSDLKey (SDL.Keysym SDL.ScancodeGrave _ _) = KeyTilde
mapSDLKey (SDL.Keysym SDL.ScancodeReturn _ _) = KeyReturn
mapSDLKey (SDL.Keysym SDL.ScancodeBackspace _ _) = KeyBackspace
mapSDLKey (SDL.Keysym SDL.ScancodeMinus _ _) = KeyMinus
mapSDLKey (SDL.Keysym SDL.ScancodeEscape _ _) = KeyEsc
mapSDLKey (SDL.Keysym SDL.ScancodePeriod _ _) = KeyPeriod
-- Numbers
mapSDLKey (SDL.Keysym SDL.Scancode1 _ _) = Key1
mapSDLKey (SDL.Keysym SDL.Scancode2 _ _) = Key2
mapSDLKey (SDL.Keysym SDL.Scancode3 _ _) = Key3
mapSDLKey (SDL.Keysym SDL.Scancode4 _ _) = Key4
mapSDLKey (SDL.Keysym SDL.Scancode5 _ _) = Key5
mapSDLKey (SDL.Keysym SDL.Scancode6 _ _) = Key6
mapSDLKey (SDL.Keysym SDL.Scancode7 _ _) = Key7
mapSDLKey (SDL.Keysym SDL.Scancode8 _ _) = Key8
mapSDLKey (SDL.Keysym SDL.Scancode9 _ _) = Key9
mapSDLKey (SDL.Keysym SDL.Scancode0 _ _) = Key0
-- Letters, qwerty order.
mapSDLKey (SDL.Keysym SDL.ScancodeQ _ _) = KeyQ
mapSDLKey (SDL.Keysym SDL.ScancodeW _ _) = KeyW
mapSDLKey (SDL.Keysym SDL.ScancodeE _ _) = KeyE
mapSDLKey (SDL.Keysym SDL.ScancodeR _ _) = KeyR
mapSDLKey (SDL.Keysym SDL.ScancodeT _ _) = KeyT
mapSDLKey (SDL.Keysym SDL.ScancodeY _ _) = KeyY
mapSDLKey (SDL.Keysym SDL.ScancodeU _ _) = KeyU
mapSDLKey (SDL.Keysym SDL.ScancodeI _ _) = KeyI
mapSDLKey (SDL.Keysym SDL.ScancodeO _ _) = KeyO
mapSDLKey (SDL.Keysym SDL.ScancodeP _ _) = KeyP
mapSDLKey (SDL.Keysym SDL.ScancodeA _ _) = KeyA
mapSDLKey (SDL.Keysym SDL.ScancodeS _ _) = KeyS
mapSDLKey (SDL.Keysym SDL.ScancodeD _ _) = KeyD
mapSDLKey (SDL.Keysym SDL.ScancodeF _ _) = KeyF
mapSDLKey (SDL.Keysym SDL.ScancodeG _ _) = KeyG
mapSDLKey (SDL.Keysym SDL.ScancodeH _ _) = KeyH
mapSDLKey (SDL.Keysym SDL.ScancodeJ _ _) = KeyJ
mapSDLKey (SDL.Keysym SDL.ScancodeK _ _) = KeyK
mapSDLKey (SDL.Keysym SDL.ScancodeL _ _) = KeyL
mapSDLKey (SDL.Keysym SDL.ScancodeZ _ _) = KeyZ
mapSDLKey (SDL.Keysym SDL.ScancodeX _ _) = KeyX
mapSDLKey (SDL.Keysym SDL.ScancodeC _ _) = KeyC
mapSDLKey (SDL.Keysym SDL.ScancodeV _ _) = KeyV
mapSDLKey (SDL.Keysym SDL.ScancodeB _ _) = KeyB
mapSDLKey (SDL.Keysym SDL.ScancodeN _ _) = KeyN
mapSDLKey (SDL.Keysym SDL.ScancodeM _ _) = KeyM
-- Anything else
mapSDLKey (SDL.Keysym sc _ _) = KeyUnknown (show sc)

key2Char :: [KeyMod] -> KeyboardKey -> Maybe Char
key2Char mods key = if shift then toUpper <$> go key else go key
  where shift = ModShift `elem` mods
        -- numbers
        go Key1 = Just '1'
        go Key2 = Just '2'
        go Key3 = Just '3'
        go Key4 = Just '4'
        go Key5 = Just '5'
        go Key6 = Just '6'
        go Key7 = Just '7'
        go Key8 = Just '8'
        go Key9 = Just '9'
        go Key0 = Just '0'
        -- letters
        go KeyQ = Just 'q'
        go KeyW = Just 'w'
        go KeyE = Just 'e'
        go KeyR = Just 'r'
        go KeyT = Just 't'
        go KeyY = Just 'y'
        go KeyU = Just 'u'
        go KeyI = Just 'i'
        go KeyO = Just 'o'
        go KeyP = Just 'p'
        go KeyA = Just 'a'
        go KeyS = Just 's'
        go KeyD = Just 'd'
        go KeyF = Just 'f'
        go KeyG = Just 'g'
        go KeyH = Just 'h'
        go KeyJ = Just 'j'
        go KeyK = Just 'k'
        go KeyL = Just 'l'
        go KeyZ = Just 'z'
        go KeyX = Just 'x'
        go KeyC = Just 'c'
        go KeyV = Just 'v'
        go KeyB = Just 'b'
        go KeyN = Just 'n'
        go KeyM = Just 'm'
        -- special
        go KeySpace = Just ' '
        go KeyPeriod = Just $ if shift then '>' else '.'
        go KeyMinus = Just $ if shift then '_' else '-'
        go _ = Nothing
