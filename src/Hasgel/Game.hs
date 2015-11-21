module Hasgel.Game (
  GameState(..), Ticcmd, PlayerCmd(..),
  gameState, ticPlayer, addTiccmd, buildTiccmd
) where

import Hasgel.Simulation (Time (..), millis2Sec)
import Hasgel.Transform (Transform (..), translate)

import qualified Linear as L

-- | State of the game.
data GameState = GameState
  { gTiccmds :: [Ticcmd] -- ^ Commands to be processed.
  , gOldTiccmds :: [Ticcmd] -- ^ Processed commands, used for recording.
  , gPlayerTransform :: Transform
  }

-- | Commands for one tic of gameplay.
data Ticcmd = Ticcmd
  { cmdMove :: Float -- ^ Horizontal movement speed.
  , cmdShoot :: Bool -- ^ True if shooting.
  } deriving (Show, Read)

-- | Player control commands.
data PlayerCmd =
  MoveLeft
  | MoveRight
  | Shoot
  deriving (Show, Ord, Eq)

addTiccmd :: GameState -> Ticcmd -> GameState
addTiccmd gs ticcmd = gs { gTiccmds = gTiccmds gs ++ [ticcmd] }

buildTiccmd :: Foldable f => f PlayerCmd -> Time -> Ticcmd
buildTiccmd inputs time = foldl build' emptyTiccmd inputs
  where playerSpeed = 4 * millis2Sec (timeDelta time)
        build' cmd MoveLeft = cmd { cmdMove = cmdMove cmd - playerSpeed }
        build' cmd MoveRight = cmd { cmdMove = cmdMove cmd + playerSpeed }
        build' cmd Shoot = cmd { cmdShoot = True }

ticPlayer :: GameState -> GameState
ticPlayer gs
  | null (gTiccmds gs) = gs
  | otherwise = let ticcmd:nextTiccmds = gTiccmds gs
                    trans = movePlayer (gPlayerTransform gs) $ cmdMove ticcmd
                in gs { gTiccmds = nextTiccmds,
                        gOldTiccmds = ticcmd : gOldTiccmds gs,
                        gPlayerTransform = trans }

movePlayer :: Transform -> Float -> Transform
movePlayer prev dx = translate prev $ L.V3 dx 0 0

emptyTiccmd :: Ticcmd
emptyTiccmd = Ticcmd { cmdMove = 0, cmdShoot = False }

gameState :: [Ticcmd] -> GameState
gameState ticcmds =
  let model = Transform L.zero (L.V3 0.5 0.5 0.5) (L.V3 0 0 (-5))
  in GameState { gTiccmds = ticcmds, gOldTiccmds = [], gPlayerTransform = model }
