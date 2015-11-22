module Hasgel.Game (
  GameState(..), Ticcmd, PlayerCmd(..),
  gameState, ticGame, addTiccmd, buildTiccmd
) where

import Hasgel.Simulation (Time (..), millis2Sec)
import Hasgel.Transform (Transform (..), translate)

import qualified Linear as L

-- | State of the game.
data GameState = GameState
  { gTiccmds :: [Ticcmd] -- ^ Commands to be processed.
  , gOldTiccmds :: [Ticcmd] -- ^ Processed commands, used for recording.
  , gPlayerTransform :: Transform
  , gPlayerShots :: [Transform]
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
  where playerSpeed = 10 * millis2Sec (timeDelta time)
        build' cmd MoveLeft = cmd { cmdMove = cmdMove cmd - playerSpeed }
        build' cmd MoveRight = cmd { cmdMove = cmdMove cmd + playerSpeed }
        build' cmd Shoot = cmd { cmdShoot = True }

ticPlayer :: GameState -> GameState
ticPlayer gs
  | null (gTiccmds gs) = gs
  | otherwise = let ticcmd:nextTiccmds = gTiccmds gs
                    currTrans = gPlayerTransform gs
                    currShots = gPlayerShots gs
                    trans = playerMove currTrans $ cmdMove ticcmd
                    shots = playerShoot currTrans currShots $ cmdShoot ticcmd
                in gs { gTiccmds = nextTiccmds,
                        gOldTiccmds = ticcmd : gOldTiccmds gs,
                        gPlayerTransform = trans,
                        gPlayerShots = shots }

ticGame :: Time -> GameState -> GameState
ticGame time = ticShots time . ticPlayer

ticShots :: Time -> GameState -> GameState
ticShots time gs
  | null (gPlayerShots gs) = gs
  | otherwise = let shotSpeed = 15 * millis2Sec (timeDelta time)
                in gs { gPlayerShots = shotMove shotSpeed <$> gPlayerShots gs }

playerMove :: Transform -> Float -> Transform
playerMove prev dx = translate prev $ L.V3 dx 0 0

playerShoot :: Transform -> [Transform] -> Bool -> [Transform]
playerShoot _ shots False = shots
playerShoot trans shots True =
  let shotPos = L.V3 0 1 0 + transformPosition trans
      newShot = Transform { transformScale = L.V3 0.25 0.5 0.25,
                            transformRotation = L.zero,
                            transformPosition = shotPos }
  in newShot : shots

shotMove :: Float -> Transform -> Transform
shotMove dy shotPos = translate shotPos $ L.V3 0 dy 0

emptyTiccmd :: Ticcmd
emptyTiccmd = Ticcmd { cmdMove = 0, cmdShoot = False }

gameState :: [Ticcmd] -> GameState
gameState ticcmds =
  let model = Transform L.zero (L.V3 1 1 1) L.zero
  in GameState { gTiccmds = ticcmds, gOldTiccmds = [],
                 gPlayerTransform = model, gPlayerShots = [] }
