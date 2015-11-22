module Hasgel.Game (
  GameState(..), Ticcmd, PlayerCmd(..), Player (..),
  gameState, ticGame, addTiccmd, buildTiccmd
) where

import Hasgel.Simulation (Milliseconds, Time (..), millis2Sec)
import Hasgel.Transform (Transform (..), translate)

import qualified Linear as L

-- | State of the game.
data GameState = GameState
  { gTiccmds :: [Ticcmd] -- ^ Commands to be processed.
  , gOldTiccmds :: [Ticcmd] -- ^ Processed commands, used for recording.
  , gPlayer :: Player
  , gPlayerShots :: [Transform]
  }

data Player = Player
  { playerTransform :: Transform
  , playerShotTime :: Milliseconds
    -- ^ Time when the next shot can be fired. This allows shooting right at
    -- the start, when the time is 0.
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

ticPlayer :: Time -> GameState -> GameState
ticPlayer time gs
  | null (gTiccmds gs) = gs
  | otherwise = let ticcmd:nextTiccmds = gTiccmds gs
                    currPlayer = gPlayer gs
                    currTrans = playerTransform currPlayer
                    currShots = gPlayerShots gs
                    trans = playerMove currTrans $ cmdMove ticcmd
                    (player, shots) =
                      playerShoot time currPlayer currShots $ cmdShoot ticcmd
                in gs { gTiccmds = nextTiccmds,
                        gOldTiccmds = ticcmd : gOldTiccmds gs,
                        gPlayer = player { playerTransform = trans },
                        gPlayerShots = shots }

ticGame :: Time -> GameState -> GameState
ticGame time = ticShots time . ticPlayer time

ticShots :: Time -> GameState -> GameState
ticShots time gs
  | null (gPlayerShots gs) = gs
  | otherwise = let shotSpeed = 15 * millis2Sec (timeDelta time)
                in gs { gPlayerShots = shotMove shotSpeed <$> gPlayerShots gs }

playerMove :: Transform -> Float -> Transform
playerMove prev dx = translate prev $ L.V3 dx 0 0

shootCooldown :: Milliseconds
shootCooldown = 500

playerShoot :: Time -> Player -> [Transform] -> Bool -> (Player, [Transform])
playerShoot _ player shots False = (player, shots)
playerShoot time player shots True
  | timeCurrent time < playerShotTime player = (player, shots)
  | otherwise = let trans = playerTransform player
                    shotPos = L.V3 0 1 0 + transformPosition trans
                    newShot = Transform { transformScale = L.V3 0.25 0.5 0.25,
                                          transformRotation = L.zero,
                                          transformPosition = shotPos }
                in (player { playerShotTime = shootCooldown + timeCurrent time },
                    newShot:shots)

shotMove :: Float -> Transform -> Transform
shotMove dy shotPos = translate shotPos $ L.V3 0 dy 0

emptyTiccmd :: Ticcmd
emptyTiccmd = Ticcmd { cmdMove = 0, cmdShoot = False }

gameState :: [Ticcmd] -> GameState
gameState ticcmds =
  let model = Transform L.zero (L.V3 1 1 1) L.zero
      player = Player { playerTransform = model, playerShotTime = 0 }
  in GameState { gTiccmds = ticcmds, gOldTiccmds = [],
                 gPlayer = player, gPlayerShots = [] }
