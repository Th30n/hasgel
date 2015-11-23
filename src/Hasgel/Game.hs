module Hasgel.Game (
  GameState(..), Ticcmd, PlayerCmd(..), Player (..),
  gameState, ticGame, addTiccmd, buildTiccmd
) where

import Control.Arrow (second)
import Control.Lens ((^.))
import Data.List (findIndex)

import Hasgel.Simulation (Milliseconds, Time (..), millis2Sec)
import Hasgel.Transform (Transform (..), translate)

import qualified Linear as L

-- | State of the game.
data GameState = GameState
  { gTiccmds :: [Ticcmd] -- ^ Commands to be processed.
  , gOldTiccmds :: [Ticcmd] -- ^ Processed commands, used for recording.
  , gPlayer :: Player
  , gPlayerShots :: [Transform]
  , gInvaders :: [Transform]
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
                    shots = gPlayerShots gs
                    ships = gInvaders gs
                    (shots', ships') = moveShots shotSpeed shots ships
                in gs { gPlayerShots = shots', gInvaders = ships' }

moveShots :: Float -> [Transform] -> [Transform] -> ([Transform], [Transform])
moveShots _ [] ships = ([], ships)
moveShots dy shots [] = (shotMove dy <$> shots, [])
moveShots dy (shot:shots) ships
  | Just ships' <- shotCollides shot dy ships = moveShots dy shots ships'
  | otherwise = let (shots', ships') = moveShots dy shots ships
                in (shotMove dy shot:shots', ships')

-- | On collision, remove a transform from list.
shotCollides :: Transform -> Float -> [Transform] -> Maybe [Transform]
shotCollides shot dy ships = do
  i <- snd $ tryMove shot (L.V3 0 dy 0) ships
  Just $ uncurry (++) $ second tail $ splitAt i ships

-- | Returns the new position and the index of the colliding object when moving
-- for the given velocity.
-- TODO: Check if speed is greater than some maximum and step accordingly.
tryMove :: Transform -> L.V3 Float -> [Transform] -> (Transform, Maybe Int)
tryMove mobj speed blockers
  | Just i <- checkPosition (translate mobj speed) blockers = (mobj, Just i)
  | otherwise = (translate mobj speed, Nothing)

-- | Returns the index of the colliding object.
checkPosition :: Transform -> [Transform] -> Maybe Int
checkPosition mobj = findIndex (bvOverlaps mobj)

-- | Returns True if the axis aligned bounding volumes of given transforms
-- overlap.
bvOverlaps :: Transform -> Transform -> Bool
bvOverlaps a b
  | aMinY > bMaxY || aMaxY < bMinY = False
  | aMinX > bMaxX || aMaxX < bMinX = False
  | aMinZ > bMaxZ || aMaxZ < bMinZ = False
  | otherwise = True
  where bounds t c = let scale = transformScale t ^. c
                         pos = transformPosition t ^. c
                     in (pos - scale, pos + scale)
        [(aMinX, aMaxX), (aMinY, aMaxY), (aMinZ, aMaxZ)] =
          bounds a <$> [L._x, L._y, L._z]
        [(bMinX, bMaxX), (bMinY, bMaxY), (bMinZ, bMaxZ)] =
          bounds b <$> [L._x, L._y, L._z]

-- | Maximum and minimum X coordinate of the game area.
gameBoundsX :: Float
gameBoundsX = 10

playerMove :: Transform -> Float -> Transform
playerMove prev dx
  | dx > 0 = translate prev $ L.V3 (min dx rx) 0 0
  | dx < 0 = translate prev $ L.V3 (max dx lx) 0 0
  | otherwise = prev
  where rx = gameBoundsX - transformPosition prev ^. L._x
        lx = (-gameBoundsX) - transformPosition prev ^. L._x

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

createInvaders :: [Transform]
createInvaders = [invader (-4), invader 0, invader 4]

invader :: Float -> Transform
invader x = Transform L.zero (L.V3 1 1 1) (L.V3 x 15 0)

gameState :: [Ticcmd] -> GameState
gameState ticcmds =
  let model = Transform L.zero (L.V3 1 1 1) L.zero
      player = Player { playerTransform = model, playerShotTime = 0 }
  in GameState { gTiccmds = ticcmds, gOldTiccmds = [],
                 gPlayer = player, gPlayerShots = [],
                 gInvaders = createInvaders }
