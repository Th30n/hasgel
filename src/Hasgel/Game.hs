module Hasgel.Game (
  GameState(..), Ticcmd, PlayerCmd(..), Player (..),
  gameState, ticGame, addTiccmd, buildTiccmd
) where

import Control.Arrow (first, second)

import Control.Lens ((^.))
import qualified Linear as L

import Hasgel.Game.Movement (tryMove)
import Hasgel.Simulation (Milliseconds, Time (..), millis2Sec)
import Hasgel.Transform (Transform (..), translate)

-- | State of the game.
data GameState = GameState
  { gTiccmds :: [Ticcmd] -- ^ Commands to be processed.
  , gOldTiccmds :: [Ticcmd] -- ^ Processed commands, used for recording.
  , gPlayer :: Player
  , gPlayerShots :: [Transform]
  , gInvaders :: [Transform]
  , gInvaderDir :: InvaderDir
  }

data InvaderDir = DirLeft | DirRight | DirDown

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
ticGame time = ticInvaders time . ticShots time . ticPlayer time

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
moveShots dy (shot:shots) ships = case tryMove shot (L.V3 0 dy 0) ships of
  (shot', Nothing) -> let (shots', ships') = moveShots dy shots ships
                      in (shot':shots', ships')
  (_, Just i) -> moveShots dy shots removeShip
    where removeShip = uncurry (++) $ second tail $ splitAt i ships

ticInvaders :: Time -> GameState -> GameState
ticInvaders time gs =
  let shipSpeed = 5 * millis2Sec (timeDelta time)
      ships = gInvaders gs
      (dir', ships') = case gInvaderDir gs of
        DirLeft -> first (\c -> if c then DirRight else DirLeft) $
                   invaderHorMove (-shipSpeed) ships
        DirRight -> first (\c -> if c then DirLeft else DirRight) $
                    invaderHorMove shipSpeed ships
        DirDown -> (DirLeft, ships) -- TODO
  in gs { gInvaders = ships', gInvaderDir = dir' }

invaderHorMove :: Float -> [Transform] -> (Bool, [Transform])
invaderHorMove _ [] = (False, [])
invaderHorMove dx (ship:ships) =
  let (c, ship') = invaderMove dx ship
      (c', ships') = invaderHorMove dx ships
  in (c || c', ship':ships')

invaderMove :: Float -> Transform -> (Bool, Transform)
invaderMove dx ship = if outside then (True, ship) else (False, ship')
  where ship' = translate ship $ L.V3 dx 0 0
        tx = transformPosition ship' ^. L._x
        outside = tx < (-gameBoundsX) || tx > gameBoundsX

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
createInvaders = let fstRow x = invader x 15
                     sndRow x = invader x 18
                     xs = [-4, 0, 4]
                 in map fstRow xs ++ map sndRow xs

invader :: Float -> Float -> Transform
invader x y = Transform L.zero (L.V3 1 1 1) (L.V3 x y 0)

gameState :: [Ticcmd] -> GameState
gameState ticcmds =
  let model = Transform L.zero (L.V3 1 1 1) L.zero
      player = Player { playerTransform = model, playerShotTime = 0 }
  in GameState { gTiccmds = ticcmds, gOldTiccmds = [],
                 gPlayer = player, gPlayerShots = [],
                 gInvaders = createInvaders, gInvaderDir = DirRight }
