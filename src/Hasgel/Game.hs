module Hasgel.Game (
  GameState(..), Ticcmd, PlayerCmd(..), Player (..),
  gameState, ticGame, addTiccmd, buildTiccmd
) where

import Control.Arrow (second)

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
  , gOldInvaderDir :: InvaderDir
  , gStartMoveDown :: Milliseconds
  }

data InvaderDir = DirLeft | DirRight | DirDown deriving (Eq, Show)

data Player = Player
  { playerTransform :: Transform
    -- | Time when the next shot can be fired. This allows shooting right at
    -- the start, when the time is 0.
  , playerShotTime :: Milliseconds
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
moveShots dy (shot:shots) ships =
  case tryMove shot (L.V3 0 dy 0) ships of
    (shot', Nothing) -> let (shots', ships') = moveShots dy shots ships
                        in (shot':shots', ships')
    (_, Just i) -> let ships' = uncurry (++) $ second tail $ splitAt i ships
                   in moveShots dy shots ships'

ticInvaders :: Time -> GameState -> GameState
ticInvaders time gs =
  let shipSpeed = 5 * millis2Sec (timeDelta time)
      ships = gInvaders gs
      (changeDir, ships') = case gInvaderDir gs of
                              DirLeft -> invaderHorMove (-shipSpeed) ships
                              DirRight -> invaderHorMove shipSpeed ships
                              DirDown ->
                                invaderMoveDown time (gStartMoveDown gs) ships
  in if not changeDir
     then gs { gInvaders = ships' }
     else case gInvaderDir gs of
            DirDown -> let newDir = if gOldInvaderDir gs == DirLeft
                                    then DirRight
                                    else DirLeft
                       in gs { gInvaderDir = newDir }
            dir -> gs { gInvaderDir = DirDown,
                        gOldInvaderDir = dir,
                        gStartMoveDown = timeCurrent time }

type ChangeDir = Bool

-- | Move down for this amount of milliseconds.
moveDownTime :: Milliseconds
moveDownTime = 200

-- | Move invaders vertically down. Sets the 'ChangeDir' to 'True' when the
-- invaders have been moving down for 'moveDownTime'.
invaderMoveDown :: Time -> Milliseconds -> [Transform] -> (ChangeDir, [Transform])
invaderMoveDown time startTime ships
  | timeCurrent time - startTime >= moveDownTime = (True, ships)
  | otherwise = let dy = -5 * millis2Sec (timeDelta time)
                in (False, fmap (`translate` L.V3 0 dy 0) ships)

-- | Moves all given invaders for the given horizontal offset and reports if the
-- direction of movement needs to change.
invaderHorMove :: Float -> [Transform] -> (ChangeDir, [Transform])
invaderHorMove _ [] = (False, [])
invaderHorMove dx (ship:ships) =
  let (c, ship') = invaderMove dx ship
      (c', ships') = invaderHorMove dx ships
  in (c || c', ship':ships')

-- | Moves the invader ship for the given horizontal offset. Returns a pair with
-- information whether to change the movement direction or not. The change
-- needs to happen when the invader hits the game field boundary.
invaderMove :: Float -> Transform -> (ChangeDir, Transform)
invaderMove dx ship = if outside then (True, ship) else (False, ship')
  where ship' = translate ship $ L.V3 dx 0 0
        outside = not $ inGameBounds ship'

-- | Maximum and minimum X coordinate of the game area.
gameBoundsX :: Float
gameBoundsX = 10

inGameBounds :: Transform -> Bool
inGameBounds transform =
  let tx = transformPosition transform ^. L._x
  in tx >= (-gameBoundsX) && tx <= gameBoundsX

playerMove :: Transform -> Float -> Transform
playerMove prev dx
  | inGameBounds new = new
  | otherwise = prev
  where new = translate prev $ L.V3 dx 0 0

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
                 gInvaders = createInvaders,
                 gInvaderDir = DirRight, gOldInvaderDir = DirLeft,
                 gStartMoveDown = 0 }
