module Hasgel.Game (
  GameState(..), Ticcmd, PlayerCmd(..), Player (..), Invader(..),
  gameState, ticGame, addTiccmd, buildTiccmd
) where

import Control.Arrow (first, second)
import Control.Monad.State
import Data.Foldable (foldl')
import Data.Maybe (fromMaybe)
import System.Random (StdGen, randomR)

import Control.Lens ((^.))
import qualified Linear as L

import Hasgel.Game.Movement (tryMove)
import Hasgel.Simulation (Milliseconds, Time (..), millis2Sec)
import Hasgel.Transform (Transform (..), defaultTransform, rotateLocal,
                         transformForward, translate)

-- | State of the game.
data GameState = GameState
  { gTiccmds :: [Ticcmd] -- ^ Commands to be processed.
  , gOldTiccmds :: [Ticcmd] -- ^ Processed commands, used for recording.
  , gPlayer :: Player
  , gShots :: [Transform]
  , gInvaders :: [Invader]
  , gExploded :: [Invader]
  , gInvaderDir :: InvaderDir
  , gOldInvaderDir :: InvaderDir
  , gStartMoveDown :: !Milliseconds
  , gShotRandom :: StdGen
  }

data InvaderDir = DirLeft | DirRight | DirDown deriving (Eq, Show)

data Invader = Invader
  { iTransform :: Transform
    -- | Time when the invader was shot and started exploding.
  , iExplodeTime :: Maybe Milliseconds
  }

data Player = Player
  { pTransform :: Transform
    -- | Time when the next shot can be fired. This allows shooting right at
    -- the start, when the time is 0.
  , pShotTime :: !Milliseconds
    -- | Time when the player was shot and started exploding.
  , pExplodeTime :: Maybe Milliseconds
  }

-- | Commands for one tic of gameplay.
data Ticcmd = Ticcmd
  { cmdMove :: !Float -- ^ Horizontal movement speed.
  , cmdShoot :: !Bool -- ^ True if shooting.
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
  -- Player dead, consume a ticcmd.
  | Just _ <- pExplodeTime $ gPlayer gs = gs { gTiccmds = tail $ gTiccmds gs }
  | otherwise = flip execState gs $ do
      ticcmd <- popTiccmd
      playerMove $ cmdMove ticcmd
      when (cmdShoot ticcmd) $
        playerShoot time

popTiccmd :: State GameState Ticcmd
popTiccmd = do
  ticcmd:nextTiccmds <- gets gTiccmds
  modify $ \gs -> gs { gTiccmds = nextTiccmds,
                       gOldTiccmds = ticcmd : gOldTiccmds gs }
  pure ticcmd

playerMove :: Float -> State GameState ()
playerMove dv = do
  trans <- playerMove' dv . pTransform <$> gets gPlayer
  modify $ \gs -> gs { gPlayer = (gPlayer gs) { pTransform = trans } }

playerShoot :: Time -> State GameState ()
playerShoot time = do
  (player, shots) <- gets $ playerShoot' time <$> gPlayer <*> gShots
  modify $ \gs -> gs { gPlayer = player, gShots = shots }

ticGame :: Time -> GameState -> GameState
ticGame time = ticInvaders time . ticShots time . ticPlayer time

ticShots :: Time -> GameState -> GameState
ticShots time gs
  | null (gShots gs) = gs
  | otherwise =
      let ships = gInvaders gs
          player = gPlayer gs
          shootables = pTransform player : map iTransform ships
          (shots, shotIxs) = moveShots time shootables $ gShots gs
          player' = if 0 `elem` shotIxs
                    then player { pExplodeTime = Just $ timeCurrent time}
                    else player
          (ships', exs') = splitShot (map (\i -> i - 1) shotIxs) ships
          exs'' = map (\e -> e { iExplodeTime = Just $ timeCurrent time }) exs'
      in gs { gPlayer = player', gShots = shots,
              gInvaders = reverse ships',
              gExploded = gExploded gs ++ exs'' }

type Shootable = Transform
type Shot = Transform

-- | Move given shots and return a pair of shots that didn't hit and
-- indexes of shootable objects that were hit.
moveShots :: Time -> [Shootable] -> [Shot] -> ([Shot], [Int])
moveShots time shootables = foldl' go ([], [])
  where speed = 15 * millis2Sec (timeDelta time)
        move = tryMove shootables
        go acc shot =
          case move shot (speed * transformForward shot) of
            -- Nothing was hit.
            (shot', Nothing) -> first (shot':) acc
            -- Hit at index 'i'.
            (_, Just i) -> second (i:) acc

splitShot :: [Int] -> [Invader] -> ([Invader], [Invader])
splitShot ixs = foldl' go ([], []) . zip [0..]
  where go (alive, shot) (i, ship) | i `elem` ixs = (alive, ship:shot)
                                   | otherwise = (ship:alive, shot)

invaderFireRate :: Milliseconds
invaderFireRate = 500

ticInvaders :: Time -> GameState -> GameState
ticInvaders time = execState $ do
  modify $ moveInvaders time
  let notForDelete inv = (timeCurrent time - fromMaybe 0 (iExplodeTime inv)) < 2000
  modify $ \gs -> gs { gExploded = filter notForDelete $ gExploded gs }
  when (timeCurrent time `mod` invaderFireRate == 0) $
    modify $ \gs -> let (shots, rng) = invaderShoot (gInvaders gs) (gShotRandom gs)
                    in gs { gShots = shots ++ gShots gs,
                            gShotRandom = rng }

type ChangeDir = Bool

-- | Move down for this amount of milliseconds.
moveDownTime :: Milliseconds
moveDownTime = 200

moveInvaders :: Time -> GameState -> GameState
moveInvaders time gs =
  let shipSpeed = 2.5 * millis2Sec (timeDelta time)
      transforms = map iTransform $ gInvaders gs
      (changeDir, ships) = case gInvaderDir gs of
                              DirLeft -> invaderHorMove (-shipSpeed) transforms
                              DirRight -> invaderHorMove shipSpeed transforms
                              DirDown ->
                                invaderMoveDown time (gStartMoveDown gs) transforms
  in if not changeDir
     then gs { gInvaders = map (flip Invader Nothing) ships }
     else case gInvaderDir gs of
            DirDown -> let newDir = if gOldInvaderDir gs == DirLeft
                                    then DirRight
                                    else DirLeft
                       in gs { gInvaderDir = newDir }
            dir -> gs { gInvaderDir = DirDown,
                        gOldInvaderDir = dir,
                        gStartMoveDown = timeCurrent time }

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

-- | Spawn shots for invader based on random generator.
invaderShoot :: [Invader] -> StdGen -> ([Transform], StdGen)
invaderShoot invaders = runState (foldM go [] invaders)
  where go shots inv = do
          chance <- state $ randomR (0, 100)
          pure $ if chance < (90 :: Int)
                 then shots
                 else mkShot (iTransform inv) : shots

-- | Maximum and minimum X coordinate of the game area.
gameBoundsX :: Float
gameBoundsX = 10

inGameBounds :: Transform -> Bool
inGameBounds transform =
  let tx = transformPosition transform ^. L._x
  in tx >= (-gameBoundsX) && tx <= gameBoundsX

playerMove' :: Float -> Transform -> Transform
playerMove' dx prev
  | inGameBounds new = new
  | otherwise = prev
  where new = translate prev $ L.V3 dx 0 0

shootCooldown :: Milliseconds
shootCooldown = 500

playerShoot' :: Time -> Player -> [Shot]  -> (Player, [Shot])
playerShoot' time player shots
  | timeCurrent time < pShotTime player = (player, shots)
  | otherwise = let trans = pTransform player
                in (player { pShotTime = shootCooldown + timeCurrent time },
                    mkShot trans : shots)

emptyTiccmd :: Ticcmd
emptyTiccmd = Ticcmd { cmdMove = 0, cmdShoot = False }

createInvaders :: [Invader]
createInvaders = let fstRow x = invader x 15
                     sndRow x = invader x 18
                     xs = [-4, 0, 4]
                 in map fstRow xs ++ map sndRow xs

-- | Create a shot transform in front of the given source transform.
mkShot :: Transform -> Transform
mkShot source =
  let transform = source { transformScale = L.V3 0.3 0.3 0.3 }
      pos = 2 * transformForward source
  in translate transform pos

invader :: Float -> Float -> Invader
invader x y =
  let transform = defaultTransform { transformPosition = L.V3 x y 0 }
  in Invader { iTransform = rotateLocal transform $ L.V3 90 0 0,
               iExplodeTime = Nothing }

gameState :: [Ticcmd] -> StdGen -> GameState
gameState ticcmds shotGen =
  let player = Player { pTransform =
                          rotateLocal defaultTransform (L.V3 90 180 0),
                        pShotTime = 0, pExplodeTime = Nothing }
  in GameState { gTiccmds = ticcmds, gOldTiccmds = [],
                 gPlayer = player, gShots = [],
                 gInvaders = createInvaders, gExploded = [],
                 gInvaderDir = DirRight, gOldInvaderDir = DirLeft,
                 gStartMoveDown = 0, gShotRandom = shotGen }
