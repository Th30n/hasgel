module Hasgel.Game (
  GameState(..), Ticcmd, PlayerCmd(..), Player (..), Invader(..),
  gameState, ticGame, addTiccmd, buildTiccmd
) where

import Control.Arrow (first, second)
import Control.Monad.State
import Data.Foldable (foldl')
import Data.Maybe (fromMaybe, isJust)
import System.Random (StdGen, randomR)

import Control.Lens (both, over, (^.))
import Data.Binary (Binary)
import qualified Data.Binary as B
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
  , gShotRandom :: StdGen
  }

data InvaderDir =
  DirLeft
  | DirRight
    -- | Time in milliseconds when direction was set to down and the
    -- previous direction.
  | DirDown !Milliseconds InvaderDir
  deriving (Eq, Show)

data Invader = Invader
  { iTransform :: Transform
    -- | Time when the invader was shot and started exploding.
  , iExplodeTime :: Maybe Milliseconds
  } deriving (Show)

data Player = Player
  { pTransform :: Transform
    -- | Time when the next shot can be fired. This allows shooting right at
    -- the start, when the time is 0.
  , pShotTime :: !Milliseconds
    -- | Time when the player was shot and started exploding.
  , pExplodeTime :: Maybe Milliseconds
  }

class Explodable a where
  explode :: Time -> a -> a
  hasExploded :: a -> Bool

instance Explodable Player where
  explode time player = player { pExplodeTime = Just $ timeCurrent time }
  hasExploded = isJust . pExplodeTime

instance Explodable Invader where
  explode time inv = inv { iExplodeTime = Just $ timeCurrent time }
  hasExploded = isJust . iExplodeTime

-- | Commands for one tic of gameplay.
data Ticcmd = Ticcmd
  { cmdMove :: !Float -- ^ Horizontal movement speed.
  , cmdShoot :: !Bool -- ^ True if shooting.
  } deriving (Show, Read)

instance Binary Ticcmd where
  put = (>>) <$> B.put . cmdMove <*> B.put . cmdShoot
  get = Ticcmd <$> B.get <*> B.get

-- | Player control commands.
data PlayerCmd =
  MoveLeft
  | MoveRight
  | Shoot
  deriving (Show, Ord, Eq)

addTiccmd :: GameState -> Ticcmd -> GameState
addTiccmd gs ticcmd = gs { gTiccmds = gTiccmds gs ++ [ticcmd] }

buildTiccmd :: Foldable f => f PlayerCmd -> Ticcmd
buildTiccmd = foldl' build' emptyTiccmd
  where playerSpeed = 10
        build' cmd MoveLeft = cmd { cmdMove = cmdMove cmd - playerSpeed }
        build' cmd MoveRight = cmd { cmdMove = cmdMove cmd + playerSpeed }
        build' cmd Shoot = cmd { cmdShoot = True }

ticPlayer :: Time -> GameState -> GameState
ticPlayer time gs
  | null (gTiccmds gs) = gs
  -- Player dead, consume a ticcmd.
  | hasExploded $ gPlayer gs = execState (void popTiccmd) gs
  | otherwise = flip execState gs $ do
      ticcmd <- popTiccmd
      let speed = cmdMove ticcmd * millis2Sec (timeDelta time)
      playerMove speed
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
ticShots _ gs | null $ gShots gs = gs
ticShots time gs =
  let ships = gInvaders gs
      player = gPlayer gs
      shootables = if not $ hasExploded player
                   then pTransform player : map iTransform ships
                   else map iTransform ships
      (shots, shotIxs) = moveShots time shootables $ gShots gs
      player' = if 0 `elem` shotIxs
                then explode time player
                else player
      (ships', exs) = splitShot (map (\i -> i - 1) shotIxs) ships
  in gs { gPlayer = player', gShots = shots, gInvaders = ships',
          gExploded = gExploded gs ++ map (explode time) exs }

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

-- | Splits the invader list by index. Each indexed invader is put into the
-- second list.
splitShot :: [Int] -> [Invader] -> ([Invader], [Invader])
splitShot ixs = over both reverse . foldl' go ([], []) . zip [0..]
  where go (alive, shot) (i, ship) | i `elem` ixs = (alive, ship:shot)
                                   | otherwise = (ship:alive, shot)

invaderFireRate :: Milliseconds
invaderFireRate = 500

ticInvaders :: Time -> GameState -> GameState
ticInvaders time = execState $ do
  modify $ moveInvaders time
  let notForDelete inv = (timeCurrent time - fromMaybe 0 (iExplodeTime inv)) < 2000
  modify $ \gs -> gs { gExploded = filter notForDelete $ gExploded gs }
  when (timeCurrent time `mod` invaderFireRate == 0) $ do
    invaders <- gets gInvaders
    let shooters = filter (canShoot invaders) invaders
    modify $ \gs -> let (shots, rng) = invaderShoot shooters (gShotRandom gs)
                    in gs { gShots = shots ++ gShots gs, gShotRandom = rng }

-- | Return True if the given invader can shoot. This is the case when the
-- invader is alive and there are no invaders in his line of fire.
canShoot :: [Invader] -> Invader -> Bool
canShoot _ inv | hasExploded inv = False
canShoot friends inv =
  let pos = transformPosition . iTransform
      posX i = pos i ^. L._x
      scaleX i = transformScale (iTransform i) ^. L._x
      leftX i = posX i - scaleX i
      rightX i = posX i + scaleX i
      above i = pos i ^. L._y >= pos inv ^. L._y
      notBlocking i = leftX i <= leftX inv && rightX i <= leftX inv ||
                      leftX i >= rightX inv && rightX i >= rightX inv
  in all (\i -> above i || notBlocking i) $ filter (\i -> pos i /= pos inv) friends

-- | Move down for this amount of milliseconds.
moveDownTime :: Milliseconds
moveDownTime = 200

moveInvaders :: Time -> GameState -> GameState
moveInvaders time gs =
  let dir = gInvaderDir gs
      move i = i { iTransform = invaderMoveDir time dir $ iTransform i }
      ships = move <$> gInvaders gs
      movedDown = case dir of
                    (DirDown start _) -> timeCurrent time - start >= moveDownTime
                    _ -> False
      changeDir = movedDown || any (not . inGameBounds . iTransform) ships
  in if not changeDir
     then gs { gInvaders = ships }
     else gs { gInvaderDir = changeInvaderDir time (gInvaderDir gs) }

changeInvaderDir :: Time -> InvaderDir -> InvaderDir
changeInvaderDir _ (DirDown _ DirLeft) = DirRight
changeInvaderDir _ (DirDown _ DirRight) = DirLeft
changeInvaderDir _ (DirDown _ _) = DirRight -- shouldn't happen
changeInvaderDir time dir = DirDown (timeCurrent time) dir

getInvaderSpeed :: Time -> Float
getInvaderSpeed time = 1.5 * millis2Sec (timeDelta time)

invaderMoveDir :: Time -> InvaderDir -> Transform -> Transform
invaderMoveDir time (DirDown _ _) = invaderMoveDown time
invaderMoveDir time DirLeft = invaderHorMove $ -getInvaderSpeed time
invaderMoveDir time DirRight = invaderHorMove $ getInvaderSpeed time

-- | Move invaders vertically down.
invaderMoveDown :: Time -> Transform -> Transform
invaderMoveDown time ship = let dy = -5 * millis2Sec (timeDelta time)
                            in ship `translate` L.V3 0 dy 0

-- | Moves the invader ship for the given horizontal offset.
invaderHorMove :: Float -> Transform -> Transform
invaderHorMove dx ship = translate ship $ L.V3 dx 0 0

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
createInvaders = do
  x <- [-8, -4..8]
  y <- [12, 15, 18]
  pure $ invader x y

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
                 gInvaderDir = DirRight,
                 gShotRandom = shotGen }
