{-# LANGUAGE BangPatterns #-}

module Hasgel.Simulation (
  Time(..), Milliseconds, Update, Simulation(..),
  maxFrameSkip, frameRate, simulationStep,
  millis2Sec, simulation, simulate
  ) where

import Data.Word (Word32)

type Milliseconds = Word32

data Time = Time
  { timeCurrent :: Milliseconds
  , timeDelta :: Milliseconds
  }

type Update a = Time -> a -> a

-- | Stores time, frame details and the data of the simulation.
data Simulation a = Simulation
   { simTime :: !Time -- ^ Total elapsed time since the start.
   , simAccumulatedTime :: !Milliseconds -- ^ Accumulated unsimulated time.
   , simFrame :: !Int -- ^ Total number of simulated frames.
   , simState :: a -- ^ The data that is being simulated.
   }

instance Functor Simulation where
  fmap f sim = sim { simState = f $ simState sim }

instance Foldable Simulation where
  foldMap f sim = simState $ f <$> sim

instance Traversable Simulation where
  sequenceA sim = let f = simState sim
                      sim' s = const s <$> sim
                  in sim' <$> f

-- | Convert milliseconds to seconds.
millis2Sec :: Fractional a => Milliseconds -> a
millis2Sec = (0.001 *) . fromIntegral

-- | Frames per second that are simulated.
frameRate :: Int
frameRate = 50

-- | Maximum number of frames that will be simulated between rendering.
maxFrameSkip :: Int
maxFrameSkip = 10

-- | Time step which is simulated.
simulationStep :: Milliseconds
simulationStep = floor $ (1000 :: Double) / fromIntegral frameRate

simulation :: a -> Simulation a
simulation a = Simulation { simTime = Time 0 simulationStep,
                            simAccumulatedTime = 0,
                            simFrame = 0, simState = a }

-- | Run the simulation update for given time step until the simulation time
-- is exhausted or 'maxFrameSkip' reached. Update simulates 'simulationStep'
-- amount of time, if given time step is less than that there may be no update.
simulate :: Simulation a -> Milliseconds -> Update a -> Simulation a
simulate sim dt update =
  let acc = dt + simAccumulatedTime sim
      sim' = sim { simAccumulatedTime = acc }
  in simulateLoop sim' acc 0 update

simulateLoop :: Simulation a -> Milliseconds -> Int  -> Update a -> Simulation a
simulateLoop sim !acc !i update
  | acc < simulationStep || i >= maxFrameSkip = sim
  | otherwise = let sim' = update (simTime sim) <$> sim
                    sim'' = updateSimTime simulationStep sim'
                    acc' = acc - simulationStep
                in simulateLoop sim'' acc' (i + 1) update

-- | Update the simulation time and frame count.
updateSimTime :: Milliseconds -> Simulation a -> Simulation a
updateSimTime !dt sim =
  let time = simTime sim
      currentTime = dt + timeCurrent time
      acc = simAccumulatedTime sim
      frames = simFrame sim
  in sim { simTime = time { timeCurrent = currentTime },
           simAccumulatedTime = acc - dt,
           simFrame = frames + 1 }
