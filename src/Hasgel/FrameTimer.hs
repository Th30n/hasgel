{-# LANGUAGE MultiParamTypeClasses #-}

module Hasgel.FrameTimer (
  FrameTimer, HasFrameTimer(..), createFrameTimer, withFrameTimer,
  getGPUAvg, getGPUMax, getCPUAvg, getCPUMax, getTimerStart,
  resetFrameTimer, now
) where

import Control.Arrow ((&&&))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (MonadState, gets, modify)

import qualified SDL

import Hasgel.GL (Query, getQueryResult, queryCounter)

type Frame = Int

data FrameTimer = FrameTimer
  { timerQueries :: ((Query, Query), (Query, Query))
  , timerFrames :: !Frame
  , timerAccum :: !Double
  , timerGPUMax :: !Double -- ^ Maximum measured GPU time in ms.
  , timerCPU :: CPUTimer
  } deriving (Show)

data CPUTimer = CPUTimer
  { cpuMax :: !Double -- ^ Maximum measured CPU time in ms.
  , cpuAvg :: !Double -- ^ Average CPU time in ms since last reset.
  , cpuLast :: !Double -- ^ Last CPU time in ms.
  , cpuStart :: !Double -- ^ Starting CPU time in ms.
  } deriving (Show)

class HasFrameTimer a where
  getFrameTimer :: a -> FrameTimer
  setFrameTimer :: a -> FrameTimer -> a

-- | Take an IO (rendering action) and measure the time it took for the GPU
-- to process the commands. Two timestamps are made: one at the start of the
-- pipeline the other at the end. Take care to flush both queries as the last
-- query might get stuck waiting for the next frame thus producing invalid
-- timings.
withFrameTimer :: (HasFrameTimer s, MonadIO m, MonadState s m) => m a -> m a
withFrameTimer action = do
  ft <- gets getFrameTimer
  let (qs, frames) = timerQueries &&& timerFrames $ ft
  let q = if odd frames then fst qs else snd qs
  queryCounter $ fst q
  r <- action
  queryCounter $ snd q
  ft' <- updateFrameTimer ft
  modify $ flip setFrameTimer ft'
  pure r

createFrameTimer :: MonadIO m => ((Query, Query), (Query, Query)) -> m FrameTimer
createFrameTimer qs = do
  nowCPU <- now
  pure $ FrameTimer qs 0 0 0 (CPUTimer 0 0 nowCPU nowCPU)

updateFrameTimer :: MonadIO m => FrameTimer -> m FrameTimer
updateFrameTimer ft@(FrameTimer _ 0 _ _ _) = pure ft { timerFrames = 1 }
updateFrameTimer (FrameTimer qs frames acc maxGPU cpuTimer) = do
  let q = if even frames then fst qs else snd qs
  startTime <- getQueryResult $ fst q
  endTime <- getQueryResult $ snd q
  cpuTimer' <- updateCPUTimer frames cpuTimer
  let ms = (*1E-6) . fromIntegral $ endTime - startTime
      maxGPU' = if frames == 1 then ms else max ms maxGPU
  pure $ FrameTimer qs (frames + 1) (acc + ms) maxGPU' cpuTimer'

updateCPUTimer :: MonadIO m => Frame -> CPUTimer -> m CPUTimer
updateCPUTimer frame cpuTimer = do
  nowCPU <- now
  let msCPU = nowCPU - cpuLast cpuTimer
      maxCPU' = if frame == 1 then msCPU else max msCPU (cpuMax cpuTimer)
  pure $ cpuTimer { cpuMax = maxCPU', cpuLast = nowCPU }

resetFrameTimer :: FrameTimer -> FrameTimer
resetFrameTimer (FrameTimer qs@(q1, q2) frames _ maxGPU cpuTimer) =
  let qs' = if odd frames then qs else (q2, q1)
  in FrameTimer qs' 1 0 maxGPU $ resetCPUTimer frames cpuTimer

resetCPUTimer :: Frame -> CPUTimer -> CPUTimer
resetCPUTimer frames cpuTimer =
  cpuTimer { cpuAvg = dt / fromIntegral frames, cpuStart = cpuLast cpuTimer }
  where dt = (-) <$> cpuLast <*> cpuStart $ cpuTimer

getGPUAvg :: FrameTimer -> Double
getGPUAvg (FrameTimer _ frames acc _ _) = acc / fromIntegral frames

getGPUMax :: FrameTimer -> Double
getGPUMax = timerGPUMax

-- | Return current time (since some arbitrary point) in milliseconds.
now :: MonadIO m => m Double
now = (*1E3) <$> SDL.time -- SDL.time returns time in seconds.

-- | Return average CPU time in milliseconds per frame.
getCPUAvg :: FrameTimer -> Double
getCPUAvg = cpuAvg . timerCPU
-- getCPUAvg ft = dt / frames
--   where frames = fromIntegral $ timerFrames ft
--         dt = (-) <$> cpuLast <*> cpuStart $ timerCPU ft

getCPUMax :: FrameTimer -> Double
getCPUMax = cpuMax . timerCPU

getTimerStart :: FrameTimer -> Double
getTimerStart = cpuStart . timerCPU
