{-# LANGUAGE MultiParamTypeClasses #-}

module Hasgel.FrameTimer (
  FrameTimer, HasFrameTimer(..), createFrameTimer, withFrameTimer, timerStart,
  getGPUTime, getCPUTime, resetFrameTimer
) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (MonadState, gets, modify)
import Data.Word (Word32)

import Hasgel.GL (Query, getQueryResult, queryCounter)

type Milliseconds = Word32

data FrameTimer = FrameTimer
  { timerQueries :: ((Query, Query), (Query, Query))
  , timerFrames :: !Int
  , timerAccum :: !Double
  , timerStart :: !Milliseconds
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
  ft@(FrameTimer qs frames _ _) <- gets getFrameTimer
  let q = if odd frames then fst qs else snd qs
  queryCounter $ fst q
  r <- action
  queryCounter $ snd q
  ft' <- updateFrameTimer ft
  modify $ flip setFrameTimer ft'
  pure r

createFrameTimer :: ((Query, Query), (Query, Query)) -> Milliseconds -> FrameTimer
createFrameTimer qs = FrameTimer qs 0 0

updateFrameTimer :: MonadIO m => FrameTimer -> m FrameTimer
updateFrameTimer ft@(FrameTimer _ 0 _ _) = pure ft { timerFrames = 1 }
updateFrameTimer (FrameTimer qs frames acc time) = do
  let q = if even frames then fst qs else snd qs
  startTime <- getQueryResult $ fst q
  endTime <- getQueryResult $ snd q
  let ms = (*1E-6) . fromIntegral $ endTime - startTime
  pure $ FrameTimer qs (frames + 1) (acc + ms) time

resetFrameTimer :: FrameTimer -> Milliseconds -> FrameTimer
resetFrameTimer (FrameTimer qs@(q1, q2) frames _ _) start =
  let qs' = if odd frames then qs else (q2, q1)
  in FrameTimer qs' 1 0 start

getGPUTime :: FrameTimer -> Double
getGPUTime (FrameTimer _ frames acc _) = acc / fromIntegral frames

getCPUTime :: FrameTimer -> Milliseconds -> Double
getCPUTime ft end = dt / frames
  where frames = fromIntegral $ timerFrames ft
        dt = fromIntegral $ end - timerStart ft
