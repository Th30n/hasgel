{-# LANGUAGE MultiParamTypeClasses #-}

module Hasgel.FrameTimer (
  FrameTimer, HasFrameTimer(..), createFrameTimer, withFrameTimer, timerStart,
  getGPUTime, getCPUTime, resetFrameTimer
) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (MonadState, get, modify)
import Data.Word (Word32)
import Graphics.GL.Core45

import Hasgel.GL (Query, getQueryResult, withQuery)

type Milliseconds = Word32

data FrameTimer = FrameTimer
  { timerQueries :: (Query, Query)
  , timerFrames :: Int
  , timerAccum :: Double
  , timerStart :: Milliseconds
  } deriving (Show)

class HasFrameTimer a where
  getFrameTimer :: a -> FrameTimer
  setFrameTimer :: a -> FrameTimer -> a

withFrameTimer :: (HasFrameTimer s, MonadIO m, MonadState s m) => m a -> m a
withFrameTimer action = do
  ft@(FrameTimer qs frames _ _) <- getFrameTimer <$> get
  let q = if odd frames then fst qs else snd qs
  r <- withQuery GL_TIME_ELAPSED q action
  ft' <- updateFrameTimer ft
  modify $ flip setFrameTimer ft'
  pure r

createFrameTimer :: (Query, Query) -> Milliseconds -> FrameTimer
createFrameTimer qs = FrameTimer qs 0 0

updateFrameTimer :: MonadIO m => FrameTimer -> m FrameTimer
updateFrameTimer ft@(FrameTimer _ 0 _ _) = pure ft { timerFrames = 1 }
updateFrameTimer (FrameTimer qs frames acc time) = do
  let q = if even frames then fst qs else snd qs
  ms <- (*1E-6) . fromIntegral <$> getQueryResult q
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
