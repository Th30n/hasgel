-- | This module corresponds to SDL 2.0 Input Events, Event Handling
-- category in official documentation wiki.
module Hasgel.SDL.Events (
  -- * Data types and enumerations
  SDL.Event(..),
  -- * Functions
  pollEvent,
) where

import Control.Monad.IO.Class (MonadIO (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (peek)

import qualified Graphics.UI.SDL as SDL

type Event = SDL.Event

-- | Polls for currently pending events. Returns 'Nothing' if no 'Event'
-- available. This is the preferred way of receiving events since it can
-- be done inside the main loop and does not suspend the main loop while
-- waiting for an event.
--
-- This function implicitly calls SDL_PumpEvents(), you can only call this
-- function in the thread that set the video mode.
pollEvent :: MonadIO m => m (Maybe Event)
pollEvent = liftIO . alloca $ \e -> do
  r <- SDL.pollEvent e
  if r == 0
    then pure Nothing
    else Just <$> peek e
