module Main where

import Test.Hspec
import Test.Hspec.QuickCheck

import Hasgel.Simulation

sim :: Simulation Milliseconds
sim = simulation 0

update :: Update Milliseconds
update _ x = x + 1

main :: IO ()
main = hspec $
  describe "simulate" $ do
    let steps dt = min (fromIntegral maxFrameSkip) $ dt `div` simulationStep
    prop "updates state" $
      \dt -> simState (simulate sim dt update) == steps dt
    prop "updates frame count" $
      \dt -> simFrame (simulate sim dt update) == fromIntegral (steps dt)
    prop "updates time" $
      \dt -> timeCurrent (simTime (simulate sim dt update)) ==
             simulationStep * steps dt
    prop "updates accumulated time" $
      \dt -> simAccumulatedTime (simulate sim dt update) ==
             if steps dt == 0 then dt else dt - (simulationStep * steps dt)
