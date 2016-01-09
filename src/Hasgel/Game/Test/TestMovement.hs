module Main where

import Test.Hspec

import Hasgel.Game.Movement
import Hasgel.Transform

import qualified Linear as L

largeMobj :: Transform
largeMobj = defaultTransform { transformScale = L.V3 1 maxSpeed 1 }

smallMobj :: Transform
smallMobj = defaultTransform { transformScale = L.V3 1 (maxSpeed / 10) 1 }

fastVelocity :: L.V3 Float
fastVelocity = L.V3 0 (10 * maxSpeed) 0

slowVelocity :: L.V3 Float
slowVelocity = L.V3 (maxSpeed / 2) 0 0

blockers :: [Transform]
blockers = [defaultTransform { transformPosition = L.V3 2.1 0 0 },
            defaultTransform { transformPosition = L.V3 0 (2.5 * maxSpeed) 0,
                               transformScale = L.V3 0.5 (maxSpeed / 4) 0.5 }]

main :: IO ()
main = hspec $
  describe "tryMove" $ do
    context "when moving objects larger than maximum move speed" $ do
      it "moves correctly fast objects" $
        tryMove [] largeMobj fastVelocity `shouldBe`
          (largeMobj { transformPosition = fastVelocity }, Nothing)
      it "detects collision of fast moving objects" $
        tryMove blockers largeMobj fastVelocity `shouldBe`
          (largeMobj { transformPosition = L.V3 0 maxSpeed 0 }, Just 1)
      it "detects collision of slow moving objects" $
        tryMove blockers largeMobj slowVelocity `shouldBe`
          (largeMobj, Just 0)
    context "when moving objects smaller than maximum move speed" $ do
      it "moves correctly fast objects" $
        tryMove [] smallMobj fastVelocity `shouldBe`
          (smallMobj { transformPosition = fastVelocity }, Nothing)
      it "fails to detect collision of fast moving objects" $
        tryMove blockers smallMobj fastVelocity `shouldBe`
          (smallMobj { transformPosition = fastVelocity }, Nothing)
      it "detects collision of slow moving objects" $
        tryMove blockers smallMobj slowVelocity `shouldBe`
          (smallMobj, Just 0)
