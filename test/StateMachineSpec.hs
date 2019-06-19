module StateMachineSpec where

import StateMachine
import Control.Monad.State
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "machine" $ do
  it "applies commands correctly" $ do
    (execState (machine (Add 5) >> machine (Mult 2)) 0) `shouldBe` 10
  it "applying the same series of commands yields the same result" $ do
    property (\events -> (run $ apply events) `shouldBe` (run $ apply events))
  describe "serialization" $ do
    it "does roundtrip correctly" $ do
      property (\event -> (deserialize . serialize $ event) `shouldBe` event)

  describe "tick" $ do
    it "sets current time greater than incoming external command's Tm" $ do
      let currentTime = 3
      (execState (tick (ExternalCommand 5 (Add 2))) currentTime) `shouldBe` 6
    it "sets current time greater or equal current value if incoming external command's Tm is smaller" $ do
      let currentTime = 7
      (execState (tick (ExternalCommand 5 (Add 2))) 7) `shouldBe` 7
    it "bumps the clock by 1 if incoming command is internal" $ do
      let currentTime = 2
      (execState (tick (InternalCommand (Add 2))) 7) `shouldBe` 8


apply :: [Command] -> State TheState [()]
apply cmds = traverse machine cmds

run :: State TheState a -> Int
run s = execState s 0

instance Arbitrary Command where
  arbitrary = do
    i <- choose(0, 100)
    elements[Add i, Mult i]
