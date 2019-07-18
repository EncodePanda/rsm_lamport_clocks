
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
    it "sets current time greater or equal current value and greater than max of highest incoming" $ do
      forAll externalCommandsAndClockGen (\(clock, cmds) -> execState (sequence ( map tick cmds )) clock `shouldSatisfy` (\result -> result >= clock && result >= maxClock cmds))
    it "the clock is bumped by 1 every time an internal incoming command is encountered" $ do
      forAll internalCommandsAndClockGen (\(clock, cmds) -> execState (sequence ( map tick cmds )) clock `shouldBe` clock + length cmds)

maxClock :: [IncomingCommand] -> Int
maxClock [] = 0
maxClock ((ExternalCommand clock _):cons) = max (maxClock cons) clock
maxClock ((InternalCommand _):cons) = maxClock cons

apply :: [Command] -> State TheState [()]
apply cmds = traverse machine cmds

run :: State TheState a -> Int
run s = execState s 0

instance Arbitrary Command where
  arbitrary = do
    i <- choose(0, 100)
    elements[Add i, Mult i]

externalCommandsAndClockGen :: Gen (Int, [IncomingCommand])
externalCommandsAndClockGen = do
  clock <- choose(0, 100)
  suchThatMap (listOf1 externalCommandGen) (\c -> Just(clock, c))

internalCommandsAndClockGen :: Gen (Int, [IncomingCommand])
internalCommandsAndClockGen = do
  clock <- choose(0, 100)
  suchThatMap (listOf1 internalCommandGen) (\c -> Just(clock, c))

internalCommandGen :: Gen IncomingCommand
internalCommandGen = do
    cmd <- arbitrary
    elements[(InternalCommand cmd)]

externalCommandGen = do
    clock <- arbitrary
    cmd <- arbitrary
    elements[(ExternalCommand clock cmd)]

instance Arbitrary IncomingCommand where
  arbitrary = oneof [internalCommandGen, externalCommandGen]
