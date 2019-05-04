module StateMachineSpec where

import StateMachine
import Control.Monad.Trans.State
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

apply :: [Command] -> State TheState [()]
apply cmds = traverse machine cmds

run :: State TheState a -> Int
run s = execState s 0

instance Arbitrary Command where
  arbitrary = do
    i <- choose(0, 100)
    elements[Add i, Mult i]
