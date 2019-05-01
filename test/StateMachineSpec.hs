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
