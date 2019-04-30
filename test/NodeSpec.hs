module NodeSpec where

import Data.List.NonEmpty
import Node
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Node" $ do
  it "parseNodeAddresses should parse a correct single element list " $ do
    parseNodeAddresses "192.168.0.1:8080" `shouldBe` Right (Node "192.168.0.1" 8080 :| [])
  it "parseNodeAddresses should parse a correct multiple element list " $ do
    parseNodeAddresses "192.168.0.1:8080,192.168.0.2:8080" `shouldBe` Right (Node "192.168.0.1" 8080  :| [Node "192.168.0.2" 8080])
