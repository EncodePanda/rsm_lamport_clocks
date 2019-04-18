module Node where

import Data.List.Split

data NodeError = NodeAddressNotParsable String

type Host = String
type Port = Int
data Node = Node Host Port
  deriving Show

parseNodeAddresses :: String -> [Node]
parseNodeAddresses =  fmap (toNode . (splitOn ":")) . (splitOn ",")
  where
    toNode (host:port:[]) = Node host (read port)

