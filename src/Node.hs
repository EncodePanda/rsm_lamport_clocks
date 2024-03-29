module Node where

import Data.List.Split
import Data.List.NonEmpty
import Data.String.Utils

data NodeError = NodeAddressNotParsable String deriving (Show, Eq)

type Host = String
type Port = Int
data Node = Node Host Port
  deriving (Show, Eq)

parseNodeAddresses :: String -> Either NodeError (NonEmpty Node)
parseNodeAddresses =  fmap fromList . traverse (toNode . (splitOn ":")) . (splitOn ",")
  where
    toNode (host:port:[]) = Right $ Node (strip host) (read $ strip port)
    toNode line = Left $ NodeAddressNotParsable $ show line
