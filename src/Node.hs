module Node where

import Data.List.Split
import Data.List.NonEmpty
import Data.String.Utils
import Types

data NodeError = NodeAddressNotParsable String deriving (Show, Eq)

parseNodeAddresses :: String -> Either NodeError (NonEmpty Address)
parseNodeAddresses =  fmap fromList . traverse (parseNodeAddress) . (splitOn ",")

parseNodeAddress :: String -> Either NodeError Address
parseNodeAddress =  _parse . (splitOn ":")
  where
    _parse (host:port:[]) = Right $ Address (strip host) (read $ strip port)
    _parse line = Left $ NodeAddressNotParsable $ show line
