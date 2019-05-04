module Types where

type Host = String
type Port = Int
data Address = Address Host Port
  deriving Eq

instance Show Address  where
  show (Address host port) = show host ++ ":" ++ show port
