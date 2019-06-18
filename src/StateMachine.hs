{-# LANGUAGE DeriveGeneric #-}
module StateMachine where

import Data.Binary
import GHC.Generics (Generic)
import Control.Monad.State
import Data.ByteString.Lazy

data Command = Add Int | Mult Int deriving (Eq, Show, Generic)

type TheState = Int

instance Binary Command

machine :: Command -> State TheState ()
machine (Add x)  = modify (+x)
machine (Mult x) = modify (*x)

serialize :: Command -> ByteString
serialize c = encode c

deserialize :: ByteString -> Command
deserialize bs = decode bs
