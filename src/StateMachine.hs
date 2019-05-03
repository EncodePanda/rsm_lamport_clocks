module StateMachine where

import Control.Monad.State

data Command = Add Int | Mult Int deriving Show

type TheState = Int

machine :: Command -> State TheState ()
machine (Add x)  = modify (+x)
machine (Mult x) = modify (*x)
