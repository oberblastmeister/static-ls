module Data.Eff where

import Data.Box

data M a = UnsafeM (IO a)

data IOE

data Local

data Lock e = UnsafeLock e

lock :: e -> Lock e
lock = UnsafeLock

combineLock :: Lock e1 -> Lock e2 -> Lock (e1, e2)
combineLock = undefined

useEff :: Lock e -> r -> (e -> Box r -> Box (M a)) -> a
useEff = undefined

data State s

runState :: s -> (Lock (State s) -> M a) -> M a
runState = undefined
