module Data.Box where

import Data.ByteString
import GHC.StaticPtr

data AntiBox a = UnsafeAntiBox a

data Box a = UnsafeBox a

intoBox :: StaticPtr a -> Box a
intoBox = UnsafeBox . deRefStaticPtr

appBox :: Box (a -> b) -> Box a -> Box b
appBox (UnsafeBox f) (UnsafeBox x) = UnsafeBox (f x)

unBox :: Box a -> a
unBox (UnsafeBox x) = x
