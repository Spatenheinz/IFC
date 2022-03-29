module Utils where

import Data.Function(on)
import Control.Monad.RWS

onlM2 :: Monad m => (b -> b -> c) -> (a -> m b) -> a -> a -> m c
onlM2 = on . liftM2

(...) :: (b -> c) -> (a1 -> a2 -> b) -> a1 -> a2 -> c
(...) = (.).(.)

(.....) :: (b -> c) -> (a1 -> a2 -> a3 -> b) -> a1 -> a2 -> a3 -> c
(.....) = (.).(.).(.)

