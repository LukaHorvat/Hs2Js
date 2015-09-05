{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}
module Library where

import Prelude hiding ((+))
import Internal
import Primitive

jzipWith f c1 c2 = jmap (uncurry f) $ jzip c1 c2

sample ((c1, c2) :: JArgs2 (JArray JNumber) (JArray JNumber)) =
    jzipWith (+) c1 c2
