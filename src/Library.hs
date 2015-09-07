{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, DataKinds, GADTs, TypeOperators #-}
module Library where

import Prelude hiding ((+))
import Internal
import Primitive
import qualified Data.List as List

jzipWith f c1 c2 = jmap (uncurry f) $ jzip c1 c2

sample ((c1, c2) :: JArgs2 (JArray JNumber) (JArray JNumber)) =
    jzipWith (+) c1 c2

sample2 ((c1, c2) :: JArgs2 (JArray JNumber) (JArray JNumber)) =
    jmap (\x -> x + jfold (+) (literal :: 0 :. 0) c1) c2
