{-# LANGUAGE KindSignatures, DataKinds, GADTs, TypeFamilies, ScopedTypeVariables, RankNTypes
           , UndecidableInstances #-}
module Primitive where

import Internal
import qualified GHC.TypeLits as GHC

data Zip c1 c2 where
    Zip :: (Collection c1, Collection c2)
        => c1 -> c2 -> Zip c1 c2

jzip :: (Collection c1, Collection c2)
     => c1 -> c2 -> Zip c1 c2
jzip = Zip

data Map f c where
    Map :: (Collection c
         , n ~ FreshName c
         , t1 ~ Element c
         , Expr e
         , f ~ (LambdaArg ('Succ n) (Element c) -> e))
        => f -> c -> Map f c

jmap :: (Collection c
      , n ~ FreshName c
      , t1 ~ Element c
      , Expr e
      , f ~ (LambdaArg ('Succ n) (Element c) -> e))
     => f -> c -> Map f c
jmap = Map

data Add a b (t :: *) where
    Add :: (Addable a b, t ~ SumType a b) => a -> b -> Add a b t

(+) :: (Addable a b, t ~ SumType a b) => a -> b -> Add a b t
(+) = Add

instance (HasFreshName c1, HasFreshName c2) => HasFreshName (Zip c1 c2) where
    type FreshName (Zip c1 c2) = Max (FreshName c1) (FreshName c2)

instance Collection (Zip c1 c2) where
    type Element (Zip c1 c2) = (Element c1, Element c2)

instance HasFreshName c => HasFreshName (Map f c) where
    type FreshName (Map f c) = 'Succ ('Succ (FreshName c))

instance Collection (Map f c) where
    type Element (Map f c) = FunctionReturn f

instance Expr (Add a b t) where
    type ExprType (Add a b t) = t

instance Expr (Map f c) where
    type ExprType (Map f c) = JArray (FunctionReturn f)

instance Expr (Zip c1 c2) where
    type ExprType (Zip c1 c2) = JArray (Element c1, Element c2)

instance (Compilable f, Compilable c) => Compilable (Map f c) where
    compile _ = "map(" ++ compile (p :: P f) ++ ", " ++ compile (p :: P c) ++ ")"

instance (Compilable c1, Compilable c2) => Compilable (Zip c1 c2) where
    compile _ = "zip(" ++ compile (p :: P c1) ++ ", " ++ compile (p :: P c2) ++ ")"

instance (Compilable a, Compilable b) => Compilable (Add a b t) where
    compile _ = "(" ++ compile (p :: P a) ++ " + " ++ compile (p :: P b) ++ ")"
