{-# LANGUAGE DataKinds, GADTs, TypeFamilies, ScopedTypeVariables, RankNTypes
           , UndecidableInstances, FlexibleContexts #-}
module Primitive where

import Internal
import qualified GHC.TypeLits as GHC

data Zip c1 c2 where
    Zip :: (JArray a ~ ExprType c1
          , JArray b ~ ExprType c2)
         => c1 -> c2 -> Zip c1 c2

jzip :: (JArray a ~ ExprType c1
       , JArray b ~ ExprType c2)
      => c1 -> c2 -> Zip c1 c2
jzip = Zip

data Map f c where
    Map :: (n1 ~ FreshName c
          , JArray t1 ~ ExprType c
          , n2 ~ FreshName e
          , n  ~ Succ (Max n1 n2)
          , f ~ (LambdaArg n t1 -> e))
         => f -> c -> Map f c

jmap :: (n1 ~ FreshName c
       , JArray t1 ~ ExprType c
       , n2 ~ FreshName e
       , n  ~ Succ (Max n1 n2)
       , f ~ (LambdaArg n t1 -> e))
      => f -> c -> Map f c
jmap = Map

data Fold f i c where
    Fold :: (JArray t ~ ExprType c
           , f ~ (LambdaArg n1 (ExprType i) -> LambdaArg n2 t -> e)
           , ExprType i ~ ExprType e
           , m1 ~ FreshName e
           , m2 ~ FreshName i
           , m3 ~ FreshName c
           , n1 ~ Succ (Max (Max m1 m2) m3)
           , n2 ~ Succ n1)
          => f -> i -> c -> Fold f i c

jfold :: (JArray t ~ ExprType c
       , f ~ (LambdaArg n1 (ExprType i) -> LambdaArg n2 t -> e)
       , ExprType i ~ ExprType e
       , m1 ~ FreshName e
       , m2 ~ FreshName i
       , m3 ~ FreshName c
       , n1 ~ Succ (Max (Max m1 m2) m3)
       , n2 ~ Succ n1)
      => f -> i -> c -> Fold f i c
jfold = Fold

data Add a b where
    Add :: (Addable (ExprType a) (ExprType b)) => a -> b -> Add a b

(+) :: (Addable (ExprType a) (ExprType b)) => a -> b -> Add a b
(+) = Add

instance (HasFreshName c1, HasFreshName c2) => HasFreshName (Zip c1 c2) where
    type FreshName (Zip c1 c2) = Max (FreshName c1) (FreshName c2)

instance (HasFreshName c, HasFreshName f) => HasFreshName (Map f c) where
    type FreshName (Map f c) = 'Succ (Max (FreshName f) (FreshName c))

instance (HasFreshName a, HasFreshName b) => HasFreshName (Add a b) where
    type FreshName (Add a b) = Succ (Max (FreshName a) (FreshName b))

instance (HasFreshName f, HasFreshName i, HasFreshName c) => HasFreshName (Fold f i c) where
    type FreshName (Fold f i c) = Succ (Max (Max (FreshName f) (FreshName i)) (FreshName c))

instance Expr (Add a b) where
    type ExprType (Add a b) = SumType (ExprType a) (ExprType b)

instance Expr (Map f c) where
    type ExprType (Map f c) = JArray (ExprType (FunctionReturn f))

instance Expr (Zip c1 c2) where
    type ExprType (Zip c1 c2) = JArray (JPair (Element c1) (Element c2))

instance Expr (Fold f i c) where
    type ExprType (Fold f i c) = ExprType i

instance (Compilable f, Compilable c) => Compilable (Map f c) where
    compile _ = "map(" ++ compile (p :: P f) ++ ", " ++ compile (p :: P c) ++ ")"

instance (Compilable c1, Compilable c2) => Compilable (Zip c1 c2) where
    compile _ = "zip(" ++ compile (p :: P c1) ++ ", " ++ compile (p :: P c2) ++ ")"

instance (Compilable a, Compilable b) => Compilable (Add a b) where
    compile _ = "(" ++ compile (p :: P a) ++ " + " ++ compile (p :: P b) ++ ")"

instance (Compilable f, Compilable i, Compilable c) => Compilable (Fold f i c) where
    compile _ = "fold(" ++ compile (p :: P f) ++ ", " ++ compile (p :: P i) ++ ", " ++ compile (p :: P c) ++ ")"
