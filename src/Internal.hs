{-# LANGUAGE ScopedTypeVariables, TypeFamilies, GADTs, DataKinds, MultiParamTypeClasses
           , FlexibleInstances, FlexibleContexts, PolyKinds #-}
module Internal where

import Data.Proxy
import Prelude hiding ((+))
import qualified Prelude

data Nat = Zero | Succ Nat

type One = 'Succ 'Zero

class TypeNum (a :: Nat) where
    intRep :: proxy a -> Int

instance TypeNum 'Zero where
    intRep _ = 0

instance TypeNum a => TypeNum ('Succ a) where
    intRep _ = intRep (Proxy :: Proxy a) Prelude.+ 1

type family Max (x :: Nat) (y :: Nat) :: Nat where
    Max 'Zero x = x
    Max x 'Zero = x
    Max ('Succ x) ('Succ y) = 'Succ (Max x y)

data JNumber
data JString
data JBool
data JArray (t :: *)

data NormalArgument (n :: Nat) t
data TupleMember (n :: Nat) (i :: Nat) t

class Argument a where
    argName :: proxy a -> String

instance TypeNum n => Argument (NormalArgument n t) where
    argName _ = "arg" ++ show (intRep (Proxy :: Proxy n))

instance (TypeNum n, TypeNum i) => Argument (TupleMember n i t) where
    argName _ = "arg" ++ show (intRep (Proxy :: Proxy n)) ++ "[" ++ show (intRep (Proxy :: Proxy i)) ++ "]"

instance TypeNum n => Argument (TupleMember n i1 t1, TupleMember n i2 t2) where
    argName _ = "arg" ++ show (intRep (Proxy :: Proxy n))

instance (TypeNum n1, TypeNum n2) => Argument (NormalArgument n1 t1, NormalArgument n2 t2) where
    argName _ = "arg" ++ show (intRep (Proxy :: Proxy n1)) ++ ", arg" ++ show (intRep (Proxy :: Proxy n2))

class HasFreshName a where
    type FreshName a :: Nat

class Collection c where
    type Element c :: *

instance Collection (JArray t) where
    type Element (JArray t) = t

instance HasFreshName (NormalArgument n t) where
    type FreshName (NormalArgument n t) = n

instance HasFreshName (TupleMember n i t) where
    type FreshName (TupleMember n i t) = n

instance Collection t => Collection (NormalArgument n t) where
    type Element (NormalArgument n t) = Element t

instance Collection t => Collection (TupleMember n i t) where
    type Element (TupleMember n i t) = Element t

type family LambdaArg (n :: Nat) t :: * where
    LambdaArg n (t1, t2) = (TupleMember n 'Zero t1, TupleMember n One t2)
    LambdaArg n t = NormalArgument n t

class Addable a b where
    type SumType a b :: k

class NativeAddable (a :: k) (b :: k) where
    type NativeSumType a b :: k

instance NativeAddable JNumber JNumber where
    type NativeSumType JNumber JNumber = JNumber

instance NativeAddable JString JString where
    type NativeSumType JString JString = JString

instance NativeAddable t1 t2 => Addable (NormalArgument n1 t1) (NormalArgument n2 t2) where
    type SumType (NormalArgument n1 t1) (NormalArgument n2 t2) = NativeSumType t1 t2

instance NativeAddable t1 t2 => Addable (TupleMember n1 i1 t1) (NormalArgument n2 t2) where
    type SumType (TupleMember n1 i1 t1) (NormalArgument n2 t2) = NativeSumType t1 t2

instance NativeAddable t1 t2 => Addable (NormalArgument n2 t2) (TupleMember n1 i1 t1) where
    type SumType (NormalArgument n2 t2) (TupleMember n1 i1 t1) = NativeSumType t1 t2

instance NativeAddable t1 t2 => Addable (TupleMember n2 i2 t2) (TupleMember n1 i1 t1) where
    type SumType (TupleMember n2 i2 t2) (TupleMember n1 i1 t1) = NativeSumType t1 t2

class Expr t where
    type ExprType t :: *

type family FunctionReturn f where
    FunctionReturn (a -> b) = b

toProxy :: a -> Proxy a
toProxy _ = Proxy

type P = Proxy

p :: Proxy a
p = Proxy

class Compilable a where
    compile :: proxy a -> String

instance (Compilable b, Argument a) => Compilable (a -> b) where
    compile _ = "function (" ++ argName (p :: P a) ++ ") { return " ++ compile (p :: P b) ++ "; }"

instance TypeNum n => Compilable (NormalArgument n t) where
    compile = argName

instance (TypeNum n, TypeNum i) => Compilable (TupleMember n i t) where
    compile = argName

type family JArgs1 t1 where
    JArgs1 t1 = NormalArgument 'Zero t1

type family JArgs2 t1 t2 where
    JArgs2 t1 t2 = (NormalArgument 'Zero t1, NormalArgument One t2)
