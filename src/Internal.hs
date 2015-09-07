{-# LANGUAGE ScopedTypeVariables, TypeFamilies, GADTs, DataKinds, MultiParamTypeClasses
           , FlexibleInstances, FlexibleContexts, PolyKinds, UndecidableInstances
           , TypeOperators #-}
module Internal where

import Data.Proxy
import Prelude hiding ((+))
import qualified Prelude
import qualified GHC.TypeLits as GHC

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

data JType = JNumber | JString | JBool | JArray JType | JPair JType JType

data NormalArgument (n :: Nat) (t :: JType)
data TupleMember (n :: Nat) (i :: Nat) (t :: JType)

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

instance HasFreshName (NormalArgument n t) where
    type FreshName (NormalArgument n t) = Zero

instance HasFreshName (TupleMember n i t) where
    type FreshName (TupleMember n i t) = Zero

instance HasFreshName b => HasFreshName (a -> b) where
    type FreshName (a -> b) = FreshName b

type family LambdaArg (n :: Nat) t :: * where
    LambdaArg n (JPair t1 t2) = (TupleMember n 'Zero t1, TupleMember n One t2)
    LambdaArg n t = NormalArgument n t

class Addable (a :: JType) (b :: JType) where
    type SumType a b :: JType

instance Addable JNumber JNumber where
    type SumType JNumber JNumber = JNumber

instance Addable JString JString where
    type SumType JString JString = JString

class Expr t where
    type ExprType t :: JType

instance Expr (NormalArgument n t) where
    type ExprType (NormalArgument n t) = t

instance Expr (TupleMember n i t) where
    type ExprType (TupleMember n i t) = t

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

type family JArgs1 (t1 :: JType) where
    JArgs1 t1 = NormalArgument 'Zero t1

type family JArgs2 (t1 :: JType) (t2 :: JType) where
    JArgs2 t1 t2 = (NormalArgument 'Zero t1, NormalArgument One t2)

type family ArrayElement (arr :: JType) :: JType where
    ArrayElement (JArray t) = t

type family Element c where
    Element c = ArrayElement (ExprType c)

data PositiveNumber (w :: GHC.Nat) (d :: GHC.Nat)
data NegativeNumber (w :: GHC.Nat) (d :: GHC.Nat)

type wholePart :. decimalPart = PositiveNumber wholePart decimalPart

type family N a where
    N (PositiveNumber n m) = NegativeNumber n m
    N (NegativeNumber n m) = PositiveNumber n m

instance Expr (PositiveNumber w d) where
    type ExprType (PositiveNumber w d) = JNumber

instance Expr (NegativeNumber w d) where
    type ExprType (NegativeNumber w d) = JNumber

instance HasFreshName (PositiveNumber w d) where
    type FreshName (PositiveNumber w d) = Zero

instance HasFreshName (NegativeNumber w d) where
    type FreshName (NegativeNumber w d) = Zero

instance (GHC.KnownNat w, GHC.KnownNat d) => Compilable (PositiveNumber w d) where
    compile _ = case (GHC.natVal (p :: P w), GHC.natVal (p :: P d)) of
        (w, 0) -> show w
        (w, d) -> show w ++ "." ++ show d

instance (GHC.KnownNat w, GHC.KnownNat d) => Compilable (NegativeNumber w d) where
    compile _ = case (GHC.natVal (p :: P w), GHC.natVal (p :: P d)) of
        (w, 0) -> "(-" ++ show w ++ ")"
        (w, d) -> "(-" ++ show w ++ "." ++ show d ++ ")"

literal :: a
literal = undefined
