-- Name : Pham Hoang Minh
-- Student Number : 1024376

module Calculus
where

data Primitive
  =  Sin  -- trigonometric: sine
  |  Cos                          -- cosine
  |  Exp  -- exponential
  deriving (Show)

infixl 6 :+:
infixl 7 :*:
infixr 9 :.:

data Function
  =  Const Rational         -- constant function
  |  Id                     -- identity
  |  Prim Primitive         -- primitive function
  |  Function :+: Function  -- addition of functions
  |  Function :*: Function  -- multiplication of functions
  |  Function :.: Function  -- composition of functions
  deriving (Show)

apply :: Function -> (Double -> Double)
apply (Const x) k = (k * 0 + fromRational x) 
apply (Id) k = k
apply (Prim Sin) k = sin k
apply (Prim Cos) k = cos k
apply (Prim Exp) k = exp k 
apply (e1 :+: e2) k= (apply e1 k) + (apply e2 k)
apply (e1 :*: e2) k= (apply e1 k) * (apply e2 k)
apply (e1 :.: e2) k= apply e1 (apply e2 k) 

derives :: Function -> Function
derives (e1 :.: e2) = (derive e1) :.: e2

derive   :: Function -> Function
derive (Const _) = Const 0
derive Id = Const 1
derive (Prim Sin) = Prim Cos
derive (Prim Cos) = (Const (-1)) :*: (Prim Sin)
derive (Prim Exp) = Prim Exp
derive (e1 :+: e2) = (derive e1) :+: (derive e2)
derive (e1 :*: e2) = e1 :*: (derive e2) :+: e2 :*: (derive e1)
derive (e1 :.: e2) = (derive e2) :*: derives (e1 :.: e2)
 
simplify :: Function -> Function
simplify (Const 0 :+: e) = e
simplify (Const 0 :*: e) = Const 0
simplify (e :+: Const 0) = e
simplify (e :*: Const 0) = Const 0

simplify (e :*: Const 1) = e
simplify (Const 1 :*: e) = e

simplify (e :+: Const k) = Const k :+: e 
simplify (e :*: Const k) = Const k :*: e

simplify (Const k :+: Const k' :+: e) = Const (k+k') :+: e
simplify (Const k :+: Const k' :*: e) = Const (k+k') :*: e

simplify (Const k :*: Const k' :+: e) = Const (k*k') :+: e
simplify (Const k :*: Const k' :*: e) = Const (k*k') :*: e