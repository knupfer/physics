{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE CPP                  #-}

module Physics.Units.Arithmetic where

import qualified Physics.Units.Base    as SI
import qualified Physics.Units.Derived as SI
import Physics.Units.Planck.Type (Planck)
import Physics.Units.Type

import Data.Coerce
import Data.Proxy
import GHC.TypeLits

value :: Coercible (f a) a => f a -> a
value = coerce

unit :: (Num a, Functor f) => f b -> f a
unit = fmap (const 1)

(*<) :: (Num x, Functor f, z ~ f x) => x -> z -> z
x *< y = fmap (x*) y

(>/) :: (Fractional x, Functor f, z ~ f x) => z -> x -> z
x >/ y = fmap (/y) x

(/<) :: (Fractional x, Functor f, Coercible (f x) ((f^-1) x)) => x -> f x -> (f^-1) x
x /< y = coerce (fmap (x/) y)

(>*<) :: (Num x, Coercible (f x) x, Coercible (f' x) x, Applicative (f>*<f')) => f x -> f' x -> (f >*< f') x
x >*< y = pure (coerce x * coerce y)

(>/<) :: (Fractional x, Coercible (f x) x, Coercible (f' x) x, Applicative (f>/<f')) => f x -> f' x -> (f >/< f') x
x >/< y = pure (coerce x / coerce y)

(>+<) :: (Num x, Applicative f, z ~ f x) => z -> z -> z
x >+< y = (+) <$> x <*> y

(>-<) :: (Num x, Applicative f, z ~ f x) => z -> z -> z
x >-< y = (-) <$> x <*> y

infixl 6 >+<, >-<
infixl 7 >*<, >/<, *<, /<, >/
infixr 8 ^+, ^-

nthRoot :: (KnownNat n, Floating x, Functor f, Coercible (f x) (NthRoot n f x)) => Proxy n -> f x -> NthRoot n f x
nthRoot p = coerce . fmap (** recip (fromInteger $ natVal p))

type SquareRoot d = NthRoot 2 d
squareRoot :: (Coercible (f x) (SquareRoot f x), Floating x, Functor f) => f x -> SquareRoot f x
squareRoot = nthRoot (Proxy :: Proxy 2)

type CubeRoot d = NthRoot 3 d
cubeRoot :: (Coercible (f x) (CubeRoot f x), Floating x, Functor f) => f x -> CubeRoot f x
cubeRoot = nthRoot (Proxy :: Proxy 3)

hypercube :: (KnownNat n, Num x, Functor f, Coercible (f x) ((f^+n) x)) => Proxy n -> f x -> (f^+n) x
hypercube p = coerce . fmap (^natVal p)

type Square d = d^+2
square :: (Coercible (f x) (Square f x), Num x, Functor f) => f x -> Square f x
square = hypercube (Proxy :: Proxy 2)

type Cube d = d^+3
cube :: (Coercible (f x) (Cube f x), Num x, Functor f) => f x -> Cube f x
cube = hypercube (Proxy :: Proxy 3)

type Tesseract d = d^+4
tesseract :: (Coercible (f x) (Tesseract f x), Num x, Functor f) => f x -> Tesseract f x
tesseract = hypercube (Proxy :: Proxy 4)

type Penteract d = d^+5
penteract :: (Coercible (f x) (Penteract f x), Num x, Functor f) => f x -> Penteract f x
penteract = hypercube (Proxy :: Proxy 5)

type family Plus a b where
  Plus ('Negative 0) x = x
  Plus x ('Negative 0) = x
  Plus ('Positive 0) x = x
  Plus x ('Positive 0) = x
  Plus ('Positive m) ('Positive n) = 'Positive (m+n)
  Plus ('Positive m) ('Negative n) = Plus ('Positive (m-1)) ('Negative (n-1))
  Plus ('Negative m) ('Positive n) = Plus ('Negative (m-1)) ('Positive (n-1))
  Plus ('Negative m) ('Negative n) = 'Negative (m+n)

type family Negate a where
  Negate ('Negative m) = 'Positive m
  Negate ('Positive 0) = 'Positive 0
  Negate ('Positive m) = 'Negative m

type family Minus a b where
  Minus x ('Negative m) = Plus x ('Positive m)
  Minus x ('Positive m) = Plus x ('Negative m)

type family (>*<) d d' where
  SI i ii iii iv v vi vii >*< SI i' ii' iii' iv' v' vi' vii' = Pretty (SI (Plus i i') (Plus ii ii') (Plus iii iii') (Plus iv iv') (Plus v v') (Plus vi vi') (Plus vii vii'))
  Planck i ii iii iv v    >*< Planck i' ii' iii' iv' v'      = Pretty (Planck (Plus i i') (Plus ii ii') (Plus iii iii') (Plus iv iv') (Plus v v'))

type family (>/<) d d' where
  SI i ii iii iv v vi vii >/< SI i' ii' iii' iv' v' vi' vii' = Pretty (SI (Minus i i') (Minus ii ii') (Minus iii iii') (Minus iv iv') (Minus v v') (Minus vi vi') (Minus vii vii'))
  Planck i ii iii iv v    >/< Planck i' ii' iii' iv' v'      = Pretty (Planck (Minus i i') (Minus ii ii') (Minus iii iii') (Minus iv iv') (Minus v v'))

type family (^+) d n where
  d ^+ 0 = d >/< d
  d ^+ n = d >*< d^+(n-1)

type family (^-) d n where
  d ^- n = d >/< d^+(n+1)

type family NthRoot n d where
  NthRoot 1 d = d
  NthRoot n (SI i ii iii iv v vi vii) = SI (Divide i n) (Divide ii n) (Divide iii n) (Divide iv n) (Divide v n) (Divide vi n) (Divide vii n)
  NthRoot n (Planck i ii iii iv v) = Planck (Divide i n) (Divide ii n) (Divide iii n) (Divide iv n) (Divide v n)

type family IsInteger a b c where
  IsInteger a b 0 = Div a b
#if __GLASGOW_HASKELL__ >= 804
  IsInteger a b c = TypeError ( 'ShowType a
                                ':<>: 'Text "/"
                                ':<>: 'ShowType b
                                ':<>: 'Text ": Rational exponents are not yet supported."
                              )
#else
type family Mod x y where
  Mod 0 y = 0
  Mod x y = Mod (x-y) y

type family Div x y where
  Div 0 y = 0
  Div x y = 1 + Div (x-y) y
#endif

type family Divide e n where
  Divide ('Positive x) y = 'Positive (IsInteger x y (Mod x y))
  Divide ('Negative x) y = 'Negative (IsInteger x y (Mod x y))

type family Pretty d where
  Pretty (SI N2 N1 P3 P2  Z  Z  Z) = SI.Siemens
  Pretty (SI N2 N1 P4 P2  Z  Z  Z) = SI.Farad
  Pretty (SI N2  Z  Z  Z  Z  Z P1) = SI.Lux
  Pretty (SI N1 P1 N2  Z  Z  Z  Z) = SI.Pascal
  Pretty (SI  Z  Z N1  Z  Z P1  Z) = SI.Katal
  Pretty (SI  Z  Z  Z  Z  Z  Z  Z) = SI.One
  Pretty (SI  Z  Z  Z  Z  Z  Z P1) = SI.Candela
  Pretty (SI  Z  Z  Z  Z  Z P1  Z) = SI.Mole
  Pretty (SI  Z  Z  Z  Z P1  Z  Z) = SI.Kelvin
  Pretty (SI  Z  Z  Z P1  Z  Z  Z) = SI.Ampere
  Pretty (SI  Z  Z P1  Z  Z  Z  Z) = SI.Second
  Pretty (SI  Z  Z P1 P1  Z  Z  Z) = SI.Coulomb
  Pretty (SI  Z P1 N2 N1  Z  Z  Z) = SI.Tesla
  Pretty (SI  Z P1  Z  Z  Z  Z  Z) = SI.Kilogram
  Pretty (SI P1  Z  Z  Z  Z  Z  Z) = SI.Metre
  Pretty (SI P1 P1 N2  Z  Z  Z  Z) = SI.Newton
  Pretty (SI P2 P1 N3 N2  Z  Z  Z) = SI.Ohm
  Pretty (SI P2 P1 N3 N1  Z  Z  Z) = SI.Volt
  Pretty (SI P2 P1 N3  Z  Z  Z  Z) = SI.Watt
  Pretty (SI P2 P1 N2 N2  Z  Z  Z) = SI.Henry
  Pretty (SI P2 P1 N2 N1  Z  Z  Z) = SI.Weber
  Pretty (SI P2 P1 N2  Z  Z  Z  Z) = SI.Joule
  Pretty d = d

