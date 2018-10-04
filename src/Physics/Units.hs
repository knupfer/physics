{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE DataKinds                  #-}

module Physics.Units where

import Data.Proxy
import GHC.TypeLits (AppendSymbol, symbolVal, KnownSymbol)
import GHC.TypeNats
import GHC.Generics

-- Type

data Exponent = Positive Nat | Negative Nat

type P4 = 'Positive 4
type P3 = 'Positive 3
type P2 = 'Positive 2
type P1 = 'Positive 1
type  Z = 'Positive 0
type N1 = 'Negative 1
type N2 = 'Negative 2
type N3 = 'Negative 3
type N4 = 'Negative 4

newtype Dim
  (metre    :: Exponent)
  (kilogram :: Exponent)
  (second   :: Exponent)
  (ampere   :: Exponent)
  (kelvin   :: Exponent)
  (mole     :: Exponent)
  (candela  :: Exponent)
  x = Dim x
  deriving (Eq, Ord, Functor, Enum, Read, Bounded, Generic, Foldable, Traversable)

-- Instances

instance Applicative (Dim i ii iii iv v vi vii) where
  pure = Dim
  Dim f <*> x = f <$> x

instance (Show x,
   (KnownSymbol ( AppendSymbol (ShowUnit "m" i)
                 ( AppendSymbol (ShowUnit "kg" ii)
                  ( AppendSymbol (ShowUnit "s" iii)
                   ( AppendSymbol (ShowUnit "A" iv)
                    ( AppendSymbol (ShowUnit "K" v)
                     ( AppendSymbol (ShowUnit "mol" vi) (ShowUnit "cd" vii)))))))))
  => Show (Dim i ii iii iv v vi vii x) where
  show (Dim x) = show x ++ symbolVal units
    where
      units :: Proxy ( AppendSymbol (ShowUnit "m" i)
                       ( AppendSymbol (ShowUnit "kg" ii)
                         ( AppendSymbol (ShowUnit "s" iii)
                           ( AppendSymbol (ShowUnit "A" iv)
                             ( AppendSymbol (ShowUnit "K" v)
                               ( AppendSymbol (ShowUnit "mol" vi) (ShowUnit "cd" vii)))))))
      units = Proxy

type family ShowUnit u e where
  ShowUnit _ ('Positive 0) = ""
  ShowUnit u e = AppendSymbol " " (AppendSymbol u (ShowExponent e))

type family ShowExponent a where
  ShowExponent ('Positive 1) = ""
  ShowExponent ('Positive n) = ShowNatural n
  ShowExponent ('Negative n) = AppendSymbol "⁻" (ShowNatural n)

type family ShowNatural a where
  ShowNatural 0 = "⁰"
  ShowNatural 1 = "¹"
  ShowNatural 2 = "²"
  ShowNatural 3 = "³"
  ShowNatural 4 = "⁴"
  ShowNatural 5 = "⁵"
  ShowNatural 6 = "⁶"
  ShowNatural 7 = "⁷"
  ShowNatural 8 = "⁸"
  ShowNatural 9 = "⁹"
  ShowNatural n = AppendSymbol (ShowNatural (Div n 10)) (ShowNatural (Mod n 10))

-- Operations

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

type family (*<) d d' where
  x *< Dim i ii iii iv v vi vii = Dim i ii iii iv v vi vii x

type family (/<) d d' where
  x /< Dim i ii iii iv v vi vii = Dim (Negate i) (Negate ii) (Negate iii) (Negate iv) (Negate v) (Negate vi) (Negate vii) x

type family (>*<) d d' where
  Dim i ii iii iv v vi vii >*< Dim i' ii' iii' iv' v' vi' vii' = Pretty (Dim (Plus i i') (Plus ii ii') (Plus iii iii') (Plus iv iv') (Plus v v') (Plus vi vi') (Plus vii vii'))

type family (>/<) d d' where
  Dim i ii iii iv v vi vii >/< Dim i' ii' iii' iv' v' vi' vii' = Pretty (Dim (Minus i i') (Minus ii ii') (Minus iii iii') (Minus iv iv') (Minus v v') (Minus vi vi') (Minus vii vii'))

type family Pretty d where
  Pretty (Dim N2 N1 P3 P2  Z  Z  Z) = Siemens
  Pretty (Dim N2 N1 P4 P2  Z  Z  Z) = Farad
  Pretty (Dim N2  Z  Z  Z  Z  Z P1) = Lux
  Pretty (Dim N1 P1 N2  Z  Z  Z  Z) = Pascal
--  Pretty (Dim  Z  Z N1  Z  Z  Z  Z) = Hertz
--  Pretty (Dim  Z  Z N1  Z  Z  Z  Z) = Becquerel
  Pretty (Dim  Z  Z N1  Z  Z P1  Z) = Katal
  Pretty (Dim  Z  Z  Z  Z  Z  Z  Z) = One
--  Pretty (Dim  Z  Z  Z  Z  Z  Z  Z) = Radian
--  Pretty (Dim  Z  Z  Z  Z  Z  Z  Z) = Steradian
  Pretty (Dim  Z  Z  Z  Z  Z  Z P1) = Candela
--  Pretty (Dim  Z  Z  Z  Z  Z  Z P1) = Lumen
  Pretty (Dim  Z  Z  Z  Z  Z P1  Z) = Mole
  Pretty (Dim  Z  Z  Z  Z P1  Z  Z) = Kelvin
  Pretty (Dim  Z  Z  Z P1  Z  Z  Z) = Ampere
  Pretty (Dim  Z  Z P1  Z  Z  Z  Z) = Second
  Pretty (Dim  Z  Z P1 P1  Z  Z  Z) = Coulomb
  Pretty (Dim  Z P1 N2 N1  Z  Z  Z) = Tesla
  Pretty (Dim  Z P1  Z  Z  Z  Z  Z) = Kilogram
  Pretty (Dim P1  Z  Z  Z  Z  Z  Z) = Metre
  Pretty (Dim P1 P1 N2  Z  Z  Z  Z) = Newton
--  Pretty (Dim P2  Z N2  Z  Z  Z  Z) = Gray
--  Pretty (Dim P2  Z N2  Z  Z  Z  Z) = Sievert
  Pretty (Dim P2 P1 N3 N2  Z  Z  Z) = Ohm
  Pretty (Dim P2 P1 N3 N1  Z  Z  Z) = Volt
  Pretty (Dim P2 P1 N3  Z  Z  Z  Z) = Watt
  Pretty (Dim P2 P1 N2 N2  Z  Z  Z) = Henry
  Pretty (Dim P2 P1 N2 N1  Z  Z  Z) = Weber
  Pretty (Dim P2 P1 N2  Z  Z  Z  Z) = Joule
  Pretty d = d

(*<) :: (d ~ Dim i ii iii iv v vi vii x, Num x) => x -> d -> d
x *< Dim y = Dim (x*y)

(/<) :: Fractional x => x -> Dim i ii iii iv v vi vii x -> Pretty (Dim (Negate i) (Negate ii) (Negate iii) (Negate iv) (Negate v) (Negate vi) (Negate vii)) x
x /< Dim y = Dim (x/y)

(>*<) :: Num x => Dim i ii iii iv v vi vii x -> Dim i' ii' iii' iv' v' vi' vii' x -> (Dim i ii iii iv v vi vii >*< Dim i' ii' iii' iv' v' vi' vii') x
Dim x >*< Dim y = Dim (x*y)

(>/<) :: Fractional x => Dim i ii iii iv v vi vii x -> Dim i' ii' iii' iv' v' vi' vii' x -> (Dim i ii iii iv v vi vii >/< Dim i' ii' iii' iv' v' vi' vii') x
Dim x >/< Dim y = Dim (x/y)

(>+<) :: (d ~ Dim i ii iii iv v vi vii x, Num x) => d -> d -> d
Dim x >+< Dim y = Dim (x+y)

(>-<) :: (d ~ Dim i ii iii iv v vi vii x, Num x) => d -> d -> d
Dim x >-< Dim y = Dim (x-y)

infixl 5 >+<, >-<
infixl 6 *<, /<
infixl 7 >*<, >/<

getValue :: Dim i ii iii iv v vi vii a -> a
getValue (Dim a) = a

getDimension :: Num a => Dim i ii iii iv v vi vii a -> Dim i ii iii iv v vi vii a
getDimension _ = Dim 1

-- Base units

type One = Dim Z Z Z Z Z Z Z
one :: Num a => One a
one = Dim 1

type Metre = Dim P1 Z Z Z Z Z Z
metre :: Num a => Metre a
metre = Dim 1

type Kilogram = Dim Z P1 Z Z Z Z Z
kilogram :: Num a => Kilogram a
kilogram = Dim 1

type Second = Dim Z Z P1 Z Z Z Z
second :: Num a => Second a
second = Dim 1

type Ampere = Dim Z Z Z P1 Z Z Z
ampere :: Num a => Ampere a
ampere = Dim 1

type Kelvin = Dim Z Z Z Z P1 Z Z
kelvin :: Num a => Kelvin a
kelvin = Dim 1

type Mole = Dim Z Z Z Z Z P1 Z
mole :: Num a => Mole a
mole = Dim 1

type Candela = Dim Z Z Z Z Z Z P1
candela :: Num a => Candela a
candela = Dim 1

-- Derived units

type Siemens   = Dim N2 N1 P3 P2  Z  Z  Z; siemens   :: Num a => Siemens   a; siemens   = Dim 1
type Farad     = Dim N2 N1 P4 P2  Z  Z  Z; farad     :: Num a => Farad     a; farad     = Dim 1
type Lux       = Dim N2  Z  Z  Z  Z  Z P1; lux       :: Num a => Lux       a; lux       = Dim 1
type Pascal    = Dim N1 P1 N2  Z  Z  Z  Z; pascal    :: Num a => Pascal    a; pascal    = Dim 1
type Hertz     = Dim  Z  Z N1  Z  Z  Z  Z; hertz     :: Num a => Hertz     a; hertz     = Dim 1
type Becquerel = Dim  Z  Z N1  Z  Z  Z  Z; becquerel :: Num a => Becquerel a; becquerel = Dim 1
type Katal     = Dim  Z  Z N1  Z  Z P1  Z; katal     :: Num a => Katal     a; katal     = Dim 1
type Radian    = Dim  Z  Z  Z  Z  Z  Z  Z; radian    :: Num a => Radian    a; radian    = Dim 1
type Steradian = Dim  Z  Z  Z  Z  Z  Z  Z; steradian :: Num a => Steradian a; steradian = Dim 1
type Lumen     = Dim  Z  Z  Z  Z  Z  Z P1; lumen     :: Num a => Lumen     a; lumen     = Dim 1
type Coulomb   = Dim  Z  Z P1 P1  Z  Z  Z; coulomb   :: Num a => Coulomb   a; coulomb   = Dim 1
type Tesla     = Dim  Z P1 N2 N1  Z  Z  Z; tesla     :: Num a => Tesla     a; tesla     = Dim 1
type Newton    = Dim P1 P1 N2  Z  Z  Z  Z; newton    :: Num a => Newton    a; newton    = Dim 1
type Gray      = Dim P2  Z N2  Z  Z  Z  Z; gray      :: Num a => Gray      a; gray      = Dim 1
type Sievert   = Dim P2  Z N2  Z  Z  Z  Z; sievert   :: Num a => Sievert   a; sievert   = Dim 1
type Ohm       = Dim P2 P1 N3 N2  Z  Z  Z; ohm       :: Num a => Ohm       a; ohm       = Dim 1
type Volt      = Dim P2 P1 N3 N1  Z  Z  Z; volt      :: Num a => Volt      a; volt      = Dim 1
type Watt      = Dim P2 P1 N3  Z  Z  Z  Z; watt      :: Num a => Watt      a; watt      = Dim 1
type Henry     = Dim P2 P1 N2 N2  Z  Z  Z; henry     :: Num a => Henry     a; henry     = Dim 1
type Weber     = Dim P2 P1 N2 N1  Z  Z  Z; weber     :: Num a => Weber     a; weber     = Dim 1
type Joule     = Dim P2 P1 N2  Z  Z  Z  Z; joule     :: Num a => Joule     a; joule     = Dim 1


