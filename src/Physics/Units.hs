{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE DataKinds                  #-}

module Physics.Units where

import Data.Proxy
import GHC.TypeLits (AppendSymbol, symbolVal, KnownSymbol)
import GHC.TypeNats

-- Type

data Exponent = Positive Nat | Negative Nat

type Null = 'Positive 0
type I    = 'Positive 1
type II   = 'Positive 2
type III  = 'Positive 3
type IV   = 'Positive 4
type V    = 'Positive 5

newtype Dim
  (metre    :: Exponent)
  (kilogram :: Exponent)
  (second   :: Exponent)
  (ampere   :: Exponent)
  (kelvin   :: Exponent)
  (mole     :: Exponent)
  (candela  :: Exponent)
  x = Dim x
  deriving (Eq, Ord, Functor, Enum, Read)

-- Show

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
  Dim i ii iii iv v vi vii >*< Dim i' ii' iii' iv' v' vi' vii' = Dim (Plus i i') (Plus ii ii') (Plus iii iii') (Plus iv iv') (Plus v v') (Plus vi vi') (Plus vii vii')

type family (>/<) d d' where
  Dim i ii iii iv v vi vii >/< Dim i' ii' iii' iv' v' vi' vii' = Dim (Minus i i') (Minus ii ii') (Minus iii iii') (Minus iv iv') (Minus v v') (Minus vi vi') (Minus vii vii')

(*<) :: Num x => x -> Dim i ii iii iv v vi vii x -> Dim i ii iii iv v vi vii x
x *< Dim y = Dim (x*y)

(/<) :: Fractional x => x -> Dim i ii iii iv v vi vii x -> Dim (Negate i) (Negate ii) (Negate iii) (Negate iv) (Negate v) (Negate vi) (Negate vii) x
x /< Dim y = Dim (x/y)

(>*<) :: Num x => Dim i ii iii iv v vi vii x -> Dim i' ii' iii' iv' v' vi' vii' x -> Dim (Plus i i') (Plus ii ii') (Plus iii iii') (Plus iv iv') (Plus v v') (Plus vi vi') (Plus vii vii') x
Dim x >*< Dim y = Dim (x*y)

(>/<) :: Fractional x => Dim i ii iii iv v vi vii x -> Dim i' ii' iii' iv' v' vi' vii' x -> Dim (Minus i i') (Minus ii ii') (Minus iii iii') (Minus iv iv') (Minus v v') (Minus vi vi') (Minus vii vii') x
Dim x >/< Dim y = Dim (x/y)

(>+<) :: Num x => Dim i ii iii iv v vi vii x -> Dim i ii iii iv v vi vii x -> Dim i ii iii iv v vi vii x
Dim x >+< Dim y = Dim (x+y)

(>-<) :: Num x => Dim i ii iii iv v vi vii x -> Dim i ii iii iv v vi vii x -> Dim i ii iii iv v vi vii x
Dim x >-< Dim y = Dim (x-y)

infixl 5 >+<, >-<
infixl 6 *<, /<
infixl 7 >*<, >/<

getValue :: Dim i ii iii iv v vi vii a -> a
getValue (Dim a) = a

getDimension :: Num a => Dim i ii iii iv v vi vii a -> Dim i ii iii iv v vi vii a
getDimension _ = Dim 1

-- Base units

type Dimensionless = Dim Null Null Null Null Null Null Null
dimensionless :: Num a => Dimensionless a
dimensionless = Dim 1

type Metre = Dim I Null Null Null Null Null Null
metre :: Num a => Metre a
metre = Dim 1

type Kilogram = Dim Null I Null Null Null Null Null
kilogram :: Num a => Kilogram a
kilogram = Dim 1

type Second = Dim Null Null I Null Null Null Null
second :: Num a => Second a
second = Dim 1

type Ampere = Dim Null Null Null I Null Null Null
ampere :: Num a => Ampere a
ampere = Dim 1

type Kelvin = Dim Null Null Null Null I Null Null
kelvin :: Num a => Kelvin a
kelvin = Dim 1

type Mole = Dim Null Null Null Null Null I Null
mole :: Num a => Mole a
mole = Dim 1

type Candela = Dim Null Null Null Null Null Null I
candela :: Num a => Candela a
candela = Dim 1

-- Derived units

type Radian = Dimensionless
radian :: Num a => Radian a
radian = Dim 1

type Steradian = Dimensionless
steradian :: Num a => Steradian a
steradian = Dim 1

type Hertz = Dimensionless >/< Second
hertz :: Num a => Hertz a
hertz = Dim 1

type Newton = Kilogram >*< Metre >/< Second >/< Second
newton :: Num a => Newton a
newton = Dim 1

type Pascal = Newton >/< Metre >/< Metre
pascal :: Num a => Pascal a
pascal = Dim 1

type Joule = Newton >*< Metre
joule :: Num a => Joule a
joule = Dim 1

type Watt = Joule >/< Second
watt :: Num a => Watt a
watt = Dim 1

type Coulomb = Second >*< Ampere
coulomb :: Num a => Coulomb a
coulomb = Dim 1

type Volt = Watt >/< Ampere
volt :: Num a => Volt a
volt = Dim 1

type Farad = Coulomb >/< Volt
farad :: Num a => Farad a
farad = Dim 1

type Ohm = Volt >/< Ampere
ohm :: Num a => Ohm a
ohm = Dim 1

type Siemens = Dimensionless >/< Ohm
siemens :: Num a => Siemens a
siemens = Dim 1

type Weber = Volt >*< Second
weber :: Num a => Weber a
weber = Dim 1

type Tesla = Weber >/< Metre >/< Metre
tesla :: Num a => Tesla a
tesla = Dim 1

type Henry = Weber >/< Ampere
henry :: Num a => Henry a
henry = Dim 1

type Lumen = Candela
lumen :: Num a => Lumen a
lumen = Dim 1

type Lux = Lumen >/< Metre >/< Metre
lux :: Num a => Lux a
lux = Dim 1

type Becquerel = Dimensionless >/< Second
becquerel :: Num a => Becquerel a
becquerel = Dim 1

type Gray = Joule >/< Kilogram
gray :: Num a => Gray a
gray = Dim 1

type Sievert = Joule >/< Kilogram
sievert :: Num a => Sievert a
sievert = Dim 1

type Katal = Mole >/< Second
katal :: Num a => Katal a
katal = Dim 1


