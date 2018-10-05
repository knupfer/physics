module Physics.Units.Base
  ( module Physics.Units.Base
  , module Physics.Units.Type
  ) where

import Physics.Units.Type

value :: Dim i ii iii iv v vi vii a -> a
value (Dim a) = a

unit :: (Num a, Functor f) => f b -> f a
unit = fmap (const 1)

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
