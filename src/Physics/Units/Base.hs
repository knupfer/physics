module Physics.Units.Base where

import Physics.Units.Type

type One = SI Z Z Z Z Z Z Z
one :: Num a => One a
one = SI 1

type Metre = SI P1 Z Z Z Z Z Z
metre :: Num a => Metre a
metre = SI 1

type Kilogram = SI Z P1 Z Z Z Z Z
kilogram :: Num a => Kilogram a
kilogram = SI 1

type Second = SI Z Z P1 Z Z Z Z
second :: Num a => Second a
second = SI 1

type Ampere = SI Z Z Z P1 Z Z Z
ampere :: Num a => Ampere a
ampere = SI 1

type Kelvin = SI Z Z Z Z P1 Z Z
kelvin :: Num a => Kelvin a
kelvin = SI 1

type Mole = SI Z Z Z Z Z P1 Z
mole :: Num a => Mole a
mole = SI 1

type Candela = SI Z Z Z Z Z Z P1
candela :: Num a => Candela a
candela = SI 1
