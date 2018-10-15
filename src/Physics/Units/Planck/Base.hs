module Physics.Units.Planck.Base where

import Physics.Units.Type

type One = Planck Z Z Z Z Z
one :: Num a => One a
one = Planck 1

type Metre = Planck P1 Z Z Z Z
metre :: Num a => Metre a
metre = Planck 1

type Kilogram = Planck Z P1 Z Z Z
kilogram :: Num a => Kilogram a
kilogram = Planck 1

type Second = Planck Z Z P1 Z Z
second :: Num a => Second a
second = Planck 1

type Coulomb = Planck Z Z Z P1 Z
coulomb :: Num a => Coulomb a
coulomb = Planck 1

type Kelvin = Planck Z Z Z Z P1
kelvin :: Num a => Kelvin a
kelvin = Planck 1
