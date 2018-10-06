{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE DataKinds           #-}

module Physics.Units.Convert where

import Data.Proxy
import GHC.TypeLits

import Physics.Units.SI
import Physics.Units.SI.Constants
import Physics.Units.Planck.Type

class ExpVal (x :: Exponent) where
  expVal :: Proxy x -> Integer

instance KnownNat n => ExpVal ('Positive n) where
  expVal _ = natVal (Proxy :: Proxy n)

instance KnownNat n => ExpVal ('Negative n) where
  expVal _ = negate (natVal (Proxy :: Proxy n))

fromSI :: forall x metre kilogram second ampere kelvin.
  ( Floating x
  , ExpVal metre
  , ExpVal kilogram
  , ExpVal (Minus second ampere)
  , ExpVal ampere
  , ExpVal kelvin
  ) => SI metre kilogram second ampere kelvin Z Z x
    -> Planck metre kilogram (Minus second ampere) ampere kelvin x
fromSI (SI x)
  = Planck
  $ x
  / value planckLength      ^^ expVal (Proxy :: Proxy metre)
  / value planckMass        ^^ expVal (Proxy :: Proxy kilogram)
  / value planckTime        ^^ expVal (Proxy :: Proxy (Minus second ampere))
  / value planckCharge      ^^ expVal (Proxy :: Proxy ampere)
  / value planckTemperature ^^ expVal (Proxy :: Proxy kelvin)

fromPlanck :: forall x metre kilogram second coulomb kelvin.
  ( Floating x
  , ExpVal metre
  , ExpVal kilogram
  , ExpVal second
  , ExpVal coulomb
  , ExpVal kelvin
  ) => Planck metre kilogram second coulomb kelvin x
    -> SI metre kilogram (Plus second coulomb) coulomb kelvin Z Z x
fromPlanck (Planck x)
  = SI
  $ x
  * value planckLength      ^^ expVal (Proxy :: Proxy metre)
  * value planckMass        ^^ expVal (Proxy :: Proxy kilogram)
  * value planckTime        ^^ expVal (Proxy :: Proxy second)
  * value planckCharge      ^^ expVal (Proxy :: Proxy coulomb)
  * value planckTemperature ^^ expVal (Proxy :: Proxy kelvin)

