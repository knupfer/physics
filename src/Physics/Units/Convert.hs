{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE DataKinds           #-}

module Physics.Units.Convert where

import Data.Proxy
import GHC.TypeLits

import Physics.Units.SI.Type
import Physics.Units.Planck.Type
import Physics.Units.Arithmetic

import qualified Physics.Units.SI.Constants as SI

class ExpVal (x :: Exponent) where
  expVal :: Proxy x -> Integer

instance KnownNat n => ExpVal ('Positive n) where
  expVal _ = natVal (Proxy :: Proxy n)

instance KnownNat n => ExpVal ('Negative n) where
  expVal _ = negate (natVal (Proxy :: Proxy n))

fromSI :: forall x metre kilogram second ampere kelvin.
  ( Fractional x
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
  / value SI.planckLength      ^^ expVal (Proxy :: Proxy metre)
  / value SI.planckMass        ^^ expVal (Proxy :: Proxy kilogram)
  / value SI.planckTime        ^^ expVal (Proxy :: Proxy (Minus second ampere))
  / value SI.planckCharge      ^^ expVal (Proxy :: Proxy ampere)
  / value SI.planckTemperature ^^ expVal (Proxy :: Proxy kelvin)

fromPlanck :: forall x metre kilogram second coulomb kelvin.
  ( Fractional x
  , ExpVal metre
  , ExpVal kilogram
  , ExpVal (Plus second coulomb)
  , ExpVal coulomb
  , ExpVal kelvin
  ) => Planck metre kilogram second coulomb kelvin x
    -> SI metre kilogram (Plus second coulomb) coulomb kelvin Z Z x
fromPlanck (Planck x)
  = SI
  $ x
  * value SI.planckLength      ^^ expVal (Proxy :: Proxy metre)
  * value SI.planckMass        ^^ expVal (Proxy :: Proxy kilogram)
  * value SI.planckTime        ^^ expVal (Proxy :: Proxy (Plus second coulomb))
  * value SI.planckCharge      ^^ expVal (Proxy :: Proxy coulomb)
  * value SI.planckTemperature ^^ expVal (Proxy :: Proxy kelvin)

