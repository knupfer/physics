{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE CPP                        #-}

module Physics.Units.Type where

import GHC.TypeLits
import GHC.Generics
import Data.Proxy

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

newtype SI
  (metre    :: Exponent)
  (kilogram :: Exponent)
  (second   :: Exponent)
  (ampere   :: Exponent)
  (kelvin   :: Exponent)
  (mole     :: Exponent)
  (candela  :: Exponent)
  x = SI x
  deriving (Eq, Ord, Functor, Enum, Read, Bounded, Generic, Foldable, Traversable)

instance Applicative (SI metre kilogram second ampere kelvin mole candela) where
  pure = SI
  SI f <*> x = f <$> x

#if __GLASGOW_HASKELL__ > 802
instance (Show x, KnownSymbol z,
   (z ~ ( AppendSymbol (ShowUnit "m" metre)
          ( AppendSymbol (ShowUnit "kg" kilogram)
            ( AppendSymbol (ShowUnit "s" second)
              ( AppendSymbol (ShowUnit "A" ampere)
                ( AppendSymbol (ShowUnit "K" kelvin)
                  ( AppendSymbol (ShowUnit "mol" mole) (ShowUnit "cd" candela)))))))))
  => Show (SI metre kilogram second ampere kelvin mole candela x) where
  show (SI x) = show x ++ symbolVal (Proxy :: Proxy z)

type family ShowUnit u e where
  ShowUnit u ('Positive 0) = ""
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
#endif

newtype Planck
  (metre    :: Exponent)
  (kilogram :: Exponent)
  (second   :: Exponent)
  (coulomb  :: Exponent)
  (kelvin   :: Exponent)
  x = Planck x
  deriving (Eq, Ord, Functor, Enum, Read, Bounded, Generic, Foldable, Traversable)

instance Applicative (Planck metre kilogram second coulomb kelvin) where
  pure = Planck
  Planck f <*> x = f <$> x

#if __GLASGOW_HASKELL__ > 802
instance (Show x, KnownSymbol z,
   (z ~ ( AppendSymbol (ShowUnit "mₚ" metre)
          ( AppendSymbol (ShowUnit "kgₚ" kilogram)
            ( AppendSymbol (ShowUnit "sₚ" second)
              ( AppendSymbol (ShowUnit "Cₚ" coulomb)
                ( ShowUnit "Kₚ" kelvin)))))))
  => Show (Planck metre kilogram second coulomb kelvin x) where
  show (Planck x) = show x ++ symbolVal (Proxy :: Proxy z)
#endif
