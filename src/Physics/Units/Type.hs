{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DataKinds                  #-}

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
