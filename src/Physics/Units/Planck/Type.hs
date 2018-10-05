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

module Physics.Units.Planck.Type
  ( module Physics.Units.Planck.Type
  , module Physics.Units.Type
  ) where

import Physics.Units.Type

import GHC.TypeLits
import GHC.Generics
import Data.Proxy

newtype Planck
  (metre    :: Exponent)
  (kilogram :: Exponent)
  (second   :: Exponent)
  (coulomb  :: Exponent)
  (kelvin   :: Exponent)
  x = Planck x
  deriving (Eq, Ord, Functor, Enum, Read, Bounded, Generic, Foldable, Traversable)

instance Applicative (Planck i ii iii iv v) where
  pure = Planck
  Planck f <*> x = f <$> x

#if __GLASGOW_HASKELL__ > 802
instance (Show x, KnownSymbol z,
   (z ~ ( AppendSymbol (ShowUnit "mₚ" i)
          ( AppendSymbol (ShowUnit "kgₚ" ii)
            ( AppendSymbol (ShowUnit "sₚ" iii)
              ( AppendSymbol (ShowUnit "Cₚ" iv)
                ( ShowUnit "Kₚ" v)))))))
  => Show (Planck i ii iii iv v x) where
  show (Planck x) = show x ++ symbolVal (Proxy :: Proxy z)
#endif
