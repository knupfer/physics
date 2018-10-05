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

module Physics.Units.SI.Type
  ( module Physics.Units.SI.Type
  , module Physics.Units.Type
  ) where

import Physics.Units.Type

import GHC.TypeLits
import GHC.Generics
import Data.Proxy

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

instance Applicative (SI i ii iii iv v vi vii) where
  pure = SI
  SI f <*> x = f <$> x

#if __GLASGOW_HASKELL__ > 802
instance (Show x, KnownSymbol z,
   (z ~ ( AppendSymbol (ShowUnit "m" i)
          ( AppendSymbol (ShowUnit "kg" ii)
            ( AppendSymbol (ShowUnit "s" iii)
              ( AppendSymbol (ShowUnit "A" iv)
                ( AppendSymbol (ShowUnit "K" v)
                  ( AppendSymbol (ShowUnit "mol" vi) (ShowUnit "cd" vii)))))))))
  => Show (SI i ii iii iv v vi vii x) where
  show (SI x) = show x ++ symbolVal (Proxy :: Proxy z)
#endif
