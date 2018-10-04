{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE DataKinds            #-}

module Physics.Units.Arithmetic where

import Physics.Units.Base
import Physics.Units.Derived

import GHC.TypeLits

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
  Dim i ii iii iv v vi vii >*< Dim i' ii' iii' iv' v' vi' vii' = Pretty (Dim (Plus i i') (Plus ii ii') (Plus iii iii') (Plus iv iv') (Plus v v') (Plus vi vi') (Plus vii vii'))

type family (>/<) d d' where
  Dim i ii iii iv v vi vii >/< Dim i' ii' iii' iv' v' vi' vii' = Pretty (Dim (Minus i i') (Minus ii ii') (Minus iii iii') (Minus iv iv') (Minus v v') (Minus vi vi') (Minus vii vii'))

type family Pretty d where
  Pretty (Dim N2 N1 P3 P2  Z  Z  Z) = Siemens
  Pretty (Dim N2 N1 P4 P2  Z  Z  Z) = Farad
  Pretty (Dim N2  Z  Z  Z  Z  Z P1) = Lux
  Pretty (Dim N1 P1 N2  Z  Z  Z  Z) = Pascal
--  Pretty (Dim  Z  Z N1  Z  Z  Z  Z) = Hertz
--  Pretty (Dim  Z  Z N1  Z  Z  Z  Z) = Becquerel
  Pretty (Dim  Z  Z N1  Z  Z P1  Z) = Katal
  Pretty (Dim  Z  Z  Z  Z  Z  Z  Z) = One
--  Pretty (Dim  Z  Z  Z  Z  Z  Z  Z) = Radian
--  Pretty (Dim  Z  Z  Z  Z  Z  Z  Z) = Steradian
  Pretty (Dim  Z  Z  Z  Z  Z  Z P1) = Candela
--  Pretty (Dim  Z  Z  Z  Z  Z  Z P1) = Lumen
  Pretty (Dim  Z  Z  Z  Z  Z P1  Z) = Mole
  Pretty (Dim  Z  Z  Z  Z P1  Z  Z) = Kelvin
  Pretty (Dim  Z  Z  Z P1  Z  Z  Z) = Ampere
  Pretty (Dim  Z  Z P1  Z  Z  Z  Z) = Second
  Pretty (Dim  Z  Z P1 P1  Z  Z  Z) = Coulomb
  Pretty (Dim  Z P1 N2 N1  Z  Z  Z) = Tesla
  Pretty (Dim  Z P1  Z  Z  Z  Z  Z) = Kilogram
  Pretty (Dim P1  Z  Z  Z  Z  Z  Z) = Metre
  Pretty (Dim P1 P1 N2  Z  Z  Z  Z) = Newton
--  Pretty (Dim P2  Z N2  Z  Z  Z  Z) = Gray
--  Pretty (Dim P2  Z N2  Z  Z  Z  Z) = Sievert
  Pretty (Dim P2 P1 N3 N2  Z  Z  Z) = Ohm
  Pretty (Dim P2 P1 N3 N1  Z  Z  Z) = Volt
  Pretty (Dim P2 P1 N3  Z  Z  Z  Z) = Watt
  Pretty (Dim P2 P1 N2 N2  Z  Z  Z) = Henry
  Pretty (Dim P2 P1 N2 N1  Z  Z  Z) = Weber
  Pretty (Dim P2 P1 N2  Z  Z  Z  Z) = Joule
  Pretty d = d

(*<) :: (Num x, Functor f, z ~ f x) => x -> z -> z
x *< y = fmap (x*) y

(>/) :: (Fractional x, Functor f, z ~ f x) => z -> x -> z
x >/ y = fmap (/y) x

(/<) :: Fractional x => x -> Dim i ii iii iv v vi vii x -> Pretty (Dim (Negate i) (Negate ii) (Negate iii) (Negate iv) (Negate v) (Negate vi) (Negate vii)) x
x /< Dim y = Dim (x/y)

(>*<) :: Num x => Dim i ii iii iv v vi vii x -> Dim i' ii' iii' iv' v' vi' vii' x -> (Dim i ii iii iv v vi vii >*< Dim i' ii' iii' iv' v' vi' vii') x
Dim x >*< Dim y = Dim (x*y)

(>/<) :: Fractional x => Dim i ii iii iv v vi vii x -> Dim i' ii' iii' iv' v' vi' vii' x -> (Dim i ii iii iv v vi vii >/< Dim i' ii' iii' iv' v' vi' vii') x
Dim x >/< Dim y = Dim (x/y)

(>+<) :: (Num x, Applicative f, z ~ f x) => z -> z -> z
x >+< y = (+) <$> x <*> y

(>-<) :: (Num x, Applicative f, z ~ f x) => z -> z -> z
x >-< y = (-) <$> x <*> y

infixl 5 >+<, >-<
infixl 6 *<, /<, >/
infixl 7 >*<, >/<
