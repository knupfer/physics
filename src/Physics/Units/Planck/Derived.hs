module Physics.Units.Planck.Derived where

import Physics.Units.Planck.Base

type Siemens   = Planck N2 N1 P1 P2  Z; siemens   :: Num a => Siemens   a; siemens   = Planck 1
type Farad     = Planck N2 N1 P2 P2  Z; farad     :: Num a => Farad     a; farad     = Planck 1
type Pascal    = Planck N1 P1 N2  Z  Z; pascal    :: Num a => Pascal    a; pascal    = Planck 1
type Hertz     = Planck  Z  Z N1  Z  Z; hertz     :: Num a => Hertz     a; hertz     = Planck 1
type Becquerel = Planck  Z  Z N1  Z  Z; becquerel :: Num a => Becquerel a; becquerel = Planck 1
type Ampere    = Planck  Z  Z N1 P1  Z; ampere    :: Num a => Ampere    a; ampere    = Planck 1
type Radian    = Planck  Z  Z  Z  Z  Z; radian    :: Num a => Radian    a; radian    = Planck 1
type Steradian = Planck  Z  Z  Z  Z  Z; steradian :: Num a => Steradian a; steradian = Planck 1
type Tesla     = Planck  Z P1 N1 N1  Z; tesla     :: Num a => Tesla     a; tesla     = Planck 1
type Newton    = Planck P1 P1 N2  Z  Z; newton    :: Num a => Newton    a; newton    = Planck 1
type Gray      = Planck P2  Z N2  Z  Z; gray      :: Num a => Gray      a; gray      = Planck 1
type Sievert   = Planck P2  Z N2  Z  Z; sievert   :: Num a => Sievert   a; sievert   = Planck 1
type Watt      = Planck P2 P1 N3  Z  Z; watt      :: Num a => Watt      a; watt      = Planck 1
type Volt      = Planck P2 P1 N2 N1  Z; volt      :: Num a => Volt      a; volt      = Planck 1
type Joule     = Planck P2 P1 N2  Z  Z; joule     :: Num a => Joule     a; joule     = Planck 1
type Ohm       = Planck P2 P1 N1 N2  Z; ohm       :: Num a => Ohm       a; ohm       = Planck 1
type Weber     = Planck P2 P1 N1 N1  Z; weber     :: Num a => Weber     a; weber     = Planck 1
type Henry     = Planck P2 P1  Z N2  Z; henry     :: Num a => Henry     a; henry     = Planck 1
