module Physics.Units.Derived where

import Physics.Units.Base

type Siemens   = Dim N2 N1 P3 P2  Z  Z  Z; siemens   :: Num a => Siemens   a; siemens   = Dim 1
type Farad     = Dim N2 N1 P4 P2  Z  Z  Z; farad     :: Num a => Farad     a; farad     = Dim 1
type Lux       = Dim N2  Z  Z  Z  Z  Z P1; lux       :: Num a => Lux       a; lux       = Dim 1
type Pascal    = Dim N1 P1 N2  Z  Z  Z  Z; pascal    :: Num a => Pascal    a; pascal    = Dim 1
type Hertz     = Dim  Z  Z N1  Z  Z  Z  Z; hertz     :: Num a => Hertz     a; hertz     = Dim 1
type Becquerel = Dim  Z  Z N1  Z  Z  Z  Z; becquerel :: Num a => Becquerel a; becquerel = Dim 1
type Katal     = Dim  Z  Z N1  Z  Z P1  Z; katal     :: Num a => Katal     a; katal     = Dim 1
type Radian    = Dim  Z  Z  Z  Z  Z  Z  Z; radian    :: Num a => Radian    a; radian    = Dim 1
type Steradian = Dim  Z  Z  Z  Z  Z  Z  Z; steradian :: Num a => Steradian a; steradian = Dim 1
type Lumen     = Dim  Z  Z  Z  Z  Z  Z P1; lumen     :: Num a => Lumen     a; lumen     = Dim 1
type Coulomb   = Dim  Z  Z P1 P1  Z  Z  Z; coulomb   :: Num a => Coulomb   a; coulomb   = Dim 1
type Tesla     = Dim  Z P1 N2 N1  Z  Z  Z; tesla     :: Num a => Tesla     a; tesla     = Dim 1
type Newton    = Dim P1 P1 N2  Z  Z  Z  Z; newton    :: Num a => Newton    a; newton    = Dim 1
type Gray      = Dim P2  Z N2  Z  Z  Z  Z; gray      :: Num a => Gray      a; gray      = Dim 1
type Sievert   = Dim P2  Z N2  Z  Z  Z  Z; sievert   :: Num a => Sievert   a; sievert   = Dim 1
type Ohm       = Dim P2 P1 N3 N2  Z  Z  Z; ohm       :: Num a => Ohm       a; ohm       = Dim 1
type Volt      = Dim P2 P1 N3 N1  Z  Z  Z; volt      :: Num a => Volt      a; volt      = Dim 1
type Watt      = Dim P2 P1 N3  Z  Z  Z  Z; watt      :: Num a => Watt      a; watt      = Dim 1
type Henry     = Dim P2 P1 N2 N2  Z  Z  Z; henry     :: Num a => Henry     a; henry     = Dim 1
type Weber     = Dim P2 P1 N2 N1  Z  Z  Z; weber     :: Num a => Weber     a; weber     = Dim 1
type Joule     = Dim P2 P1 N2  Z  Z  Z  Z; joule     :: Num a => Joule     a; joule     = Dim 1


