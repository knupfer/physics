module Physics.Units.Derived where

import Physics.Units.Type

type Siemens   = SI N2 N1 P3 P2  Z  Z  Z; siemens   :: Num a => Siemens   a; siemens   = SI 1
type Farad     = SI N2 N1 P4 P2  Z  Z  Z; farad     :: Num a => Farad     a; farad     = SI 1
type Lux       = SI N2  Z  Z  Z  Z  Z P1; lux       :: Num a => Lux       a; lux       = SI 1
type Pascal    = SI N1 P1 N2  Z  Z  Z  Z; pascal    :: Num a => Pascal    a; pascal    = SI 1
type Hertz     = SI  Z  Z N1  Z  Z  Z  Z; hertz     :: Num a => Hertz     a; hertz     = SI 1
type Becquerel = SI  Z  Z N1  Z  Z  Z  Z; becquerel :: Num a => Becquerel a; becquerel = SI 1
type Katal     = SI  Z  Z N1  Z  Z P1  Z; katal     :: Num a => Katal     a; katal     = SI 1
type Radian    = SI  Z  Z  Z  Z  Z  Z  Z; radian    :: Num a => Radian    a; radian    = SI 1
type Steradian = SI  Z  Z  Z  Z  Z  Z  Z; steradian :: Num a => Steradian a; steradian = SI 1
type Lumen     = SI  Z  Z  Z  Z  Z  Z P1; lumen     :: Num a => Lumen     a; lumen     = SI 1
type Coulomb   = SI  Z  Z P1 P1  Z  Z  Z; coulomb   :: Num a => Coulomb   a; coulomb   = SI 1
type Tesla     = SI  Z P1 N2 N1  Z  Z  Z; tesla     :: Num a => Tesla     a; tesla     = SI 1
type Newton    = SI P1 P1 N2  Z  Z  Z  Z; newton    :: Num a => Newton    a; newton    = SI 1
type Gray      = SI P2  Z N2  Z  Z  Z  Z; gray      :: Num a => Gray      a; gray      = SI 1
type Sievert   = SI P2  Z N2  Z  Z  Z  Z; sievert   :: Num a => Sievert   a; sievert   = SI 1
type Ohm       = SI P2 P1 N3 N2  Z  Z  Z; ohm       :: Num a => Ohm       a; ohm       = SI 1
type Volt      = SI P2 P1 N3 N1  Z  Z  Z; volt      :: Num a => Volt      a; volt      = SI 1
type Watt      = SI P2 P1 N3  Z  Z  Z  Z; watt      :: Num a => Watt      a; watt      = SI 1
type Henry     = SI P2 P1 N2 N2  Z  Z  Z; henry     :: Num a => Henry     a; henry     = SI 1
type Weber     = SI P2 P1 N2 N1  Z  Z  Z; weber     :: Num a => Weber     a; weber     = SI 1
type Joule     = SI P2 P1 N2  Z  Z  Z  Z; joule     :: Num a => Joule     a; joule     = SI 1


