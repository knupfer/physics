# Physics

`physics` is a library for working with quantities in a performant and
type safe manner. It provides some common constants and functions to
work on quantities.

## Typesafety

Quantities do not have an instance of Num to prevent multiplication without changing the units.
Special functions are provided which substitute common operations like `>+<`, `>-<`, `>/<`, `>*<`.

The naming convention is that operators get on quantity facing sides a
greater than or an less than sign.  If you want to multiply a quantity with a number you can use `*<`.

## Planck units

`physics` supports planck units, albeit for most cases they are not
recommended.  The standard used are SI units.

```haskell
import Physics.Units
import qualified Physics.Units.Planck as P

main = do
  print lightspeed
  -- 299792458 m s⁻¹
  print $ fromSI lightspeed
  -- 1.0 mₚ sₚ⁻¹
  print . fromPlanck $ P.metre >/< P.second
  -- 2.99792458e8 m s⁻¹
```

## Constants

Constants are divided into measured constants (which are sorted by
increasing precision), exact constants (lightspeed for example) and
derived constants.  All derived constants are defined in terms of
measured and exact constants and not simply as a number.

Because of this, updating constants is quite easy, only a small number
of measured constants need to be updated when more precise
measurements are available.

## Performance

`physics` strives to be as performant as possible.  Quantities are
newtypes and operators are defined in terms of num operators.

## Example usage

```haskell
{-# LANGUAGE TypeOperators #-}

module Main where

import Physics.Units

main :: IO ()
main = do
  print $ fmap round momentum
  -- 992 m kg s⁻¹
  print $ fmap round kineticEnergy
  -- 5176 m² kg s⁻²

mass :: Kilogram Double
mass = 95 *< kilogram

time :: Second Double
time = 9.58 *< second

distance :: Metre Double
distance = 100 *< metre

speed :: (Metre >/< Second) Double
speed = distance >/< time

momentum :: (Newton >*< Second) Double
momentum = mass >*< speed

kineticEnergy :: Joule Double
kineticEnergy = 1/2 *< mass >*< square speed

-- This would be a type error:
-- kineticEnergy = 1/2 *< mass >*< speed
```

## Compatibility

`physics` works with GHC 7.10.3 or newer.  Some features like showing
an Unit or nicer type errors are only available on newer ghcs.
