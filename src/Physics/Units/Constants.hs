{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds     #-}
module Physics.Units.Constants
  ( module Physics.Units.Constants
  , module Physics.Units.Constants.Measured
  ) where

import Physics.Units.Base
import Physics.Units.Derived
import Physics.Units.Arithmetic
import Physics.Units.Constants.Measured

-- Base exact constants

hyperfineSplittingFrequencyOfCaesium133 :: Num a => (One >/< Second) a
hyperfineSplittingFrequencyOfCaesium133 = SI 9192631770

lightspeed :: Num a => (Metre >/< Second) a
lightspeed = SI 299792458

planckConstant :: Fractional a => (Kilogram >*< Square Metre >/< Second) a
planckConstant = SI 6.62607015e-34

elementaryCharge :: Fractional a => (Ampere >*< Second) a
elementaryCharge = SI 1.602176634e-19

boltzmannConstant :: Fractional a => (Kilogram >*< Square Metre >/< Square Second >/< Kelvin) a
boltzmannConstant = SI 1.380649e-23

avogadroConstant :: Num a => (One >/< Mole) a
avogadroConstant = SI (602214076*10^(15 :: Int))

luminousEfficacy :: Num a => (Candela >*< Cube Second >/< Kilogram >/< Square Metre) a
luminousEfficacy = SI 683

-- Derived exact constants

secondRadiationConstant :: Fractional a => (Metre >*< Kelvin) a
secondRadiationConstant = planckConstant >*< lightspeed >/< boltzmannConstant

molarPlanckConstant :: Fractional a => (Joule >*< Second >/< Mole) a
molarPlanckConstant = planckConstant >*< avogadroConstant

firstRadiationConstant :: Floating a => (Watt >*< Square Metre) a
firstRadiationConstant = 2*pi *< planckConstant >*< square lightspeed

firstRadiationConstantForSpectralRadiance :: Floating a => (Watt >*< Square Metre) a
firstRadiationConstantForSpectralRadiance = 2 *< planckConstant >*< square lightspeed

efimovFactor :: Fractional a => One a
efimovFactor = 22.7 *< one

conductanceQuantum :: Fractional a => Siemens a
conductanceQuantum = 2 *< square elementaryCharge >/< planckConstant

inverseConductanceQuantum :: Fractional a => Ohm a
inverseConductanceQuantum = 1 /<conductanceQuantum

reducedPlanckConstant :: Floating a => (Joule >*< Second) a
reducedPlanckConstant = 1/(2*pi) *< planckConstant

josephsonConstant :: Fractional a => (One >/< Volt >/< Second) a
josephsonConstant = 2 *< elementaryCharge >/< planckConstant

magneticFluxQuantum :: Fractional a => Weber a
magneticFluxQuantum = 1 /<josephsonConstant

vonKlitzingConstant :: Fractional a => Ohm a
vonKlitzingConstant = planckConstant >/< square elementaryCharge

faradayConstant :: Fractional a => (Coulomb >/< Mole) a
faradayConstant = elementaryCharge >*< avogadroConstant

gasConstant :: Fractional a => (Joule >/< Mole >/< Kelvin) a
gasConstant = boltzmannConstant >*< avogadroConstant

stefanBoltzmannConstant :: Floating a => (Watt >/< Square Metre >/< Tesseract Kelvin) a
stefanBoltzmannConstant = 2/15*pi**5 *< tesseract boltzmannConstant >/< cube planckConstant >/< square lightspeed

electronVolt :: Fractional a => Joule a
electronVolt = elementaryCharge >/< coulomb >*< joule

-- Derived uncertain constants

planckLength :: Floating a => Metre a
planckLength = squareRoot $ reducedPlanckConstant >/< cube lightspeed
  >*< gravitationalConstant

planckMass :: Floating a => Kilogram a
planckMass = squareRoot $ reducedPlanckConstant >*< lightspeed
  >/< gravitationalConstant

planckTime :: Floating a => Second a
planckTime = squareRoot $ reducedPlanckConstant >/< penteract lightspeed
  >*< gravitationalConstant

planckCharge :: Floating a => Coulomb a
planckCharge = elementaryCharge
  >/< squareRoot fineStructureConstant

planckTemperature :: Floating a => Kelvin a
planckTemperature = squareRoot $ reducedPlanckConstant >*< penteract lightspeed >/< square boltzmannConstant
  >/< gravitationalConstant

magneticConstant :: Fractional a => (Newton >/< Square Ampere) a
magneticConstant = 2 *< planckConstant >/< lightspeed >/< square elementaryCharge
  >*< fineStructureConstant

electricConstant :: Fractional a => (Farad >/< Metre) a
electricConstant = 1/2 *< square elementaryCharge >/< planckConstant >/< lightspeed
  >/< fineStructureConstant

impedanceOfVacuum :: Fractional a => Ohm a
impedanceOfVacuum = 2 *< planckConstant >/< square elementaryCharge
  >*< fineStructureConstant

coulombConstant :: Floating a => (Kilogram >*< Cube Metre >/< Tesseract Second >/< Square Ampere) a
coulombConstant = lightspeed >*< reducedPlanckConstant >/< square elementaryCharge
  >*< fineStructureConstant

bohrMagneton :: Floating a => (Joule >/< Tesla) a
bohrMagneton = 1/(8*pi) *< elementaryCharge >*< lightspeed
  >/< rydbergConstant >*< square fineStructureConstant

electronMass :: Fractional a => Kilogram a
electronMass = 2 *< planckConstant >/< lightspeed
  >*< rydbergConstant >/< square fineStructureConstant

electronMolarMass :: Fractional a => (Kilogram >/< Mole) a
electronMolarMass = 2 *< planckConstant >*< avogadroConstant >/< lightspeed
  >*< rydbergConstant >/< square fineStructureConstant

unifiedAtomicMassUnit :: Fractional a => Kilogram a
unifiedAtomicMassUnit = 2 *< planckConstant >/< lightspeed
  >*< rydbergConstant >/< square fineStructureConstant >/< electronRelativeMass

molarMassConstant :: Fractional a => (Kilogram >/< Mole) a
molarMassConstant = 2 *< planckConstant >*< avogadroConstant >/< lightspeed
  >*< rydbergConstant >/< square fineStructureConstant >/< electronRelativeMass

atomicMassOfCarbon12 :: Fractional a => Kilogram a
atomicMassOfCarbon12 = 24 *< planckConstant >/< lightspeed
  >*< rydbergConstant >/< square fineStructureConstant >/< electronRelativeMass

molarMassOfCarbon12 :: Fractional a => (Kilogram >/< Mole) a
molarMassOfCarbon12 = 24 *< planckConstant >*< avogadroConstant >/< lightspeed
  >*< rydbergConstant >/< square fineStructureConstant >/< electronRelativeMass

nuclearMagneton :: Floating a => (Joule >/< Tesla) a
nuclearMagneton = 1/2 *< elementaryCharge >*< reducedPlanckConstant
  >/< protonMass

bohrRadius :: Floating a => Metre a
bohrRadius = 1/(4*pi)
  *< fineStructureConstant >/< rydbergConstant

classicalElectronRadius :: Floating a => Metre a
classicalElectronRadius = 1/(4*pi)
  *< cube fineStructureConstant >/< rydbergConstant

hartreeEnergy :: Fractional a => Joule a
hartreeEnergy = 2 *< planckConstant >*< lightspeed
  >*< rydbergConstant

quantumOfCirculation :: Fractional a => (Square Metre >/< Second) a
quantumOfCirculation = 1/4 *< lightspeed
  >*< square fineStructureConstant >/< rydbergConstant

thomsonCrossSection :: Floating a => Square Metre a
thomsonCrossSection = 1/(6*pi)
  *< square (cube fineStructureConstant) >/< square rydbergConstant
