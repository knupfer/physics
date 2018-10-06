{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds     #-}
module Physics.Units.SI.Constants
  ( module Physics.Units.SI.Constants
  , module Physics.Units.SI.Constants.Measured
  ) where

import Physics.Units.SI
import Physics.Units.SI.Constants.Measured

-- Base exact constants

hyperfineSplittingFrequencyOfCaesium133 :: Num a => a *<Second^-1
hyperfineSplittingFrequencyOfCaesium133 = SI 9192631770

lightspeed :: Num a => a *<Metre>/<Second
lightspeed = SI 299792458

planckConstant :: Fractional a => a *<Kilogram>*<Metre^+2>/<Second
planckConstant = SI 6.62607015e-34

elementaryCharge :: Fractional a => a *<Ampere>*<Second
elementaryCharge = SI 1.602176634e-19

boltzmannConstant :: Fractional a => a *<Kilogram>*<Metre^+2>/<Second^+2>/<Kelvin
boltzmannConstant = SI 1.380649e-23

avogadroConstant :: Num a => a *<Mole^-1
avogadroConstant = SI (602214076*10^(15 :: Int))

luminousEfficacy :: Num a => a *<Candela>*<Second^+3>/<Kilogram>/<Metre^+2
luminousEfficacy = SI 683

-- Derived exact constants

secondRadiationConstant :: Fractional a => a *<Metre>*<Kelvin
secondRadiationConstant = planckConstant>*<lightspeed>/<boltzmannConstant

molarPlanckConstant :: Fractional a => a *<Joule>*<Second>/<Mole
molarPlanckConstant = planckConstant>*<avogadroConstant

firstRadiationConstant :: Floating a => a *<Watt>*<Metre^+2
firstRadiationConstant = 2*pi*<planckConstant>*<lightspeed>*<lightspeed

firstRadiationConstantForSpectralRadiance :: Floating a => a *<Watt>*<Metre^+2
firstRadiationConstantForSpectralRadiance = 2*<planckConstant>*<lightspeed>*<lightspeed

efimovFactor :: Fractional a => One a
efimovFactor = 22.7 *< one

conductanceQuantum :: Fractional a => Siemens a
conductanceQuantum = 2*<elementaryCharge>*<elementaryCharge>/<planckConstant

inverseConductanceQuantum :: Fractional a => Ohm a
inverseConductanceQuantum = 1/<conductanceQuantum

reducedPlanckConstant :: Floating a => a *<Joule>*<Second
reducedPlanckConstant = planckConstant>/2*pi

josephsonConstant :: Fractional a => a *<One>/<Volt>/<Second
josephsonConstant = 2*<elementaryCharge>/<planckConstant

magneticFluxQuantum :: Fractional a => Weber a
magneticFluxQuantum = 1/<josephsonConstant

vonKlitzingConstant :: Fractional a => Ohm a
vonKlitzingConstant = planckConstant>/<elementaryCharge>/<elementaryCharge

faradayConstant :: Fractional a => a *<Coulomb>/<Mole
faradayConstant = elementaryCharge>*<avogadroConstant

gasConstant :: Fractional a => a *<Joule>/<Mole>/<Kelvin
gasConstant = boltzmannConstant>*<avogadroConstant

stefanBoltzmannConstant :: Floating a => a *<Watt>/<Metre^+2>/<Kelvin^+4
stefanBoltzmannConstant = 2*pi^(5::Int)*<boltzmannConstant>*<boltzmannConstant>*<boltzmannConstant>*<boltzmannConstant>/<planckConstant>/<planckConstant>/<planckConstant>/<lightspeed>/<lightspeed>/15

-- Derived uncertain constants

planckLength :: Floating a => Metre a
planckLength = SI . sqrt . value $ reducedPlanckConstant>/<lightspeed>/<lightspeed>/<lightspeed
  >*< gravitationalConstant

planckMass :: Floating a => Kilogram a
planckMass = SI . sqrt . value $ reducedPlanckConstant>*<lightspeed
  >/< gravitationalConstant

planckTime :: Floating a => Second a
planckTime = SI . sqrt . value $ reducedPlanckConstant>/<lightspeed>/<lightspeed>/<lightspeed>/<lightspeed>/<lightspeed
  >*< gravitationalConstant

planckCharge :: Floating a => Coulomb a
planckCharge = elementaryCharge
  >/< fmap sqrt fineStructureConstant

planckTemperature :: Floating a => Kelvin a
planckTemperature = SI . sqrt . value $ reducedPlanckConstant>*<lightspeed>*<lightspeed>*<lightspeed>*<lightspeed>*<lightspeed>/<boltzmannConstant>/<boltzmannConstant
  >/< gravitationalConstant

magneticConstant :: Fractional a => a *<Newton>/<Ampere^+2
magneticConstant = 2*<planckConstant>/<lightspeed>/<elementaryCharge>/<elementaryCharge
  >*< fineStructureConstant

electricConstant :: Fractional a => a *<Farad>/<Metre
electricConstant = 0.5*<elementaryCharge>*<elementaryCharge>/<planckConstant>/<lightspeed
  >/< fineStructureConstant

impedanceOfVacuum :: Fractional a => Ohm a
impedanceOfVacuum = 2*<planckConstant>/<elementaryCharge>/<elementaryCharge
  >*< fineStructureConstant

coulombConstant :: Floating a => a *<Kilogram>*<Metre^+3>/<Second^+4>/<Ampere^+2
coulombConstant = lightspeed>*<reducedPlanckConstant>/<elementaryCharge>/<elementaryCharge
  >*< fineStructureConstant

bohrMagneton :: Floating a => a *<Joule>/<Tesla
bohrMagneton = 1/(8*pi)*<elementaryCharge>*<lightspeed
  >/< rydbergConstant >*< fineStructureConstant >*< fineStructureConstant

electronMass :: Fractional a => Kilogram a
electronMass = 2*<planckConstant>/<lightspeed
  >*< rydbergConstant >/< fineStructureConstant >/< fineStructureConstant

electronMolarMass :: Fractional a => a *<Kilogram>/<Mole
electronMolarMass = 2*<planckConstant>*<avogadroConstant>/<lightspeed
  >*< rydbergConstant >/< fineStructureConstant >/< fineStructureConstant

unifiedAtomicMassUnit :: Fractional a => Kilogram a
unifiedAtomicMassUnit = 2*<planckConstant>/<lightspeed
  >*< rydbergConstant >/< fineStructureConstant >/< fineStructureConstant >/< electronRelativeMass

molarMassConstant :: Fractional a => a *<Kilogram>/<Mole
molarMassConstant = 2*<planckConstant>*<avogadroConstant>/<lightspeed
  >*< rydbergConstant >/< fineStructureConstant >/< fineStructureConstant >/< electronRelativeMass

atomicMassOfCarbon12 :: Fractional a => Kilogram a
atomicMassOfCarbon12 = 24*<planckConstant>/<lightspeed
  >*< rydbergConstant >/< fineStructureConstant >/< fineStructureConstant >/< electronRelativeMass

molarMassOfCarbon12 :: Fractional a => a *<Kilogram>/<Mole
molarMassOfCarbon12 = 24*<planckConstant>*<avogadroConstant>/<lightspeed
  >*< rydbergConstant >/< fineStructureConstant >/< fineStructureConstant >/< electronRelativeMass

nuclearMagneton :: Floating a => a *<Joule>/<Tesla
nuclearMagneton = 0.5*<elementaryCharge>*<reducedPlanckConstant
  >/< protonMass

bohrRadius :: Floating a => Metre a
bohrRadius = 1/(4*pi)
  /< rydbergConstant >*< fineStructureConstant

classicalElectronRadius :: Floating a => Metre a
classicalElectronRadius = 1/(4*pi)
  /< rydbergConstant >*< fineStructureConstant >*< fineStructureConstant >*< fineStructureConstant

hartreeEnergy :: Fractional a => Joule a
hartreeEnergy = 2*<planckConstant>*<lightspeed
  >*< rydbergConstant

quantumOfCirculation :: Fractional a => a *<Metre^+2>/<Second
quantumOfCirculation = 0.25*<lightspeed
  >/< rydbergConstant >*< fineStructureConstant >*< fineStructureConstant

thomsonCrossSection :: Floating a => a *<Metre^+2
thomsonCrossSection = 1/(6*pi) *<one
  >/< rydbergConstant >/< rydbergConstant >*< fmap (^(6 :: Int)) fineStructureConstant
