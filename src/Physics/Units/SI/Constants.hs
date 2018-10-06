{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds     #-}
module Physics.Units.SI.Constants
  ( module Physics.Units.SI.Constants
  , module Physics.Units.SI.Constants.Measured
  ) where

import Physics.Units.SI
import Physics.Units.SI.Constants.Measured

-- Base exact constants

hyperfineSplittingFrequencyOfCaesium133 :: Num a => (Second^-1) a
hyperfineSplittingFrequencyOfCaesium133 = SI 9192631770

lightspeed :: Num a => (Metre>/<Second) a
lightspeed = SI 299792458

planckConstant :: Fractional a => (Kilogram>*<Metre^+2>/<Second) a
planckConstant = SI 6.62607015e-34

elementaryCharge :: Fractional a => (Ampere>*<Second) a
elementaryCharge = SI 1.602176634e-19

boltzmannConstant :: Fractional a => (Kilogram>*<Metre^+2>/<Second^+2>/<Kelvin) a
boltzmannConstant = SI 1.380649e-23

avogadroConstant :: Num a => (Mole^-1) a
avogadroConstant = SI (602214076*10^(15 :: Int))

luminousEfficacy :: Num a => (Candela>*<Second^+3>/<Kilogram>/<Metre^+2) a
luminousEfficacy = SI 683

-- Derived exact constants

secondRadiationConstant :: Fractional a => (Metre>*<Kelvin) a
secondRadiationConstant = planckConstant>*<lightspeed>/<boltzmannConstant

molarPlanckConstant :: Fractional a => (Joule>*<Second>/<Mole) a
molarPlanckConstant = planckConstant>*<avogadroConstant

firstRadiationConstant :: Floating a => (Watt>*<Metre^+2) a
firstRadiationConstant = 2*pi *<planckConstant>*<lightspeed>*<lightspeed

firstRadiationConstantForSpectralRadiance :: Floating a => (Watt>*<Metre^+2) a
firstRadiationConstantForSpectralRadiance = 2 *<planckConstant>*<lightspeed>*<lightspeed

efimovFactor :: Fractional a => One a
efimovFactor = 22.7 *<one

conductanceQuantum :: Fractional a => Siemens a
conductanceQuantum = 2 *<elementaryCharge>*<elementaryCharge>/<planckConstant

inverseConductanceQuantum :: Fractional a => Ohm a
inverseConductanceQuantum = 1 /<conductanceQuantum

reducedPlanckConstant :: Floating a => (Joule>*<Second) a
reducedPlanckConstant = 1/(2*pi) *<planckConstant

josephsonConstant :: Fractional a => (One>/<Volt>/<Second) a
josephsonConstant = 2 *<elementaryCharge>/<planckConstant

magneticFluxQuantum :: Fractional a => Weber a
magneticFluxQuantum = 1 /<josephsonConstant

vonKlitzingConstant :: Fractional a => Ohm a
vonKlitzingConstant = planckConstant>/<elementaryCharge>/<elementaryCharge

faradayConstant :: Fractional a => (Coulomb>/<Mole) a
faradayConstant = elementaryCharge>*<avogadroConstant

gasConstant :: Fractional a => (Joule>/<Mole>/<Kelvin) a
gasConstant = boltzmannConstant>*<avogadroConstant

stefanBoltzmannConstant :: Floating a => (Watt>/<Metre^+2>/<Kelvin^+4) a
stefanBoltzmannConstant = 2/15*pi**5 *<boltzmannConstant>*<boltzmannConstant>*<boltzmannConstant>*<boltzmannConstant>/<planckConstant>/<planckConstant>/<planckConstant>/<lightspeed>/<lightspeed

-- Derived uncertain constants

planckLength :: Floating a => Metre a
planckLength = SI . sqrt . value $ reducedPlanckConstant>/<lightspeed>/<lightspeed>/<lightspeed
  >*<gravitationalConstant

planckMass :: Floating a => Kilogram a
planckMass = SI . sqrt . value $ reducedPlanckConstant>*<lightspeed
  >/<gravitationalConstant

planckTime :: Floating a => Second a
planckTime = SI . sqrt . value $ reducedPlanckConstant>/<lightspeed>/<lightspeed>/<lightspeed>/<lightspeed>/<lightspeed
  >*<gravitationalConstant

planckCharge :: Floating a => Coulomb a
planckCharge = elementaryCharge
  >/<fmap sqrt fineStructureConstant

planckTemperature :: Floating a => Kelvin a
planckTemperature = SI . sqrt . value $ reducedPlanckConstant>*<lightspeed>*<lightspeed>*<lightspeed>*<lightspeed>*<lightspeed>/<boltzmannConstant>/<boltzmannConstant
  >/<gravitationalConstant

magneticConstant :: Fractional a => (Newton>/<Ampere^+2) a
magneticConstant = 2 *<planckConstant>/<lightspeed>/<elementaryCharge>/<elementaryCharge
  >*<fineStructureConstant

electricConstant :: Fractional a => (Farad>/<Metre) a
electricConstant = 1/2 *<elementaryCharge>*<elementaryCharge>/<planckConstant>/<lightspeed
  >/<fineStructureConstant

impedanceOfVacuum :: Fractional a => Ohm a
impedanceOfVacuum = 2 *<planckConstant>/<elementaryCharge>/<elementaryCharge
  >*<fineStructureConstant

coulombConstant :: Floating a => (Kilogram>*<Metre^+3>/<Second^+4>/<Ampere^+2) a
coulombConstant = lightspeed>*<reducedPlanckConstant>/<elementaryCharge>/<elementaryCharge
  >*<fineStructureConstant

bohrMagneton :: Floating a => (Joule>/<Tesla) a
bohrMagneton = 1/(8*pi) *<elementaryCharge>*<lightspeed
  >/<rydbergConstant>*<fineStructureConstant>*<fineStructureConstant

electronMass :: Fractional a => Kilogram a
electronMass = 2 *<planckConstant>/<lightspeed
  >*<rydbergConstant>/<fineStructureConstant>/<fineStructureConstant

electronMolarMass :: Fractional a => (Kilogram>/<Mole) a
electronMolarMass = 2 *<planckConstant>*<avogadroConstant>/<lightspeed
  >*<rydbergConstant>/<fineStructureConstant>/<fineStructureConstant

unifiedAtomicMassUnit :: Fractional a => Kilogram a
unifiedAtomicMassUnit = 2 *<planckConstant>/<lightspeed
  >*<rydbergConstant>/<fineStructureConstant>/<fineStructureConstant>/<electronRelativeMass

molarMassConstant :: Fractional a => (Kilogram>/<Mole) a
molarMassConstant = 2 *<planckConstant>*<avogadroConstant>/<lightspeed
  >*<rydbergConstant>/<fineStructureConstant>/<fineStructureConstant>/<electronRelativeMass

atomicMassOfCarbon12 :: Fractional a => Kilogram a
atomicMassOfCarbon12 = 24 *<planckConstant>/<lightspeed
  >*<rydbergConstant>/<fineStructureConstant>/<fineStructureConstant>/<electronRelativeMass

molarMassOfCarbon12 :: Fractional a => (Kilogram>/<Mole) a
molarMassOfCarbon12 = 24 *<planckConstant>*<avogadroConstant>/<lightspeed
  >*<rydbergConstant>/<fineStructureConstant>/<fineStructureConstant>/<electronRelativeMass

nuclearMagneton :: Floating a => (Joule>/<Tesla) a
nuclearMagneton = 1/2 *<elementaryCharge>*<reducedPlanckConstant
  >/<protonMass

bohrRadius :: Floating a => Metre a
bohrRadius = 1/(4*pi)
  *<fineStructureConstant>/<rydbergConstant

classicalElectronRadius :: Floating a => Metre a
classicalElectronRadius = 1/(4*pi)
  *<fineStructureConstant>*<fineStructureConstant>*<fineStructureConstant>/<rydbergConstant

hartreeEnergy :: Fractional a => Joule a
hartreeEnergy = 2 *<planckConstant>*<lightspeed
  >*<rydbergConstant

quantumOfCirculation :: Fractional a => (Metre^+2>/<Second) a
quantumOfCirculation = 1/4 *<lightspeed
   >*<fineStructureConstant>*<fineStructureConstant>/<rydbergConstant

thomsonCrossSection :: Floating a => (Metre^+2) a
thomsonCrossSection = 1/(6*pi)
  *<fmap (**6) fineStructureConstant>/<rydbergConstant>/<rydbergConstant
