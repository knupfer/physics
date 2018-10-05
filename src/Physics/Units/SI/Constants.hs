{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds     #-}
module Physics.Units.SI.Constants where

import Physics.Units.SI

-- Planck units

planckLength :: Fractional a => Metre a
planckLength = SI 1.616229e-35

planckMass :: Fractional a => Kilogram a
planckMass = SI 2.176470e-8

planckTime :: Fractional a => Second a
planckTime = SI 5.39116e-44

planckCharge :: Fractional a => Coulomb a
planckCharge = SI 1.875545956e-18

planckTemperature :: Num a => Kelvin a
planckTemperature = SI (1416808*10^(26::Int))

-- Universal constants

lightspeed :: Num a => a *<Metre>/<Second
lightspeed = SI 299792458

gravitationalConstant :: Fractional a => a *<Metre^+3>/<Kilogram>/<Second^+2
gravitationalConstant = SI 6.67408e-11

planckConstant :: Fractional a => a *<Joule>*<Second
planckConstant = SI 6.626070040e-34

reducedPlanckConstant :: Fractional a => a *<Joule>*<Second
reducedPlanckConstant = SI 1.054571800e-34

-- Electromagnetic constants

magneticConstant :: Floating a => a *<Newton>/<Ampere^+2
magneticConstant = SI (pi*4e-7)

electricConstant :: Floating a => a *<Farad>/<Metre
electricConstant = one>/<(magneticConstant>*<lightspeed>*<lightspeed)

impedanceOfVacuum :: Floating a => Ohm a
impedanceOfVacuum = magneticConstant>*<lightspeed

coulombConstant :: Fractional a => a *<Kilogram>*<Metre^+3>/<Second^+4>/<Ampere^+2
coulombConstant = 1e-7*<lightspeed>*<lightspeed>*<newton>/<ampere>/<ampere

elementaryCharge :: Fractional a => Coulomb a
elementaryCharge = 1.6021766208e-19*<coulomb

bohrMagneton :: Fractional a => a *<Joule>/<Tesla
bohrMagneton = 9.274009994e-24 *<joule>/<tesla

conductanceQuantum :: Fractional a => Siemens a
conductanceQuantum = 7.7480917310e-5 *<siemens

inverseConductanceQuantum :: Fractional a => Ohm a
inverseConductanceQuantum = 12906.4037278 *<ohm

josephsonConstant :: Num a => a *<One>/<Volt>/<Second
josephsonConstant = SI (4835978525*10^(5::Int))

magneticFluxQuantum :: Fractional a => Weber a
magneticFluxQuantum = 2.067833831e-15 *<weber

nuclearMagneton :: Fractional a => a *<Joule>/<Tesla
nuclearMagneton = 5.050783699e-27*<joule>/<tesla

vonKlitzingConstant :: Fractional a => Ohm a
vonKlitzingConstant = 25812.8074555 *< ohm

-- Atomic and nuclear constants

bohrRadius :: Fractional a => Metre a
bohrRadius = 5.2917721067e-11 *< metre

classicalElectronRadius :: Fractional a => Metre a
classicalElectronRadius = 2.8179403227e-15 *< metre

electronMass :: Fractional a => Kilogram a
electronMass = 9.10938356e-31 *< kilogram

fermiCouplingConstant :: Num a => a *<Joule^-2
fermiCouplingConstant = SI (45437957*10^(7::Int))

fineStructureConstant :: Fractional a => One a
fineStructureConstant = 7.2973525664e-3*<one

hartreeEnergy :: Fractional a => Joule a
hartreeEnergy = 4.359744650e-18 *< joule

protonMass :: Fractional a => Kilogram a
protonMass = 1.672621898e-27 *< kilogram

quantumOfCirculation :: Fractional a => a *<Metre^+2>/<Second
quantumOfCirculation = 3.6369475486e-4 *<metre>*<metre>/<second

rydbergConstant :: Fractional a => a /< Metre
rydbergConstant = 10973731.568508 /<metre

thomsonCrossSection :: Fractional a => a *<Metre^+2
thomsonCrossSection = 6.6524587158e-29*<metre>*<metre

weakMixingAngle :: Fractional a => One a
weakMixingAngle = 0.2223 *< one

efimovFactor :: Fractional a => One a
efimovFactor = 22.7 *< one

-- Physico-chemical constants

atomicMassConstant :: Fractional a => Kilogram a
atomicMassConstant = 1.660539040e-27 *< kilogram

avogadroConstant :: Num a => a *<Mole^-1
avogadroConstant = SI (6022140857*10^(14::Int))

boltzmannConstant :: Fractional a => a *<Joule>/<Kelvin
boltzmannConstant = 1.38064852e-23*<joule>/<kelvin

faradayConstant :: Fractional a => a *<Coulomb>/<Mole
faradayConstant = 96485.33289 *<coulomb>/<mole

firstRadiationConstant :: Fractional a => a *<Watt>*<Metre^+2
firstRadiationConstant = 3.741771790e-16*<watt>*<metre>*<metre

firstRadiationConstantForSpectralRadiance :: Fractional a => a *<Watt>*<Metre^+2
firstRadiationConstantForSpectralRadiance = 1.191042953e-16*<watt>*<metre>*<metre

lochschmidtConstant :: Num a => a *<Metre^-3
lochschmidtConstant = SI (26867811*10^(18::Int))

gasConstant :: Fractional a => a *<Joule>/<Mole>/<Kelvin
gasConstant = SI 8.3144598

molarPlanckConstant :: Fractional a => a *<Joule>*<Second>/<Mole
molarPlanckConstant = SI 3.9903127110e-10

secondRadiationConstant :: Fractional a => a *<Metre>*<Kelvin
secondRadiationConstant = SI 1.43877736e-2

stefanBoltzmannConstant :: Fractional a => a *<Watt>/<Metre^+2>/<Kelvin^+4
stefanBoltzmannConstant = SI 5.670367e-8

wienDisplacementLawConstant :: Fractional a => a *<Metre>*<Kelvin
wienDisplacementLawConstant = SI 2.8977729e-3

wienBonalEntropyDisplacementLawConstant :: Fractional a => a *<Metre>*<Kelvin
wienBonalEntropyDisplacementLawConstant = SI 3.0029152e-3

-- Adopted values

molarMassConstant :: Fractional a => a *<Kilogram>*<Mole
molarMassConstant = SI 1e-3

molarMassOfCarbon12 :: Fractional a => a *<Kilogram>*<Mole
molarMassOfCarbon12 = SI 1.2e-2

standardAccelerationOfGravity :: Fractional a => a *<Metre>/<Second^+2
standardAccelerationOfGravity = SI 9.80665

standardAtmosphere :: Num a => Pascal a
standardAtmosphere = SI 101325
