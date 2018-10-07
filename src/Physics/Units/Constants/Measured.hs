{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds     #-}
module Physics.Units.Constants.Measured where

import Physics.Units.Arithmetic
import Physics.Units.Type
import Physics.Units.Base

electronRelativeMass :: Fractional a => One a
electronRelativeMass = 5.485799111e-4 *< one

fineStructureConstant :: Fractional a => One a
fineStructureConstant = 7.2973525664e-3 *< one

rydbergConstant :: Fractional a => (One >/< Metre) a
rydbergConstant = 10973731.568508 *< one >/< metre

gravitationalConstant :: Fractional a => (Cube Metre >/< Kilogram >/< Square Second) a
gravitationalConstant = 6.67408e-11 *< cube metre >/< kilogram >/< square second

protonMass :: Fractional a => Kilogram a
protonMass = 1.672621898e-27 *< kilogram

weakMixingAngle :: Fractional a => One a
weakMixingAngle = 0.2223 *< one

wienDisplacementLawConstant :: Fractional a => (Metre >*< Kelvin) a
wienDisplacementLawConstant = 2.8977729e-3 *< metre >*< kelvin
