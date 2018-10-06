{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds     #-}
module Physics.Units.SI.Constants.Measured where

import Physics.Units.SI

electronRelativeMass :: Fractional a => One a
electronRelativeMass =  5.485799111e-4 *<one

fineStructureConstant :: Fractional a => One a
fineStructureConstant = 7.2973525664e-3 *<one

rydbergConstant :: Fractional a => (Metre^-1) a
rydbergConstant = 1.0973731568508e-7 /<metre

gravitationalConstant :: Fractional a => (Metre^+3>/<Kilogram>/<Second^+2) a
gravitationalConstant = 6.67408e-11 *<metre>*<metre>*<metre>/<kilogram>/<second>/<second

protonMass :: Fractional a => Kilogram a
protonMass = 1.672621898e-27 *<kilogram

weakMixingAngle :: Fractional a => One a
weakMixingAngle = 0.2223 *<one

wienDisplacementLawConstant :: Fractional a => (Metre>*<Kelvin) a
wienDisplacementLawConstant = 2.8977729e-3*<metre>*<kelvin
