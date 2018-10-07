{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds     #-}
module Physics.Units.Constants.Measured where

import Physics.Units.Arithmetic
import Physics.Units.Type
import Physics.Units.Base

-- Measured constants sorted by ascending precision

weakMixingAngle             :: Fractional a => One a
gravitationalConstant       :: Fractional a => (Cube Metre >/< Kilogram >/< Square Second) a
wienDisplacementLawConstant :: Fractional a => (Metre >*< Kelvin) a
protonMass                  :: Fractional a => Kilogram a
electronRelativeMass        :: Fractional a => One a
fineStructureConstant       :: Fractional a => One a
rydbergConstant             :: Fractional a => (One >/< Metre) a

weakMixingAngle             = SI 2.223e-1          -- (21)
gravitationalConstant       = SI 6.67408e-11       -- (31)
wienDisplacementLawConstant = SI 2.8977729e-3      -- (17)
protonMass                  = SI 1.672621898e-27   -- (21)
fineStructureConstant       = SI 7.2973525664e-3   -- (17)
electronRelativeMass        = SI 5.48579909070e-4  -- (16)
rydbergConstant             = SI 1.0973731568508e7 -- (65)
