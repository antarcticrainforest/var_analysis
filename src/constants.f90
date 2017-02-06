!*************************************************************************************************************************
! This module defines some useful constants.
!
! Tim Hume.
! 18 August 2006.
!*************************************************************************************************************************

MODULE CONSTANTS
USE PORTABLE

REAL (KIND=RK8), PARAMETER  :: PI       = 3.14159265
REAL (KIND=RK8), PARAMETER  :: REARTH   = 6371000           ! Radius of the Earth, in metres.
REAL (KIND=RK8), PARAMETER  :: OMEGA    = 7.292E-05         ! Rotational rate of Earth (rad/s) (note: 1 rev/sidereal day)
REAL (KIND=RK8), PARAMETER  :: RD       = 287.04            ! Gas constant for dry air.
REAL (KIND=RK8), PARAMETER  :: RW       = 461.50            ! Gas constant for saturated air.
REAL (KIND=RK8), PARAMETER  :: G        = 9.8               ! Gravitational acceleration, at Earth's surface (m/s^2)
REAL (KIND=RK8), PARAMETER  :: LV0      = 2.5E6
REAL (KIND=RK8), PARAMETER  :: LV       = 2.501E6
REAL (KIND=RK8), PARAMETER  :: LS       = 2.834E6
REAL (KIND=RK8), PARAMETER  :: LS1      = 2.834E6
REAL (KIND=RK8), PARAMETER  :: CPD      = 1005.7            ! Heat capacity of dry air at constant pressure (J/kg/K)
REAL (KIND=RK8), PARAMETER  :: CPV      = 1870.0            ! Heat capacity of water vapour at constant pressure (J/kg/K)
REAL (KIND=RK8), PARAMETER  :: CL       = 4190.0            ! Specific heat capcity of liquid water (J/kg/K)
REAL (KIND=RK8), PARAMETER  :: T0       = 273.15            ! Temperature in Kelvin of 0 Celsius. (K)
REAL (KIND=RK8), PARAMETER  :: CD       = 3.0E-3            ! Skin friction coefficient.

END MODULE CONSTANTS
