!*************************************************************************************************************************
! This module defines some settings used by the variational analysis code.
!
! Tim Hume.
! 18 August 2006.
!*************************************************************************************************************************

MODULE SETTINGS
USE PORTABLE

REAL    (KIND=RK8), PARAMETER   :: PSTART=200.0             ! Where to start upper level filtering. Not currently used.
REAL    (KIND=RK8), PARAMETER   :: PCRIT=200.0              ! 200 hPa OK for the Tropics, no greater than 300 hPa for SGP.

INTEGER (KIND=IK4), PARAMETER   :: NVBUDGET_COLUMN=5
INTEGER (KIND=IK4), PARAMETER   :: NVBUDGET_LAYER=6
INTEGER (KIND=IK4), PARAMETER   :: NTERMMAX=7
INTEGER (KIND=IK4), PARAMETER   :: NTERMMAXV=8
INTEGER (KIND=IK4), PARAMETER   :: NAD=3                    ! This controls which constraints are used in the analysis
                                                            ! 1 = Mass only
                                                            ! 3 = Mass, heat and water.

INTEGER (KIND=IK4), PARAMETER   :: NSTX0=10                 ! The X-size of a grid which covers the network (used in 2D_PUT.F90).
INTEGER (KIND=IK4), PARAMETER   :: NSTY0=9                  ! The Y-size of a grid which covers the network (used in 2D_PUT.F90).

INTEGER (KIND=IK4), PARAMETER   :: NVU=10                   ! Number of upper air variables.
INTEGER (KIND=IK4), PARAMETER   :: NVS= NVU                 ! Number of surface variables.

REAL (KIND=RK8), PARAMETER      :: PT=90.0                  ! Top of the variational analysis (hPa)
INTEGER (KIND=IK4), PARAMETER   :: START_YEAR=2004          ! Start year for the variational analysis.
INTEGER (KIND=IK4), PARAMETER   :: START_MONTH=10          ! Start month for the variational analysis.
INTEGER (KIND=IK4), PARAMETER   :: START_DAY = 1          ! Start day for the variational analysis.
END MODULE SETTINGS
