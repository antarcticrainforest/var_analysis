!***********************************************************************************************************************************
! PHYSICS module
!
! This module contains several subroutines which calculate various physics. One of the useful things about including these
! subroutines in a module is that we don't need to define explicit interfaces for the subroutines.
!
! Tim Hume.
! 2 October 2006.
!***********************************************************************************************************************************
MODULE PHYSICS
CONTAINS

!***********************************************************************************************************************************
! S_R_TO_T_Z
!
! Converted from the IDL procedure of the same name.
!
! Tim Hume.
! 23 August 2006.
!***********************************************************************************************************************************
SUBROUTINE S_R_TO_T_Z(P, PS, ZS, S, R, T, Z)
USE PORTABLE
USE CONSTANTS

IMPLICIT NONE
REAL (KIND=RK8), DIMENSION(:), INTENT(IN)       :: P    ! Pressures of a vertical column (hPa)
REAL (KIND=RK8), INTENT(IN)                     :: PS   ! Surface pressure (hPa)
REAL (KIND=RK8), INTENT(IN)                     :: ZS   ! Surface geopotential height (m)
REAL (KIND=RK8), DIMENSION(:), INTENT(IN)       :: S    ! (K)
REAL (KIND=RK8), DIMENSION(:), INTENT(IN)       :: R    ! Mixing ration (g/kg)
REAL (KIND=RK8), DIMENSION(:), INTENT(OUT)      :: T    ! Temperature at each level (K)
REAL (KIND=RK8), DIMENSION(:), INTENT(OUT)      :: Z    ! Geopotential height at each level (m)

!
! Local variables.
!
REAL (KIND=RK8)                                 :: G2       ! 2*G
REAL (KIND=RK8)                                 :: EP       ! RD/RW
INTEGER (KIND=IK4)                              :: II       ! Counter
REAL (KIND=RK8)                                 :: RV, A1, P1, T1, R1, Z1, P2, R2

!
! Assign and initialise some variables.
!
G2  = 2.0*G
EP  = RD/RW
T   = 0.0
Z   = 0.0

WHERE (P .GE. PS)
    Z   = ZS
    T   = S(1) - G*ZS/CPD
END WHERE

P1  = PS
T1  = T(1)
R1  = R(1)
Z1  = ZS

DO II=TRANSFER(MAXLOC(P, MASK=P.LT.PS), 0),SIZE(P)
    P2      = P(II)
    R2      = R(II)
    RV      = RD*0.5*((1+R1/EP)/(1+R1)+(1+R2/EP)/(1+R2))
    A1      = RV/2.0/CPD*LOG(P1/P2)
    T(II)   = (S(II) - G/CPD*Z1 - A1*T1)/(1.0 + A1)
    Z(II)   = Z1 + RV*(T1+T(II))/G2*LOG(P1/P2)

    P1      = P2
    R1      = R2
    Z1      = Z(II)
    T1      = T(II)
END DO

END SUBROUTINE S_R_TO_T_Z

!***********************************************************************************************************************************
! T_R_TO_S_Z
!
! This subroutine calculates s and z from p, ps and r.
! It is ported from the IDL procedure with the same name.
!
! Tim Hume.
! 22 September 2006.
!***********************************************************************************************************************************

SUBROUTINE T_R_TO_S_Z(P, PS, ZS, T, R, S, Z)
USE PORTABLE
USE CONSTANTS

IMPLICIT NONE
REAL (KIND=RK8), INTENT(IN)                             :: PS       ! Surface pressure (hPa).
REAL (KIND=RK8), INTENT(IN)                             :: ZS       ! Surface geopotential height (m).
REAL (KIND=RK8), DIMENSION(:), INTENT(IN)               :: P        ! Pressure levels (hPa).
REAL (KIND=RK8), DIMENSION(:), INTENT(IN)               :: T        ! Temperature (K).
REAL (KIND=RK8), DIMENSION(:), INTENT(IN)               :: R        ! Mixing ration (g/kg).
REAL (KIND=RK8), DIMENSION(:), INTENT(OUT)              :: S        ! Dry static energy divided by CPD (K).
REAL (KIND=RK8), DIMENSION(:), INTENT(OUT)              :: Z        ! Geopotential height (m).

!
! Local variables.
!
REAL (KIND=RK8)                                         :: EP, G2, P1, T1, R1, P2, R2, T2, RV
INTEGER (KIND=IK4)                                      :: LL       ! Counter.

!
! Do some basic checks of the input data. This is an unfortunate necessity (otherwise bad input data can cause overflows in the
! calculations. If bad data are detected, set the geopotential height and dry static energy to an undefined value (-9999.99)
!



IF ((PS .LE. 0) .OR. (MINVAL(P) .LE. 0) .OR. (MINVAL(T) .LT. 0) .OR. (MINVAL(R) .LT. 0)) THEN
    S   = -9999.99
    Z   = -9999.99
    RETURN
END IF

!
! Set some variables which are frequently used.
!
EP  = RD/RW
G2  = 2.0*G

!
! Where P is greater than PS, we are presumably below the surface. Set the geopotential height and dry static energy to surface
! values. We also ensure the geopotential height of the lowest level is the same as the surface.
!

Z(1)    = ZS
Z       = 0
S       = 0

WHERE (P .GE. PS)
    Z   = ZS
    S   = T(1) + G*ZS/CPD
END WHERE

!
! For each non-surface level, calculate the geopotential height and dry static energy.
!
P1  = PS
T1  = T(1)
R1  = R(1)
DO LL=MAX(1,TRANSFER(MAXLOC(P, MASK=P.LT.PS), 0)),SIZE(P)
    P2      = P(LL)
    T2      = T(LL)
    R2      = R(LL)
    RV      = RD*0.5*((1+R1/EP)/(1+R1) + (1+R2/EP)/(1+R2))
    IF (LL.gt.1)  Z(LL)   = Z(LL-1) + RV*(T1+T2)/G2*LOG(P1/P2)
    T1      = T2
    P1      = P2
    R1      = R2
    S(LL)   = T(LL) + G*Z(LL)/CPD
END DO


END SUBROUTINE T_R_TO_S_Z

!*******************************************************************************************************************************
! DIVERG
!
! This subroutine computes the divergence of a field.
! It has been ported from the IDL procedure of the same name.
!
! Tim Hume.
! 22 August 2006.
!*******************************************************************************************************************************

SUBROUTINE DIVERG(UNITH, VAR, U, V, DIVU, DIVV, DIV1)
USE PORTABLE

IMPLICIT NONE
REAL (KIND=RK8), DIMENSION(:), INTENT(IN)     :: UNITH      ! One dimensional array with y columns.
REAL (KIND=RK8), DIMENSION(:,:), INTENT(IN)   :: VAR        ! Array with x columns and y rows containing the variable.
REAL (KIND=RK8), DIMENSION(:,:), INTENT(IN)   :: U          ! Array with x columns and y rows containing U-component of wind.
REAL (KIND=RK8), DIMENSION(:,:), INTENT(IN)   :: V          ! Array with x columns and y rows containing V-component of wind.
REAL (KIND=RK8), DIMENSION(:,:), INTENT(IN)   :: DIVU       ! Array with x columns and y rows containing DIVU.
REAL (KIND=RK8), DIMENSION(:,:), INTENT(IN)   :: DIVV       ! Array with x columns and y rows containing DIVV.
REAL (KIND=RK8), DIMENSION(:), INTENT(OUT)    :: DIV1       ! Array with x columns containing divergence.

DIV1    = MATMUL((U*VAR*DIVU + V*VAR*DIVV), UNITH)

END SUBROUTINE DIVERG

!***********************************************************************************************************************************

SUBROUTINE FCORLX(UNITH, NSTU, F, V, FCX1)
USE PORTABLE

IMPLICIT NONE
INTEGER(KIND=IK4), INTENT(IN)                     :: NSTU   ! Number of stations
REAL (KIND=RK8), DIMENSION(NSTU), INTENT(IN)      :: UNITH
REAL (KIND=RK8), DIMENSION(:,:), INTENT(IN)       :: F      ! Coriolis parameter at each station and level.
REAL (KIND=RK8), DIMENSION(:,:), INTENT(IN)       :: V      ! V-wind component at each station and level.
REAL (KIND=RK8), DIMENSION(:), INTENT(OUT)        :: FCX1

FCX1    = MATMUL((V*F/NSTU), UNITH)

END SUBROUTINE FCORLX

!***********************************************************************************************************************************

SUBROUTINE FCORLY(UNITH, NSTU, F, U, FCY1)
USE PORTABLE

IMPLICIT NONE
INTEGER(KIND=IK4), INTENT(IN)                     :: NSTU   ! Number of stations
REAL (KIND=RK8), DIMENSION(NSTU), INTENT(IN)      :: UNITH
REAL (KIND=RK8), DIMENSION(:,:), INTENT(IN)       :: F      ! Coriolis parameter at each station and level.
REAL (KIND=RK8), DIMENSION(:,:), INTENT(IN)       :: U      ! U-wind component at each station and level.
REAL (KIND=RK8), DIMENSION(:), INTENT(OUT)        :: FCY1

FCY1    = MATMUL((-U*F/NSTU), UNITH)

END SUBROUTINE FCORLY

!***********************************************************************************************************************************

SUBROUTINE FPGD(UNITH, Z, DZDX, FP1)
USE PORTABLE
USE CONSTANTS

IMPLICIT NONE
REAL (KIND=RK8), DIMENSION(:), INTENT(IN)     :: UNITH
REAL (KIND=RK8), DIMENSION(:,:), INTENT(IN)   :: Z
REAL (KIND=RK8), DIMENSION(:,:), INTENT(IN)   :: DZDX
REAL (KIND=RK8), DIMENSION(:), INTENT(OUT)    :: FP1

FP1 = MATMUL((-G*Z*DZDX), UNITH)

END SUBROUTINE FPGD

!*******************************************************************************************************************************
! HEIGHT
!
! This subroutine computes the geopotential height.
! It has been ported from the IDL procedure of the same name.
!
! Tim Hume.
! 21 August 2006.
!*******************************************************************************************************************************

SUBROUTINE HEIGHT(NP, KS, KT, P, T, R, Z)
USE PORTABLE
USE CONSTANTS

IMPLICIT NONE

INTEGER (KIND=IK4), INTENT(IN)                :: NP         ! Number of pressure levels in the P, T, R and Z arrays.
INTEGER (KIND=IK4), INTENT(IN)                :: KS         ! The level which the surface is at.
INTEGER (KIND=IK4), INTENT(IN)                :: KT         ! Level at which the variational analysis stops.
REAL (KIND=RK8), DIMENSION(NP), INTENT(IN)    :: P          ! Pressure of each level.
REAL (KIND=RK8), DIMENSION(NP), INTENT(IN)    :: T          ! Temperature of each level.
REAL (KIND=RK8), DIMENSION(NP), INTENT(IN)    :: R          ! Mixing ratio at each level?
REAL (KIND=RK8), DIMENSION(NP), INTENT(OUT)   :: Z          ! Geopotential height.

!
! Local variables.
!
REAL (KIND=RK8)                               :: EP, R1, DZ ! Temporary variables.
INTEGER (KIND=IK4)                            :: KK         ! Counter.

EP  = RD/RW

DO KK=KS+1,KT
    R1      = RD*0.5*((1+R(KK-1)/EP)/(1+R(KK-1))+(1+R(KK)/EP)/(1+R(KK)))
    DZ      = R1/G*0.5*(T(KK-1)/P(KK-1)+T(KK)/P(KK))*(P(KK-1)-P(KK))
    Z(KK)   = Z(KK-1)+DZ
END DO

END SUBROUTINE HEIGHT

!*******************************************************************************************************************************
! CALHT2
!
! This subroutine computes ...
! It has been ported from the IDL procedure of the same name.
!
! Tim Hume.
! 14 September 2006.
!*******************************************************************************************************************************

SUBROUTINE CALHT2(PS, ZS, N, P, TC, R, HT, DEW)
USE PORTABLE
USE CONSTANTS

IMPLICIT NONE
REAL (KIND=RK8), INTENT(IN)                       :: PS     ! Surface pressure (units).
REAL (KIND=RK8), INTENT(IN)                       :: ZS     ! Surface geopotential (units).
INTEGER (KIND=IK4), INTENT(IN)                    :: N      ! Number of vertical levels.
REAL (KIND=RK8), DIMENSION(N), INTENT(IN)         :: P      ! Pressures (units).
REAL (KIND=RK8), DIMENSION(N), INTENT(IN)         :: TC     ! Temperatures (Celsius).
REAL (KIND=RK8), DIMENSION(N), INTENT(INOUT)      :: R      ! Relative humidity (%).
REAL (KIND=RK8), DIMENSION(N), INTENT(OUT)        :: HT     ! Geopotential height (units).
REAL (KIND=RK8), DIMENSION(N), INTENT(OUT)        :: DEW    ! Dew-point temperature? (units).

!
! Local variables.
!
REAL (KIND=RK8), DIMENSION(N)                     :: RH     ! Relative humidity converted to 0-1 range.
REAL (KIND=RK8), DIMENSION(N)                     :: T      ! Temperature in Kelvin.
REAL (KIND=RK8), DIMENSION(N)                     :: ES     ! Saturation vapour pressure.
REAL (KIND=RK8), DIMENSION(N)                     :: RS     ! Saturated mixing ratio.
REAL (KIND=RK8)                                   :: EP     ! = RD/RW
INTEGER (KIND=IK4)                                :: KS     ! Index of first vertical level at or above the surface.
REAL (KIND=RK8)                                   :: R1, DZ

!
! Convert the relative humidity to the 0-1 range. Any humidities less than 0 or greater than 1 are set to 1. Not sure of the
! reasoning behind making RH<0 go to 1 ... maybe missing data is better substituted with 1 RH than some other value?
!
RH  = R/100.0               
WHERE ((RH .LT. 0.0) .OR. (RH .GT. 1.0)) RH = 1.0

T   = TC + T0               ! Convert temperatures in Celsius to Kelvin.

!
! Calculate the saturation vapour pressure using a modified form of the Clausius-Clayperon equation. I believe this equation might
! be in Emanuel (1994): "Atmospheric Convection" (maybe).
!
ES  = 1.003*EXP(53.67957 - 6743.769/T - 4.8451*LOG(T))
EP  = RD/RW                 ! This will be used several times in this subroutine (so save resources by only calculating it once).

RS  = EP*ES/(P-ES)          ! Calculate the saturated mixing ratio.

!
! Overwrite the input relative humidities (R, in %) with mixing ratio.
!
R   = RS*RH

!
! Locate the first level, at or above the surface (which has pressure PS). Default to 1 if we can't find the surface.
!
KS  = MAX(TRANSFER(MAXLOC(P, MASK = P .LE. PS), 0),1)

!
! Work out the geopotential height of the first level. I added a little bit of code to deal with the situation where KS=1. The IDL
! code did not seem to deal with this, assuming that there was always at least one pressure level "below the surface", and did a
! simple interpolation. If there is no level below the surface, can't do interpolation.
!
R1      = RD*0.5*((1+R(MAX(1,KS-1))/EP)/(1+R(MAX(1,KS-1))) + (1+R(KS)/EP)/(1+R(KS)))
DZ      = R1/G*0.5*(T(MAX(1,KS-1))/P(MAX(1,KS-1)) + T(KS)/P(KS)) * (PS/P(KS))
HT(KS)  = ZS + DZ

!
! Work out the geopotential height of all the vertical levels above the surface.
!
CALL HEIGHT(N, KS, N, P, T, R, HT)

!
! Assign the surface geopotential height to all levels below the surface.
!
HT(1:MAX(1,KS-1))  = ZS

END SUBROUTINE CALHT2

!***********************************************************************************************************************************
! CALC_STATE2
!
! This subroutine calculates various thermodynamic properties of the atmosphere.
! It is a port of the IDL procedure with the same name.
!
! Tim Hume.
! 23 August 2006.
!***********************************************************************************************************************************

SUBROUTINE CALC_STATE2(P, T, TD, E, Z, R, RH, H, HDRY, S)
USE PORTABLE
USE CONSTANTS

IMPLICIT NONE
REAL (KIND=RK8), INTENT(IN)     :: P    ! Pressure (Pa) - check units.
REAL (KIND=RK8), INTENT(IN)     :: T    ! Temperature (K).
REAL (KIND=RK8), INTENT(IN)     :: TD   ! Dew point temperature (K).
REAL (KIND=RK8), INTENT(IN)     :: E    ! Vapour pressure (Pa - check units.
REAL (KIND=RK8), INTENT(IN)     :: Z    ! Geopotential height (m) - check units.
REAL (KIND=RK8), INTENT(OUT)    :: R    ! Mixing ratio.
REAL (KIND=RK8), INTENT(IN)     :: RH   ! Relative humidity (decimal fraction between 0 and 1). 
REAL (KIND=RK8), INTENT(OUT)    :: H    !
REAL (KIND=RK8), INTENT(OUT)    :: HDRY !
REAL (KIND=RK8), INTENT(OUT)    :: S    !

!
! Local variables.
!
REAL (KIND=RK8)                 :: EP, ES, RS, LV1

EP      = RD/RW
ES      = 1.003*EXP(53.67957 - 6743.769/T - 4.8451*LOG(T))  ! An empirical formula for the saturation vapour pressure.
RS      = EP*ES/(P-ES)
R       = RS*RH
LV1     = LV0 + (CPV-CL)*(T-T0)
H       = (CPD + R*CL)*T + LV1*R + (1.0 + R)*G*Z
HDRY    = CPD*T + G*Z
S       = CPD*LOG(T) - RD*LOG(P) + LV0*R/T - R*RW*LOG(RH)
END SUBROUTINE CALC_STATE2

END MODULE PHYSICS
