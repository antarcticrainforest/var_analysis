!***********************************************************************************************************************************
! NUMERICS module
!
! This module contains subroutines which do things such as interpolation.
!
! Tim Hume.
! 2 October 2006.
!***********************************************************************************************************************************
MODULE NUMERICS
CONTAINS

!********************************************************************************************************************************
! ITPS
!
! This subroutine interpolates surface pressure level data to the level where the surface is. This should not usually be
! necessary, because ideally the level where the surface pressure is and the level where the surface is should be the same.
!
! Tim Hume.
! 22 August 2006.
!********************************************************************************************************************************

SUBROUTINE ITPS(KS, KB, D, DS)
USE PORTABLE

IMPLICIT NONE

INTEGER (KIND=IK4), INTENT(IN)              :: KS   ! Level where the surface pressure is.
INTEGER (KIND=IK4), INTENT(IN)              :: KB   ! Level where the actual surface is.
REAL (KIND=RK8), DIMENSION(:),INTENT(INOUT) :: D    ! Array to hold interpolated data.
REAL (KIND=RK8), INTENT(IN)                 :: DS   ! Value of D at the level where the surface pressure is.

!
! Local variables.
!
INTEGER (KIND=IK4)                          :: KK   ! Counter.

IF (KS .LT. KB) THEN
    !
    ! If we enter this loop, the surface pressure level is lower than the level where the surface is. No other case
    ! (apart from the ideal one, where the surface pressure and surface levels are the same) is considered.
    !
    D(KS)       = DS
    DO KK=KS+1,KB-1
        D(KK)   = DS + (D(KB) - DS)/(KB - KS)*(KK - KS)
    END DO
END IF

END SUBROUTINE ITPS

!***********************************************************************************************************************************
! GMEAN
!
! This subroutine calculates the spatial average of data on the variational analysis grid. It optionally does time smoothing.
! The subroutine has been ported from the IDL procedure with the same name.
!
! Tim Hume.
! 10 October 2006.
!***********************************************************************************************************************************

SUBROUTINE GMEAN(MG, DP, NV, NST, NT, TSMOOTH)
USE PORTABLE

IMPLICIT NONE
INTEGER (KIND=IK4), INTENT(IN)                      :: NT           ! Number of time steps.
INTEGER (KIND=IK4), INTENT(IN)                      :: TSMOOTH      ! 0 = no smoothing.
INTEGER (KIND=IK4), INTENT(IN)                      :: NV           ! Number of variables (only the last variable is averaged).
INTEGER (KIND=IK4), INTENT(IN)                      :: NST          ! Number of stations in the analysis grid.
REAL (KIND=RK8), DIMENSION(NT), INTENT(OUT)         :: MG           ! The spatial average of DP is stored in this array.
REAL (KIND=RK8), DIMENSION(NV,NST,NT), INTENT(IN)   :: DP           ! Data to be averaged, and possibly time smoothed.

MG  = SUM(DP(NV,:,:), DIM=1)/NST        ! Average the last variable along the station dimension (DIM=1, because the rank of the
                                        ! array subsection is only two (the variable dimension "collapses" when the subsection
                                        ! is extracted)).
IF (TSMOOTH .GT. 0) THEN
    CALL SMOOTH(MG, SIZE(MG), TSMOOTH, .FALSE.)
END IF

END SUBROUTINE GMEAN

!***********************************************************************************************************************************
! SMOOTH
!
! This subroutine applies a box car filter to a one dimensional array. It allows the option of cyclic data (where the last element
! in the data array is adjacent to the first element).
!
! Tim Hume.
! 4 September 2006.
!***********************************************************************************************************************************
SUBROUTINE SMOOTH(ARRAY, N, FILTER_WIDTH, CYCLIC)
USE PORTABLE

IMPLICIT NONE
INTEGER (KIND=IK4), INTENT(IN)                      :: N            ! Number of elements in ARRAY.
INTEGER (KIND=IK4), INTENT(IN)                      :: FILTER_WIDTH ! Width of the box car filter.
REAL (KIND=RK8), DIMENSION(N), INTENT(INOUT)        :: ARRAY        ! The array of data being filtered.
LOGICAL, INTENT(IN)                                 :: CYCLIC       ! Set to .TRUE. if data are cyclic.

!
! Local variables.
!
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE          :: TMPARRAY1,TMPARRAY2     ! Temporary array used for filtering.
INTEGER (KIND=IK4)                                  :: TMPWIDTH     ! Filter width.
INTEGER (KIND=IK4)                                  :: MEMST        ! Status code from memory allocation functions.
INTEGER (KIND=IK4)                                  :: II           ! Counter.

!
! First check that (i) the filter width is not too large, and (ii) the filter width is an odd number. If the filter width is an even
! number, we set a temporary filter width which is one greater than the value passed to the subroutine.
!
IF (MOD(FILTER_WIDTH, 2) .EQ. 0) THEN
    TMPWIDTH    = FILTER_WIDTH + 1
ELSE
    TMPWIDTH    = FILTER_WIDTH
END IF

IF (TMPWIDTH .GT. N) THEN
    PRINT *,'W: Setting filter width to be the same as the number of points in array, ',N
    TMPWIDTH    = N
END IF


!
! Allocate a temporary array to be used with filtering. If the data are cyclic, then this array is slightly larger, to allow
! us to nicely handle data at the beginning and ends of the array.
!
IF (CYCLIC) THEN
    ALLOCATE(TMPARRAY1(N+TMPWIDTH-1), TMPARRAY2(N+TMPWIDTH-1), STAT=MEMST)
    IF (MEMST .NE. 0) THEN
        PRINT *,'Not able to allocate memory for the temporary filtering array'
        STOP '1'
    END IF
    TMPARRAY1(1:TMPWIDTH/2)                  = ARRAY(N-TMPWIDTH/2+1:N)   ! I think this still works when TMPWIDTH/2 = 0
    TMPARRAY1(TMPWIDTH/2+1:TMPWIDTH/2+N)     = ARRAY(1:N)
    TMPARRAY1(TMPWIDTH/2+N+1:TMPWIDTH+N-1)   = ARRAY(1:TMPWIDTH/2)
ELSE
    ALLOCATE(TMPARRAY1(N), TMPARRAY2(N), STAT=MEMST)
    IF (MEMST .NE. 0) THEN
        PRINT *,'Not able to allocate memory for the temporary filtering array'
        STOP '1'
    END IF
    TMPARRAY1    = ARRAY
END IF

!
! Now do the box car filtering.
!
DO II=1,SIZE(TMPARRAY2)
        TMPARRAY2(II)   = SUM(TMPARRAY1(MAX(1, II-TMPWIDTH/2):MIN(II+TMPWIDTH/2, SIZE(TMPARRAY2))))/TMPWIDTH
END DO

ARRAY(1:N)  = TMPARRAY2(TMPWIDTH/2+1:TMPWIDTH/2+N)

!
! Deallocate allocated memory.
!
IF (ALLOCATED(TMPARRAY1))    DEALLOCATE(TMPARRAY1)
IF (ALLOCATED(TMPARRAY2))    DEALLOCATE(TMPARRAY2)

END SUBROUTINE SMOOTH

!***********************************************************************************************************************************
! CALDEV
!
! This subroutine calculated the RMSE of the input variables (in the DU array). The subroutine has been ported from the IDL
! procedure with the same name.
!
! Tim Hume.
! 23 August 2006.
!***********************************************************************************************************************************

SUBROUTINE CALDEV(DU, KT, KS, NSTU, NTU, NP, NVU, DEVM2)
USE PORTABLE
USE CONSTANTS

INTEGER (KIND=IK4), INTENT(IN)                          :: KT       ! Top level for the variational analysis.
INTEGER (KIND=IK4), INTENT(IN)                          :: KS       ! Surface level for the variational analysis.
INTEGER (KIND=IK4), INTENT(IN)                          :: NSTU     ! Number of stations.
INTEGER (KIND=IK4), INTENT(IN)                          :: NTU      ! Number of times.
INTEGER (KIND=IK4), INTENT(IN)                          :: NP       ! Number of pressure levels.
INTEGER (KIND=IK4), INTENT(IN)                          :: NVU      ! Number of variables.
REAL (KIND=RK8), DIMENSION(NVU,NP,NSTU,NTU), INTENT(IN) :: DU       ! Holds the data (see assim.f90 for a description of the array)
REAL (KIND=RK8), DIMENSION(NVU,NP)                      :: DEVM2    ! Holds the RMSE and other stuff.

!
! Local variables.
!
REAL (KIND=RK8), DIMENSION(NVU,NP)                      :: DEVM     ! Holds the time mnd station ean of the variables.
REAL (KIND=RK8), DIMENSION(NVU,NP,NSTU,NTU)             :: DEV      ! The difference between the variables and the mean (DEVM)

INTEGER (KIND=IK4)                                      :: II, IST  !  Counters

!
! Here we calculate the mean of the five variables accross the stations at each level. When calculating the mean, neglect the
! start and end times.
!
DEVM(2:6,KS:KT) = SUM(SUM(DU(2:6,KS:KT,:,2:NTU-1),DIM=4),DIM=3)/NSTU/(NTU-2)

!
! Now calculate the difference from the mean. This is easiest to do in a traditional loop.
!
DEV                 = 0.0
DO II=2,NTU-1
    DO IST=1,NSTU
        DEV(:,:,IST,II) = DU(:,:,IST,II) - DEVM(:,:)
    END DO
END DO

!
! Finally, calculate the RMSE.
DEVM2               = DEVM
DEVM2(2:6,KS:KT)    = SQRT(SUM(SUM(DEV(2:6,KS:KT,:,2:NTU-1)**2,DIM=4),DIM=3)/NSTU/(NTU-2))

DEVM2(2,:)          = DEVM2(2,:)/LV0*CPD    ! The other stuff mentioned above.

END SUBROUTINE CALDEV

!*************************************************************************************************************************
! This subroutine has been converted from the IDL procedure of the same name.
!
! I'm not totally sure about how it works, but here is my best guess:
!
! The subroutine appears to calculate gradient and divergence components for the array of stations. These will then be 
! used later to calculate the actual gradients and divergences of various fields, without the need to re-solve the 
! various equations that are solved in this subroutine (which are reasonably computationally expensive).
!
! Tim Hume.
! 18 August 2006.
!*************************************************************************************************************************

SUBROUTINE HORIZONTAL_FIELD(NST, LON, LAT, X, Y, F, DZDX, DZDY, DIVU, DIVV)
USE CONSTANTS

IMPLICIT NONE
INTEGER (KIND=IK4), INTENT(IN)                :: NST          ! The number of stations in the array.
REAL (KIND=RK8), DIMENSION(NST), INTENT(IN)   :: LON          ! The longitudes of each station.
REAL (KIND=RK8), DIMENSION(NST), INTENT(IN)   :: LAT          ! The latitudes of each station.
REAL (KIND=RK8), DIMENSION(NST), INTENT(OUT)  :: X            ! The cartesian x-coordinates of each station.
REAL (KIND=RK8), DIMENSION(NST), INTENT(OUT)  :: Y            ! The cartesian y-coordinates of each station.
REAL (KIND=RK8), DIMENSION(NST), INTENT(OUT)  :: F            ! Coriolis parameter at each station.
REAL (KIND=RK8), DIMENSION(NST), INTENT(OUT)  :: DZDX         !
REAL (KIND=RK8), DIMENSION(NST), INTENT(OUT)  :: DZDY         !
REAL (KIND=RK8), DIMENSION(NST), INTENT(OUT)  :: DIVU         !
REAL (KIND=RK8), DIMENSION(NST), INTENT(OUT)  :: DIVV         !

!
! Local variables.
!
REAL (KIND=RK8), DIMENSION(NST)               :: Z1
REAL (KIND=RK8), DIMENSION(NST+2)             :: X1, Y1               ! Holds coordinates of next station in the "loop".
INTEGER (KIND=IK4)                            :: II                   ! Counter.
REAL (KIND=RK8)                               :: AX, AY, BX, BY, C3   ! Temporary variables.
REAL (KIND=RK8)                               :: AREA                 ! Area of sounding array.

!
! Calculate the cartesian co-ordinates of each station, and the coriolis parameter at each station.
!

X   = 2.0*SIN((LON-LON(1))/360.0*2*PI/2.0)*REARTH*COS(LAT/180.0*PI)
Y   = 2.0*SIN((LAT-LAT(1))/360.0*2*PI/2.0)*REARTH
F   = 2.0*OMEGA*SIN(LAT/180.0*PI)

!
! Calculate the gradient terms. We do this by fitting a plane of best fit to the points.
!
Z1  = 0
DO II=1,NST
    Z1(II)   = 1.0
    CALL LINE_FIT_XYZ(NST, X, Y, Z1, DZDX(II), DZDY(II), C3)
    Z1(II)   = 0.0
END DO

!
! Set X1 and Y1 to contain the coordinates of the next station in the "loop"
!
DO II=1,NST
    X1(II+1)    = X(II)
    Y1(II+1)    = Y(II)
END DO
X1(1)       = X1(NST+1)
Y1(1)       = Y1(NST+1)
X1(NST+2)   = X1(2)
Y1(NST+2)   = Y1(2)

!
! Calculate the area of the sounding array.
!
AREA        = 0.0
DO II=2,NST-1
    AX      = X(1) - X(II)
    AY      = Y(1) - Y(II)
    BX      = X(1) - X(II+1)
    BY      = Y(1) - Y(II+1)
    AREA    = AREA + ABS(AX*BY - AY*BX)/2.0
END DO

!
! Now calculate the divergence terms using the line integral method (see Davies-Jones (1993) equations 8-10)
!
DO II=1,NST
    DIVU(II)    = (Y1(II+2) - Y1(II))/2.0/AREA
    DIVV(II)    = -(X1(II+2) - X1(II))/2.0/AREA
END DO

END SUBROUTINE HORIZONTAL_FIELD

!*******************************************************************************************************************************
! line_fit_xyz
!
! This subroutine has been converted from the IDL version to F90.
! What it does is find the least squares fit plane through a number of points and returns the slope of the plane 
! (in the x and y directions), and the z-value where it passes through (x, y) = (0, 0).
!
! Tim Hume.
! 16 August 2006.
!*******************************************************************************************************************************

SUBROUTINE LINE_FIT_XYZ(N,X,Y,Z,DZDX,DZDY,Z0)
USE PORTABLE
USE LU

IMPLICIT NONE

INTEGER (KIND=IK4), INTENT(IN)              :: N            ! The number of points
REAL (KIND=RK8), DIMENSION(N), INTENT(IN)   :: X            ! The cartesian x-coordinates of each point
REAL (KIND=RK8), DIMENSION(N), INTENT(IN)   :: Y            ! The cartesian y-coordinates of each point
REAL (KIND=RK8), DIMENSION(N), INTENT(IN)   :: Z            ! The z-value at each point
REAL (KIND=RK8), INTENT(OUT)                :: DZDX         ! Slope of the plane in the x-direction
REAL (KIND=RK8), INTENT(OUT)                :: DZDY         ! Slope of the plane in the y-direction
REAL (KIND=RK8), INTENT(OUT)                :: Z0           ! z-value at (x,y) = (0,0)

!
! Local variables.
!
REAL (KIND=RK8), DIMENSION(3,3)             :: A 
REAL (KIND=RK8), DIMENSION(3)               :: B
INTEGER (KIND=IK4), DIMENSION(3)            :: INDX
INTEGER (KIND=IK4)                          :: CODE, D

!
! By solving the equation    | sum(x^2)  sum(xy)     sum(x)  |     | sum(xz) |
!                            | sum(xy)   sum(y^2)    sum(y)  | X = | sum(yz) |
!                            | sum(x)    sum(y)      N       |     | sum(z)  |
!
! for X, we can find the slopes DZDX, DZDY aand the intercept Z0 (all defined above) for the plane of best fit:
!
!     | DZDX |
! X = | DZDY |
!     |  Z0  |
!
! Whoever worked this out was pretty clever.
!

A = RESHAPE(SOURCE=(/   SUM(X*X), SUM(X*Y), SUM(X), &
&                       SUM(X*Y), SUM(Y*Y), SUM(Y), &
&                       SUM(X),   SUM(Y),   REAL(N, RK8) /), SHAPE=(/ 3, 3 /))

B = (/ SUM(X*Z), SUM(Y*Z), SUM(Z) /)

!
! These calls solve the matrix equation AX=B. First, we calculate the LU decomposition of A using the LUDCMP
! subroutine, then we solve for X using the LUBKSB subroutine. Details on the maths behind all this can be found
! from many sources.
!
CALL LUDCMP(A, 3, INDX, D, CODE)
IF (CODE .EQ. 1) THEN
    PRINT *,'W: Tried to do a LU decomposition on a singular matrix.'
    PRINT *,'   This code is not clever enough to handle this case.'
    STOP '1'
END IF
CALL LUBKSB(A, 3, INDX, B)

DZDX = B(1)
DZDY = B(2)
Z0   = B(3)

END SUBROUTINE LINE_FIT_XYZ

!*******************************************************************************************************************************
! WINDOWN
!
! This subroutine finds the index of the vertical level which includes the X1 (an argument to the subroutine) level.
! It has been ported from the IDL procedure of the same name.
!
! Tim Hume.
! 20 September 2006.
!*******************************************************************************************************************************

SUBROUTINE WINDOWN(N, X, DX, X1, L0, L1)
USE PORTABLE

IMPLICIT NONE
INTEGER (KIND=IK4), INTENT(IN)                  :: N        ! Number of vertical layers.
REAL (KIND=RK8), DIMENSION(N), INTENT(IN)       :: X        ! Array containing the levels of each layer.
REAL(KIND=RK8), INTENT(IN)                      :: DX       ! Layer depth (assume layers are equal depth).
REAL (KIND=RK8), INTENT(IN)                     :: X1       ! We are searching for the layer which includes this level.
INTEGER (KIND=IK4), INTENT(IN)                  :: L0       ! The bottom layer to start searching at.
INTEGER (KIND=IK4), INTENT(OUT)                 :: L1       ! The index of the layer which contains the X1 level.

!
! Local variables.
!
REAL (KIND=RK8)                                 :: XA, XB   ! Top and bottom of the vertical layer.
INTEGER (KIND=IK4)                              :: LL       ! Layer number.

L1      = -1
LL      = L0
DO WHILE ((LL .LE. N) .AND. (L1 .EQ. -1))
    XA  = X(LL) - DX*(0.5+0.1)
    XB  = X(LL) + DX*(0.5-0.1)
    IF ((X1 .GE. XA) .AND. (X1 .LT. XB)) L1 = LL
    LL  = LL + 1
END DO

END SUBROUTINE WINDOWN

!**************************************************************************************************************************
! This monster subroutine was converted from the IDL procedure with the same name. It forms the main part of the
! variational analysis code. I have done my best to comment the code (often guessing).
!
! This is messy code ... it is basically a direct translation from the IDL to Fortran 90.
! I've tried to simplify a few things, making use of some Fortran 90 features.
!
! Tim Hume.
! 18 August 2006.
!**************************************************************************************************************************

SUBROUTINE ASSIM(NP, DP, P, KS, KB, KT, NSTU, LONU, LATU, DU, NSTS, LONS, LATS, DSS, &
&   DIVBVAR, FCX, FCY, FPX, FPY, BUDGET, NADVAR, WVAR, DUS)
USE PORTABLE
USE CONSTANTS
USE SETTINGS
USE LU
USE PHYSICS

IMPLICIT NONE

INTEGER (KIND=IK4), INTENT(IN)                                      :: NP       ! Number of pressure levels.
REAL (KIND=RK8), INTENT(IN)                                         :: DP       ! Spacing of the pressure levels (Pa).
REAL (KIND=RK8), DIMENSION(NP), INTENT(IN)                          :: P        ! Pressure of each level (hPa)
INTEGER (KIND=IK4), INTENT(IN)                                      :: KS       ! Index of level where the surface pressure is.
INTEGER (KIND=IK4), INTENT(IN)                                      :: KB       ! Index of level where the surface is.
INTEGER (KIND=IK4), INTENT(IN)                                      :: KT       ! Index of the top level.
INTEGER (KIND=IK4), INTENT(IN)                                      :: NSTU     ! Number of upper air stations.
REAL (KIND=RK8), DIMENSION(NSTU), INTENT(IN)                        :: LONU     ! Longitudes of the upper air stations
REAL (KIND=RK8), DIMENSION(NSTU), INTENT(IN)                        :: LATU     ! Latitudes of the upper air stations
REAL (KIND=RK8), DIMENSION(NVU,NP,NSTU), INTENT(INOUT)              :: DU       ! Array containing all the upper level data.
                                                                                ! DU(1,:,:)   = 1.0
                                                                                ! DU(2,:,:)   = rLv0/cpd
                                                                                ! DU(3,:,:)   = dry static energy
                                                                                ! DU(4,:,:)   = u
                                                                                ! DU(5,:,:)   = v
                                                                                ! DU(6,:,:)   = T
                                                                                ! DU(7,:,:)   = z
                                                                                ! DU(8,:,:)   = P
                                                                                ! DU(9,:,:)   = lon
                                                                                ! DU(10,:,:)  = lat
INTEGER (KIND=IK4), INTENT(IN)                                      :: NSTS     ! Number of surface level stations
REAL (KIND=RK8), DIMENSION(NSTS), INTENT(IN)                        :: LONS     ! Longitudes of the surface stations.
REAL (KIND=RK8), DIMENSION(NSTS), INTENT(IN)                        :: LATS     ! Latitudes of the surface stations.
REAL (KIND=RK8), DIMENSION(NVS, NSTS), INTENT(INOUT)                :: DSS      ! Array containing all the surface level data.
REAL (KIND=RK8), DIMENSION(NVBUDGET_LAYER,NP), INTENT(OUT)          :: DIVBVAR  !
REAL (KIND=RK8), DIMENSION(NP), INTENT(OUT)                         :: FCX      !
REAL (KIND=RK8), DIMENSION(NP), INTENT(OUT)                         :: FCY      !
REAL (KIND=RK8), DIMENSION(NP), INTENT(OUT)                         :: FPX      !
REAL (KIND=RK8), DIMENSION(NP), INTENT(OUT)                         :: FPY      !
REAL (KIND=RK8), DIMENSION(NVBUDGET_COLUMN, NTERMMAX), INTENT(INOUT):: BUDGET   !
INTEGER (KIND=IK4), INTENT(IN)                                      :: NADVAR   !
REAL (KIND=RK8), DIMENSION(5,NP, NSTU), INTENT(INOUT)               :: WVAR     !
REAL (KIND=RK8), DIMENSION(NVU,NP,NSTU), INTENT(OUT)                :: DUS      !

!
! Local variables.
!
REAL (KIND=RK8), DIMENSION(NP, NSTU)                :: F                ! Coriolis parameter at each station and level.
REAL (KIND=RK8), DIMENSION(NP, NSTU)                :: DZDX             ! 
REAL (KIND=RK8), DIMENSION(NP, NSTU)                :: DZDY             !
REAL (KIND=RK8), DIMENSION(NP, NSTU)                :: DIVU             !
REAL (KIND=RK8), DIMENSION(NP, NSTU)                :: DIVV             !

REAL (KIND=RK8), DIMENSION(NSTU)                    :: X                ! Cartesian x-coords of the stations at a single level.
REAL (KIND=RK8), DIMENSION(NSTU)                    :: Y                ! Cartesian y-coords of the stations at a single level.
REAL (KIND=RK8), DIMENSION(NSTS)                    :: XS               ! Cartesian x-coords of the surface stations.
REAL (KIND=RK8), DIMENSION(NSTS)                    :: YS               ! Cartesian y-coords of the surface stations.
REAL (KIND=RK8), DIMENSION(1,NSTS)                  :: FS               ! Coriolis parameters of the surface stations.
REAL (KIND=RK8), DIMENSION(1,NSTS)                  :: DZDXS            ! DZDX of the surface stations.
REAL (KIND=RK8), DIMENSION(1,NSTS)                  :: DZDYS            ! DZDY of the surface stations.
REAL (KIND=RK8), DIMENSION(1,NSTS)                  :: DIVUS            ! DIVU of the surface stations.
REAL (KIND=RK8), DIMENSION(1,NSTS)                  :: DIVVS            ! DIVV of the surface stations.

INTEGER (KIND=IK4)                                  :: KK, MM, IST, MB, MB1, MVAR, MAD   ! Counters.
REAL (KIND=RK8)                                     :: TWO      = 2.0
REAL(KIND=RK8), DIMENSION(5)                        :: AD       = 1.0   !
INTEGER (KIND=IK4)                                  :: NTERM            !

! The IDL code is messy ... these were all declared for the first time in the middle of other stuff.
! For safety, we initialise many of the variables to 0 (unless the IDL code specifies otherwise). Actually, if you look at
! the accompanying make files, you'll see I use a g95 option for initially zeroing variables when they are declared.

REAL (KIND=RK8), DIMENSION(NSTU)                    :: UNITH
REAL (KIND=RK8), DIMENSION(NSTS)                    :: UNITHS
REAL (KIND=RK8), DIMENSION(NP)                      :: UNITV
REAL (KIND=RK8), DIMENSION(NP)                      :: UNITV1
REAL (KIND=RK8), DIMENSION(NVBUDGET_COLUMN)         :: C
REAL (KIND=RK8), DIMENSION(NVBUDGET_COLUMN)         :: B
REAL (KIND=RK8), DIMENSION(NVBUDGET_COLUMN, NVBUDGET_COLUMN, NP, NSTU)    :: DBDVAR   ! Important one.
REAL (KIND=RK8), DIMENSION(NVBUDGET_COLUMN, NVBUDGET_COLUMN)              :: A
REAL (KIND=RK8), DIMENSION(NVBUDGET_COLUMN)                               :: LD

REAL (KIND=RK8), DIMENSION(NP, NSTU)                :: BVAR
REAL (KIND=RK8), DIMENSION(NP, NSTU)                :: UNITVAR                      ! This is called UNIT in the IDL code.
REAL (KIND=RK8), DIMENSION(NP, NSTU)                :: U
REAL (KIND=RK8), DIMENSION(NP, NSTU)                :: V
REAL (KIND=RK8), DIMENSION(NP, NSTU)                :: R
REAL (KIND=RK8), DIMENSION(NP, NSTU)                :: H
REAL (KIND=RK8), DIMENSION(NP, NSTU)                :: Z 
REAL (KIND=RK8), DIMENSION(NP, NSTU)                :: T

REAL (KIND=RK8), DIMENSION(NP, NSTU)                :: BVAR1
REAL (KIND=RK8), DIMENSION(NP, NSTU)                :: U1
REAL (KIND=RK8), DIMENSION(NP, NSTU)                :: V1
REAL (KIND=RK8), DIMENSION(NP)                      :: R2
REAL (KIND=RK8), DIMENSION(NP)                      :: H2
REAL (KIND=RK8), DIMENSION(NP)                      :: Z2
REAL (KIND=RK8), DIMENSION(NP, NSTU)                :: Z1

REAL (KIND=RK8), DIMENSION(1, NSTS)                 :: BVARS
REAL (KIND=RK8), DIMENSION(1, NSTS)                 :: US
REAL (KIND=RK8), DIMENSION(1, NSTS)                 :: VS
REAL (KIND=RK8), DIMENSION(1, NSTS)                 :: ZS
REAL (KIND=RK8), DIMENSION(1, NSTS)                 :: TS
INTEGER (KIND=IK4), DIMENSION(4)                    :: MADN = (/ 4, 5, 2, 3 /) 

REAL (KIND=RK8), DIMENSION(NAD, NAD)                :: A1
!REAL (KIND=RK8), DIMENSION(NAD, NAD)                :: A2
REAL (KIND=RK8), DIMENSION(NAD)                     :: B1, LD1
REAL (KIND=RK8), DIMENSION(NP)                      :: DIV1, DIV2               ! Divergences, for NP levels.
REAL (KIND=RK8), DIMENSION(1)                       :: DIVS                     ! Divergence for a single level.
REAL (KIND=RK8), DIMENSION(NP)                      :: FCX1, FPX1, FCY1, FPY1
REAL (KIND=RK8), DIMENSION(1)                       :: FCXS, FCYS, FPXS, FPYS
INTEGER (KIND=IK4)                                  :: D, CODE
INTEGER (KIND=IK4), DIMENSION(NAD)                  :: INDX
REAL (KIND=RK8), DIMENSION(NP)                      :: SC, RC
REAL (KIND=RK8), DIMENSION(NP,NSTU)                      :: OLD1, OLD2,test1,test2               ! Divergences, for NP levels.

!
! What a lot of stuff that was ... surely it can be reduced!
!
! Initialise a whole lot of variables. For safety, we initialise lots of things with 0.0 (unless the IDL code specified otherwise).
! This is because Fortran does not guarantee that variables will be initialised to any particular value when they are declared.
!
F       = 0.0
DZDX    = 0.0   
DZDY    = 0.0
DIVU    = 0.0
DIVV    = 0.0
X       = 0.0
Y       = 0.0
XS      = 0.0
YS      = 0.0
FS      = 0.0
DZDXS   = 0.0
DZDYS   = 0.0
DIVUS   = 0.0
DIVVS   = 0.0
UNITH   = 1.0
UNITHS  = 1.0
UNITV   = 0.0
UNITV1  = 0.0
C       = 0.0
B       = 0.0
DBDVAR  = 0.0
A       = 0.0
LD      = 0.0
BVAR    = 0.0
UNITVAR = 0.0
U       = 0.0
V       = 0.0
R       = 0.0
H       = 0.0
Z       = 0.0
T       = 0.0
BVAR1   = 0.0
U1      = 0.0
V1      = 0.0
R2      = 0.0
H2      = 0.0
Z2      = 0.0
Z1      = 0.0
BVARS   = 0.0
US      = 0.0
VS      = 0.0
ZS      = 0.0
TS      = 0.0
A1      = 0.0
!A2      = 0.0
B1      = 0.0
LD1     = 0.0
DIV1    = 0.0
DIV2    = 0.0
DIVS    = 0.0
FCX1    = 0.0
FPX1    = 0.0
FCY1    = 0.0
FPY1    = 0.0
FCXS    = 0.0
FCYS    = 0.0
FPXS    = 0.0
FPYS    = 0.0
D       = 0
CODE    = 0
INDX    = 0
SC      = 0.0
RC      = 0.0
OLD1    = 0.0
OLD2    = 0.0
test1 = 0.0
test2 = 0.0
!
! Calculate the components of the horizontal gradient and divergence at each vertical level, and at the surface.
! All the temporary arrays used in the IDL procedure have been replaced by Fortran 90 array sections. Simplifies
! things a bit.
!

DO KK=KB,KT
    CALL HORIZONTAL_FIELD(NSTU, DU(9,KK,:), DU(10,KK,:), X, Y, F(KK,:), DZDX(KK,:), DZDY(KK,:), DIVU(KK,:), DIVV(KK,:))
END DO

CALL HORIZONTAL_FIELD(NSTS, LONS, LATS, XS, YS, FS, DZDXS, DZDYS, DIVUS, DIVVS)
DO MM=1,5
    WVAR(MM,:,:)=WVAR(MM,:,:)/AD(MM)
END DO

DO MM=1,NVBUDGET_COLUMN
    DU(MM,1:NP,1:NSTU) = DU(MM,1:NP,1:NSTU)*AD(MM)
    DSS(MM,1:NSTS)=DSS(MM,1:NSTS)*AD(MM)
    NTERM=INT(BUDGET(MM,1), KIND=IK4)
    BUDGET(MM,2:NTERM+2)=BUDGET(MM,2:NTERM+2)*AD(MM)
END DO

!
! Initialise more variables.
!
DUS             = DU

UNITV(KB:KT)    = 1.0
UNITV1(KS:KT)   = 1.0
UNITV1(KS)      = 0.5 + (BUDGET(1,7) - P(KS))/DP*100.
UNITV1(KT)      = 0.5
UNITV(KB)       = 0.5 + (BUDGET(1,7) - P(KB))/DP*100.
UNITV(KT)       = 0.5

!
! Print a warning if the surface level is different than the level at which surface pressure is located (they should be
! the same).
!
IF (KB .NE. KS) THEN
    PRINT *,'Warning: KB different from KS. Try to make them the same'
END IF

!
! Do more stuff.
!
DO MB=1,NVBUDGET_COLUMN
    NTERM   = INT(BUDGET(MB, 1), 4)
    C(MB)   = -SUM(BUDGET(MB,3:NTERM+1))
END DO

!
! Fill out UNITVAR, R, H, U, V, Z and T from the big DUS array (which was passed into this procedure as DU)
!
UNITVAR(KB:KT,:)    = DUS(1,KB:KT,:)
R(KB:KT,:)          = DUS(2,KB:KT,:)
H(KB:KT,:)          = DUS(3,KB:KT,:)
U(KB:KT,:)          = DUS(4,KB:KT,:)
V(KB:KT,:)          = DUS(5,KB:KT,:)
Z(KB:KT,:)          = DUS(7,KB:KT,:)
T(KB:KT,:)          = DUS(6,KB:KT,:)

!
! Fill out US, VS, ZS and TS from the big DSS array.
!
US(1,:)             = DSS(4,:)
VS(1,:)             = DSS(5,:)
ZS(1,:)             = DSS(7,:)
TS(1,:)             = DSS(6,:)

!
! I think this is where the partial derivatives to the five constraint equations (equations (14)-(17) in Zhang and Lin) are
! calculated ... maybe.
!
DO KK=KB,KT
    DBDVAR(1,1,KK,:)    = UNITVAR(KK,:)*DIVU(KK,:)*DP/G
    DBDVAR(1,2,KK,:)    = UNITVAR(KK,:)*DIVV(KK,:)*DP/G
    DBDVAR(2,1,KK,:)    = R(KK,:)*DIVU(KK,:)*DP/G
    DBDVAR(2,2,KK,:)    = R(KK,:)*DIVV(KK,:)*DP/G
    DBDVAR(2,3,KK,:)    = (U(KK,:)*DIVU(KK,:) + V(KK,:)*DIVV(KK,:))*DP/G
    DBDVAR(3,1,KK,:)    = H(KK,:)*DIVU(KK,:)*DP/G
    DBDVAR(3,2,KK,:)    = H(KK,:)*DIVV(KK,:)*DP/G
    DBDVAR(3,4,KK,:)    = (U(KK,:)*DIVU(KK,:) + V(KK,:)*DIVV(KK,:))*DP/G
    DBDVAR(4,1,KK,:)    = (2.0*U(KK,:)*DIVU(KK,:) + V(KK,:)*DIVV(KK,:))*DP/G
    DBDVAR(4,2,KK,:)    = (U(KK,:)*DIVV(KK,:) - F(KK,:)/NSTU)*DP/G
    DBDVAR(4,4,KK,:)    = (KT+1-KK)*RD*DP/100.0/P(KK)*DZDX(KK,:)*DP/G
    DBDVAR(5,1,KK,:)    = (V(KK,:)*DIVU(KK,:) + F(KK,:)/NSTU)*DP/G
    DBDVAR(5,2,KK,:)    = (U(KK,:)*DIVU(KK,:) + 2.0*V(KK,:)*DIVV(KK,:))*DP/G
    DBDVAR(5,4,KK,:)    = (KT+1-KK)*RD*DP/100.0/P(KK)*DZDY(KK,:)*DP/G
END DO

DO MB=1,5                   ! This loops over the first five variables in the dus array.
    BVAR(KB:KT,:)       = DUS(MB,KB:KT,:)
    BVARS(1,:)          = DSS(MB,:)

    DO MB1=1,5
        DO IST=1,NSTU
            DO KK=KB,KT
                U1(KK,IST)  = -DBDVAR(MB1,1,KK,IST)/TWO/WVAR(4,KK,IST)
                V1(KK,IST)  = -DBDVAR(MB1,2,KK,IST)/TWO/WVAR(5,KK,IST)
                R2(KK)      = 0.0
                H2(KK)      = -DBDVAR(MB1,4,KK,IST)/TWO/WVAR(3,KK,IST)
                test1(KK,IST)=-WVAR(4,KK,IST)
                test2(KK,IST)=-WVAR(5,KK,IST)
            END DO
            CALL HEIGHT(NP, KB, KT, P, H2, R2, Z2)
            Z1(:,IST)       = Z2(:)
        END DO
        CALL DIVERG(UNITH=UNITH, VAR=BVAR, U=U1, V=V1, DIVU=DIVU, DIVV=DIVV, DIV1=DIV2)
        A(MB,MB1)           = DOT_PRODUCT(DIV2,UNITV)*DP/G          ! Equivalent to IDL code: transpose(div2)#unitv*dp/g
        !
        ! Print a warning if ABS(MB,MB1) is greater than 100.
        !
        !IF (ABS(A(MB,MB1)) .GT. 100) THEN
            !PRINT *,'Warning: A(MB,MB1) > 100',MB,MB1,A(MB,MB1)!,DIV2,'..',UNITV ,'..',DP,'..',G
            !  print *, OLD1,'....',test1,'#',MB1
            !  print *, OLD2,'....',test2,'#',MB1
        !else
        !    print *, U1,'...#....',V1
        !END IF
        OLD1=test1
        OLD2=test2
        !
        ! Calculate extra stuff for MB=4
        !
        IF (MB .EQ. 4) THEN
            CALL FCORLX(UNITH, NSTU, F, V1, FCX1)
            CALL FPGD(UNITH, Z1, DZDX, FPX1)
            A(4,MB1)    = A(4,MB1) - DOT_PRODUCT(FCX1,UNITV)*DP/G - DOT_PRODUCT(FPX1,UNITV)*DP/G
        END IF

        !
        ! Calculate extra stuff for MB=5
        !
        IF (MB .EQ. 5) THEN
            CALL FCORLY(UNITH, NSTU, F, U1, FCY1)
            CALL FPGD(UNITH, Z1, DZDY, FPY1)
            A(5,MB1)    = A(5,MB1) - DOT_PRODUCT(FCY1,UNITV)*DP/G - DOT_PRODUCT(FPY1,UNITV)*DP/G
        END IF
    END DO

    CALL DIVERG(UNITH, BVAR, U, V, DIVU, DIVV, DIV1)
    CALL DIVERG(UNITHS, BVARS, US, VS, DIVUS, DIVVS, DIVS)
    CALL ITPS(KS, KB, DIV1, DIVS(1))
    DIVBVAR(MB,KS:KT)   = DIV1(KS:KT)/AD(MB)
    B(MB)               = DOT_PRODUCT(DIV1,UNITV1)*DP/G + C(MB)
    NTERM               = INT(BUDGET(MB, 1), KIND=IK4)
    BUDGET(MB, NTERM+2) = -DOT_PRODUCT(DIV1,UNITV1)*DP/G
END DO

CALL DIVERG(UNITH, T, U, V, DIVU, DIVV, DIV1)
CALL DIVERG(UNITHS, TS, US, VS, DIVUS, DIVVS, DIVS)
CALL ITPS(KS, KB, DIV1, DIVS(1))
DIVBVAR(6,KS:KT)        = DIV1(KS:KT)

DO MVAR=1,NADVAR
    MM  = MADN(MVAR)
    DO MB1=1,5
        BVAR1(KB:KT,:)  = -DBDVAR(MB1,MVAR,KB:KT,:)/2.0/WVAR(MM,KB:KT,:)
        CALL DIVERG(UNITH, BVAR1, U, V, DIVU, DIVV, DIV1)
        A(MM,MB1)       = A(MM,MB1) + DOT_PRODUCT(DIV1,UNITV)*DP/G
    END DO
END DO

CALL FCORLX(UNITH, NSTU, F, V, FCX1)
CALL FCORLX(UNITHS, NSTS, FS, VS, FCXS)
CALL ITPS(KS, KB, FCX1, FCXS(1))
BUDGET(4,6)     = DOT_PRODUCT(FCX1,UNITV1)*DP/G

CALL FPGD(UNITH, Z, DZDX, FPX1)
CALL FPGD(UNITHS, ZS, DZDXS, FPXS)
CALL ITPS(KS, KB, FPX1, FPXS(1))
BUDGET(4,7)     = DOT_PRODUCT(FPX1,UNITV1)*DP/G
B(4)            = B(4) - BUDGET(4,6) - BUDGET(4,7)

CALL FCORLY(UNITH, NSTU, F, U, FCY1)
CALL FCORLY(UNITHS, NSTS, FS, US, FCYS)
CALL ITPS(KS, KB, FCY1, FCYS(1))
BUDGET(5,6)     = DOT_PRODUCT(FCY1,UNITV1)*DP/G

CALL FPGD(UNITH, Z, DZDY, FPY1)
CALL FPGD(UNITHS, ZS, DZDYS, FPYS)
CALL ITPS(KS, KB, FPY1, FPYS(1))
BUDGET(5,7)     = DOT_PRODUCT(FPY1,UNITV1)*DP/G
B(5)            = B(5) - BUDGET(5,6) - BUDGET(5,7)

FCX             = FCX1
FCY             = FCY1
FPX             = FPX1
FPY             = FPY1

BUDGET(1:5,2)   = -B(1:5)
B               = -B

A1              = 0.0
B1              = 0.0
A1(1:NAD,1:NAD) = A(1:NAD,1:NAD)
B1(1:NAD)       = B(1:NAD)
!A2              = A1

IF (NAD .LT. 3 ) THEN
    LD1(1)  = B1(1)/A1(1,1)
ELSE
    CALL LUDCMP(A=A1, N=NAD, INDX=INDX, D=D, CODE=CODE)
    CALL LUBKSB(A=A1, N=NAD, INDX=INDX, B=B1)
    LD1     = B1
ENDIF

LD(1:NAD)   = LD1(1:NAD)

!
! I think this is where equation (24) in Zhang and Lin is implemented.
!

DO MAD=1,NADVAR
    MM = MADN(MAD)
    DO MB=1,NAD
        if (MM .eq. 3) then
        endif
        DUS(MM,KB:KT,1:NSTU)    = DUS(MM,KB:KT,1:NSTU) - LD(MB)*DBDVAR(MB,MAD,KB:KT,1:NSTU)/2.0/WVAR(MM,KB:KT,1:NSTU)
    END DO
END DO

!
! Update T and Z based on s and r. This comment is copied directly from IDL code.
!
DO IST=1,NSTU
    SC(1:NP)    = DUS(3,1:NP,IST)
    RC(1:NP)    = DUS(2,1:NP,IST)*CPD/LV0
    RC(1)       = DSS(2,IST)*CPD/LV0
    SC(1)       = DSS(3,IST)
    CALL S_R_TO_T_Z(P, DSS(8,IST), DSS(7,IST), SC, RC, DUS(6,1:NP,IST), DUS(7,1:NP,IST))
END DO

DO MM=1,NVBUDGET_COLUMN
    DUS(MM,1:NP,1:NSTU)     = DUS(MM,1:NP,1:NSTU)/AD(MM)
    NTERM   = INT(BUDGET(MM,1), KIND=IK4)
    BUDGET(MM,2:NTERM+2)    = BUDGET(MM,2:NTERM+2)/AD(MM)
END DO

END SUBROUTINE ASSIM

!**************************************************************************************************************************
! CALC_BUDGET_LAYER
!
! This was ported from the IDL procedure with the same name.
!
! Tim Hume.
! 15 June 2007.
!**************************************************************************************************************************

SUBROUTINE CALC_BUDGET_LAYER(NP, NTU, P, BUDGET_LAYER, BUDGET_COLUMNS, AVE_QS, AVE_SS)
USE PORTABLE
USE CONSTANTS
USE SETTINGS

IMPLICIT NONE

INTEGER (KIND=IK4), INTENT(IN)                                              :: NP               ! Number of pressure levels.
INTEGER (KIND=IK4), INTENT(IN)                                              :: NTU              ! Number of times.
REAL (KIND=RK8), DIMENSION(NP), INTENT(IN)                                  :: P                ! Pressure of each level (hPa)
REAL (KIND=RK8), DIMENSION(NTU), INTENT(IN)                                 :: AVE_QS
REAL (KIND=RK8), DIMENSION(NTU), INTENT(IN)                                 :: AVE_SS
REAL (KIND=RK8), DIMENSION(NVBUDGET_COLUMN,NTERMMAX,NTU), INTENT(IN)        :: BUDGET_COLUMNS
REAL (KIND=RK8), DIMENSION(NVBUDGET_LAYER,NTERMMAXV,NP,NTU), INTENT(INOUT)  :: BUDGET_LAYER

!
! Local variables.
!
REAL (KIND=RK8)                                                     :: DP               ! Vertical resolution (hPa)
REAL (KIND=RK8)                                                     :: DP2
REAL (KIND=RK8), DIMENSION(6,NP+1)                                  :: OMEGA_VB

INTEGER (KIND=IK4)                                                  :: KS               ! Index of surface level.
INTEGER (KIND=IK4)                                                  :: KT               ! Index of top level.
INTEGER (KIND=IK4)                                                  :: K1, K2
INTEGER (KIND=IK4)                                                  :: NTERM

INTEGER (KIND=IK4)                                                  :: KK,LL,MVB,ITERM  ! Counters.

!
! Calculate vertical resolution.
!
DP  = P(1) - P(2)

DO LL=2,NTU-1
    KS  = INT(BUDGET_LAYER(1,2,1,LL), KIND=IK4)
    KT  = INT(BUDGET_LAYER(1,4,1,LL), KIND=IK4)

    OMEGA_VB = 0.0
    OMEGA_VB(1,KS)  = -BUDGET_COLUMNS(1,3,LL)*G/100.0                                   ! hPa/s

    DO KK=KS,KT
        BUDGET_LAYER(1,4,KK,LL) = -BUDGET_LAYER(1,3,KK,LL)                              ! -dw/dp
        DP2                     = DP
        IF (KK .EQ. KS) DP2     = (BUDGET_COLUMNS(1,7,LL) - P(KS)) + 0.5*DP
        IF (KK .EQ. KT) DP2     = 0.5*DP
        OMEGA_VB(1,KK+1)        = OMEGA_VB(1,KK) + BUDGET_LAYER(1,4,KK,LL)*DP2          ! DP2 vertically integrated not zero.
    END DO

    DO MVB=2,6
        DO KK=KS,KT+1
            K1                  = MAX(KS, KK-1)
            K2                  = MIN(KT,KK)
            OMEGA_VB(MVB,KK)    = OMEGA_VB(1,KK) * (BUDGET_LAYER(MVB,1,K1,LL) + BUDGET_LAYER(MVB,1,K2,LL))*0.5
        END DO
    END DO

    KK                  = KS
    MVB                 = 2
    OMEGA_VB(MVB,KK)    = OMEGA_VB(1,KK)*AVE_QS(LL)
    MVB                 = 3
    OMEGA_VB(MVB,KK)    = OMEGA_VB(1,KK)*AVE_SS(LL)

    DO MVB=1,6
        NTERM                               = INT(BUDGET_LAYER(MVB,1,1,1), KIND=IK4)
        DO KK=KS,KT
            BUDGET_LAYER(MVB,NTERM+3,KK,LL) = 0.5*(OMEGA_VB(MVB,KK) + OMEGA_VB(MVB,KK+1))
        END DO
        BUDGET_LAYER(MVB,NTERM+3,KT,LL)     = OMEGA_VB(MVB,KT+1)
    END DO

    !
    ! Vertical flux advection.
    !
    DO MVB=1,6
        NTERM                               = INT(BUDGET_LAYER(MVB,1,1,1), KIND=IK4)
        DO KK=KS,KT
            BUDGET_LAYER(MVB,4,KK,LL)       = -(OMEGA_VB(MVB,KK) - OMEGA_VB(MVB,KK+1))/DP
        END DO
        DP2                                 = 0.5*DP
        BUDGET_LAYER(MVB,4,KT,LL)           = -(OMEGA_VB(MVB,KT) - OMEGA_VB(MVB,KT+1))/DP2
        DP2                                 = 0.5*DP + (BUDGET_COLUMNS(1,7,LL) - P(KS))
        BUDGET_LAYER(MVB,4,KS,LL)           = -(OMEGA_VB(MVB,KS) - OMEGA_VB(MVB,KS+1))/DP2
    END DO

    !
    ! True advections.
    !
    DO MVB=2,6
        BUDGET_LAYER(MVB,3,KS:KT,LL)    = BUDGET_LAYER(MVB,3,KS:KT,LL) - BUDGET_LAYER(MVB,1,KS:KT,LL)*BUDGET_LAYER(1,3,KS:KT,LL)
        BUDGET_LAYER(MVB,4,KS:KT,LL)    = BUDGET_LAYER(MVB,4,KS:KT,LL) + BUDGET_LAYER(MVB,1,KS:KT,LL)*BUDGET_LAYER(1,3,KS:KT,LL)
    END DO

    DO MVB=1,5
        NTERM                               = INT(BUDGET_LAYER(MVB,1,1,1), KIND=IK4)
        DO KK=KS,KT
            BUDGET_LAYER(MVB,NTERM+2,KK,LL) = 0.0
            DO ITERM=2,NTERM+1
                BUDGET_LAYER(MVB,NTERM+2,KK,LL) = BUDGET_LAYER(MVB,NTERM+2,KK,LL) - BUDGET_LAYER(MVB,ITERM,KK,LL)
            END DO
        END DO
    END DO

    DO KK=KS,KT
        BUDGET_LAYER(2,8,KK,LL) = BUDGET_LAYER(2,3,KK,LL) + BUDGET_LAYER(2,4,KK,LL)
        BUDGET_LAYER(3,8,KK,LL) = BUDGET_LAYER(3,3,KK,LL) + BUDGET_LAYER(3,4,KK,LL)
        BUDGET_LAYER(2,7,KK,LL) = BUDGET_LAYER(3,5,KK,LL) + BUDGET_LAYER(2,5,KK,LL) - BUDGET_LAYER(3,7,KK,LL)
    END DO
END DO

END SUBROUTINE CALC_BUDGET_LAYER

END MODULE NUMERICS
