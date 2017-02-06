!***********************************************************************************************************************************
! VARIATIONAL_ANALYSIS
!
! This is the main variational analysis program. It has been directly ported from the IDL procedure called assim_twpice.f90, with
! apppropriate Fortran 95 modifications. The IDL procedure assumed the user had already pre-processed the input data using a suite
! of IDL scripts. The results of this pre-processing were assumed to be already loaded into memory once the main variational
! analysis procedure was started. We can't do this with Fortran 95 (unless we make one huge monolithic program which does both
! the pre-processing and variational analysis), so instead we read the preprocessed data from a NetCDF file.
!
! Inputs:   * Pre-processed data in NetCDF format. These can be generated using the other software distributed with this code:
!               see the documentation.
!           * Settings specific to the variational analysis are set in the SETTINGS module (settings.f90). If you change any of
!               these settings, you will need to re-compile the software.
!
! Tim Hume.
! 24 August 2006.
!***********************************************************************************************************************************
PROGRAM VARIATIONAL_ANALYSIS
USE PORTABLE
USE CONSTANTS                       ! This module contains various geophysical and meteorological constants.
USE SETTINGS                        ! This module contains settings specific to the variational analysis code.
USE PHYSICS
USE NUMERICS
USE IO                              ! This module contains a variety of I/O procedures.

!***********************************************************************************************************************************
! Local variables. There are a lot of these.
!***********************************************************************************************************************************
IMPLICIT NONE

CHARACTER (LEN=512)                                 :: INPUTFILE    ! Input filename
CHARACTER (LEN=512)                                 :: OUTPUTFILE   ! Output filename

REAL (KIND=RK8), DIMENSION(:,:,:,:), ALLOCATABLE    :: DU           ! Holds the pre-processed upper level input data.
REAL (KIND=RK8), DIMENSION(:,:,:), ALLOCATABLE      :: DS           ! Holds the pre-processed surface level input data.
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE          :: P            ! Holds the pressure levels for the variational analysis.
                                                                    ! It is assumed the pressure levels are equally spaced. (hPa)
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE          :: TU           ! Holds the times of the variational analysis.
                                                                    ! It is assumed the time steps are equally spaced. (days)
INTEGER (KIND=IK4)                                  :: NSTU         ! Number of stations supplying upper air data.
!INTEGER (KIND=IK4)                                  :: NSTS         ! Number of stations supplying surface data.
INTEGER (KIND=IK4)                                  :: NP           ! Number of pressure levels.
INTEGER (KIND=IK4)                                  :: NTU          ! Number of times.
CHARACTER (LEN=64), DIMENSION(:), ALLOCATABLE       :: VU           ! Names of state variables.
CHARACTER (LEN=64), DIMENSION(:), ALLOCATABLE       :: STN          ! Names of the stations.

REAL (KIND=RK8)                                     :: DP           ! Pressure level spacing (Pa)
REAL (KIND=RK8)                                     :: DTU          ! Time step spacing (s)
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE          :: UNITV        !
REAL (KIND=RK8), DIMENSION(:,:,:,:), ALLOCATABLE    :: DUS          ! A working array (contains DU initially).
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE          :: R1
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE          :: H1
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE          :: T1
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE          :: Z1
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE          :: T10
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE          :: Z10
REAL (KIND=RK8), DIMENSION(:,:), ALLOCATABLE        :: ERR
REAL (KIND=RK8), DIMENSION(:,:,:,:), ALLOCATABLE    :: BUDGET_LAYER
REAL (KIND=RK8), DIMENSION(:,:,:,:), ALLOCATABLE    :: BUDGET_LAYERS
REAL (KIND=RK8), DIMENSION(:,:,:), ALLOCATABLE      :: BUDGET_COLUMN
REAL (KIND=RK8), DIMENSION(:,:,:), ALLOCATABLE      :: BUDGET_COLUMNS
CHARACTER (LEN=64), DIMENSION(:,:), ALLOCATABLE     :: VBUDGET_COLUMN
CHARACTER (LEN=64), DIMENSION(:,:), ALLOCATABLE     :: VBUDGET_LAYER
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE          :: AVE_QS
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE          :: AVE_SS
INTEGER (KIND=IK4), DIMENSION(:,:), ALLOCATABLE     :: BOUNDARY ! Contains the various boundary levels used in the analysis.
                                                                ! BOUNDARY(1,1:NTU) = KS at each time step.
                                                                ! BOUNDARY(2,1:NTU) = KB at each time step.
                                                                ! BOUNDARY(3,1:NTU) = KT at each time step.
INTEGER (KIND=IK4)                                  :: KS       ! The level where the surface pressure is.
INTEGER (KIND=IK4)                                  :: KB       ! The level where the actual surface is (should be the same as KS)
INTEGER (KIND=IK4)                                  :: KT       ! The level where the variational analysis will stop at.

INTEGER (KIND=IK4), DIMENSION(:), ALLOCATABLE       :: STRU     ! The STRU array needs some explaining. It would appear that the
                                                                ! first element in the array represents the number of stations on
                                                                ! the perimeter of our sounding array. In the TWP-ICE case, this
                                                                ! will be 5 (Mount Bundy, Point Stuart, Pirlamgimpi, Cape Don and
                                                                ! the Southern Surveyor). The second element of the array appears to
                                                                ! represent the number of stations in the centre of the array (ie
                                                                ! not on the perimeter). In the TWP-ICE case, this is 1 (Darwin).
                                                                ! Then, the next NSTU elements are simply the identifying numbers
                                                                ! for each station. The central stations are numbered first (ie
                                                                ! station 1 = Darwin), followed by the perimeter stations (station 2
                                                                ! = Pirlangimpi and so on). I may be wrong on this, as it is not
                                                                ! documented anywhere in the IDL code.  One further point: the IDL
                                                                ! code declares the STRU array to be type REAL. However, it seems
                                                                ! that type INTEGER is always used for the station numbers.
                                                                ! Therefore, to avoid unnecessary conversions of REALs to INTEGERs,
                                                                ! the Fortran code sets the type to INTEGER.

INTEGER (KIND=IK4)                                  :: NSTU1    ! The number of stations on the perimeter of the array.
INTEGER (KIND=IK4)                                  :: NSTU1C   ! The number of stations in the centre of the array.
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE          :: LONU1    ! Longitudes of the upper air stations.
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE          :: LATU1    ! Latitudes of the upper air stations.
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE          :: X1       ! Cartesian x-coordinates of stations.
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE          :: Y1       ! Cartesian y-coordinates of stations.
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE          :: F1       ! Coriolis parameter at the stations.
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE          :: DZDX1    ! dz/dx at the stations.
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE          :: DZDY1    ! dz/dy at the stations.
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE          :: DIVU1    ! Used to calculate the divergence at the stations.
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE          :: DIVV1    ! Used to calculate the divergence at the stations.
REAL (KIND=RK8), DIMENSION(:,:,:), ALLOCATABLE      :: DU1      ! Input data for a single time step.
INTEGER (KIND=IK4)                                  :: NADMAX=5 !
INTEGER (KIND=IK4)                                  :: NADVAR=4 !
INTEGER (KIND=IK4)                                  :: NSTS1    ! Number of surface stations on the array perimeter?
INTEGER (KIND=IK4)                                  :: NSTS1C   ! Number of surface stations in the centre of the array?
INTEGER (KIND=IK4), DIMENSION(:), ALLOCATABLE       :: STRS     ! Same as STRU (see above), but for surface stations.
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE          :: LONS1    ! Longitudes of surface stations.
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE          :: LATS1    ! Latitudes of surface stations.
REAL (KIND=RK8), DIMENSION(:,:), ALLOCATABLE        :: DS1      ! Input data for the surface for a single time step.
INTEGER (KIND=IK4)                                  :: NSTU2
INTEGER (KIND=IK4)                                  :: NSTS2
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE          :: DUA
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE          :: DDU1DT
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE          :: WEIGHT
REAL (KIND=RK8)                                     :: DSA
REAL (KIND=RK8)                                     :: DDS1DT
REAL (KIND=RK8)                                     :: SDDU1DT
REAL (KIND=RK8), DIMENSION(:,:), ALLOCATABLE        :: BUDGET1
INTEGER (KIND=IK4)                                  :: NTERM    ! Refers to an index of an array.
REAL (KIND=RK8), DIMENSION(:,:,:), ALLOCATABLE      :: WVAR
REAL (KIND=RK8), DIMENSION(:,:), ALLOCATABLE        :: DEVM2
REAL (KIND=RK8)                                     :: ALPHA    !
REAL (KIND=RK8)                                     :: JUNKT, JUNKU, JUNKV  ! The names say it all.
!INTEGER (KIND=IK4), DIMENSION(5)                    :: M00      !
!INTEGER (KIND=IK4), DIMENSION(5)                    :: MULTI_SD !
REAL (KIND=RK8), DIMENSION(:,:), ALLOCATABLE        :: DIVVB1   !
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE          :: FCX1, FCY1, FPX1, FPY1
REAL (KIND=RK8), DIMENSION(:,:,:), ALLOCATABLE      :: DUS1
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE          :: ZC, TC

INTEGER (KIND=IK4)                                  :: ITER         ! Iteration counter
LOGICAL                                             :: CONT_ITER    ! Flag to indicate if we are going to continue iterating.
CHARACTER                                           :: RESPONSE     ! Does the user want to continue iteration?
LOGICAL                                             :: LOOP_ONCE    ! This flag is used to create DO WHILE loops which execute
                                                                    ! at least once. It was necessary because the original IDL code
                                                                    ! contained GOTO statements that effectively created loops that
                                                                    ! needed to be executed at least once.
INTEGER (KIND=IK4)                                  :: LL, IST, IST1, MB, KK  ! Counters.

INTEGER (KIND=IK4)                                  :: ST           ! Return code from ALLOCATE/DEALLOCATE functions.

!***********************************************************************************************************************************
! At this stage, we read in the pre-processed data from 2D_PUT, 3D_PUT and BUDGET_PUT
!***********************************************************************************************************************************

!
! Read the data which were pre-processed by 3D_PUT. From these, we can set set a multitude of variables.
!
PRINT *,'Enter the name of the 3D data file (written by 3D_PUT)'
READ (FMT='(A512)', UNIT=5) INPUTFILE
CALL IPT_3D_NETCDF(INPUTFILE, DU=DU, VU=VU, DS=DS, LEV=P, T=TU, STN=STN, WEIGHT=WEIGHT, BOUNDARY=BOUNDARY, STRU=STRU, STRS=STRS)

NTU     = SIZE(TU)
PRINT *, 'SIZE NTU',NTU
NSTU    = SIZE(STRU)-2
!NSTS    = SIZE(STRS)-2
NP      = SIZE(P)
DTU     = (TU(2) - TU(1))*86400.0       ! Time step (convert from days to seconds)
NSTU1   = STRU(1)
NSTU1C  = STRU(2)
NSTS1   = STRS(1)
NSTS1C  = STRS(2)

ALLOCATE(LONU1(NSTU1), LATU1(NSTU1), X1(NSTU1), Y1(NSTU1), F1(NSTU1), DZDX1(NSTU1), DZDY1(NSTU1), &
& DIVU1(NSTU1), DIVV1(NSTU1), DU1(NVU,NP,NSTU1), LONS1(NSTS1), LATS1(NSTS1), DS1(NVS,NSTS1), &
& DUA(NP), DDU1DT(NP), WVAR(5,NP,NSTU1), DEVM2(NVU,NP), STAT=ST)
IF (ST .NE. 0 ) THEN
    PRINT *,'E: Can not allocate one or more of the LONU1, LATU1, DU1 etc arrays'
    STOP '1'
END IF
LONU1   = 0.0
LATU1   = 0.0
X1      = 0.0
Y1      = 0.0
F1      = 0.0
DZDX1   = 0.0
DZDY1   = 0.0
DIVU1   = 0.0
DIVV1   = 0.0
DU1     = 0.0
LONS1   = 0.0
LATS1   = 0.0
DS1     = 0.0
DUA     = 0.0
DDU1DT  = 0.0
WVAR    = 0.0
DEVM2   = 0.0

!
! Read the data which were processed by BUDGET_PUT.
!
PRINT *,'Enter the name of the data file written by BUDGET_PUT'
READ (FMT='(A512)', UNIT=5) INPUTFILE
CALL IPT_BUDGET_NETCDF(INPUTFILE=INPUTFILE, BUDGET_COLUMN=BUDGET_COLUMN, BUDGET_LAYER=BUDGET_LAYER, &
& VBUDGET_COLUMN=VBUDGET_COLUMN, VBUDGET_LAYER=VBUDGET_LAYER, AVE_QS=AVE_QS, AVE_SS=AVE_SS)
!
! Allocate enough space for the arrays.
!
ALLOCATE(DUS(NVU,NP,NSTU,NTU), UNITV(NP), R1(NP), H1(NP), T1(NP), Z1(NP), T10(NP), &
& Z10(NP), ERR(NAD,NTU), BUDGET_LAYERS(NVBUDGET_LAYER, NTERMMAXV, NP, NTU), BUDGET_COLUMNS(NVBUDGET_COLUMN, NTERMMAX, NTU), &
& DIVVB1(NVBUDGET_LAYER,NP), DUS1(NVU,NP,NSTU), FCX1(NP), FCY1(NP), FPX1(NP), FPY1(NP), TC(NP), ZC(NP), &
& BUDGET1(NVBUDGET_COLUMN,NTERMMAX), STAT=ST)
IF (ST .NE. 0) THEN
    PRINT *,'E: Unable to allocate memory for all the required arrays'
    STOP '1'
ENDIF
DUS             = 0.0
UNITV           = 0.0
R1              = 0.0
H1              = 0.0
T1              = 0.0
Z1              = 0.0
T10             = 0.0
Z10             = 0.0
ERR             = 0.0
BUDGET_LAYERS   = 0.0
BUDGET_COLUMNS  = 0.0
DIVVB1          = 0.0
DUS1            = 0.0
FCX1            = 0.0
FCY1            = 0.0
FPX1            = 0.0
FPY1            = 0.0
TC              = 0.0
ZC              = 0.0
BUDGET1         = 0.0

!***********************************************************************************************************************************
! The variational analysis starts here.
!***********************************************************************************************************************************
!
! Set some variables with initial values.
!

UNITV           = 1.0
DUS             = DU
BUDGET_LAYERS   = BUDGET_LAYER
BUDGET_COLUMNS  = BUDGET_COLUMN
ITER            = 0
CONT_ITER       = .TRUE.

ITER_LOOP: DO WHILE (CONT_ITER)                 ! This is the outer most loop for the variational analysis.
    !
    ! Print the current iteration number.
    !
    PRINT *, 'Iteration number: ',ITER

    TIME_LOOP: DO LL=2,NTU-1                    ! Loop through times, but don't do the first and last time step.
        PRINT *,'ITER, TIME = ',ITER,LL         ! Let user know where we are up to.

        !
        ! Set the surface pressure, actual surface and top boundaries.
        !
        KS              = BOUNDARY(1,LL)
        KB              = BOUNDARY(2,LL)
        KT              = BOUNDARY(3,LL)
        DP              = (P(KS)-P(KS+1))*100.  ! Vertical level spacing in Pa (should be constant).

        UNITV(KS:KT)    = 1.0
        UNITV(KT)       = 0.5
        UNITV(KS)       = 0.5 + (BUDGET_COLUMNS(1,7,LL) - P(KS))/DP*100.0
       
        !
        ! Set the longitudes and latitudes of each upper air station. At this stage, we also extract the 
        ! input data from the DUS array for a single time step.
        !
        DO IST1=1,NSTU1
            IST                     = STRU(IST1+2+NSTU1C)
            LONU1(IST1)             = DUS(9,KB,IST,LL)
            LATU1(IST1)             = DUS(10,KB,IST,LL)
            DU1(1:NVU,KB:NP,IST1)   = DUS(1:NVU,KB:NP,IST,LL)
        END DO
        ! write(6,*) DU1(3,KB,1)
        !
        ! Set the longitudes and latitudes of each surface station. Also extract the input data from the
        ! DS array.
        !
        DO IST1=1,NSTS1
            IST                     = STRS(IST1+2+NSTS1C)
            LONS1(IST1)             = DS(9,IST,LL)
            LATS1(IST1)             = DS(10,IST,LL)
            DS1(1:NVS,IST1)         = DS(1:NVS,IST,LL)
        END DO

        NSTU2                       = NSTU1+NSTU1C
        NSTS2                       = NSTS1+NSTS1C

        VAR_LOOP: DO MB=2,6     ! Loop over the input variables.
            !
            ! Calculate the time derivatives for each upper air variable (averaged over the stations in the array). The centred
            ! difference method is used for calculating the derivatives.
            !
            DUA                 = 0.0
            DDU1DT              = 0.0
            DO KK=KB,NP
                DO IST1=1,NSTU2
                    IST         = STRU(IST1+2)
                    DUA(KK)     = DUA(KK) + DUS(MB,KK,IST,LL)*WEIGHT(IST)/SUM(WEIGHT)
                    DDU1DT(KK)  = DDU1DT(KK) + (DUS(MB,KK,IST,LL+1) - DUS(MB,KK,IST,LL-1))/2.0/DTU*WEIGHT(IST)/SUM(WEIGHT)
                END DO
            END DO
            DSA                 = 0.0
            DDS1DT              = 0.0
            !
            ! Also calculate the time derivatives for the surface variables.
            !
            DO IST1=1,NSTS2
                IST             = STRS(IST1+2)
                DSA             = DSA + DS(MB,IST,LL)/NSTS2
                DDS1DT          = DDS1DT + (DS(MB,IST,LL+1) - DS(MB,IST,LL-1))/2.0/DTU/NSTS2
            END DO

            !
            ! Interpolate the surface data.
            !
            CALL ITPS(KS,KB,DUA,DSA)
            CALL ITPS(KS,KB,DDU1DT, DDS1DT)

            SDDU1DT = DOT_PRODUCT(UNITV(KS:KT), DDU1DT(KS:KT))
            BUDGET_LAYERS(MB,1,:,LL)    = DUA(:)
            BUDGET_LAYERS(MB,2,:,LL)    = -DDU1DT(:)            ! Should not affect results (this comment is from the IDL code).

            IF (MB .LE. NVBUDGET_COLUMN) THEN
                BUDGET_COLUMNS(MB,3,LL) = -SDDU1DT*DP/G
            END IF

        END DO VAR_LOOP

        DO MB=1,NVBUDGET_COLUMN
            NTERM   = INT(BUDGET_COLUMN(MB,1,LL), 4)
            BUDGET1(MB,1:NTERM+2)       = BUDGET_COLUMNS(MB,1:NTERM+2,LL)
        END DO

        BUDGET1(1,7)                    = BUDGET_COLUMNS(1,7,LL)

        CALL HORIZONTAL_FIELD(NSTU1, LONU1, LATU1, X1, Y1, F1, DZDX1, DZDY1, DIVU1, DIVV1)
        DIVU1   = ABS(DIVU1/MAXVAL(DIVU1))
        DIVV1   = ABS(DIVV1/MAXVAL(DIVV1))

        CALL CALDEV(DU,KT,KS,NSTU,NTU,NP,NVU,DEVM2)

        DO IST=1,NSTU1
            DO KK=KB,KT
                ALPHA=1.0
                IF (P(KK) .LE. PCRIT) THEN
                    ALPHA       = (P(KK) - P(KT+1))/(PCRIT - P(KT+1))
                    ALPHA       = ALPHA**4
                END IF

                WVAR(1,KK,IST)  = 1.0/1.0
                WVAR(2,KK,IST)  = 1.0/(DU1(2,KK,IST)*0.03)      ! q

                JUNKT           = 0.2 + 0.1*DEVM2(6,KK)
                JUNKT           = JUNKT*ALPHA
                WVAR(3,KK,IST)  = 1.0/JUNKT

                JUNKU           = 0.5 + 0.2*DEVM2(4,KK)
                JUNKU           = JUNKU*ALPHA
                WVAR(4,KK,IST)  = 1.0/JUNKU*DIVU1(IST)

                JUNKV           = 0.5 + 0.2*DEVM2(5,KK)
                JUNKV           = JUNKV*ALPHA
                WVAR(5,KK,IST)  = 1.0/JUNKV*DIVV1(IST)

            END DO
        END DO

!        M00         = (/ 0,7,6,4,5 /)   ! What is this ... it doesn't seem to be used anywhere?
!        MULTI_SD    = (/ 0,10,5,6,7 /)  ! What is this?

        CALL ASSIM(NP   = NP, &     ! INTENT(IN):       Number of pressure levels.
            & DP        = DP,   &   ! INTENT(IN):       Spacing of the pressure levels (Pa).
            & P         = P,    &   ! INTENT(IN):       Pressure of each level (hPa).
            & KS        = KS, &     ! INTENT(IN):       Index of level where the surface pressure is.
            & KB        = KB, &     ! INTENT(IN):       Index of level where the surface is.
            & KT        = KT, &     ! INTENT(IN):       Index of the top level.
            & NSTU      = NSTU1, &  ! INTENT(IN):       Number of multi-level stations.
            & LONU      = LONU1, &  ! INTENT(IN):       Longitude of the multi-level stations.
            & LATU      = LATU1, &  ! INTENT(IN):       Latitude of the multi-level stations.
            & DU        = DU1, &    ! INTENT(INOUT):    The multi-level data. See elsewhere (e.g. assim.f90 for more detail).
            & NSTS      = NSTS1, &  ! INTENT(IN):       Number of surface stations.
            & LONS      = LONS1, &  ! INTENT(IN):       Longitudes of the surface stations.
            & LATS      = LATS1, &  ! INTENT(IN):       Latitudes of the surface stations.
            & DSS       = DS1, &    ! INTENT(INOUT):    Surface level data.
            & DIVBVAR   = DIVVB1, & ! INTENT(OUT):
            & FCX       = FCX1, &   ! INTENT(OUT):
            & FCY       = FCY1, &   ! INTENT(OUT):
            & FPX       = FPX1, &   ! INTENT(OUT):
            & FPY       = FPY1, &   ! INTENT(OUT):
            & BUDGET    = BUDGET1,& ! INTENT(INOUT):
            & NADVAR    = NADVAR, & ! INTENT(IN):
            & WVAR      = WVAR, &   ! INTENT(INOUT):
            & DUS       = DUS1)     ! INTENT(OUT):

        DO IST1=1,NSTU1
            R1(KB:KT)           = DUS1(2,KB:KT,IST1)/LV0*CPD
            H1(KB:KT)           = DUS1(3,KB:KT,IST1)
            T1(KB:KT)           = DUS1(6,KB:KT,IST1)
            Z1(KB:KT)           = DUS1(7,KB:KT,IST1)
            !
            ! Some iteration happens in here.
            !
            LOOP_ONCE   = .FALSE.
            DO WHILE ((MAXVAL(ABS(T1 - T10)) .GT. 0.05) .OR. (MAXVAL(ABS(Z1 - Z10)) .GT. 1.0) .OR. (.NOT. LOOP_ONCE ))
                LOOP_ONCE               = .TRUE.
                T10                     = T1
                Z10                     = Z1
                T1                      = H1 - G*Z1/CPD
                CALL HEIGHT(NP,KB,KT,P,T1,R1,Z1)
            END DO
            DUS1(6,KB:KT,IST1)          = T1(KB:KT)
            DUS1(7,KB:KT,IST1)          = Z1(KB:KT)
        END DO

        DO KK=KS,KT
            BUDGET_LAYERS(1,1,KK,LL)    = SUM(DUS1(7,KK,:))/NSTU1
        END DO
        BUDGET_LAYERS(1:6,3,:,LL)       = -DIVVB1(1:6,:)
        BUDGET_LAYERS(4,5,:,LL)         = FCX1(:)
        BUDGET_LAYERS(4,6,:,LL)         = FPX1(:)
        BUDGET_LAYERS(5,5,:,LL)         = FCY1(:)
        BUDGET_LAYERS(5,6,:,LL)         = FPY1(:)
        BUDGET_COLUMNS(4,6,LL)          = BUDGET1(4,6)
        BUDGET_COLUMNS(4,7,LL)          = BUDGET1(4,7)
        BUDGET_COLUMNS(5,6,LL)          = BUDGET1(5,6)
        BUDGET_COLUMNS(5,7,LL)          = BUDGET1(5,7)
        DO MB=1,NADMAX
            NTERM                       = INT(BUDGET_COLUMN(MB,1,LL), 4)
            BUDGET_COLUMNS(MB,2,LL)     = BUDGET1(MB,2)
            BUDGET_COLUMNS(MB,NTERM+2,LL)   = BUDGET1(MB,NTERM+2)
        END DO
        ERR(1:NAD,LL)   = BUDGET1(1:NAD,2)

        IF (ITER .GT. 0) THEN
            DO IST1=1,NSTU1
                IST = STRU(IST1+2+NSTU1C)
                DUS(1:NVU,KB:KT,IST,LL) = DUS1(1:NVU,KB:KT,IST1)
            END DO
        END IF

        DO IST=1,NSTU 
             ! Hack alert: The IDL code referred to a variable NST. I think this should be the same as NSTU?
             ! Hack alert: Got rid of several temporary variables in an effort to simplify things.
            CALL S_R_TO_T_Z(P, DUS(8,1,IST,LL), DUS(7,1,IST,LL), DUS(3,:,IST,LL), DUS(2,:,IST,LL)*CPD/LV0, TC, ZC)
            DUS(6,:,IST,LL) = TC(:)
            DUS(7,:,IST,LL) = ZC(:)
        END DO

    END DO TIME_LOOP    ! At long last!

    IF (ITER .EQ. 0) THEN
        BUDGET_LAYER    = BUDGET_LAYERS
        BUDGET_COLUMN   = BUDGET_COLUMNS
    END IF

    IF (ITER .LT. 2) THEN
        ITER        = ITER + 1
        CONT_ITER   = .TRUE.
    ELSE
        PRINT *,'More iteration?'
        READ(FMT='(A1)', UNIT=5) RESPONSE
        IF ((RESPONSE .EQ. 'Y') .OR. (RESPONSE .EQ. 'y')) THEN
            ITER        = ITER + 1
            CONT_ITER   = .TRUE.
        ELSE
            CONT_ITER   = .FALSE.
        END IF
    ENDIF

END DO ITER_LOOP

!
! Fill in the BUDGET_LAYER and BUDGET_LAYERS arrays.
!
CALL CALC_BUDGET_LAYER(NP, NTU, P, BUDGET_LAYER, BUDGET_COLUMN, AVE_QS, AVE_SS)
CALL CALC_BUDGET_LAYER(NP, NTU, P, BUDGET_LAYERS, BUDGET_COLUMNS, AVE_QS, AVE_SS)

!
! At this point, the IDL code sets some new arrays containing the results of the variational analysis. These can then be output by 
! another IDL procedure. However, for this Fortran code, it is more appropriate that the results are written to a file at this
! stage.
!

PRINT *,'Enter the name of the file to write the adjusted state data to'
READ (FMT='(A512)', UNIT=5) OUTPUTFILE
CALL OPT_3D_NETCDF(OUTPUTFILE, DU=DUS, VU=VU, DS=DS, P=P, T=TU, STN=STN, WEIGHT=WEIGHT, BOUNDARY=BOUNDARY, STRU=STRU, STRS=STRS)

PRINT *,'Enter the name of the file to write the adjusted budget data to'
READ (FMT='(A512)', UNIT=5) OUTPUTFILE
CALL OPT_BUDGET_NETCDF(OUTPUTFILE, BUDGET_COLUMNS, BUDGET_LAYERS, VBUDGET_COLUMN, VBUDGET_LAYER, AVE_QS, AVE_SS, P, TU) 


!***********************************************************************************************************************************
! For tidyness, deallocate all the arrays which have been allocated.
!***********************************************************************************************************************************

IF (ALLOCATED(DU))              DEALLOCATE(DU)
IF (ALLOCATED(DS))              DEALLOCATE(DS)
IF (ALLOCATED(DUS))             DEALLOCATE(DUS)
IF (ALLOCATED(P))               DEALLOCATE(P)
IF (ALLOCATED(TU))              DEALLOCATE(TU)
IF (ALLOCATED(UNITV))           DEALLOCATE(UNITV)
IF (ALLOCATED(R1))              DEALLOCATE(R1)
IF (ALLOCATED(H1))              DEALLOCATE(H1)
IF (ALLOCATED(T1))              DEALLOCATE(T1)
IF (ALLOCATED(Z1))              DEALLOCATE(Z1)
IF (ALLOCATED(T10))             DEALLOCATE(T10)
IF (ALLOCATED(Z10))             DEALLOCATE(Z10)
IF (ALLOCATED(ERR))             DEALLOCATE(ERR)
IF (ALLOCATED(BUDGET_LAYER))    DEALLOCATE(BUDGET_LAYER)
IF (ALLOCATED(BUDGET_LAYERS))   DEALLOCATE(BUDGET_LAYERS)
IF (ALLOCATED(BUDGET_COLUMN))   DEALLOCATE(BUDGET_COLUMN)
IF (ALLOCATED(BUDGET_COLUMNS))  DEALLOCATE(BUDGET_COLUMNS)
IF (ALLOCATED(VBUDGET_COLUMN))  DEALLOCATE(VBUDGET_COLUMN)
IF (ALLOCATED(VBUDGET_LAYER))   DEALLOCATE(VBUDGET_LAYER)
IF (ALLOCATED(BOUNDARY))        DEALLOCATE(BOUNDARY)
IF (ALLOCATED(STRU))            DEALLOCATE(STRU)
IF (ALLOCATED(LONU1))           DEALLOCATE(LONU1)
IF (ALLOCATED(LATU1))           DEALLOCATE(LATU1)
IF (ALLOCATED(X1))              DEALLOCATE(X1)
IF (ALLOCATED(Y1))              DEALLOCATE(Y1)
IF (ALLOCATED(F1))              DEALLOCATE(F1)
IF (ALLOCATED(DZDX1))           DEALLOCATE(DZDX1)
IF (ALLOCATED(DZDY1))           DEALLOCATE(DZDY1)
IF (ALLOCATED(DIVU1))           DEALLOCATE(DIVU1)
IF (ALLOCATED(DIVV1))           DEALLOCATE(DIVV1)
IF (ALLOCATED(DU1))             DEALLOCATE(DU1)
IF (ALLOCATED(STRS))            DEALLOCATE(STRS)
IF (ALLOCATED(LONS1))           DEALLOCATE(LONS1)
IF (ALLOCATED(LATS1))           DEALLOCATE(LATS1)
IF (ALLOCATED(DS1))             DEALLOCATE(DS1)
IF (ALLOCATED(DUA))             DEALLOCATE(DUA)
IF (ALLOCATED(DDU1DT))          DEALLOCATE(DDU1DT)
IF (ALLOCATED(WEIGHT))          DEALLOCATE(WEIGHT)
IF (ALLOCATED(BUDGET1))         DEALLOCATE(BUDGET1)
IF (ALLOCATED(WVAR))            DEALLOCATE(WVAR)
IF (ALLOCATED(DEVM2))           DEALLOCATE(DEVM2)
IF (ALLOCATED(DIVVB1))          DEALLOCATE(DIVVB1)
IF (ALLOCATED(DUS1))            DEALLOCATE(DUS1)
IF (ALLOCATED(FCX1))            DEALLOCATE(FCX1)
IF (ALLOCATED(FCY1))            DEALLOCATE(FCY1)
IF (ALLOCATED(FPX1))            DEALLOCATE(FPX1)
IF (ALLOCATED(FPY1))            DEALLOCATE(FPY1)
IF (ALLOCATED(TC))              DEALLOCATE(TC)
IF (ALLOCATED(ZC))              DEALLOCATE(ZC)
IF (ALLOCATED(VU))              DEALLOCATE(VU)
IF (ALLOCATED(STN))             DEALLOCATE(STN)

END PROGRAM VARIATIONAL_ANALYSIS
