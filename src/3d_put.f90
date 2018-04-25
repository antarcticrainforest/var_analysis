!***********************************************************************************************************************************
! 3D_PUT
!
! This program pre-processes the 3D input data for the variational analysis. The code is ported from the IDL procedure with
! the same name. Where appropriate, Fortran 90 modifications and simplifications have been made.
!
! Tim Hume.
! 29 August 2006.
!***********************************************************************************************************************************
PROGRAM PUT_3D
USE PORTABLE        ! To ensure portability to other compilers and platforms.
USE IO              ! IO subroutines are included in this module.
USE CONSTANTS       ! Various constants.
USE SETTINGS        ! Various settings for the variational analysis.
USE PHYSICS         ! Contains some physics subroutines.
USE NUMERICS

IMPLICIT NONE

!***********************************************************************************************************************************
! Local variables.
!***********************************************************************************************************************************

!
! The following variables deal with the data from the observed soundings.
!
CHARACTER (LEN=512)                                     :: INPUTAF          ! Name of the file.
INTEGER (KIND=IK4)                                      :: NVR_F            ! Number of variables in the file.
INTEGER (KIND=IK4)                                      :: NPF              ! Number of pressure levels in the file.
INTEGER (KIND=IK4)                                      :: NSTU             ! Number of stations in the file.
INTEGER (KIND=IK4)                                      :: NTF              ! Number of time steps in the file.
CHARACTER (LEN=64)                                      :: INSTRUMENTF      ! Name of the data source.
CHARACTER (LEN=64), DIMENSION(:), ALLOCATABLE           :: VR_F             ! Variable names.
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: PF               ! Pressure levels.
CHARACTER (LEN=64), DIMENSION(:), ALLOCATABLE           :: STU              ! Names of the stations.
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: TF               ! Time step values.
REAL (KIND=RK8), DIMENSION(:,:,:,:), ALLOCATABLE        :: DB_F             ! Sounding data.
REAL (KIND=RK8), DIMENSION(:,:,:,:), ALLOCATABLE        :: DB_F2            ! A temporary array used during the temporal, vertical
                                                                            ! and spatial filtering of DB_F
!REAL (KIND=RK8)                                         :: P_WEI0, P_WEI1   ! Weighting factors used for vertical smoothing.
!INTEGER (KIND=IK4)                                      :: MSMOOTH3         ! Controls the amount of spatial smoothing.
REAL (KIND=RK8), DIMENSION(:,:,:), ALLOCATABLE          :: DMEAN            ! Station mean of DB_F

!
! The following variables deal with the data from the observed soundings.
!
CHARACTER (LEN=512)                                     :: INPUTS           ! Name of the observed sounding file.
INTEGER (KIND=IK4)                                      :: NVU0             ! Number of variables in the sounding file.
INTEGER (KIND=IK4)                                      :: NP               ! Number of pressure levels in the sounding file.
INTEGER (KIND=IK4)                                      :: NSTS             ! Number of stations in the sounding file.
INTEGER (KIND=IK4)                                      :: NTU              ! Number of time steps in the sounding file.
CHARACTER (LEN=64)                                      :: INSTRUMENT       ! Name of the sounding instrument.
CHARACTER (LEN=64), DIMENSION(:), ALLOCATABLE           :: VU0              ! Sounding variable names.
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: P                ! Sounding pressure levels.
CHARACTER (LEN=64), DIMENSION(:), ALLOCATABLE           :: STS              ! Names of the sounding stations.
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: TU               ! Sounding time step values.
REAL (KIND=RK8), DIMENSION(:,:,:,:), ALLOCATABLE        :: DB_S             ! Sounding data.

!
! The following variables are used for the surface data.
!
CHARACTER (LEN=512)                                     :: INPUTFILE        ! Data filename.
CHARACTER (LEN=64)                                      :: INSTRUMENT1      ! Name of instrument which created the data.
INTEGER (KIND=IK4)                                      :: NV1              ! Number of variables in the file.
INTEGER (KIND=IK4)                                      :: NST              ! Number of stations.
INTEGER (KIND=IK4)                                      :: NT               ! Number of time steps.
CHARACTER (LEN=64), DIMENSION(:), ALLOCATABLE           :: V1               ! Variable name(s).
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: LONF             ! Station longitude(s).
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: LATF             ! Station latitude(s).
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: T                ! Time steps.
REAL (KIND=RK8), DIMENSION(:,:,:), ALLOCATABLE          :: D_PS, D_U, D_V,& ! Surface pressure, U, V, T, RH and Z fields.
                                                            &  D_T, D_RH, D_Z   

!
! Data required for the variational analysis
!
REAL (KIND=RK8), DIMENSION(:,:,:,:), ALLOCATABLE        :: DU0
REAL (KIND=RK8), DIMENSION(:,:,:,:), ALLOCATABLE        :: DU
CHARACTER (LEN=64), DIMENSION(:), ALLOCATABLE           :: VU               ! Names and units of variables in DU.
REAL (KIND=RK8), DIMENSION(:,:,:), ALLOCATABLE          :: DS               ! Surface level data.
REAL (KIND=RK8)                                         :: DWS              ! Value of some data in DU at the surface.
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: RH               ! Holds a column of relative humidity values.
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: TC               ! Holds a column of temperature values.
INTEGER (KIND=IK4), DIMENSION(:), ALLOCATABLE           :: STRU, STRS
INTEGER (KIND=IK4)                                      :: NSTU1, NSTU1C
REAL (KIND=RK8)                                         :: DP               ! Vertical level pressure spacing (is constant).
!REAL (KIND=RK8)                                         :: DTU              ! Time step (in seconds).
REAL (KIND=RK8)                                         :: PS               ! Surface pressure.
INTEGER (KIND=IK4), DIMENSION(:,:), ALLOCATABLE         :: BOUNDARY
CHARACTER (LEN=2), DIMENSION(3)                         :: VBOUNDARY        ! Names of the surface, bottom and top level indices.
INTEGER (KIND=IK4), DIMENSION(:,:), ALLOCATABLE         :: KB1              ! Vertical indices of the surface levels at each 
                                                                            ! station. These can change with time, as PS changes.
INTEGER (KIND=IK4)                                      :: KT               ! Index of the level at the top of the analysis.
INTEGER (KIND=IK4)                                      :: KS, KS2, KB2     ! Index of the level where the surface is.
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: WEIGHT
REAL (KIND=RK8)                                         :: TMP_P0, TMP_LON, TMP_LAT, TMP_U, TMP_V, TMP_T, TMP_TD, &
                                                            & TMP_VAP_PRES, TMP_RH, TMP_Z, TMP_R, TMP_HDRY, TMP_S, TMP_H
CHARACTER (LEN=512)                                     :: OUTPUTFILE       ! Data filename.

!
! Other variables.
!
!REAL (KIND=RK8)                                         :: ALPHA, BETA      ! Used in upper level spatial filtering.
INTEGER (KIND=IK4)                                      :: MEMST            ! Status returned from memory allocation operations.
INTEGER (KIND=IK4)                                      :: KK, MM, II, IST, LL, IST1   ! Counters.
!
! Get the filenames for the various input files from the user.
!
PRINT *,'Enter the name of the gridded analysis file'
READ (FMT='(A512)', UNIT=5) INPUTAF

PRINT *,'Enter the name of the sounding data file'
READ (FMT='(A512)', UNIT=5) INPUTS

!
! Now read the sounding files. The following subroutine makes use of the Fortran TR15581 extensions.
! Make sure your compiler supports these.
!
! Here is what I think are in the data arrays:
!
! Variable Number       Description and units
! --------------------- ---------------------
! 1                     Temperature (K)
! 2                     Relative humidity (0-1)
! 3                     U (m/s)
! 4                     V (m/s)
!
! The stations should be ordered so that the perimeter stations are first, and the central facility is last.
!
CALL IPT_VHT(TRIM(INPUTAF), INSTRUMENTF, NVR_F, NPF, NSTU, NTF, VR_F, PF, STU, TF, DB_F)
CALL IPT_VHT(TRIM(INPUTS), INSTRUMENT, NVU0, NP, NSTS, NTU, VU0, P, STS, TU, DB_S)          ! Why read this ... doesn't seem to be
                                                                                            ! used anywhere?
!
! Read some surface fields (Ps etc). We query the user for the individual filenames. The IDL script did not do this, but instead
! hard coded the filenames into the script. To avoid the hassle of manually typing all these names in, you can use the
! pre-processing wrapper script which is distributed with this code.
!

PRINT *,'Enter the name of the surface pressure data file'
READ (FMT='(A512)', UNIT=5) INPUTFILE
CALL IPT_HT(TRIM(INPUTFILE), INSTRUMENT1, NV1, NST, NT, V1, STU, LONF, LATF, T, D_PS)

PRINT *,'Enter the name of the surface U data file'
READ (FMT='(A512)', UNIT=5) INPUTFILE
CALL IPT_HT(TRIM(INPUTFILE), INSTRUMENT1, NV1, NST, NT, V1, STU, LONF, LATF, T, D_U)

PRINT *,'Enter the name of the surface V data file'
READ (FMT='(A512)', UNIT=5) INPUTFILE
CALL IPT_HT(TRIM(INPUTFILE), INSTRUMENT1, NV1, NST, NT, V1, STU, LONF, LATF, T, D_V)

PRINT *,'Enter the name of the surface T data file'
READ (FMT='(A512)', UNIT=5) INPUTFILE
CALL IPT_HT(TRIM(INPUTFILE), INSTRUMENT1, NV1, NST, NT, V1, STU, LONF, LATF, T, D_T)

PRINT *,'Enter the name of the surface RH data file'
READ (FMT='(A512)', UNIT=5) INPUTFILE
CALL IPT_HT(TRIM(INPUTFILE), INSTRUMENT1, NV1, NST, NT, V1, STU, LONF, LATF, T, D_RH)

PRINT *,'Enter the name of the surface Z data file'
READ (FMT='(A512)', UNIT=5) INPUTFILE
CALL IPT_HT(TRIM(INPUTFILE), INSTRUMENT1, NV1, NST, NT, V1, STU, LONF, LATF, T, D_Z)

!
! Do a little check to see if there is a problem with the data.
!
if ((NPF .NE. NP) .OR. (NTF .NE. NTU) .OR. (NT .NE. NTU) .OR. (NST .NE. NSTU)) THEN
    PRINT '("E: Something is not correct: NPF=",I6,"   NP=",I6)', NPF, NP
    PRINT '("                             NTF=",I6,"  NTU=",I6)', NTF, NTU
    PRINT '("                              NT=",I6,"  NTU=",I6)', NT, NTU
    PRINT '("                             NST=",I6," NSTU=",I6)', NST, NSTU
    STOP '1'
END IF

!***********************************************************************************************************************************
! Now we have read in the raw data, start processing it.
!***********************************************************************************************************************************

!
! Allocate space for various arrays used in the following code.
!
ALLOCATE(DB_F2(NVR_F,NPF,NSTU,NTF), STAT=MEMST)
IF (MEMST .NE. 0) THEN
    PRINT *,'E: Unable to allocate DB_F2'
    STOP '1'
ENDIF

!
! Label the stations F1 ... FNSTU (where NSTU is the number of the final station)
!
DO II=1,NSTU
    WRITE(UNIT=STU(II), FMT='("F",I0.1)') II 
ENDDO

!
! Ensure DB_F2(2,:,:,:) (relative humidity) lies in the range 0-1. This code is
! a bit unusual, a RH of -1% is set to 5%, but an RH of 3% stays at 3%. Don't
! know the justification for this.
!

write(77,*) DB_F(2,:,:,:)

WHERE (DB_F(2,:,:,:) .GT. 1.0) DB_F(2,:,:,:)  = 1.0
WHERE (DB_F(2,:,:,:) .LE. 0.0) DB_F(2,:,:,:)  = 0.05

!
! First we filter the backgound analysis field (DB_F) in time. The IDL code uses a number of temporary variables. I've tried to
! simplify things quite a bit. The filtering algorithm is this:
!
! filtered_variable(t) = 0.25*variable(t-1) + 0.5*variable(t) + 0.25*variable(t+1)
!

DB_F2   = 0.25*CSHIFT(DB_F, -1, 4) + 0.5*DB_F + 0.25*CSHIFT(DB_F, 1, 4)
        ! ------------------------   --------   -----------------------
        ! Previous time step         This time  Next time step
        ! The CSHIFT function simply shifts all the data at each time (the 4th dimension in the DB_F array) forward (shift=-1) or
        ! backwards in time (shift=1) by one time step.

DB_F2(:,:,:,1)      = 0.5*DB_F(:,:,:,1)   + 0.5*DB_F(:,:,:,2)     ! Need to treat first and last times slightly differently.
DB_F2(:,:,:,NTF)    = 0.5*DB_F(:,:,:,NTF) + 0.5*DB_F(:,:,:,NTF-1) ! filtered_variable(1)   = 0.5*variable(1)   + 0.5*variable(2)
                                                                  ! filtered_variable(NTF) = 0.5*variable(NTF) + 0.5*variable(NTF-1)
DB_F                = DB_F2

!
! Now do vertical smoothing on the background field. We won't smooth the moisture field (the second variable in the DB_F array). The
! smoothing algorithm is very similar to the time filtering algorithm, except the weighting factors are a function of pressure.
!
! 20-June-2007: We no longer do vertical filtering for TWP-ICE.
!

!DO KK=2,NPF-1                                       ! Need this loop, because the weighting factors are different for each 
!                                                    ! pressure level.
!    P_WEI0          = 0.3333 + PF(KK)/PF(1)*0.6667  ! Weighting factor for current pressure level.
!    P_WEI1          = (1 - P_WEI0)/2.0              ! Weighting factor for adjacent (above and below) pressure levels.
!    DB_F2(:,KK,:,:) = P_WEI1*DB_F(:,KK-1,:,:) + P_WEI0*DB_F(:,KK,:,:) + P_WEI1*DB_F(:,KK+1,:,:)
!END DO
!
!DB_F2(:,1,:,:)      = DB_F(:,1,:,:)                 ! The IDL code had a more complex expression which reduced to this.
!DB_F2(:,NPF,:,:)    = 0.5*DB_F(:,NPF,:,:) + 0.5*DB_F(:,NPF-1,:,:)
!DB_F2(2,:,:,:)      = DB_F(2,:,:,:)                 ! Don't smooth the moisture field (second variable)
!
!DB_F                = DB_F2

!
! Now do spatial smoothing on the background field. We do different amounts of smoothing depending on what vertical level we are on
! (hence the need to loop over pressures.
!
! 20-June-2007: We don't do spatial smoothing for TWP-ICE.
!
!DO MM=1,NVR_F
!    DO KK=1,NPF
!        IF (PF(KK) .GT. 300.) THEN
!            MSMOOTH3    = 3
!        ELSE
!            MSMOOTH3    = 5     ! More smoothing for high levels.
!        END IF
!        DO II=1,NTF
!            !
!            ! OK, here is where the spatial smoothing happens. What we do is lay the stations out in a 1D line:
!            !
!            ! data(station 1), data(station 2) ... data(station NSTU-1)
!            ! where data(station N) is for the variable, vertical level and time step specified by the surrounding DO loops.
!            !
!            ! Hack alert! I can't figure out why the IDL code only used the stations 1 through NSTU-1, but here is my guess: I
!            ! believe this is a "hard code". Stations 1 thorugh NSTU-1 are on the perimeter of the array, and station NSTU is in the
!            ! centre of the array. It appears the smoothing is only going to be applied to the stations on the perimeter of the
!            ! array. If my guess is correct, then this area of the code should be made more general, so that hard coding is not
!            ! necessary (maybe we should carry around a variable with the number of stations on the perimeter of the array?)
!            !
!            ! OK, after that little digression, return to the description of the filtering. We are going to apply a box car filter
!            ! to the one dimensional array described above. As it is, there will be problems at the end of the array (where the
!            ! "box runs off" the ends). Noting that the station data is cyclic (ie the first station in the array is adjacent to the
!            ! last station in the array), we can safely apply a box car filter to this array:
!            !
!            ! data(stn 1), ... data(stn NTSU-1), data(stn 1), ... data(stn NTSU-1), data(stn 1), ... data(stn NTSU-1)
!            !
!            ! Then, we can take the filtered data from the NTSU-1 stations in the "middle" section of the array 
!            ! (data(stn 1) ... data(stn NTSU-1))
!            !
!            ! Note on the filtering: The smooth function used in the IDL code appears to be a simple box car filter. The width of
!            ! the filter is specified by the MSMOOTH3 variable set above.
!            !
!            CALL SMOOTH(DB_F(MM, KK, 1:NSTU-1, II), NSTU-1, MSMOOTH3, .TRUE.)
!        END DO
!    END DO
!END DO
!
!!
!! Do more spatial filtering on the upper level data.
!!
!
!ALLOCATE(DMEAN(NVR_F,NPF,NTF), STAT=MEMST)
!IF (MEMST .NE. 0) THEN
!    PRINT *,'E: Cant allocate DMEAN array'
!    STOP '1'
!END IF
!
!DMEAN   = SUM(DB_F, DIM=3)/NSTU                         ! Average of the DB_F array over the stations (third dimension).
!
!DO KK=1,NPF
!    ALPHA   = MAX(0, (PSTART-P(KK))/(PSTART-P(NP)))     ! The IDL code refers to P array, should this actually be the
!                                                        ! PF array? (even though they both should be the same).
!    BETA    = 1. - ALPHA
!    DO II=1,NSTU
!        DB_F2(:,KK,II,:)    = ALPHA*DMEAN(:,KK,:) + BETA*DB_F(:,KK,II,:)
!    END DO
!END DO
!DB_F    = DB_F2

!
! Now give the user the option to output the filtered data. 
!
PRINT *,'Save filtered data to file? No filename entered means no save.'
READ (FMT='(A512)', UNIT=5) OUTPUTFILE
IF (LEN_TRIM(OUTPUTFILE) .GT. 0) THEN
    CALL OPT_VHT_NETCDF(OUTPUTFILE, INSTRUMENTF, NVR_F, NPF, NSTU, NTF, VR_F, PF, STU, TF, DB_F)
ENDIF

!
! Now we start to create the du array which is used by the variational analysis.
!

ALLOCATE(DU0(NVU,NP,NSTU,NTU), STAT=MEMST)
IF (MEMST .NE. 0) THEN
    PRINT *,'E: Unable to allocate memory for the DU0 array'
    STOP '1'
END IF

!
! Populate DU0 with the "coordinate variables"
!
DU0(1,:,:,:)    = SPREAD(SPREAD(TU, DIM=1, NCOPIES=NSTU), DIM=1, NCOPIES=NP)    ! Copy time steps into DU0 array.
DU0(2,:,:,:)    = SPREAD(SPREAD(P, DIM=2, NCOPIES=NSTU), DIM=3, NCOPIES=NTU)    ! Pressure levels.
DU0(3,:,:,:)    = SPREAD(SPREAD(LONF, DIM=1, NCOPIES=NP), DIM=3, NCOPIES=NTU)   ! Longitudes.
DU0(4,:,:,:)    = SPREAD(SPREAD(LATF, DIM=1, NCOPIES=NP), DIM=3, NCOPIES=NTU)   ! Latitudes.

!
! Copy the upper level data into DU0
!
DU0(5,:,:,:)    = DB_F(3,:,:,:)
DU0(6,:,:,:)    = DB_F(4,:,:,:)
DU0(7,:,:,:)    = DB_F(1,:,:,:) - T0        ! Convert K (in DB_F) to C.
DU0(9,:,:,:)    = DB_F(2,:,:,:) * 100.0     ! Convert to % (in DU0).

!
! Add the surface layer data to DU0
!
DU0(2,1,:,:)    = D_PS(2,:,:)               ! Surface pressure.
DU0(5,1,:,:)    = D_U(2,:,:)                ! Surface wind u-component.
DU0(6,1,:,:)    = D_V(2,:,:)                ! Surface wind v-component.
DU0(7,1,:,:)    = D_T(2,:,:)                ! Surface temperature.
DU0(9,1,:,:)    = D_RH(2,:,:)               ! Surface relative humidity.
DU0(10,1,:,:)   = D_Z(2,:,:)                ! Surface geopotential.

!
! Fill the upper levels of the DU0 array with geopotential height and dew (point temperature?)
! This comment is from the IDL code: Use cal_state2 later to avoid dew calculation.
!
ALLOCATE(TC(NP), RH(NP), STAT=MEMST)
IF (MEMST .NE. 0) THEN
    PRINT *,'E: Unable to allocate memory for TC or RH'
    STOP '1'
END IF

DO II=1,NTU
    DO IST=1,NSTU
        TC(:)   = DU0(7,:,IST,II)   ! Temperatures in Celsius extracted from the DU0 array.
        RH(:)   = DU0(9,:,IST,II)   ! RH in %
        CALL CALHT2(DU0(2,1,IST,II), DU0(10,1,IST,II), NP, P, TC, RH, DU0(10,:,IST,II), DU0(8,:,IST,II))
        !           ---------------  ----------------         --  --  ----------------  ----------------
        !                 Ps                Zs                T   RH  Geopotential Ht.  Dew Pt. Temp.?
        ! The CALHT2 subroutine will convert the supplied relative humidities (RH) to mixing ratios.

        DU0(8,:,IST,II) = TC
    END DO
END DO

!
! Now here is a very error prone part of the code? In the IDL code, the data arrays are "rotated" so that the data for the SGP
! central facility is at the beginning of the arrays. At Darwin, the central facility is at Darwin airport, so _provided_ the raw
! data are provided in the correct order, this rotation should be OK.
!
DU0     = CSHIFT(DU0, SHIFT=-1, DIM=3)  ! Shift all data by one station. The last station (the central facility) gets shifted off
                                     ! the end, and cycles to the beginning.
STU     = CSHIFT(STU, SHIFT=-1, DIM=1)
LATF    = CSHIFT(LATF, SHIFT=-1, DIM=1)
LONF    = CSHIFT(LONF, SHIFT=-1, DIM=1)

!
! Miscellaneous stuff happens here.
!
ALLOCATE(STRU(NSTU+2), STRS(NSTU+2), STAT=MEMST)
IF (MEMST .NE. 0) THEN
    PRINT *,'E: Unable to allocate memory for STRS or STRU'
    STOP '1'
END IF

DP      = P(1) - P(2)               ! Level spacing.
!DTU     = (TU(2) - TU(1)) * 86400.  ! Time step in seconds.

STRU    = (/ NSTU-1, 1, (II, II=1,NSTU) /)  ! This is the beginning of incredible ugliness ... the contents of this array are array
                                            ! indices, and you can imagine the consequences of this when code is ported from a
                                            ! language where array indices start at 0 to a language where the indices start at 1.
STRS    = STRU

ALLOCATE(BOUNDARY(3, NTU), STAT=MEMST)
IF (MEMST .NE. 0) THEN
    PRINT *,'E: Unable to allocate memory for BOUNDARY array.'
    STOP '1'
END IF

!
! Find the vertical index for the top of the variational analysis (which is at the level PT, specified in settings.f90).
CALL WINDOWN(NP, P, DP, PT, 1, KT)

!
! Now find the vertical indices for the surface level at each of the stations.
!
NSTU1   = STRU(1)
NSTU1C  = STRU(2)

ALLOCATE(KB1(NSTU1, NTU), STAT=MEMST)
IF (MEMST .NE. 0) THEN
    PRINT *,'E: Unable to allocate memory for KB1 array.'
    STOP '1'
END IF

DO LL=1,NTU     ! This loop is needed because the surface pressure changes with time, and it might affect the surface level index.
    !
    ! First do the perimeter facilities.
    !
    DO IST1=1,NSTU1
    IST = STRU(IST1+2+NSTU1C)
        PS  = DU0(2,1,IST,LL)
        CALL WINDOWN(NP, P, DP, PS, 1, KB1(IST1, LL))
    END DO
    !
    ! Now do the central facility.
    !
    PS  = DU0(2,1,1,LL)
    CALL WINDOWN(NP, P, DP, PS, 1, BOUNDARY(1,LL))
END DO

!
! OK ... in the previous bit of code, BOUNDARY(1,:) was set to be the level index of the surface, for the central facility.
! We'll set BOUNDARY(2,:) to be the same as (BOUNDARY(1,:); BOUNDARY(2,:) is the level index of the bottom of the variational
! analysis, so it makes sense to have it the same as the level index of the surface. Finally, we'll set BOUNDARY(3,:) to be the 
! level index of the top of the variational analysis (KT). We'll also define an array which simply contains the names of these
! three surfaces (ks, surface level index; kb, variational analysis bottom level index and kt, variational analysis top level
! index.)
!
BOUNDARY(2,:)   = BOUNDARY(1,:)
BOUNDARY(3,:)   = KT
VBOUNDARY       = (/ "ks", "kb", "kt" /)

!
! Allocate the DU array. This is similar to the DU0 array, but the number of variables in the array is specified as a setting
! in settings.f90
!
ALLOCATE(DU(NVU, NP, NSTU, NTU), STAT=MEMST)
IF (MEMST .NE. 0) THEN
    PRINT *,'E: Unable to allocate memory for DU array.'
    STOP '1'
END IF
DU=0.0
DO LL=1,NTU
    DO IST=1,NSTU
        PS  = DU0(2,1,IST,LL)
        CALL WINDOWN(NP, P, DP, PS, 1, KS)
        !
        ! Deal with surface level data.
        !
        TMP_P0          = DU0(2,1,IST,LL)
        TMP_LON         = DU0(3,1,IST,LL)
        TMP_LAT         = DU0(4,1,IST,LL)
        TMP_U           = DU0(5,1,IST,LL)
        TMP_V           = DU0(6,1,IST,LL)
        TMP_T           = DU0(7,1,IST,LL) + T0          ! Convert from Celsius to Kelvin.
        TMP_TD          = DU0(8,1,IST,LL) + T0          ! Convert from Celsius to Kelvin.
        TMP_VAP_PRES    = 99999.0                       ! Missing value.
        TMP_RH          = DU0(9,1,IST,LL)/100.0         ! Convert from % to decimal fraction.
        TMP_Z           = DU0(10,1,IST,LL)

        TMP_R           = -9999.0*CPD/LV0               ! This is dodgy (-9999.0 is a missing value!), but its in the IDL code.
        TMP_HDRY        = -9999.0*CPD                   ! This is also dodgy.
        IF (TMP_RH .LT. 0.05) TMP_RH = 0.05             ! Constrain RH to be >= 0.05

        IF ((TMP_P0 .GT. -9990.) .AND. (TMP_T .GT. -8999.) .AND. (TMP_RH .GT. -10)) THEN
            !
            ! Notice the TMP_RH test in the IF statement above is redundant, because TMP_RH was set to be >= 0.05. We'll
            ! keep it the same as the IDL code so the two codes are more similar.
            !
            ! Use calc_state2 to .....
            !
            CALL CALC_STATE2(TMP_P0, TMP_T, TMP_TD, TMP_VAP_PRES, TMP_Z, TMP_R, TMP_RH, TMP_H, TMP_HDRY, TMP_S)
	    
	    !write(77,*) TMP_P0, TMP_T, TMP_TD, TMP_VAP_PRES, TMP_Z, TMP_R, TMP_RH, TMP_H, TMP_HDRY, TMP_S
        END IF
	            
        DU(1,1,IST,LL)  = 1.0
        DU(2,1,IST,LL)  = TMP_R*LV0/CPD
        DU(3,1,IST,LL)  = TMP_HDRY/CPD
        DU(4,1,IST,LL)  = TMP_U
        DU(5,1,IST,LL)  = TMP_V
        DU(6,1,IST,LL)  = TMP_T
        DU(7,1,IST,LL)  = TMP_Z
        DU(8,1,IST,LL)  = TMP_P0
        DU(9,1,IST,LL)  = TMP_LON
        DU(10,1,IST,LL) = TMP_LAT

        !
        ! Deal with the other (non-surface) levels.
        !
        DO KK=MAX(2,KS),NP         ! Process all the levels, even if KT is less than NP (the unused levels won't matter)
            TMP_P0          = DU0(2,KK,IST,LL)
            TMP_LON         = DU0(3,KK,IST,LL)
            TMP_LAT         = DU0(4,KK,IST,LL)
            TMP_U           = DU0(5,KK,IST,LL)
            TMP_V           = DU0(6,KK,IST,LL)
            TMP_T           = DU0(7,KK,IST,LL) + T0         ! Convert from Celsius to Kelvin.
            TMP_TD          = DU0(8,KK,IST,LL) + T0         ! Convert from Celsius to Kelvin.  
            TMP_VAP_PRES    = -99999.0                      ! Missing value
            TMP_RH          = DU0(9,KK,IST,LL)/100.0        ! Convert from % to decimal fraction.
            TMP_Z           = DU0(10,KK,IST,LL)

            TMP_R           = -9999.0*CPD/LV0               ! Dodgy initialisation!
            TMP_HDRY        = -9999.9*CPD                   ! Dodgy initialistaion!
            IF (TMP_RH .LT. 0.05) TMP_RH = 0.05             ! Minimum RH is 0.05 (5%)

            IF ((TMP_P0 .GT. -9990.) .AND. (TMP_T .GT. -8999.) .AND. (TMP_RH .GT. -10.0)) THEN      ! See previous comment.
                !
                ! Here we calculate R, H, HDRY and S.
                !
		
                CALL CALC_STATE2(TMP_P0, TMP_T, TMP_TD, TMP_VAP_PRES, TMP_Z, TMP_R, TMP_RH, TMP_H, TMP_HDRY, TMP_S)
	    !write(77,*) TMP_P0, TMP_T, TMP_TD, TMP_VAP_PRES, TMP_Z, TMP_R, TMP_RH, TMP_H, TMP_HDRY, TMP_S
            END IF


            DU(1,KK,IST,LL)     = 1.0
            DU(2,KK,IST,LL)     = TMP_R*LV0/CPD
            DU(3,KK,IST,LL)     = TMP_HDRY/CPD
            DU(4,KK,IST,LL)     = TMP_U
            DU(5,KK,IST,LL)     = TMP_V
            DU(6,KK,IST,LL)     = TMP_T
            DU(7,KK,IST,LL)     = TMP_Z
            DU(8,KK,IST,LL)     = TMP_P0
            DU(9,KK,IST,LL)     = TMP_LON
            DU(10,KK,IST,LL)    = TMP_LAT
        END DO
    END DO
END DO

!
! Re-calculate the dry static energy and height.
!

DO LL=1,NTU
    DO IST=1,NSTU
       
        TMP_P0  = DU(8,1,IST,LL)
        TMP_Z   = DU(7,1,IST,LL)
        CALL T_R_TO_S_Z(P, TMP_P0, TMP_Z, DU(6,:,IST,LL), DU(2,:,IST,LL)*CPD/LV0, DU(3,:,IST,LL), DU(7,:,IST,LL))
        !               -  ------  -----  --------------  ----------------------  --------------  --------------
        !               P  Ps      Zs     T               R                       S               Z
    END DO
END DO

!
! Here we do a vertical interpolation, so that the data for all the perimeter stations start at the same vertical level as
! the data for the central facility starts at.
!

DO LL=1,NTU                                                 ! Time loop necessary because surface level can change as Ps changes.
    DO IST=1,NSTU
        PS  = DU(8,1,IST,LL)                                ! The surface pressure at station IST, time step LL.
        KS2 = BOUNDARY(1,LL)                                ! This is the surface level index for the central facility.
        CALL WINDOWN(NP, P, DP, PS, 1, KB2)                 ! Find the surface level index for station IST at time step LL.
        IF (KB2 .GT. KS2) THEN                              ! We need to interpolate down from level KB2 to KS2.
            DO MM=1,NVU                                     ! Loop over all the variables in DU.
                DWS = DU(MM,1,IST,LL)                       ! The value of variable MM at the surface.
                CALL ITPS(KS2, KB2, DU(MM,:,IST,LL), DWS)   ! Do the interpolation.
            END DO
        END IF
    END DO
END DO

!
! Set an array with the names and units of the variables in the DU array.
!
ALLOCATE(VU(NVU), STAT=MEMST)
IF (MEMST .NE. 0) THEN
    PRINT *,'E: Cant allocate VU array'
    STOP '1'
END IF

VU  = (/ "unit           ", "r*Lv0/Cpd (K)  ", "hdry/Cpd (K)   ", "u (m/s)        ", "v (m/s)        ", &
    &    "T (K)          ", "z (m)          ", "p (hPa)        ", "lon (deg)      ", "lat (deg)      " /)

!
! Copy the surface data from DU into DS.
!
NSTS    = NSTU

ALLOCATE(DS(NVS,NSTS,NTU), STAT=MEMST)
IF (MEMST .NE. 0) THEN
    PRINT *,'E: Cant allocate DS array'
    STOP '1'
END IF
DS      = DU(:,1,:,:)

!
! Set the weights.
!
ALLOCATE(WEIGHT(NSTU), STAT=MEMST)
IF (MEMST .NE. 0) THEN
    PRINT *,'E: Cant allocate WEIGHT array'
    STOP '1'
END IF

WEIGHT      = 1.0
WEIGHT(1)   = REAL(NSTU-1)

!
! This part is new to the Fortran code. The IDL code relied on variables which were set here being carried forward when the
! other IDL procedures were run. Obviously this is not going to happen with a Fortran program, unless we save the required variables
! here and read them back later in the other parts of the variational analysis which require them.
!
OUTPUTFILE=''
DO WHILE (LEN_TRIM(OUTPUTFILE) .EQ. 0)
    PRINT *,'Enter file name to save 3D data to: '
    READ (FMT='(A512)', UNIT=5) OUTPUTFILE
END DO
CALL OPT_3D_NETCDF(OUTPUTFILE, DU=DU, VU=VU, DS=DS, P=PF, T=TF, & 
  & STN=STU, WEIGHT=WEIGHT, BOUNDARY=BOUNDARY, STRU=STRU, STRS=STRS)

!
! We're finished with this!
!
PRINT *,'Now run BUDGET_PUT before you run the variational analysis'

!
! For tidiness, deallocate all the allocated arrays.
!

IF (ALLOCATED(VR_F))        DEALLOCATE(VR_F)
IF (ALLOCATED(PF))          DEALLOCATE(PF)
IF (ALLOCATED(STU))         DEALLOCATE(STU)
IF (ALLOCATED(TF))          DEALLOCATE(TF)
IF (ALLOCATED(DB_F))        DEALLOCATE(DB_F)
IF (ALLOCATED(DB_F2))       DEALLOCATE(DB_F2)
IF (ALLOCATED(DMEAN))       DEALLOCATE(DMEAN)
IF (ALLOCATED(VU0))         DEALLOCATE(VU0)
IF (ALLOCATED(P))           DEALLOCATE(P)
IF (ALLOCATED(STS))         DEALLOCATE(STS)
IF (ALLOCATED(TU))          DEALLOCATE(TU)
IF (ALLOCATED(DB_S))        DEALLOCATE(DB_S)
IF (ALLOCATED(V1))          DEALLOCATE(V1)
IF (ALLOCATED(LONF))        DEALLOCATE(LONF)
IF (ALLOCATED(LATF))        DEALLOCATE(LATF)
IF (ALLOCATED(T))           DEALLOCATE(T)
IF (ALLOCATED(D_PS))        DEALLOCATE(D_PS)
IF (ALLOCATED(D_U))         DEALLOCATE(D_U)
IF (ALLOCATED(D_V))         DEALLOCATE(D_V)
IF (ALLOCATED(D_T))         DEALLOCATE(D_T)
IF (ALLOCATED(D_RH))        DEALLOCATE(D_RH)
IF (ALLOCATED(D_Z))         DEALLOCATE(D_Z)
IF (ALLOCATED(DU0))         DEALLOCATE(DU0)
!IF (ALLOCATED(DU))          DEALLOCATE(DU)
IF (ALLOCATED(TC))          DEALLOCATE(TC)
IF (ALLOCATED(RH))          DEALLOCATE(RH)
IF (ALLOCATED(STRU))        DEALLOCATE(STRU)
IF (ALLOCATED(STRS))        DEALLOCATE(STRS)
IF (ALLOCATED(BOUNDARY))    DEALLOCATE(BOUNDARY)
IF (ALLOCATED(KB1))         DEALLOCATE(KB1)
IF (ALLOCATED(VU))          DEALLOCATE(VU)
IF (ALLOCATED(DS))          DEALLOCATE(DS)
IF (ALLOCATED(WEIGHT))      DEALLOCATE(WEIGHT)

END PROGRAM PUT_3D
