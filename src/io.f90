!***********************************************************************************************************************************
! MODULE IO
!
! This module contains various IO subroutines.
!
! Tim Hume.
! 30 August 2006.
!***********************************************************************************************************************************
MODULE IO

CONTAINS

    !*******************************************************************************************************************************
    ! IPT_VHT
    !
    ! This subroutine has been imported from the IDL procedure of the same name. It reads 3D data (e.g. sounding data)
    !
    ! The input data format is very similar to that expected by the IDL procedure, so converting IDL format input data to the format
    ! that this subroutine can read should be easy. The following description uses Fortran format speciifiers.
    !
    ! Line              Format      Description
    ! ------            ----------- ------------------------------------------------------------------------------------------------
    ! 1                 A64         Description of the instrument which produced the data contained in this file.
    ! 2                 I6          Number of variables.
    ! 3                 I6          Number of pressure levels.
    ! 4                 I6          Number of stations.
    ! 5                 I6          Number of time steps.
    ! Next NV           A64         Names of the variables (NV is read from line 2)
    ! Next NP           F7.2        Pressure levels (NP is read from line 3). Units are hPa.
    ! Next NST          A64         Names of the stations (NST is read from line 4)
    ! Next NT           F10.5       Time steps (NT is read from line5). Units are days.
    ! Next NV*NP*NST*NT F10.3       The data.
    !
    ! The above format is quite cumbersome, but it is easy enough to generate. If the data are not formatted correctly, then this
    ! subroutine will probably fail with an IO error (which is caught at statement label 9999 below).
    !
    ! Note on Fortran compatibility. This subroutine makes use of the TR15581 extension, which allows allocatable arrays to be
    ! passed to procedures. To enable these extensions in g95, use the -ftr15581 compiler option. If you are using another compiler,
    ! you will need to find if it also supports this extension (many do).
    !
    ! Tim Hume.  
    ! 29 August 2006.
    !*******************************************************************************************************************************

    SUBROUTINE IPT_VHT(INPUTFILE, INSTRUMENT, NV, NP, NST, NT, V, P, ST, T, D)
    USE PORTABLE

    IMPLICIT NONE
    CHARACTER (LEN=*), INTENT(IN)                                       :: INPUTFILE    ! Name of the input file.
    CHARACTER (LEN=64), INTENT(OUT)                                     :: INSTRUMENT   ! Instrument name.
    INTEGER (KIND=IK4), INTENT(OUT)                                     :: NV           ! Number of variables?
    INTEGER (KIND=IK4), INTENT(OUT)                                     :: NP           ! Number of pressure levels.
    INTEGER (KIND=IK4), INTENT(OUT)                                     :: NST          ! Number of stations.
    INTEGER (KIND=IK4), INTENT(OUT)                                     :: NT           ! Number of time steps.
    CHARACTER (LEN=64), DIMENSION(:), ALLOCATABLE, INTENT(OUT)          :: V            ! Variable names?
    REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE, INTENT(OUT)             :: P            ! Values of the pressure levels.
    CHARACTER (LEN=64), DIMENSION(:), ALLOCATABLE, INTENT(OUT)          :: ST           ! Station names?
    REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE, INTENT(OUT)             :: T            ! Time values?
    REAL (KIND=RK8), DIMENSION(:,:,:,:), ALLOCATABLE, INTENT(OUT)       :: D            ! The data.

    !
    ! Local variables.
    !
    INTEGER (KIND=IK4)                                                  :: IOST         ! IO status (0 (OK) or +ve (error))
    INTEGER (KIND=IK4)                                                  :: MEMST        ! Status from ALLOCATE commands.
    INTEGER (KIND=IK4)                                                  :: II,JJ,KK,LL  ! Counters

    IOST = 0
    PRINT *,'I: Reading data from ',INPUTFILE

    !
    ! Open the data file. Be cautious, and don't let the file be written to.
    !
    OPEN (UNIT=100, IOSTAT=IOST, ERR=9999, FILE=INPUTFILE, STATUS='OLD', ACCESS='SEQUENTIAL', FORM='FORMATTED', ACTION='READ')

    !
    ! Read the file headers.
    !
    READ (FMT='(A64)', UNIT=100, ERR=9999, IOSTAT=IOST) INSTRUMENT         ! Name of the instrument producing the data.
    READ (FMT='(I6)', UNIT=100, ERR=9999, IOSTAT=IOST) NV                  ! Number of variables.
    READ (FMT='(I6)', UNIT=100, ERR=9999, IOSTAT=IOST) NP                  ! Number of pressure levels.
    READ (FMT='(I6)', UNIT=100, ERR=9999, IOSTAT=IOST) NST                 ! Number of stations.
    READ (FMT='(I6)', UNIT=100, ERR=9999, IOSTAT=IOST) NT                  ! Number of time steps.

    !
    ! Allocate enough space in the data arrays. We might as well be paranoid, and check if the arrays have already been
    ! allocated (and if so, deallocate them).
    !
    IF (ALLOCATED(V))   DEALLOCATE(V)
    IF (ALLOCATED(P))   DEALLOCATE(P)
    IF (ALLOCATED(ST))  DEALLOCATE(ST)
    IF (ALLOCATED(T))   DEALLOCATE(T)
    IF (ALLOCATED(D))   DEALLOCATE(D)

    ALLOCATE(V(NV), P(NP), ST(NST), T(NT), D(NV,NP,NST,NT), STAT=MEMST)
    IF (MEMST .NE. 0) THEN
        PRINT *,'E: Unable to allocate memory to hold 3D data'
        STOP '1'
    END IF

    !
    ! Read the data arrays.
    !
    READ (FMT='(A64)', UNIT=100, ERR=9999, IOSTAT=IOST) (V(II), II=1,NV)       ! Read the variable names.
    READ (FMT='(F7.2)', UNIT=100, ERR=9999, IOSTAT=IOST) (P(II), II=1,NP)      ! Read the pressure levels.
    READ (FMT='(A64)', UNIT=100, ERR=9999, IOSTAT=IOST) (ST(II), II=1,NST)     ! Read the station names.
    READ (FMT='(F10.5)', UNIT=100, ERR=9999, IOSTAT=IOST) (T(II), II=1,NT)     ! Read the times.

    !
    ! Read all the data. Don't know how (or if) this can be done with an implicit do loop, therefore will do it the old fashioned
    ! way.
    !
    DO LL=1,NT
        DO KK=1,NST
            DO JJ=1,NP
                DO II=1,NV
                    READ(FMT='(F10.3)', UNIT=100, ERR=9999, IOSTAT=IOST) D(II,JJ,KK,LL)
                END DO
            END DO
        END DO
    END DO

    !
    ! Close the data file.
    !
    CLOSE (UNIT=100, ERR=9999, IOSTAT=IOST, STATUS='KEEP')

    !
    ! The next block of code deals with IO errors (if they occurred).
    !
    9999 CONTINUE
    IF (IOST .GT. 0) THEN
        PRINT *,'E: An IO error occurred while reading ',INPUTFILE
        STOP '1'
    END IF

    END SUBROUTINE IPT_VHT

    !*******************************************************************************************************************************
    ! IPT_HT
    !
    ! This subroutine has been ported from the IDL procedure with the same name. It reads data on a single level (eg Ps).
    !
    ! The data format is very similar to the format described in the previous subroutine:
    !
    ! Line number       Format      Description
    ! -------------     ----------- ------------------------------------------------------------------------------------------------
    ! 1                 A64         Name of instrument which created these data.
    ! 2                 I6          Number of variables in the file.
    ! 3                 I6          Number of stations in the file.
    ! 4                 I6          Number of time steps in the file.
    ! Next NV           A64         Variable names.
    ! Next NST          A64         Station names.
    ! Next NST          F10.5       Station longitudes.
    ! Next NST          F10.5       Station latitudes.
    ! Next NT           F10.5       Time steps.
    ! Next NV*NST*NT    F10.3       The data.
    !
    ! Tim Hume.
    ! 30 August 2006.
    !*******************************************************************************************************************************
    SUBROUTINE IPT_HT(INPUTFILE, INSTRUMENT, NV, NST, NT, V, ST, LON, LAT, T, D)
    USE PORTABLE

    IMPLICIT NONE

    CHARACTER (LEN=*), INTENT(IN)                                       :: INPUTFILE    ! Input data filename.
    CHARACTER (LEN=64), INTENT(OUT)                                     :: INSTRUMENT   ! Name of instrument which created the data.
    INTEGER (KIND=IK4), INTENT(OUT)                                     :: NV           ! Number of variables?
    INTEGER (KIND=IK4), INTENT(OUT)                                     :: NST          ! Number of stations.
    INTEGER (KIND=IK4), INTENT(OUT)                                     :: NT           ! Number of time steps.
    CHARACTER (LEN=64), DIMENSION(:), ALLOCATABLE, INTENT(OUT)          :: V            ! Names of the variables.
    CHARACTER (LEN=64), DIMENSION(:), ALLOCATABLE, INTENT(OUT)          :: ST           ! Names of the stations.
    REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE, INTENT(OUT)             :: LON          ! Longitudes of the stations.
    REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE, INTENT(OUT)             :: LAT          ! Latitudes of the stations.
    REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE, INTENT(OUT)             :: T            ! Time steps.
    REAL (KIND=RK8), DIMENSION(:,:,:), ALLOCATABLE, INTENT(OUT)         :: D            ! The data.

    !
    ! Local variables.
    !
    INTEGER (KIND=IK4)                                                  :: IOST         ! Status of IO operations.
    INTEGER (KIND=IK4)                                                  :: MEMST        ! Status of memory allocation operations.
    INTEGER (KIND=IK4)                                                  :: II, JJ, KK   ! Counters.

    IOST    = 0
    PRINT *,'I: Reading data from ',INPUTFILE

    !
    ! Open the data file. Be cautious, and don't let the file be written to.
    !
    OPEN (UNIT=100, IOSTAT=IOST, ERR=9999, FILE=INPUTFILE, STATUS='OLD', ACCESS='SEQUENTIAL', FORM='FORMATTED', ACTION='READ')

    !
    ! Read the file headers.
    !
    READ (FMT='(A64)', UNIT=100, ERR=9999, IOSTAT=IOST) INSTRUMENT         ! Name of the instrument producing the data.
    READ (FMT='(I6)', UNIT=100, ERR=9999, IOSTAT=IOST) NV                  ! Number of variables.
    READ (FMT='(I6)', UNIT=100, ERR=9999, IOSTAT=IOST) NST                 ! Number of stations.
    READ (FMT='(I6)', UNIT=100, ERR=9999, IOSTAT=IOST) NT                  ! Number of time steps.

    !
    ! Allocate enough space in the data arrays. We might as well be paranoid, and check if the arrays have already been
    ! allocated (and if so, deallocate them).
    !
    IF (ALLOCATED(V))   DEALLOCATE(V)
    IF (ALLOCATED(ST))  DEALLOCATE(ST)
    IF (ALLOCATED(LON)) DEALLOCATE(LON)
    IF (ALLOCATED(LAT)) DEALLOCATE(LAT)
    IF (ALLOCATED(D))   DEALLOCATE(D)

    ALLOCATE(V(NV), ST(NST), LON(NST), LAT(NST), T(NT), D(NV,NST,NT), STAT=MEMST)
    IF (MEMST .NE. 0) THEN
        PRINT *,'E: Unable to allocate memory to hold 2D data'
        STOP '1'
    END IF

    !
    ! Read the data arrays.
    !
    READ (FMT='(A64)', UNIT=100, ERR=9999, IOSTAT=IOST) (V(II), II=1,NV)       ! Read the variable names.
    READ (FMT='(A64)', UNIT=100, ERR=9999, IOSTAT=IOST) (ST(II), II=1,NST)     ! Read the station names.
    READ (FMT='(F10.5)', UNIT=100, ERR=9999, IOSTAT=IOST) (LON(II), II=1,NST)  ! Read the station longitudes.
    READ (FMT='(F10.5)', UNIT=100, ERR=9999, IOSTAT=IOST) (LAT(II), II=1,NST)  ! Read the station latitudes.
    READ (FMT='(F10.5)', UNIT=100, ERR=9999, IOSTAT=IOST) (T(II), II=1,NT)     ! Read the times.

    !
    ! Read all the data. Don't know how (or if) this can be done with an implicit do loop, therefore will do it the old fashioned
    ! way.
    !
    DO KK=1,NT
        DO JJ=1,NST
            DO II=1,NV
                READ(FMT='(F10.3)', UNIT=100, ERR=9999, IOSTAT=IOST) D(II,JJ,KK)
            END DO
        END DO
    END DO

    !
    ! Close the data file.
    !
    CLOSE (UNIT=100, ERR=9999, IOSTAT=IOST, STATUS='KEEP')

    !
    ! The next block of code deals with IO errors (if they occurred).
    !
    9999 CONTINUE
    IF (IOST .GT. 0) THEN
        PRINT *,'E: An IO error occurred while reading ',INPUTFILE
        STOP '1'
    END IF

    END SUBROUTINE

    !*******************************************************************************************************************************
    ! OPT_VHT_NETCDF
    !
    ! This subroutine is based on the OPT_VHT subroutine, but outputs in NetCDF format.
    !
    !
    ! Tim Hume.
    ! 11 August 2006.
    !*******************************************************************************************************************************
    SUBROUTINE OPT_VHT_NETCDF(OUTPUTFILE, INSTRUMENT, NV, NP, NST, NT, V, P, ST, T, D)
    USE PORTABLE
    USE NETCDF

    IMPLICIT NONE

    CHARACTER (LEN=*), INTENT(IN)                                       :: OUTPUTFILE   ! Name of the output file.
    CHARACTER (LEN=*), INTENT(IN)                                       :: INSTRUMENT   ! Instrument name.
    INTEGER (KIND=IK4), INTENT(IN)                                      :: NV           ! Number of variables?
    INTEGER (KIND=IK4), INTENT(IN)                                      :: NP           ! Number of pressure levels.
    INTEGER (KIND=IK4), INTENT(IN)                                      :: NST          ! Number of stations.
    INTEGER (KIND=IK4), INTENT(IN)                                      :: NT           ! Number of time steps.
    CHARACTER (LEN=*), DIMENSION(NV),  INTENT(IN)                       :: V            ! Variable names?
    REAL (KIND=RK8), DIMENSION(NP), INTENT(IN)                          :: P            ! Values of the pressure levels.
    CHARACTER (LEN=*), DIMENSION(NST), INTENT(IN)                       :: ST           ! Station names?
    REAL (KIND=RK8), DIMENSION(NT), INTENT(IN)                          :: T            ! Time values?
    REAL (KIND=RK8), DIMENSION(NV,NP,NST,NT), INTENT(IN)                :: D            ! The data.

    !
    ! Local variables.
    !
    INTEGER (KIND=IK4)          :: NCID                                                 ! ID of NetCDF file.
    INTEGER (KIND=IK4)          :: V_DIM_ID, P_DIM_ID, ST_DIM_ID, T_DIM_ID, STR_DIM_ID  ! Dimension IDs.
    INTEGER (KIND=IK4)          :: V_VAR_ID, P_VAR_ID, ST_VAR_ID, T_VAR_ID, DATA_VAR_ID ! Variable IDs.
    INTEGER (KIND=IK4)          :: IOST                                                 ! I/O status.
    INTEGER (KIND=IK4)          :: II                                                   ! Counter.
    INTEGER (KIND=IK4)          :: STRLEN                                               ! String length.
    CHARACTER (LEN=64)          :: TMPSTR                                               ! Temporary string.

    IOST    = NF90_NOERR
    !
    ! Create the NetCDF file.
    !
    IOST    = NF90_CREATE(OUTPUTFILE, NF90_NOCLOBBER, NCID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    !
    ! Define the dimensions.
    !
    IOST    = NF90_DEF_DIM(NCID, "variables", NV, V_DIM_ID)     ! The data array contains many variables.
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_DIM(NCID, "levels", NP, P_DIM_ID)        ! Vertical levels.
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_DIM(NCID, "stations", NST, ST_DIM_ID)    ! Stations.
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_DIM(NCID, "time", NT, T_DIM_ID)          ! Time steps.
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_DIM(NCID, "string", 64, STR_DIM_ID)      ! This dimension is used for character strings.
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    !
    ! Define the variables.
    !
    IOST    = NF90_DEF_VAR(NCID, "variables", NF90_CHAR, (/ STR_DIM_ID, V_DIM_ID /), V_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "levels", NF90_DOUBLE, (/ P_DIM_ID /), P_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "stations", NF90_CHAR, (/ STR_DIM_ID, ST_DIM_ID /), ST_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "time", NF90_DOUBLE, (/ T_DIM_ID /), T_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "data", NF90_DOUBLE, (/ V_DIM_ID, P_DIM_ID, ST_DIM_ID, T_DIM_ID /), DATA_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    !
    ! Write the instrument type as a global attribute. Make sure the string is null terminated (for C programs).
    !
    IOST    = NF90_PUT_ATT(NCID, NF90_GLOBAL, "instrument", TRIM(INSTRUMENT)//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    !
    ! We've finished defining stuff, leave the definition mode.
    !
    IOST    = NF90_ENDDEF(NCID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    !
    ! Write the names of the variables. Terminate strings with a null byte, in case the data are subsequently read by a C program.
    !
    DO II=1,NV
        TMPSTR  = TRIM(V(II))
        STRLEN  = MIN(63, LEN_TRIM(TMPSTR))           ! The characters after this positon will be replaced by null characters.
        TMPSTR(STRLEN+1:64)    = REPEAT(CHAR(0), 64-STRLEN)
        IOST    = NF90_PUT_VAR(NCID, V_VAR_ID, TMPSTR, (/ 1, II /), (/ 64, 1 /))
        IF (IOST .NE. NF90_NOERR) GO TO 9999
    END DO

    !
    ! Write the names of the stations. Terminate strings with a null byte, in case the data are subsequently read by a C program.
    !
    DO II=1,NST
        TMPSTR  = TRIM(ST(II))
        STRLEN  = MIN(63, LEN_TRIM(TMPSTR))           ! The characters after this positon will be replaced by null characters.
        TMPSTR(STRLEN+1:64)    = REPEAT(CHAR(0) , 64-STRLEN)
        IOST    = NF90_PUT_VAR(NCID, ST_VAR_ID, TMPSTR, (/ 1, II /), (/ 64, 1 /))
        IF (IOST .NE. NF90_NOERR) GO TO 9999
    END DO

    !
    ! Write the pressure levels.
    !
    IOST    = NF90_PUT_VAR(NCID, P_VAR_ID, P)
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    !
    ! Write the time steps.
    !
    IOST    = NF90_PUT_VAR(NCID, T_VAR_ID, T)
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    !
    ! Write the big data array.
    !
    IOST    = NF90_PUT_VAR(NCID, DATA_VAR_ID, D)
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    !
    ! Close the NetCDF file.
    !
    IOST    = NF90_CLOSE(NCID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    !
    ! Catch any NetCDF errors here.
    !
    9999 IF (IOST .NE. NF90_NOERR) THEN
        PRINT *,'E: Problem creating NetCDF file ',OUTPUTFILE
        PRINT *, TRIM(NF90_STRERROR(IOST))
        STOP '1'
    END IF

    END SUBROUTINE OPT_VHT_NETCDF

    !*******************************************************************************************************************************
    ! OPT_3D_NETCDF
    !
    ! This subroutine writes the data from 3D_PUT to a NetCDF file. We'll need a corresponding subroutine to read the data into the
    ! variational analysis.
    !
    ! Tim Hume.
    ! 2 October 2006.
    !*******************************************************************************************************************************

    SUBROUTINE OPT_3D_NETCDF(OUTPUTFILE, DU, VU, DS, P, T, STN, WEIGHT, BOUNDARY, STRU, STRS)
    USE PORTABLE
    USE NETCDF

    IMPLICIT NONE

    CHARACTER (LEN=*), INTENT(IN)                                       :: OUTPUTFILE   ! Name of the output file.
    CHARACTER (LEN=64), DIMENSION(:), INTENT(IN)                        :: VU           ! Names of the variables in DU.
    REAL (KIND=RK8), DIMENSION(:,:,:), INTENT(IN)                       :: DS           ! Surface level data.
    REAL (KIND=RK8), DIMENSION(:), INTENT(IN)                           :: P            ! Pressure levels.
    REAL (KIND=RK8), DIMENSION(:), INTENT(IN)                           :: T            ! Time steps.          
    CHARACTER (LEN=64), DIMENSION(:), INTENT(IN)                        :: STN          ! Names of the stations.
    REAL (KIND=RK8), DIMENSION(:), INTENT(IN)                           :: WEIGHT       ! Weights for each station.
    INTEGER (KIND=IK4), DIMENSION(:,:), INTENT(IN)                      :: BOUNDARY     ! BOUNDARY array.
    INTEGER (KIND=IK4), DIMENSION(:), INTENT(IN)                        :: STRU         ! STRU array.
    INTEGER (KIND=IK4), DIMENSION(:), INTENT(IN)                        :: STRS         ! STRS array.
    !
    ! Local variables.
    !
    INTEGER (KIND=IK4)          :: NCID                                                 ! ID of NetCDF file.
    INTEGER (KIND=IK4)          :: V_DIM_ID, P_DIM_ID, ST_DIM_ID, T_DIM_ID, STR_DIM_ID, BND_DIM_ID
    INTEGER (KIND=IK4)          :: STRU_DIM_ID, STRS_DIM_ID, WEIGHT_DIM_ID
    INTEGER (KIND=IK4)          :: V_VAR_ID, P_VAR_ID, ST_VAR_ID, T_VAR_ID, DU_VAR_ID, DS_VAR_ID, BOUNDARY_VAR_ID
    INTEGER (KIND=IK4)          :: STRU_VAR_ID, STRS_VAR_ID, WEIGHT_VAR_ID
    INTEGER (KIND=IK4)          :: IOST                                                 ! I/O status.
    INTEGER (KIND=IK4)          :: II                                                   ! Counter.
    INTEGER (KIND=IK4)          :: STRLEN                                               ! String length.
    CHARACTER (LEN=64)          :: TMPSTR                                               ! Temporary string.
    REAL (KIND=RK8), DIMENSION(:,:,:,:),INTENT(IN)                                 :: DU           ! The 3D data.
    IOST    = NF90_NOERR
    !
    ! Create the NetCDF file.
    !
    IOST    = NF90_CREATE(OUTPUTFILE, NF90_NOCLOBBER, NCID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    !
    ! Define the dimensions.
    !
    IOST    = NF90_DEF_DIM(NCID, "variables", SIZE(DU, DIM=1), V_DIM_ID)    ! The data array contains many variables.
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_DIM(NCID, "levels", SIZE(DU, DIM=2), P_DIM_ID)       ! Vertical levels.
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_DIM(NCID, "stations", SIZE(DU, DIM=3), ST_DIM_ID)    ! Stations.
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_DIM(NCID, "time", SIZE(DU, DIM=4), T_DIM_ID)         ! Time steps.
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_DIM(NCID, "string", 64, STR_DIM_ID)                  ! This dimension is used for character strings.
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_DIM(NCID, "bnd", SIZE(BOUNDARY, DIM=1), BND_DIM_ID)  ! This dimension is used for the BOUNDARY array.
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_DIM(NCID, "stru", SIZE(STRU), STRU_DIM_ID)           ! This dimension is used for the STRU array.
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_DIM(NCID, "strs", SIZE(STRS), STRS_DIM_ID)           ! This dimension is used for the STRS array.
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_DIM(NCID, "weight", SIZE(WEIGHT), WEIGHT_DIM_ID)     ! This dimension is used for the WEIGHT array.
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    !
    ! Define the variables.
    !
    IOST    = NF90_DEF_VAR(NCID, "variables", NF90_CHAR, (/ STR_DIM_ID, V_DIM_ID /), V_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "levels", NF90_DOUBLE, (/ P_DIM_ID /), P_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "stations", NF90_CHAR, (/ STR_DIM_ID, ST_DIM_ID /), ST_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "time", NF90_DOUBLE, (/ T_DIM_ID /), T_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "du", NF90_DOUBLE, (/ V_DIM_ID, P_DIM_ID, ST_DIM_ID, T_DIM_ID /), DU_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "ds", NF90_DOUBLE, (/ V_DIM_ID, ST_DIM_ID, T_DIM_ID /), DS_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "boundary", NF90_INT, (/ BND_DIM_ID, T_DIM_ID /), BOUNDARY_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "stru", NF90_INT, (/ STRU_DIM_ID /), STRU_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "strs", NF90_INT, (/ STRS_DIM_ID /), STRS_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "weight", NF90_DOUBLE, (/ WEIGHT_DIM_ID /), WEIGHT_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    !
    ! Create various attributes. Make sure strings are null terminated (for C programs).
    !
!    IOST    = NF90_PUT_ATT(NCID, NF90_GLOBAL, "instrument", TRIM(INSTRUMENT)//CHAR(0))
!    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, T_VAR_ID, "units", 'Days since 2004-10-01T00:00:00 UTC'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, P_VAR_ID, "units", 'hPa'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    !
    ! We've finished defining stuff, leave the definition mode.
    !
    IOST    = NF90_ENDDEF(NCID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    !
    ! Write the names of the variables. Terminate strings with a null byte, in case the data are subsequently read by a C program.
    !
    IF (SIZE(DU, DIM=1) .NE. SIZE(VU)) THEN
        PRINT *,'E: Size of variable dimension in DU is different than VU'
        STOP '1'
    END IF

    DO II=1,SIZE(DU, DIM=1)
        TMPSTR      = TRIM(VU(II))
        STRLEN      = MIN(63, LEN_TRIM(TMPSTR))           ! The characters after this positon will be replaced by null characters.
        TMPSTR(STRLEN+1:64)    = REPEAT(CHAR(0), 64-STRLEN)
        IOST        = NF90_PUT_VAR(NCID, V_VAR_ID, TMPSTR, (/ 1, II /), (/ 64, 1 /))
        IF (IOST .NE. NF90_NOERR) GO TO 9999
    END DO

    !
    ! Write the names of the stations. Terminate strings with a null byte, in case the data are subsequently read by a C program.
    !
    DO II=1,SIZE(DU, DIM=3)
        TMPSTR  = TRIM(STN(II))
        STRLEN  = MIN(63, LEN_TRIM(TMPSTR))           ! The characters after this positon will be replaced by null characters.
        TMPSTR(STRLEN+1:64)    = REPEAT(CHAR(0), 64-STRLEN)
        IOST    = NF90_PUT_VAR(NCID, ST_VAR_ID, TMPSTR, (/ 1, II /), (/ 64, 1 /))
        IF (IOST .NE. NF90_NOERR) GO TO 9999
    END DO

    !
    ! Write the pressure levels.
    !
    IOST    = NF90_PUT_VAR(NCID, P_VAR_ID, P)
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    !
    ! Write the time steps.
    !
    IOST    = NF90_PUT_VAR(NCID, T_VAR_ID, T)
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    !
    ! Write the data arrays.
    !
    IOST    = NF90_PUT_VAR(NCID, DU_VAR_ID, DU)
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_PUT_VAR(NCID, DS_VAR_ID, DS)
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_PUT_VAR(NCID, BOUNDARY_VAR_ID, BOUNDARY)
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_PUT_VAR(NCID, STRU_VAR_ID, STRU)
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_PUT_VAR(NCID, STRS_VAR_ID, STRS)
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_PUT_VAR(NCID, WEIGHT_VAR_ID, WEIGHT)
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    !
    ! Close the NetCDF file.
    !
    IOST    = NF90_CLOSE(NCID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    !
    ! Catch any NetCDF errors here.
    !
    9999 CONTINUE
    IF (IOST .NE. NF90_NOERR) THEN
        PRINT *,'E: Problem creating NetCDF file ',OUTPUTFILE
        PRINT *, TRIM(NF90_STRERROR(IOST))
        STOP '1'
    END IF

    END SUBROUTINE

    !*******************************************************************************************************************************
    ! IPT_3D_NETCDF
    !
    ! This subroutine reads the NetCDF file written by OPT_3D_NETCDF.
    !
    ! Tim Hume.
    ! 22 March 2007
    !*******************************************************************************************************************************

    SUBROUTINE IPT_3D_NETCDF(INPUTFILE, DU, VU, DS, T, STN, WEIGHT, LEV, BOUNDARY, STRU, STRS)
    USE PORTABLE
    USE NETCDF

    IMPLICIT NONE

    CHARACTER (LEN=*), INTENT(IN)                                           :: INPUTFILE    ! Name of the input file.
    REAL (KIND=RK8), DIMENSION(:,:,:,:), INTENT(OUT), ALLOCATABLE, OPTIONAL :: DU           ! The 3D data.
    CHARACTER (LEN=64), DIMENSION(:), INTENT(OUT), ALLOCATABLE, OPTIONAL    :: VU           ! Names of the variables in DU.
    REAL (KIND=RK8), DIMENSION(:,:,:), INTENT(OUT), ALLOCATABLE, OPTIONAL   :: DS           ! Surface level data.
    REAL (KIND=RK8), DIMENSION(:), INTENT(OUT), ALLOCATABLE, OPTIONAL       :: T            ! Time steps.          
    CHARACTER (LEN=64), DIMENSION(:), INTENT(OUT), ALLOCATABLE, OPTIONAL    :: STN          ! Names of the stations.
    REAL (KIND=RK8), DIMENSION(:), INTENT(OUT), ALLOCATABLE, OPTIONAL       :: LEV          ! Vertical levels in analysis.
    INTEGER (KIND=IK4), DIMENSION(:,:), INTENT(OUT), ALLOCATABLE, OPTIONAL  :: BOUNDARY     ! Boundary array.
    INTEGER (KIND=IK4), DIMENSION(:), INTENT(OUT), ALLOCATABLE, OPTIONAL    :: STRU         ! STRU array.
    INTEGER (KIND=IK4), DIMENSION(:), INTENT(OUT), ALLOCATABLE, OPTIONAL    :: STRS         ! STRS array.
    REAL (KIND=RK8), DIMENSION(:), INTENT(OUT), ALLOCATABLE, OPTIONAL       :: WEIGHT       ! WEIGHT array.

    !
    ! Local variables.
    !
    INTEGER (KIND=IK4)          :: NCID                                                 ! ID of NetCDF file.

    INTEGER (KIND=IK4)          :: T_DIM_ID                                             ! Time dimension ID.
    INTEGER (KIND=IK4)          :: T_DIM_LEN                                            ! Number of time steps.
    INTEGER (KIND=IK4)          :: STN_DIM_ID                                           ! Station dimension ID.
    INTEGER (KIND=IK4)          :: STN_DIM_LEN                                          ! Number of stations.
    INTEGER (KIND=IK4)          :: LEV_DIM_ID                                           ! Vertical level dimension ID.
    INTEGER (KIND=IK4)          :: LEV_DIM_LEN                                          ! Number of vertical levels.
    INTEGER (KIND=IK4)          :: VAR_DIM_ID                                           ! Variable dimension ID.
    INTEGER (KIND=IK4)          :: VAR_DIM_LEN                                          ! Number of variables.
    INTEGER (KIND=IK4)          :: STR_DIM_ID                                           ! String dimension ID.
    INTEGER (KIND=IK4)          :: STR_DIM_LEN                                          ! String length.
    INTEGER (KIND=IK4)          :: BND_DIM_ID                                           ! Dimension used for BOUNDARY array.
    INTEGER (KIND=IK4)          :: BND_DIM_LEN                                          ! Length of BND dimension.
    INTEGER (KIND=IK4)          :: STRU_DIM_ID                                          ! Dimension used for STRU array.
    INTEGER (KIND=IK4)          :: STRU_DIM_LEN                                         ! Length of STRU dimension.
    INTEGER (KIND=IK4)          :: STRS_DIM_ID                                          ! Dimension used for STRS array.
    INTEGER (KIND=IK4)          :: STRS_DIM_LEN                                         ! Length of STRS dimension.
    INTEGER (KIND=IK4)          :: WEIGHT_DIM_ID                                        ! Dimension used for WEIGHT array.
    INTEGER (KIND=IK4)          :: WEIGHT_DIM_LEN                                       ! Length of WEIGHT dimension.

    INTEGER (KIND=IK4)          :: T_VAR_ID                                             ! Time variable ID.
    INTEGER (KIND=IK4)          :: DU_VAR_ID                                            ! 3D data variable ID.
    INTEGER (KIND=IK4)          :: DS_VAR_ID                                            ! Surface data variable ID.
    INTEGER (KIND=IK4)          :: STN_VAR_ID                                           ! Station names variable ID.
    INTEGER (KIND=IK4)          :: LEV_VAR_ID                                           ! Vertical levels variable ID.
    INTEGER (KIND=IK4)          :: VU_VAR_ID                                            ! Variable names variable ID.
    INTEGER (KIND=IK4)          :: BOUNDARY_VAR_ID                                      ! ID of BOUNDARY variable.
    INTEGER (KIND=IK4)          :: STRU_VAR_ID                                          ! ID of STRU variable.
    INTEGER (KIND=IK4)          :: STRS_VAR_ID                                          ! ID of STRS variable.
    INTEGER (KIND=IK4)          :: WEIGHT_VAR_ID                                        ! ID of WEIGHT variable.

    INTEGER (KIND=IK4)          :: MEMST                                                ! Status of memory allocations.
    INTEGER (KIND=IK4)          :: IOST                                                 ! I/O status.
    LOGICAL                     :: NCERROR

    IOST    = NF90_NOERR
    NCERROR = .FALSE.
    !
    ! Open the NetCDF file.
    !
    IOST    = NF90_OPEN(INPUTFILE, NF90_NOWRITE, NCID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    !
    ! Get the dimension IDs, and the length of the dimensions.
    !
    IOST    = NF90_INQ_DIMID(NCID, "time", T_DIM_ID)                            ! Time steps.
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_INQUIRE_DIMENSION(NCID, T_DIM_ID, LEN=T_DIM_LEN)             ! Get the number of time steps.
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_INQ_DIMID(NCID, "stations", STN_DIM_ID)                      ! Time steps.
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_INQUIRE_DIMENSION(NCID, STN_DIM_ID, LEN=STN_DIM_LEN)         ! Get the number of stations.
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_INQ_DIMID(NCID, "levels", LEV_DIM_ID)                        ! Vertical levels.
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_INQUIRE_DIMENSION(NCID, LEV_DIM_ID, LEN=LEV_DIM_LEN)         ! Number of vertical levels.
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_INQ_DIMID(NCID, "variables", VAR_DIM_ID)                     ! Variables.
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_INQUIRE_DIMENSION(NCID, VAR_DIM_ID, LEN=VAR_DIM_LEN)         ! Get the number of variables.
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_INQ_DIMID(NCID, "string", STR_DIM_ID)                        ! String dimension.
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_INQUIRE_DIMENSION(NCID, STR_DIM_ID, LEN=STR_DIM_LEN)         ! String length.
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_INQ_DIMID(NCID, "bnd", BND_DIM_ID)                           ! bnd dimension.
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_INQUIRE_DIMENSION(NCID, BND_DIM_ID, LEN=BND_DIM_LEN)         ! bnd dimension length (should be 3).
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_INQ_DIMID(NCID, "stru", STRU_DIM_ID)                         ! stru dimension.
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_INQUIRE_DIMENSION(NCID, STRU_DIM_ID, LEN=STRU_DIM_LEN)       ! stru dimension length.
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_INQ_DIMID(NCID, "strs", STRS_DIM_ID)                         ! strs dimension.
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_INQUIRE_DIMENSION(NCID, STRS_DIM_ID, LEN=STRS_DIM_LEN)       ! strs dimension length.
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_INQ_DIMID(NCID, "weight", WEIGHT_DIM_ID)                     ! weight dimension.
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_INQUIRE_DIMENSION(NCID, WEIGHT_DIM_ID, LEN=WEIGHT_DIM_LEN)   ! weight dimension length.
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    !
    ! Read in the data. Only read those variables which the user requests in the optional subroutine arguments.
    ! When allocating memory, be paranoid, check it hasn't already been allocated.
    !
    IF (PRESENT(DU)) THEN
        IF (ALLOCATED(DU))          DEALLOCATE(DU)
        ALLOCATE(DU(VAR_DIM_LEN,LEV_DIM_LEN,STN_DIM_LEN,T_DIM_LEN), STAT=MEMST)
        IF (MEMST .NE. 0) THEN
            PRINT *,'E: IPT_3D_NETCDF: Unable to allocate memory to hold DU'
            STOP '1'
        END IF
        IOST = NF90_INQ_VARID(NCID, "du", DU_VAR_ID)
        IF (IOST .NE. NF90_NOERR) GO TO 9999
        IOST = NF90_GET_VAR(NCID, DU_VAR_ID, DU)
        IF (IOST .NE. NF90_NOERR) GO TO 9999
    END IF
    IF (PRESENT(VU)) THEN
        IF (ALLOCATED(VU))          DEALLOCATE(VU)
        ALLOCATE(VU(VAR_DIM_LEN), STAT=MEMST)
        IF (MEMST .NE. 0) THEN
            PRINT *,'E: IPT_3D_NETCDF: Unable to allocate memory to hold VU'
            STOP '1'
        END IF
        IOST = NF90_INQ_VARID(NCID, "variables", VU_VAR_ID)
        IF (IOST .NE. NF90_NOERR) GO TO 9999
        IOST = NF90_GET_VAR(NCID, VU_VAR_ID, VU)
        IF (IOST .NE. NF90_NOERR) GO TO 9999
    END IF

    IF (PRESENT(DS)) THEN
        IF (ALLOCATED(DS))          DEALLOCATE(DS)
        ALLOCATE(DS(VAR_DIM_LEN,STN_DIM_LEN,T_DIM_LEN), STAT=MEMST)
        IF (MEMST .NE. 0) THEN
            PRINT *,'E: IPT_3D_NETCDF: Unable to allocate memory to hold DS'
            STOP '1'
        END IF
        IOST = NF90_INQ_VARID(NCID, "ds", DS_VAR_ID)
        IF (IOST .NE. NF90_NOERR) GO TO 9999
        IOST = NF90_GET_VAR(NCID, DS_VAR_ID, DS)
        IF (IOST .NE. NF90_NOERR) GO TO 9999
    END IF

    IF (PRESENT(T)) THEN
        IF (ALLOCATED(T))           DEALLOCATE(T)
        ALLOCATE(T(T_DIM_LEN), STAT=MEMST)
        IF (MEMST .NE. 0) THEN
            PRINT *,'E: IPT_3D_NETCDF: Unable to allocate memory to hold T'
            STOP '1'
        END IF
        IOST = NF90_INQ_VARID(NCID, "time", T_VAR_ID)
        IF (IOST .NE. NF90_NOERR) GO TO 9999
        IOST = NF90_GET_VAR(NCID, T_VAR_ID, T)
        IF (IOST .NE. NF90_NOERR) GO TO 9999
    END IF

    IF (PRESENT(STN)) THEN
        IF (ALLOCATED(STN))         DEALLOCATE(STN)
        ALLOCATE(STN(STN_DIM_LEN), STAT=MEMST)
        IF (MEMST .NE. 0) THEN
            PRINT *,'E: IPT_3D_NETCDF: Unable to allocate memory to hold STN'
            STOP '1'
        END IF
        IOST = NF90_INQ_VARID(NCID, "stations", STN_VAR_ID)
        IF (IOST .NE. NF90_NOERR) GO TO 9999
        IOST = NF90_GET_VAR(NCID, STN_VAR_ID, STN)
        IF (IOST .NE. NF90_NOERR) GO TO 9999
    END IF

    IF (PRESENT(LEV)) THEN
        IF (ALLOCATED(LEV))         DEALLOCATE(LEV)
        ALLOCATE(LEV(LEV_DIM_LEN), STAT=MEMST)
        IF (MEMST .NE. 0) THEN
            PRINT *,'E: IPT_3D_NETCDF: Unable to allocate memory to hold LEV'
            STOP '1'
        END IF
        IOST = NF90_INQ_VARID(NCID, "levels", LEV_VAR_ID)
        IF (IOST .NE. NF90_NOERR) GO TO 9999
        IOST = NF90_GET_VAR(NCID, LEV_VAR_ID, LEV)
        IF (IOST .NE. NF90_NOERR) GO TO 9999
    END IF

    IF (PRESENT(BOUNDARY)) THEN
        IF (ALLOCATED(BOUNDARY))    DEALLOCATE(BOUNDARY)
        ALLOCATE(BOUNDARY(BND_DIM_LEN,T_DIM_LEN), STAT=MEMST)
        IF (MEMST .NE. 0) THEN
            PRINT *,'E: IPT_3D_NETCDF: Unable to allocate memory to hold BOUNDARY'
            STOP '1'
        END IF
        IOST = NF90_INQ_VARID(NCID, "boundary", BOUNDARY_VAR_ID)
        IF (IOST .NE. NF90_NOERR) GO TO 9999
        IOST = NF90_GET_VAR(NCID, BOUNDARY_VAR_ID, BOUNDARY)
        IF (IOST .NE. NF90_NOERR) GO TO 9999
    END IF

    IF (PRESENT(STRU)) THEN
        IF (ALLOCATED(STRU))        DEALLOCATE(STRU)
        ALLOCATE(STRU(STRU_DIM_LEN), STAT=MEMST)
        IF (MEMST .NE. 0) THEN
            PRINT *,'E: IPT_3D_NETCDF: Unable to allocate memory to hold STRU'
            STOP '1'
        END IF
        IOST = NF90_INQ_VARID(NCID, "stru", STRU_VAR_ID)
        IF (IOST .NE. NF90_NOERR) GO TO 9999
        IOST = NF90_GET_VAR(NCID, STRU_VAR_ID, STRU)
        IF (IOST .NE. NF90_NOERR) GO TO 9999
    END IF

    IF (PRESENT(STRS)) THEN
        IF (ALLOCATED(STRS))        DEALLOCATE(STRS)
        ALLOCATE(STRS(STRS_DIM_LEN), STAT=MEMST)
        IF (MEMST .NE. 0) THEN
            PRINT *,'E: IPT_3D_NETCDF: Unable to allocate memory to hold STRS'
            STOP '1'
        END IF
        IOST = NF90_INQ_VARID(NCID, "strs", STRS_VAR_ID)
        IF (IOST .NE. NF90_NOERR) GO TO 9999
        IOST = NF90_GET_VAR(NCID, STRS_VAR_ID, STRS)
        IF (IOST .NE. NF90_NOERR) GO TO 9999
    END IF

    IF (PRESENT(WEIGHT)) THEN
        IF (ALLOCATED(WEIGHT))        DEALLOCATE(WEIGHT)
        ALLOCATE(WEIGHT(WEIGHT_DIM_LEN), STAT=MEMST)
        IF (MEMST .NE. 0) THEN
            PRINT *,'E: IPT_3D_NETCDF: Unable to allocate memory to hold WEIGHT'
            STOP '1'
        END IF
        IOST = NF90_INQ_VARID(NCID, "weight", WEIGHT_VAR_ID)
        IF (IOST .NE. NF90_NOERR) GO TO 9999
        IOST = NF90_GET_VAR(NCID, WEIGHT_VAR_ID, WEIGHT)
        IF (IOST .NE. NF90_NOERR) GO TO 9999
    END IF

    !
    ! Close the NetCDF file.
    !
    IOST    = NF90_CLOSE(NCID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    !
    ! Catch any NetCDF errors here.
    !
    9999 CONTINUE
    IF (NCERROR) THEN
        PRINT *,'E: IPT_3D_NETCDF: NetCDF error encountered'
        IF (IOST .NE. NF90_NOERR) THEN
            PRINT *, TRIM(NF90_STRERROR(IOST))
        END IF
        STOP '1'
    END IF

    END SUBROUTINE IPT_3D_NETCDF

    !*******************************************************************************************************************************
    ! OPT_BUDGET_NETCDF
    !
    ! This subroutine writes the data from BUDGET_PUT to a NetCDF file. 
    !
    ! Tim Hume.
    ! 27 March 2007.
    !*******************************************************************************************************************************

    SUBROUTINE OPT_BUDGET_NETCDF(OUTPUTFILE, BUDGET_COLUMN, BUDGET_LAYER, VBUDGET_COLUMN, VBUDGET_LAYER, AVE_QS, AVE_SS, P, T)
    USE PORTABLE
    USE NETCDF

    IMPLICIT NONE

    CHARACTER (LEN=*), INTENT(IN)                                       :: OUTPUTFILE       ! Name of the output file.
    REAL (KIND=RK8), DIMENSION(:,:,:), INTENT(IN)                       :: BUDGET_COLUMN
    REAL (KIND=RK8), DIMENSION(:,:,:,:), INTENT(IN)                     :: BUDGET_LAYER
    CHARACTER (LEN=64), DIMENSION(:,:), INTENT(IN)                      :: VBUDGET_COLUMN
    CHARACTER (LEN=64), DIMENSION(:,:), INTENT(IN)                      :: VBUDGET_LAYER
    REAL (KIND=RK8), DIMENSION(:), INTENT(IN)                           :: AVE_QS
    REAL (KIND=RK8), DIMENSION(:), INTENT(IN)                           :: AVE_SS
    REAL (KIND=RK8), DIMENSION(:), INTENT(IN)                           :: P
    REAL (KIND=RK8), DIMENSION(:), INTENT(IN)                           :: T

    !
    ! Local variables.
    !
    INTEGER (KIND=IK4)          :: NCID                                                 ! ID of NetCDF file.
    INTEGER (KIND=IK4)          :: BCV_DIM_ID, BCT_DIM_ID, BLV_DIM_ID, BLT_DIM_ID, P_DIM_ID, T_DIM_ID, STR_DIM_ID
    INTEGER (KIND=IK4)          :: BC_VAR_ID, BL_VAR_ID, VBC_VAR_ID, VBL_VAR_ID, AVE_QS_VAR_ID, AVE_SS_VAR_ID, P_VAR_ID, T_VAR_ID
    INTEGER (KIND=IK4)          :: IOST                                                 ! I/O status.
    INTEGER (KIND=IK4)          :: II, JJ                                               ! Counter.
    INTEGER (KIND=IK4)          :: STRLEN                                               ! String length.
    CHARACTER (LEN=64)          :: TMPSTR                                               ! Temporary string.

    !
    ! Create the NetCDF file.
    !
    IOST    = NF90_NOERR
    IOST    = NF90_CREATE(OUTPUTFILE, NF90_NOCLOBBER, NCID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    !
    ! Define the dimensions.
    !
    IOST    = NF90_DEF_DIM(NCID, "bcv", SIZE(BUDGET_COLUMN, DIM=1), BCV_DIM_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_DIM(NCID, "bct", SIZE(BUDGET_COLUMN, DIM=2), BCT_DIM_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_DIM(NCID, "blv", SIZE(BUDGET_LAYER, DIM=1), BLV_DIM_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_DIM(NCID, "blt", SIZE(BUDGET_LAYER, DIM=2), BLT_DIM_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_DIM(NCID, "levels", SIZE(P, DIM=1), P_DIM_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_DIM(NCID, "time", SIZE(T, DIM=1), T_DIM_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_DIM(NCID, "string", 64, STR_DIM_ID)                  ! This dimension is used for character strings.
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    !
    ! Define the variables.
    !
    IOST    = NF90_DEF_VAR(NCID, "budget_column", NF90_DOUBLE, (/ BCV_DIM_ID, BCT_DIM_ID, T_DIM_ID /), BC_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "budget_layer", NF90_DOUBLE, (/ BLV_DIM_ID, BLT_DIM_ID, P_DIM_ID, T_DIM_ID /), BL_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "vbudget_column", NF90_CHAR, (/ STR_DIM_ID, BCV_DIM_ID, BCT_DIM_ID /), VBC_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "vbudget_layer", NF90_CHAR, (/ STR_DIM_ID, BLV_DIM_ID, BLT_DIM_ID /), VBL_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "ave_qs", NF90_DOUBLE, (/ T_DIM_ID /), AVE_QS_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "ave_ss", NF90_DOUBLE, (/ T_DIM_ID /), AVE_SS_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "levels", NF90_DOUBLE, (/ P_DIM_ID /), P_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "time", NF90_DOUBLE, (/ T_DIM_ID /), T_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    !
    ! Create various attributes. Make sure strings are null terminated (for C programs).
    !
    IOST    = NF90_PUT_ATT(NCID, T_VAR_ID, "units", 'Days since 2004-10-01T00:00:00 UTC'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, P_VAR_ID, "units", 'hPa'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    !
    ! We've finished defining stuff, leave the definition mode.
    !
    IOST    = NF90_ENDDEF(NCID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    !
    ! Write VBUDGET_COLUMN and VBUDGET_LAYER. Terminate strings with a null byte, in case the data are subsequently 
    ! read by a C program.
    !
    DO II=1,SIZE(BUDGET_COLUMN, DIM=1)
        DO JJ=1,SIZE(BUDGET_COLUMN, DIM=2)
            TMPSTR      = TRIM(VBUDGET_COLUMN(II,JJ))
            STRLEN      = MIN(63, LEN_TRIM(TMPSTR))         ! The characters after this positon will be replaced by null characters.
            TMPSTR(STRLEN+1:64)    = REPEAT(CHAR(0), 64-STRLEN)
            IOST        = NF90_PUT_VAR(NCID, VBC_VAR_ID, TMPSTR, (/ 1, II, JJ /), (/ 64, 1, 1 /))
            IF (IOST .NE. NF90_NOERR) GO TO 9999
        END DO
    END DO

    DO II=1,SIZE(BUDGET_LAYER, DIM=1)
        DO JJ=1,SIZE(BUDGET_LAYER, DIM=2)
            TMPSTR      = TRIM(VBUDGET_LAYER(II,JJ))
            STRLEN      = MIN(63, LEN_TRIM(TMPSTR))         ! The characters after this positon will be replaced by null characters.
            TMPSTR(STRLEN+1:64)    = REPEAT(CHAR(0), 64-STRLEN)
            IOST        = NF90_PUT_VAR(NCID, VBL_VAR_ID, TMPSTR, (/ 1, II, JJ /), (/ 64, 1, 1 /))
            IF (IOST .NE. NF90_NOERR) GO TO 9999
        END DO
    END DO

    !
    ! Write the pressure levels.
    !
    IOST    = NF90_PUT_VAR(NCID, P_VAR_ID, P)
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    !
    ! Write the time steps.
    !
    IOST    = NF90_PUT_VAR(NCID, T_VAR_ID, T)
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    !
    ! Write the data arrays.
    !
    IOST    = NF90_PUT_VAR(NCID, BC_VAR_ID, BUDGET_COLUMN)
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_PUT_VAR(NCID, BL_VAR_ID, BUDGET_LAYER)
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_PUT_VAR(NCID, AVE_QS_VAR_ID, AVE_QS)
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_PUT_VAR(NCID, AVE_SS_VAR_ID, AVE_SS)
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    !
    ! Close the NetCDF file.
    !
    IOST    = NF90_CLOSE(NCID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    !
    ! Catch any NetCDF errors here.
    !
    9999 CONTINUE
    IF (IOST .NE. NF90_NOERR) THEN
        PRINT *,'E: Problem creating NetCDF file ',OUTPUTFILE
        PRINT *, TRIM(NF90_STRERROR(IOST))
        STOP '1'
    END IF

    END SUBROUTINE OPT_BUDGET_NETCDF

    !*******************************************************************************************************************************
    ! IPT_BUDGET_NETCDF
    !
    ! This subroutine reads the NetCDF file written by OPT_BUDGET_NETCDF.
    !
    ! Tim Hume.
    ! 27 March 2007
    !*******************************************************************************************************************************

    SUBROUTINE IPT_BUDGET_NETCDF(INPUTFILE, BUDGET_COLUMN, BUDGET_LAYER, VBUDGET_COLUMN, VBUDGET_LAYER, AVE_QS, AVE_SS, P, T)
    USE PORTABLE
    USE NETCDF

    IMPLICIT NONE

    CHARACTER (LEN=*), INTENT(IN)                                           :: INPUTFILE    ! Name of the input file.
    REAL (KIND=RK8), DIMENSION(:,:,:), INTENT(OUT), ALLOCATABLE, OPTIONAL   :: BUDGET_COLUMN
    REAL (KIND=RK8), DIMENSION(:,:,:,:), INTENT(OUT), ALLOCATABLE, OPTIONAL :: BUDGET_LAYER
    CHARACTER (LEN=64), DIMENSION(:,:), INTENT(OUT), ALLOCATABLE, OPTIONAL  :: VBUDGET_COLUMN
    CHARACTER (LEN=64), DIMENSION(:,:), INTENT(OUT), ALLOCATABLE, OPTIONAL  :: VBUDGET_LAYER
    REAL (KIND=RK8), DIMENSION(:), INTENT(OUT), ALLOCATABLE, OPTIONAL       :: AVE_QS
    REAL (KIND=RK8), DIMENSION(:), INTENT(OUT), ALLOCATABLE, OPTIONAL       :: AVE_SS
    REAL (KIND=RK8), DIMENSION(:), INTENT(OUT), ALLOCATABLE, OPTIONAL       :: P
    REAL (KIND=RK8), DIMENSION(:), INTENT(OUT), ALLOCATABLE, OPTIONAL       :: T

    !
    ! Local variables.
    !
    INTEGER (KIND=IK4)          :: NCID                                                 ! ID of NetCDF file.

    INTEGER (KIND=IK4)          :: BCV_DIM_ID, BCT_DIM_ID, BLV_DIM_ID, BLT_DIM_ID, P_DIM_ID, T_DIM_ID, STR_DIM_ID
    INTEGER (KIND=IK4)          :: BCV_DIM_LEN, BCT_DIM_LEN, BLV_DIM_LEN, BLT_DIM_LEN, P_DIM_LEN, T_DIM_LEN, STR_DIM_LEN
    INTEGER (KIND=IK4)          :: BC_VAR_ID, BL_VAR_ID, VBC_VAR_ID, VBL_VAR_ID, AVE_QS_VAR_ID, AVE_SS_VAR_ID, P_VAR_ID, T_VAR_ID
    INTEGER (KIND=IK4)          :: MEMST                                                ! Status of memory allocations.
    INTEGER (KIND=IK4)          :: IOST                                                 ! I/O status.
    LOGICAL                     :: NCERROR

    IOST    = NF90_NOERR
    NCERROR = .FALSE.
    !
    ! Open the NetCDF file.
    !
    IOST    = NF90_OPEN(INPUTFILE, NF90_NOWRITE, NCID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    !
    ! Get the dimension IDs, and the length of the dimensions.
    !
    IOST    = NF90_INQ_DIMID(NCID, "time", T_DIM_ID)                            ! Time steps.
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_INQUIRE_DIMENSION(NCID, T_DIM_ID, LEN=T_DIM_LEN)             ! Get the number of time steps.
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_INQ_DIMID(NCID, "levels", P_DIM_ID)                          ! Pressure levels.
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_INQUIRE_DIMENSION(NCID, P_DIM_ID, LEN=P_DIM_LEN)             ! Get number of levels.
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_INQ_DIMID(NCID, "string", STR_DIM_ID)                        ! String dimension.
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_INQUIRE_DIMENSION(NCID, STR_DIM_ID, LEN=STR_DIM_LEN)         ! String length.
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IF (STR_DIM_LEN .NE. 64) THEN
        PRINT *,'E: STRING dimension length is not 64'
        GO TO 9999
    END IF

    IOST    = NF90_INQ_DIMID(NCID, "bcv", BCV_DIM_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_INQUIRE_DIMENSION(NCID, BCV_DIM_ID, LEN=BCV_DIM_LEN)
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_INQ_DIMID(NCID, "bct", BCT_DIM_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_INQUIRE_DIMENSION(NCID, BCT_DIM_ID, LEN=BCT_DIM_LEN)
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_INQ_DIMID(NCID, "blv", BLV_DIM_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_INQUIRE_DIMENSION(NCID, BLV_DIM_ID, LEN=BLV_DIM_LEN)
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_INQ_DIMID(NCID, "blt", BLT_DIM_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_INQUIRE_DIMENSION(NCID, BLT_DIM_ID, LEN=BLT_DIM_LEN)
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    !
    ! Read in the data. Only read those variables which the user requests in the optional subroutine arguments.
    ! When allocating memory, be paranoid, check it hasn't already been allocated.
    !
    IF (PRESENT(BUDGET_COLUMN)) THEN
        IF (ALLOCATED(BUDGET_COLUMN))          DEALLOCATE(BUDGET_COLUMN)
        ALLOCATE(BUDGET_COLUMN(BCV_DIM_LEN,BCT_DIM_LEN,T_DIM_LEN), STAT=MEMST)
        IF (MEMST .NE. 0) THEN
            PRINT *,'E: IPT_BUDGET_NETCDF: Unable to allocate memory to hold BUDGET_COLUMN'
            STOP '1'
        END IF
        IOST = NF90_INQ_VARID(NCID, "budget_column", BC_VAR_ID)
        IF (IOST .NE. NF90_NOERR) GO TO 9999
        IOST = NF90_GET_VAR(NCID, BC_VAR_ID, BUDGET_COLUMN)
        IF (IOST .NE. NF90_NOERR) GO TO 9999
    END IF

    IF (PRESENT(BUDGET_LAYER)) THEN
        IF (ALLOCATED(BUDGET_LAYER))          DEALLOCATE(BUDGET_LAYER)
        ALLOCATE(BUDGET_LAYER(BLV_DIM_LEN,BLT_DIM_LEN,P_DIM_LEN,T_DIM_LEN), STAT=MEMST)
        IF (MEMST .NE. 0) THEN
            PRINT *,'E: IPT_BUDGET_NETCDF: Unable to allocate memory to hold BUDGET_LAYER'
            STOP '1'
        END IF
        IOST = NF90_INQ_VARID(NCID, "budget_layer", BL_VAR_ID)
        IF (IOST .NE. NF90_NOERR) GO TO 9999
        IOST = NF90_GET_VAR(NCID, BL_VAR_ID, BUDGET_LAYER)
        IF (IOST .NE. NF90_NOERR) GO TO 9999
    END IF

    IF (PRESENT(VBUDGET_COLUMN)) THEN
        IF (ALLOCATED(VBUDGET_COLUMN))          DEALLOCATE(VBUDGET_COLUMN)
        ALLOCATE(VBUDGET_COLUMN(BCV_DIM_LEN,BCT_DIM_LEN), STAT=MEMST)
        IF (MEMST .NE. 0) THEN
            PRINT *,'E: IPT_BUDGET_NETCDF: Unable to allocate memory to hold VBUDGET_COLUMN'
            STOP '1'
        END IF
        IOST = NF90_INQ_VARID(NCID, "vbudget_column", VBC_VAR_ID)
        IF (IOST .NE. NF90_NOERR) GO TO 9999
        IOST = NF90_GET_VAR(NCID, VBC_VAR_ID, VBUDGET_COLUMN)
        IF (IOST .NE. NF90_NOERR) GO TO 9999
    END IF

    IF (PRESENT(VBUDGET_LAYER)) THEN
        IF (ALLOCATED(VBUDGET_LAYER))          DEALLOCATE(VBUDGET_LAYER)
        ALLOCATE(VBUDGET_LAYER(BLV_DIM_LEN,BLT_DIM_LEN), STAT=MEMST)
        IF (MEMST .NE. 0) THEN
            PRINT *,'E: IPT_BUDGET_NETCDF: Unable to allocate memory to hold VBUDGET_LAYER'
            STOP '1'
        END IF
        IOST = NF90_INQ_VARID(NCID, "vbudget_layer", VBL_VAR_ID)
        IF (IOST .NE. NF90_NOERR) GO TO 9999
        IOST = NF90_GET_VAR(NCID, VBL_VAR_ID, VBUDGET_LAYER)
        IF (IOST .NE. NF90_NOERR) GO TO 9999
    END IF

    IF (PRESENT(AVE_QS)) THEN
        IF (ALLOCATED(AVE_QS))           DEALLOCATE(AVE_QS)
        ALLOCATE(AVE_QS(T_DIM_LEN), STAT=MEMST)
        IF (MEMST .NE. 0) THEN
            PRINT *,'E: IPT_BUDGET_NETCDF: Unable to allocate memory to hold AVE_QS'
            STOP '1'
        END IF
        IOST = NF90_INQ_VARID(NCID, "ave_qs", AVE_QS_VAR_ID)
        IF (IOST .NE. NF90_NOERR) GO TO 9999
        IOST = NF90_GET_VAR(NCID, AVE_QS_VAR_ID, AVE_QS)
        IF (IOST .NE. NF90_NOERR) GO TO 9999
    END IF

    IF (PRESENT(AVE_SS)) THEN
        IF (ALLOCATED(AVE_SS))           DEALLOCATE(AVE_SS)
        ALLOCATE(AVE_SS(T_DIM_LEN), STAT=MEMST)
        IF (MEMST .NE. 0) THEN
            PRINT *,'E: IPT_BUDGET_NETCDF: Unable to allocate memory to hold AVE_SS'
            STOP '1'
        END IF
        IOST = NF90_INQ_VARID(NCID, "ave_ss", AVE_SS_VAR_ID)
        IF (IOST .NE. NF90_NOERR) GO TO 9999
        IOST = NF90_GET_VAR(NCID, AVE_SS_VAR_ID, AVE_SS)
        IF (IOST .NE. NF90_NOERR) GO TO 9999
    END IF

    IF (PRESENT(P)) THEN
        IF (ALLOCATED(P))         DEALLOCATE(P)
        ALLOCATE(P(P_DIM_LEN), STAT=MEMST)
        IF (MEMST .NE. 0) THEN
            PRINT *,'E: IPT_BUDGET_NETCDF: Unable to allocate memory to hold P'
            STOP '1'
        END IF
        IOST = NF90_INQ_VARID(NCID, "levels", P_VAR_ID)
        IF (IOST .NE. NF90_NOERR) GO TO 9999
        IOST = NF90_GET_VAR(NCID, P_VAR_ID, P)
        IF (IOST .NE. NF90_NOERR) GO TO 9999
    END IF

    IF (PRESENT(T)) THEN
        IF (ALLOCATED(T))           DEALLOCATE(T)
        ALLOCATE(T(T_DIM_LEN), STAT=MEMST)
        IF (MEMST .NE. 0) THEN
            PRINT *,'E: IPT_BUDGET_NETCDF: Unable to allocate memory to hold T'
            STOP '1'
        END IF
        IOST = NF90_INQ_VARID(NCID, "time", T_VAR_ID)
        IF (IOST .NE. NF90_NOERR) GO TO 9999
        IOST = NF90_GET_VAR(NCID, T_VAR_ID, T)
        IF (IOST .NE. NF90_NOERR) GO TO 9999
    END IF

    !
    ! Close the NetCDF file.
    !
    IOST    = NF90_CLOSE(NCID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    !
    ! Catch any NetCDF errors here.
    !
    9999 CONTINUE
    IF (NCERROR) THEN
        PRINT *,'E: IPT_BUDGET_NETCDF: NetCDF error encountered'
        IF (IOST .NE. NF90_NOERR) THEN
            PRINT *, TRIM(NF90_STRERROR(IOST))
        END IF
        STOP '1'
    END IF

    END SUBROUTINE IPT_BUDGET_NETCDF

    !*******************************************************************************************************************************
    ! OPT_2D_NETCDF
    !
    ! This subroutine writes the data from 2D_PUT to a NetCDF file. We'll need a corresponding subroutine to read the data into the
    ! variational analysis.
    !
    ! Tim Hume.
    ! 11 October 2006.
    !*******************************************************************************************************************************

    SUBROUTINE OPT_2D_NETCDF(OUTPUTFILE, T, BAR_PRES, ADPS, PRECIP, LPRECIP, EVAPOR, SHF, RL, &
        & RADIATIONT, RADIATIONB, RADIATION, TAOX, TAOY)
    USE PORTABLE
    USE NETCDF

    IMPLICIT NONE

    CHARACTER (LEN=*), INTENT(IN)                                       :: OUTPUTFILE   ! Name of the output file.
    REAL (KIND=RK8), DIMENSION(:), INTENT(IN)                           :: T            ! Time.
    REAL (KIND=RK8), DIMENSION(:), INTENT(IN)                           :: BAR_PRES     ! Barometric pressure (hPa).
    REAL (KIND=RK8), DIMENSION(:), INTENT(IN)                           :: ADPS         ! V.grad(PS)
    REAL (KIND=RK8), DIMENSION(:), INTENT(IN)                           :: PRECIP
    REAL (KIND=RK8), DIMENSION(:), INTENT(IN)                           :: LPRECIP
    REAL (KIND=RK8), DIMENSION(:), INTENT(IN)                           :: EVAPOR
    REAL (KIND=RK8), DIMENSION(:), INTENT(IN)                           :: SHF
    REAL (KIND=RK8), DIMENSION(:), INTENT(IN)                           :: RL
    REAL (KIND=RK8), DIMENSION(:), INTENT(IN)                           :: RADIATIONT
    REAL (KIND=RK8), DIMENSION(:), INTENT(IN)                           :: RADIATIONB
    REAL (KIND=RK8), DIMENSION(:), INTENT(IN)                           :: RADIATION
    REAL (KIND=RK8), DIMENSION(:), INTENT(IN)                           :: TAOX
    REAL (KIND=RK8), DIMENSION(:), INTENT(IN)                           :: TAOY

    !
    ! Local variables.
    !
    INTEGER (KIND=IK4)          :: NCID                                                 ! ID of NetCDF file.
    INTEGER (KIND=IK4)          :: T_DIM_ID                                             ! Time dimension ID.
    INTEGER (KIND=IK4)          :: T_VAR_ID                                             ! Time coordinate variable.
    INTEGER (KIND=IK4)          :: BAR_PRES_VAR_ID
    INTEGER (KIND=IK4)          :: ADPS_VAR_ID
    INTEGER (KIND=IK4)          :: PRECIP_VAR_ID
    INTEGER (KIND=IK4)          :: LPRECIP_VAR_ID
    INTEGER (KIND=IK4)          :: EVAPOR_VAR_ID
    INTEGER (KIND=IK4)          :: SHF_VAR_ID
    INTEGER (KIND=IK4)          :: RL_VAR_ID
    INTEGER (KIND=IK4)          :: RADIATIONT_VAR_ID
    INTEGER (KIND=IK4)          :: RADIATIONB_VAR_ID
    INTEGER (KIND=IK4)          :: RADIATION_VAR_ID
    INTEGER (KIND=IK4)          :: TAOX_VAR_ID
    INTEGER (KIND=IK4)          :: TAOY_VAR_ID
    INTEGER (KIND=IK4)          :: IOST                                                 ! I/O status.
    LOGICAL                     :: NCERROR                                               ! Did a NetCDF function fail?

    IOST    = NF90_NOERR
    NCERROR = .FALSE.
    !
    ! Create the NetCDF file.
    !
    IOST    = NF90_CREATE(OUTPUTFILE, NF90_NOCLOBBER, NCID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    !
    ! Define the dimensions.
    !
    IOST    = NF90_DEF_DIM(NCID, "time", SIZE(T), T_DIM_ID)                          ! Time steps.
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    !
    ! Define the variables.
    !
    NCERROR = .FALSE.
    NCERROR = (NF90_DEF_VAR(NCID, "time", NF90_DOUBLE, (/ T_DIM_ID /), T_VAR_ID)                    .NE. NF90_NOERR)
    NCERROR = (NF90_DEF_VAR(NCID, "bar_pres", NF90_DOUBLE, (/ T_DIM_ID /), BAR_PRES_VAR_ID)         .NE. NF90_NOERR) .OR. NCERROR
    NCERROR = (NF90_DEF_VAR(NCID, "adps", NF90_DOUBLE, (/ T_DIM_ID /), ADPS_VAR_ID)                 .NE. NF90_NOERR) .OR. NCERROR
    NCERROR = (NF90_DEF_VAR(NCID, "precip", NF90_DOUBLE, (/ T_DIM_ID /), PRECIP_VAR_ID)             .NE. NF90_NOERR) .OR. NCERROR
    NCERROR = (NF90_DEF_VAR(NCID, "lprecip", NF90_DOUBLE, (/ T_DIM_ID /), LPRECIP_VAR_ID)           .NE. NF90_NOERR) .OR. NCERROR
    NCERROR = (NF90_DEF_VAR(NCID, "evapor", NF90_DOUBLE, (/ T_DIM_ID /), EVAPOR_VAR_ID)             .NE. NF90_NOERR) .OR. NCERROR
    NCERROR = (NF90_DEF_VAR(NCID, "shf", NF90_DOUBLE, (/ T_DIM_ID /), SHF_VAR_ID)                   .NE. NF90_NOERR) .OR. NCERROR
    NCERROR = (NF90_DEF_VAR(NCID, "rl", NF90_DOUBLE, (/ T_DIM_ID /), RL_VAR_ID)                     .NE. NF90_NOERR) .OR. NCERROR
    NCERROR = (NF90_DEF_VAR(NCID, "radiationt", NF90_DOUBLE, (/ T_DIM_ID /), RADIATIONT_VAR_ID)     .NE. NF90_NOERR) .OR. NCERROR
    NCERROR = (NF90_DEF_VAR(NCID, "radiationb", NF90_DOUBLE, (/ T_DIM_ID /), RADIATIONB_VAR_ID)     .NE. NF90_NOERR) .OR. NCERROR
    NCERROR = (NF90_DEF_VAR(NCID, "radiation", NF90_DOUBLE, (/ T_DIM_ID /), RADIATION_VAR_ID)       .NE. NF90_NOERR) .OR. NCERROR
    NCERROR = (NF90_DEF_VAR(NCID, "taox", NF90_DOUBLE, (/ T_DIM_ID /), TAOX_VAR_ID)                 .NE. NF90_NOERR) .OR. NCERROR
    NCERROR = (NF90_DEF_VAR(NCID, "taoy", NF90_DOUBLE, (/ T_DIM_ID /), TAOY_VAR_ID)                 .NE. NF90_NOERR) .OR. NCERROR
    IF (NCERROR) GO TO 9999

    !
    ! Create various attributes. Make sure strings are null terminated (for C programs).
    !
    NCERROR = .FALSE.
    NCERROR = (NF90_PUT_ATT(NCID, T_VAR_ID, "long_name", 'Time'//CHAR(0))                           .NE. NF90_NOERR)
    NCERROR = (NF90_PUT_ATT(NCID, T_VAR_ID, "units", 'Days since 2004-12-31T00:00:00 UTC'//CHAR(0)) .NE. NF90_NOERR) .OR. NCERROR
    NCERROR = (NF90_PUT_ATT(NCID, T_VAR_ID, "missing_value", NF90_FILL_DOUBLE)                      .NE. NF90_NOERR) .OR. NCERROR
    NCERROR = (NF90_PUT_ATT(NCID, BAR_PRES_VAR_ID, "long_name", 'Barometric pressure'//CHAR(0))     .NE. NF90_NOERR) .OR. NCERROR
    NCERROR = (NF90_PUT_ATT(NCID, BAR_PRES_VAR_ID, "units", 'Pa'//CHAR(0))                          .NE. NF90_NOERR) .OR. NCERROR
    NCERROR = (NF90_PUT_ATT(NCID, BAR_PRES_VAR_ID, "missing_value", NF90_FILL_DOUBLE)               .NE. NF90_NOERR) .OR. NCERROR
    NCERROR = (NF90_PUT_ATT(NCID, ADPS_VAR_ID, "long_name", 'V.grad(Ps)'//CHAR(0))                  .NE. NF90_NOERR) .OR. NCERROR
    NCERROR = (NF90_PUT_ATT(NCID, ADPS_VAR_ID, "units", 'Pa/s'//CHAR(0))                            .NE. NF90_NOERR) .OR. NCERROR
    NCERROR = (NF90_PUT_ATT(NCID, ADPS_VAR_ID, "missing_value", NF90_FILL_DOUBLE)                   .NE. NF90_NOERR) .OR. NCERROR
    NCERROR = (NF90_PUT_ATT(NCID, PRECIP_VAR_ID, "long_name", 'precip * Lv0/Cpd'//CHAR(0))          .NE. NF90_NOERR) .OR. NCERROR
    NCERROR = (NF90_PUT_ATT(NCID, PRECIP_VAR_ID, "units", 'K/s'//CHAR(0))                           .NE. NF90_NOERR) .OR. NCERROR
    NCERROR = (NF90_PUT_ATT(NCID, PRECIP_VAR_ID, "missing_value", NF90_FILL_DOUBLE)                 .NE. NF90_NOERR) .OR. NCERROR
    NCERROR = (NF90_PUT_ATT(NCID, LPRECIP_VAR_ID, "long_name", 'precip *Lv/Cpd'//CHAR(0))           .NE. NF90_NOERR) .OR. NCERROR
    NCERROR = (NF90_PUT_ATT(NCID, LPRECIP_VAR_ID, "units", 'K/s'//CHAR(0))                          .NE. NF90_NOERR) .OR. NCERROR
    NCERROR = (NF90_PUT_ATT(NCID, LPRECIP_VAR_ID, "missing_value", NF90_FILL_DOUBLE)                .NE. NF90_NOERR) .OR. NCERROR
    NCERROR = (NF90_PUT_ATT(NCID, EVAPOR_VAR_ID, "long_name", 'Evaporation * Lv0/Cpd'//CHAR(0))     .NE. NF90_NOERR) .OR. NCERROR
    NCERROR = (NF90_PUT_ATT(NCID, EVAPOR_VAR_ID, "units", 'K/s'//CHAR(0))                           .NE. NF90_NOERR) .OR. NCERROR
    NCERROR = (NF90_PUT_ATT(NCID, EVAPOR_VAR_ID, "missing_value", NF90_FILL_DOUBLE)                 .NE. NF90_NOERR) .OR. NCERROR
    NCERROR = (NF90_PUT_ATT(NCID, SHF_VAR_ID, "long_name", 'Sensible heating rate'//CHAR(0))        .NE. NF90_NOERR) .OR. NCERROR
    NCERROR = (NF90_PUT_ATT(NCID, SHF_VAR_ID, "units", 'K/s'//CHAR(0))                              .NE. NF90_NOERR) .OR. NCERROR
    NCERROR = (NF90_PUT_ATT(NCID, SHF_VAR_ID, "missing_value", NF90_FILL_DOUBLE)                    .NE. NF90_NOERR) .OR. NCERROR
    NCERROR = (NF90_PUT_ATT(NCID, RL_VAR_ID, "long_name", 'Rl * Lv0/Cpd'//CHAR(0))                  .NE. NF90_NOERR) .OR. NCERROR
    NCERROR = (NF90_PUT_ATT(NCID, RL_VAR_ID, "units", 'K/s'//CHAR(0))                               .NE. NF90_NOERR) .OR. NCERROR
    NCERROR = (NF90_PUT_ATT(NCID, RL_VAR_ID, "missing_value", NF90_FILL_DOUBLE)                     .NE. NF90_NOERR) .OR. NCERROR
    NCERROR = (NF90_PUT_ATT(NCID, RADIATIONT_VAR_ID, "long_name", 'Radiative heating-TOA'//CHAR(0)) .NE. NF90_NOERR) .OR. NCERROR
    NCERROR = (NF90_PUT_ATT(NCID, RADIATIONT_VAR_ID, "units", 'K/s'//CHAR(0))                       .NE. NF90_NOERR) .OR. NCERROR
    NCERROR = (NF90_PUT_ATT(NCID, RADIATIONT_VAR_ID, "missing_value", NF90_FILL_DOUBLE)             .NE. NF90_NOERR) .OR. NCERROR
    NCERROR = (NF90_PUT_ATT(NCID, RADIATIONB_VAR_ID, "long_name", 'Radiative heating-SFC'//CHAR(0)) .NE. NF90_NOERR) .OR. NCERROR
    NCERROR = (NF90_PUT_ATT(NCID, RADIATIONB_VAR_ID, "units", 'K/s'//CHAR(0))                       .NE. NF90_NOERR) .OR. NCERROR
    NCERROR = (NF90_PUT_ATT(NCID, RADIATIONB_VAR_ID, "missing_value", NF90_FILL_DOUBLE)             .NE. NF90_NOERR) .OR. NCERROR
    NCERROR = (NF90_PUT_ATT(NCID, RADIATION_VAR_ID, "long_name", 'Radiative heating-COL'//CHAR(0))  .NE. NF90_NOERR) .OR. NCERROR
    NCERROR = (NF90_PUT_ATT(NCID, RADIATION_VAR_ID, "units", 'K/s'//CHAR(0))                        .NE. NF90_NOERR) .OR. NCERROR
    NCERROR = (NF90_PUT_ATT(NCID, RADIATION_VAR_ID, "missing_value", NF90_FILL_DOUBLE)              .NE. NF90_NOERR) .OR. NCERROR
    NCERROR = (NF90_PUT_ATT(NCID, TAOX_VAR_ID, "long_name", 'X-shearing stress'//CHAR(0))           .NE. NF90_NOERR) .OR. NCERROR
    NCERROR = (NF90_PUT_ATT(NCID, TAOX_VAR_ID, "units", 'N'//CHAR(0))                               .NE. NF90_NOERR) .OR. NCERROR
    NCERROR = (NF90_PUT_ATT(NCID, TAOX_VAR_ID, "missing_value", NF90_FILL_DOUBLE)                   .NE. NF90_NOERR) .OR. NCERROR
    NCERROR = (NF90_PUT_ATT(NCID, TAOY_VAR_ID, "long_name", 'Y-shearing stress'//CHAR(0))           .NE. NF90_NOERR) .OR. NCERROR
    NCERROR = (NF90_PUT_ATT(NCID, TAOY_VAR_ID, "units", 'N'//CHAR(0))                               .NE. NF90_NOERR) .OR. NCERROR
    NCERROR = (NF90_PUT_ATT(NCID, TAOY_VAR_ID, "missing_value", NF90_FILL_DOUBLE)                   .NE. NF90_NOERR) .OR. NCERROR
    IF (NCERROR) GO TO 9999

    !
    ! We've finished defining stuff, leave the definition mode.
    !
    IOST    = NF90_ENDDEF(NCID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    !
    ! Write the data.
    !
    NCERROR = .FALSE.
    NCERROR = (NF90_PUT_VAR(NCID, T_VAR_ID, T)                      .NE. NF90_NOERR)
    NCERROR = (NF90_PUT_VAR(NCID, BAR_PRES_VAR_ID, BAR_PRES)        .NE. NF90_NOERR) .OR. NCERROR
    NCERROR = (NF90_PUT_VAR(NCID, ADPS_VAR_ID, ADPS)                .NE. NF90_NOERR) .OR. NCERROR
    NCERROR = (NF90_PUT_VAR(NCID, PRECIP_VAR_ID, PRECIP)            .NE. NF90_NOERR) .OR. NCERROR
    NCERROR = (NF90_PUT_VAR(NCID, LPRECIP_VAR_ID, LPRECIP)          .NE. NF90_NOERR) .OR. NCERROR
    NCERROR = (NF90_PUT_VAR(NCID, EVAPOR_VAR_ID, EVAPOR)            .NE. NF90_NOERR) .OR. NCERROR
    NCERROR = (NF90_PUT_VAR(NCID, SHF_VAR_ID, SHF)                  .NE. NF90_NOERR) .OR. NCERROR
    NCERROR = (NF90_PUT_VAR(NCID, RL_VAR_ID, RL)                    .NE. NF90_NOERR) .OR. NCERROR
    NCERROR = (NF90_PUT_VAR(NCID, RADIATIONT_VAR_ID, RADIATIONT)    .NE. NF90_NOERR) .OR. NCERROR
    NCERROR = (NF90_PUT_VAR(NCID, RADIATIONB_VAR_ID, RADIATIONB)    .NE. NF90_NOERR) .OR. NCERROR
    NCERROR = (NF90_PUT_VAR(NCID, RADIATION_VAR_ID, RADIATION)      .NE. NF90_NOERR) .OR. NCERROR
    NCERROR = (NF90_PUT_VAR(NCID, TAOX_VAR_ID, TAOX)                .NE. NF90_NOERR) .OR. NCERROR
    NCERROR = (NF90_PUT_VAR(NCID, TAOY_VAR_ID, TAOY)                .NE. NF90_NOERR) .OR. NCERROR
    IF (NCERROR) GO TO 9999

    !
    ! Close the NetCDF file.
    !
    IOST    = NF90_CLOSE(NCID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    !
    ! Catch any NetCDF errors here.
    !
    9999 CONTINUE
    IF (NCERROR) THEN
        PRINT *,'E: NetCDF error encountered'
        IF (IOST .NE. NF90_NOERR) THEN
            PRINT *, TRIM(NF90_STRERROR(IOST))
        END IF
        STOP '1'
    END IF

    END SUBROUTINE OPT_2D_NETCDF

    !*******************************************************************************************************************************
    ! IPT_2D_NETCDF
    !
    ! This subroutine reads the NetCDF file written by OPT_2D_NETCDF.
    !
    ! Tim Hume.
    ! 11 October 2006.
    !*******************************************************************************************************************************

    SUBROUTINE IPT_2D_NETCDF(INPUTFILE, T, BAR_PRES, ADPS, PRECIP, LPRECIP, EVAPOR, SHF, RL, &
        & RADIATIONT, RADIATIONB, RADIATION, TAOX, TAOY)
    USE PORTABLE
    USE NETCDF

    IMPLICIT NONE

    CHARACTER (LEN=*), INTENT(IN)                                       :: INPUTFILE    ! Name of the input file.
    REAL (KIND=RK8), DIMENSION(:), INTENT(OUT), ALLOCATABLE             :: T            ! Time.
    REAL (KIND=RK8), DIMENSION(:), INTENT(OUT), ALLOCATABLE             :: BAR_PRES     ! Barometric pressure (hPa).
    REAL (KIND=RK8), DIMENSION(:), INTENT(OUT), ALLOCATABLE             :: ADPS         ! V.grad(PS)
    REAL (KIND=RK8), DIMENSION(:), INTENT(OUT), ALLOCATABLE             :: PRECIP
    REAL (KIND=RK8), DIMENSION(:), INTENT(OUT), ALLOCATABLE             :: LPRECIP
    REAL (KIND=RK8), DIMENSION(:), INTENT(OUT), ALLOCATABLE             :: EVAPOR
    REAL (KIND=RK8), DIMENSION(:), INTENT(OUT), ALLOCATABLE             :: SHF
    REAL (KIND=RK8), DIMENSION(:), INTENT(OUT), ALLOCATABLE             :: RL
    REAL (KIND=RK8), DIMENSION(:), INTENT(OUT), ALLOCATABLE             :: RADIATIONT
    REAL (KIND=RK8), DIMENSION(:), INTENT(OUT), ALLOCATABLE             :: RADIATIONB
    REAL (KIND=RK8), DIMENSION(:), INTENT(OUT), ALLOCATABLE             :: RADIATION
    REAL (KIND=RK8), DIMENSION(:), INTENT(OUT), ALLOCATABLE             :: TAOX
    REAL (KIND=RK8), DIMENSION(:), INTENT(OUT), ALLOCATABLE             :: TAOY

    !
    ! Local variables.
    !
    INTEGER (KIND=IK4)          :: NCID                                                 ! ID of NetCDF file.
    INTEGER (KIND=IK4)          :: T_DIM_ID                                             ! Time dimension ID.
    INTEGER (KIND=IK4)          :: T_DIM_LEN                                            ! Length of the time dimension.
    INTEGER (KIND=IK4)          :: T_VAR_ID                                             ! Time coordinate variable.
    INTEGER (KIND=IK4)          :: BAR_PRES_VAR_ID
    INTEGER (KIND=IK4)          :: ADPS_VAR_ID
    INTEGER (KIND=IK4)          :: PRECIP_VAR_ID
    INTEGER (KIND=IK4)          :: LPRECIP_VAR_ID
    INTEGER (KIND=IK4)          :: EVAPOR_VAR_ID
    INTEGER (KIND=IK4)          :: SHF_VAR_ID
    INTEGER (KIND=IK4)          :: RL_VAR_ID
    INTEGER (KIND=IK4)          :: RADIATIONT_VAR_ID
    INTEGER (KIND=IK4)          :: RADIATIONB_VAR_ID
    INTEGER (KIND=IK4)          :: RADIATION_VAR_ID
    INTEGER (KIND=IK4)          :: TAOX_VAR_ID
    INTEGER (KIND=IK4)          :: TAOY_VAR_ID
    INTEGER (KIND=IK4)          :: MEMST                                                ! Status of memory allocations.
    INTEGER (KIND=IK4)          :: IOST                                                 ! I/O status.
    LOGICAL                     :: NCERROR                                              ! Did a NetCDF function fail?

    IOST    = NF90_NOERR
    NCERROR = .FALSE.
    !
    ! Open the NetCDF file.
    !
    IOST    = NF90_OPEN(INPUTFILE, NF90_NOWRITE, NCID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    !
    ! Get the time dimension ID, and the length of the dimension.
    !
    IOST    = NF90_INQ_DIMID(NCID, "time", T_DIM_ID)                            ! Time steps.
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_INQUIRE_DIMENSION(NCID, T_DIM_ID, LEN=T_DIM_LEN)             ! Get the time dimension length.
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    !
    ! Allocate enough memory to hold all the data. Be paranoid ... check that arrays have not already been allocated.
    !
    IF (ALLOCATED(T))           DEALLOCATE(T)
    IF (ALLOCATED(BAR_PRES))    DEALLOCATE(BAR_PRES)
    IF (ALLOCATED(ADPS))        DEALLOCATE(ADPS)
    IF (ALLOCATED(PRECIP))      DEALLOCATE(PRECIP)
    IF (ALLOCATED(LPRECIP))     DEALLOCATE(LPRECIP)
    IF (ALLOCATED(EVAPOR))      DEALLOCATE(EVAPOR)
    IF (ALLOCATED(SHF))         DEALLOCATE(SHF)
    IF (ALLOCATED(RL))          DEALLOCATE(RL)
    IF (ALLOCATED(RADIATIONT))  DEALLOCATE(RADIATIONT)
    IF (ALLOCATED(RADIATIONB))  DEALLOCATE(RADIATIONB)
    IF (ALLOCATED(RADIATION))   DEALLOCATE(RADIATION)
    IF (ALLOCATED(TAOX))        DEALLOCATE(TAOX)
    IF (ALLOCATED(TAOY))        DEALLOCATE(TAOY)
    ALLOCATE(T(T_DIM_LEN), BAR_PRES(T_DIM_LEN), ADPS(T_DIM_LEN), PRECIP(T_DIM_LEN), LPRECIP(T_DIM_LEN), EVAPOR(T_DIM_LEN), &
        & SHF(T_DIM_LEN), RL(T_DIM_LEN), RADIATIONT(T_DIM_LEN), RADIATIONB(T_DIM_LEN), RADIATION(T_DIM_LEN), TAOX(T_DIM_LEN), &
        & TAOY(T_DIM_LEN), STAT=MEMST)
    IF (MEMST .NE. 0) THEN
        PRINT *,'E: Unable to allocate memory to hold 2D data'
        STOP '1'
    END IF

    !
    ! Get the variable IDs.
    !
    NCERROR = .FALSE.
    NCERROR = (NF90_INQ_VARID(NCID, "time", T_VAR_ID)                   .NE. NF90_NOERR) .OR. NCERROR
    NCERROR = (NF90_INQ_VARID(NCID, "bar_pres", BAR_PRES_VAR_ID)        .NE. NF90_NOERR) .OR. NCERROR
    NCERROR = (NF90_INQ_VARID(NCID, "adps", ADPS_VAR_ID)                .NE. NF90_NOERR) .OR. NCERROR
    NCERROR = (NF90_INQ_VARID(NCID, "precip", PRECIP_VAR_ID)            .NE. NF90_NOERR) .OR. NCERROR
    NCERROR = (NF90_INQ_VARID(NCID, "lprecip", LPRECIP_VAR_ID)          .NE. NF90_NOERR) .OR. NCERROR
    NCERROR = (NF90_INQ_VARID(NCID, "evapor", EVAPOR_VAR_ID)            .NE. NF90_NOERR) .OR. NCERROR
    NCERROR = (NF90_INQ_VARID(NCID, "shf", SHF_VAR_ID)                  .NE. NF90_NOERR) .OR. NCERROR
    NCERROR = (NF90_INQ_VARID(NCID, "rl", RL_VAR_ID)                    .NE. NF90_NOERR) .OR. NCERROR
    NCERROR = (NF90_INQ_VARID(NCID, "radiationt", RADIATIONT_VAR_ID)    .NE. NF90_NOERR) .OR. NCERROR
    NCERROR = (NF90_INQ_VARID(NCID, "radiationb", RADIATIONB_VAR_ID)    .NE. NF90_NOERR) .OR. NCERROR
    NCERROR = (NF90_INQ_VARID(NCID, "radiation", RADIATION_VAR_ID)      .NE. NF90_NOERR) .OR. NCERROR
    NCERROR = (NF90_INQ_VARID(NCID, "taox", TAOX_VAR_ID)                .NE. NF90_NOERR) .OR. NCERROR
    NCERROR = (NF90_INQ_VARID(NCID, "taoy", TAOY_VAR_ID)                .NE. NF90_NOERR) .OR. NCERROR
    IF (NCERROR) GO TO 9999

    !
    ! Read the data.
    !
    NCERROR = .FALSE.
    NCERROR = (NF90_GET_VAR(NCID, T_VAR_ID, T)                          .NE. NF90_NOERR)
    NCERROR = (NF90_GET_VAR(NCID, BAR_PRES_VAR_ID, BAR_PRES)            .NE. NF90_NOERR) .OR. NCERROR
    NCERROR = (NF90_GET_VAR(NCID, ADPS_VAR_ID, ADPS)                    .NE. NF90_NOERR) .OR. NCERROR
    NCERROR = (NF90_GET_VAR(NCID, PRECIP_VAR_ID, PRECIP)                .NE. NF90_NOERR) .OR. NCERROR
    NCERROR = (NF90_GET_VAR(NCID, LPRECIP_VAR_ID, LPRECIP)              .NE. NF90_NOERR) .OR. NCERROR
    NCERROR = (NF90_GET_VAR(NCID, EVAPOR_VAR_ID, EVAPOR)                .NE. NF90_NOERR) .OR. NCERROR
    NCERROR = (NF90_GET_VAR(NCID, SHF_VAR_ID, SHF)                      .NE. NF90_NOERR) .OR. NCERROR
    NCERROR = (NF90_GET_VAR(NCID, RL_VAR_ID, RL)                        .NE. NF90_NOERR) .OR. NCERROR
    NCERROR = (NF90_GET_VAR(NCID, RADIATIONT_VAR_ID, RADIATIONT)        .NE. NF90_NOERR) .OR. NCERROR
    NCERROR = (NF90_GET_VAR(NCID, RADIATIONB_VAR_ID, RADIATIONB)        .NE. NF90_NOERR) .OR. NCERROR
    NCERROR = (NF90_GET_VAR(NCID, RADIATION_VAR_ID, RADIATION)          .NE. NF90_NOERR) .OR. NCERROR
    NCERROR = (NF90_GET_VAR(NCID, TAOX_VAR_ID, TAOX)                    .NE. NF90_NOERR) .OR. NCERROR
    NCERROR = (NF90_GET_VAR(NCID, TAOY_VAR_ID, TAOY)                    .NE. NF90_NOERR) .OR. NCERROR
    IF (NCERROR) GO TO 9999

    !
    ! Close the NetCDF file.
    !
    IOST    = NF90_CLOSE(NCID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    !
    ! Catch any NetCDF errors here.
    !
    9999 CONTINUE
    IF (NCERROR) THEN
        PRINT *,'E: NetCDF error encountered'
        IF (IOST .NE. NF90_NOERR) THEN
            PRINT *, TRIM(NF90_STRERROR(IOST))
        END IF
        STOP '1'
    END IF

    END SUBROUTINE IPT_2D_NETCDF

    !*******************************************************************************************************************************
    ! IPT_2DRAW_NETCDF
    !
    ! This subroutine reads the "raw" data required by "2D_PUT". The raw data could have originated from observations, or
    ! from model output (e.g. ECMWF analyses).
    !
    ! Tim Hume.
    ! 30 January 2007.
    !*******************************************************************************************************************************

    SUBROUTINE IPT_2DRAW_NETCDF(INPUTFILE, VAR_NAME, VAR)
    USE PORTABLE
    USE NETCDF

    IMPLICIT NONE

    CHARACTER (LEN=*), INTENT(IN)                                       :: INPUTFILE    ! Name of the input file.
    CHARACTER (LEN=*), INTENT(IN)                                       :: VAR_NAME     ! Name of the variable to read.
    REAL (KIND=RK8), DIMENSION(:), INTENT(OUT), ALLOCATABLE             :: VAR          ! Holds the data we are reading.


    !
    ! Local variables.
    !
    INTEGER (KIND=IK4)          :: NCID                                                 ! ID of NetCDF file.
    INTEGER (KIND=IK4)          :: T_DIM_ID                                             ! Time dimension ID.
    INTEGER (KIND=IK4)          :: T_DIM_LEN                                            ! Length of the time dimension.
    INTEGER (KIND=IK4)          :: VAR_ID                                               ! Variable ID.
    INTEGER (KIND=IK4)          :: MEMST                                                ! Status of memory allocations.
    INTEGER (KIND=IK4)          :: IOST                                                 ! I/O status.

    !
    ! Open the NetCDF file.
    !
    IOST    = NF90_OPEN(INPUTFILE, NF90_NOWRITE, NCID)
    IF (IOST .NE. NF90_NOERR) THEN
        PRINT *,'E: ',NF90_STRERROR(IOST)
        GO TO 9999
    END IF

    !
    ! Get the time dimension ID, and the length of the dimension.
    !
    IOST    = NF90_INQ_DIMID(NCID, "time", T_DIM_ID)                            ! Time steps.
    IF (IOST .NE. NF90_NOERR) THEN
        PRINT *,'E: ',NF90_STRERROR(IOST)
        GO TO 9999
    END IF
    IOST    = NF90_INQUIRE_DIMENSION(NCID, T_DIM_ID, LEN=T_DIM_LEN)             ! Get the time dimension length.
    IF (IOST .NE. NF90_NOERR) THEN
        PRINT *,'E: ',NF90_STRERROR(IOST)
        GO TO 9999
    END IF

    !
    ! Allocate enough memory to hold all the data. Be paranoid ... check that arrays have not already been allocated.
    !
    IF (ALLOCATED(VAR))           DEALLOCATE(VAR)
    ALLOCATE(VAR(T_DIM_LEN), STAT=MEMST)
    IF (MEMST .NE. 0) THEN
        PRINT *,'E: Unable to allocate memory to hold data'
        GO TO 9999
    END IF

    !
    ! Get the variable IDs.
    !
    IOST = NF90_INQ_VARID(NCID, VAR_NAME, VAR_ID)
    IF (IOST .NE. NF90_NOERR) THEN
        PRINT *,'E: ',NF90_STRERROR(IOST)
        GO TO 9999
    END IF

    !
    ! Read the data.
    !
    IOST = NF90_GET_VAR(NCID, VAR_ID, VAR)
    IF (IOST .NE. NF90_NOERR) THEN
        PRINT *,'E: ',NF90_STRERROR(IOST)
        GO TO 9999
    END IF

    !
    ! Close the NetCDF file.
    !
    IOST    = NF90_CLOSE(NCID)
    IF (IOST .NE. NF90_NOERR) THEN
        PRINT *,'E: ',NF90_STRERROR(IOST)
        GO TO 9999
    END IF

    9999 CONTINUE       ! Errors will be sent here.

    END SUBROUTINE IPT_2DRAW_NETCDF

    !*******************************************************************************************************************************
    ! OPT_FORCING_NETCDF
    !
    ! This subroutine writes the forcing data to a NetCDF file (this is the final product which SCM/CRM modellers will use).
    !
    ! Tim Hume.
    ! 27 March 2007.
    !*******************************************************************************************************************************

    SUBROUTINE OPT_FORCING_NETCDF(OUTPUTFILE, SFC_DATA, ML_DATA, CF_LON, CF_LAT, CF_PHIS, PLEVS)
    USE PORTABLE
    USE NETCDF
    USE TIME

    IMPLICIT NONE

    CHARACTER (LEN=*), INTENT(IN)                                       :: OUTPUTFILE       ! Name of the output file.
    REAL (KIND=RK8), DIMENSION(:,:), INTENT(IN)                         :: SFC_DATA         ! Surface forcing data.
    REAL (KIND=RK8), DIMENSION(:,:,:), INTENT(IN)                       :: ML_DATA          ! Multi-levcel forcing data.
    REAL (KIND=RK4), INTENT(IN)                                         :: CF_LON           ! Central facility longitude.
    REAL (KIND=RK4), INTENT(IN)                                         :: CF_LAT           ! Central facility latitude.
    REAL (KIND=RK4), INTENT(IN)                                         :: CF_PHIS          ! Central facility geopotential height.
    REAL (KIND=RK8), DIMENSION(:), INTENT(IN)                           :: PLEVS            ! Pressure levels in analysis.

    !
    ! Local variables.
    !
    INTEGER (KIND=IK4)                  :: NCID                                                 ! ID of NetCDF file.
    INTEGER (KIND=IK4)                  :: TIME_DIM_ID, LEV_DIM_ID                              ! Dimension IDs.
    INTEGER (KIND=IK4)                  :: BASE_TIME_VAR_ID                                     ! Variable ID for base_time
    INTEGER (KIND=IK4)                  :: TIME_VAR_ID
    INTEGER (KIND=IK4)                  :: TIME_OFFSET_VAR_ID
    INTEGER (KIND=IK4)                  :: YEAR_VAR_ID
    INTEGER (KIND=IK4)                  :: MONTH_VAR_ID
    INTEGER (KIND=IK4)                  :: DAY_VAR_ID
    INTEGER (KIND=IK4)                  :: HOUR_VAR_ID
    INTEGER (KIND=IK4)                  :: MINUTE_VAR_ID
    INTEGER (KIND=IK4)                  :: LAT_VAR_ID
    INTEGER (KIND=IK4)                  :: LON_VAR_ID
    INTEGER (KIND=IK4)                  :: PHIS_VAR_ID
    INTEGER (KIND=IK4)                  :: LEV_VAR_ID
    INTEGER (KIND=IK4)                  :: TEMP_VAR_ID
    INTEGER (KIND=IK4)                  :: Q_VAR_ID
    INTEGER (KIND=IK4)                  :: U_VAR_ID
    INTEGER (KIND=IK4)                  :: V_VAR_ID
    INTEGER (KIND=IK4)                  :: OMEGA_VAR_ID
    INTEGER (KIND=IK4)                  :: DIV_VAR_ID
    INTEGER (KIND=IK4)                  :: TADVH_VAR_ID
    INTEGER (KIND=IK4)                  :: TADVV_VAR_ID
    INTEGER (KIND=IK4)                  :: QADVH_VAR_ID
    INTEGER (KIND=IK4)                  :: QADVV_VAR_ID
    INTEGER (KIND=IK4)                  :: S_VAR_ID
    INTEGER (KIND=IK4)                  :: SADVH_VAR_ID
    INTEGER (KIND=IK4)                  :: SADVV_VAR_ID
    INTEGER (KIND=IK4)                  :: DSDT_VAR_ID
    INTEGER (KIND=IK4)                  :: DTDT_VAR_ID
    INTEGER (KIND=IK4)                  :: DQDT_VAR_ID
    INTEGER (KIND=IK4)                  :: Q1_VAR_ID
    INTEGER (KIND=IK4)                  :: Q2_VAR_ID
    INTEGER (KIND=IK4)                  :: CLD_VAR_ID
    INTEGER (KIND=IK4)                  :: PREC_VAR_ID
    INTEGER (KIND=IK4)                  :: LH_VAR_ID
    INTEGER (KIND=IK4)                  :: SH_VAR_ID
    INTEGER (KIND=IK4)                  :: PSA_VAR_ID
    INTEGER (KIND=IK4)                  :: PSI_VAR_ID
    INTEGER (KIND=IK4)                  :: TSAIR_VAR_ID
    INTEGER (KIND=IK4)                  :: TSKIN_VAR_ID
    INTEGER (KIND=IK4)                  :: RHAIR_VAR_ID
    INTEGER (KIND=IK4)                  :: WSPD_VAR_ID
    INTEGER (KIND=IK4)                  :: US_VAR_ID
    INTEGER (KIND=IK4)                  :: VS_VAR_ID
    INTEGER (KIND=IK4)                  :: SRFRAD_VAR_ID
    INTEGER (KIND=IK4)                  :: FLUT_VAR_ID
    INTEGER (KIND=IK4)                  :: FSNT_VAR_ID
    INTEGER (KIND=IK4)                  :: SOLIN_VAR_ID
    INTEGER (KIND=IK4)                  :: CLDLOW_VAR_ID
    INTEGER (KIND=IK4)                  :: CLDMED_VAR_ID
    INTEGER (KIND=IK4)                  :: CLDHGH_VAR_ID
    INTEGER (KIND=IK4)                  :: CLDTOT_VAR_ID
    INTEGER (KIND=IK4)                  :: CLDTHK_VAR_ID
    INTEGER (KIND=IK4)                  :: CLDTOP_VAR_ID
    INTEGER (KIND=IK4)                  :: LWP_VAR_ID
    INTEGER (KIND=IK4)                  :: CDH2ODT_VAR_ID
    INTEGER (KIND=IK4)                  :: CH2OADV_VAR_ID
    INTEGER (KIND=IK4)                  :: EVAP_VAR_ID
    INTEGER (KIND=IK4)                  :: CDSDT_VAR_ID
    INTEGER (KIND=IK4)                  :: CSADV_VAR_ID
    INTEGER (KIND=IK4)                  :: CRAD_VAR_ID
    INTEGER (KIND=IK4)                  :: CLH_VAR_ID
    INTEGER (KIND=IK4)                  :: OMEGAS_VAR_ID
    INTEGER (KIND=IK4)                  :: QS_VAR_ID
    INTEGER (KIND=IK4)                  :: S2M_VAR_ID
    INTEGER (KIND=IK4)                  :: PW_VAR_ID
    INTEGER (KIND=IK4)                  :: FLUS_VAR_ID
    INTEGER (KIND=IK4)                  :: FLDS_VAR_ID
    INTEGER (KIND=IK4)                  :: FSUS_VAR_ID
    INTEGER (KIND=IK4)                  :: FSDS_VAR_ID

    INTEGER (KIND=IK4)                  :: NT                                                   ! Number of time steps.
    INTEGER (KIND=IK4)                  :: IOST                                                 ! I/O status.
    INTEGER (KIND=IK4), DIMENSION(8)    :: DATE_TIME                                            ! Holds date and time components.
    CHARACTER (LEN=64)                  :: TMPSTR                                               ! Temporary string.

    !
    ! Create the NetCDF file.
    !
    IOST    = NF90_NOERR
    IOST    = NF90_CREATE(OUTPUTFILE, NF90_NOCLOBBER, NCID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    !
    ! Define the dimensions.
    !
    NT      = SIZE(SFC_DATA, DIM=2)                                                             ! Number of time steps.
    IOST    = NF90_DEF_DIM(NCID, "time", NT, TIME_DIM_ID)                                     ! Don't output first and last times.
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_DIM(NCID, "lev", SIZE(ML_DATA, DIM=2), LEV_DIM_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    !
    ! Define the variables and attributes. Make sure that strings are null terminated (to be nice to C programs).
    !
    IOST    = NF90_DEF_VAR(NCID, "base_time", NF90_DOUBLE, BASE_TIME_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, BASE_TIME_VAR_ID, "long_name", 'Base time in Epoch'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, BASE_TIME_VAR_ID, "units", 'seconds since 1970-01-01T00:00:00Z'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    WRITE(UNIT=TMPSTR, FMT='(I4.4,"-",I2.2,"-",I2.2,"T",I2.2,":",I2.2,":",I2.2,"Z")') &
        & INT(SFC_DATA(2,1)),INT(SFC_DATA(3,1)),INT(SFC_DATA(4,1)),INT(SFC_DATA(5,1)),INT(SFC_DATA(6,1)),0
    IOST    = NF90_PUT_ATT(NCID, BASE_TIME_VAR_ID, "string", TMPSTR//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "time", NF90_DOUBLE, (/ TIME_DIM_ID /), TIME_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    WRITE(UNIT=TMPSTR, FMT='(I4.4)') INT(SFC_DATA(2,1))
    IOST    = NF90_PUT_ATT(NCID, TIME_VAR_ID, "long_name", 'Calendar day fraction of the year '//TMPSTR//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    WRITE(UNIT=TMPSTR, FMT='(I4.4,"-12-31")') INT(SFC_DATA(2,1)-1)
    IOST    = NF90_PUT_ATT(NCID, TIME_VAR_ID, "units", 'days since '//TMPSTR//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
!    IOST    = NF90_PUT_ATT(NCID, TIME_VAR_ID, "calendar", 'proleptic_gregorian'//CHAR(0))
!    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, TIME_VAR_ID, "axis", 'T'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "time_offset", NF90_DOUBLE, (/ TIME_DIM_ID /), TIME_OFFSET_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, TIME_OFFSET_VAR_ID, "long_name", 'Time offset from base_time'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    WRITE(UNIT=TMPSTR, FMT='(I4.4,"-",I2.2,"-",I2.2,"T",I2.2,":",I2.2,":",I2.2,"Z")') &
        & INT(SFC_DATA(2,1)),INT(SFC_DATA(3,1)),INT(SFC_DATA(4,1)),INT(SFC_DATA(5,1)),INT(SFC_DATA(6,1)),0
    IOST    = NF90_PUT_ATT(NCID, TIME_OFFSET_VAR_ID, "units", 'seconds since '//TMPSTR//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "year", NF90_INT, (/ TIME_DIM_ID /), YEAR_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, YEAR_VAR_ID, "long_name", 'Year'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, YEAR_VAR_ID, "units", CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, YEAR_VAR_ID, "missing_value", INT(-9999, KIND=IK4))
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "month", NF90_INT, (/ TIME_DIM_ID /), MONTH_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, MONTH_VAR_ID, "long_name", 'Month'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, MONTH_VAR_ID, "units", CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, MONTH_VAR_ID, "missing_value", INT(-9999, KIND=IK4))
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "day", NF90_INT, (/ TIME_DIM_ID /), DAY_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, DAY_VAR_ID, "long_name", 'Day'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, DAY_VAR_ID, "units", CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, DAY_VAR_ID, "missing_value", INT(-9999, KIND=IK4))
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "hour", NF90_INT, (/ TIME_DIM_ID /), HOUR_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, HOUR_VAR_ID, "long_name", 'Hour'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, HOUR_VAR_ID, "units", CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, HOUR_VAR_ID, "missing_value", INT(-9999, KIND=IK4))
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "minute", NF90_INT, (/ TIME_DIM_ID /), MINUTE_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, MINUTE_VAR_ID, "long_name", 'Minute'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, MINUTE_VAR_ID, "units", CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, MINUTE_VAR_ID, "missing_value", INT(-9999, KIND=IK4))
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "lat", NF90_FLOAT, LAT_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, LAT_VAR_ID, "long_name", 'latitude'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, LAT_VAR_ID, "units", 'degrees North'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, LAT_VAR_ID, "missing_value", REAL(-9999, KIND=RK4))
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "lon", NF90_FLOAT, LON_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, LON_VAR_ID, "long_name", 'longitude'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, LON_VAR_ID, "units", 'degrees East'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, LON_VAR_ID, "missing_value", REAL(-9999, KIND=RK4))
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "phis", NF90_FLOAT, PHIS_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, PHIS_VAR_ID, "long_name", 'surface geopotential height'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, PHIS_VAR_ID, "units", 'm^2/s^2'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, PHIS_VAR_ID, "missing_value", REAL(-9999, KIND=RK4))
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "lev", NF90_DOUBLE, (/ LEV_DIM_ID /), LEV_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, LEV_VAR_ID, "long_name", 'pressure levels'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, LEV_VAR_ID, "units", 'hPa'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, LEV_VAR_ID, "missing_value", REAL(-9999, KIND=RK8))
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "T", NF90_FLOAT, (/ LEV_DIM_ID, TIME_DIM_ID /), TEMP_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, TEMP_VAR_ID, "long_name", 'Temperature'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, TEMP_VAR_ID, "units", 'K'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, TEMP_VAR_ID, "missing_value", REAL(-9999, KIND=RK4))
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "r", NF90_FLOAT, (/ LEV_DIM_ID, TIME_DIM_ID /), Q_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, Q_VAR_ID, "long_name", 'Water vapour mixing ratio'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, Q_VAR_ID, "units", 'g/kg'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, Q_VAR_ID, "missing_value", REAL(-9999, KIND=RK4))
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "u", NF90_FLOAT, (/ LEV_DIM_ID, TIME_DIM_ID /), U_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, U_VAR_ID, "long_name", 'Horizontal wind U component'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, U_VAR_ID, "units", 'm/s'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, U_VAR_ID, "missing_value", REAL(-9999, KIND=RK4))
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "v", NF90_FLOAT, (/ LEV_DIM_ID, TIME_DIM_ID /), V_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, V_VAR_ID, "long_name", 'Horizontal wind V component'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, V_VAR_ID, "units", 'm/s'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, V_VAR_ID, "missing_value", REAL(-9999, KIND=RK4))
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "omega", NF90_FLOAT, (/ LEV_DIM_ID, TIME_DIM_ID /), OMEGA_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, OMEGA_VAR_ID, "long_name", 'vertical velocity'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, OMEGA_VAR_ID, "units", 'hPa/hour'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, OMEGA_VAR_ID, "missing_value", REAL(-9999, KIND=RK4))
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "div", NF90_FLOAT, (/ LEV_DIM_ID, TIME_DIM_ID /), DIV_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, DIV_VAR_ID, "long_name", 'Horizontal wind divergence'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, DIV_VAR_ID, "units", '1/s'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, DIV_VAR_ID, "missing_value", REAL(-9999, KIND=RK4))
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "T_adv_h", NF90_FLOAT, (/ LEV_DIM_ID, TIME_DIM_ID /), TADVH_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, TADVH_VAR_ID, "long_name", 'Horizontal temperature Advection'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, TADVH_VAR_ID, "units", 'K/hour'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, TADVH_VAR_ID, "missing_value", REAL(-9999, KIND=RK4))
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "T_adv_v", NF90_FLOAT, (/ LEV_DIM_ID, TIME_DIM_ID /), TADVV_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, TADVV_VAR_ID, "long_name", 'Vertical temperature Advection'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, TADVV_VAR_ID, "units", 'K/hour'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, TADVV_VAR_ID, "missing_value", REAL(-9999, KIND=RK4))
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "r_adv_h", NF90_FLOAT, (/ LEV_DIM_ID, TIME_DIM_ID /), QADVH_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, QADVH_VAR_ID, "long_name", 'Horizontal r advection'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, QADVH_VAR_ID, "units", 'g/kg/hour'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, QADVH_VAR_ID, "missing_value", REAL(-9999, KIND=RK4))
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "r_adv_v", NF90_FLOAT, (/ LEV_DIM_ID, TIME_DIM_ID /), QADVV_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, QADVV_VAR_ID, "long_name", 'Vertical r advection'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, QADVV_VAR_ID, "units", 'g/kg/hour'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, QADVV_VAR_ID, "missing_value", REAL(-9999, KIND=RK4))
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "s", NF90_FLOAT, (/ LEV_DIM_ID, TIME_DIM_ID /), S_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, S_VAR_ID, "long_name", 'Dry static energy'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, S_VAR_ID, "units", 'K'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, S_VAR_ID, "missing_value", REAL(-9999, KIND=RK4))
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "s_adv_h", NF90_FLOAT, (/ LEV_DIM_ID, TIME_DIM_ID /), SADVH_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, SADVH_VAR_ID, "long_name", 'Horizontal dry static energy advection'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, SADVH_VAR_ID, "units", 'K/hour'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, SADVH_VAR_ID, "missing_value", REAL(-9999, KIND=RK4))
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "s_adv_v", NF90_FLOAT, (/ LEV_DIM_ID, TIME_DIM_ID /), SADVV_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, SADVV_VAR_ID, "long_name", 'Vertical dry static energy advection'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, SADVV_VAR_ID, "units", 'K/hour'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, SADVV_VAR_ID, "missing_value", REAL(-9999, KIND=RK4))
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "dsdt", NF90_FLOAT, (/ LEV_DIM_ID, TIME_DIM_ID /), DSDT_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, DSDT_VAR_ID, "long_name", 'd(dry static energy)/dt'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, DSDT_VAR_ID, "units", 'K/hour'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, DSDT_VAR_ID, "missing_value", REAL(-9999, KIND=RK4))
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "dTdt", NF90_FLOAT, (/ LEV_DIM_ID, TIME_DIM_ID /), DTDT_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, DTDT_VAR_ID, "long_name", 'd(temperature)/dt'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, DTDT_VAR_ID, "units", 'K/hour'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, DTDT_VAR_ID, "missing_value", REAL(-9999, KIND=RK4))
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "drdt", NF90_FLOAT, (/ LEV_DIM_ID, TIME_DIM_ID /), DQDT_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, DQDT_VAR_ID, "long_name", 'd(water vapour mixing ratio)/dt'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, DQDT_VAR_ID, "units", 'g/kg/hour'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, DQDT_VAR_ID, "missing_value", REAL(-9999, KIND=RK4))
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "q1", NF90_FLOAT, (/ LEV_DIM_ID, TIME_DIM_ID /), Q1_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, Q1_VAR_ID, "long_name", 'Apparent heat sources Yanai (1973)'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, Q1_VAR_ID, "units", 'K/hour'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, Q1_VAR_ID, "missing_value", REAL(-9999, KIND=RK4))
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "q2", NF90_FLOAT, (/ LEV_DIM_ID, TIME_DIM_ID /), Q2_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, Q2_VAR_ID, "long_name", 'Apparent moisture sinks Yanai (1973)'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, Q2_VAR_ID, "units", 'K/hour'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, Q2_VAR_ID, "missing_value", REAL(-9999, KIND=RK4))
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "cld", NF90_FLOAT, (/ LEV_DIM_ID, TIME_DIM_ID /), CLD_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, CLD_VAR_ID, "long_name", 'Cloud fraction'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, CLD_VAR_ID, "units", '%'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, CLD_VAR_ID, "missing_value", REAL(-9999, KIND=RK4))
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "prec_srf", NF90_FLOAT, (/ TIME_DIM_ID /), PREC_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, PREC_VAR_ID, "long_name", 'Surface precipitation'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, PREC_VAR_ID, "units", 'mm/hour'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, PREC_VAR_ID, "missing_value", REAL(-9999, KIND=RK4))
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "LH", NF90_FLOAT, (/ TIME_DIM_ID /), LH_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, LH_VAR_ID, "long_name", 'Surface latent heat flux, upward positive'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, LH_VAR_ID, "units", 'W/m^2'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, LH_VAR_ID, "missing_value", REAL(-9999, KIND=RK4))
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "SH", NF90_FLOAT, (/ TIME_DIM_ID /), SH_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, SH_VAR_ID, "long_name", 'Surface sensible heat flux, upward positive'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, SH_VAR_ID, "units", 'W/m^2'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, SH_VAR_ID, "missing_value", REAL(-9999, KIND=RK4))
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "p_srf_aver", NF90_FLOAT, (/ TIME_DIM_ID /), PSA_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, PSA_VAR_ID, "long_name", 'Surface pressure averaged over the domain'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, PSA_VAR_ID, "units", 'hPa'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, PSA_VAR_ID, "missing_value", REAL(-9999, KIND=RK4))
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "p_srf_center", NF90_FLOAT, (/ TIME_DIM_ID /), PSI_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, PSI_VAR_ID, "long_name", 'Surface pressure at centre of the domain'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, PSI_VAR_ID, "units", 'hPa'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, PSI_VAR_ID, "missing_value", REAL(-9999, KIND=RK4))
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "T_srf", NF90_FLOAT, (/ TIME_DIM_ID /), TSAIR_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, TSAIR_VAR_ID, "long_name", '2m air temperature'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, TSAIR_VAR_ID, "units", 'Celsius'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, TSAIR_VAR_ID, "missing_value", REAL(-9999, KIND=RK4))
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "T_skin", NF90_FLOAT, (/ TIME_DIM_ID /), TSKIN_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, TSKIN_VAR_ID, "long_name", 'Surface skin temperature'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, TSKIN_VAR_ID, "units", 'Celsius'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, TSKIN_VAR_ID, "missing_value", REAL(-9999, KIND=RK4))
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "RH_srf", NF90_FLOAT, (/ TIME_DIM_ID /), RHAIR_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, RHAIR_VAR_ID, "long_name", '2m air relative humidity'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, RHAIR_VAR_ID, "units", '%'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, RHAIR_VAR_ID, "missing_value", REAL(-9999, KIND=RK4))
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "wspd_srf", NF90_FLOAT, (/ TIME_DIM_ID /), WSPD_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, WSPD_VAR_ID, "long_name", '10m wind speed'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, WSPD_VAR_ID, "units", 'm/s'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, WSPD_VAR_ID, "missing_value", REAL(-9999, KIND=RK4))
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "u_srf", NF90_FLOAT, (/ TIME_DIM_ID /), US_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, US_VAR_ID, "long_name", '10m U component'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, US_VAR_ID, "units", 'm/s'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, US_VAR_ID, "missing_value", REAL(-9999, KIND=RK4))
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "v_srf", NF90_FLOAT, (/ TIME_DIM_ID /), VS_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, VS_VAR_ID, "long_name", '10m V component'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, VS_VAR_ID, "units", 'm/s'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, VS_VAR_ID, "missing_value", REAL(-9999, KIND=RK4))
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "rad_net_srf", NF90_FLOAT, (/ TIME_DIM_ID /), SRFRAD_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, SRFRAD_VAR_ID, "long_name", 'Surface net radiation, downward positive'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, SRFRAD_VAR_ID, "units", 'W/m^2'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, SRFRAD_VAR_ID, "missing_value", REAL(-9999, KIND=RK4))
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "lw_net_toa", NF90_FLOAT, (/ TIME_DIM_ID /), FLUT_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, FLUT_VAR_ID, "long_name", 'TOA LW flux, upward positive'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, FLUT_VAR_ID, "units", 'W/m^2'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, FLUT_VAR_ID, "missing_value", REAL(-9999, KIND=RK4))
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "sw_net_toa", NF90_FLOAT, (/ TIME_DIM_ID /), FSNT_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, FSNT_VAR_ID, "long_name", 'TOA net SW flux, downward positive'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, FSNT_VAR_ID, "units", 'W/m^2'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, FSNT_VAR_ID, "missing_value", REAL(-9999, KIND=RK4))
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "sw_dn_toa", NF90_FLOAT, (/ TIME_DIM_ID /), SOLIN_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, SOLIN_VAR_ID, "long_name", 'TOA solar insolation'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, SOLIN_VAR_ID, "units", 'W/m^2'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, SOLIN_VAR_ID, "missing_value", REAL(-9999, KIND=RK4))
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "cld_low", NF90_FLOAT, (/ TIME_DIM_ID /), CLDLOW_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, CLDLOW_VAR_ID, "long_name", 'Satellite-measured low cloud'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, CLDLOW_VAR_ID, "units", '%'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, CLDLOW_VAR_ID, "missing_value", REAL(-9999, KIND=RK4))
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "cld_mid", NF90_FLOAT, (/ TIME_DIM_ID /), CLDMED_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, CLDMED_VAR_ID, "long_name", 'Satellite-measured middle cloud'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, CLDMED_VAR_ID, "units", '%'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, CLDMED_VAR_ID, "missing_value", REAL(-9999, KIND=RK4))
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "cld_high", NF90_FLOAT, (/ TIME_DIM_ID /), CLDHGH_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, CLDHGH_VAR_ID, "long_name", 'Satellite-measured high cloud'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, CLDHGH_VAR_ID, "units", '%'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, CLDHGH_VAR_ID, "missing_value", REAL(-9999, KIND=RK4))
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "cld_tot", NF90_FLOAT, (/ TIME_DIM_ID /), CLDTOT_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, CLDTOT_VAR_ID, "long_name", 'Satellite-measured total cloud'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, CLDTOT_VAR_ID, "units", '%'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, CLDTOT_VAR_ID, "missing_value", REAL(-9999, KIND=RK4))
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "cld_thick", NF90_FLOAT, (/ TIME_DIM_ID /), CLDTHK_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, CLDTHK_VAR_ID, "long_name", 'Satellite-measured cloud thickness'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, CLDTHK_VAR_ID, "units", 'km'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, CLDTHK_VAR_ID, "missing_value", REAL(-9999, KIND=RK4))
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "cld_top", NF90_FLOAT, (/ TIME_DIM_ID /), CLDTOP_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, CLDTOP_VAR_ID, "long_name", 'Satellite-measured cloud top'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, CLDTOP_VAR_ID, "units", 'km'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, CLDTOP_VAR_ID, "missing_value", REAL(-9999, KIND=RK4))
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "LWP", NF90_FLOAT, (/ TIME_DIM_ID /), LWP_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, LWP_VAR_ID, "long_name", 'MWR-measured cloud liquid water path'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, LWP_VAR_ID, "units", 'cm'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, LWP_VAR_ID, "missing_value", REAL(-9999, KIND=RK4))
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "dh2odt_col", NF90_FLOAT, (/ TIME_DIM_ID /), CDH2ODT_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, CDH2ODT_VAR_ID, "long_name", 'Column-integrated dH2O/dt'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, CDH2ODT_VAR_ID, "units", 'mm/hour'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, CDH2ODT_VAR_ID, "missing_value", REAL(-9999, KIND=RK4))
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "h2o_adv_col", NF90_FLOAT, (/ TIME_DIM_ID /), CH2OADV_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, CH2OADV_VAR_ID, "long_name", 'Column-integrated H2O advection'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, CH2OADV_VAR_ID, "units", 'mm/hour'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, CH2OADV_VAR_ID, "missing_value", REAL(-9999, KIND=RK4))
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "evap_srf", NF90_FLOAT, (/ TIME_DIM_ID /), EVAP_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, EVAP_VAR_ID, "long_name", 'Surface evaporation'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, EVAP_VAR_ID, "units", 'mm/hour'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, EVAP_VAR_ID, "missing_value", REAL(-9999, KIND=RK4))
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "dsdt_col", NF90_FLOAT, (/ TIME_DIM_ID /), CDSDT_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, CDSDT_VAR_ID, "long_name", 'Column d(dry static energy)/dt'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, CDSDT_VAR_ID, "units", 'W/m^2'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, CDSDT_VAR_ID, "missing_value", REAL(-9999, KIND=RK4))
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "s_adv_col", NF90_FLOAT, (/ TIME_DIM_ID /), CSADV_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, CSADV_VAR_ID, "long_name", 'Column dry static energy advection'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, CSADV_VAR_ID, "units", 'W/m^2'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, CSADV_VAR_ID, "missing_value", REAL(-9999, KIND=RK4))
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "rad_heat_col", NF90_FLOAT, (/ TIME_DIM_ID /), CRAD_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, CRAD_VAR_ID, "long_name", 'Column radiative heating'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, CRAD_VAR_ID, "units", 'W/m^2'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, CRAD_VAR_ID, "missing_value", REAL(-9999, KIND=RK4))
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "LH_col", NF90_FLOAT, (/ TIME_DIM_ID /), CLH_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, CLH_VAR_ID, "long_name", 'Column latent heating'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, CLH_VAR_ID, "units", 'W/m^2'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, CLH_VAR_ID, "missing_value", REAL(-9999, KIND=RK4))
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "omega_srf", NF90_FLOAT, (/ TIME_DIM_ID /), OMEGAS_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, OMEGAS_VAR_ID, "long_name", 'Surface omega'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, OMEGAS_VAR_ID, "units", 'hPa/hour'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, OMEGAS_VAR_ID, "missing_value", REAL(-9999, KIND=RK4))
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "r_srf", NF90_FLOAT, (/ TIME_DIM_ID /), QS_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, QS_VAR_ID, "long_name", '2m water vapour mixing ratio'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, QS_VAR_ID, "units", 'g/kg'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, QS_VAR_ID, "missing_value", REAL(-9999, KIND=RK4))
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "s_srf", NF90_FLOAT, (/ TIME_DIM_ID /), S2M_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, S2M_VAR_ID, "long_name", '2m dry static energy'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, S2M_VAR_ID, "units", 'K'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, S2M_VAR_ID, "missing_value", REAL(-9999, KIND=RK4))
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "PW", NF90_FLOAT, (/ TIME_DIM_ID /), PW_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, PW_VAR_ID, "long_name", 'MWR-measured column precipitable water'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, PW_VAR_ID, "units", 'cm'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, PW_VAR_ID, "missing_value", REAL(-9999, KIND=RK4))
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "lw_up_srf", NF90_FLOAT, (/ TIME_DIM_ID /), FLUS_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, FLUS_VAR_ID, "long_name", 'Surface upwelling LW'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, FLUS_VAR_ID, "units", 'W/m^2'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, FLUS_VAR_ID, "missing_value", REAL(-9999, KIND=RK4))
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "lw_dn_srf", NF90_FLOAT, (/ TIME_DIM_ID /), FLDS_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, FLDS_VAR_ID, "long_name", 'Surface downwelling LW'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, FLDS_VAR_ID, "units", 'W/m^2'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, FLDS_VAR_ID, "missing_value", REAL(-9999, KIND=RK4))
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "sw_up_srf", NF90_FLOAT, (/ TIME_DIM_ID /), FSUS_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, FSUS_VAR_ID, "long_name", 'Surface downwelling SW'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, FSUS_VAR_ID, "units", 'W/m^2'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, FSUS_VAR_ID, "missing_value", REAL(-9999, KIND=RK4))
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_DEF_VAR(NCID, "sw_dn_srf", NF90_FLOAT, (/ TIME_DIM_ID /), FSDS_VAR_ID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, FSDS_VAR_ID, "long_name", 'Surface downwelling SW'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, FSDS_VAR_ID, "units", 'W/m^2'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, FSDS_VAR_ID, "missing_value", REAL(-9999, KIND=RK4))
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    !
    ! Define the global attributes. The TWP-ICE stuff shouldn't be hard coded.
    !
    IOST    = NF90_PUT_ATT(NCID, NF90_GLOBAL, "Conventions", 'CF-1.0'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, NF90_GLOBAL, "title", 'VarAnalysis 3hr SndgBased Products V1.1: TWP-ICE'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    CALL DATE_AND_TIME(VALUES=DATE_TIME)
    WRITE(UNIT=TMPSTR, FMT='(I4.4,"-",I2.2,"-",I2.2,"T",I2.2,":",I2.2,":",I2.2,".",I3.3,SP,I5)') &
        & DATE_TIME(1), DATE_TIME(2), DATE_TIME(3), DATE_TIME(5), DATE_TIME(6), DATE_TIME(7), DATE_TIME(8), DATE_TIME(4)
    IOST    = NF90_PUT_ATT(NCID, NF90_GLOBAL, "date_created", TMPSTR//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_ATT(NCID, NF90_GLOBAL, "software", 'Fortran 95 variational analysis software'//CHAR(0))
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    !
    ! We've finished defining stuff, leave the definition mode.
    !
    IOST    = NF90_ENDDEF(NCID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    !
    ! Write the data.
    !
    IOST    = NF90_PUT_VAR(NCID, BASE_TIME_VAR_ID, REAL(DATE_TIME_TO_UNIX(YYYY=INT(SFC_DATA(2,1), KIND=IK4), &
        & MO=INT(SFC_DATA(3,1), KIND=IK4), DD=INT(SFC_DATA(4,1), KIND=IK4), HH=INT(SFC_DATA(5,1), KIND=IK4), &
        & MM=INT(SFC_DATA(6,1), KIND=IK4))-SFC_DATA(1,1)*86400, KIND=RK8))
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_PUT_VAR(NCID,    TIME_VAR_ID,        SFC_DATA(1,:))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_VAR(NCID,    TIME_OFFSET_VAR_ID, (SFC_DATA(1,:) - SFC_DATA(1,1))*86400.)
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_VAR(NCID,    YEAR_VAR_ID,        SFC_DATA(2,:))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_VAR(NCID,    MONTH_VAR_ID,       SFC_DATA(3,:))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_VAR(NCID,    DAY_VAR_ID,         SFC_DATA(4,:))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_VAR(NCID,    HOUR_VAR_ID,        SFC_DATA(5,:))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_VAR(NCID,    MINUTE_VAR_ID,      SFC_DATA(6,:))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_VAR(NCID,    LAT_VAR_ID,         CF_LAT)
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_VAR(NCID,    LON_VAR_ID,         CF_LON)
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_VAR(NCID,    PHIS_VAR_ID,        CF_PHIS)
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_VAR(NCID,    LEV_VAR_ID,         PLEVS)
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_PUT_VAR(NCID,    TEMP_VAR_ID,        ML_DATA(1,:,:)) 
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_VAR(NCID,    Q_VAR_ID,           ML_DATA(2,:,:))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_VAR(NCID,    U_VAR_ID,           ML_DATA(3,:,:))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_VAR(NCID,    V_VAR_ID,           ML_DATA(4,:,:))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_VAR(NCID,    OMEGA_VAR_ID,       ML_DATA(5,:,:))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_VAR(NCID,    DIV_VAR_ID,         ML_DATA(6,:,:))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_VAR(NCID,    TADVH_VAR_ID,       ML_DATA(7,:,:))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_VAR(NCID,    TADVV_VAR_ID,       ML_DATA(8,:,:))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_VAR(NCID,    QADVH_VAR_ID,       ML_DATA(9,:,:))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_VAR(NCID,    QADVV_VAR_ID,       ML_DATA(10,:,:))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_VAR(NCID,    S_VAR_ID,           ML_DATA(11,:,:))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_VAR(NCID,    SADVH_VAR_ID,       ML_DATA(12,:,:))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_VAR(NCID,    SADVV_VAR_ID,       ML_DATA(13,:,:))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_VAR(NCID,    DSDT_VAR_ID,        ML_DATA(14,:,:))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_VAR(NCID,    DTDT_VAR_ID,        ML_DATA(15,:,:))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_VAR(NCID,    DQDT_VAR_ID,        ML_DATA(16,:,:))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_VAR(NCID,    Q1_VAR_ID,          ML_DATA(17,:,:))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_VAR(NCID,    Q2_VAR_ID,          ML_DATA(18,:,:))
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    IOST    = NF90_PUT_VAR(NCID,    PREC_VAR_ID,        SFC_DATA(7,:))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_VAR(NCID,    LH_VAR_ID,          SFC_DATA(8,:))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_VAR(NCID,    SH_VAR_ID,          SFC_DATA(9,:))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_VAR(NCID,    PSA_VAR_ID,         SFC_DATA(10,:))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_VAR(NCID,    PSI_VAR_ID,         SFC_DATA(11,:))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_VAR(NCID,    TSAIR_VAR_ID,       SFC_DATA(12,:))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_VAR(NCID,    TSKIN_VAR_ID,       SFC_DATA(13,:))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_VAR(NCID,    RHAIR_VAR_ID,       SFC_DATA(14,:))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_VAR(NCID,    WSPD_VAR_ID,        SFC_DATA(15,:))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_VAR(NCID,    US_VAR_ID,          SFC_DATA(16,:))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_VAR(NCID,    VS_VAR_ID,          SFC_DATA(17,:))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_VAR(NCID,    SRFRAD_VAR_ID,      SFC_DATA(18,:))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_VAR(NCID,    FLUT_VAR_ID,        SFC_DATA(19,:))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_VAR(NCID,    FSNT_VAR_ID,        SFC_DATA(20,:))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    !IOST    = NF90_PUT_VAR(NCID,    SOLIN_VAR_ID,       SFC_DATA(21,:))
    !IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_VAR(NCID,    CLDLOW_VAR_ID,      SFC_DATA(22,:))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_VAR(NCID,    CLDMED_VAR_ID,      SFC_DATA(23,:))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_VAR(NCID,    CLDHGH_VAR_ID,      SFC_DATA(24,:))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_VAR(NCID,    CLDTOT_VAR_ID,      SFC_DATA(25,:))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_VAR(NCID,    CLDTHK_VAR_ID,      SFC_DATA(26,:))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_VAR(NCID,    CLDTOP_VAR_ID,      SFC_DATA(27,:))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_VAR(NCID,    LWP_VAR_ID,         SFC_DATA(28,:))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_VAR(NCID,    CDH2ODT_VAR_ID,     SFC_DATA(29,:))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_VAR(NCID,    CH2OADV_VAR_ID,     SFC_DATA(30,:))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_VAR(NCID,    EVAP_VAR_ID,        SFC_DATA(31,:))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_VAR(NCID,    CDSDT_VAR_ID,       SFC_DATA(32,:))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_VAR(NCID,    CSADV_VAR_ID,       SFC_DATA(33,:))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_VAR(NCID,    CRAD_VAR_ID,        SFC_DATA(34,:))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_VAR(NCID,    CLH_VAR_ID,         SFC_DATA(35,:))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_VAR(NCID,    OMEGAS_VAR_ID,      SFC_DATA(36,:))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_VAR(NCID,    QS_VAR_ID,          SFC_DATA(37,:))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_VAR(NCID,    S2M_VAR_ID,         SFC_DATA(38,:))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_VAR(NCID,    PW_VAR_ID,          SFC_DATA(39,:))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_VAR(NCID,    FLUS_VAR_ID,        SFC_DATA(40,:))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_VAR(NCID,    FLDS_VAR_ID,        SFC_DATA(41,:))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_VAR(NCID,    FSUS_VAR_ID,        SFC_DATA(42,:))
    IF (IOST .NE. NF90_NOERR) GO TO 9999
    IOST    = NF90_PUT_VAR(NCID,    FSDS_VAR_ID,        SFC_DATA(43,:))
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    !
    ! Close the NetCDF file.
    !
    IOST    = NF90_CLOSE(NCID)
    IF (IOST .NE. NF90_NOERR) GO TO 9999

    !
    ! Catch any NetCDF errors here.
    !
    9999 CONTINUE
    IF (IOST .NE. NF90_NOERR) THEN
        PRINT *,'E: Problem creating NetCDF file ',OUTPUTFILE
        PRINT *, TRIM(NF90_STRERROR(IOST))
        STOP '1'
    END IF

    END SUBROUTINE OPT_FORCING_NETCDF

END MODULE IO
