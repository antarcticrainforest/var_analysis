!***********************************************************************************************************************************
! 2D_PUT
!
! This is a port of the IDL procedure with the same name. The variational analysis software should be run in this order:
!
! 1) 2D_PUT (this program)
! 2) 3D_PUT
! 3) BUDGET_PUT
! 4) VARIATIONAL_ANALYSIS
! 5) PROCESS_VA_OUTPUT
!
! Tim Hume.
! 4 October 2006.
!***********************************************************************************************************************************
PROGRAM PUT_2D
USE PORTABLE                ! To ensure portability to other compilers and platforms.
USE CONSTANTS               ! Pi etc.
USE SETTINGS                ! Various settings for the variational analysis (the idea is only this file needs to be changed when
                            ! the variational analysis is re-configured).
USE IO                      ! IO subroutines.
USE PHYSICS                 ! Physics subroutines.
USE NUMERICS                ! Interpolation, matrix stuff etc.

!***********************************************************************************************************************************
! Local variables are defined here.
!***********************************************************************************************************************************
CHARACTER (LEN=512)                                     :: INPUTFILE        ! Used for the various input file names.
CHARACTER (LEN=512)                                     :: OUTPUTFILE       ! Output file name.

!
! The following variables hold the surface data which is read in to this program.
!
INTEGER (KIND=IK4)                                      :: NT               ! Number of time steps in input data.
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: T                ! Time steps for input data.
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: MG_P             ! Pressure.
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: MG_LH            ! Latent heat flux.
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: MG_PREC          ! Precipitation rate.
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: MG_LIQ           ! Liquid water column.
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: MG_ICE           ! Ice column.
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: MG_VAP           ! Vapour column.
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: MG_RADS          ! Net surface radiation.
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: MG_LWT           ! LW at TOA.
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: MG_INS           ! Solar insolation.
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: MG_SWT           ! SW at TOA.
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: MG_LWNS          ! Net LW at surface.
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: MG_SWNS          ! Net SW at surface.
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: MG_SH            ! Sensible heat flux.
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: MG_U             ! West-East wind component.
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: MG_V             ! South-North wind component.
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: MG_WSPD          ! Wind speed.
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: MG_TAOX
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: MG_TAOY
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: MG_TAOX1
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: MG_TAOY1
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: MG_RO
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: MG_T             ! Temperature.
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: MG_RH            ! Relative humidity.

!
! Variables which are output from this program.
!
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: BAR_PRES         ! Pressure
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: ADPS             ! V.grad(Ps)
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: PRECIP           ! Precipitation rate
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: EVAPOR           ! Evaporation rate
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: LPRECIP          ! Latent heat release due to precipitation
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: SHF              ! Sensible heat flux.
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: RL               ! Cloud liquid water.
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: RADIATIONB       ! Radiation at the surface.
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: RADIATIONT       ! Radiation at the top.
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: RADIATION        ! Radiational heating in the column.
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: TAOX             ! X component of wind stress.
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: TAOY             ! Y component of wind stress.

INTEGER (KIND=IK4)                                      :: MEMST            ! Status from memory allocation.
CHARACTER                                               :: KBD_INPUT        ! Holds single character input from the user.

!***********************************************************************************************************************************
! Finished defining the local variables.
!***********************************************************************************************************************************

!
! Read the model constraint data. This comes from (currently) ECMWF analyses. Later on in the program, some of these
! variables will be overwritten with observations.
!
PRINT *,'Enter the name of the file which contains the model constraint data'
READ (FMT='(A512)', UNIT=5) INPUTFILE
CALL IPT_2DRAW_NETCDF(INPUTFILE, 'time', T)                                         ! Time (units in NetCDF file)
! this is skin temperature. I recommend 2m temp
CALL IPT_2DRAW_NETCDF(INPUTFILE, 'sktsfc', MG_T)                                    ! Surface temperature           (K)
!CALL IPT_2DRAW_NETCDF(INPUTFILE, 'no2tsfc', MG_T)
CALL IPT_2DRAW_NETCDF(INPUTFILE, 'spsfc', MG_P)                                     ! Surface pressure              (hPa)
CALL IPT_2DRAW_NETCDF(INPUTFILE, 'ewsssfc', MG_TAOX1)                               ! Surface stress (E-W dir)      (N s/m^2)
CALL IPT_2DRAW_NETCDF(INPUTFILE, 'nssssfc', MG_TAOY1)                               ! Surface stress (N-S dir)      (N s/m^2)
CALL IPT_2DRAW_NETCDF(INPUTFILE, 'no10usfc', MG_U)                                  ! Surface U                     (m/s)
CALL IPT_2DRAW_NETCDF(INPUTFILE, 'no10vsfc', MG_V)                                  ! Surface V                     (m/s)
CALL IPT_2DRAW_NETCDF(INPUTFILE, 'sshfsfc', MG_SH)                                  ! Surface sensible heat flux    (W/m^2 down)
CALL IPT_2DRAW_NETCDF(INPUTFILE, 'slhfsfc', MG_LH)                                  ! Surface latent heat flux      (W/m^2 down)
CALL IPT_2DRAW_NETCDF(INPUTFILE, 'ssrsfc', MG_SWNS)                                 ! Net surface SW radiation      (W/m^2 down)
CALL IPT_2DRAW_NETCDF(INPUTFILE, 'strsfc', MG_LWNS)                                 ! Net surface LW radiation      (W/m^2 down)
CALL IPT_2DRAW_NETCDF(INPUTFILE, 'swt', MG_SWT)                                  ! Net TOA SW radiation          (W/m^2 down)
CALL IPT_2DRAW_NETCDF(INPUTFILE, 'lwt', MG_LWT)                                  ! Net TOA LW radiation          (W/m^2 down)


! This seemed to be missing from the model retrieval . Added MS Feb 2009
CALL IPT_2DRAW_NETCDF(INPUTFILE, 'no2dsfc', MG_RH)                              ! Dew point temp (K) (will convert to RH)


! Now convert dew point temperature to relative humidity: (Using Bolton's formula for now --> check whether this is good enough
MG_RH = exp(17.67*((MG_RH-273.15)/(MG_RH-29.65) - ((MG_T-273.15)/(MG_T-29.65))))

PRINT *,'DID IT'
!
! The number of times can be determined from the size of one of the variables read in above. Once this has been done, allocate
! memory for some of the other MG_ variables (which have not already had memory allocated above).
!
NT=SIZE(MG_T, 1)

ALLOCATE(MG_RADS(NT), MG_ICE(NT), MG_WSPD(NT), MG_TAOX(NT), MG_TAOY(NT), MG_RO(NT), STAT=MEMST)
IF (MEMST .NE. 0) THEN
    PRINT *,'E: Unable to allocate memory'
    GOTO 9999
END IF

!
! Process some of the radiation data. There is a good chance these will be replaced later by observations read in from a file.
!
MG_RADS = MG_SWNS + MG_LWNS                                                         ! Net radiation at the surface.
!   !  this is a silly hack.
		                                                                    ! Upward is positive. Hack alert! This may
                                                                                    ! be wrong. The user should pre-process data
                                                                                    ! so that the radiation signs are correct.
!
! Read the MWR data.
!
MG_ICE=0                                                                            ! Don't have ICE data at present.

PRINT *,'Enter the name of the file which contains the MWR data'
READ (FMT='(A512)', UNIT=5) INPUTFILE
CALL IPT_2DRAW_NETCDF(INPUTFILE, 'be_lwp', MG_LIQ) ! These both need to be in cm!
CALL IPT_2DRAW_NETCDF(INPUTFILE, 'be_pwv', MG_VAP)

IF ((SIZE(MG_LIQ,1) .NE. NT) .OR. (SIZE(MG_VAP,1) .NE. NT)) THEN                    ! MG_LIQ and MG_VAP should be same size.
    PRINT *,'E: Number of time steps in the MWR file is incorrect: ',SIZE(MG_LIQ)
    GOTO 9999
END IF

!
! Read the area average, radar derived precipitation.
!
PRINT *,'Enter the name of the file which contains the radar precipitation data'
READ (FMT='(A512)', UNIT=5) INPUTFILE

CALL IPT_2DRAW_NETCDF(INPUTFILE, 'pret', MG_PREC)                                    ! Surface rainfall (mm/hour)
IF (SIZE(MG_PREC,1) .NE. NT) THEN
    PRINT *,'E: Number of times in precip file different than in background data file'
    GOTO 9999
END IF
MG_PREC = MG_PREC*24    ! Convert rain rate to mm/day.

!
! Read the surface radiation data. Don't care if data originate from models or observations ... the user deals with this
! in pre-processing stage.
!

PRINT *,'Enter the name of the file which contains the surface radiation data'
READ (FMT='(A512)', UNIT=5) INPUTFILE
CALL IPT_2DRAW_NETCDF(INPUTFILE, 'surf_net', MG_RADS)

IF (SIZE(MG_RADS,1) .NE. NT) THEN 
    PRINT *,'E: Number of time steps in the surface radiation file is incorrect'
    GOTO 9999
END IF

!
! Read the TOA radiation data. Don't care if data originate from models or observations ... the user deals with this
! in pre-processing stage.
!



PRINT *,'Enter the name of the file which contains the TOA radiation data'
READ (FMT='(A512)', UNIT=5) INPUTFILE
CALL IPT_2DRAW_NETCDF(INPUTFILE, 'swt', MG_SWT)
CALL IPT_2DRAW_NETCDF(INPUTFILE, 'lwt', MG_LWT)
!CALL IPT_2DRAW_NETCDF(INPUTFILE, 'ins', MG_INS) dont read this. Doesn't seem to do anything

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! TOA data must be in Wm^-2 DOWN.
! (Both in ECMWF and obs file)
!
MG_LWT  = -MG_LWT

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

IF ((SIZE(MG_SWT,1) .NE. NT) .OR. (SIZE(MG_LWT,1) .NE. NT) ) THEN 
    PRINT *,'E: Number of time steps in the TOA radiation file is incorrect'
    GOTO 9999
END IF

!
! Now give the user the opportunity to replace some of the ECMWF surface fields with observed surface fields.
!
PRINT *,'Which surface fields do you want to use: 0=ECMWF, 1=Observed'
READ (FMT='(A1)', UNIT=5) KBD_INPUT

IF (KBD_INPUT .EQ. '1') THEN
    !
    ! Replace some of the ECMWF surface fields with observed surface fields. Be paranoid with memory (de)allocation issues.
    !
    PRINT *,'Enter the name of the file which contains the surface observations'
    READ (FMT='(A512)', UNIT=5) INPUTFILE

    IF (ALLOCATED(MG_T))    DEALLOCATE(MG_T)
    IF (ALLOCATED(MG_U))    DEALLOCATE(MG_U)
    IF (ALLOCATED(MG_V))    DEALLOCATE(MG_V)
    IF (ALLOCATED(MG_P))    DEALLOCATE(MG_P)
    IF (ALLOCATED(MG_RH))   DEALLOCATE(MG_RH)

    CALL IPT_2DRAW_NETCDF(INPUTFILE, 'Ts', MG_T)
    CALL IPT_2DRAW_NETCDF(INPUTFILE, 'Us', MG_U)
    CALL IPT_2DRAW_NETCDF(INPUTFILE, 'Vs', MG_V)
    CALL IPT_2DRAW_NETCDF(INPUTFILE, 'Ps', MG_P)
    CALL IPT_2DRAW_NETCDF(INPUTFILE, 'RHs', MG_RH)
END IF

!
! Now allow the use of observed sensible and latent heat fluxes. This part differs from the original code in that it allowed the
! option of mixed observations and model outputs. Instead, we assume that if the user wants to mix observed and modelled data,
! they pre-process their data before they reach this stage. In fact, at a later time, we might remove all the "choices" for modelled
! and observed data from this code, and simply assume that the input files contain the desired "blend" of modelled and observed data.
!
PRINT *,'Which sensible and latent heat flux fields do you want to use: 0=ECMWF, 1=Observed'
READ (FMT='(A1)', UNIT=5) KBD_INPUT

IF (KBD_INPUT .EQ. '1') THEN
    !
    ! Replace the ECMWF fields with observed fields. Be paranoid with memory (de)allocation issues.
    !
    PRINT *,'Enter the name of the file which contains the sensible and latent heat flux observations'
    READ (FMT='(A512)', UNIT=5) INPUTFILE

    IF (ALLOCATED(MG_SH))   DEALLOCATE(MG_SH)
    IF (ALLOCATED(MG_LH))   DEALLOCATE(MG_LH)

    CALL IPT_2DRAW_NETCDF(INPUTFILE, 'SH', MG_SH)
    CALL IPT_2DRAW_NETCDF(INPUTFILE, 'LH', MG_LH)

    !
    ! This code uses a different sign convention for the heat fluxes than the observation files.
    !
!    MG_SH = -MG_SH
!    MG_LH = -MG_LH
END IF

!
! Derive some other fields.
!
MG_WSPD = SQRT(MG_U**2 + MG_V**2)               ! Calculate wind speed from U and V.
MG_RO   = MG_P*100.0/RD/MG_T                    ! Calculate the air density at the surface.
MG_TAOX = -CD*MG_RO*MG_WSPD*MG_U                ! X component of wind stress (N)
MG_TAOY = -CD*MG_RO*MG_WSPD*MG_V                ! Y component of wind stress (N)


!
! OK ... now we write the 2D fields to a NetCDF file. These fields will be used by other programs.
! Before we write the data, we do a few unit conversions and other derivations.
!
ALLOCATE(BAR_PRES(NT), ADPS(NT), PRECIP(NT), EVAPOR(NT), LPRECIP(NT), SHF(NT), RL(NT), RADIATIONB(NT), RADIATIONT(NT), &
    & RADIATION(NT), TAOX(NT), TAOY(NT), STAT=MEMST)
IF (MEMST .NE. 0) THEN
    PRINT *,'E: Unable to allocate memory'
    GOTO 9999
END IF

BAR_PRES    = MG_P*100.                         ! Convert hPa to Pa.
ADPS        = 0.0                               ! Set to 0.
PRECIP      = MG_PREC/86400.                    ! Convert mm/day to mm/s.

WHERE (MG_T .GE. 0)
    EVAPOR  = -MG_LH/LV0                        ! mm/s upward.
    LPRECIP = LV0*PRECIP/CPD                    ! K/s.
ELSEWHERE
    EVAPOR  = -MG_LH/LS                         ! mm/s upward.
    LPRECIP = LS*PRECIP/CPD                     ! K/s.
ENDWHERE

SHF         = -MG_SH                            ! W/m^2 upward.
RL          = MG_LIQ*10.                        ! Convert cm to mm.
RADIATIONB  = -MG_RADS                          ! W/m^2 upward.
RADIATIONT  = MG_SWT-MG_LWT                     ! W/m^2 downward.
RADIATION   = RADIATIONT+RADIATIONB             ! W/m^2 column heating.
TAOX        = MG_TAOX                           ! N.
TAOY        = MG_TAOY                           ! N.

PRECIP      = PRECIP*LV0/CPD                    ! K/s
RL          = RL*LV0/CPD + MG_ICE*10.*LS1/CPD   ! K/s
EVAPOR      = EVAPOR*LV0/CPD                    ! K/s
SHF         = SHF/CPD                           ! K/s
RADIATIONT  = RADIATIONT/CPD                    ! K/s
RADIATIONB  = RADIATIONB/CPD                    ! K/s
RADIATION   = RADIATION/CPD                     ! K/s


PRINT *,'Enter the name of the output file'
READ (FMT='(A512)', UNIT=5) OUTPUTFILE
CALL OPT_2D_NETCDF(TRIM(OUTPUTFILE), T, BAR_PRES, ADPS, PRECIP, LPRECIP, EVAPOR, SHF, RL, RADIATIONT, RADIATIONB, &
    & RADIATION, TAOX, TAOY)

PRINT *,'You need to run 3D_PUT and BUDGET_PUT to set up for ASSIM'

9999 CONTINUE               ! If an error occurs, execution will have jumped to this point.
!
! De-allocate allocated arrays. This is mainly for tidiness.
!
IF (ALLOCATED(T))           DEALLOCATE(T)
IF (ALLOCATED(MG_P))        DEALLOCATE(MG_P)
IF (ALLOCATED(MG_LH))       DEALLOCATE(MG_LH)
IF (ALLOCATED(MG_PREC))     DEALLOCATE(MG_PREC)
IF (ALLOCATED(MG_LIQ))      DEALLOCATE(MG_LIQ)
IF (ALLOCATED(MG_ICE))      DEALLOCATE(MG_ICE)
IF (ALLOCATED(MG_VAP))      DEALLOCATE(MG_VAP)
IF (ALLOCATED(MG_RADS))     DEALLOCATE(MG_RADS)
IF (ALLOCATED(MG_LWT))      DEALLOCATE(MG_LWT)
IF (ALLOCATED(MG_SWT))      DEALLOCATE(MG_SWT)
IF (ALLOCATED(MG_INS))      DEALLOCATE(MG_INS)
IF (ALLOCATED(MG_SH))       DEALLOCATE(MG_SH)
IF (ALLOCATED(MG_U))        DEALLOCATE(MG_U)
IF (ALLOCATED(MG_V))        DEALLOCATE(MG_V)
IF (ALLOCATED(MG_WSPD))     DEALLOCATE(MG_WSPD)
IF (ALLOCATED(MG_TAOX))     DEALLOCATE(MG_TAOX)
IF (ALLOCATED(MG_TAOY))     DEALLOCATE(MG_TAOY)
IF (ALLOCATED(MG_TAOX1))    DEALLOCATE(MG_TAOX1)
IF (ALLOCATED(MG_TAOY1))    DEALLOCATE(MG_TAOY1)
IF (ALLOCATED(MG_RO))       DEALLOCATE(MG_RO)
IF (ALLOCATED(MG_T))        DEALLOCATE(MG_T)
IF (ALLOCATED(MG_RH))       DEALLOCATE(MG_RH)
IF (ALLOCATED(BAR_PRES))    DEALLOCATE(BAR_PRES)
IF (ALLOCATED(ADPS))        DEALLOCATE(ADPS)
IF (ALLOCATED(PRECIP))      DEALLOCATE(PRECIP)
IF (ALLOCATED(EVAPOR))      DEALLOCATE(EVAPOR)
IF (ALLOCATED(LPRECIP))     DEALLOCATE(LPRECIP)
IF (ALLOCATED(SHF))         DEALLOCATE(SHF)
IF (ALLOCATED(RL))          DEALLOCATE(RL)
IF (ALLOCATED(RADIATIONB))  DEALLOCATE(RADIATIONB)
IF (ALLOCATED(RADIATIONT))  DEALLOCATE(RADIATIONT)
IF (ALLOCATED(RADIATION))   DEALLOCATE(RADIATION)
IF (ALLOCATED(TAOX))        DEALLOCATE(TAOX)
IF (ALLOCATED(TAOY))        DEALLOCATE(TAOY)

END PROGRAM PUT_2D
