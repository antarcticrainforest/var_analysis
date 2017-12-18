!***********************************************************************************************************************************
! PROCESS_VA_OUTPUT
!
! This is a port of the IDL procedure which processes the output of the variational analysis.
!
! Tim Hume.
! 2 April 2007.
!***********************************************************************************************************************************
PROGRAM PROCESS_VA_OUTPUT
USE PORTABLE                ! To ensure portability to other compilers and platforms.
USE CONSTANTS               ! Pi etc.
USE SETTINGS                ! Various settings for the variational analysis (the idea is only this file needs to be changed when
                            ! the variational analysis is re-configured).
USE IO                      ! IO subroutines.
USE PHYSICS                 ! Physics subroutines.
USE NUMERICS                ! Interpolation, matrix stuff etc. Also includes other miscellaneous subroutines.
USE TIME                    ! Time and date stuff.

!***********************************************************************************************************************************
! Local variables are defined here.
!***********************************************************************************************************************************
IMPLICIT NONE

CHARACTER (LEN=512)                                     :: INPUTFILE        ! Used for the various input file names.
CHARACTER (LEN=512)                                     :: OUTPUTFILE       ! Output file name.

REAL (KIND=RK8), DIMENSION(:,:,:,:), ALLOCATABLE        :: BUDGET_LAYER     ! BUDGET_LAYER array.
REAL (KIND=RK8), DIMENSION(:,:,:), ALLOCATABLE          :: BUDGET_COLUMN    ! BUDGET_COLUMN array,
CHARACTER (LEN=64), DIMENSION(:,:), ALLOCATABLE         :: VBUDGET_COLUMN   ! Names of variables in BUDGET_COLUMN.
CHARACTER (LEN=64), DIMENSION(:,:), ALLOCATABLE         :: VBUDGET_LAYER    ! Names of variables in BUDGET_LAYER.
!INTEGER (KIND=IK4)                                      :: NV_BCOLUMN       ! Number of variables in BUDGET_COLUMN.
!INTEGER (KIND=IK4)                                      :: NTERM_BC         ! Number of terms in BUDGET_COLUMN.
INTEGER (KIND=IK4)                                      :: NT_BC            ! Number of time steps in BUDGET_COLUMN.
!INTEGER (KIND=IK4)                                      :: NV_BLAYER        ! Number of variables in BUDGET_LAYER.
!INTEGER (KIND=IK4)                                      :: NTERM_BL         ! Number of terms in BUDGET_LAYER.
INTEGER (KIND=IK4)                                      :: NP_BL            ! Number of vertical levels.
INTEGER (KIND=IK4)                                      :: NT_BL            ! Number of time steps.

REAL (KIND=RK8), DIMENSION(:,:,:,:), ALLOCATABLE        :: D_FINAL          ! The final state array.
CHARACTER (LEN=64), DIMENSION(:), ALLOCATABLE           :: V                ! Names of variables in D_FINAL.
CHARACTER (LEN=64), DIMENSION(:), ALLOCATABLE           :: ST               ! Names of stations in D_FINAL.
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: P                ! Pressure levels in D_FINAL.
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: T                ! Time steps in D_FINAL.
!INTEGER (KIND=IK4)                                      :: NV, NP, NST, NT  ! Sizes of the four dimensions in D_FINAL.
INTEGER (KIND=IK4)                                      :: NP, NT      ! Sizes of some of the dimensions in D_FINAL.

REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: MG_TSKIN         ! Surface (skin) temperature.
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: MG_T             !
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: MG_P             !
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: MG_TAOX1         !
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: MG_TAOY1         !
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: MG_U             !
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: MG_V             !
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: MG_SH            !
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: MG_LH            !
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: MG_SWNS          !
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: MG_LWNS          !
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: MG_SWT           ! Shortwave radiation at the TOA
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: MG_LWT           ! Longwave radiation at the TOA
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: MG_INS           !
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: MG_CLDTZ         !
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: MG_CLDDZ         !
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: MG_TAIR          ! 2m temperature.
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: MG_TDAIR         ! Surface dewpoint temperature.
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: MG_SWSDN         ! Downward shortwave radiation at the surface.
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: MG_LWSDN         ! Downward longwave radiation at the surface.
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: MG_LCC           !
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: MG_MCC           !
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: MG_HCC           !
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: MG_TCC           !
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: MG_SR            !
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: MG_PWC           !
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: MG_RH            !
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: MG_PREC          !
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: MG_LIQ           ! Total column liquid water.
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: MG_VAP           ! Total column water vapour.
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: MG_RADS          !
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: MG_LWSUP         ! Upward longwave radiation at the surface.
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: MG_SWSUP         ! Upward shortwave radiation at the surface.
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: MG_WSPD          ! Surface wind speed.
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: MG_RO            !
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: MG_TAOX          !
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: MG_TAOY          !
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: MG_ZSFC          ! Surface geopotential.

REAL (KIND=RK8), DIMENSION(:,:), ALLOCATABLE            :: DD               ! Holds surface output.
REAL (KIND=RK8), DIMENSION(:,:,:), ALLOCATABLE          :: DD2              ! Holds multi-level output
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: PNEW             ! Levels which are actually output.
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: TNEW             ! Times which are actually output.
INTEGER (KIND=IK4)                                      :: NTNEW, NPNEW     ! Size of TNEW and PNEW arrays.

CHARACTER (LEN=64)                                      :: INSTRUMENTF      !
INTEGER (KIND=IK4)                                      :: NV1, NSTF        !
CHARACTER (LEN=64), DIMENSION(:), ALLOCATABLE           :: V1               !
CHARACTER (LEN=64), DIMENSION(:), ALLOCATABLE           :: STF              !
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: LONF             !
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE              :: LATF             !
REAL (KIND=RK8), DIMENSION(:,:,:), ALLOCATABLE          :: DF_P             !

INTEGER (KIND=IK4), DIMENSION(:), ALLOCATABLE           :: YY               ! Year
INTEGER (KIND=IK4), DIMENSION(:), ALLOCATABLE           :: MO               ! Month
INTEGER (KIND=IK4), DIMENSION(:), ALLOCATABLE           :: DY               ! Day
INTEGER (KIND=IK4), DIMENSION(:), ALLOCATABLE           :: HH               ! Hour
INTEGER (KIND=IK4), DIMENSION(:), ALLOCATABLE           :: MM               ! Minute

CHARACTER                                               :: KBD_INPUT
INTEGER (KIND=IK4)                                      :: MEMST            ! Status code returned from ALLOCATE statements.
INTEGER (KIND=IK4)                                      :: IOSTATUS         ! Status code returned from I/O statements.
INTEGER (KIND=IK4)                                      :: TMP_IOSTATUS     ! Another status code returned from I/O statements.
INTEGER (KIND=IK4)                                      :: II,JJ,KK         ! Counters.

CHARACTER (LEN=64), DIMENSION(43)                       :: STRHEAD          ! Used to store surface variable names.
CHARACTER (LEN=64), DIMENSION(18)                       :: STRHEAD2         ! Used to store multi-level variable names.

!
! Read the final state (after all the assimilation has been done).
!
PRINT *,'Enter the name of the file containing the state variables'
READ (FMT='(A512)', UNIT=5) INPUTFILE
CALL IPT_3D_NETCDF(INPUTFILE=INPUTFILE, DU=D_FINAL, VU=V, LEV=P, T=T, STN=ST)
!NV  = SIZE(V)
NP  = SIZE(P)
!NST = SIZE(ST)
NT  = SIZE(T)
!
! Read the budget terms.
!
PRINT *,'Enter the name of the data file containing the budget terms'
READ (FMT='(A512)', UNIT=5) INPUTFILE
CALL IPT_BUDGET_NETCDF(INPUTFILE=INPUTFILE, BUDGET_COLUMN=BUDGET_COLUMN, BUDGET_LAYER=BUDGET_LAYER, &
& VBUDGET_COLUMN=VBUDGET_COLUMN, VBUDGET_LAYER=VBUDGET_LAYER)
!NV_BCOLUMN  = SIZE(BUDGET_COLUMN, DIM=1)
!NTERM_BC    = SIZE(BUDGET_COLUMN, DIM=2)
NT_BC       = SIZE(BUDGET_COLUMN, DIM=3)
!NV_BLAYER   = SIZE(BUDGET_LAYER, DIM=1)
!NTERM_BL    = SIZE(BUDGET_LAYER, DIM=2)
NP_BL       = SIZE(BUDGET_LAYER, DIM=3)
NT_BL       = SIZE(BUDGET_LAYER, DIM=4)

!
! Check the dimensions in the BUDGET_COLUMN and BUDGET_LAYER arrays are the same size as in the D_FINAL array.
!
IF ((NT_BC .NE. NT_BL) .OR. (NT_BC .NE. NT)) THEN
    PRINT *,'E: NT_BC, NT_BL and NT are not the same: ',NT_BC, NT_BL, NT
    GOTO 9999
END IF

IF (NP_BL .NE. NP) THEN
    PRINT *,'E: NP_BL and NP are not the same: ',NP_BL, NP
    GOTO 9999
END IF

!
! Read a lot of surface data. This is essentially repeating a lot of stuff that was done in 2D_PUT. I'm sure we could read in
! the output of 2D_PUT to save a lot of work here, but this would casue the Fortran code to diverge too much from the IDL code,
! and make detection of bugs etc more difficult.
!
PRINT *,'Enter the name of the file which contains the surface data'
READ (FMT='(A512)', UNIT=5) INPUTFILE

CALL IPT_2DRAW_NETCDF(INPUTFILE, 'time', T)                                         ! Time (units in NetCDF file)
CALL IPT_2DRAW_NETCDF(INPUTFILE, 'sktsfc', MG_TSKIN)                                ! Surface temperature           (K)
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
CALL IPT_2DRAW_NETCDF(INPUTFILE, 'no2tsfc', MG_TAIR)                                ! 2m air temperature            (K)
CALL IPT_2DRAW_NETCDF(INPUTFILE, 'no2dsfc', MG_TDAIR)                               ! 2m dew-point temperature      (K)
CALL IPT_2DRAW_NETCDF(INPUTFILE, 'ssrdsfc', MG_SWSDN)                               ! Downward sfc solar radiation  (W/m^2 down)
CALL IPT_2DRAW_NETCDF(INPUTFILE, 'strdsfc', MG_LWSDN)                               ! Downward sfc LW radiation     (W/m^2 down)
CALL IPT_2DRAW_NETCDF(INPUTFILE, 'lccsfc', MG_LCC)                                  ! Low-level cloud cover         (%)
CALL IPT_2DRAW_NETCDF(INPUTFILE, 'mccsfc', MG_MCC)                                  ! Mid-level cloud cover         (%)
CALL IPT_2DRAW_NETCDF(INPUTFILE, 'hccsfc', MG_HCC)                                  ! High-level cloud cover        (%)
CALL IPT_2DRAW_NETCDF(INPUTFILE, 'tccsfc', MG_TCC)                                  ! Total cloud cover             (%)
CALL IPT_2DRAW_NETCDF(INPUTFILE, 'srsfc', MG_SR)                                    ! Surface roughness             (m)
CALL IPT_2DRAW_NETCDF(INPUTFILE, 'twcsfc', MG_PWC)                                  ! Total column water            (kg/m^2)
!CALL IPT_2DRAW_NETCDF(INPUTFILE, 'rhsfc', MG_RH)                                    ! Surface relative humidity     (%)
CALL IPT_2DRAW_NETCDF(INPUTFILE, 'zsfc', MG_ZSFC)                                   ! Surface geopotential.         (m)


! Now convert dew point temperature to relative humidity: (Using Bolton's formula for now --> check whether this is good enough
ALLOCATE(MG_RH(NT))
MG_RH = exp(17.67*((MG_TDAIR-273.15)/(MG_TDAIR-29.65) - ((MG_TAIR-273.15)/(MG_TAIR-29.65))))


!
! Read the MWR data.
!
PRINT *,'Enter the name of the file which contains the microwave radiometer'
READ (FMT='(A512)', UNIT=5) INPUTFILE

CALL IPT_2DRAW_NETCDF(INPUTFILE, 'be_pwv', MG_VAP)
CALL IPT_2DRAW_NETCDF(INPUTFILE, 'be_lwp', MG_LIQ)

IF (SIZE(MG_VAP,1) .NE. NT) THEN
    PRINT *,'E: Number of times in microwave radiometer file different than in ECMWF data file'
    GOTO 9999
END IF

!
! Read the radar rain data.
!
PRINT *,'Enter the name of the file which contains the radar precipitation data'
READ (FMT='(A512)', UNIT=5) INPUTFILE

CALL IPT_2DRAW_NETCDF(INPUTFILE, 'pret', MG_PREC)                                   ! Surface rainfall (mm/hour)
IF (SIZE(MG_PREC,1) .NE. NT) THEN
    PRINT *,'E: Number of times in precip file different than in background data file'
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
    CALL IPT_2DRAW_NETCDF(INPUTFILE, 'Ts', MG_TAIR)
    CALL IPT_2DRAW_NETCDF(INPUTFILE, 'Us', MG_U)
    CALL IPT_2DRAW_NETCDF(INPUTFILE, 'Vs', MG_V)
    CALL IPT_2DRAW_NETCDF(INPUTFILE, 'Ps', MG_P)
    CALL IPT_2DRAW_NETCDF(INPUTFILE, 'RHs', MG_RH)
END IF

!
! Now allocate space for some more surface variables, and change some of the ones we have already read in.
!
ALLOCATE(MG_RADS(NT), MG_LWSUP(NT), MG_SWSUP(NT), MG_WSPD(NT), MG_RO(NT), MG_TAOX(NT), &
& MG_TAOY(NT), MG_CLDDZ(NT), MG_CLDTZ(NT), STAT=MEMST)
IF (MEMST .NE. 0) THEN
    PRINT *,'E: Unable to allocate memory'
    GOTO 9999
END IF

MG_CLDDZ    = -9999.;
MG_CLDTZ    = -9999.;
MG_RADS     = MG_SWNS + MG_LWNS
MG_LWSUP    = MG_LWSDN - MG_LWNS
MG_SWSUP    = MG_SWSDN - MG_SWNS


MG_WSPD     = SQRT(MG_U**2 + MG_V**2)
MG_RO       = MG_P*100.0/RD/MG_TAIR
MG_TAOX     = -CD*MG_RO*MG_WSPD*MG_U
MG_TAOY     = -CD*MG_RO*MG_WSPD*MG_V

!
! Allow the use of observed surface radiation data This part differs from the original code, which blended ocean and land radiation
! fluxes. Instead, we assume this blending has already been performed in the data pre-processing stage.
!

    !
    ! Replace the ECMWF fields with observed fields. Be paranoid with memory (de)allocation issues.
    !
    PRINT *,'Enter the name of the file which contains the surface radiation observations'
    READ (FMT='(A512)', UNIT=5) INPUTFILE

!    IF (ALLOCATED(MG_RADS))     DEALLOCATE(MG_RADS)
!    IF (ALLOCATED(MG_LWSDN))    DEALLOCATE(MG_LWSDN)
!    IF (ALLOCATED(MG_LWSUP))    DEALLOCATE(MG_LWSUP)
!    IF (ALLOCATED(MG_SWSDN))    DEALLOCATE(MG_SWSDN)
!    IF (ALLOCATED(MG_SWSUP))    DEALLOCATE(MG_SWSUP)

    CALL IPT_2DRAW_NETCDF(INPUTFILE, 'surf_net', MG_RADS)
!    CALL IPT_2DRAW_NETCDF(INPUTFILE, 'lwup', MG_LWSUP)
!    CALL IPT_2DRAW_NETCDF(INPUTFILE, 'lwdn', MG_LWSDN)
!    CALL IPT_2DRAW_NETCDF(INPUTFILE, 'swup', MG_SWSUP)
!    CALL IPT_2DRAW_NETCDF(INPUTFILE, 'swdn', MG_SWSDN)

!
! Allow the use of satellite observed TOA radiation iand cloud data.
!
    !
    ! Replace the ECMWF fields with observed fields. Be paranoid with memory (de)allocation issues.
    !
    PRINT *,'Enter the name of the file which contains the satellite TOA radiation and cloud observations'
    READ (FMT='(A512)', UNIT=5) INPUTFILE

    IF (ALLOCATED(MG_SWT))      DEALLOCATE(MG_SWT)
    IF (ALLOCATED(MG_LWT))      DEALLOCATE(MG_LWT)
    IF (ALLOCATED(MG_INS))      DEALLOCATE(MG_INS)
!    IF (ALLOCATED(MG_LCC))      DEALLOCATE(MG_LCC)
!    IF (ALLOCATED(MG_MCC))      DEALLOCATE(MG_MCC)
!    IF (ALLOCATED(MG_HCC))      DEALLOCATE(MG_HCC)
!    IF (ALLOCATED(MG_TCC))      DEALLOCATE(MG_TCC)
!    IF (ALLOCATED(MG_CLDDZ))    DEALLOCATE(MG_CLDDZ)
!    IF (ALLOCATED(MG_CLDTZ))    DEALLOCATE(MG_CLDTZ)

    CALL IPT_2DRAW_NETCDF(INPUTFILE, 'swt', MG_SWT)
    CALL IPT_2DRAW_NETCDF(INPUTFILE, 'lwt', MG_LWT)
!    CALL IPT_2DRAW_NETCDF(INPUTFILE, 'ins', MG_INS)
!    CALL IPT_2DRAW_NETCDF(INPUTFILE, 'cldtot', MG_TCC)
!    CALL IPT_2DRAW_NETCDF(INPUTFILE, 'cldhgh', MG_HCC)
!    CALL IPT_2DRAW_NETCDF(INPUTFILE, 'cldmid', MG_MCC)
!    CALL IPT_2DRAW_NETCDF(INPUTFILE, 'cldlow', MG_LCC)
!    CALL IPT_2DRAW_NETCDF(INPUTFILE, 'cldtz', MG_CLDTZ)
!    CALL IPT_2DRAW_NETCDF(INPUTFILE, 'clddz', MG_CLDDZ)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! TOA data must be in Wm^-2 DOWN.
! (Both in ECMWF and obs file)
!
MG_LWT      = -MG_LWT
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


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

PRINT *,'Enter the name of the surface pressure data file'
READ (FMT='(A512)', UNIT=5) INPUTFILE
CALL IPT_HT(TRIM(INPUTFILE), INSTRUMENTF, NV1, NSTF, NT, V1, STF, LONF, LATF, T, DF_P)

!
! Now start output of single level fields.
!
ALLOCATE(YY(NT), MO(NT), DY(NT), HH(NT), MM(NT), STAT=MEMST)
IF (MEMST .NE. 0) THEN
    PRINT *,'E: Unable to allocate memory'
    GOTO 9999
END IF

CALL DAYS_TO_DATE(STYEAR=START_YEAR,STMONTH=START_MONTH,STDAY=START_DAY, DAYS=T, YYYY=YY, MO=MO, DD=DY, HH=HH, MM=MM)

STRHEAD = (/'Calday                                   ', 'Year                                     ', &
          & 'Month                                    ', 'Day                                      ', &
          & 'Hour                                     ', 'Minute                                   ', &
          & 'Prec(mm/hour)                            ', 'LH_(upward_W/m^2                         ', &
          & 'SH_(upward_W/m2)                         ', 'Area_Mean_Ps(mb)                         ', &
          & 'Central_Facility_Ps(mb)                  ', 'Ts_Air(C)                                ', &
          & 'Ts_skin(C)                               ', 'Sfc_Air_RH(%)                            ', &
          & 'Srf_wind_speed(m/s)                      ', 'u_wind_(m/s)                             ', &
          & 'v_wind(m/s)                              ', 'Srf_Net_Dn_Rad(W/m2)                     ', &
          & 'TOA_LW_Up(W/m2)                          ', 'TOA_SW_Dn(W/m2)                          ', &
          & 'Srf_Dew_temp (C)                         ', 'Lowcld(%)                                ', &
          & 'Midcld(%)                                ', 'Hghcld(%)                                ', &
          & 'Totcld(%)                                ', 'Albedo()                                 ', &
          & 'Cld_ice_path(cm)                         ', 'Cld_liquid_path(cm)                      ', &
          & 'd(Column_H2O)/dt_(mm/hour)               ', 'Column_H2O_Advection_(mm/hour)           ', &
          & 'Srf_Evaporation_(mm/hour)                ', 'd(Column_Dry_Static_Energy)/dt_(W/m2)    ', &
          & 'Column_Dry_Static_Energy_Advection_(W/m2)', 'Column_Radiative_Heating_(W/m2)          ', &
          & 'Column_Latent_heating_(W/m2)             ', 'omega_surface_(mb/hr)                    ', &
          & 'qs_surface_(kg/kg)                       ', 's_surface_(K)                            ', &
          & 'precip_water_(cm)                        ', 'Srf_LWUP_(W/m2)                          ', &
          & 'Srf_LWDN_(W/m2)                          ', 'Srf_SWUP_(W/m2)                          ', &
          & 'Srf_SWDN_(W/m2)                          '/)

!
! Allocate the array which holds the surface output data, and then fill it.
!
ALLOCATE(DD(SIZE(STRHEAD),NT-2), STAT=MEMST)
IF (MEMST .NE. 0) THEN
    PRINT *,'E: Unable to allocate memory for DD array.'
    GOTO 9999
END IF

DD(1,:)     = T(2:NT-1)
DD(2,:)     = YY(2:NT-1)
DD(3,:)     = MO(2:NT-1)
DD(4,:)     = DY(2:NT-1)
DD(5,:)     = HH(2:NT-1)
DD(6,:)     = MM(2:NT-1)
DD(7,:)     = MG_PREC(2:NT-1)
DD(8,:)     = -MG_LH(2:NT-1)
DD(9,:)     = -MG_SH(2:NT-1)
DD(10,:)    = MG_P(2:NT-1)
DD(11,:)    = DF_P(2,6,2:NT-1)                       ! Pressure from the central facility.
DD(12,:)    = MG_TAIR(2:NT-1) - T0                   ! Convert from K to C.
DD(13,:)    = MG_TSKIN(2:NT-1) - T0                  ! Convert from K to C.
DD(14,:)    = MG_RH(2:NT-1)
DD(15,:)    = MG_WSPD(2:NT-1)
DD(16,:)    = MG_U(2:NT-1)
DD(17,:)    = MG_V(2:NT-1)
DD(18,:)    = MG_RADS(2:NT-1)
DD(19,:)    = MG_LWT(2:NT-1)
DD(20,:)    = MG_SWT(2:NT-1)
!DD(21,:)    = MG_INS(2:NT-1) ! don't have any insolation values.
!DD(21,:)    = MG_SWT(2:NT-1) - MG_LWT(2:NT-1)
DD(22,:)    = MG_LCC(2:NT-1)
DD(23,:)    = MG_MCC(2:NT-1)
DD(24,:)    = MG_HCC(2:NT-1)
DD(25,:)    = MG_TCC(2:NT-1)
DD(26,:)    = MG_CLDDZ(2:NT-1)
DD(27,:)    = MG_CLDTZ(2:NT-1)
DD(28,:)    = MG_LIQ(2:NT-1)
DD(29,:)    = -BUDGET_COLUMN(2,3,2:NT-1)*CPD/LV*3600.
DD(30,:)    = BUDGET_COLUMN(2,7,2:NT-1)*CPD/LV*3600.
DD(31,:)    = BUDGET_COLUMN(2,4,2:NT-1)*CPD/LV*3600.
DD(32,:)    = -BUDGET_COLUMN(3,3,2:NT-1)*CPD
DD(33,:)    = BUDGET_COLUMN(3,7,2:NT-1)*CPD
DD(34,:)    = BUDGET_COLUMN(3,4,2:NT-1)*CPD
DD(35,:)    = BUDGET_COLUMN(3,5,2:NT-1)*CPD
DD(36,:)    = -BUDGET_COLUMN(1,3,2:NT-1)*9.8*36.0
DD(37,:)    = BUDGET_COLUMN(1,5,2:NT-1)*CPD/LV
DD(38,:)    = BUDGET_COLUMN(1,6,2:NT-1)
DD(39,:)    = MG_VAP(2:NT-1)
DD(40,:)    = MG_LWSUP(2:NT-1)
DD(41,:)    = MG_LWSDN(2:NT-1)
DD(42,:)    = MG_SWSUP(2:NT-1)
DD(43,:)    = MG_SWSDN(2:NT-1)

!
! We now derive the skin temperature from the surface LW with emissivity 0.98.
!
DD(13,:)    = ((DD(40,:) - (1-0.98)*DD(41,:))/(0.98*5.67*1e-8))**0.25 -T0

!
! Now deal with the multi-layer fields. We deal with data on 25 hPa levels slightly differently than data on 50 hPa levels. I
! think all of this ugliness results from hacks being added to the code over many years. I'm too tired to sort this out now, so will
! reproduce what is ugly, but works.
!
STRHEAD2    = (/'Temp_(K)                      ','H2O_Mixing_Ratio_(g/kg)       ', &
              & 'u_wind_(m/s)                  ','v_wind_(m/s)                  ', &
              & 'omega_(mb/hour)               ','Wind_Div_(1/s)                ', &
              & 'Horizontal_Temp_Advec_(K/hour)','Vertical_T_Advec(K/hour)      ', &
              & 'Horizontal_q_Advec_(g/kg/hour)','Vertical_q_Advec(g/kg/hour)   ', &
              & 's(Dry_Static_Energy)(K)       ','Horizontal_s_Advec_(K/hour)   ', &
              & 'Vertical_s_Advec(K/hour)      ','ds/dt(K/hour)                 ', &
              & 'DT/dt(K/hour)                 ','dq/dt_(g/kg/hour)             ', &
              & 'Q1_(k/hour)                   ','Q2_(K/hour)                   ' /)

IF (INT(P(1)-P(2)+0.5, IK4) .EQ. 50) THEN
    !
    ! Data are on 50 hPa levels.
    !
    ALLOCATE(DD2(SIZE(STRHEAD2),NP-4,NT-2), PNEW(NP-4), TNEW(NT-2), STAT=MEMST)
    IF (MEMST .NE. 0) THEN
        PRINT *,'E: Unable to allocate memory for DD2, PNEW or TNEW array.'
        GOTO 9999
    END IF

    PNEW(1:NP-4)            = P(4:NP-1)
    TNEW(:)                 = T(2:NT-1)
    DD2(1,1:NP-4,:)    = BUDGET_LAYER(6,1,5:NP-1,2:NT-1)                                           ! K
    DD2(2,1:NP-4,:)    = BUDGET_LAYER(2,1,5:NP-1,2:NT-1)*CPD/LV*1000.0                             ! g/kg
    DD2(3,1:NP-4,:)    = BUDGET_LAYER(4,1,5:NP-1,2:NT-1)                                           ! u(m/s)
    DD2(4,1:NP-4,:)    = BUDGET_LAYER(5,1,5:NP-1,2:NT-1)                                           ! v(m/s)
    DD2(5,1:NP-4,:)    = BUDGET_LAYER(1,6,5:NP-1,2:NT-1)*3600.                                     ! w(hPa/hr)
    DD2(6,1:NP-4,:)    = -BUDGET_LAYER(1,3,5:NP-1,2:NT-1)                                          ! div 1/s
    DD2(7,1:NP-4,:)    = BUDGET_LAYER(6,3,5:NP-1,2:NT-1)*3600.                                     ! K/hr
    DD2(8,1:NP-4,:)    = BUDGET_LAYER(6,4,5:NP-1,2:NT-1)*3600.                                     ! K/hr
    DD2(9,1:NP-4,:)    = BUDGET_LAYER(2,3,5:NP-1,2:NT-1)*CPD/LV*3.6*1.0E6
    DD2(10,1:NP-4,:)   = BUDGET_LAYER(2,4,5:NP-1,2:NT-1)*CPD/LV*3.6*1.0E6
    DD2(11,1:NP-4,:)   = BUDGET_LAYER(3,1,5:NP-1,2:NT-1)*3600.                                     ! K
    DD2(12,1:NP-4,:)   = BUDGET_LAYER(3,3,5:NP-1,2:NT-1)*3600.                                     ! K/hr
    DD2(13,1:NP-4,:)   = BUDGET_LAYER(3,4,5:NP-1,2:NT-1)*3600.                                     ! K/hr
    DD2(14,1:NP-4,:)   = -BUDGET_LAYER(3,2,5:NP-1,2:NT-1)*3600.                                    ! K/hr
    DD2(15,1:NP-4,:)   = -BUDGET_LAYER(6,2,5:NP-1,2:NT-1)*3600.                                    ! K/hr
    DD2(16,1:NP-4,:)   = -BUDGET_LAYER(2,2,5:NP-1,2:NT-1)*CPD/LV*3.6*1.0E6
    DD2(17,1:NP-4,:)   = BUDGET_LAYER(3,5,5:NP-1,2:NT-1)*3600.                                     ! K/hr
    DD2(18,1:NP-4,:)   = -BUDGET_LAYER(2,5,5:NP-1,2:NT-1)*3600.                                    ! K/hr
    ! Xie 08/09/02 (T advection derived from S)
    DD2(7,1:NP-4,:)    = DD2(12,1:NP-4,:) - (DD2(14,1:NP-4,:)-DD2(15,1:NP-4,:))     ! K/hr

    NTNEW   = NT-2
    NPNEW   = NP-5
    !
    ! At this point, the IDL code had a big nested loop which didn't do anything (all "actions" had been commmented out).
    ! The purpose of the original loop was to look for missing values (and put -9999.0 everywhere missing data occurred).
    !
ELSE IF (INT(P(1)-P(2)+0.5, IK4) .EQ. 25) THEN
    !
    ! Data are on 25 hPa levels.
    !
    ALLOCATE(DD2(SIZE(STRHEAD2),NP-5,NT-2), PNEW(NP-5), TNEW(NT-2), STAT=MEMST)
    IF (MEMST .NE. 0) THEN
        PRINT *,'E: Unable to allocate memory for DD2, PNEW or TNEW array.'
        GOTO 9999
    END IF
    PNEW(1:NP-5)                = P(5:NP-1)
    TNEW(:)                = T(2:NT-1)
    DD2(1,1:NP-5,:)        = BUDGET_LAYER(6,1,5:NP-1,2:NT-1)                   ! (K)
    DD2(2,1:NP-5,:)        = BUDGET_LAYER(2,1,5:NP-1,2:NT-1)*CPD/LV*1000.0     ! g/kg
    DD2(3,1:NP-5,:)        = BUDGET_LAYER(4,1,5:NP-1,2:NT-1)                   ! u (m/s)
    DD2(4,1:NP-5,:)        = BUDGET_LAYER(5,1,5:NP-1,2:NT-1)                   ! v (m/s)
    DD2(5,1:NP-5,:)        = BUDGET_LAYER(1,6,5:NP-1,2:NT-1)*3600.0            ! w (hPa/hr)
    DD2(6,1:NP-5,:)        = -BUDGET_LAYER(1,3,5:NP-1,2:NT-1)                  ! div (1/s)
    DD2(7,1:NP-5,:)        = BUDGET_LAYER(6,3,5:NP-1,2:NT-1)*3600.0            ! K/hr
    DD2(8,1:NP-5,:)        = BUDGET_LAYER(6,4,5:NP-1,2:NT-1)*3600.0            ! K/hr
    DD2(9,1:NP-5,:)        = BUDGET_LAYER(2,3,5:NP-1,2:NT-1)*CPD/LV*3.6*1.0E6
    DD2(10,1:NP-5,:)       = BUDGET_LAYER(2,4,5:NP-1,2:NT-1)*CPD/LV*3.6*1.0E6
    DD2(11,1:NP-5,:)       = BUDGET_LAYER(3,1,5:NP-1,2:NT-1)                   ! K
    DD2(12,1:NP-5,:)       = BUDGET_LAYER(3,3,5:NP-1,2:NT-1)*3600.0            ! K/hr
    DD2(13,1:NP-5,:)       = BUDGET_LAYER(3,4,5:NP-1,2:NT-1)*3600.0            ! K/hr
    DD2(14,1:NP-5,:)       = -BUDGET_LAYER(3,2,5:NP-1,2:NT-1)*3600.0           ! K/hr
    DD2(15,1:NP-5,:)       = -BUDGET_LAYER(6,2,5:NP-1,2:NT-1)*3600.0           ! K/hr
    DD2(16,1:NP-5,:)       = -BUDGET_LAYER(2,2,5:NP-1,2:NT-1)*CPD/LV*3.6*1.0E6
    DD2(17,1:NP-5,:)       = BUDGET_LAYER(3,5,5:NP-1,2:NT-1)*3600.0            ! K/hr
    DD2(18,1:NP-5,:)       = -BUDGET_LAYER(2,5,5:NP-1,2:NT-1)*3600.0           ! K/hr
    ! Xie 08/09/02 (T advection derived from S)
    DD2(7,1:NP-5,:)        = DD2(12,1:NP-5,:) - (DD2(14,1:NP-5,:) - DD2(15,1:NP-5,:))   ! K/hr

    DO II=1,NP-5
        IF (PNEW(II) .LE. 90) DD2(7,II,:)  = 0.0
    END DO

    NTNEW   = NT-2
    NPNEW   = NP-5

    DO II=NPNEW,1,-1
        DO JJ=1,NTNEW
            IF (PNEW(II) .GT. MG_P(JJ+1)) THEN
                DO KK=1,SIZE(STRHEAD2)
                    DD2(KK,II,JJ)   = DD2(KK,II+1,JJ)
                END DO
            END IF
        END DO
    END DO
ELSE
    PRINT *,'E: Multi-level data have a non-standard spacing: ',P(1)-P(2),' hPa'
    GOTO 9999
END IF

!
! Write all the forcing data to a NetCDF file.
!
PRINT *,'Enter the name of the NetCDF-level output file'
READ (FMT='(A512)', UNIT=5) OUTPUTFILE

CALL OPT_FORCING_NETCDF(OUTPUTFILE, SFC_DATA=DD, ML_DATA=DD2(:,:,:), CF_LON=-9999., CF_LAT=-9999., &
    & CF_PHIS=REAL(MG_ZSFC(1),KIND=RK4), PLEVS=PNEW)

!
! Now write the multi-level data to disc.
!
PRINT *,'Enter the name of the multi-level output file'
READ (FMT='(A512)', UNIT=5) OUTPUTFILE

OPEN (UNIT=1, FILE=OUTPUTFILE, STATUS='NEW', IOSTAT=IOSTATUS)
IF (IOSTATUS .NE. 0) THEN
    PRINT *,'E: Unable to open ',TRIM(OUTPUTFILE),' FOR WRITING'
    GOTO 9999
END IF

!
! We'll catch if any of these I/O statements failed at the end of the WRITE statements.
!
WRITE(UNIT=1, FMT='(A)', IOSTAT=IOSTATUS) 'Length of each field (nt):'
WRITE(UNIT=1, FMT='(I5)', IOSTAT=TMP_IOSTATUS) NTNEW
IOSTATUS = IOSTATUS + ABS(TMP_IOSTATUS)
WRITE(UNIT=1, FMT='(A)', IOSTAT=TMP_IOSTATUS) 'Number of Pressure levels (np):'
IOSTATUS = IOSTATUS + ABS(TMP_IOSTATUS)
WRITE(UNIT=1, FMT='(I5)', IOSTAT=TMP_IOSTATUS) NPNEW
IOSTATUS = IOSTATUS + ABS(TMP_IOSTATUS)
WRITE(UNIT=1, FMT='(A)', IOSTAT=TMP_IOSTATUS) 'Pressure Levels p(np)_mb:'
IOSTATUS = IOSTATUS + ABS(TMP_IOSTATUS)
WRITE(UNIT=1, FMT='(5E15.7)', IOSTAT=TMP_IOSTATUS) PNEW
IOSTATUS = IOSTATUS + ABS(TMP_IOSTATUS)
!
! Times (year, month, day etc) really should be stored as integers. Never mind.
!
WRITE(UNIT=1, FMT='(A)', IOSTAT=TMP_IOSTATUS) 'Time t(nt):'
IOSTATUS = IOSTATUS + ABS(TMP_IOSTATUS)
WRITE(UNIT=1, FMT='(5E15.7)', IOSTAT=TMP_IOSTATUS) T(1:NT-1)
IOSTATUS = IOSTATUS + ABS(TMP_IOSTATUS)
WRITE(UNIT=1, FMT='(A)', IOSTAT=TMP_IOSTATUS) 'Year yy(nt):'
IOSTATUS = IOSTATUS + ABS(TMP_IOSTATUS)
WRITE(UNIT=1, FMT='(5E15.7)', IOSTAT=TMP_IOSTATUS) REAL(YY(1:NT-1), KIND=RK8)
IOSTATUS = IOSTATUS + ABS(TMP_IOSTATUS)
WRITE(UNIT=1, FMT='(A)', IOSTAT=TMP_IOSTATUS) 'Month mo(nt):'
IOSTATUS = IOSTATUS + ABS(TMP_IOSTATUS)
WRITE(UNIT=1, FMT='(5E15.7)', IOSTAT=TMP_IOSTATUS) REAL(MO(1:NT-1), KIND=RK8)
IOSTATUS = IOSTATUS + ABS(TMP_IOSTATUS)
WRITE(UNIT=1, FMT='(A)', IOSTAT=TMP_IOSTATUS) 'Day dy(nt):'
IOSTATUS = IOSTATUS + ABS(TMP_IOSTATUS)
WRITE(UNIT=1, FMT='(5E15.7)', IOSTAT=TMP_IOSTATUS) REAL(DY(1:NT-1), KIND=RK8)
IOSTATUS = IOSTATUS + ABS(TMP_IOSTATUS)
WRITE(UNIT=1, FMT='(A)', IOSTAT=TMP_IOSTATUS) 'Hour hh(nt):'
IOSTATUS = IOSTATUS + ABS(TMP_IOSTATUS)
WRITE(UNIT=1, FMT='(5E15.7)', IOSTAT=TMP_IOSTATUS) REAL(HH(1:NT-1), KIND=RK8)
IOSTATUS = IOSTATUS + ABS(TMP_IOSTATUS)
WRITE(UNIT=1, FMT='(A)', IOSTAT=TMP_IOSTATUS) 'Minutes mm(nt):'
IOSTATUS = IOSTATUS + ABS(TMP_IOSTATUS)
WRITE(UNIT=1, FMT='(5E15.7)', IOSTAT=TMP_IOSTATUS) REAL( MM(1:NT-1), KIND=RK8)
IOSTATUS = IOSTATUS + ABS(TMP_IOSTATUS)

WRITE(UNIT=1, FMT='(A)', IOSTAT=TMP_IOSTATUS) 'Number of Multi-Level Fields:'
IOSTATUS = IOSTATUS + ABS(TMP_IOSTATUS)
WRITE(UNIT=1, FMT='(I5)', IOSTAT=TMP_IOSTATUS) SIZE(STRHEAD2)
IOSTATUS = IOSTATUS + ABS(TMP_IOSTATUS)
DO II=1,SIZE(STRHEAD2)
    WRITE(UNIT=1, FMT='(A)', IOSTAT=TMP_IOSTATUS) TRIM(STRHEAD2(II)) 
    IOSTATUS = IOSTATUS + ABS(TMP_IOSTATUS)
    DO JJ=1,NPNEW
        WRITE(UNIT=1, FMT='(5E15.7)', IOSTAT=TMP_IOSTATUS) DD2(II,JJ,1:NTNEW)
        IOSTATUS = IOSTATUS + ABS(TMP_IOSTATUS)
    END DO
END DO

IF (IOSTATUS .NE. 0) THEN
    PRINT *,'E: One or more of the multi-level WRITE statements to ',TRIM(OUTPUTFILE),' failed'
    GOTO 9999
END IF

CLOSE (UNIT=1, IOSTAT=IOSTATUS)
IF (IOSTATUS .NE. 0) THEN
    PRINT *,'E: Unable to close ',OUTPUTFILE
    GOTO 9999
END IF


9999 CONTINUE               ! Previously detected errors will cause program execution to jump here.
!
! De-allocate allocated arrays. This is mainly for tidiness.
!
IF (ALLOCATED(BUDGET_LAYER))                    DEALLOCATE(BUDGET_LAYER)
IF (ALLOCATED(BUDGET_COLUMN))                   DEALLOCATE(BUDGET_COLUMN)
IF (ALLOCATED(VBUDGET_COLUMN))                  DEALLOCATE(VBUDGET_COLUMN)
IF (ALLOCATED(VBUDGET_LAYER))                   DEALLOCATE(VBUDGET_LAYER)
IF (ALLOCATED(D_FINAL))                         DEALLOCATE(D_FINAL)
IF (ALLOCATED(V))                               DEALLOCATE(V)
IF (ALLOCATED(ST))                              DEALLOCATE(ST)
IF (ALLOCATED(P))                               DEALLOCATE(P)
IF (ALLOCATED(T))                               DEALLOCATE(T)
IF (ALLOCATED(MG_TSKIN))                        DEALLOCATE(MG_TSKIN)
IF (ALLOCATED(MG_T))                            DEALLOCATE(MG_T)
IF (ALLOCATED(MG_P))                            DEALLOCATE(MG_P)
IF (ALLOCATED(MG_TAOX1))                        DEALLOCATE(MG_TAOX1)
IF (ALLOCATED(MG_TAOY1))                        DEALLOCATE(MG_TAOY1)
IF (ALLOCATED(MG_U))                            DEALLOCATE(MG_U)
IF (ALLOCATED(MG_V))                            DEALLOCATE(MG_V)
IF (ALLOCATED(MG_SH))                           DEALLOCATE(MG_SH)
IF (ALLOCATED(MG_LH))                           DEALLOCATE(MG_LH)
IF (ALLOCATED(MG_SWNS))                         DEALLOCATE(MG_SWNS)
IF (ALLOCATED(MG_LWNS))                         DEALLOCATE(MG_LWNS)
IF (ALLOCATED(MG_SWT))                          DEALLOCATE(MG_SWT)
IF (ALLOCATED(MG_LWT))                          DEALLOCATE(MG_LWT)
IF (ALLOCATED(MG_INS))                          DEALLOCATE(MG_INS)
IF (ALLOCATED(MG_CLDTZ))                        DEALLOCATE(MG_CLDTZ)
IF (ALLOCATED(MG_CLDDZ))                        DEALLOCATE(MG_CLDDZ)
IF (ALLOCATED(MG_TAIR))                         DEALLOCATE(MG_TAIR)
IF (ALLOCATED(MG_TDAIR))                        DEALLOCATE(MG_TDAIR)
IF (ALLOCATED(MG_SWSDN))                        DEALLOCATE(MG_SWSDN)
IF (ALLOCATED(MG_LWSDN))                        DEALLOCATE(MG_LWSDN)
IF (ALLOCATED(MG_LCC))                          DEALLOCATE(MG_LCC)
IF (ALLOCATED(MG_MCC))                          DEALLOCATE(MG_MCC)
IF (ALLOCATED(MG_HCC))                          DEALLOCATE(MG_HCC)
IF (ALLOCATED(MG_TCC))                          DEALLOCATE(MG_TCC)
IF (ALLOCATED(MG_SR))                           DEALLOCATE(MG_SR)
IF (ALLOCATED(MG_PWC))                          DEALLOCATE(MG_PWC)
IF (ALLOCATED(MG_RH))                           DEALLOCATE(MG_RH)
IF (ALLOCATED(MG_PREC))                         DEALLOCATE(MG_PREC)
IF (ALLOCATED(MG_LIQ))                          DEALLOCATE(MG_LIQ)
IF (ALLOCATED(MG_VAP))                          DEALLOCATE(MG_VAP)
IF (ALLOCATED(MG_LWSUP))                        DEALLOCATE(MG_LWSUP)
IF (ALLOCATED(MG_SWSUP))                        DEALLOCATE(MG_SWSUP)
IF (ALLOCATED(MG_WSPD))                         DEALLOCATE(MG_WSPD)
IF (ALLOCATED(MG_RO))                           DEALLOCATE(MG_RO)
IF (ALLOCATED(MG_TAOX))                         DEALLOCATE(MG_TAOX)
IF (ALLOCATED(MG_TAOY))                         DEALLOCATE(MG_TAOY)
IF (ALLOCATED(V1))                              DEALLOCATE(V1)
IF (ALLOCATED(STF))                             DEALLOCATE(STF)
IF (ALLOCATED(LONF))                            DEALLOCATE(LONF)
IF (ALLOCATED(LATF))                            DEALLOCATE(LATF)
IF (ALLOCATED(DF_P))                            DEALLOCATE(DF_P)
IF (ALLOCATED(YY))                              DEALLOCATE(YY)
IF (ALLOCATED(MO))                              DEALLOCATE(MO)
IF (ALLOCATED(DY))                              DEALLOCATE(DY)
IF (ALLOCATED(HH))                              DEALLOCATE(HH)
IF (ALLOCATED(MM))                              DEALLOCATE(MM)
IF (ALLOCATED(DD))                              DEALLOCATE(DD)
IF (ALLOCATED(DD2))                             DEALLOCATE(DD2)
IF (ALLOCATED(PNEW))                            DEALLOCATE(PNEW)
IF (ALLOCATED(TNEW))                            DEALLOCATE(TNEW)

END PROGRAM PROCESS_VA_OUTPUT
