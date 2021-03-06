!***********************************************************************************************************************************
! BUDGET_PUT
!
! This is a port of the IDL procedure with the same name.
!
! Tim Hume.
! 12 October 2006.
!***********************************************************************************************************************************

PROGRAM BUDGET_PUT
USE PORTABLE
USE CONSTANTS
USE SETTINGS
USE IO
USE PHYSICS
USE NUMERICS

!
! Local variables.
!
IMPLICIT NONE

CHARACTER (LEN=64), DIMENSION(NVBUDGET_COLUMN,NTERMMAX)                 :: VBUDGET_COLUMN   ! Description of terms in BUDGET_COLUMN.
CHARACTER (LEN=64), DIMENSION(NVBUDGET_LAYER,NTERMMAXV)                 :: VBUDGET_LAYER    ! Description of terms in BUDGET_LAYER.
REAL (KIND=RK8), DIMENSION(:,:,:), ALLOCATABLE                          :: BUDGET_COLUMN    ! Budget terms used in the assimilation.
REAL (KIND=RK8), DIMENSION(:,:,:,:), ALLOCATABLE                        :: BUDGET_LAYER     ! Budget terms used in the assimilation.

CHARACTER (LEN=512)                                                     :: INPUTFILE        ! Input data filename.
CHARACTER (LEN=512)                                                     :: OUTPUTFILE       ! Output data filename.
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE                              :: T                ! Time steps.
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE                              :: BAR_PRES         ! Surface pressure.
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE                              :: ADPS             ! V.grad(PS)
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE                              :: PRECIP           ! Latent heating rate - precipitation.
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE                              :: LPRECIP          ! Latent heating rate - precipitation.
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE                              :: EVAPOR           ! Latent heating rate - evaporation.
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE                              :: SHF              ! Sensible heating rate at surface.
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE                              :: RL               ! Cloud liquid latent heating rate.
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE                              :: RADIATIONT       ! Net radiation (TOA).
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE                              :: RADIATIONB       ! Net radiation (SFC).
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE                              :: RADIATION        ! Column radiation.
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE                              :: TAOX
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE                              :: TAOY
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE                              :: AVE_QS
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE                              :: AVE_SS
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE                              :: DPSDT
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE                              :: VDPS
REAL (KIND=RK8), DIMENSION(:,:,:), ALLOCATABLE                          :: DS               ! Surface data from 3d_put.
REAL (KIND=RK8), DIMENSION(:), ALLOCATABLE                              :: LEVELS           ! Vertical levels in var. analysis.
INTEGER (KIND=IK4), DIMENSION(:,:), ALLOCATABLE                         :: BOUNDARY         ! Boundary array.

INTEGER (KIND=IK4)                                                      :: NTU              ! Number of time steps.
REAL (KIND=RK8)                                                         :: DTU              ! Time step (in seconds).
INTEGER (KIND=IK4)                                                      :: NP               ! Number of vertical levels.
INTEGER (KIND=IK4)                                                      :: MEMST            ! Status code from ALLOCATE.            
INTEGER (KIND=IK4)                                                      :: LL               ! Counter.

!
! First we need to read in the stuff generated by the 2D_PUT and 3D_PUT programs.
!
PRINT *,'Enter the name of the 2D data file (written by 2D_PUT)'
READ (FMT='(A512)', UNIT=5) INPUTFILE
CALL IPT_2D_NETCDF(INPUTFILE, T, BAR_PRES, ADPS, PRECIP, LPRECIP, EVAPOR, SHF, RL, RADIATIONT, RADIATIONB, RADIATION, TAOX, TAOY)

PRINT *,'Enter the name of the 3D data file (written by 3D_PUT)'
READ (FMT='(A512)', UNIT=5) INPUTFILE
CALL IPT_3D_NETCDF(INPUTFILE, DS=DS, LEV=LEVELS, BOUNDARY=BOUNDARY)

!
! VBUDGET_COLUMN is an array containing the names of the terms in the BUDGET_COLUMN array.
!
VBUDGET_COLUMN = RESHAPE(SOURCE= &
&(/'mass (kg/s/m^2)      ','water*Lv/Cp (K/s/m^2)','heat/CP (K/s/m^2)    ','momen_x (N/m^2)      ','momen_y (N/m^2)      ',&
&  'rsd_mass             ','rsd_water            ','rsd_heat             ','rsd_momen_x          ','rsd_momen_y          ',&
&  '-Dps/dt              ','-dsr/dt              ','-dsh/dt              ','-dsu/dt              ','-dsv/dt              ',&
&  '-sdivuv              ','evapor               ','radiation            ','taox                 ','taoy                 ',&
&  '                     ','-precip              ','L*precip             ','-sdivuvu             ','-sdivuvv             ',&
&  '                     ','-dsrldt              ','shf                  ','fcx                  ','fcy                  ',&
&  '                     ','-sdivuvr             ','-sdivuvh             ','fpx                  ','fpy                  '/), &
& SHAPE=(/NVBUDGET_COLUMN,NTERMMAX/))

!
! Allocate memory for various arrays.
!
NTU = SIZE(T)                       ! Number of time steps.
DTU = (T(2) - T(1))*24.0*3600.0     ! Time step (convert from days to seconds).
NP  = SIZE(LEVELS)                  ! Number of levels in the variational analysis.

ALLOCATE(BUDGET_COLUMN(NVBUDGET_COLUMN,NTERMMAX,NTU), BUDGET_LAYER(NVBUDGET_LAYER,NTERMMAXV,NP,NTU), &
& AVE_QS(NTU), AVE_SS(NTU), DPSDT(NTU), VDPS(NTU), STAT=MEMST)
IF (MEMST .NE. 0) THEN
    PRINT *,'E: Unable to allocate memory'
    STOP 1
END IF

!
! Fill the BUDGET_COLUMN array. This is a direct port of the IDL code.
!
DO LL=2,NTU-1
    BUDGET_COLUMN(1,1,LL)   = 2
    BUDGET_COLUMN(2,1,LL)   = 5
    BUDGET_COLUMN(3,1,LL)   = 5
    BUDGET_COLUMN(4,1,LL)   = 3
    BUDGET_COLUMN(5,1,LL)   = 3
    BUDGET_COLUMN(1,3,LL)   = 0.0
    DPSDT(LL)               = 0.0
    ADPS(LL)                = 0.0
    VDPS(LL)                = ADPS(LL)
    BUDGET_COLUMN(1,7,LL)   = BAR_PRES(LL)/100.0                                                    ! Convert Pa to hPa
    BUDGET_COLUMN(2,4,LL)   = EVAPOR(LL)
    BUDGET_COLUMN(2,5,LL)   = -PRECIP(LL)
    BUDGET_COLUMN(2,6,LL)   = -(RL(LL+1) - RL(LL-1))/2.0/DTU
    AVE_QS(LL)              = SUM(DS(2,:,LL))/SIZE(DS(2,:,LL)) 
    BUDGET_COLUMN(1,5,LL)   = AVE_QS(LL)
    BUDGET_COLUMN(2,6,LL)   = -(RL(LL+1) - RL(LL-1))/2.0/DTU + BUDGET_COLUMN(1,3,LL)*AVE_QS(LL)     ! K/m^2/s
    BUDGET_COLUMN(3,4,LL)   = RADIATION(LL)
    BUDGET_COLUMN(3,5,LL)   = LPRECIP(LL)
    AVE_SS(LL)              = SUM(DS(3,:,LL))/SIZE(DS(3,:,LL))                                      ! K
    BUDGET_COLUMN(3,6,LL)   = SHF(LL) + (RL(LL+1) - RL(LL-1))/2.0/DTU + BUDGET_COLUMN(1,3,LL)*AVE_SS(LL)
    BUDGET_COLUMN(1,6,LL)   = AVE_SS(LL)
    BUDGET_COLUMN(4,4,LL)   = TAOX(LL)
    BUDGET_COLUMN(5,4,LL)   = TAOY(LL)
END DO

BUDGET_COLUMN(1,7,1)    = BAR_PRES(1)/100.0             ! Convert Pa to hPa.
BUDGET_COLUMN(1,7,NTU)  = BAR_PRES(NTU)/100.0           ! Convert Pa to hPa.

!
! Fill out the VBUDGET_LAYER array.
!
VBUDGET_LAYER = RESHAPE(SOURCE= &
&(/'                    ','(avg_r)*L/Cpd       ','(avg_Ed)/Cpd        ','avg_u               ', &
                                            &    'avg_v               ','avg_T               ', &
&  '                    ','(-dr/dt)*L/Cpd      ','(-dEd/dt)/Cpd       ','-du/dt              ', &
                                            &    '-dv/dt              ','-dT/dt              ', &
&  '-divuv              ','(-divuvr)*L/Cpd     ','(-divuvEd)/Cpd      ','-divuvu             ', &
                                            &    '-divuvv             ','-divuvT             ', &
&  '-domega/dp          ','-d(omega*r)/dp*L/Cpd','-d(omega*Ed)/dp/Cpd ','-d(omega*u)/dp      ', &
                                            &    '-d(omega*v)/dp      ','-d(omega*T)/dp      ', &
&  '                    ','-Q2/Cpd             ','Q1/Cpd              ','FCx                 ', &
                                            &    'FCy                 ','                    ', &
&  'omega               ','omega*r*L/Cpd       ','omega*Ed/Cpd        ','FPx                 ', &
                                            &    'FPy                 ','omega*T             ', &
&  '                    ','(Q1-Q2-QR)/Cpd      ','QR/Cpd              ','Fx                  ', &
                                            &    'Fy                  ','                    ', &
&  '                    ','(drdt*L/cpd)ls      ','(dEddt/cpd)ls       ','omega*u             ', &
                                            &    'omega*v             ','                    '/), &
& SHAPE=(/NVBUDGET_LAYER,NTERMMAXV/))

BUDGET_LAYER(1,1,1,1)       = 3
BUDGET_LAYER(2,1,1,1)       = 3
BUDGET_LAYER(3,1,1,1)       = 3
BUDGET_LAYER(4,1,1,1)       = 5
BUDGET_LAYER(5,1,1,1)       = 5
BUDGET_LAYER(6,1,1,1)       = 3

BUDGET_LAYER(1,2,1,2:NTU-1) = REAL(BOUNDARY(1,2:NTU-1), KIND=RK8)
BUDGET_LAYER(1,3,1,2:NTU-1) = REAL(BOUNDARY(2,2:NTU-1), KIND=RK8)
BUDGET_LAYER(1,4,1,2:NTU-1) = REAL(BOUNDARY(3,2:NTU-1), KIND=RK8)

!
! Write BUDGET_LAYER and BUDGET_COLUMN to a NetCDF file.
!
PRINT *,'Enter a file name to write BUDGET_LAYER and BUDGET_COLUMN to'
READ (FMT='(A512)', UNIT=5) OUTPUTFILE
CALL OPT_BUDGET_NETCDF(OUTPUTFILE, BUDGET_COLUMN, BUDGET_LAYER, VBUDGET_COLUMN, VBUDGET_LAYER, AVE_QS, AVE_SS, LEVELS, T) 

!
! For tidiness, deallocate the allocated arrays.
!
IF (ALLOCATED(BUDGET_COLUMN))       DEALLOCATE(BUDGET_COLUMN)
IF (ALLOCATED(BUDGET_LAYER))        DEALLOCATE(BUDGET_LAYER)
IF (ALLOCATED(AVE_QS))              DEALLOCATE(AVE_QS)
IF (ALLOCATED(AVE_SS))              DEALLOCATE(AVE_SS)
IF (ALLOCATED(DPSDT))               DEALLOCATE(DPSDT)
IF (ALLOCATED(VDPS))                DEALLOCATE(VDPS)
IF (ALLOCATED(T))                   DEALLOCATE(T)
IF (ALLOCATED(BAR_PRES))            DEALLOCATE(BAR_PRES)
IF (ALLOCATED(ADPS))                DEALLOCATE(ADPS)
IF (ALLOCATED(PRECIP))              DEALLOCATE(PRECIP)
IF (ALLOCATED(LPRECIP))             DEALLOCATE(LPRECIP)
IF (ALLOCATED(EVAPOR))              DEALLOCATE(EVAPOR)
IF (ALLOCATED(SHF))                 DEALLOCATE(SHF)
IF (ALLOCATED(RL))                  DEALLOCATE(RL)
IF (ALLOCATED(RADIATIONT))          DEALLOCATE(RADIATIONT)
IF (ALLOCATED(RADIATIONB))          DEALLOCATE(RADIATIONB)
IF (ALLOCATED(RADIATION))           DEALLOCATE(RADIATION)
IF (ALLOCATED(TAOX))                DEALLOCATE(TAOX)
IF (ALLOCATED(TAOY))                DEALLOCATE(TAOY)
IF (ALLOCATED(DS))                  DEALLOCATE(DS)
IF (ALLOCATED(BOUNDARY))            DEALLOCATE(BOUNDARY)
IF (ALLOCATED(LEVELS))              DEALLOCATE(LEVELS)

END PROGRAM BUDGET_PUT
