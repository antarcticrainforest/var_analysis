! TIME module
!
! Time manipulation subroutines and functions.
!
! Tim Hume.
! 5 April 2007.

MODULE TIME
USE PORTABLE

CONTAINS

SUBROUTINE DAYS_TO_DATE(STYEAR, STMONTH,STDAY,DAYS, YYYY, MO, DD, HH, MM, SS)

IMPLICIT NONE

INTEGER (KIND=IK4), INTENT(IN)                          :: STYEAR       ! The year which the day count is relative to.
INTEGER (KIND=IK4), INTENT(IN)                          :: STMONTH      ! The month which the day count is relative to.
INTEGER (KIND=IK4), INTENT(IN)                          :: STDAY        ! The day which the day count is relative to.
REAL (KIND=RK8), DIMENSION(:), INTENT(IN)               :: DAYS         ! The time in days relative to STYEAR-STMONTH-STDAYT00:00:00
INTEGER (KIND=IK4), DIMENSION(:), INTENT(OUT), OPTIONAL :: YYYY         ! The year.
INTEGER (KIND=IK4), DIMENSION(:), INTENT(OUT), OPTIONAL :: MO           ! The month.
INTEGER (KIND=IK4), DIMENSION(:), INTENT(OUT), OPTIONAL :: DD           ! The day.
INTEGER (KIND=IK4), DIMENSION(:), INTENT(OUT), OPTIONAL :: HH           ! The hour.
INTEGER (KIND=IK4), DIMENSION(:), INTENT(OUT), OPTIONAL :: MM           ! The minute.
INTEGER (KIND=IK4), DIMENSION(:), INTENT(OUT), OPTIONAL :: SS           ! The second.

INTEGER (KIND=IK4)                                      :: NT           ! Number of times in the time arrays.
INTEGER (KIND=IK4), DIMENSION(12)                       :: MDAYS    = (/31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/)
INTEGER (KIND=IK4)                                      :: II           ! Counter.
REAL (KIND=RK8)                                         :: TDN
INTEGER (KIND=IK4)                                      :: TYY, TMO, TDD, THH, TMM, TSS, TTI

NT  = SIZE(DAYS)

DO II=1,NT
    TYY = STYEAR
    !
    ! We do a bit of rounding stuff here, based on what outputs the user wants.
    !
    IF (PRESENT(SS)) THEN
        TDN = INT((DAYS(II)+(STDAY))*86400 + 0.5)/86400.      ! Round to the nearest second.
    ELSE IF (PRESENT(MM)) THEN
        TDN = INT((DAYS(II)+(STDAY))*1440 + 0.5)/1440.        ! Round to the nearest minute.
    ELSE
        TDN = INT((DAYS(II)+(STDAY))*24 + 0.5)/24.            ! Round to the nearest hour.
    END IF

    !
    ! First calculate the date
    !
    TMO = STMONTH
    DO WHILE (TDN .GE. (MDAYS(TMO)+1))
        IF (LEAPYEAR(TYY)) THEN
            MDAYS(2) = 29
        ELSE
            MDAYS(2) = 28
        END IF
        DO WHILE ((TDN .GE. (MDAYS(TMO)+1)))
            TDN = TDN - MDAYS(TMO)
            TMO = TMO + 1
            if (tmo > 12) exit
        END DO
        if (TMO > 12) then
            TYY = TYY + 1
            TMO = 1
       endif
    END DO
    TDD = INT(TDN, KIND=IK4)
    !
    ! Now calculate the time, to the nearest second.
    !
    TTI = INT((TDN - TDD)*86400. + 0.5, KIND=IK4)     ! Round to the nearest second.
    THH = TTI/3600
    TMM = (TTI - THH*3600)/60
    TSS = TTI - THH*3600 - TMM*60

    IF (PRESENT(YYYY))  YYYY(II)    = TYY
    IF (PRESENT(MO))    MO(II)      = TMO
    IF (PRESENT(DD))    DD(II)      = TDD
    IF (PRESENT(HH))    HH(II)      = THH
    IF (PRESENT(MM))    MM(II)      = TMM
    IF (PRESENT(SS))    SS(II)      = TSS
END DO
    

END SUBROUTINE DAYS_TO_DATE

INTEGER (KIND=IK4) FUNCTION DATE_TIME_TO_UNIX (YYYY, MO, DD, HH, MM, SS)

INTEGER (KIND=IK4), INTENT(IN), OPTIONAL    :: YYYY                     ! The year.
INTEGER (KIND=IK4), INTENT(IN), OPTIONAL    :: MO                       ! The month.
INTEGER (KIND=IK4), INTENT(IN), OPTIONAL    :: DD                       ! The day.
INTEGER (KIND=IK4), INTENT(IN), OPTIONAL    :: HH                       ! The hour.
INTEGER (KIND=IK4), INTENT(IN), OPTIONAL    :: MM                       ! The minute.
INTEGER (KIND=IK4), INTENT(IN), OPTIONAL    :: SS                       ! The second.

INTEGER (KIND=IK4), DIMENSION(6)            :: DATETIME                 ! Parts of the date and time.
INTEGER (KIND=IK4)                          :: YYCNT                    ! Year counter.
INTEGER (KIND=IK4)                          :: MOCNT                    ! Month counter.
INTEGER (KIND=IK4), DIMENSION(12)           :: MDAYS = (/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /)
INTEGER (KIND=IK4)                          :: UTIME                    ! UNIX time.

!
! Set the date and time parts (and use default values if they are not specified by the user).
!
IF (PRESENT(YYYY)) THEN
    DATETIME(1) = YYYY
ELSE
    DATETIME(1) = 1970
ENDIF

IF (PRESENT(MO)) THEN
    DATETIME(2) = MO
ELSE
    DATETIME(2) = 1
ENDIF

IF (PRESENT(DD)) THEN
    DATETIME(3) = DD
ELSE
    DATETIME(3) = 1
ENDIF

IF (PRESENT(HH)) THEN
    DATETIME(4) = HH
ELSE
    DATETIME(4) = 0
ENDIF

IF (PRESENT(MM)) THEN
    DATETIME(5) = MM
ELSE
    DATETIME(5) = 0
ENDIF

IF (PRESENT(SS)) THEN
    DATETIME(6) = SS
ELSE
    DATETIME(6) = 0
ENDIF

!
! Check inputs are valid.
!
IF ((DATETIME(1) .LT. 1970) .OR. (DATETIME(1) .GT. 2037)) THEN
    PRINT *, 'W: Year is out of range (1970-2037)'
    DATE_TIME_TO_UNIX   = -1
    RETURN
ENDIF

IF ((DATETIME(2) .LT. 1) .OR. (DATETIME(2) .GT. 12)) THEN
    PRINT *, 'W: Month is out of range (1-12)'
    DATE_TIME_TO_UNIX   = -1
    RETURN
ENDIF

IF (LEAPYEAR(DATETIME(1))) THEN     ! We'll use these results later as well.
    MDAYS(2)    = 29
ELSE
    MDAYS(2)    = 28
ENDIF
IF ((DATETIME(3) .LT. 1) .OR. (DATETIME(3) .GT. MDAYS(DATETIME(2)))) THEN
    PRINT *,'W: Day is out of range (1-',MDAYS(DATETIME(2)),')'
    DATE_TIME_TO_UNIX   = -1
    RETURN
ENDIF

IF ((DATETIME(4) .LT. 0) .OR. (DATETIME(4) .GT. 23)) THEN
    PRINT *,'W: Hour is out of range (0-23)'
    DATE_TIME_TO_UNIX   = -1
    RETURN
ENDIF

IF ((DATETIME(5) .LT. 0) .OR. (DATETIME(4) .GT. 59)) THEN
    PRINT *,'W: Minute is out of range (0-59)'
    DATE_TIME_TO_UNIX   = -1
    RETURN
ENDIF

IF ((DATETIME(6) .LT. 0) .OR. (DATETIME(6) .GT. 59)) THEN
    PRINT *,'W: Second is out of range (0-59)'
    DATE_TIME_TO_UNIX   = -1
    RETURN
ENDIF

!
! Now compute the time.
!
YYCNT   = 1970
UTIME   = 0
DO WHILE (YYCNT .LT. DATETIME(1))
    IF (LEAPYEAR(YYCNT)) THEN
        UTIME   = UTIME + 31622400  ! The number of seconds in a leap year.
    ELSE
        UTIME   = UTIME + 31536000  ! The number of seconds in a normal year (not taking account of leap seconds).
    END IF
    YYCNT   = YYCNT + 1
END DO

MOCNT=1
DO WHILE (MOCNT .LT. DATETIME(2))
    UTIME   = UTIME + MDAYS(MOCNT)*86400    ! The number of seconds in the month.
    MOCNT   = MOCNT + 1
END DO

DATE_TIME_TO_UNIX   = UTIME + (DATETIME(3)-1)*86400 + DATETIME(4)*3600 + DATETIME(5)*60 + DATETIME(6)

END FUNCTION DATE_TIME_TO_UNIX

!***********************************************************************************************************************************
! LEAPYEAR
!
! This function simply tests if a year is a leap year or not.
!
! Tim Hume.
! 5 April 2007.
!***********************************************************************************************************************************
LOGICAL FUNCTION LEAPYEAR(YEAR)

IMPLICIT NONE
INTEGER (KIND=IK4), INTENT(IN)                          :: YEAR         ! The year being tested.

IF (MOD(YEAR,4) .EQ. 0) THEN
    IF ((MOD(YEAR,100) .EQ. 0) .AND. (MOD(YEAR,400) .NE. 0)) THEN
        LEAPYEAR    = .FALSE.
    ELSE
        LEAPYEAR    = .TRUE.
    END IF
ELSE
    LEAPYEAR        = .FALSE.
END IF

END FUNCTION LEAPYEAR

END MODULE TIME
