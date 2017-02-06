!*******************************************************
!*    LU decomposition routines used by test_lu.f90    *
!*                                                     *
!*                 F90 version by J-P Moreau, Paris    *
!* --------------------------------------------------- *
!* Reference:                                          *
!*                                                     *
!* "Numerical Recipes by W.H. Press, B. P. Flannery,   *
!*  S.A. Teukolsky and W.T. Vetterling, Cambridge      *
!*  University Press, 1986".                           *
!*                                                     * 
!* This module has been tidied up by Tim Hume.
!*******************************************************
MODULE LU
USE PORTABLE

CONTAINS

!  ***************************************************************
!  * Given an N x N matrix A, this routine replaces it by the LU *
!  * decomposition of a rowwise permutation of itself. A and N   *
!  * are input. INDX is an output vector which records the row   *
!  * permutation effected by the partial pivoting; D is output   *
!  * as -1 or 1, depending on whether the number of row inter-   *
!  * changes was even or odd, respectively. This routine is used *
!  * in combination with LUBKSB to solve linear equations or to  *
!  * invert a matrix. Return code is 1, if matrix is singular.   *
!  ***************************************************************
SUBROUTINE LUDCMP(A,N,INDX,D,CODE)

IMPLICIT NONE
INTEGER (KIND=IK4), INTENT(IN)                  :: N        ! The size of the array.
REAL (KIND=RK8), DIMENSION(N,N), INTENT(INOUT)  :: A        ! Array to operate on.
INTEGER (KIND=IK4), DIMENSION(N), INTENT(OUT)   :: INDX
INTEGER (KIND=IK4), INTENT(OUT)                 :: D
INTEGER (KIND=IK4), INTENT(OUT)                 :: CODE

!
! Local variables.
!
INTEGER (KIND=IK4), PARAMETER                   :: NMAX=100
REAL (KIND=RK8), PARAMETER                      :: TINY=1.5E-16
REAL (KIND=RK8)                                 :: AMAX, DUM, TOTAL
REAL (KIND=RK8), DIMENSION(NMAX)                :: VV
INTEGER (KIND=IK4)                              :: IMAX
INTEGER (KIND=IK4)                              :: I, J, K                               ! Counters.

D=1; CODE=0; 
IMAX=1          ! Initialise IMAX so that the compiler does not complain that it is used when uninitialised (it never will be,
                ! but the compiler can't know that).

DO I=1,N
    AMAX=0.0
    DO J=1,N
        IF (ABS(A(I,J)).GT.AMAX) AMAX=ABS(A(I,J))
    END DO
    IF(AMAX.LT.TINY) THEN
        CODE = 1
        RETURN
    END IF
    VV(I) = 1.0 / AMAX
END DO

DO J=1,N
    DO I=1,J-1
        TOTAL = A(I,J)
        DO K=1,I-1
            TOTAL = TOTAL - A(I,K)*A(K,J) 
        END DO
        A(I,J) = TOTAL
    END DO
    AMAX = 0.0
    DO I=J,N
        TOTAL = A(I,J)
        DO K=1,J-1
            TOTAL = TOTAL - A(I,K)*A(K,J) 
        END DO
        A(I,J) = TOTAL
        DUM = VV(I)*ABS(TOTAL)
        IF(DUM.GE.AMAX) THEN
            IMAX = I
            AMAX = DUM
        END IF
    END DO

    IF(J.NE.IMAX) THEN
        DO K=1,N
            DUM = A(IMAX,K)
            A(IMAX,K) = A(J,K)
            A(J,K) = DUM
        END DO
        D = -D
        VV(IMAX) = VV(J)
    END IF

    INDX(J) = IMAX
    IF(ABS(A(J,J)) < TINY) A(J,J) = TINY

    IF(J.NE.N) THEN
        DUM = 1.0 / A(J,J)
        DO I=J+1,N
            A(I,J) = A(I,J)*DUM
        END DO
    END IF 
END DO

RETURN
END SUBROUTINE LUDCMP


!  ******************************************************************
!  * Solves the set of N linear equations A . X = B.  Here A is     *
!  * input, not as the matrix A but rather as its LU decomposition, *
!  * determined by the routine LUDCMP. INDX is input as the permuta-*
!  * tion vector returned by LUDCMP. B is input as the right-hand   *
!  * side vector B, and returns with the solution vector X. A, N and*
!  * INDX are not modified by this routine and can be used for suc- *
!  * cessive calls with different right-hand sides. This routine is *
!  * also efficient for plain matrix inversion.                     *
!  ******************************************************************
SUBROUTINE LUBKSB(A,N,INDX,B)

IMPLICIT NONE
INTEGER (KIND=IK4)                            :: N
REAL (KIND=RK8), DIMENSION(N,N), INTENT(IN)   :: A
REAL (KIND=RK8), DIMENSION(N), INTENT(INOUT)  :: B
INTEGER (KIND=IK4), DIMENSION(N), INTENT(IN)  :: INDX

!
! Local variables.
!
REAL (KIND=RK8)                               :: TOTAL
INTEGER (KIND=IK4)                            :: I, J, LL, II

II = 0

DO I=1,N
    LL = INDX(I)
    TOTAL = B(LL)
    B(LL) = B(I)
    IF(II.NE.0) THEN
        DO J=II,I-1
            TOTAL = TOTAL - A(I,J)*B(J)
        END DO
    ELSE IF(TOTAL .NE. 0.0) THEN
        II = I
    END IF
    B(I) = TOTAL
END DO

DO I=N,1,-1
    TOTAL = B(I)
    IF(I < N) THEN
        DO J=I+1,N
            TOTAL = TOTAL - A(I,J)*B(J)
        END DO
    END IF
    B(I) = TOTAL / A(I,I)
END DO

RETURN
END SUBROUTINE LUBKSB

END MODULE LU

! end of file lu.f90
