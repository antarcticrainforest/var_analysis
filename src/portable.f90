!*************************************************************************************************************************
! This module defines some stuff (kind types etc) for portability. This should ensure that the code will compile on
! compilers and platforms with different kind values.
!
! Tim Hume.
! 15 September 2006.
!*************************************************************************************************************************

MODULE PORTABLE

INTEGER, PARAMETER  :: IK1 = SELECTED_INT_KIND(2)       ! Probably a 1 or 2 byte integer.
INTEGER, PARAMETER  :: IK2 = SELECTED_INT_KIND(4)       ! Probably a 2 byte integer.
INTEGER, PARAMETER  :: IK4 = SELECTED_INT_KIND(9)       ! Probably a 4 byte integer.

INTEGER, PARAMETER  :: RK4 = SELECTED_REAL_KIND(4,30)   ! Probably a 4 byte real.
!INTEGER, PARAMETER  :: RK8 = SELECTED_REAL_KIND(4,30)   ! Probably a 4 byte real.   ! Testing sensitivity to REAL type.
INTEGER, PARAMETER  :: RK8 = SELECTED_REAL_KIND(10,200) ! Probably a 8 byte real.

END MODULE PORTABLE
