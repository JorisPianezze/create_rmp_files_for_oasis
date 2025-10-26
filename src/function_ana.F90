! #########################################################
SUBROUTINE function_ana(ni, nj, xcoor, ycoor, fnc_ana)
! #########################################################

! ---------------------------------------------------------
!
!   Compute analytical function
!
! ---------------------------------------------------------

IMPLICIT NONE

INTEGER,                INTENT(IN   ) :: ni, nj
REAL, DIMENSION(ni,nj), INTENT(IN   ) :: xcoor, ycoor
REAL, DIMENSION(ni,nj), INTENT(  OUT) :: fnc_ana

INTEGER         :: i,j
REAL, PARAMETER :: dp_pi=3.14159265359
REAL, PARAMETER :: dp_conv = dp_pi/180.
REAL            :: dp_length, coef, coefmult

DO j=1,nj
  DO i=1,ni
  
    dp_length = 1.2*dp_pi
    coef = 2.
    coefmult = 1.
    fnc_ana(i,j) = coefmult*(coef - COS( dp_pi*(ACOS( COS(xcoor(i,j)*dp_conv)*COS(ycoor(i,j)*dp_conv) )/dp_length)) )
    
  ENDDO
ENDDO

! #########################################################
END SUBROUTINE function_ana
! #########################################################
