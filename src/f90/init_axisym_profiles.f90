SUBROUTINE init_axisym_profiles(s, t)

  USE mod_main, ONLY : SP
  USE mod_params, ONLY : nx

  IMPLICIT NONE

  INTEGER :: l, m = 0
  COMPLEX(kind=SP), DIMENSION(nx,nx) :: r, s, t

!***********************************************************************
!                                                                      *
!  Produce axisymmetric flow profiles (m = 0)                          *
!                                                                      *
!***********************************************************************

  write(*,*) 'calculating spectrum'
  m = 0

!***********************************************************************
!                                                                      *
!  Create the spectrum for the rotation profile (toroidal flow).       *
!                                                                      *
!***********************************************************************
  l = 1; t(l+1,m+1) = 1483.
  l = 3; t(l+1,m+1) = -26.
  l = 5; t(l+1,m+1) = -4.
!***********************************************************************
!                                                                      *
!  Create the spectrum for the meridional circulation (poloidal flow). *
!                                                                      *
!***********************************************************************
  l = 2; s(l+1,m+1) = 17.
  l = 4; s(l+1,m+1) = -4.

END SUBROUTINE init_axisym_profiles
