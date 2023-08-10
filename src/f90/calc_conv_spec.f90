SUBROUTINE calc_conv_spec(ihour, amp, r, s, t)

  USE mod_main, ONLY : SP
  USE mod_params, ONLY : lmax, nx

  IMPLICIT NONE

  INTEGER, INTENT(IN) :: ihour
  INTEGER :: l, l1, m, m1, idummy

  REAL(kind=SP), DIMENSION(lmax,3), INTENT(IN) :: amp
  REAL(kind=SP) :: hours, el, phase

! Exponential part of polar complex number
  COMPLEX(kind=SP) :: getArg, arg

  COMPLEX(kind=SP), DIMENSION(nx,nx), INTENT(INOUT) :: r, s, t

! Initialize radial, poloidal and toroidal spectral coefficients to zero.
  r = (0., 0.); s = (0., 0.); t = (0., 0.)

  ! Initialize rotation and meridional flow spectral profiles
  CALL init_axisym_profiles(s,t)

  idummy=1
  CALL SRAND(idummy)

  !***********************************************************************
  !                                                                      *
  !  Calculate the convection spectrum.                                  *
  !                                                                      *
  !***********************************************************************
      write(*,*) 'Calculating convection spectrum.'
      DO l = 1,lmax

  	    l1 = l+1
  	    el = float(l)

  	    DO m = 1,l

          arg = (0., 0.)
  	      m1 = m+1
  !
  ! Add phase information for rotation (r & s use same arg value)
  !
          arg = getArg(ihour, l, m)

  	      r(l1,m1) = -1.*amp(l,1)*arg
  		    s(l1,m1) = amp(l,2)*arg

  !
  ! Use random phases for toroidal component (t uses different arg value)
  !
          arg = getArg(ihour, l, m)
  		    t(l1,m1) = amp(l,3)*arg

  	      END DO
  	    END DO

  	  WRITE(*,*) 'Velocity spectra calculated'

END SUBROUTINE calc_conv_spec

!*******************************************************
!                                                      *
!  Get exponential term of complex spectral amplitude  *
!                                                      *
!*******************************************************

COMPLEX FUNCTION getArg(ihour, l, m)

  USE mod_main, ONLY : SP
  USE mod_params, ONLY : pi, xi, omega0, omega2, omega4

  IMPLICIT NONE

  INTEGER, INTENT(IN) :: ihour, l, m
  REAL(kind=SP) :: hours, em, factor2, factor4, phase
  COMPLEX(kind=SP) :: argmt

  hours = float(ihour)

  em = float(m)/sqrt(l*(l+1.))
  factor2 = (1.-em*em)
  factor4 = (1.-em*em*em*em)

  phase = 2.*pi * rand()  ! random phase for spectral amps
  phase = phase + m*hours * (omega0 + omega2*factor2 + omega4*factor4)
  argmt = cos(phase) + xi*sin(phase)

END FUNCTION getArg
