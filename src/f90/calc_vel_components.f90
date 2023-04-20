SUBROUTINE calc_velocity_components(r, s, t, u, v, w)

  USE mod_main, ONLY : SP
  USE mod_params, ONLY : xi, pi, nx, nxhalf, lmax, dtheta, nphi

  IMPLICIT NONE

  INTEGER :: i, j, jn, js
  INTEGER :: l, l1, l2, m, m1, m2
  INTEGER :: fac, ieo
  REAL(kind=SP) :: theta, x, sintheta, rst, em, hfac, v1, v2

! Spherical Harmonic coefficients: coef; Associated Legendre Polynomials, p
  REAL(kind=SP)       :: coef(nx,nx), p(nx)

! Spherical harmonic coefficients: Radial, r; Poloidal, s; Toroidal,t
  COMPLEX(kind=SP), DIMENSION(nx,nx) :: r, s, t

! Sums of northern (odd) and southern (even) hemispheric spectra coefficients
! for given order, m, over all degrees, l, at given latitude acos(x)
  COMPLEX(kind=SP)    :: sum1, sum2, sum3, sum4, sum5, sum6

! Northern and southern hemispheric velocity vector components:
! phi, u (toroidal); theta, v (poloidal); radius, w (radial)

  COMPLEX(kind=SP), DIMENSION(nphi) :: unorth, usouth
  COMPLEX(kind=SP), DIMENSION(nphi) :: vnorth, vsouth
  COMPLEX(kind=SP), DIMENSION(nphi) :: wnorth, wsouth

! Velocity vector component maps
! Increase dimension by 4 to produce wrap-around border for interpolation
  REAL(kind=SP), DIMENSION(nphi+4,nx+4) :: u, v, w

! Initialize complex spectralcoefficients in both hemispheres to zero
  unorth = (0., 0.); usouth = (0., 0.)
  vnorth = (0., 0.); vsouth = (0., 0.)
  wnorth = (0., 0.); wsouth = (0., 0.)

!  Calculate Associated Legendre Polynomial coefficients
	CALL plmcoef(coef)
	WRITE(*,*) 'Legendre recurrance coefficients calculated.'

! Loop over hemispheric pixel latitudes (N & S are both covered within loop)
  DO j = 1,nxhalf

    ! Northern and southern pixel coordinates
    jn       = nxhalf+j
    js       = nxhalf+1-j

    theta    = 0.5*pi - (j-0.5)*dtheta
    x        = COS(theta)
    sintheta = SIN(theta)
    rst      = 1.0/sintheta

    write(*,*) ' line ',j,' of ',nxhalf,' started at ', 180.*theta/pi &
             , ' degrees latitude.'

!***********************************************************************
!                                                                      *
!  Calculate the spectral coefficients for wavenumber m at all x.      *
!                                                                      *
!  For m > 1:                                                          *
!  Split non-axisymmetric signal into equal positive and               *
!  negative frequencies.                                               *
!                                                                      *
!***********************************************************************
! Loop over spherical harmonic order, m
    DO m = 0,lmax-1

      m1 = m+1
      m2 = nphi+1-m
      em = FLOAT(m)

! Produce assoc. Legendre polynomials for current latitude
      CALL plm(m,x,coef,p)

! Reset complex velocity spectral amplitude arrays
      sum1 = (0.,0.)  ! Toroidal, North
      sum2 = (0.,0.)  ! Toroidal, South
      sum3 = (0.,0.)  ! Poloidal, North
      sum4 = (0.,0.)  ! Poloidal, South
      sum5 = (0.,0.)  ! Radial, North
      sum6 = (0.,0.)  ! Radial, South

      fac  = CEILING(em/(em+1))    ! m = 0, fac = 0; fac = 1 otherwise
      hfac = 1. - 0.5*fac          ! m = 0, hfac = 1.0; hfac = 0.5 otherwise

! Loop over spherical harmonic order, l
      DO l = m,lmax-1

        l1   = l+1
        l2   = l+2
        ieo  = 1-2*mod(l-m,2)
        v1   = l*p(l2)/coef(l2,m1) - (l+1.)*p(l)/coef(l1,m1)
        v2   = -m*p(l1)*fac

! Sum toroidal, poloidal, radial velocity amplitudes
        sum1 = sum1 - t(l1,m1)*v1 + xi*s(l1,m1)*v2
        sum2 = sum2 + ieo * (t(l1,m1)*v1 + xi*s(l1,m1)*v2)
        sum3 = sum3 + s(l1,m1)*v1 + xi*t(l1,m1)*v2
        sum4 = sum4 - ieo * (s(l1,m1)*v1 - xi*t(l1,m1)*v2)
        sum5 = sum5 + r(l1,m1)*p(l1)
        sum6 = sum6 + ieo * r(l1,m1)*p(l1)

      END DO

! Combine with numerical factors and 1/sin(lat) where necessary
! With the above sums, these commands represent H88 eqns 9-11:
! - velocity components at pixel latitude theta_j for harmonic order, m.
      unorth(m1) = hfac*sum1*rst
      usouth(m1) = hfac*sum2*rst
      vnorth(m1) = hfac*sum3*rst
      vsouth(m1) = hfac*sum4*rst
      wnorth(m1) = hfac*sum5
      wsouth(m1) = hfac*sum6

      IF (m > 0) THEN

        unorth(m2) = hfac*CONJG(sum1)*rst
        usouth(m2) = hfac*CONJG(sum2)*rst
        vnorth(m2) = hfac*CONJG(sum3)*rst
        vsouth(m2) = hfac*CONJG(sum4)*rst
        wnorth(m2) = hfac*CONJG(sum5)
        wsouth(m2) = hfac*CONJG(sum6)

      END IF

    END DO

!***********************************************************************
!                                                                      *
!  Calculate the vector velocity components at all phi positions.      *
!                                                                      *
!***********************************************************************

! Initialize all N and S hemisphere complex velocity components to zero
    unorth(lmax+1:nphi-lmax+1) = 0.
    usouth(lmax+1:nphi-lmax+1) = 0.
    vnorth(lmax+1:nphi-lmax+1) = 0.
    vsouth(lmax+1:nphi-lmax+1) = 0.
    wnorth(lmax+1:nphi-lmax+1) = 0.
    wsouth(lmax+1:nphi-lmax+1) = 0.

! Take FFT along latitudinal strip to determine complex velocities
    call four1_sp(unorth,nphi,-1)
    call four1_sp(usouth,nphi,-1)
    call four1_sp(vnorth,nphi,-1)
    call four1_sp(vsouth,nphi,-1)
    call four1_sp(wnorth,nphi,-1)
    call four1_sp(wsouth,nphi,-1)

! Calculate velocity profile along latitudinal strip
! Take real part of complex velocities
    u(1:nphi,jn) = REAL(unorth(1:nphi))
    u(1:nphi,js) = REAL(usouth(1:nphi))
    v(1:nphi,jn) = REAL(vnorth(1:nphi))
    v(1:nphi,js) = REAL(vsouth(1:nphi))
    w(1:nphi,jn) = REAL(wnorth(1:nphi))
    w(1:nphi,js) = REAL(wsouth(1:nphi))

  END DO

END SUBROUTINE calc_velocity_components
