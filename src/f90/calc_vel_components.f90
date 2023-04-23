SUBROUTINE calc_velocity_components(r, s, t, u, v, w)

  USE mod_main, ONLY : SP
  USE mod_params, ONLY : xi, pi, nx, nxhalf, lmax, dtheta, nphi

  IMPLICIT NONE

  INTEGER :: i, j, ivc, ijh
  INTEGER :: l, l1, l2, m, m1, m2
  INTEGER :: fac, ieo
  INTEGER, DIMENSION(2) :: jh ! North(1) / South(2) latitude pixel coords
  REAL(kind=SP) :: theta, x, sintheta, rst, rstterm, em, hfac, v1, v2

! Spherical Harmonic coefficients: coef; Associated Legendre Polynomials, p
  REAL(kind=SP)       :: coef(nx,nx), p(nx)

! Spherical harmonic coefficients: Radial, r; Poloidal, s; Toroidal,t
  COMPLEX(kind=SP), DIMENSION(nx,nx) :: r, s, t

! Sums of northern (odd) and southern (even) hemispheric spectra coefficients
! for given order, m, over all degrees, l, at given latitude acos(x)
! 1, 2: Toroidal N/S; 4, 4: Poloidal N/S; 5, 6: Radial N/S;
  COMPLEX(kind=SP), DIMENSION(6) :: sum

! Northern and southern hemispheric velocity vector components:
! phi: 1, 2 (toroidal); theta: 3, 4 (poloidal); radius:5, 6 (radial)
! Odd: North; Even: South
  COMPLEX(kind=SP), DIMENSION(6,nphi) :: vc = (0., 0.)

! Velocity vector component maps
! Increase dimension by 4 to produce wrap-around border for interpolation
  REAL(kind=SP), DIMENSION(nphi+4,nx+4) :: u, v, w

!  Calculate Associated Legendre Polynomial coefficients
	CALL plmcoef(coef)
	WRITE(*,*) 'Legendre recurrance coefficients calculated.'

! Loop over hemispheric pixel latitudes (N & S are both covered within loop)
  DO j = 1,nxhalf

    ! Northern and southern pixel coordinates
    jh(1)       = nxhalf+j
    jh(2)       = nxhalf+1-j

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

! Reset cumulative complex velocity spectral amplitude array
      sum = (0.,0.)

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
        sum(1) = sum(1) - t(l1,m1)*v1 + xi*s(l1,m1)*v2
        sum(2) = sum(2) + ieo * (t(l1,m1)*v1 + xi*s(l1,m1)*v2)
        sum(3) = sum(3) + s(l1,m1)*v1 + xi*t(l1,m1)*v2
        sum(4) = sum(4) - ieo * (s(l1,m1)*v1 - xi*t(l1,m1)*v2)
        sum(5) = sum(5) + r(l1,m1)*p(l1)
        sum(6) = sum(6) + ieo * r(l1,m1)*p(l1)

      END DO

! Combine with numerical factors and 1/sin(lat) where necessary
! With the above sums, these commands represent H88 eqns 9-11:
! - velocity components at pixel latitude theta_j for harmonic order, m.

      DO ivc = 1, 6

        IF (ivc > 4) THEN
          rstterm = 1
        ELSE
          rstterm = rst
        END IF

        vc(ivc,m1) = hfac*sum(ivc)*rstterm
        IF (m > 0) vc(ivc,m2) = hfac*CONJG(sum(ivc))*rstterm

      END DO

    END DO

!***********************************************************************
!                                                                      *
!  Calculate the vector velocity components at all phi positions.      *
!                                                                      *
!***********************************************************************

! Initialize all N and S hemisphere complex velocity components to zero
    DO ivc = 1, 6
      vc(ivc,lmax+1:nphi-lmax+1) = 0.
    END DO

! Take FFT along latitudinal strip to determine complex velocities
    DO ivc = 1, 6
      call four1_sp(vc(ivc,:),nphi,-1)
    END DO

! Calculate velocity profiles along N,S latitudinal strips (1,2 = N,S)
! Take real part of complex velocities
    DO ijh = 1, 2

      u(1:nphi,jh(ijh)) = REAL(vc(ijh, 1:nphi))     ! toroidal: u(n,s)=vc(1,2)
      v(1:nphi,jh(ijh)) = REAL(vc(ijh+2, 1:nphi))   ! poloidal: v(n,s)=vc(3,4)
      w(1:nphi,jh(ijh)) = REAL(vc(ijh+4, 1:nphi))   ! radial:   w(n,s)=vc(5,6) 

    END DO

  END DO

END SUBROUTINE calc_velocity_components
