PROGRAM make_fake_build_f90

  USE mod_main, ONLY: SP
  USE mod_params, ONLY: lmax, nx, nphi

  IMPLICIT NONE

  INTEGER :: ihour, ifile, idum
  INTEGER :: l, l1, m, m1, m2, i, j

  REAL(kind=SP) :: hours
  REAL(kind=SP) :: el, em

! Convection spectrum amplitudes (Radial, Poloidal, Toroidal)
  REAL(kind=SP), DIMENSION(lmax,3) :: amp

! Spherical harmonic coefficients: Radial, r; Poloidal, s; Toroidal,t
  COMPLEX(kind=SP), DIMENSION(nx,nx) :: r, s, t

! Spherical Harmonic coefficients: coef; Associated Legendre Polynomials, p
  REAL(kind=SP)       :: coef(nx,nx), p(nx)

! Velocity vector component maps
! Increase dimension by 4 to produce wrap-around border for interpolation
  REAL(kind=SP), DIMENSION(nphi+4,nx+4) :: u, v, w

! Output file path/name parameters
  CHARACTER (LEN=6)   :: fpref              ! Output file name prefix

! Initialize velocity vector components to zero
  u = 0.; v = 0.; w = 0.

! Read convection spectral coefficients
  CALL read_conv_spec_file(amp)

  DO ihour = 1, 1

!  Prepare terms for new iteration
    CALL init_next_loop(ihour, ifile, idum)

!  Determine spherical harmonic coefficients
    CALL read_conv_spec(ihour, idum, amp, r, s, t)

! Calculate velocity components from spectral coefficients
    CALL calc_velocity_components(r, s, t, u, v, w)

! Write velocity components to file
    CALL write_output_file(ifile, 'uvel_1', u, 2*nx+4, nx+4)  ! tor
    CALL write_output_file(ifile, 'vvel_1', v, 2*nx+4, nx+4)  ! pol
    CALL write_output_file(ifile, 'wvel_1', w, 2*nx+4, nx+4)  ! rad

    WRITE(*,*) ''
    WRITE(*,*) 'At x = ', nx/2, ' y = ', nx/2
    WRITE(*,*) 'Toroidal velocity component, u = ', u(nx/2,nx/2)
    WRITE(*,*) 'Poloidal velocity component, v = ', v(nx/2,nx/2)
    WRITE(*,*) 'Radial velocity component, w = ', w(nx/2,nx/2)


  END DO

END PROGRAM
