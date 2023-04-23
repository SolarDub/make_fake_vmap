PROGRAM make_fake_build_f90

  USE mod_main, ONLY: SP
  USE mod_params, ONLY: lmax, nx, nphi

  IMPLICIT NONE

  INTEGER :: ihour, nxby2 = nx/2

! Convection spectrum amplitudes (Radial, Poloidal, Toroidal)
  REAL(kind=SP), DIMENSION(lmax,3) :: amp

! Spherical harmonic coefficients: Radial, r; Poloidal, s; Toroidal,t
  COMPLEX(kind=SP), DIMENSION(nx,nx) :: r, s, t

! Velocity vector component maps
! Increase dimension by 4 to produce wrap-around border for interpolation
  REAL(kind=SP), DIMENSION(nphi+4,nx+4) :: u, v, w

! Initialize velocity vector components to zero
  u = 0.; v = 0.; w = 0.

! Read convection spectral coefficients
  CALL read_conv_spec_file(amp)

  DO ihour = 1, 1

!  Determine spherical harmonic coefficients
    CALL calc_conv_spec(ihour, amp, r, s, t)

! Calculate velocity components from spectral coefficients
    CALL calc_velocity_components(r, s, t, u, v, w)

! Write velocity components to file
    CALL write_output_file(ihour, 'uvel_1', u, 2*nx+4, nx+4)  ! tor
    CALL write_output_file(ihour, 'vvel_1', v, 2*nx+4, nx+4)  ! pol
    CALL write_output_file(ihour, 'wvel_1', w, 2*nx+4, nx+4)  ! rad

! Write velocity components at map-center to screen
    CALL write_results(u(nxby2,nxby2), v(nxby2,nxby2), w(nxby2,nxby2), nxby2)

  END DO

END PROGRAM
