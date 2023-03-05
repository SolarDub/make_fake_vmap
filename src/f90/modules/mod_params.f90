MODULE mod_params

  USE mod_main

  IMPLICIT NONE

  !***************************
  ! Mathematical parameters
  !***************************

  COMPLEX(kind=SP), PARAMETER  :: xi = (0., 1.) ! sqrt(-1) = 0 + 1i

  REAL(kind=SP), PARAMETER     :: pi = 4.*atan(1.0)
  REAL(kind=SP), PARAMETER     :: root3 = sqrt(3.)
  REAL(kind=SP), PARAMETER     :: root5 = sqrt(5.)
  REAL(kind=SP), PARAMETER     :: root7 = sqrt(7.)
  REAL(kind=SP), PARAMETER     :: root9 = sqrt(9.)

  !***************************
  ! Image Map parameters
  !***************************

  INTEGER, PARAMETER           :: nx = 512     ! Number of poloidal pixels
  INTEGER, PARAMETER           :: nxhalf=nx/2   ! Number of pole-to-eqtr pixels
  INTEGER, PARAMETER           :: lmax=nx-2     ! Largest SH degree to compute
  INTEGER, PARAMETER           :: nphi=2*nx     ! Number of toroidal pixels
!  INTEGER, PARAMETER           :: nphi=nx     ! Number of toroidal pixels

  REAL(kind=SP), PARAMETER     :: dphi = 2.*pi/nphi ! Lng angle pixel-step
  REAL(kind=SP), PARAMETER     :: dtheta=pi/nx      ! Lat angle pixel-step

  !***********************************
  ! Solar Physical & image parameters
  !***********************************

  !  Differential rotation profile coefficients in radians per day
    REAL(kind=SP), PARAMETER     :: omega0 = (15.0-1.) * pi/(24.*180.)
    REAL(kind=SP), PARAMETER     :: omega2 = -9.0 * pi/(24.*180.)
    REAL(kind=SP), PARAMETER     :: omega4 = 4.0 * pi/(24.*180.)

  ! Solar Doppler image characteristics
    REAL(kind=SP), PARAMETER     :: x0=(nx/2)+0.5            ! x-center
    REAL(kind=SP), PARAMETER     :: y0=(nx/2)+0.5            ! y-center
    REAL(kind=SP), PARAMETER     :: radius = (nx/2)-10      ! image radius
    REAL(kind=SP), PARAMETER     :: rsq = radius*radius ! Squared-radius

  ! Polar tilt-angle
    REAL(kind=SP), PARAMETER     :: b0 = 0      ! Degrees (general: -1.62895)
    REAL(kind=SP), PARAMETER     :: br = b0 * pi/180.  ! Radians
    REAL(kind=SP), PARAMETER     :: cosb0 = cos(br)
  	REAL(kind=SP), PARAMETER     :: sinb0 = sin(br)

  ! Angular radius of sun viewed from a 1.0019 AU distance
    REAL(kind=SP), PARAMETER     :: s0 = 959.65/1.0019       ! Arcseconds
  	REAL(kind=SP), PARAMETER     :: sr = s0*pi/(180.*3600.)  ! Radians

  ! Scaled solar radius
    REAL(kind=SP), PARAMETER     :: radsol = radius*cos(sr)
  	REAL(kind=SP), PARAMETER     :: radsolsq = radsol*radsol
  	REAL(kind=SP), PARAMETER     :: scale = sr/radius  ! Radians per pixel

    !*****************
    ! File parameters
    !*****************

! Output
    CHARACTER (LEN=9), PARAMETER :: opath = "./output/" ! Output file path
    CHARACTER (LEN=5), PARAMETER :: oext = ".data"      ! Output file extension


END MODULE
