SUBROUTINE read_conv_spec_file(amp)

  USE mod_main, ONLY : SP
  USE mod_params, ONLY : lmax

  IMPLICIT NONE

  INTEGER :: l

  REAL(kind=SP), DIMENSION(lmax,3) :: amp

  !***********************************************************************
  !                                                                      *
  !  Read in the convection spectrum.                                    *
  !  1: radial, 2; poloidal, 3: toroidal                                 *
  !                                                                      *
  !***********************************************************************
      OPEN(unit=7,file='fakespec.txt',status='old')

        DO l = 1,lmax

    	    READ(7,*) amp(l,1), amp(l,2), amp(l,3)
    	    IF (l .lt. 20) WRITE(*,*) amp(l,1), amp(l,2), amp(l,3)

  	    END DO

  	  CLOSE(7)

  	  WRITE(*,*) 'Velocity spectrum read from file'

END SUBROUTINE read_conv_spec_file
