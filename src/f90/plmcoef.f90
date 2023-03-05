!***********************************************************************
!                                                                      *
!  calculate coefficient matrix for generation of legendre polynomials *
!  of degree l and order m in subroutine plm.                          *
!                                                                      *
!***********************************************************************
SUBROUTINE plmcoef(coef)

  USE mod_main, ONLY : SP
  USE mod_params, ONLY : lmax, nx

  INTEGER :: l, m
  REAL(kind=SP), DIMENSION(nx,nx)  :: coef

  m = 0
  x1 = 0.5
  coef(m+1,m+1) = sqrt(x1)

! Generate plm over all l for m = 0
  DO l = m+1,lmax+1
  	coef(l+1,m+1) = sqrt(((2.*l+1.)/(l+m))*((2.*l-1.)/(l-m)))
	  coef(m+1,l+1) = sqrt(((2.*l+1.)/(l+m))*((l+m-1.)/(l-m))   &
       	          * ((l-m-1.)/(2.*l-3)))
  END DO

! Generate plm over all l for m > 0
  DO m = 1,lmax+1
  	x1 = x1 * (2.*m+1.)/(2.*m)
	  coef(m+1,m+1) = sqrt(x1)

	  DO l = m+1,lmax+1
	    coef(l+1,m+1) = sqrt(((2.*l+1.)/(l+m))*((2.*l-1.)/(l-m)))
	    coef(m+1,l+1) = sqrt(((2.*l+1.)/(l+m))*((l+m-1.)/(l-m))  &
     	              * ((l-m-1.)/(2.*l-3)))
	  END DO

  END DO

  m = lmax
  x1 = x1 * (2.*m+1.)/(2.*m)
  coef(m+1,m+1) = sqrt(x1)
  RETURN

END SUBROUTINE
