!***********************************************************************
!                                                                      *
!  This subroutine calculates the associated legendre polynomials      *
!  using the recurrance relation for increasing degree l.              *
!  The coefficients for the relation are calculated once in            *
!  S/R PLMCOEF and passed as arguement coef.                           *
!  The results, p(l,m,x), are returned in array p(l+1).                *
!                                                                      *
!***********************************************************************
SUBROUTINE plm(m,x,coef,p)

  USE mod_main, ONLY : SP
  USE mod_params, ONLY : nx

  REAL(kind=SP), DIMENSION(nx) :: p
  REAL(kind=SP), DIMENSION(nx,nx) :: coef
!***********************************************************************
!                                                                      *
!  lls is the index to the first non-zero p.                           *
!   mm .le. lls .le. lmaxp+1                                           *
!   If all p's are zero; i.e., .lt. 10**minlp, then lls=lmaxp+1        *
!                                                                      *
!***********************************************************************
  INTEGER :: l, ll, lls, mm, lp, lp1, minlp
  REAL(kind=SP) :: x2, alp, pa, pm1, pm2
  REAL(kind=SP) :: rlpa(20)
  REAL(kind=SP) :: r10p10, r10m10

  DATA    r10p10,r10m10/1.0e+10,1.0e-10/
  DATA    minlp/-20/
  DATA    rlpa/1.0e-1,  1.0e-2,  1.0e-3,  1.0e-4,  1.0e-5, &
        		   1.0e-6,  1.0e-7,  1.0e-8,  1.0e-9,  1.0e-10, &
        		   1.0e-11, 1.0e-12, 1.0e-13, 1.0e-14, 1.0e-15, &
        		   1.0e-16, 1.0e-17, 1.0e-18, 1.0e-19, 1.0e-20/

  mm    = m+1
  lls   = mm
  lmaxp = nx
  lmax  = lmaxp-1

! Initialize all plm values to zero
  p(1:lmaxp) = 0.

  IF (abs(x) .gt. 1.) THEN
   	WRITE(*,*) 'Legendre polynomial argument out of range.'
    RETURN
  ENDIF

  IF(mm .gt. lmaxp) THEN
    WRITE(*,*) 'Order of Legendre polynomial out of range.'
    RETURN
  ENDIF

  IF (x .eq. 1. .and. m .eq. 0) THEN
    DO l = 0,lmax
      p(l+1) = sqrt(l+0.5)
    ENDDO
    RETURN
  ENDIF

  IF (x .eq. -1. .and. m .eq. 0) THEN
  	DO l = 0,lmax
      isgn   = 1-2*mod(l,2)
      p(l+1) = isgn*sqrt(l+0.5)
    ENDDO
	  RETURN
  ENDIF

  IF (abs(x) .eq. 1.) RETURN

  x2  = sqrt(1.-x*x)
  alp = alog10(coef(mm,mm)) + float(m)*alog10(x2)
  lp  = alp

  IF (lp .lt. minlp) THEN
    alp = alp-float(lp)
    pa  = 10.0**alp
    lls = mm+1
  ELSE
    pa    = coef(mm,mm)*(x2**m)
    p(mm) = pa
  ENDIF

  pm2 = 0.0
  pm1 = pa

  IF (lp .lt. minlp) THEN
    DO ll = mm+1,lmaxp
      pa  = coef(ll,mm)*x*pm1 - coef(mm,ll)*pm2
      pm2 = pm1
      pm1 = pa
      IF (pa .gt. r10p10) THEN
        pm2 = pm2*r10m10
        pm1 = pm1*r10m10
        lp  = lp+10
        lp1 = lp
        IF (lp1 .ge. minlp) THEN
          pm2 = pm2*rlpa(-lp1)
          pm1 = pm1*rlpa(-lp1)
          lp  = 0
          EXIT
        ENDIF
      ENDIF
      lls = ll+1
    ENDDO
  ENDIF

  IF (lls .ge. lmaxp) RETURN

  DO ll = lls+1,lmaxp
    pa    = coef(ll,mm)*x*pm1 - coef(mm,ll)*pm2
    pm2   = pm1
    pm1   = pa
    p(ll) = pm1
  ENDDO

  RETURN

END SUBROUTINE
