SUBROUTINE write_results(uc, vc, wc, nxby2)

  USE mod_main, ONLY : SP

  IMPLICIT NONE

  REAL(kind=SP) :: uc, vc, wc, nxby2

  WRITE(*,*) ''
  WRITE(*,*) 'At x = ', nxby2, ' y = ', nxby2
  WRITE(*,*) 'Toroidal velocity component, u = ', uc
  WRITE(*,*) 'Poloidal velocity component, v = ', vc
  WRITE(*,*) 'Radial velocity component, w = ', wc

END SUBROUTINE write_results
