SUBROUTINE init_next_loop(ihour, ifile, idum)

  USE mod_main, ONLY : SP

  IMPLICIT NONE

  INTEGER, INTENT(IN) :: ihour
  INTEGER, INTENT(OUT) :: ifile, idum

  ifile = 1000 + (ihour-1)*4

  idum = -1

END SUBROUTINE init_next_loop
