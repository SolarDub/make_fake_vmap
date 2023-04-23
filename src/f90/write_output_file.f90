SUBROUTINE write_output_file(ihour, fpref, A, M, N)

  USE mod_main, ONLY : SP

  IMPLICIT NONE

  INTEGER :: ihour, M, N, i, j
  REAL(kind=SP), DIMENSION(M,N), INTENT(IN) :: A      ! Velocity map array

  CHARACTER (LEN=6), INTENT(IN)  :: fpref        ! Output file name prefix

  CALL write_to_file(create_filename(ihour, fpref), A)

  CONTAINS

    FUNCTION create_filename(ihour, fpref) RESULT(filename)

      USE mod_params, ONLY : opath, oext

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: ihour
      INTEGER :: ifile, n1000, n100, n10, n1

      CHARACTER (LEN=6),  INTENT(IN)   :: fpref     ! Output file name prefix
      CHARACTER (LEN=23) :: filename                ! Output file name

      ifile = 1000 + (ihour-1)*4

      n1000 = int(ifile/1000)
      n100  = int((ifile-1000*n1000)/100)
      n10   = int((ifile-1000*n1000-100*n100)/10)
      n1    = ifile-1000*n1000-100*n100-10*n10

      filename =  opath // fpref &
              // char(48+n100) // char(48+n10) //char(48+n1) &
              // oext

    END FUNCTION create_filename

    SUBROUTINE write_to_file(filename, A)

      USE mod_params, ONLY : nx

      IMPLICIT NONE

      REAL(kind=SP), DIMENSION(M,N), INTENT(IN) :: A     ! Velocity map array
      CHARACTER (LEN=23), INTENT(IN) :: filename         ! Output file name

      write(*,*) 'Writing file: ', filename

      open(unit=1,file=filename,access='direct', status='unknown',recl=(4*nx))
      do j=1,nx
        write(1,rec=j) (A(i,j),i=1,nx)
      enddo
      close(1)

    END SUBROUTINE write_to_file

END SUBROUTINE write_output_file
