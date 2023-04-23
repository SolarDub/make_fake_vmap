SUBROUTINE write_output_file(ifile, fpref, A, M, N)

  USE mod_main, ONLY : SP
  USE mod_params, ONLY : nx

  IMPLICIT NONE

  INTEGER :: ifile, M, N, i, j
  REAL(kind=SP), DIMENSION(M,N) :: A

  CHARACTER (LEN=6), INTENT(IN)  :: fpref        ! Output file name prefix
  CHARACTER (LEN=23) :: filename                 ! Output file name

  filename = create_filename(ifile, fpref)

  write(*,*) 'Writing file: ', filename

  open(unit=1,file=filename,access='direct', status='unknown',recl=(4*nx))
  do j=1,nx
    write(1,rec=j) (A(i,j),i=1,nx)
  enddo
  close(1)

  CONTAINS

    FUNCTION create_filename(ifile, fpref) RESULT(filename)

      USE mod_params, ONLY : opath, oext

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: ifile
      INTEGER :: n1000, n100, n10, n1

      CHARACTER (LEN=6),  INTENT(IN)   :: fpref     ! Output file name prefix
      CHARACTER (LEN=23) :: filename                ! Output file name

      n1000 = int(ifile/1000)
      n100  = int((ifile-1000*n1000)/100)
      n10   = int((ifile-1000*n1000-100*n100)/10)
      n1    = ifile-1000*n1000-100*n100-10*n10

      filename =  opath // fpref &
              // char(48+n100) // char(48+n10) //char(48+n1) &
              // oext

    END FUNCTION create_filename

END SUBROUTINE write_output_file
