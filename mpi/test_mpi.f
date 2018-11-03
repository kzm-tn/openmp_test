      PROGRAM TEST_MPI

      IMPLICIT REAL*8 (A-H, O-Z)
      INCLUDE 'mpif.h'

      CALL MPI_Init(ierr)
      CALL MPI_Comm_size(MPI_COMM_WORLD, Nproc, ierr)
      CALL MPI_Comm_rank(MPI_COMM_WORLD, Nid, ierr)

      IF (Nid.EQ.0) WRITE(*,*) 'Nproc = ', Nproc
      WRITE(*,*) 'Hello world from processor ', Nid
      CALL MPI_FInalize(ierr)

      STOP
      END
