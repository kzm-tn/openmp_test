module parallel_mpi
  implicit none
  include 'mpif.h'
  integer :: myrank, nprocs

contains

  subroutine mpi_initialize
    implicit none
    integer :: ierr

    call mpi_init(ierr)
    call mpi_comm_rank(mpi_comm_world, myrank, ierr)
    call mpi_comm_size(mpi_comm_world, nprocs, ierr)

    if (myrank == 0) then
        write(*, *) "Num of Parallel calculations: ", nprocs
    end if

  end subroutine mpi_initialize

  subroutine mpi_finalize
    implicit none
    integer :: ierr

    call finalize(ierr)
  end subroutine mpi_finalize
end module parallel_mpi
