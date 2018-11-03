program sum_reduction
  !$ use omp_lib
  implicit none
  integer i, sum
  sum = 0
  !$ call omp_set_dynamic(.false.)
  !$ call omp_set_num_threads(3)
  !$OMP parallel
    !$OMP do reduction(+:sum)
    do i = 1, 100
      sum = sum + i
    enddo
    !$OMP enddo
  !$OMP end parallel
  write(6, *) 'sum = ', sum
end program sum_reduction
