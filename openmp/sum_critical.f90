program sum_critical
  !$ use omp_lib
  implicit none
  integer psum, sum, i
  sum = 0
  !$ call omp_set_dynamic(.false.)
  !$ call omp_set_num_threads(3)
  !$OMP parallel default(none) private(i, psum) shared(sum)
    psum = 0
    !$OMP do
    do i = 1, 100
      psum = psum + 1
    enddo
    !$ write(6. *) 'id, psum = ', omp_get_thread_num(), psum
    !$OMP critical
      sum = sum + psum
    !$OMP end critical
  !$OMP end critical
  write(6, *) 'sum = ', sum
end program sum_critical
