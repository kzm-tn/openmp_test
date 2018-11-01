program scope_test
  !$ use omp_lib
  integer :: ms = 100, mp=100
  !$ call omp_set_dynamic(.false.)
  !$ call omp_set_num_threads(2)
  write(*, *) 'serial region, mp, ms = ', mp, ms
  !$OMP parallel default(none) &
  !$OMP private(mp) shared(ms)
    !$ mp = omp_get_thread_num()
    !$ write(*, *) 'id, mp, ms = ', omp_get_thread_num(), mp, ms
  !$OMP end parallel
  write(*, *) 'serial region, mp, ms= ', mp, ms
end program scope_test
