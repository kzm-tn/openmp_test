program scope_test2
  !$ use omp_lib
  integer :: mp = 0, ms = 100
  !$ call omp_set_dynamic(.false.)
  !$ call omp_set_num_threads(3)
  !$OMP parallel private(mp) shared(ms)
    !$ call cal_mp(mp, ms)
    !$ write(*, *) 'id, mp = ', omp_get_thread_num(), mp
  !$OMP end parallel
end program scope_test2


subroutine cal_mp(mp, ms)
  !$ use omp_lib
  integer mp, ms  ! mp and ms have the same sharing attribution as main program.
  mp = (1 + omp_get_thread_num()) * ms
end subroutine cal_mp
