program chk_clause
  !$ use omp_lib
  integer :: flag = 1
  !$OMP parallel if (flag == 0) num_threads(2)
    !$ write(*, *) 'p-region-1, id = ', omp_get_thread_num()
  !$OMP end parallel
  !$OMP parallel if (flag == 1) num_threads(2)
    !$ write(*, *) 'p-region-2, id= ', omp_get_thread_num()
  !$OMP end parallel
end program chk_clause
