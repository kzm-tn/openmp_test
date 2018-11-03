module var_mod
  integer m
end module var_mod

program wrong_scope
  use var_mod
  !$ use omp_lib
  m = 100
  write(*, *) 'm before p-region = ', m
  !$ call omp_set_dynamic(.false.)
  !$ call omp_set_num_threads(2)
  !$OMP parallel private(m)
    !$ m = omp_get_thread_num()
    !$ write(*, *) 'before subr : ', omp_get_thread_num(), m
    call mplus10
    !$ write(*, *), 'after subr : ', omp_get_thread_num(), m
  !$OMP end parallel
  write(*, *) 'm after p-region = ', m
end program wrong_scope

subroutine mplus10
  use var_mod
  m = m + 10
end subroutine mplus10
