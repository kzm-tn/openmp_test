subroutine mv(y, a, x, n)
  !$ use omp_lib
  integer n, i ,j
  real(8) x(n), y(n), a(n, n)
  !$OMP parallel default(none) shared(y, a, x, n) &
  !$OMP   private(i, j)
  !$OMP do
  do i = 1, n
    y(i) = 0.0d0
    do j = 1, n
      y(i) = y(i) + a(i, j) * x(j)
    enddo
  enddo
  !$OMP enddo
  !$OMP end parallel
end subroutine mv
