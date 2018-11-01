        program hello_omp
!$ use omp_lib ! OpenMPのmoduleの使用宣言
C          implicit none
!$OMP parallel
          write(*,*) 'hello! N=', omp_get_num_threads(), " and I am", 
     &         omp_get_thread_num()
!$OMP end parallel 
        end program hello_omp
