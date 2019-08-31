      program MAIN

      use STRUCT
      use PCG
      use solver_ICCG_mc

      implicit REAL*8 (A-H,O-Z)
      real(kind=8), dimension(:), allocatable :: WK

!C     
!C-- INIT.
!C      call INPUT
      call INPUT_COMMANDLINE
      call POINTER_INIT
      call BOUNDARY_CELL
      call CELL_METRICS
      call POI_GEN

!C 
!C-- MAIN SOLVER
      PHI=  0.d0

      ISET= 0

      allocate (WK(ICELTOT))

      Stime= omp_get_wtime()
!$acc data 
!$acc& copyin(BFORCE, D, indexL, itemL, indexU, itemU, AL, AU)
!$acc& copyout(PHI)
        call solve_ICCG_mc                                              &
     &      ( ICELTOT, NPL, NPU, indexL, itemL, indexU, itemU, D,       &
     &        BFORCE,  PHI, AL, AU, NCOLORtot, PEsmpTOT,                &
     &        SMPindex, SMPindexG, EPSICCG, ITR, IER)
      Etime= omp_get_wtime()
!$acc end data

      write (*,'(//, a, i10)') 'N= ', ICELTOT
      write (*,'(1pe16.6, a)') Etime-Stime, ' sec. (solver)'

      do ic0= 1, ICELTOT
        icel= NEWtoOLD(ic0)
        WK(icel)= PHI(ic0)
      enddo

      do icel= 1, ICELTOT
        PHI(icel)= WK(icel)
      enddo
!      call OUTUCD

      stop
      end

