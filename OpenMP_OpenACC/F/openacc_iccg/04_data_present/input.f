!C
!C***
!C*** INPUT
!C***
!C
!C    INPUT CONTROL DATA
!C
      subroutine INPUT
      use STRUCT
      use PCG

      implicit REAL*8 (A-H,O-Z)

      character*80 CNTFIL

!C
!C-- CNTL. file
      open  (11, file='INPUT.DAT', status='unknown')
        read (11,*) NX, NY, NZ
        read (11,*) DX, DY, DZ
        read (11,*) EPSICCG
        read (11,*) PEsmpTOT
        read (11,*) NCOLORtot
        write (*,'(//a,i8)') '### THREAD number=', PEsmpTOT
      close (11)
!C===
      return
      end


      subroutine INPUT_COMMANDLINE
      
      use STRUCT
      use PCG

      implicit none
      character*80 CNTFIL
      integer :: i, length1, status1, length2, status2
      character(:), allocatable :: arg1,arg2
      character*100 :: arg
      intrinsic :: command_argument_count, get_command_argument
      
!C
!C-- CNTL. file
      open  (11, file='INPUT.DAT', status='unknown')
        read (11,*) NX, NY, NZ
        read (11,*) DX, DY, DZ
        read (11,*) EPSICCG
        read (11,*) PEsmpTOT
        read (11,*) NCOLORtot
      close (11)
!C===

      i = 1
      do while (i <= command_argument_count()) 
         call get_command_argument(i  ,length=length1,status=status1)
         call get_command_argument(i+1,length=length2,status=status2)
         if (status1 == 0 .and. status2 == 0) then

            allocate (character(length1) :: arg1)
            allocate (character(length2) :: arg2)
            call get_command_argument(i  , arg1, status = status1)
            call get_command_argument(i+1, arg2, status = status2)
            if (status1 == 0 .and. status2 == 0) then

               if(arg1=="-nx" .or. arg1=="-NX") then 
                  read (arg2,*) NX
               end if
               if(arg1=="-ny" .or. arg1=="-NY") then 
                  read (arg2,*) NY
               end if
               if(arg1=="-nz" .or. arg1=="-NZ") then 
                  read (arg2,*) NZ
               end if
               if(arg1=="-n" .or. arg1=="-N") then 
                  read (arg2,*) NX
                  NY = NX
                  NZ = NX
               end if
               if(arg1=="-dx" .or. arg1=="-DX") then 
                  read (arg2,*) DX
               end if
               if(arg1=="-dy" .or. arg1=="-DY") then 
                  read (arg2,*) DY
               end if
               if(arg1=="-dz" .or. arg1=="-DZ") then 
                  read (arg2,*) DZ
               end if
               if(arg1=="-e" .or. arg1=="-E" 
     &              .or. arg1=="-eps" .or. arg1=="-EPS" 
     &              .or. arg1=="-epsiccg" .or. arg1=="-EPSICCG" 
     &              .or. arg1=="-err" .or. arg1=="-ERR" ) then 
                  read (arg2,*) EPSICCG
               end if
               if(arg1=="-PEsmpTOT" .or. arg1=="-nt") then 
                  read (arg2,*) PEsmpTOT
               end if
               if(arg1=="-NCOLORtot" .or. arg1=="-ncolor" 
     &              .or. arg1=="-nc" .or. arg1=="-c" 
     &              .or. arg1=="-color" .or. arg1=="-COLOR" 
     &              .or. arg1=="-NCOLOR" .or. arg1=="-NC" ) then 
                  read (arg2,*) NCOLORtot
               end if

            end if
            deallocate (arg1)
            deallocate (arg2)
         else 
            print *, 'Error', status1, 'on argument', i
            print *, 'Error', status2, 'on argument', i+1
         end if
         i = i+2
      end do

      write (*,'(//a,i8)') '### THREAD number=', PEsmpTOT

      return

      end
