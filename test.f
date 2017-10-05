      Program main
      Implicit None
      Character*100 :: fname
      Integer(kind=8) :: nno,nel,nfa
      Integer(kind=8),allocatable,dimension(:,:) :: ifn,ife
      Integer(kind=4) :: ierr
      Real*8,allocatable,dimension(:,:) :: x
      Integer(kind=8) :: n,ifnt(0:4)


      fname = 'grid.cas'
      call inFluent_FortranWrapperInit( trim( fname ), nno, nel, nfa, ierr )
      PRINT*,'Result of Init() ierr=',ierr

      allocate( ifn(0:4,nfa), ife(2,nfa),
     &          x(3,nno), STAT=ierr )
      PRINT*,'Result of arrays allocation ierr=',ierr

      call inFluent_FortranWrapperFill( ifn, ife, x, ierr )
      PRINT*,'Result of Fill() ierr=',ierr

      call inFluent_FortranWrapperTerm( ierr )
      PRINT*,'Result of Term() ierr=',ierr

c--- write a tecplot file with all _faces_ (using "nfa" for no. of elements)
      open(unit=10,file='ALL_FACES.dat',status='unknown',form='formatted')
      write(10,*) 'variables = x y z'
      write(10,*) 'zone T="faces"'
      write(10,*) 'NODES=',nno,', ELEMENTS=',nfa
      write(10,*) 'ET=QUADRILATERAL'
      write(10,*) 'F=FEPOINT'
      do n = 1,nno
         write(10,*) x(:,n)
      enddo
      do n = 1,nfa
         ifnt(:) = ifn(:,n)
         if( ifnt(4) .le. 0 ) ifnt(4) = ifnt(3)
         write(10,100) ifnt(1:4)
      enddo
 100  Format(4(1x,i10))
      close(10)

      deallocate( x, ifn, ife )

      End

