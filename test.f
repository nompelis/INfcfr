      Program main
      Implicit None
      Character*100 :: fname
      Integer(kind=8) :: nno,nel,nfa
      Integer(kind=8),allocatable,dimension(:,:) :: ifn,ife,izone
      Real*8,allocatable,dimension(:,:) :: x
      Integer(kind=4) :: ierr,nz,k
      Integer(kind=8) :: n,ifnt(0:4)


      fname = 'grid.cas'
      call inFluent_FortranWrapperInit( trim( fname ), nz, nno, nel, nfa, ierr )
      PRINT*,'Result of Init() ierr=',ierr,' nz=',nz

      allocate( ifn(0:4,nfa), ife(2,nfa), izone(4,nz),
     &          x(3,nno), STAT=ierr )
      PRINT*,'Result of arrays allocation ierr=',ierr

      call inFluent_FortranWrapperFill( izone, ifn, ife, x, ierr )
      PRINT*,'Result of Fill() ierr=',ierr
      do n = 1,nz
         write(*,101) 'ZONE',n,'->',izone(:,n)
      enddo
 101  Format(a4,1x,i5,1x,a2,1x,4(1x,i9))

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

c--- write a tecplot file with all _faces_ (using "nfa" for no. of elements)
      open(unit=10,file='FACES.dat',status='unknown',form='formatted')
      write(10,*) 'variables = x y z'
      do k = 1,nz
      if( izone(4,k) .eq. 13 ) then

         write(10,*) 'zone T="face group ',k,'"'
         write(10,*) 'NODES=',nno,', ELEMENTS=',izone(3,k)
         write(10,*) 'ET=QUADRILATERAL'
         write(10,*) 'F=FEPOINT'
         do n = 1,nno
            write(10,*) x(:,n)
         enddo
         do n = izone(1,k),izone(2,k)
            ifnt(:) = ifn(:,n)
            if( ifnt(4) .le. 0 ) ifnt(4) = ifnt(3)
            write(10,100) ifnt(1:4)
         enddo

      endif
      enddo
      close(10)



      deallocate( x, ifn, ife, izone )

      End

