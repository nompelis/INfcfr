ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c *** A program driver to demonstrate the use of the Fortran wrapper of the
c *** Fluent case-file reader library. This code invokes the library to read
c *** a case file. Then, it makes cells by invoking a subroutine to construct
c *** each cell by returning the element's nodes in an array; the node ordering
c *** is specific to my own conventions outlined in my notes.
c ***
c *** Created:       IN <nompelis@nobelware.com> 20171006
c *** Last modified: IN <nompelis@nobelware.com> 20171016
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      Program main
      Implicit None
      Character*100 :: fname
      Integer(kind=8) :: nno,nel,nfa
      Integer(kind=8),allocatable,dimension(:,:) :: ifn,ife,izone
      Real*8,allocatable,dimension(:,:) :: x
      Integer(kind=4) :: ierr,nz,k
      Integer(kind=8) :: n,ifnt(0:4)


c--- read a Fluent case-file and transfer it to arrays
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


c--- call a subroutine to create cells
      call make_cells(nz,nno,nel,nfa,ifn,ife,izone,x)

c--- drop arrays
      deallocate( x, ifn, ife, izone )

      End

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c *** A subroutine to create cells from arrays adhering to a Fluent case file
c *** format. The routine simply loops over cells and invokes special cell
c *** construction functions but does nothing with the cells formed.
c *** NOTE: At this point in the development process I am testing for tets and
c *** I have a _hardwired_ tet-cell output testing code block.
c *** 
c *** Created:       IN <nompelis@nobelware.com> 20171006
c *** Last modified: IN <nompelis@nobelware.com> 20171016
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      Subroutine make_cells(nz,nno,nel,nfa,ifn,ife,izone,x)
      Use inMesh_Elements
      Implicit None
      Integer(kind=8) :: nno,nel,nfa
      Integer(kind=4) :: nz
      Integer(kind=8),dimension(0:4,nfa) :: ifn
      Integer(kind=8),dimension(2,nfa) :: ife
      Integer(kind=8),dimension(4,nz) :: izone
      Real*8,dimension(3,nno) :: x
      Integer(kind=4) :: ierr

      Integer(kind=8),allocatable,dimension(:,:) :: ief,ien
      Integer(kind=8) :: i,j,k
      Integer(kind=8) :: ntri,nqua
      Integer(kind=8) :: ieno(0:8),iefo(0:4)
      Logical, parameter :: itest = .TRUE.


      PRINT*,'Number of cells:',nel

      allocate(ief(0:6,nel), ien(0:8,nel), STAT=ierr)

      IF( itest ) THEN
c--- write a tecplot file with all cells; first put the nodes
c--- THIS IS FOR DEVELOPMENT TESTING PURPOSES
      open(unit=10,file='CELLS.dat',status='unknown',form='formatted')
      write(10,*) 'variables = x y z'
      write(10,*) 'zone T="Tetrahedra"'
      write(10,*) 'NODES=',nno,', ELEMENTS=',nel
      write(10,*) 'ET=TETRAHEDRON'
      write(10,*) 'F=FEPOINT'
      do i = 1,nno
         write(10,*) x(:,i)
      enddo
      ENDIF

c--- initialize (face-neighbours / nodes) arrays
      do i = 1,nel
         ief(:,i) = 0
         ien(:,i) = 0
      enddo

c--- form list of surfaces bounding each element
      do j = 1,nfa

         i = ife(1,j)
         if( i .gt. 0 ) then
            ief(0,i) = ief(0,i) + 1
            ief( ief(0,i) ,i) = j
         endif

         i = ife(2,j)
         if( i .gt. 0 ) then
            ief(0,i) = ief(0,i) + 1
            ief( ief(0,i) ,i) = j
         endif

      enddo
c     do i = 1,nel
c        write(*,100) ief(:,i)
c     enddo
c100  Format(7(1x,i6))


c--- form individual volume elements (cells) from surface elements
      do i = 1,nel
         ntri = 0
         nqua = 0

         do k = 1,ief(0,i)
            if( ifn(0, ief(k,i) ) .eq. 3 ) ntri = ntri + 1
            if( ifn(0, ief(k,i) ) .eq. 4 ) nqua = nqua + 1
         enddo
         if( ntri+nqua .lt. 4 .OR.  ntri+nqua .gt. 6 ) then
            PRINT*,'Problematic element!'
            PRINT*,'ntri=',ntri,'nqua=',nqua
            STOP
         endif

         if( ntri .eq. 4 .AND. nqua .eq. 0 ) then
            call inMesh_Elements_FormTetrahedronFromFaces(
     &                  nfa, ifn, ife, i, ief(0,i),
     &                  ieno, iefo, ierr )
         else if( ntri .eq. 4 .AND. nqua .eq. 1 ) then
            call inMesh_Elements_FormPyramidFromFaces(
     &                  nfa, ifn, ife, i, ief(0,i),
     &                  ieno, iefo, ierr )
         else if( ntri .eq. 3 .AND. nqua .eq. 2 ) then
            call inMesh_Elements_FormPrizmFromFaces(
     &                  nfa, ifn, ife, i, ief(0,i),
     &                  ieno, iefo, ierr )
         else if( ntri .eq. 0 .AND. nqua .eq. 6 ) then
            call inMesh_Elements_FormBrickFromFaces(
     &                  nfa, ifn, ife, i, ief(0,i),
     &                  ieno, iefo, ierr )
         else
            PRINT*,'Problematic element!'
            PRINT*,'ntri=',ntri,'nqua=',nqua
            STOP
         endif

      IF( itest ) THEN
c--- dump element node-connectivity to the file; this is not generic...
c--- THIS IS FOR DEVELOPMENT TESTING PURPOSES
         write(10,*) ieno(1:4)
      ENDIF

      enddo

      IF( itest ) THEN
c--- close the file; THIS IS FOR DEVELOPMENT TESTING PURPOSES
      close(10)
      ENDIF

      End subroutine

