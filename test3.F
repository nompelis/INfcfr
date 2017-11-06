ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c *** A program driver to demonstrate the use of the Fortran wrapper of the
c *** Fluent case-file reader library. This code invokes the library to read
c *** a case file. Then, it makes cells by invoking a subroutine to construct
c *** each cell by returning the element's nodes in an array; the node ordering
c *** is specific to my own conventions outlined in my notes.
c *** This code is intended to be used for testing scalar field gradient
c *** construction using different methods. It is based off the "demo1" code.
c ***
c *** Created:       IN <nompelis@nobelware.com> 20171006
c *** Last modified: IN <nompelis@nobelware.com> 20171027
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      Program main
      Implicit None
      Character*100 :: fname
      Integer(kind=8) :: nno,nel,nfa,nge,net
      Integer(kind=8),allocatable,dimension(:,:) :: ifn,ife,izone
      Integer(kind=8),allocatable,dimension(:,:) :: ief,ien
      Real*8,allocatable,dimension(:,:) :: x,xce,xcf,dq
      Real*8,allocatable,dimension(:) :: q
      Integer(kind=4) :: ierr,nz,k
      Integer(kind=8) :: n,ifnt(0:4),i,j
      Logical, parameter :: itest = .FALSE.


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

c--- count ghost cells
      nge = 0
      do j = 1,nfa
         if( ife(1,j) .le. 0 ) nge = nge + 1
         if( ife(2,j) .le. 0 ) nge = nge + 1
      enddo
      net = nel + nge
      PRINT*,'Number of ghost cells:',nge
      PRINT*,'Number of total cells:',net

c--- create arrays to hold cell & face data
      allocate( ief(0:6,nel),ien(0:8,nel), STAT=ierr)
      allocate( xce(3,net),xcf(3,nfa),q(net),dq(3,net), STAT=ierr)

c--- call a subroutine to create cells
      call make_cells(nz,nno,nel,nfa,ifn,ife,izone,x,ief,ien)

      IF( itest ) THEN
c--- write a tecplot file with all cells; first put the nodes
c--- THIS IS FOR DEVELOPMENT TESTING PURPOSES
      open(unit=10,file='CELLS.dat',status='unknown',form='formatted')
      write(10,*) 'variables = x y z'
      write(10,*) 'zone T="Tetrahedra"'
      write(10,*) 'NODES=',nno,', ELEMENTS=',nel
      write(10,*) 'ET=BRICK'
      write(10,*) 'F=FEPOINT'
      do n = 1,nno
         write(10,*) x(:,n)
      enddo
      do i = 1,nel
         write(10,*) ien(1:8,i)
      enddo
      close(10)
      ENDIF

c--- call a subroutine to create additional geometric data
      call make_geometry(nz,nno,nel,nge,net,nfa,
     &                   ifn,ife,izone,x,ief,ien,xce,xcf)





c--- drop arrays
      deallocate( x, ifn, ife, izone, ief, ien, xce, xcf, q, dq )

      End


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c *** A subroutine to create cells from arrays adhering to a Fluent case file
c *** format. The routine simply loops over cells and invokes special cell
c *** construction functions. It fills arrays that are returned to the calling
c *** function/routine.
c ***
c *** Created:       IN <nompelis@nobelware.com> 20171027
c *** Last modified: IN <nompelis@nobelware.com> 20171106
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      Subroutine make_geometry(nz,nno,nel,nge,net,
     &                         nfa,ifn,ife,izone,x,ief,ien,xce,xcf)
      Use inMesh_Elements
      Implicit None
      Integer(kind=8),intent(IN) :: nno,nel,nfa,nge,net
      Integer(kind=4),intent(IN) :: nz
      Integer(kind=8),intent(IN) :: ifn(0:4,nfa)
      Integer(kind=8),intent(INOUT) :: ife(2,nfa)
      Integer(kind=8),intent(IN) :: izone(4,nz)
      Integer(kind=8),intent(IN) :: ief(0:6,nel)
      Integer(kind=8),intent(IN) :: ien(0:8,nel)
      Real*8,intent(IN) :: x(3,nno)
      Real*8,intent(OUT) :: xce(3,net),xcf(3,nfa)
      Integer(kind=8) :: ntri,nqua
      Integer(kind=4) :: ierr

      Integer(kind=8) :: i,j,k,ii
      Real*8 :: dx1(3),dx2(3), xn(3),xl
      Logical, parameter :: itest = .TRUE.


      PRINT*,'Creating geometry'
      xce(:,:) = 0.0d0
      xcf(:,:) = 0.0d0


c--- form element centroids (all subjectivity in construction is in inMesh API)
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
            call inMesh_Tetrahedron_CentroidFromVertices(
     &                x(1, ien(1,i) ),
     &                x(1, ien(2,i) ),
     &                x(1, ien(3,i) ),
     &                x(1, ien(4,i) ), xce(1,i) )
         else if( ntri .eq. 4 .AND. nqua .eq. 1 ) then
            call inMesh_Pyramid_CentroidFromVertices(
     &                x(1, ien(1,i) ),
     &                x(1, ien(2,i) ),
     &                x(1, ien(3,i) ),
     &                x(1, ien(4,i) ),
     &                x(1, ien(5,i) ), xce(1,i) )
         else if( ntri .eq. 3 .AND. nqua .eq. 2 ) then
            call inMesh_Wedge_CentroidFromVertices(
     &                x(1, ien(1,i) ),
     &                x(1, ien(2,i) ),
     &                x(1, ien(3,i) ),
     &                x(1, ien(4,i) ),
     &                x(1, ien(5,i) ),
     &                x(1, ien(6,i) ), xce(1,i) )
         else if( ntri .eq. 0 .AND. nqua .eq. 6 ) then
            call inMesh_Brick_CentroidFromVertices(
     &                x(1, ien(1,i) ),
     &                x(1, ien(2,i) ),
     &                x(1, ien(3,i) ),
     &                x(1, ien(4,i) ),
     &                x(1, ien(5,i) ),
     &                x(1, ien(6,i) ),
     &                x(1, ien(7,i) ),
     &                x(1, ien(8,i) ), xce(1,i) )
         else
            PRINT*,'Problematic element!'
            PRINT*,'ntri=',ntri,'nqua=',nqua
            STOP
         endif
#ifdef _DEBUG2_
         PRINT*,'Centroid:',xce(:,i)
#endif
      enddo
#ifdef _DEBUG_
      open(unit=10,file='CELL_CENTROIDS.dat',status='unknown')
      write(10,*) 'variables = x y z'
      write(10,*) 'zone T="Cell centroids"'
      do i = 1,nel
         write(10,*) xce(:,i)
      enddo
      close(10)
#endif

c--- form face centroids
      do j = 1,nfa
         if( ifn(0,j) .eq. 3 ) then
            xcf(:,j) = (x(:, ifn(1,j) ) +
     &                  x(:, ifn(2,j) ) +
     &                  x(:, ifn(3,j) ))/3.0d0
         else if( ifn(0,j) .eq. 4 ) then
            xcf(:,j) = (x(:, ifn(1,j) )*2.0d0 +
     &                  x(:, ifn(2,j) ) +
     &                  x(:, ifn(3,j) )*2.0d0 +
     &                  x(:, ifn(4,j) ))/6.0d0
         else
            PRINT*,'Problematic face!'
            PRINT*,'ifn(0,j)=',ifn(0,j)
            STOP
         endif


#ifdef _DEBUG2_
         PRINT*,'Centroid:',xcf(:,i)
#endif
      enddo
#ifdef _DEBUG_
      open(unit=10,file='FACE_CENTROIDS.dat',status='unknown')
      write(10,*) 'variables = x y z'
      write(10,*) 'zone T="Face centroids"'
      do j = 1,nfa
         write(10,*) xcf(:,j)
      enddo
      close(10)
#endif

c--- form ghost-cell centroids
c--- (Uses an extrapolation in the face-normal direction!)
      ii = nel
      do k = 1,nz
         if( izone(4,k) .eq. 13 ) then
            PRINT*,'Zone of faces:',k
            do j = izone(1,k),izone(2,k)

               if( ife(1,j).le.0 ) then
                  i = ife(2,j)
                  ii = ii + 1
                  ife(1,j) = ii

                  if( ifn(0,j).eq.3 ) then
                     dx1(:) = x(:, ifn(2,j)) - x(:, ifn(1,j))
                     dx2(:) = x(:, ifn(3,j)) - x(:, ifn(1,j))
                     xn(1) =  dx1(2)*dx2(3) -dx2(2)*dx1(3)
                     xn(2) = -dx1(1)*dx2(3) +dx2(1)*dx1(3)
                     xn(3) =  dx1(1)*dx2(2) -dx2(1)*dx1(2)
                  else
                     dx1(:) = x(:, ifn(2,j)) - x(:, ifn(1,j))
                     dx2(:) = x(:, ifn(3,j)) - x(:, ifn(1,j))
                     xn(1) =  dx1(2)*dx2(3) -dx2(2)*dx1(3)
                     xn(2) = -dx1(1)*dx2(3) +dx2(1)*dx1(3)
                     xn(3) =  dx1(1)*dx2(2) -dx2(1)*dx1(2)
                     dx1(:) = x(:, ifn(3,j)) - x(:, ifn(1,j))
                     dx2(:) = x(:, ifn(4,j)) - x(:, ifn(1,j))
                     xn(1) = (xn(1) +dx1(2)*dx2(3) -dx2(2)*dx1(3))*0.5d0
                     xn(2) = (xn(2) -dx1(1)*dx2(3) +dx2(1)*dx1(3))*0.5d0
                     xn(3) = (xn(3) +dx1(1)*dx2(2) -dx2(1)*dx1(2))*0.5d0
                  endif
                  xl = sqrt(xn(1)**2 + xn(2)**2 + xn(3)**2)
                  xn(:) = xn(:)/xl

                  dx1(:) = xcf(:,j) - xce(:,i)
                  xl = dx1(1)*xn(1) + dx1(2)*xn(2) + dx1(3)*xn(3)
                  xce(:,ii) = xce(:,i) + 2.0d0*xl*xn(:)
               endif

               if( ife(2,j).le.0 ) then
                  i = ife(1,j)
                  ii = ii + 1
                  ife(2,j) = ii

                  if( ifn(0,j).eq.3 ) then
                     dx1(:) = x(:, ifn(2,j)) - x(:, ifn(1,j))
                     dx2(:) = x(:, ifn(3,j)) - x(:, ifn(1,j))
                     xn(1) =  dx1(2)*dx2(3) -dx2(2)*dx1(3)
                     xn(2) = -dx1(1)*dx2(3) +dx2(1)*dx1(3)
                     xn(3) =  dx1(1)*dx2(2) -dx2(1)*dx1(2)
                  else
                     dx1(:) = x(:, ifn(2,j)) - x(:, ifn(1,j))
                     dx2(:) = x(:, ifn(3,j)) - x(:, ifn(1,j))
                     xn(1) =  dx1(2)*dx2(3) -dx2(2)*dx1(3)
                     xn(2) = -dx1(1)*dx2(3) +dx2(1)*dx1(3)
                     xn(3) =  dx1(1)*dx2(2) -dx2(1)*dx1(2)
                     dx1(:) = x(:, ifn(3,j)) - x(:, ifn(1,j))
                     dx2(:) = x(:, ifn(4,j)) - x(:, ifn(1,j))
                     xn(1) = (xn(1) +dx1(2)*dx2(3) -dx2(2)*dx1(3))*0.5d0
                     xn(2) = (xn(2) -dx1(1)*dx2(3) +dx2(1)*dx1(3))*0.5d0
                     xn(3) = (xn(3) +dx1(1)*dx2(2) -dx2(1)*dx1(2))*0.5d0
                  endif
                  xl = sqrt(xn(1)**2 + xn(2)**2 + xn(3)**2)
                  xn(:) = xn(:)/xl

                  dx1(:) = xcf(:,j) - xce(:,i)
                  xl = dx1(1)*xn(1) + dx1(2)*xn(2) + dx1(3)*xn(3)
                  xce(:,ii) = xce(:,i) + 2.0d0*xl*xn(:)
               endif

            enddo
         endif
      enddo
#ifdef _DEBUG_
      open(unit=10,file='GHOST_CENTROIDS.dat',status='unknown')
      write(10,*) 'variables = x y z'
      do k = 1,nz
         if( izone(4,k) .eq. 13 .AND. k.ne.3 ) then   ! special case!
            PRINT*,'Writing zone of faces:',k
            write(10,*) 'zone T="Boundary faces"'
            write(10,*) 'NODES=',nno
            write(10,*) 'ELEMENTS=',izone(2,k)-izone(1,k)+1
            write(10,*) 'ET=QUADRILATERAL,F=FEPOINT'
            do i = 1,nno
               write(10,*) x(:,i)
            enddo
            do j = izone(1,k),izone(2,k)
               if( ifn(0,j) .eq. 4) write(10,*) ifn(1:4,j)
               if( ifn(0,j) .eq. 3) write(10,*) ifn(1:3,j),ifn(3,j)
            enddo

            PRINT*,'Writing zone of centroid-vectors:',k
            write(10,*) 'zone T="Centroids as vectors"'
            write(10,*) 'NODES=',nel+nge
            write(10,*) 'ELEMENTS=',izone(2,k)-izone(1,k)+1
            write(10,*) 'ET=TRIANGLE,F=FEPOINT'
            do i = 1,nel+nge
               write(10,*) xce(:,i)
            enddo
            do j = izone(1,k),izone(2,k)
               write(10,*) ife(1,j),ife(2,j),ife(1,j)
            enddo
         endif
      enddo
      close(10)
#endif

      End subroutine

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c *** A subroutine to create cells from arrays adhering to a Fluent case file
c *** format. The routine simply loops over cells and invokes special cell
c *** construction functions. It fills arrays that are returned to the calling
c *** function/routine.
c *** 
c *** Created:       IN <nompelis@nobelware.com> 20171006
c *** Last modified: IN <nompelis@nobelware.com> 20171102
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      Subroutine make_cells(nz,nno,nel,nfa,ifn,ife,izone,x,ief,ien)
      Use inMesh_Elements
      Implicit None
      Integer(kind=8),intent(IN) :: nno,nel,nfa
      Integer(kind=4),intent(IN) :: nz
      Integer(kind=8),intent(IN),dimension(0:4,nfa) :: ifn
      Integer(kind=8),intent(IN),dimension(2,nfa) :: ife
      Integer(kind=8),intent(IN),dimension(4,nz) :: izone
      Real*8,dimension(3,nno),intent(IN) :: x
      Integer(kind=8),dimension(0:6,nel),intent(OUT) :: ief
      Integer(kind=8),dimension(0:8,nel),intent(OUT) :: ien
      Integer(kind=4) :: ierr

      Integer(kind=8) :: i,j,k
      Integer(kind=8) :: ntri,nqua
      Integer(kind=8) :: ieno(0:8),iefo(0:8)
      Logical, parameter :: itest = .TRUE.


      PRINT*,'Number of cells:',nel

      IF( itest ) THEN
c--- write a tecplot file with all cells; first put the nodes
c--- THIS IS FOR DEVELOPMENT TESTING PURPOSES
      open(unit=10,file='CELLS.dat',status='unknown',form='formatted')
      write(10,*) 'variables = x y z'
      write(10,*) 'zone T="Elements as hexahedra"'
      write(10,*) 'NODES=',nno,', ELEMENTS=',nel
      write(10,*) 'ET=BRICK'
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

         ieno(:) = 0

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
c--- store data to returned array
            ien(:,i) = ieno(:)
c--- modify nodes to plot as a "brick" element
            ieno(5:8) = ieno(4)
            ieno(4) = ieno(3)
         else if( ntri .eq. 4 .AND. nqua .eq. 1 ) then
            call inMesh_Elements_FormPyramidFromFaces(
     &                  nfa, ifn, ife, i, ief(0,i),
     &                  ieno, iefo, ierr )
c--- store data to returned array
            ien(:,i) = ieno(:)
c--- modify nodes to plot as a "brick" element
            ieno(6:8) = ieno(5)
         else if( ntri .eq. 3 .AND. nqua .eq. 2 ) then
            call inMesh_Elements_FormWedgeFromFaces(
     &                  nfa, ifn, ife, i, ief(0,i),
     &                  ieno, iefo, ierr )
c--- store data to returned array
            ien(:,i) = ieno(:)
c--- modify nodes to plot as a "brick" element
            ieno(7:8) = ieno(6)
            ieno(6) = ieno(5)
            ieno(5) = ieno(4)
            ieno(4) = ieno(3)
         else if( ntri .eq. 0 .AND. nqua .eq. 6 ) then
            call inMesh_Elements_FormBrickFromFaces(
     &                  nfa, ifn, ife, i, ief(0,i),
     &                  ieno, iefo, ierr )
c--- store data to returned array
            ien(:,i) = ieno(:)
         else
            PRINT*,'Problematic element!'
            PRINT*,'ntri=',ntri,'nqua=',nqua
            STOP
         endif

      IF( itest ) THEN
c--- dump element node-connectivity to the file; this is not generic...
c--- THIS IS FOR DEVELOPMENT TESTING PURPOSES
         write(10,*) ieno(1:8)
      ENDIF

      enddo

      IF( itest ) THEN
c--- close the file; THIS IS FOR DEVELOPMENT TESTING PURPOSES
      close(10)
      ENDIF

      End subroutine

