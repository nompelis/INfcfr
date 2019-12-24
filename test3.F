ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c *** A program driver to demonstrate the use of the Fortran wrapper of the
c *** Fluent case-file reader library. This code invokes the library to read
c *** a case file. Then, it makes cells by invoking a subroutine to construct
c *** each cell by returning the element's nodes in an array; the node ordering
c *** is specific to my own conventions outlined in my notes.
c *** This code is intended to be used for testing scalar field gradient
c *** construction using different methods. It is based off the "demo1" code
c *** and has been substantially modified.
c ***
c *** Created:       IN <nompelis@nobelware.com> 20171006
c *** Last modified: IN <nompelis@nobelware.com> 20191216
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      Program main
      Implicit None
      Character*100 :: fname
      Integer(kind=8) :: nno,nel,nfa,nge,net
      Integer(kind=8),allocatable,dimension(:,:) :: ifn,ife,izone
      Integer(kind=8),allocatable,dimension(:,:) :: ief,ien
      Real*8,allocatable,dimension(:,:) :: x,xce,xcf,dq
      Real*8,allocatable,dimension(:) :: s,q,vol
      Real*8,allocatable,dimension(:,:,:) :: ssx
      Integer(kind=4) :: ierr,nz,k
      Integer(kind=8) :: n,ifnt(0:4),i,j
      Logical, parameter :: itest = .FALSE.


c--- read a Fluent case-file and transfer it to arrays
      fname = 'grid.cas'
      call inFluent_FortranWrapperInit( trim( fname ), nz, nno, nel, nfa, ierr )
      PRINT*,'Result of Init() ierr=',ierr,' nz=',nz

      allocate( ifn(0:4,nfa), ife(2,nfa), izone(5,nz),
     &          x(3,nno), STAT=ierr )
      PRINT*,'Result of arrays allocation ierr=',ierr

      call inFluent_FortranWrapperFill( izone, ifn, ife, x, ierr )
      PRINT*,'Result of Fill() ierr=',ierr
      do n = 1,nz
         write(*,101) 'ZONE',n,'->',izone(:,n)
      enddo
 101  Format(a4,1x,i5,1x,a2,1x,4(1x,i12),3x,i4)

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

c--- swap Fluent's convention of cr,cl to mine of left,right (in array "ife")
      do n = 1,nfa
         i = ife(1,n)
         ife(1,n) = ife(2,n)
         ife(2,n) = i
      enddo

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
      allocate( xce(3,net),vol(nel), STAT=ierr)
      allocate( xcf(3,nfa),ssx(3,3,nfa),s(nfa), STAT=ierr)
      allocate( q(net),dq(3,net), STAT=ierr)

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
     &                   ifn,ife,izone,x,ief,ien,xce,vol,xcf,s,ssx)

c--- call a subroutine to test gradient reconstruction
      call test_gradient(nz,nno,nel,nge,net,nfa,
     &                   ifn,ife,izone,x,ief,ien,xce,vol,xcf,ssx,s,q,dq)

c--- drop arrays
      deallocate( x, ifn, ife, izone, ief, ien, xce, vol, xcf, s, ssx, q, dq )

      End

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c *** A subroutine to perform gradient reconstruction from field data using
c *** the Weighted Least-Squares method. The weights used are tested here.
c *** Several methods are used:
c *** 1. A standard inverse-distance weight based on my "hyper-plane regression"
c *** method with 4 equations
c *** 2. The WLSQ(N) according to Shima, Kitamura & Haga (AIAA J. Vol 51, No 11,
c *** Nov. 2013) with N=0
c *** 3. The WLSQ(N) according to Shima, Kitamura & Haga (AIAA J. Vol 51, No 11,
c *** Nov. 2013) with N=2
c *** 4. The WLSQ(G) according to SK&H
c ***
c *** Created:       IN <nompelis@nobelware.com> 20171107
c *** Last modified: IN <nompelis@nobelware.com> 20191216
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      Subroutine test_gradient(nz,nno,nel,nge,net,nfa,
     &                         ifn,ife,izone,x,ief,ien,xce,vol,xcf,ssx,s,q,dq)
      Implicit None
      Integer(kind=8),intent(IN) :: nno,nel,nfa,nge,net
      Integer(kind=4),intent(IN) :: nz
      Integer(kind=8),intent(IN) :: ifn(0:4,nfa)
      Integer(kind=8),intent(IN) :: ife(2,nfa)
      Integer(kind=8),intent(IN) :: izone(5,nz)
      Integer(kind=8),intent(IN) :: ief(0:6,nel)
      Integer(kind=8),intent(IN) :: ien(0:8,nel)
      Real*8,intent(IN) :: x(3,nno)
      Real*8,intent(IN) :: xce(3,net),xcf(3,nfa)
      Real*8,intent(IN) :: vol(nel),ssx(3,3,nfa),s(nfa)
      Real*8,intent(INOUT) :: q(net),dq(3,net)

      Integer(kind=8) :: i,j,k,ii,ieni(8)
      Real*8 :: xx(3),delta
      Integer(kind=4) :: imethod = 5
      Real*8 :: rmat(4,4),rhs(4),xo(3),rd,wei(6),wem
      Real*8 :: xn(3),delq,rl,xy(3),beta,sgn
      Logical, parameter :: itest = .TRUE.


      PRINT*,'Testing gradients'

c--- Impose a field variable and its analytical gradient vector
      delta = 60.0d0
      do i = 1,nel+nge
         xx(:) = xce(:,i)
c======= Field variable
         q(i) = sin( xx(2)*delta )
c======= Analytical (exact) gradient of field variable
         dq(1,i) = 0.0d0
         dq(2,i) = delta*cos( xx(2)*delta )
         dq(3,i) = 0.0d0
      enddo

c--- Output the data
      open(unit=10,file='CELL_DATA.dat',status='unknown')
      write(10,*) 'variables = x y z q'
      write(10,*) 'zone T="Cell-center data"'
      write(10,*) 'NODES=',nno
      write(10,*) 'ELEMENTS=',nel
      write(10,*) 'DATAPACKING=BLOCK, ZONETYPE=FEBRICK'
      write(10,*) 'VARLOCATION=([1-3]=NODAL,[4]=CELLCENTERED)'
      do i = 1,nno
         write(10,*) x(1,i)
      enddo
      do i = 1,nno
         write(10,*) x(2,i)
      enddo
      do i = 1,nno
         write(10,*) x(3,i)
      enddo
      do i = 1,nel
         write(10,*) q(i)
      enddo
      do i = 1,nel
         if( ien(0,i) .eq. 4 ) then
            ieni(1:3) = ien(1:3,i)
            ieni(4) = ien(3,i)
            ieni(5:8) = ien(4,i)
         else if( ien(0,i) .eq. 5 ) then
            ieni(1:4) = ien(1:4,i)
            ieni(5:8) = ien(4,i)
         else if( ien(0,i) .eq. 6 ) then
            ieni(1:3) = ien(1:3,i)
            ieni(4) = ien(3,i)
            ieni(5:7) = ien(4:6,i)
            ieni(8) = ien(6,i)
         else if( ien(0,i) .eq. 8 ) then
            ieni(:) = ien(1:8,i)
         else
            PRINT*,'Prolematic element connectivity'
            STOP
         endif

         write(10,*) ieni(:)
      enddo
      close(10)

c--- Output the analytical gradient
      open(unit=10,file='CELL_GRADIENT.dat',status='unknown')
      write(10,*) 'variables = x y z dq_x dq_y dq_z'
      write(10,*) 'zone T="Cell-center grad"'
      write(10,*) 'NODES=',nno
      write(10,*) 'ELEMENTS=',nel
      write(10,*) 'DATAPACKING=BLOCK, ZONETYPE=FEBRICK'
      write(10,*) 'VARLOCATION=([1-3]=NODAL,[4-6]=CELLCENTERED)'
      do i = 1,nno
         write(10,*) x(1,i)
      enddo
      do i = 1,nno
         write(10,*) x(2,i)
      enddo
      do i = 1,nno
         write(10,*) x(3,i)
      enddo
      do i = 1,nel
         write(10,*) dq(1,i)
      enddo
      do i = 1,nel
         write(10,*) dq(2,i)
      enddo
      do i = 1,nel
         write(10,*) dq(3,i)
      enddo
      do i = 1,nel
         if( ien(0,i) .eq. 4 ) then
            ieni(1:3) = ien(1:3,i)
            ieni(4) = ien(3,i)
            ieni(5:8) = ien(4,i)
         else if( ien(0,i) .eq. 5 ) then
            ieni(1:4) = ien(1:4,i)
            ieni(5:8) = ien(4,i)
         else if( ien(0,i) .eq. 6 ) then
            ieni(1:3) = ien(1:3,i)
            ieni(4) = ien(3,i)
            ieni(5:7) = ien(4:6,i)
            ieni(8) = ien(6,i)
         else if( ien(0,i) .eq. 8 ) then
            ieni(:) = ien(1:8,i)
         else
            PRINT*,'Prolematic element connectivity'
            STOP
         endif

         write(10,*) ieni(:)
      enddo
      close(10)

c--- Calculate the approximate gradient
c--- (Disregard all arithmetic efficiency concerns for now...)
      do i = 1,nel
         dq(:,i) = 0.0d0

         rmat(:,:) = 0.0d0
         rhs(:) = 0.0d0

         Select Case( imethod )

         Case(1)   ! IN's WLS

c--- form weights
         wem = 0.0d0                  ! maximum weight value
         xo(:) = xce(:,i)             ! get cell centroid
         do k = 1,ief(0,i)            ! loop over faces of element
            j = ief(k,i)              ! get face number
            if( ife(1,j) .eq. i ) then
               ii = ife(2,j)          ! get adjacent cell number
            else
               ii = ife(1,j)          ! get adjacent cell number
            endif
            xx(:) = xce(:,ii) - xo(:) ! get cent-to-cent vector
            rd = sqrt( xx(1)**2 + xx(2)**2 + xx(3)**2 )

            wei(k) = 1.0d0/rd
            if( wem .lt. wei(k) ) wem = wei(k)
         enddo
         do k = 1,ief(0,i)
            wei(k) = wei(k)/wem       ! normalize weights
         enddo

c--- use weights
         do k = 1,ief(0,i)            ! loop over faces of element
            j = ief(k,i)              ! get face number
            if( ife(1,j) .eq. i ) then
               ii = ife(2,j)          ! get adjacent cell number
            else
               ii = ife(1,j)          ! get adjacent cell number
            endif
            xx(:) = xce(:,ii) - xo(:) ! get cent-to-cent vector

            rmat(1:3,1) = rmat(1:3,1) + xx(1)*xx(:) *wei(k)
            rmat(1:3,2) = rmat(1:3,2) + xx(2)*xx(:) *wei(k)
            rmat(1:3,3) = rmat(1:3,3) + xx(3)*xx(:) *wei(k)
            rmat(1:3,4) = rmat(1:3,4) +       xx(:) *wei(k)
            rmat(4,1:3) = rmat(4,1:3) +       xx(:) *wei(k)
            rmat(4,4)   = rmat(4,4)   +       1.0d0 *wei(k)

            rhs(1:3) = rhs(1:3) + xx(:)*q(ii) *wei(k)
            rhs(4)   = rhs(4)   +       q(ii) *wei(k)
         enddo

         call invert_fbf( rmat )

         dq(:,i) = rmat(1:3,1)*rhs(1) +
     &             rmat(1:3,2)*rhs(2) +
     &             rmat(1:3,3)*rhs(3) +
     &             rmat(1:3,4)*rhs(4)

         Case(2)   ! SK&H WLSQ(N=0)

c--- form weights
         wem = 0.0d0                  ! maximum weight value
         xo(:) = xce(:,i)             ! get cell centroid
         do k = 1,ief(0,i)            ! loop over faces of element
            j = ief(k,i)              ! get face number
            if( ife(1,j) .eq. i ) then
               ii = ife(2,j)          ! get adjacent cell number
            else
               ii = ife(1,j)          ! get adjacent cell number
            endif
            xx(:) = xce(:,ii) - xo(:) ! get cent-to-cent vector
            rd = sqrt( xx(1)**2 + xx(2)**2 + xx(3)**2 )

            xn(:) = xx(:)/rd                 ! (x_L)_j
            delq = q(ii) - q(i)              ! (Delta q)_j
            rl = rd                          ! L_j
            wei(k) =    1.0d0                ! w_j

            rmat(1:3,1) = rmat(1:3,1) + wei(k)*xx(:)*xx(1)
            rmat(1:3,2) = rmat(1:3,2) + wei(k)*xx(:)*xx(2)
            rmat(1:3,3) = rmat(1:3,3) + wei(k)*xx(:)*xx(3)
            !---- need a 4th equation given the hard-wired 4x4 inversion
            rmat(4,4) = 1.0d0                ! over-write without worries...

            rhs(1:3) = rhs(1:3) + wei(k)*rl*delq*xn(:)
         enddo

         call invert_fbf( rmat )

         dq(:,i) = rmat(1:3,1)*rhs(1) +
     &             rmat(1:3,2)*rhs(2) +
     &             rmat(1:3,3)*rhs(3) +
     &             rmat(1:3,4)*rhs(4)

         Case(3)   ! SK&H WLSQ(N=2)

c--- form weights
         wem = 0.0d0                  ! maximum weight value
         xo(:) = xce(:,i)             ! get cell centroid
         do k = 1,ief(0,i)            ! loop over faces of element
            j = ief(k,i)              ! get face number
            if( ife(1,j) .eq. i ) then
               ii = ife(2,j)          ! get adjacent cell number
            else
               ii = ife(1,j)          ! get adjacent cell number
            endif
            xx(:) = xce(:,ii) - xo(:) ! get cent-to-cent vector
            rd = sqrt( xx(1)**2 + xx(2)**2 + xx(3)**2 )

            xn(:) = xx(:)/rd                 ! (x_L)_j
            delq = q(ii) - q(i)              ! (Delta q)_j
            rl = rd                          ! L_j
            wei(k) =    1.0d0/rl**2          ! w_j

            rmat(1:3,1) = rmat(1:3,1) + wei(k)*xx(:)*xx(1)
            rmat(1:3,2) = rmat(1:3,2) + wei(k)*xx(:)*xx(2)
            rmat(1:3,3) = rmat(1:3,3) + wei(k)*xx(:)*xx(3)
            !---- need a 4th equation given the hard-wired 4x4 inversion
            rmat(4,4) = 1.0d0                ! over-write without worries...

            rhs(1:3) = rhs(1:3) + wei(k)*rl*delq*xn(:)
         enddo

         call invert_fbf( rmat )

         dq(:,i) = rmat(1:3,1)*rhs(1) +
     &             rmat(1:3,2)*rhs(2) +
     &             rmat(1:3,3)*rhs(3) +
     &             rmat(1:3,4)*rhs(4)      ! last entry is not needed

         Case(4)   ! SK&H WLSQ(G)

c--- form weights
         wem = 0.0d0                  ! maximum weight value
         xo(:) = xce(:,i)             ! get cell centroid
         do k = 1,ief(0,i)            ! loop over faces of element
            j = ief(k,i)              ! get face number
            if( ife(1,j) .eq. i ) then
               ii = ife(2,j)          ! get adjacent cell number
            else
               ii = ife(1,j)          ! get adjacent cell number
            endif
            xx(:) = xce(:,ii) - xo(:) ! get cent-to-cent vector
            rd = sqrt( xx(1)**2 + xx(2)**2 + xx(3)**2 )

            xn(:) = xx(:)/rd                 ! (x_L)_j
            delq = q(ii) - q(i)              ! (Delta q)_j
            rl = rd                          ! L_j
            wei(k) =    1.0d0/rl**2          ! w_j

            xy(:) = xcf(:,j) - xo(:) ! get cent-to-face vector
            wei(k) = 2.0d0 *
     &               (xy(1)*ssx(1,1,j) + xy(2)*ssx(2,1,j) + xy(3)*ssx(3,1,j)) /
     &               (xx(1)*ssx(1,1,j) + xx(2)*ssx(2,1,j) + xx(3)*ssx(3,1,j))
            wei(k) = wei(k)**2*s(j)/rl

            rmat(1:3,1) = rmat(1:3,1) + wei(k)*xx(:)*xx(1)
            rmat(1:3,2) = rmat(1:3,2) + wei(k)*xx(:)*xx(2)
            rmat(1:3,3) = rmat(1:3,3) + wei(k)*xx(:)*xx(3)
            !---- need a 4th equation given the hard-wired 4x4 inversion
            rmat(4,4) = 1.0d0                ! over-write without worries...

            rhs(1:3) = rhs(1:3) + wei(k)*rl*delq*xn(:)
         enddo

         call invert_fbf( rmat )

         dq(:,i) = rmat(1:3,1)*rhs(1) +
     &             rmat(1:3,2)*rhs(2) +
     &             rmat(1:3,3)*rhs(3) +
     &             rmat(1:3,4)*rhs(4)      ! last entry is not needed

         Case(5)   ! SK&H GLSQ (i.e. hybrid formula of G-G and WLSQ(G))

c--- form weights
         beta = 0.0d0      ! TEMPORARY (to force only G-G)

         wem = 0.0d0                  ! maximum weight value
         xo(:) = xce(:,i)             ! get cell centroid
         do k = 1,ief(0,i)            ! loop over faces of element
            j = ief(k,i)              ! get face number
            if( ife(1,j) .eq. i ) then
               ii = ife(2,j)          ! get adjacent cell number
               sgn = +1.0d0           ! outward-pointing convention
            else
               ii = ife(1,j)          ! get adjacent cell number
               sgn = -1.0d0           ! outward-pointing convention
            endif
            xx(:) = xce(:,ii) - xo(:) ! get cent-to-cent vector
            rd = sqrt( xx(1)**2 + xx(2)**2 + xx(3)**2 )

            xn(:) = xx(:)/rd                 ! (x_L)_j
            delq = q(ii) - q(i)              ! (Delta q)_j
            rl = rd                          ! L_j
            wei(k) =    1.0d0/rl**2          ! w_j

            xy(:) = xcf(:,j) - xo(:) ! get cent-to-face vector
            wei(k) = 2.0d0 *
     &               (xy(1)*ssx(1,1,j) + xy(2)*ssx(2,1,j) + xy(3)*ssx(3,1,j)) /
     &               (xx(1)*ssx(1,1,j) + xx(2)*ssx(2,1,j) + xx(3)*ssx(3,1,j))
            wei(k) = wei(k)**2*s(j)/rl

            !--- same entries as WLSQ(G) weighed by beta (Eq.14 with D=1)
            rmat(1:3,1) = rmat(1:3,1) + beta*wei(k)*xx(:)*xx(1)
            rmat(1:3,2) = rmat(1:3,2) + beta*wei(k)*xx(:)*xx(2)
            rmat(1:3,3) = rmat(1:3,3) + beta*wei(k)*xx(:)*xx(3)

            !--- same entries as WLSQ(G) weighed by beta (Eq.14 with D=1)
            rhs(1:3) = rhs(1:3) + beta*wei(k)*rl*delq*xn(:)

            !--- augmenting RHS with G-G RHS (Eq.14 with D=1)
            rhs(1:3) = rhs(1:3) + 2.0d0*(1.0d0 - beta) *
     &                            0.5d0*s(j)*sgn*ssx(:,1,j)*(q(ii)+q(i))
     &   !                        0.5d0*s(j)*sgn*ssx(:,1,j)*delq
         enddo

         !--- augmenting LHS diagonal entries from G-G (Eq.14 with D=1 / Eq.15)
         rmat(1,1) = rmat(1,1) + 2.0d0*(1.0d0 - beta)*vol(i)
         rmat(2,2) = rmat(2,2) + 2.0d0*(1.0d0 - beta)*vol(i)
         rmat(3,3) = rmat(3,3) + 2.0d0*(1.0d0 - beta)*vol(i)

         !---- need a 4th equation given the hard-wired 4x4 inversion
         rmat(4,4) = 1.0d0                ! over-write without worries...

         call invert_fbf( rmat )

         dq(:,i) = rmat(1:3,1)*rhs(1) +
     &             rmat(1:3,2)*rhs(2) +
     &             rmat(1:3,3)*rhs(3) +
     &             rmat(1:3,4)*rhs(4)      ! last entry is not needed

         End Select

      enddo

c--- Output the approximate gradient
      open(unit=10,file='CELL_APPROX_GRADIENT.dat',status='unknown')
      write(10,*) 'variables = x y z dq_x dq_y dq_z'
      write(10,*) 'zone T="Cell-center approx"'
      write(10,*) 'NODES=',nno
      write(10,*) 'ELEMENTS=',nel
      write(10,*) 'DATAPACKING=BLOCK, ZONETYPE=FEBRICK'
      write(10,*) 'VARLOCATION=([1-3]=NODAL,[4-6]=CELLCENTERED)'
      do i = 1,nno
         write(10,*) x(1,i)
      enddo
      do i = 1,nno
         write(10,*) x(2,i)
      enddo
      do i = 1,nno
         write(10,*) x(3,i)
      enddo
      do i = 1,nel
         write(10,*) dq(1,i)
      enddo
      do i = 1,nel
         write(10,*) dq(2,i)
      enddo
      do i = 1,nel
         write(10,*) dq(3,i)
      enddo
      do i = 1,nel
         if( ien(0,i) .eq. 4 ) then
            ieni(1:3) = ien(1:3,i)
            ieni(4) = ien(3,i)
            ieni(5:8) = ien(4,i)
         else if( ien(0,i) .eq. 5 ) then
            ieni(1:4) = ien(1:4,i)
            ieni(5:8) = ien(4,i)
         else if( ien(0,i) .eq. 6 ) then
            ieni(1:3) = ien(1:3,i)
            ieni(4) = ien(3,i)
            ieni(5:7) = ien(4:6,i)
            ieni(8) = ien(6,i)
         else if( ien(0,i) .eq. 8 ) then
            ieni(:) = ien(1:8,i)
         else
            PRINT*,'Prolematic element connectivity'
            STOP
         endif

         write(10,*) ieni(:)
      enddo
      close(10)

      End subroutine

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c *** A subroutine to invert a 4x4 matrix.
c ***
c *** Created:       IN <nompelis@nobelware.com> 20041220
c *** Last modified: IN <nompelis@nobelware.com> 20171107
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      Subroutine invert_fbf( aa )
      Implicit None
      Real*8, intent(INOUT) :: aa(4,4)
      Real*8 :: cc(4,4),det


      cc(1,1) = - aa(2,4)*aa(3,3)*aa(4,2) + aa(2,3)*aa(3,4)*aa(4,2)
     &          + aa(2,4)*aa(3,2)*aa(4,3) - aa(2,2)*aa(3,4)*aa(4,3)
     &          - aa(2,3)*aa(3,2)*aa(4,4) + aa(2,2)*aa(3,3)*aa(4,4)

      cc(1,2) = + aa(1,4)*aa(3,3)*aa(4,2) - aa(1,3)*aa(3,4)*aa(4,2)
     &          - aa(1,4)*aa(3,2)*aa(4,3) + aa(1,2)*aa(3,4)*aa(4,3)
     &          + aa(1,3)*aa(3,2)*aa(4,4) - aa(1,2)*aa(3,3)*aa(4,4)

      cc(1,3) = - aa(1,4)*aa(2,3)*aa(4,2) + aa(1,3)*aa(2,4)*aa(4,2)
     &          + aa(1,4)*aa(2,2)*aa(4,3) - aa(1,2)*aa(2,4)*aa(4,3)
     &          - aa(1,3)*aa(2,2)*aa(4,4) + aa(1,2)*aa(2,3)*aa(4,4)

      cc(1,4) = + aa(1,4)*aa(2,3)*aa(3,2) - aa(1,3)*aa(2,4)*aa(3,2)
     &          - aa(1,4)*aa(2,2)*aa(3,3) + aa(1,2)*aa(2,4)*aa(3,3)
     &          + aa(1,3)*aa(2,2)*aa(3,4) - aa(1,2)*aa(2,3)*aa(3,4)


      cc(2,1) = + aa(2,4)*aa(3,3)*aa(4,1) - aa(2,3)*aa(3,4)*aa(4,1)
     &          - aa(2,4)*aa(3,1)*aa(4,3) + aa(2,1)*aa(3,4)*aa(4,3)
     &          + aa(2,3)*aa(3,1)*aa(4,4) - aa(2,1)*aa(3,3)*aa(4,4)

      cc(2,2) = - aa(1,4)*aa(3,3)*aa(4,1) + aa(1,3)*aa(3,4)*aa(4,1)
     &          + aa(1,4)*aa(3,1)*aa(4,3) - aa(1,1)*aa(3,4)*aa(4,3)
     &          - aa(1,3)*aa(3,1)*aa(4,4) + aa(1,1)*aa(3,3)*aa(4,4)

      cc(2,3) = + aa(1,4)*aa(2,3)*aa(4,1) - aa(1,3)*aa(2,4)*aa(4,1)
     &          - aa(1,4)*aa(2,1)*aa(4,3) + aa(1,1)*aa(2,4)*aa(4,3)
     &          + aa(1,3)*aa(2,1)*aa(4,4) - aa(1,1)*aa(2,3)*aa(4,4)

      cc(2,4) = - aa(1,4)*aa(2,3)*aa(3,1) + aa(1,3)*aa(2,4)*aa(3,1)
     &          + aa(1,4)*aa(2,1)*aa(3,3) - aa(1,1)*aa(2,4)*aa(3,3)
     &          - aa(1,3)*aa(2,1)*aa(3,4) + aa(1,1)*aa(2,3)*aa(3,4)


      cc(3,1) = - aa(2,4)*aa(3,2)*aa(4,1) + aa(2,2)*aa(3,4)*aa(4,1)
     &          + aa(2,4)*aa(3,1)*aa(4,2) - aa(2,1)*aa(3,4)*aa(4,2)
     &          - aa(2,2)*aa(3,1)*aa(4,4) + aa(2,1)*aa(3,2)*aa(4,4)

      cc(3,2) = + aa(1,4)*aa(3,2)*aa(4,1) - aa(1,2)*aa(3,4)*aa(4,1)
     &          - aa(1,4)*aa(3,1)*aa(4,2) + aa(1,1)*aa(3,4)*aa(4,2)
     &          + aa(1,2)*aa(3,1)*aa(4,4) - aa(1,1)*aa(3,2)*aa(4,4)

      cc(3,3) = - aa(1,4)*aa(2,2)*aa(4,1) + aa(1,2)*aa(2,4)*aa(4,1)
     &          + aa(1,4)*aa(2,1)*aa(4,2) - aa(1,1)*aa(2,4)*aa(4,2)
     &          - aa(1,2)*aa(2,1)*aa(4,4) + aa(1,1)*aa(2,2)*aa(4,4)

      cc(3,4) = + aa(1,4)*aa(2,2)*aa(3,1) - aa(1,2)*aa(2,4)*aa(3,1)
     &          - aa(1,4)*aa(2,1)*aa(3,2) + aa(1,1)*aa(2,4)*aa(3,2)
     &          + aa(1,2)*aa(2,1)*aa(3,4) - aa(1,1)*aa(2,2)*aa(3,4)


      cc(4,1) = + aa(2,3)*aa(3,2)*aa(4,1) - aa(2,2)*aa(3,3)*aa(4,1)
     &          - aa(2,3)*aa(3,1)*aa(4,2) + aa(2,1)*aa(3,3)*aa(4,2)
     &          + aa(2,2)*aa(3,1)*aa(4,3) - aa(2,1)*aa(3,2)*aa(4,3)

      cc(4,2) = - aa(1,3)*aa(3,2)*aa(4,1) + aa(1,2)*aa(3,3)*aa(4,1)
     &          + aa(1,3)*aa(3,1)*aa(4,2) - aa(1,1)*aa(3,3)*aa(4,2)
     &          - aa(1,2)*aa(3,1)*aa(4,3) + aa(1,1)*aa(3,2)*aa(4,3)

      cc(4,3) = + aa(1,3)*aa(2,2)*aa(4,1) - aa(1,2)*aa(2,3)*aa(4,1)
     &          - aa(1,3)*aa(2,1)*aa(4,2) + aa(1,1)*aa(2,3)*aa(4,2)
     &          + aa(1,2)*aa(2,1)*aa(4,3) - aa(1,1)*aa(2,2)*aa(4,3)

      cc(4,4) = - aa(1,3)*aa(2,2)*aa(3,1) + aa(1,2)*aa(2,3)*aa(3,1)
     &          + aa(1,3)*aa(2,1)*aa(3,2) - aa(1,1)*aa(2,3)*aa(3,2)
     &          - aa(1,2)*aa(2,1)*aa(3,3) + aa(1,1)*aa(2,2)*aa(3,3)


      det = + aa(1,4)*aa(2,3)*aa(3,2)*aa(4,1) - aa(1,3)*aa(2,4)*aa(3,2)*aa(4,1)
     &      - aa(1,4)*aa(2,2)*aa(3,3)*aa(4,1) + aa(1,2)*aa(2,4)*aa(3,3)*aa(4,1)
     &      + aa(1,3)*aa(2,2)*aa(3,4)*aa(4,1) - aa(1,2)*aa(2,3)*aa(3,4)*aa(4,1)
     &      - aa(1,4)*aa(2,3)*aa(3,1)*aa(4,2) + aa(1,3)*aa(2,4)*aa(3,1)*aa(4,2)
     &      + aa(1,4)*aa(2,1)*aa(3,3)*aa(4,2) - aa(1,1)*aa(2,4)*aa(3,3)*aa(4,2)
     &      - aa(1,3)*aa(2,1)*aa(3,4)*aa(4,2) + aa(1,1)*aa(2,3)*aa(3,4)*aa(4,2)
     &      + aa(1,4)*aa(2,2)*aa(3,1)*aa(4,3) - aa(1,2)*aa(2,4)*aa(3,1)*aa(4,3)
     &      - aa(1,4)*aa(2,1)*aa(3,2)*aa(4,3) + aa(1,1)*aa(2,4)*aa(3,2)*aa(4,3)
     &      + aa(1,2)*aa(2,1)*aa(3,4)*aa(4,3) - aa(1,1)*aa(2,2)*aa(3,4)*aa(4,3)
     &      - aa(1,3)*aa(2,2)*aa(3,1)*aa(4,4) + aa(1,2)*aa(2,3)*aa(3,1)*aa(4,4)
     &      + aa(1,3)*aa(2,1)*aa(3,2)*aa(4,4) - aa(1,1)*aa(2,3)*aa(3,2)*aa(4,4)
     &      - aa(1,2)*aa(2,1)*aa(3,3)*aa(4,4) + aa(1,1)*aa(2,2)*aa(3,3)*aa(4,4)

      det = 1.0d0/det
      aa = cc*det

      End subroutine

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c *** A subroutine to create cells from arrays adhering to a Fluent case file
c *** format. The routine simply loops over cells and invokes special cell
c *** construction functions. It fills arrays that are returned to the calling
c *** function/routine.
c ***
c *** Created:       IN <nompelis@nobelware.com> 20171027
c *** Last modified: IN <nompelis@nobelware.com> 20191223
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      Subroutine make_geometry(nz,nno,nel,nge,net,nfa,
     &                         ifn,ife,izone,x,ief,ien,xce,vol,xcf,s,ssx)
      Use inMesh_Elements
      Implicit None
      Integer(kind=8),intent(IN) :: nno,nel,nfa,nge,net
      Integer(kind=4),intent(IN) :: nz
      Integer(kind=8),intent(IN) :: ifn(0:4,nfa)
      Integer(kind=8),intent(INOUT) :: ife(2,nfa)
      Integer(kind=8),intent(IN) :: izone(5,nz)
      Integer(kind=8),intent(IN) :: ief(0:6,nel)
      Integer(kind=8),intent(IN) :: ien(0:8,nel)
      Real*8,intent(IN) :: x(3,nno)
      Real*8,intent(OUT) :: xce(3,net),vol(nel),xcf(3,nfa), s(nfa),ssx(3,3,nfa)
      Integer(kind=8) :: ntri,nqua
      Integer(kind=4) :: ierr

      Integer(kind=8) :: i,j,k,ii
      Real*8 :: dx1(3),dx2(3), xn(3),xl,yn(3),yl
      Logical, parameter :: itest = .TRUE.


      PRINT*,'Creating geometry'
      xce(:,:) = 0.0d0
      xcf(:,:) = 0.0d0
      s(:) = 0.0d0
      ssx(:,:,:) = 0.0d0


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
            call inMesh_Tetrahedron_VolumeFromVertices(
     &                x(1, ien(1,i) ),
     &                x(1, ien(2,i) ),
     &                x(1, ien(3,i) ),
     &                x(1, ien(4,i) ), vol(i) )
         else if( ntri .eq. 4 .AND. nqua .eq. 1 ) then
            call inMesh_Pyramid_CentroidFromVertices(
     &                x(1, ien(1,i) ),
     &                x(1, ien(2,i) ),
     &                x(1, ien(3,i) ),
     &                x(1, ien(4,i) ),
     &                x(1, ien(5,i) ), xce(1,i), vol(i) )
         else if( ntri .eq. 2 .AND. nqua .eq. 3 ) then
            call inMesh_Wedge_CentroidFromVertices(
     &                x(1, ien(1,i) ),
     &                x(1, ien(2,i) ),
     &                x(1, ien(3,i) ),
     &                x(1, ien(4,i) ),
     &                x(1, ien(5,i) ),
     &                x(1, ien(6,i) ), xce(1,i), vol(i) )
         else if( ntri .eq. 0 .AND. nqua .eq. 6 ) then
            call inMesh_Brick_CentroidFromVertices(
     &                x(1, ien(1,i) ),
     &                x(1, ien(2,i) ),
     &                x(1, ien(3,i) ),
     &                x(1, ien(4,i) ),
     &                x(1, ien(5,i) ),
     &                x(1, ien(6,i) ),
     &                x(1, ien(7,i) ),
     &                x(1, ien(8,i) ), xce(1,i), vol(i) )
         else
            PRINT*,'Problematic element!'
            PRINT*,'ntri=',ntri,'nqua=',nqua
            STOP
         endif
#ifdef _DEBUG2_
         WRITE(*,300) 'Centroid:',xce(:,i)
         WRITE(*,300) 'Volume:  ',vol(i)
 300     Format(a9,3(1x,e20.13))
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

c--- form face centroids, face basis and face area
      do j = 1,nfa
         if( ifn(0,j) .eq. 3 ) then
            xcf(:,j) = (x(:, ifn(1,j) ) +
     &                  x(:, ifn(2,j) ) +
     &                  x(:, ifn(3,j) ))/3.0d0

            dx1(:) = x(:, ifn(2,j)) - x(:, ifn(1,j))
            dx2(:) = x(:, ifn(3,j)) - x(:, ifn(1,j))
            xn(1) =  dx1(2)*dx2(3) -dx2(2)*dx1(3)
            xn(2) = -dx1(1)*dx2(3) +dx2(1)*dx1(3)
            xn(3) =  dx1(1)*dx2(2) -dx2(1)*dx1(2)
            xl = sqrt(xn(1)**2 + xn(2)**2 + xn(3)**2)
            s(j) = 0.5d0*xl
            xl = 1.0d0/xl
            ssx(:,1,j) = xn(:)*xl
            xl = 1.0d0/sqrt(dx1(1)**2 + dx1(2)**2 + dx1(3)**2)
            ssx(:,2,j) = dx1(:)*xl
            xn(1) =  ssx(2,1,j)*ssx(3,2,j) -ssx(2,2,j)*ssx(3,1,j)
            xn(2) = -ssx(1,1,j)*ssx(3,2,j) +ssx(1,2,j)*ssx(3,1,j)
            xn(3) =  ssx(1,1,j)*ssx(2,2,j) -ssx(1,2,j)*ssx(2,1,j)
            xl = 1.0d0/sqrt(xn(1)**2 + xn(2)**2 + xn(3)**2)
            ssx(:,3,j) = xn(:)*xl

         else if( ifn(0,j) .eq. 4 ) then
            xcf(:,j) = (x(:, ifn(1,j) )*2.0d0 +
     &                  x(:, ifn(2,j) ) +
     &                  x(:, ifn(3,j) )*2.0d0 +
     &                  x(:, ifn(4,j) ))/6.0d0

            dx1(:) = x(:, ifn(2,j)) - x(:, ifn(1,j))
            dx2(:) = x(:, ifn(3,j)) - x(:, ifn(1,j))
            xn(1) =  dx1(2)*dx2(3) -dx2(2)*dx1(3)
            xn(2) = -dx1(1)*dx2(3) +dx2(1)*dx1(3)
            xn(3) =  dx1(1)*dx2(2) -dx2(1)*dx1(2)
            xl = sqrt(xn(1)**2 + xn(2)**2 + xn(3)**2)
            s(j) = 0.5d0*xl

            xl = 1.0d0/sqrt(dx2(1)**2 + dx2(2)**2 + dx2(3)**2)
            ssx(:,2,j) = dx2(:)*xl

            dx1(:) = dx2(:)
            dx2(:) = x(:, ifn(4,j)) - x(:, ifn(1,j))
            yn(1) = +dx1(2)*dx2(3) -dx2(2)*dx1(3)
            yn(2) = -dx1(1)*dx2(3) +dx2(1)*dx1(3)
            yn(3) = +dx1(1)*dx2(2) -dx2(1)*dx1(2)
            yl = sqrt(yn(1)**2 + yn(2)**2 + yn(3)**2)
            s(j) = s(j) + 0.5d0*yl

            xn(:) = xn(:) + yn(:)
            xl = 1.0d0/sqrt(xn(1)**2 + xn(2)**2 + xn(3)**2)
            ssx(:,1,j) = xn(:)*xl
            xn(1) =  ssx(2,1,j)*ssx(3,2,j) -ssx(2,2,j)*ssx(3,1,j)
            xn(2) = -ssx(1,1,j)*ssx(3,2,j) +ssx(1,2,j)*ssx(3,1,j)
            xn(3) =  ssx(1,1,j)*ssx(2,2,j) -ssx(1,2,j)*ssx(2,1,j)
            xl = 1.0d0/sqrt(xn(1)**2 + xn(2)**2 + xn(3)**2)
            ssx(:,3,j) = xn(:)*xl

         else
            PRINT*,'Problematic face!'
            PRINT*,'ifn(0,j)=',ifn(0,j)
            STOP
         endif


#ifdef _DEBUG2_
         WRITE(*,200) 'Face centroid:',xcf(:,i)
 200     Format(a14,3(1x,e20.13))
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

#ifdef _DEBUG_
      open(unit=10,file='FACE_BASIS.dat',status='unknown')
      write(10,*) 'variables = x y z u v w'
      write(10,*) 'zone T="Face basis"'
      do j = 1,nfa
         write(10,100) xcf(:,j),ssx(:,1,j)
         write(10,100) xcf(:,j),ssx(:,2,j)
         write(10,100) xcf(:,j),ssx(:,3,j)
      enddo
 100  Format(6(1x,e16.9))
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
c *** Last modified: IN <nompelis@nobelware.com> 20191216
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      Subroutine make_cells(nz,nno,nel,nfa,ifn,ife,izone,x,ief,ien)
      Use inMesh_Elements
      Implicit None
      Integer(kind=8),intent(IN) :: nno,nel,nfa
      Integer(kind=4),intent(IN) :: nz
      Integer(kind=8),intent(IN),dimension(0:4,nfa) :: ifn
      Integer(kind=8),intent(IN),dimension(2,nfa) :: ife
      Integer(kind=8),intent(IN),dimension(5,nz) :: izone
      Real*8,dimension(3,nno),intent(IN) :: x
      Integer(kind=8),dimension(0:6,nel),intent(OUT) :: ief
      Integer(kind=8),dimension(0:8,nel),intent(OUT) :: ien
      Integer(kind=4) :: ierr

      Integer(kind=8) :: i,j,k
      Integer(kind=8) :: ntri,nqua
      Integer(kind=8) :: ieno(0:8),iefo(0:6)
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
         iefo(:) = 0

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
c--- modify faces-of-element and nodes-of-element array
            ien(:,i) = ieno(:)
            ief(:,i) = iefo(:)
c--- modify nodes to plot as a "brick" element
            ieno(5:8) = ieno(4)
            ieno(4) = ieno(3)
         else if( ntri .eq. 4 .AND. nqua .eq. 1 ) then
            call inMesh_Elements_FormPyramidFromFaces(
     &                  nfa, ifn, ife, i, ief(0,i),
     &                  ieno, iefo, ierr )
c--- modify faces-of-element and nodes-of-element array
            ien(:,i) = ieno(:)
            ief(:,i) = iefo(:)
c--- modify nodes to plot as a "brick" element
            ieno(6:8) = ieno(5)
         else if( ntri .eq. 3 .AND. nqua .eq. 2 ) then
            call inMesh_Elements_FormWedgeFromFaces(
     &                  nfa, ifn, ife, i, ief(0,i),
     &                  ieno, iefo, ierr )
c--- modify faces-of-element and nodes-of-element array
            ien(:,i) = ieno(:)
            ief(:,i) = iefo(:)
c--- modify nodes to plot as a "brick" element
            ieno(7:8) = ieno(6)
            ieno(6) = ieno(5)
            ieno(5) = ieno(4)
            ieno(4) = ieno(3)
         else if( ntri .eq. 0 .AND. nqua .eq. 6 ) then
            call inMesh_Elements_FormBrickFromFaces(
     &                  nfa, ifn, ife, i, ief(0,i),
     &                  ieno, iefo, ierr )
c--- modify faces-of-element and nodes-of-element array
            ien(:,i) = ieno(:)
            ief(:,i) = iefo(:)
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

