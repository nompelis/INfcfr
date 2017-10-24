c/**************************************************************************
c Copyright (c) 2011-2017, Ioannis Nompelis
c All rights reserved.
c
c Redistribution and use in source and binary forms, with or without any
c modification, are permitted provided that the following conditions are met:
c 1. Redistribution of source code must retain the above copyright
c    notice, this list of conditions and the following disclaimer.
c 2. Redistribution in binary form must reproduce the above copyright
c    notice, this list of conditions and the following disclaimer in the
c    documentation and/or other materials provided with the distribution.
c 3. All advertising materials mentioning features or use of this software
c    must display the following acknowledgement:
c    "This product includes software developed by Ioannis Nompelis."
c 4. Neither the name of Ioannis Nompelis and his partners/affiliates nor the
c    names of other contributors may be used to endorse or promote products
c    derived from this software without specific prior written permission.
c 5. Redistribution or use of source code and binary forms for profit must
c    have written permission of the copyright holder.
c 
c THIS SOFTWARE IS PROVIDED BY IOANNIS NOMPELIS ''AS IS'' AND ANY
c EXPRESSED OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
c WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
c DISCLAIMED. IN NO EVENT SHALL IOANNIS NOMPELIS BE LIABLE FOR ANY
c DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
c (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
c LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
c ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
c (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
c SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
c **************************************************************************/

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c *** Module to store standard mesh elements
c *** Created:       IN <nompelis@nobelware.com> 20171006 
c *** Last modified: IN <nompelis@nobelware.com> 20171024
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       Module inMesh_Elements
c      Use iso_c_binding
c      Use ISO_FORTRAN_ENV

       Type inMesh_Tetrahedron_t
          !--- Global face numbers
          Integer(kind=8) :: f1,f2,f3,f4

          !--- Global node numbers
          Integer(kind=8) :: n1,n2,n3,n4

          !--- Convention of the faces
          !--- Any face of the tetrahedron's 1-4 can be any triangle and it
          !--- can have any of three positive or negative alignments. Positive
          !--- numbers imply outward pointing normals. Permutations of 1,2,3
          !--- imply deviations from the "cannonical" alignment.
          Integer(kind=2) :: fc(4)
       End Type

       Type inMesh_Pyramid_t
          !--- Global face numbers
          Integer(kind=8) :: f1,f2,f3,f4,f5

          !--- Global node numbers
          Integer(kind=8) :: n1,n2,n3,n4,n5

          !--- Convention of the faces
          !--- The base of the pyramid's can only be a quadrilateral (4 nodes)
          !--- and its face convention is (by construction) -1 or +1. The
          !--- remaining four faces 2-5 are triangles and can have any of three
          !--- possible alignments. Positive numbers imply outward-pointing
          !--- normals. permutations of 1,2,3 imply deviations from the
          !--- "cannonical" alignment.
          Integer(kind=2) :: fc(5)
       End Type

       Type inMesh_Wedge_t
          !--- Global face numbers
          Integer(kind=8) :: f1,f2,f3,f4,f5

          !--- Global node numbers
          Integer(kind=8) :: n1,n2,n3,n4,n5,n6

          !--- Convention of the faces
          !--- The base of the wegde is the first triangle encountered in the
          !--- input list of surface elements and its face convention is (by
          !--- construction) -1 or +1. The remaining faces can have any of
          !--- three possible alignemnts for the top face and four possible
          !--- alignments for the lateral quadrilaterals. Positive numbers imply
          !--- outward-pointing normals. permutations of 1,2,3 or 4 imply
          !--- deviations from the "cannonical" alignment.
          Integer(kind=2) :: fc(5)
       End Type

       Type inMesh_Brick_t
          !--- Global face numbers
          Integer(kind=8) :: f1,f2,f3,f4,f5,f6

          !--- Global node numbers
          Integer(kind=8) :: n1,n2,n3,n4,n5,n6,n7,n8

          !--- Convention of the faces
          !--- The base of the brick is the first face encountered in the list
          !--- and its face convention is (by construction) -1 or +1.
          !--- The remaining faces can have any of four possible alignemnts.
          !--- Positive numbers imply  outward-pointing normals. permutations of
          !--- 1,2,3,4 imply deviations from the "cannonical" alignment.
          !--- Face numbers for a brick are opposing eachother for sequential
          !--- odd and even numbers (i.e. 1 is opposite 2). Faces 1 and 2 are
          !--- the base and top, face 3 has nodes 1 & 2, face 5 has nodes 1 & 4.
          Integer(kind=2) :: fc(6)
       End Type


       CONTAINS

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c *** Routine to form a tetrahedron from four triangles
c *** This routine assumes that what is passed in "ifn" are _valid_ triangles
c *** (Definitions on the validity of triangles are found in my notes.)
c *** Created:       IN <nompelis@nobelware.com> 20171006 
c *** Last modified: IN <nompelis@nobelware.com> 20171016
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       Subroutine inMesh_Elements_FormTetrahedronFromFaces(
     &                  nfa, ifn, ife, i, iefi,
     &                  ieno, iefo, ierr )
       Implicit None
       Integer(kind=4), intent(OUT) :: ierr
       Integer(kind=8), intent(IN) :: nfa,i
       Integer(kind=8), intent(IN) :: ifn(0:4,nfa),ife(2,nfa)
       Integer(kind=8), intent(IN) :: iefi(0:4)
c --- Variables to return to what would be a numerical solver that is aware
c --- of the conventions used.
       Integer(kind=8), intent(OUT) :: ieno(0:8),iefo(0:4)
c --- Internal variable for this example
       Type(inMesh_Tetrahedron_t) :: tet
       Integer(kind=8) :: j1,j2,j3,j4,jj
       Integer(kind=2) :: k,ik,kk


#ifdef _DEBUG_
       PRINT*,'Constructing a tetrahedron; number',i
       PRINT*,'Faces:', iefi(1:4)
#endif

c --- Ssanity checks
       if( iefi(0) .ne. 4 )then
          PRINT*,'Not a tetrahedron (does not have 4 faces)'
          ierr = 1
          return
       endif

       if( (iefi(1) .le. 0 .OR. iefi(1) .gt. nfa) .OR.
     &     (iefi(2) .le. 0 .OR. iefi(2) .gt. nfa) .OR.
     &     (iefi(3) .le. 0 .OR. iefi(3) .gt. nfa) .OR.
     &     (iefi(4) .le. 0 .OR. iefi(4) .gt. nfa) ) then
          PRINT*,'Face numbers out of bounds'
          ierr = 2
          return
       endif


       j1 = -1
       j2 = -1
       j3 = -1
       j4 = -1

       tet%n1 = 0
       tet%n2 = 0
       tet%n3 = 0
       tet%n4 = 0

       tet%f1 = 0
       tet%f2 = 0
       tet%f3 = 0
       tet%f4 = 0

       tet%fc(:) = 0

c --- Select the first face of the tet. to be the first one in the list
c --- Copy node numbers and face number
c --- By convention (as in Fluent case files), right is where the thumb points
c --- when listing nodes by a right-hand rule.
c --- By a starting choice we set the face-convention flag to +1 or -1
       j1 = iefi(1)
       tet%f1 = j1               ! set 1st face
       if( ife(2,j1) .eq. i ) then      ! triangle's "right" face is the tetr.
#ifdef _DEBUG_
          PRINT*,'Tetrahedron is on the right of face 1'
#endif
          tet%n1 = ifn(1, j1 )   ! set 1st node
          tet%n2 = ifn(2, j1 )   ! set 2nd node
          tet%n3 = ifn(3, j1 )   ! set 3rd node
          tet%fc(1) = -1         ! edge number and direction (sign)
       else
#ifdef _DEBUG_
          PRINT*,'Tetrahedron is on the left of face 1'
#endif
          tet%n1 = ifn(2, j1 )   ! set 1st node is the 2nd one
          tet%n2 = ifn(1, j1 )   ! set 2nd node is the 1st one
          tet%n3 = ifn(3, j1 )   ! set 3rd node is the same
          tet%fc(1) = +1         ! edge number and direction (sign)
       endif

c --- Find face 2; look for 2 matching nodes for n1,n2 of the tetrahedron
c --- Determine orientation of face 2 (using ife); set conventions
c --- Also set 4th node of the tetrahedron
#ifdef _DEBUG_
       PRINT*,'Will match nodes:',tet%n1,tet%n2
#endif
       j2 = -1                   ! undefined number
       do k = 2,4                ! loop over remaining faces; 1st excluded
          jj = iefi(k)           ! face number
#ifdef _DEBUG_
       PRINT*,'Trial face:',k,jj
#endif
          ik = 0                 ! number of nodes matched for this trial face
          do kk = 1,3            ! loop over trial face's nodes (always 3)
             if( ifn(kk,jj) .eq. tet%n1 ) ik = ik + 1
             if( ifn(kk,jj) .eq. tet%n2 ) ik = ik + 1
#ifdef _DEBUG_
       PRINT*,'Node:',kk,ifn(kk,jj)
#endif
          enddo
#ifdef _DEBUG_
       PRINT*,'After testing ik=',ik
#endif
          if( ik .eq. 2 ) then
             j2 = jj
          endif
       enddo
#ifdef _DEBUG_
       PRINT*,'Identified face 2 as:',j2
#endif
       do kk = 1,3            ! loop over 2nd face's nodes (always 3)
          if( ifn(kk,j2) .ne. tet%n1 .AND.  ifn(kk,j2) .ne. tet%n2 ) then
             tet%n4 = ifn(kk,j2)
          endif
       enddo
#ifdef _DEBUG_
       PRINT*,' Assigned fourth node of tet:',tet%n4
       PRINT*,' Left/right elems:',ife(:,j2)
#endif

       if( ife(1,j2) .eq. i ) then    ! tetrahedron is left
          tet%fc(2) = +1              ! preliminary edge direction (sign)
          if( ifn(3,j2) .eq. tet%n1 ) then
             tet%fc(2) = +2           ! correct edge direction (sign)
          endif
          if( ifn(2,j2) .eq. tet%n1 ) then
             tet%fc(2) = +3           ! correct edge direction (sign)
          endif
       else                           ! tetrahedron is right
          tet%fc(2) = -1              ! preliminary edge direction (sign)
          if( ifn(1,j2) .eq. tet%n1 ) then
             tet%fc(2) = -2           ! correct edge direction (sign)
          endif
          if( ifn(3,j2) .eq. tet%n1 ) then
             tet%fc(2) = -3           ! correct edge direction (sign)
          endif
       endif
#ifdef _DEBUG_
       PRINT*,'Identified face 2 convention:',tet%fc(2)
#endif

c --- Find face 3; look for 2 matching nodes for n2,n3 of the tetrahedron
c --- Determine orientation of face 3 (using ife); set conventions
#ifdef _DEBUG_
       PRINT*,'Will match nodes:',tet%n2,tet%n3
#endif
       j3 = -1                   ! undefined number
       do k = 2,4                ! loop over remaining faces; 1st excluded
          jj = iefi(k)           ! face number
#ifdef _DEBUG_
       PRINT*,'Trial face:',k,jj
#endif
          ik = 0                 ! number of nodes matched for this trial face
          do kk = 1,3            ! loop over trial face's nodes (always 3)
             if( ifn(kk,jj) .eq. tet%n2 ) ik = ik + 1
             if( ifn(kk,jj) .eq. tet%n3 ) ik = ik + 1
#ifdef _DEBUG_
       PRINT*,'Node:',kk,ifn(kk,jj)
#endif
          enddo
#ifdef _DEBUG_
       PRINT*,'After testing ik=',ik
#endif
          if( ik .eq. 2 ) then
             j3 = jj
          endif
       enddo
#ifdef _DEBUG_
       PRINT*,'Identified face 3 as:',j3
       PRINT*,' Left/right elems:',ife(:,j3)
#endif

       if( ife(1,j3) .eq. i ) then    ! tetrahedron is left
          tet%fc(3) = +1              ! preliminary edge direction (sign)
          if( ifn(3,j3) .eq. tet%n2 ) then
             tet%fc(3) = +2           ! correct edge direction (sign)
          endif
          if( ifn(2,j3) .eq. tet%n2 ) then
             tet%fc(3) = +3           ! correct edge direction (sign)
          endif
       else                           ! tetrahedron is right
          tet%fc(3) = -1              ! preliminary edge direction (sign)
          if( ifn(1,j3) .eq. tet%n2 ) then
             tet%fc(3) = -2           ! correct edge direction (sign)
          endif
          if( ifn(3,j3) .eq. tet%n2 ) then
             tet%fc(3) = -3           ! correct edge direction (sign)
          endif
       endif
#ifdef _DEBUG_
       PRINT*,'Identified face 3 convention:',tet%fc(3)
#endif

c --- Assign face 4; it is the remaining (un-asigned) face of the tetrahedron
c --- Determine orientation of face 4 (using ife); set conventions
       do k = 2,4                ! loop over remaining faces; 1st excluded
          jj = iefi(k)           ! face number
          if( j1.ne.jj .AND. j2.ne.jj .AND. j3.ne.jj ) j4 = jj
       enddo
#ifdef _DEBUG_
       PRINT*,'Identified face 4 as:',j4
#endif

       if( ife(1,j4) .eq. i ) then    ! tetrahedron is left
          tet%fc(4) = +1              ! preliminary edge direction (sign)
          if( ifn(3,j4) .eq. tet%n3 ) then
             tet%fc(4) = +2           ! correct edge direction (sign)
          endif
          if( ifn(2,j4) .eq. tet%n3 ) then
             tet%fc(4) = +3           ! correct edge direction (sign)
          endif
       else                           ! tetrahedron is right
          tet%fc(4) = -1              ! preliminary edge direction (sign)
          if( ifn(1,j4) .eq. tet%n3 ) then
             tet%fc(4) = -2           ! correct edge direction (sign)
          endif
          if( ifn(3,j4) .eq. tet%n3 ) then
             tet%fc(4) = -3           ! correct edge direction (sign)
          endif
       endif
#ifdef _DEBUG_
       PRINT*,'Identified face 4 convention:',tet%fc(4)
#endif

c --- assign structure members
       tet%f1 = j1
       tet%f2 = j2
       tet%f3 = j3
       tet%f4 = j4

c --- Assign returned structures
       ieno(0) = 4
       ieno(1) = tet%n1
       ieno(2) = tet%n2
       ieno(3) = tet%n3
       ieno(4) = tet%n4
       iefo(0) = 4
       iefo(1) = j1
       iefo(2) = j2
       iefo(3) = j3
       iefo(4) = j4

       End subroutine

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c *** Routine to form a pyramid cell element from four triangles and a quad.
c *** This routine assumes that what is passed in "ifn" are _valid_ surf. elems.
c *** (Definitions on the validity of surface elements are found in my notes.)
c *** Created:       IN <nompelis@nobelware.com> 20171014
c *** Last modified: IN <nompelis@nobelware.com> 20171016
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       Subroutine inMesh_Elements_FormPyramidFromFaces(
     &                  nfa, ifn, ife, i, iefi,
     &                  ieno, iefo, ierr )
       Implicit None
       Integer(kind=4), intent(OUT) :: ierr
       Integer(kind=8), intent(IN) :: nfa,i
       Integer(kind=8), intent(IN) :: ifn(0:4,nfa),ife(2,nfa)
       Integer(kind=8), intent(IN) :: iefi(0:5)
c --- Variables to return to what would be a numerical solver that is aware
c --- of the conventions used.
       Integer(kind=8), intent(OUT) :: ieno(0:8),iefo(0:5)
c --- Internal variable for this example
       Type(inMesh_Pyramid_t) :: pyr
       Integer(kind=8) :: j1,j2,j3,j4,j5,jj
       Integer(kind=2) :: k,ik,kk


#ifdef _DEBUG_
       PRINT*,'Constructing a pyramid; number',i
       PRINT*,'Faces:', iefi(1:5)
#endif

c --- Ssanity checks
       if( iefi(0) .ne. 5 )then
          PRINT*,'Not a pyramid (does not have 5 faces)'
          ierr = 1
          return
       endif

       if( (iefi(1) .le. 0 .OR. iefi(1) .gt. nfa) .OR.
     &     (iefi(2) .le. 0 .OR. iefi(2) .gt. nfa) .OR.
     &     (iefi(3) .le. 0 .OR. iefi(3) .gt. nfa) .OR.
     &     (iefi(4) .le. 0 .OR. iefi(4) .gt. nfa) .OR.
     &     (iefi(5) .le. 0 .OR. iefi(5) .gt. nfa) ) then
          PRINT*,'Face numbers out of bounds'
          ierr = 2
          return
       endif


       j1 = -1
       j2 = -1
       j3 = -1
       j4 = -1
       j5 = -1

       pyr%n1 = 0
       pyr%n2 = 0
       pyr%n3 = 0
       pyr%n4 = 0
       pyr%n5 = 0

       pyr%f1 = 0
       pyr%f2 = 0
       pyr%f3 = 0
       pyr%f4 = 0
       pyr%f5 = 0

       pyr%fc(:) = 0

c --- Select the first face of the pyramid to be the 4-node one
c --- Copy node numbers and face number
c --- By convention (as in Fluent case files), right is where the thumb points
c --- when listing nodes by a right-hand rule.
c --- By a starting choice we set the face-convention flag to +1 or -1
       do jj = 1,iefi(0)
          if( ifn(0, iefi(jj) ) .eq. 4 ) j1 = jj
       enddo
#ifdef _DEBUG_
       PRINT*,'First face is number ',j1
#endif
       pyr%f1 = j1               ! set 1st face
       if( ife(2,j1) .eq. i ) then      ! quad's "right" face is the pyramid
#ifdef _DEBUG_
          PRINT*,'Pyramid is on the right of face 1'
#endif
          pyr%n1 = ifn(1, j1 )   ! set 1st node
          pyr%n2 = ifn(2, j1 )   ! set 2nd node
          pyr%n3 = ifn(3, j1 )   ! set 3rd node
          pyr%n4 = ifn(4, j1 )   ! set 4th node
          pyr%fc(1) = -1         ! edge number and direction (sign)
       else
#ifdef _DEBUG_
          PRINT*,'Pyramid is on the left of face 1'
#endif
          pyr%n1 = ifn(2, j1 )   ! set 1st node
          pyr%n2 = ifn(1, j1 )   ! set 2nd node
          pyr%n3 = ifn(4, j1 )   ! set 3rd node
          pyr%n4 = ifn(3, j1 )   ! set 4th node
          pyr%fc(1) = +1         ! edge number and direction (sign)
       endif
#ifdef _DEBUG_
       PRINT*,'First face nodes:',pyr%n1,pyr%n2,pyr%n3,pyr%n4
#endif

c --- Find face 2; look for 2 matching nodes for n1,n2 of the pyramid
c --- Determine orientation of face 2 (using ife); set conventions
c --- Also set 5th node of the pyramid
#ifdef _DEBUG_
       PRINT*,'Will match nodes:',pyr%n1,pyr%n2
#endif
       j2 = -1                   ! undefined number
       do k = 1,5                ! loop over all faces
          jj = iefi(k)           ! face number

          if( jj .ne. j1 ) then  ! exclude the first (base) face
#ifdef _DEBUG_
       PRINT*,'Trial face:',k,jj
#endif
             ik = 0              ! number of nodes matched for this trial face
             do kk = 1,3         ! loop over trial face's nodes (always 3)
                if( ifn(kk,jj) .eq. pyr%n1 ) ik = ik + 1
                if( ifn(kk,jj) .eq. pyr%n2 ) ik = ik + 1
#ifdef _DEBUG_
       PRINT*,'Node:',kk,ifn(kk,jj)
#endif
             enddo
#ifdef _DEBUG_
       PRINT*,'After testing ik=',ik
#endif
             if( ik .eq. 2 ) then
                j2 = jj
             endif

          endif
       enddo
#ifdef _DEBUG_
       PRINT*,'Identified face 2 as:',j2
#endif

       do kk = 1,3            ! loop over 2nd face's nodes (always 3)
          if( ifn(kk,j2) .ne. pyr%n1 .AND.  ifn(kk,j2) .ne. pyr%n2 ) then
             pyr%n5 = ifn(kk,j2)
          endif
       enddo
#ifdef _DEBUG_
       PRINT*,' Assigned fifth node of pyramid:',pyr%n5
       PRINT*,' Left/right elems:',ife(:,j2)
#endif
       if( ife(1,j2) .eq. i ) then    ! pyramid is left
          pyr%fc(2) = +1              ! preliminary edge direction (sign)
          if( ifn(3,j2) .eq. pyr%n1 ) then
             pyr%fc(2) = +2           ! correct edge direction (sign)
          endif
          if( ifn(2,j2) .eq. pyr%n1 ) then
             pyr%fc(2) = +3           ! correct edge direction (sign)
          endif
       else                           ! pyramid is right
          pyr%fc(2) = -1              ! preliminary edge direction (sign)
          if( ifn(1,j2) .eq. pyr%n1 ) then
             pyr%fc(2) = -2           ! correct edge direction (sign)
          endif
          if( ifn(3,j2) .eq. pyr%n1 ) then
             pyr%fc(2) = -3           ! correct edge direction (sign)
          endif
       endif
#ifdef _DEBUG_
       PRINT*,'Identified face 2 convention:',pyr%fc(2)
#endif

c --- Find face 3; look for 2 matching nodes for n2,n3 of the pyramid
c --- Determine orientation of face 3 (using ife); set conventions
#ifdef _DEBUG_
       PRINT*,'Will match nodes:',pyr%n2,pyr%n3
#endif
       j3 = -1                   ! undefined number
       do k = 1,5                ! loop over all faces
          jj = iefi(k)           ! face number

          if( jj .ne. j1 ) then  ! exclude the first (base) face
#ifdef _DEBUG_
       PRINT*,'Trial face:',k,jj
#endif
             ik = 0              ! number of nodes matched for this trial face
             do kk = 1,3         ! loop over trial face's nodes (always 3)
                if( ifn(kk,jj) .eq. pyr%n2 ) ik = ik + 1
                if( ifn(kk,jj) .eq. pyr%n3 ) ik = ik + 1
#ifdef _DEBUG_
       PRINT*,'Node:',kk,ifn(kk,jj)
#endif
             enddo
#ifdef _DEBUG_
       PRINT*,'After testing ik=',ik
#endif
             if( ik .eq. 2 ) then
                j3 = jj
             endif
          endif
       enddo
#ifdef _DEBUG_
       PRINT*,'Identified face 3 as:',j3
       PRINT*,' Left/right elems:',ife(:,j3)
#endif

       if( ife(1,j3) .eq. i ) then    ! pyramid is left
          pyr%fc(3) = +1              ! preliminary edge direction (sign)
          if( ifn(3,j3) .eq. pyr%n2 ) then
             pyr%fc(3) = +2           ! correct edge direction (sign)
          endif
          if( ifn(2,j3) .eq. pyr%n2 ) then
             pyr%fc(3) = +3           ! correct edge direction (sign)
          endif
       else                           ! pyramid is right
          pyr%fc(3) = -1              ! preliminary edge direction (sign)
          if( ifn(1,j3) .eq. pyr%n2 ) then
             pyr%fc(3) = -2           ! correct edge direction (sign)
          endif
          if( ifn(3,j3) .eq. pyr%n2 ) then
             pyr%fc(3) = -3           ! correct edge direction (sign)
          endif
       endif
#ifdef _DEBUG_
       PRINT*,'Identified face 3 convention:',pyr%fc(3)
#endif

c --- Find face 4; look for 2 matching nodes for n3,n4 of the pyramid
c --- Determine orientation of face 4 (using ife); set conventions
#ifdef _DEBUG_
       PRINT*,'Will match nodes:',pyr%n3,pyr%n4
#endif
       j4 = -1                   ! undefined number
       do k = 1,5                ! loop over all faces
          jj = iefi(k)           ! face number

          if( jj .ne. j1 ) then  ! exclude the first (base) face
#ifdef _DEBUG_
       PRINT*,'Trial face:',k,jj
#endif
             ik = 0              ! number of nodes matched for this trial face
             do kk = 1,3         ! loop over trial face's nodes (always 3)
                if( ifn(kk,jj) .eq. pyr%n3 ) ik = ik + 1
                if( ifn(kk,jj) .eq. pyr%n4 ) ik = ik + 1
#ifdef _DEBUG_
       PRINT*,'Node:',kk,ifn(kk,jj)
#endif
             enddo
#ifdef _DEBUG_
       PRINT*,'After testing ik=',ik
#endif
             if( ik .eq. 2 ) then
                j4 = jj
             endif
          endif
       enddo
#ifdef _DEBUG_
       PRINT*,'Identified face 4 as:',j4
       PRINT*,' Left/right elems:',ife(:,j4)
#endif

       if( ife(1,j4) .eq. i ) then    ! pyramid is left
          pyr%fc(4) = +1              ! preliminary edge direction (sign)
          if( ifn(3,j4) .eq. pyr%n3 ) then
             pyr%fc(4) = +2           ! correct edge direction (sign)
          endif
          if( ifn(2,j4) .eq. pyr%n3 ) then
             pyr%fc(4) = +3           ! correct edge direction (sign)
          endif
       else                           ! pyramid is right
          pyr%fc(4) = -1              ! preliminary edge direction (sign)
          if( ifn(1,j4) .eq. pyr%n3 ) then
             pyr%fc(4) = -2           ! correct edge direction (sign)
          endif
          if( ifn(3,j4) .eq. pyr%n3 ) then
             pyr%fc(4) = -3           ! correct edge direction (sign)
          endif
       endif
#ifdef _DEBUG_
       PRINT*,'Identified face 4 convention:',pyr%fc(4)
#endif

c --- Find face 5; look for 2 matching nodes for n4,n1 of the pyramid
c --- Determine orientation of face 5 (using ife); set conventions
#ifdef _DEBUG_
       PRINT*,'Will match nodes:',pyr%n4,pyr%n1
#endif
       j5 = -1                   ! undefined number
       do k = 1,5                ! loop over all faces
          jj = iefi(k)           ! face number

          if( jj .ne. j1 ) then  ! exclude the first (base) face
#ifdef _DEBUG_
       PRINT*,'Trial face:',k,jj
#endif
             ik = 0              ! number of nodes matched for this trial face
             do kk = 1,3         ! loop over trial face's nodes (always 3)
                if( ifn(kk,jj) .eq. pyr%n4 ) ik = ik + 1
                if( ifn(kk,jj) .eq. pyr%n1 ) ik = ik + 1
#ifdef _DEBUG_
       PRINT*,'Node:',kk,ifn(kk,jj)
#endif
             enddo
#ifdef _DEBUG_
       PRINT*,'After testing ik=',ik
#endif
             if( ik .eq. 2 ) then
                j5 = jj
             endif
          endif
       enddo
#ifdef _DEBUG_
       PRINT*,'Identified face 5 as:',j5
       PRINT*,' Left/right elems:',ife(:,j5)
#endif

       if( ife(1,j5) .eq. i ) then    ! pyramid is left
          pyr%fc(5) = +1              ! preliminary edge direction (sign)
          if( ifn(3,j5) .eq. pyr%n4 ) then
             pyr%fc(5) = +2           ! correct edge direction (sign)
          endif
          if( ifn(2,j5) .eq. pyr%n4 ) then
             pyr%fc(5) = +3           ! correct edge direction (sign)
          endif
       else                           ! pyramid is right
          pyr%fc(5) = -1              ! preliminary edge direction (sign)
          if( ifn(1,j5) .eq. pyr%n4 ) then
             pyr%fc(5) = -2           ! correct edge direction (sign)
          endif
          if( ifn(3,j5) .eq. pyr%n4 ) then
             pyr%fc(5) = -3           ! correct edge direction (sign)
          endif
       endif
#ifdef _DEBUG_
       PRINT*,'Identified face 5 convention:',pyr%fc(5)
#endif

c --- assign structure members
       pyr%f1 = j1
       pyr%f2 = j2
       pyr%f3 = j3
       pyr%f4 = j4
       pyr%f5 = j5

c --- Assign returned structures
       ieno(0) = 5
       ieno(1) = pyr%n1
       ieno(2) = pyr%n2
       ieno(3) = pyr%n3
       ieno(4) = pyr%n4
       ieno(5) = pyr%n5
       iefo(0) = 5
       iefo(1) = j1
       iefo(2) = j2
       iefo(3) = j3
       iefo(4) = j4
       iefo(5) = j5

       End subroutine

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c *** Routine to form a wedge cell element from three triangles and two quads.
c *** This routine assumes that what is passed in "ifn" are _valid_ surf. elems.
c *** (Definitions on the validity of surface elements are found in my notes.)
c *** Created:       IN <nompelis@nobelware.com> 20171014
c *** Last modified: IN <nompelis@nobelware.com> 20171022
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       Subroutine inMesh_Elements_FormWedgeFromFaces(
     &                  nfa, ifn, ife, i, iefi,
     &                  ieno, iefo, ierr )
       Implicit None
       Integer(kind=4), intent(OUT) :: ierr
       Integer(kind=8), intent(IN) :: nfa,i
       Integer(kind=8), intent(IN) :: ifn(0:4,nfa),ife(2,nfa)
       Integer(kind=8), intent(IN) :: iefi(0:5)
c --- Variables to return to what would be a numerical solver that is aware
c --- of the conventions used.
       Integer(kind=8), intent(OUT) :: ieno(0:8),iefo(0:5)
c --- Internal variable for this example
       Type(inMesh_Wedge_t) :: wed
       Integer(kind=8) :: j1,j2,j3,j4,j5,jj
       Integer(kind=2) :: k,ik,kk


#ifdef _DEBUG_
       PRINT*,'Constructing a wedge; number',i
       PRINT*,'Faces:', iefi(1:5)
#endif

c --- Ssanity checks
       if( iefi(0) .ne. 5 )then
          PRINT*,'Not a wedge (does not have 5 faces)'
          ierr = 1
          return
       endif

       if( (iefi(1) .le. 0 .OR. iefi(1) .gt. nfa) .OR.
     &     (iefi(2) .le. 0 .OR. iefi(2) .gt. nfa) .OR.
     &     (iefi(3) .le. 0 .OR. iefi(3) .gt. nfa) .OR.
     &     (iefi(4) .le. 0 .OR. iefi(4) .gt. nfa) .OR.
     &     (iefi(5) .le. 0 .OR. iefi(5) .gt. nfa) ) then
          PRINT*,'Face numbers out of bounds'
          ierr = 2
          return
       endif


       j1 = -1
       j2 = -1
       j3 = -1
       j4 = -1
       j5 = -1

       wed%n1 = 0
       wed%n2 = 0
       wed%n3 = 0
       wed%n4 = 0
       wed%n5 = 0
       wed%n6 = 0

       wed%f1 = 0
       wed%f2 = 0
       wed%f3 = 0
       wed%f4 = 0
       wed%f5 = 0

       wed%fc(:) = 0

c --- Select the first 3-node face of the wedge to be face 1 (the base)
c --- Copy node numbers and face number
c --- By convention (as in Fluent case files), right is where the thumb points
c --- when listing nodes by a right-hand rule.
c --- By a starting choice we set the face-convention flag to +1 or -1
       do jj = 1,iefi(0)
          if( j1 .eq. -1 ) then
             if( ifn(0, iefi(jj) ) .eq. 3 ) j1 = jj
          endif
       enddo
#ifdef _DEBUG_
       PRINT*,'First face is number ',j1
#endif
       wed%f1 = j1               ! set 1st face
       if( ife(2,j1) .eq. i ) then      ! triangle's "right" cell is the wedge
#ifdef _DEBUG_
          PRINT*,'Wedge is on the right of face 1'
#endif
          wed%n1 = ifn(1, j1 )   ! set 1st node
          wed%n2 = ifn(2, j1 )   ! set 2nd node
          wed%n3 = ifn(3, j1 )   ! set 3rd node
          wed%fc(1) = -1         ! edge number and direction (sign)
       else
#ifdef _DEBUG_
          PRINT*,'Wedge is on the left of face 1'
#endif
          wed%n1 = ifn(2, j1 )   ! set 1st node
          wed%n2 = ifn(1, j1 )   ! set 2nd node
          wed%n3 = ifn(3, j1 )   ! set 3rd node
          wed%fc(1) = +1         ! edge number and direction (sign)
       endif
#ifdef _DEBUG_
       PRINT*,'First face nodes:',wed%n1,wed%n2,wed%n3
#endif

c --- Find face 3; look for 2 matching nodes for n1,n2 of the wedge
c --- Determine orientation of face 3 (using ife); set conventions
c --- Also set 4th,5th node of the wedge
#ifdef _DEBUG_
       PRINT*,'Will match nodes:',wed%n1,wed%n2
#endif
       j3 = -1                   ! undefined number
       do k = 1,5                ! loop over all faces
          jj = iefi(k)           ! face number

          if( jj .ne. j1 ) then  ! exclude the wedge's first (base) face
#ifdef _DEBUG_
       PRINT*,'Trial face:',k,jj
#endif
             ik = 0              ! number of nodes matched for this trial face
             do kk = 1,ifn(0,jj) ! loop over all the trial face's nodes
                if( ifn(kk,jj) .eq. wed%n1 ) ik = ik + 1
                if( ifn(kk,jj) .eq. wed%n2 ) ik = ik + 1
#ifdef _DEBUG_
       PRINT*,'Node:',kk,ifn(kk,jj)
#endif
             enddo
#ifdef _DEBUG_
       PRINT*,'After testing ik=',ik
#endif
             if( ik .eq. 2 ) then
                j3 = jj
             endif

          endif
       enddo
#ifdef _DEBUG_
       PRINT*,'Identified face 3 as:',j3
       PRINT*,' Left/right elems:',ife(:,j3)
#endif

       if( ife(1,j3) .eq. i ) then    ! wedge is left
          wed%fc(3) = +1              ! preliminary edge direction (sign)
          wed%n5 = ifn(3,j3)
          wed%n4 = ifn(4,j3)
          if( ifn(4,j3) .eq. wed%n1 ) then
             wed%fc(3) = +2           ! correct edge direction (sign)
             wed%n5 = ifn(2,j3)
             wed%n4 = ifn(3,j3)
          endif
          if( ifn(3,j3) .eq. wed%n1 ) then
             wed%fc(3) = +3           ! correct edge direction (sign)
             wed%n5 = ifn(1,j3)
             wed%n4 = ifn(2,j3)
          endif
          if( ifn(2,j3) .eq. wed%n1 ) then
             wed%fc(3) = +4           ! correct edge direction (sign)
             wed%n5 = ifn(4,j3)
             wed%n4 = ifn(1,j3)
          endif
       else                           ! wedge is right
          wed%fc(3) = -1              ! preliminary edge direction (sign)
          wed%n4 = ifn(3,j3)
          wed%n5 = ifn(4,j3)
          if( ifn(1,j3) .eq. wed%n1 ) then
             wed%fc(3) = -2           ! correct edge direction (sign)
             wed%n4 = ifn(2,j3)
             wed%n5 = ifn(3,j3)
          endif
          if( ifn(4,j3) .eq. wed%n1 ) then
             wed%fc(3) = -3           ! correct edge direction (sign)
             wed%n4 = ifn(1,j3)
             wed%n5 = ifn(2,j3)
          endif
          if( ifn(3,j3) .eq. wed%n1 ) then
             wed%fc(3) = -4           ! correct edge direction (sign)
             wed%n4 = ifn(4,j3)
             wed%n5 = ifn(1,j3)
          endif
       endif
#ifdef _DEBUG_
       PRINT*,'Identified face 3 convention:',wed%fc(3)
#endif

c --- Find face 2; look for 2 matching nodes for n4,n5 of the wedge
c --- Determine orientation of face 2 (using ife); set conventions
#ifdef _DEBUG_
       PRINT*,'Will match nodes:',wed%n4,wed%n5
#endif
       j2 = -1                   ! undefined number
       do k = 1,5                ! loop over all faces
          jj = iefi(k)           ! face number

          if( jj .ne. j3 ) then  ! exclude the wedge's f3
#ifdef _DEBUG_
       PRINT*,'Trial face:',k,jj
#endif
             ik = 0              ! number of nodes matched for this trial face
             do kk = 1,ifn(0,jj) ! loop over all the trial face's nodes
                if( ifn(kk,jj) .eq. wed%n4 ) ik = ik + 1
                if( ifn(kk,jj) .eq. wed%n5 ) ik = ik + 1
#ifdef _DEBUG_
       PRINT*,'Node:',kk,ifn(kk,jj)
#endif
             enddo
#ifdef _DEBUG_
       PRINT*,'After testing ik=',ik
#endif
             if( ik .eq. 2 ) then
                j2 = jj
             endif

          endif
       enddo
#ifdef _DEBUG_
       PRINT*,'Identified face 2 as:',j2
#endif
       do kk = 1,3            ! loop over 2nd face's nodes (always 3)
          if( ifn(kk,j2) .ne. wed%n4 .AND.  ifn(kk,j2) .ne. wed%n5 ) then
             wed%n6 = ifn(kk,j2)
          endif
       enddo
#ifdef _DEBUG_
       PRINT*,' Assigned sixth node of wedge:',wed%n6
       PRINT*,' Left/right elems:',ife(:,j2)
#endif

       if( ife(1,j2) .eq. i ) then    ! wedge is left
          wed%fc(2) = +1              ! preliminary edge direction (sign)
          if( ifn(3,j2) .eq. wed%n4 ) then
             wed%fc(2) = +2           ! correct edge direction (sign)
          endif
          if( ifn(2,j2) .eq. wed%n4 ) then
             wed%fc(2) = +3           ! correct edge direction (sign)
          endif
       else
          wed%fc(2) = -1              ! preliminary edge direction (sign)
          if( ifn(1,j2) .eq. wed%n4 ) then
             wed%fc(2) = -2           ! correct edge direction (sign)
          endif
          if( ifn(3,j2) .eq. wed%n4 ) then
             wed%fc(2) = -3           ! correct edge direction (sign)
          endif
       endif
#ifdef _DEBUG_
       PRINT*,'Identified face 2 convention:',wed%fc(2)
#endif

c --- Find face 4; look for 2 matching nodes for n2,n3 of the wedge
c --- Determine orientation of face 4 (using ife); set conventions
#ifdef _DEBUG_
       PRINT*,'Will match nodes:',wed%n2,wed%n3
#endif
       j4 = -1                   ! undefined number
       do k = 1,5                ! loop over all faces
          jj = iefi(k)           ! face number

          if( jj .ne. j1 ) then  ! exclude the wedge's f1
#ifdef _DEBUG_
       PRINT*,'Trial face:',k,jj
#endif
             ik = 0              ! number of nodes matched for this trial face
             do kk = 1,ifn(0,jj) ! loop over all the trial face's nodes
                if( ifn(kk,jj) .eq. wed%n2 ) ik = ik + 1
                if( ifn(kk,jj) .eq. wed%n3 ) ik = ik + 1
#ifdef _DEBUG_
       PRINT*,'Node:',kk,ifn(kk,jj)
#endif
             enddo
#ifdef _DEBUG_
       PRINT*,'After testing ik=',ik
#endif
             if( ik .eq. 2 ) then
                j4 = jj
             endif

          endif
       enddo
#ifdef _DEBUG_
       PRINT*,'Identified face 4 as:',j4
#endif

       if( ife(1,j4) .eq. i ) then    ! wedge is left
          wed%fc(4) = +1              ! preliminary edge direction (sign)
          if( ifn(4,j4) .eq. wed%n2 ) then
             wed%fc(4) = +2           ! correct edge direction (sign)
          endif
          if( ifn(3,j4) .eq. wed%n2 ) then
             wed%fc(4) = +3           ! correct edge direction (sign)
          endif
          if( ifn(2,j4) .eq. wed%n2 ) then
             wed%fc(4) = +4           ! correct edge direction (sign)
          endif
       else                           ! wedge is right
          wed%fc(4) = -1              ! preliminary edge direction (sign)
          if( ifn(1,j4) .eq. wed%n2 ) then
             wed%fc(4) = -2           ! correct edge direction (sign)
          endif
          if( ifn(4,j4) .eq. wed%n2 ) then
             wed%fc(4) = -3           ! correct edge direction (sign)
          endif
          if( ifn(3,j4) .eq. wed%n2 ) then
             wed%fc(4) = -4           ! correct edge direction (sign)
          endif
       endif
#ifdef _DEBUG_
       PRINT*,'Identified face 4 convention:',wed%fc(4)
#endif

c --- Find face 5; look for which face is un-assigned
c --- Determine orientation of face 5 (using ife); set conventions
       j5 = -1                   ! undefined number
       do k = 1,5                ! loop over all faces
          jj = iefi(k)           ! face number

          if( jj .ne. j1 .AND. jj .ne. j2 .AND.
     &        jj .ne. j3 .AND. jj .ne. j4 ) j5 = jj
       enddo
#ifdef _DEBUG_
       PRINT*,'Identified face 5 as:',j5
#endif

       if( ife(1,j5) .eq. i ) then    ! wedge is left
          wed%fc(5) = +1              ! preliminary edge direction (sign)
          if( ifn(4,j5) .eq. wed%n3 ) then
             wed%fc(5) = +2           ! correct edge direction (sign)
          endif
          if( ifn(3,j5) .eq. wed%n3 ) then
             wed%fc(5) = +3           ! correct edge direction (sign)
          endif
          if( ifn(2,j5) .eq. wed%n3 ) then
             wed%fc(5) = +4           ! correct edge direction (sign)
          endif
       else                           ! wedge is right
          wed%fc(5) = -1              ! preliminary edge direction (sign)
          if( ifn(1,j5) .eq. wed%n3 ) then
             wed%fc(5) = -2           ! correct edge direction (sign)
          endif
          if( ifn(4,j5) .eq. wed%n3 ) then
             wed%fc(5) = -3           ! correct edge direction (sign)
          endif
          if( ifn(3,j5) .eq. wed%n3 ) then
             wed%fc(5) = -4           ! correct edge direction (sign)
          endif
       endif
#ifdef _DEBUG_
       PRINT*,'Identified face 5 convention:',wed%fc(5)
#endif

c --- assign structure members
       wed%f1 = j1
       wed%f2 = j2
       wed%f3 = j3
       wed%f4 = j4
       wed%f5 = j5

c --- Assign returned structures
       ieno(0) = 6
       ieno(1) = wed%n1
       ieno(2) = wed%n2
       ieno(3) = wed%n3
       ieno(4) = wed%n4
       ieno(5) = wed%n5
       ieno(6) = wed%n6
       iefo(0) = 5
       iefo(1) = j1
       iefo(2) = j2
       iefo(3) = j3
       iefo(4) = j4
       iefo(5) = j5

       End subroutine

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c *** Routine to form a brick element from six quadrilaterals
c *** This routine assumes that what is passed in "ifn" are _valid_ surf. elems.
c *** (Definitions on the validity of surface elements are found in my notes.)
c *** Created:       IN <nompelis@nobelware.com> 20171014
c *** Last modified: IN <nompelis@nobelware.com> 20171024
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       Subroutine inMesh_Elements_FormBrickFromFaces(
     &                  nfa, ifn, ife, i, iefi,
     &                  ieno, iefo, ierr )
       Implicit None
       Integer(kind=4), intent(OUT) :: ierr
       Integer(kind=8), intent(IN) :: nfa,i
       Integer(kind=8), intent(IN) :: ifn(0:4,nfa),ife(2,nfa)
       Integer(kind=8), intent(IN) :: iefi(0:6)
c --- Variables to return to what would be a numerical solver that is aware
c --- of the conventions used.
       Integer(kind=8), intent(OUT) :: ieno(0:8),iefo(0:6)
c --- Internal variable for this example
       Type(inMesh_Brick_t) :: bri
       Integer(kind=8) :: j1,j2,j3,j4,j5,j6,jj
       Integer(kind=2) :: k,ik,kk


#ifdef _DEBUG_
       PRINT*,'Constructing a brick; number',i
       PRINT*,'Faces:', iefi(1:6)
#endif

c --- Ssanity checks
       if( iefi(0) .ne. 6 )then
          PRINT*,'Not a brick (does not have 6 faces)'
          ierr = 1
          return
       endif

       if( (iefi(1) .le. 0 .OR. iefi(1) .gt. nfa) .OR.
     &     (iefi(2) .le. 0 .OR. iefi(2) .gt. nfa) .OR.
     &     (iefi(3) .le. 0 .OR. iefi(3) .gt. nfa) .OR.
     &     (iefi(4) .le. 0 .OR. iefi(4) .gt. nfa) .OR.
     &     (iefi(5) .le. 0 .OR. iefi(5) .gt. nfa) .OR.
     &     (iefi(6) .le. 0 .OR. iefi(6) .gt. nfa) ) then
          PRINT*,'Face numbers out of bounds'
          ierr = 2
          return
       endif

       j1 = -1
       j2 = -1
       j3 = -1
       j4 = -1
       j5 = -1
       j6 = -1

       bri%n1 = 0
       bri%n2 = 0
       bri%n3 = 0
       bri%n4 = 0
       bri%n5 = 0
       bri%n6 = 0
       bri%n7 = 0
       bri%n8 = 0

       bri%f1 = 0
       bri%f2 = 0
       bri%f3 = 0
       bri%f4 = 0
       bri%f5 = 0
       bri%f6 = 0

       bri%fc(:) = 0

c --- Select the first face of the brick to be face 1 (the base)
c --- Copy node numbers and face number
c --- By convention (as in Fluent case files), right is where the thumb points
c --- when listing nodes by a right-hand rule.
c --- By a starting choice we set the face-convention flag to +1 or -1
       j1 = iefi(1)
#ifdef _DEBUG_
       PRINT*,'First face is number ',j1
#endif
       bri%f1 = j1               ! set 1st face
       if( ife(2,j1) .eq. i ) then      ! quad's "right" cell is the brick
#ifdef _DEBUG_
          PRINT*,'Brick is on the right of face 1'
#endif
          bri%n1 = ifn(1, j1 )   ! set 1st node
          bri%n2 = ifn(2, j1 )   ! set 2nd node
          bri%n3 = ifn(3, j1 )   ! set 3rd node
          bri%n4 = ifn(4, j1 )   ! set 4th node
          bri%fc(1) = -1         ! edge number and direction (sign)
       else
#ifdef _DEBUG_
          PRINT*,'Brick is on the left of face 1'
#endif
          bri%n1 = ifn(2, j1 )   ! set 1st node
          bri%n2 = ifn(1, j1 )   ! set 2nd node
          bri%n3 = ifn(4, j1 )   ! set 3rd node
          bri%n4 = ifn(3, j1 )   ! set 4th node
          bri%fc(1) = +1         ! edge number and direction (sign)
       endif
#ifdef _DEBUG_
       PRINT*,'First face nodes:',bri%n1,bri%n2,bri%n3,bri%n4
#endif

c --- Find face 3; look for 2 matching nodes for n1,n2 of the brick
c --- Determine orientation of face 3 (using ife); set conventions
c --- Also set 5th,6th node of the brick
#ifdef _DEBUG_
       PRINT*,'Will match nodes:',bri%n1,bri%n2
#endif
       j3 = -1                   ! undefined number
       do k = 2,6                ! loop over remaining faces
          jj = iefi(k)           ! face number

#ifdef _DEBUG_
       PRINT*,'Trial face:',k,jj
#endif
          ik = 0                 ! number of nodes matched for this trial face
          do kk = 1,4            ! loop over all the trial face's nodes
             if( ifn(kk,jj) .eq. bri%n1 ) ik = ik + 1
             if( ifn(kk,jj) .eq. bri%n2 ) ik = ik + 1
#ifdef _DEBUG_
       PRINT*,'Node:',kk,ifn(kk,jj)
#endif
          enddo
#ifdef _DEBUG_
       PRINT*,'After testing ik=',ik
#endif
          if( ik .eq. 2 ) then
             j3 = jj
          endif
       enddo
#ifdef _DEBUG_
       PRINT*,'Identified face 3 as:',j3
       PRINT*,' Left/right elems:',ife(:,j3)
#endif

       if( ife(1,j3) .eq. i ) then    ! brick is left
          bri%fc(3) = +1              ! preliminary edge direction (sign)
          bri%n6 = ifn(3,j3)
          bri%n5 = ifn(4,j3)
          if( ifn(4,j3) .eq. bri%n1 ) then
             bri%fc(3) = +2           ! correct edge direction (sign)
             bri%n6 = ifn(2,j3)
             bri%n5 = ifn(3,j3)
          endif
          if( ifn(3,j3) .eq. bri%n1 ) then
             bri%fc(3) = +3           ! correct edge direction (sign)
             bri%n6 = ifn(1,j3)
             bri%n5 = ifn(2,j3)
          endif
          if( ifn(2,j3) .eq. bri%n1 ) then
             bri%fc(3) = +4           ! correct edge direction (sign)
             bri%n6 = ifn(4,j3)
             bri%n5 = ifn(1,j3)
          endif
       else                           ! brick is right
          bri%fc(3) = -1              ! preliminary edge direction (sign)
          bri%n5 = ifn(3,j3)
          bri%n6 = ifn(4,j3)
          if( ifn(1,j3) .eq. bri%n1 ) then
             bri%fc(3) = -2           ! correct edge direction (sign)
             bri%n5 = ifn(2,j3)
             bri%n6 = ifn(3,j3)
          endif
          if( ifn(4,j3) .eq. bri%n1 ) then
             bri%fc(3) = -3           ! correct edge direction (sign)
             bri%n5 = ifn(1,j3)
             bri%n6 = ifn(2,j3)
          endif
          if( ifn(3,j3) .eq. bri%n1 ) then
             bri%fc(3) = -4           ! correct edge direction (sign)
             bri%n5 = ifn(4,j3)
             bri%n6 = ifn(1,j3)
          endif
       endif
#ifdef _DEBUG_
       PRINT*,'Identified face 3 convention:',bri%fc(3)
#endif

c --- Find face 2; look for 2 matching nodes for n5,n6 of the brick
c --- Determine orientation of face 2 (using ife); set conventions
#ifdef _DEBUG_
       PRINT*,'Will match nodes:',bri%n5,bri%n6
#endif
       j2 = -1                   ! undefined number
       do k = 2,6                ! loop over faces other than face 1
          jj = iefi(k)           ! face number

          if( jj .ne. j3 ) then  ! exclude the brick's f3
#ifdef _DEBUG_
       PRINT*,'Trial face:',k,jj
#endif
             ik = 0              ! number of nodes matched for this trial face
             do kk = 1,4         ! loop over all the trial face's nodes
                if( ifn(kk,jj) .eq. bri%n5 ) ik = ik + 1
                if( ifn(kk,jj) .eq. bri%n6 ) ik = ik + 1
#ifdef _DEBUG_
       PRINT*,'Node:',kk,ifn(kk,jj)
#endif
             enddo
#ifdef _DEBUG_
       PRINT*,'After testing ik=',ik
#endif
             if( ik .eq. 2 ) then
                j2 = jj
             endif

          endif
       enddo
#ifdef _DEBUG_
       PRINT*,'Identified face 2 as:',j2
       PRINT*,' Left/right elems:',ife(:,j2)
#endif

       if( ife(1,j2) .eq. i ) then    ! brick is left
          bri%fc(2) = +1              ! preliminary edge direction (sign)
          bri%n7 = ifn(3,j2)
          bri%n8 = ifn(4,j2)
          if( ifn(4,j2) .eq. bri%n5 ) then
             bri%fc(2) = +2           ! correct edge direction (sign)
             bri%n7 = ifn(2,j2)
             bri%n8 = ifn(3,j2)
          endif
          if( ifn(3,j2) .eq. bri%n5 ) then
             bri%fc(2) = +3           ! correct edge direction (sign)
             bri%n7 = ifn(1,j2)
             bri%n8 = ifn(2,j2)
          endif
          if( ifn(2,j2) .eq. bri%n5 ) then
             bri%fc(2) = +4           ! correct edge direction (sign)
             bri%n7 = ifn(4,j2)
             bri%n8 = ifn(1,j2)
          endif
       else
          bri%fc(2) = -1              ! preliminary edge direction (sign)
          bri%n8 = ifn(3,j2)
          bri%n7 = ifn(4,j2)
          if( ifn(1,j2) .eq. bri%n5 ) then
             bri%fc(2) = -2           ! correct edge direction (sign)
             bri%n8 = ifn(2,j2)
             bri%n7 = ifn(3,j2)
          endif
          if( ifn(4,j2) .eq. bri%n5 ) then
             bri%fc(2) = -3           ! correct edge direction (sign)
             bri%n8 = ifn(1,j2)
             bri%n7 = ifn(2,j2)
          endif
          if( ifn(3,j2) .eq. bri%n5 ) then
             bri%fc(2) = -4           ! correct edge direction (sign)
             bri%n8 = ifn(4,j2)
             bri%n7 = ifn(1,j2)
          endif
       endif
#ifdef _DEBUG_
       PRINT*,'Identified face 2 convention:',bri%fc(2)
#endif

c --- Find face 4; look for 2 matching nodes for n7,n8 of the brick
c --- Determine orientation of face 4 (using ife); set conventions
#ifdef _DEBUG_
       PRINT*,'Will match nodes:',bri%n5,bri%n6
#endif
       j4 = -1                   ! undefined number
       do k = 2,6                ! loop over faces other than face 1
          jj = iefi(k)           ! face number

          if( jj.ne.j1 .AND. jj.ne.j2 ) then  ! exclude the brick's f1 or f2
#ifdef _DEBUG_
       PRINT*,'Trial face:',k,jj
#endif
             ik = 0              ! number of nodes matched for this trial face
             do kk = 1,4         ! loop over all the trial face's nodes
                if( ifn(kk,jj) .eq. bri%n7 ) ik = ik + 1
                if( ifn(kk,jj) .eq. bri%n8 ) ik = ik + 1
#ifdef _DEBUG_
       PRINT*,'Node:',kk,ifn(kk,jj)
#endif
             enddo
#ifdef _DEBUG_
       PRINT*,'After testing ik=',ik
#endif
             if( ik .eq. 2 ) then
                j4 = jj
             endif

          endif
       enddo
#ifdef _DEBUG_
       PRINT*,'Identified face 4 as:',j4
       PRINT*,' Left/right elems:',ife(:,j4)
#endif

       if( ife(1,j4) .eq. i ) then    ! brick is left
          bri%fc(4) = +1              ! preliminary edge direction (sign)
          if( ifn(4,j4) .eq. bri%n3 ) then
             bri%fc(4) = +2           ! correct edge direction (sign)
          endif
          if( ifn(3,j4) .eq. bri%n3 ) then
             bri%fc(4) = +3           ! correct edge direction (sign)
          endif
          if( ifn(2,j4) .eq. bri%n3 ) then
             bri%fc(4) = +4           ! correct edge direction (sign)
          endif
       else
          bri%fc(4) = -1              ! preliminary edge direction (sign)
          if( ifn(1,j4) .eq. bri%n3 ) then
             bri%fc(4) = -2           ! correct edge direction (sign)
          endif
          if( ifn(4,j4) .eq. bri%n3 ) then
             bri%fc(4) = -3           ! correct edge direction (sign)
          endif
          if( ifn(3,j4) .eq. bri%n3 ) then
             bri%fc(4) = -4           ! correct edge direction (sign)
          endif
       endif
#ifdef _DEBUG_
       PRINT*,'Identified face 4 convention:',bri%fc(4)
#endif

c --- Find face 6; look for 2 matching nodes for n2,n3 of the brick
c --- Determine orientation of face 6 (using ife); set conventions
#ifdef _DEBUG_
       PRINT*,'Will match nodes:',bri%n2,bri%n3
#endif
       j6 = -1                   ! undefined number
       do k = 2,6                ! loop over faces other than face 1
          jj = iefi(k)           ! face number

          if( jj.ne.j3 .AND. jj.ne.j4 ) then  ! exclude the brick's f3 or f4
#ifdef _DEBUG_
       PRINT*,'Trial face:',k,jj
#endif
             ik = 0              ! number of nodes matched for this trial face
             do kk = 1,4         ! loop over all the trial face's nodes
                if( ifn(kk,jj) .eq. bri%n2 ) ik = ik + 1
                if( ifn(kk,jj) .eq. bri%n3 ) ik = ik + 1
#ifdef _DEBUG_
       PRINT*,'Node:',kk,ifn(kk,jj)
#endif
             enddo
#ifdef _DEBUG_
       PRINT*,'After testing ik=',ik
#endif
             if( ik .eq. 2 ) then
                j6 = jj
             endif

          endif
       enddo
#ifdef _DEBUG_
       PRINT*,'Identified face 6 as:',j6
       PRINT*,' Left/right elems:',ife(:,j6)
#endif

       if( ife(1,j6) .eq. i ) then    ! brick is left
          bri%fc(6) = +1              ! preliminary edge direction (sign)
          if( ifn(4,j6) .eq. bri%n2 ) then
             bri%fc(6) = +2           ! correct edge direction (sign)
          endif
          if( ifn(3,j6) .eq. bri%n2 ) then
             bri%fc(6) = +3           ! correct edge direction (sign)
          endif
          if( ifn(2,j6) .eq. bri%n2 ) then
             bri%fc(6) = +4           ! correct edge direction (sign)
          endif
       else
          bri%fc(6) = -1              ! preliminary edge direction (sign)
          if( ifn(1,j6) .eq. bri%n2 ) then
             bri%fc(6) = -2           ! correct edge direction (sign)
          endif
          if( ifn(4,j6) .eq. bri%n2 ) then
             bri%fc(6) = -3           ! correct edge direction (sign)
          endif
          if( ifn(3,j6) .eq. bri%n2 ) then
             bri%fc(6) = -4           ! correct edge direction (sign)
          endif
       endif
#ifdef _DEBUG_
       PRINT*,'Identified face 6 convention:',bri%fc(6)
#endif

c --- Find face 5; look for the face that is un-attached
c --- Determine orientation of face 5 (using ife); set conventions
#ifdef _DEBUG_
       PRINT*,'Will match nodes:',bri%n2,bri%n3
#endif
       j5 = -1                   ! undefined number
       do k = 2,6                ! loop over faces other than face 1
          jj = iefi(k)           ! face number

          if( jj.ne.j2 .AND. jj.ne.j3 .AND. jj.ne.j4 .AND. jj.ne.j6 ) then
#ifdef _DEBUG_
       PRINT*,'Trial face:',k,jj
#endif
             j5 = jj
          endif
       enddo
#ifdef _DEBUG_
       PRINT*,'Identified face 5 as:',j5
       PRINT*,' Left/right elems:',ife(:,j5)
#endif

       if( ife(1,j5) .eq. i ) then    ! brick is left
          bri%fc(5) = +1              ! preliminary edge direction (sign)
          if( ifn(4,j5) .eq. bri%n4 ) then
             bri%fc(5) = +2           ! correct edge direction (sign)
          endif
          if( ifn(3,j5) .eq. bri%n4 ) then
             bri%fc(5) = +3           ! correct edge direction (sign)
          endif
          if( ifn(2,j5) .eq. bri%n4 ) then
             bri%fc(5) = +4           ! correct edge direction (sign)
          endif
       else
          bri%fc(5) = -1              ! preliminary edge direction (sign)
          if( ifn(1,j5) .eq. bri%n4 ) then
             bri%fc(5) = -2           ! correct edge direction (sign)
          endif
          if( ifn(4,j5) .eq. bri%n4 ) then
             bri%fc(5) = -3           ! correct edge direction (sign)
          endif
          if( ifn(3,j5) .eq. bri%n4 ) then
             bri%fc(5) = -4           ! correct edge direction (sign)
          endif
       endif
#ifdef _DEBUG_
       PRINT*,'Identified face 5 convention:',bri%fc(5)
#endif

c --- assign structure members
       bri%f1 = j1
       bri%f2 = j2
       bri%f3 = j3
       bri%f4 = j4
       bri%f5 = j5
       bri%f6 = j6

c --- Assign returned structures
       ieno(0) = 8
       ieno(1) = bri%n1
       ieno(2) = bri%n2
       ieno(3) = bri%n3
       ieno(4) = bri%n4
       ieno(5) = bri%n5
       ieno(6) = bri%n6
       ieno(7) = bri%n7
       ieno(8) = bri%n8
       iefo(0) = 6
       iefo(1) = j1
       iefo(2) = j2
       iefo(3) = j3
       iefo(4) = j4
       iefo(5) = j5
       iefo(6) = j6

       End subroutine

       End module

