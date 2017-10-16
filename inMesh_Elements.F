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
c *** Last modified: IN <nompelis@nobelware.com> 20171014
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
          !--- "Any face of the tetrahedron's 1-4 can be any triangle and it
          !--- can have any of three positive or negative alignments. Positive
          !--- numbers imply outward pointing normals. Permutations of 1,2,3
          !--- imply deviations from the "cannonical" alignment.
          Integer(kind=2) :: fc(4)
       End Type

       Type inMesh_Pyramid_t
       End Type

       Type inMesh_Prizm_t
       End Type

       Type inMesh_Brick_t
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


       PRINT*,'Constructing a tetrahedron; number',i
       PRINT*,'Faces:', iefi(1:4)

c --- Ssanity checks
       if( iefi(0) .ne. 4 )then
          PRINT*,'Not a tetrahedron (does not have 4 faces)'
          ierr = 1
          return
       endif

       if( (iefi(1) .le. 0 .OR. iefi(1) .gt. nfa) .OR.
     &     (iefi(2) .le. 0 .OR. iefi(2) .gt. nfa) .OR.
     &     (iefi(3) .le. 0 .OR. iefi(3) .gt. nfa) ) then
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
          PRINT*,'Tetrahedron is on the right of face 1'
          tet%n1 = ifn(1, j1 )   ! set 1st node
          tet%n2 = ifn(2, j1 )   ! set 2nd node
          tet%n3 = ifn(3, j1 )   ! set 3rd node
          tet%fc(1) = -1         ! edge number and direction (sign)
       else
          PRINT*,'Tetrahedron is on the left of face 1'
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
c *** Last modified: IN <nompelis@nobelware.com> 20171014
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       Subroutine inMesh_Elements_FormPyramidFromFaces(
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
       Type(inMesh_Pyramid_t) :: pyr


       PRINT*,'Constructing a pyramid; number',i
       PRINT*,'Faces:', iefi(1:4)
       PRINT*,'NOT YET IMPLEMENTED; EXITING!'
       STOP



       End subroutine

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c *** Routine to form a prizm cell element from two triangles and two quads.
c *** This routine assumes that what is passed in "ifn" are _valid_ surf. elems.
c *** (Definitions on the validity of surface elements are found in my notes.)
c *** Created:       IN <nompelis@nobelware.com> 20171014
c *** Last modified: IN <nompelis@nobelware.com> 20171014
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       Subroutine inMesh_Elements_FormPrizmFromFaces(
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
       Type(inMesh_Prizm_t) :: pri


       PRINT*,'Constructing a prizm; number',i
       PRINT*,'Faces:', iefi(1:4)
       PRINT*,'NOT YET IMPLEMENTED; EXITING!'
       STOP



       End subroutine

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c *** Routine to form a brick element from six quadrilaterals
c *** This routine assumes that what is passed in "ifn" are _valid_ surf. elems.
c *** (Definitions on the validity of surface elements are found in my notes.)
c *** Created:       IN <nompelis@nobelware.com> 20171014
c *** Last modified: IN <nompelis@nobelware.com> 20171014
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       Subroutine inMesh_Elements_FormBrickFromFaces(
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
       Type(inMesh_Brick_t) :: bri


       PRINT*,'Constructing a brick; number',i
       PRINT*,'Faces:', iefi(1:4)
       PRINT*,'NOT YET IMPLEMENTED; EXITING!'
       STOP



       End subroutine

       End module

