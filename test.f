      Program main
      Implicit None
      Character*100 :: fname
      Integer(kind=8) :: nno,nel,nfa
      Integer(kind=8),allocatable,dimension(:,:) :: ifn,ife
      Integer(kind=4) :: ierr

      fname = 'grid.cas'
      call inFluent_FortranWrapperInit( trim( fname ), nno, nel, nfa, ierr )
      PRINT*,'Result of Init() ierr=',ierr




      call inFluent_FortranWrapperTerm( ierr )
      PRINT*,'Result of Term() ierr=',ierr

      End

