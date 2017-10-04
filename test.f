      Program main
      Implicit None
      Character*100 :: fname
      Integer(kind=8) :: nno,nel,nfa

      fname = 'grid.cas'
      call inFluent_FortranWrapperInit( trim( fname ), nno, nel, nfa )






      call inFluent_FortranWrapperTerm()

      End

