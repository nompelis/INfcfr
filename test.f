      Program main
      Implicit None
      Character*100 :: fname

      fname = 'grid.cas'
      call inFluent_FortranWrapperInit( trim( fname ) )




      call inFluent_FortranWrapperTerm()

      End

