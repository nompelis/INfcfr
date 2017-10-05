/***********************************************************************/
/* A wrapper for invoking the library through a Fortran interface      */
/*                                                                     */
/* Version 1.9                                                         */
/* Copyright 2011-2017 Ioannis Nompelis <nompelis@nobelware.com>       */
/***********************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>


#include "fluent.h"
#include "fluent_defs.h"

// a variable to store the globally accessible case
static struct my_fluentcase fortran_cas;


//
// Function to wrap the Fortran initialization procedure
//

int inFluent_FortranWrapperInit( char *filename,
                                 long *nno, long *nel, long *nfa ) {

   struct my_fluentcase *cas = &( fortran_cas );

   inFluent_InitCase( cas );
   inFluent_ReadCase( cas, filename );
   *nno = cas->nno;
   *nel = cas->nel;
   *nfa = cas->nfa;

   if( cas->nno <= 0 ) {
      return(1);
   }

   return(0);
}

void influent_fortranwrapperinit_( char *f, long *nno, long *nel, long *nfa,
                                   int *ierr,
                                   int nf ) {
#ifdef _DEBUG_
   printf("Fortran wrapper initialization called\n");
   printf(" - Filename: (%d) \"%s\"\n", nf, f );
#endif

   *ierr = inFluent_FortranWrapperInit( f, nno, nel, nfa );

#ifdef _DEBUG_
   printf(" - Sizes after reading\n");
   printf("   nno = %ld, nel = %ld, nfa = %ld \n", *nno, *nel, *nfa );
#endif
}


//
// Function to wrap the Fortran termination procedure
//

int inFluent_FortranWrapperTerm() {

   struct my_fluentcase *cas = &( fortran_cas );

   inFluent_ClearCase( cas );

   return(0);
}

void influent_fortranwrapperterm_( int *ierr ) {
#ifdef _DEBUG_
   printf("Fortran wrapper termination called\n");
#endif

   *ierr = inFluent_FortranWrapperTerm();
}

