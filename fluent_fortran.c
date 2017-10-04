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

int inFluent_FortranWrapperInit( char *filename ) {

   struct my_fluentcase *cas = &( fortran_cas );

   inFluent_InitCase( cas );
   inFluent_ReadCase( cas, filename );

   return(0);
}

void influent_fortranwrapperinit_( char *f, int nf ) {
#ifdef _DEBUG_
   printf("Fortran wrapper initialization called\n");
   printf(" - Filename: (%d) \"%s\"\n", nf, f );
#endif

   (void) inFluent_FortranWrapperInit( f );
}


//
// Function to wrap the Fortran termination procedure
//

int inFluent_FortranWrapperTerm() {

   struct my_fluentcase *cas = &( fortran_cas );

   inFluent_ClearCase( cas );

   return(0);
}

void influent_fortranwrapperterm_() {
#ifdef _DEBUG_
   printf("Fortran wrapper termination called\n");
#endif

   (void) inFluent_FortranWrapperTerm();
}

