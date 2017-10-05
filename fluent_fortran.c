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
                                 int *nzone, long *nno, long *nel, long *nfa )
{

   struct my_fluentcase *cas = &( fortran_cas );

   inFluent_InitCase( cas );
   inFluent_ReadCase( cas, filename );
   *nzone = cas->nzone;
   *nno = cas->nno;
   *nel = cas->nel;
   *nfa = cas->nfa;

   if( cas->nno <= 0 ) {
      return(1);
   }

   return(0);
}

void influent_fortranwrapperinit_( char *f,
                                   int *nzone, long *nno, long *nel, long *nfa,
                                   int *ierr,
                                   int nf )
{
#ifdef _DEBUG_
   printf("Fortran wrapper initialization called\n");
   printf(" - Filename: (%d) \"%s\"\n", nf, f );
#endif

   *ierr = inFluent_FortranWrapperInit( f, nzone, nno, nel, nfa );

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

void influent_fortranwrapperterm_( int *ierr )
{
#ifdef _DEBUG_
   printf("Fortran wrapper termination called\n");
#endif

   *ierr = inFluent_FortranWrapperTerm();
}


//
// Function to wrap the Fortran array transfer procedure
//

int influent_fortranwrapperfill_( long *izone,
                                  long *ifn, long *ife, double *x, int *ierr )
{
   long nno,nel,nfa;
   struct my_fluentcase *cas = &( fortran_cas );
   long n;
   int nz = cas->nzone;

#ifdef _DEBUG_
   printf("Fortran wrapper array fill called\n");
#endif

   nno = cas->nno;
   nel = cas->nel;
   nfa = cas->nfa;

   // copy zones's bounds
   for(n=0;n<nz;++n) {
      izone[4*n+0] = cas->zones[n].nstart;
      izone[4*n+1] = cas->zones[n].nend;
      izone[4*n+2] = izone[n*4+1] - izone[n*4+0] + 1;
      izone[4*n+3] = cas->zones[n].type;
   }

   // copy node coefficients
   for(n=0;n<nno;++n) {
      x[n*3+0] = cas->x[n];
      x[n*3+1] = cas->y[n];
      x[n*3+2] = cas->z[n];
   }

   // copy face data
   for(n=0;n<nfa;++n) {
      ifn[n*5+1] = cas->ifn[n][0];
      ifn[n*5+2] = cas->ifn[n][1];
      ifn[n*5+3] = cas->ifn[n][2];
      ifn[n*5+4] = cas->ifn[n][3];
      if( cas->ifn[n][3] > 0 ) {
         ifn[n*5+0] = 4;
      } else {
         ifn[n*5+0] = 3;
      }

      ife[n*2+0] = cas->ife[n][0];
      ife[n*2+1] = cas->ife[n][1];
   }


#ifdef _DEBUG_
   printf(" - Done filling arrays \n");
#endif

   return(0);
}



