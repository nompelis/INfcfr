/***********************************************************************/
/* A demo driver code to test Fluent case file reading.                */
/*                                                                     */
/* Version 1.9                                                         */
/* Copyright 2011-2016 Ioannis Nompelis <nompelis@nobelware.com>       */
/***********************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>


#include "fluent.h"
#include "fluent_defs.h"

#ifdef _USE_HDF
#include "hdf5_struct.h"
#endif


int tecplot_groups_opengl_style( struct my_fluentcase *cas, char *filename )
{
   FILE *fp;
   int n;

   if( cas->type != 3 ) {
      fprintf(stdout," e  This function is for displaying only 3D casee \n");
      return(1);
   }

   fp = fopen( filename, "w" );
   fprintf(fp,"variables = x y z \n");

   for(n=0;n<cas->nzone;++n) {
      struct my_fluentzone *zp = &( cas->zones[n] );
      if( zp->type == 13 ) {
         long nface = zp->nend - zp->nstart + 1;
         long j,jj,i;

         fprintf(stdout," i  Writing zone: %d \n", zp->id);
         fprintf(fp,"zone T=\"%d\", N=%ld,E=%ld,ET=QUADRILATERAL,F=FEPOINT \n",
              zp->id, nface*4, nface);

         for(j=zp->nstart;j<=zp->nend;++j) {
            jj = j - zp->nstart;
            i = zp->ifn[jj][0] - 1;
            fprintf(fp,"%lf %lf %lf\n", cas->x[i],cas->y[i],cas->z[i]);
            i = zp->ifn[jj][1] - 1;
            fprintf(fp,"%lf %lf %lf\n", cas->x[i],cas->y[i],cas->z[i]);
            i = zp->ifn[jj][2] - 1;
            fprintf(fp,"%lf %lf %lf\n", cas->x[i],cas->y[i],cas->z[i]);
            i = zp->ifn[jj][3] - 1;
            if( i < 0 ) {
               i = zp->ifn[jj][2] - 1;
            }
            fprintf(fp,"%lf %lf %lf\n", cas->x[i],cas->y[i],cas->z[i]);
         }

         i = 0;
         for(j=zp->nstart;j<=zp->nend;++j) {
            fprintf(fp, "%ld %ld %ld %ld\n",
                    i+1,i+2,i+3,i+4);
            i += 4;
         }
      }
   }

   fclose(fp);

   return(0);
}



/* Driver */
int main() {

   struct my_fluentcase cas;
#ifdef _USE_HDF
   struct my_HDF_vars hdfvars;
#endif
   inFluent_InitCase(&cas);
   inFluent_ReadCase(&cas,"grid.cas");
   (void) tecplot_groups_opengl_style( &cas, (char *) "DATA.dat" );
   inFluent_ClearCase(&cas);

// when using conversion to HDF do this
#ifdef _USE_HDF
   inFluent_HDF_Startup(&hdfvars);
#endif

#ifdef _USE_HDF
// inFluent_InitCase(&cas);
// inFluent_ConvertCaseHDF(&cas, "grid.cas", &hdfvars);
// inFluent_ClearCase(&cas);
#endif
   return(0);
}

