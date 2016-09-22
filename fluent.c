/***********************************************************************/
/* Routines to define and read a "simple" Fluent case file. This code  */
/* is not fully Fluent case format compliant. It is my adoptation of   */
/* the Fluent case format for use in my codes.                         */
/* The code can write an HDF file with the case-file contents.         */
/*                                                                     */
/* Version 1.9                                                         */
/* Copyright 2011-2016 Ioannis Nompelis <nompelis@nobelware.com>       */
/***********************************************************************/

/**************************************************************************
 Copyright (c) 2011-2016, Ioannis Nompelis
 All rights reserved.

 Redistribution and use in source and binary forms, with or without any
 modification, are permitted provided that the following conditions are met:
 1. Redistribution of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.
 2. Redistribution in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.
 3. All advertising materials mentioning features or use of this software
    must display the following acknowledgement:
    "This product includes software developed by Ioannis Nompelis."
 4. Neither the name of Ioannis Nompelis and his partners/affiliates nor the
    names of other contributors may be used to endorse or promote products
    derived from this software without specific prior written permission.
 5. Redistribution or use of source code and binary forms for profit must
    have written permission of the copyright holder.
 
 THIS SOFTWARE IS PROVIDED BY IOANNIS NOMPELIS ''AS IS'' AND ANY
 EXPRESSED OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 DISCLAIMED. IN NO EVENT SHALL IOANNIS NOMPELIS BE LIABLE FOR ANY
 DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 **************************************************************************/

/* -----------------------------------------------------------------
    "What this file is and what it isn't."

   - Will display the line number in the file at state changes.
   - Will read nodes and faces correctly. It will allocate memory
     for the necessary arrays, but it has a bit of my twist to it.
   - Will store the number of cells, but it will not identify any
     cell-region of the domain in any zone as anything special. And
     this means that any zone with an id corresponding to cells in
     the case file will be left blank.
   - Will store the dimensionality of the problem (physical space)
   - Will echo comments to the screen.
   - There is significant error trapping but no clean-up on fatal
     errors. (This needs to be changed in the future.)
   ---------------------------------------------------------------- */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>


#include "fluent.h"
#include "fluent_defs.h"

#ifdef _USE_HDF
#include <hdf5.h>
#include "hdf5_struct.h"
#endif


/*
 * Fucntion to handle the reading of nodes from the case file
 * Ioannis Nompelis <nompelis@nobelware.com>       Created: 20130617
 * Ioannis Nompelis <nompelis@nobelware.com> Last modified: 20160914
 */

int inFluent_HandleCells(struct my_fluentcase *cas, char *data,
                         long *nline, FILE *fp) {

   char *FUNC = "inFluent_HandleCells";
   int id,nd;
   long istart,iend;
   struct my_fluentzone *zp;
   int ins;


   // note that it is like this:
   // (12 (zone-id first-index last-index type XX))     OR
   // (12 (0       first-index last-index type))
   sscanf(data,"(12 (%x %lx %lx 0 %d)",&id,&istart,&iend,&nd);

   if(id == 0) {
      fprintf(stdout," i [%s]  Case file provided total number of cells: %ld \n",FUNC,iend);

      cas->nel = iend;
      ins = 0;
   } else {
      fprintf(stdout," i [%s]  Found cell-zone %d with bounds [%ld:%ld] \n",FUNC,id,istart,iend);

      if(cas->nzone == cas->mzone) {
         fprintf(stdout," e [%s]  Not enough preallocated zones (%d) \n",FUNC,cas->mzone);
         return(1);
      } else {
         zp = (struct my_fluentzone *) &(cas->zones[cas->nzone]);

         zp->id   = id;  // this may have to be moved further down
         zp->type = 12;  // this may have to be moved further down
         zp->nstart = istart;
         zp->nend   = iend;
         zp->iattr  = nd;
      }

      if(cas->nel <= 0) {
         zp->imem = 20;

         ins = 2;
      } else {
         zp->imem = 2;
         ins = 1;
      }

   }

// if(ins == 1) {
//    for(n=istart-1;n<iend;++n) {
//       fgets(line,100,fp);
//       *nline = *nline + 1;
//
//       sscanf(line,"%lf %lf %lf",&x,&y,&z);
//
//       // some data needs to be inserted somewhare
//       // this is likely volumetric designator data
//    }
//
// } else if(ins == 2) {
//
//    for(n=istart-1;n<iend;++n) {
//       fgets(line,100,fp);
//       *nline = *nline + 1;
//
//       sscanf(line,"%lf %lf %lf",&x,&y,&z);
//
//       // some data needs to be inserted somewhare
//       // this is likely volumetric designator data
//    }
// }

   if(ins != 0) {
//    fgets(line,100,fp);
//    *nline = *nline + 1;
//    if(strncmp("))",line,2) != 0) {
//       fprintf(stdout," e [%s]  Could not find cells-terminating line\n",FUNC);
//        return(1);
//    } else {
//       fprintf(stdout," i [%s]  Got info on %ld cells \n",FUNC,iend-istart+1);
//       zp->nstart = istart;
//       zp->nend   = iend;
//   //  fprintf(stdout,"          This zones's bounds %ld %ld \n",zp->nstart,zp->nend);
//    }

      fprintf(stdout," i [%s]  Got info on %ld cells \n",FUNC,iend-istart+1);
      zp->nstart = istart;
      zp->nend   = iend;

      cas->nzone += 1;
   }

   return(0);
}


/*
 * Fucntion to handle the reading of nodes from the case file
 * Ioannis Nompelis <nompelis@nobelware.com>       Created: 20110401
 * Ioannis Nompelis <nompelis@nobelware.com> Last modified: 20140516
 */

int inFluent_HandleNodes(struct my_fluentcase *cas, char *data,
                         long *nline, FILE *fp) {

   int id,nd;
   long istart,iend;
   size_t size;
   struct my_fluentzone *zp;
   long n;
   int ins;
   double x,y,z;
   char line[200];


   // not that it is like this:
   // (10 (zone-id first-index last-index type ND)(     OR
   // (10 (0       first-index last-index type ND))
   // where "type" is always zero
   sscanf(data,"(10 (%x %lx %lx 0 %d)",&id,&istart,&iend,&nd);

   if(id == 0) {
      fprintf(stdout," i [inFluent_HandleNodes]  Case file provided total number of nodes: %ld \n",iend);

      size = iend*sizeof(double);
      cas->x = (double *) malloc(size);
      cas->y = (double *) malloc(size);
      cas->z = (double *) malloc(size);
      if(cas->x == NULL || cas->y == NULL || cas->z == NULL) {
         fprintf(stdout," e [inFluent_HandleNodes]  Could not allocate memory for nodes \n");
         if(cas->x != NULL) free(cas->x);
         if(cas->y != NULL) free(cas->y);
         if(cas->z != NULL) free(cas->z);
         cas->x = NULL;
         cas->y = NULL;
         cas->z = NULL;
      } else {
         fprintf(stdout," i [inFluent_HandleNodes]  Allocated %f MBs for nodes \n",
                 ((double) (size*3)) / ((double) (1024*1024)) );
      }

      cas->nno = iend;
      ins = 0;
   } else {
      fprintf(stdout," i [inFluent_HandleNodes]  Found node-zone %d with bounds [%ld:%ld] \n",id,istart,iend);

      if(cas->nzone == cas->mzone) {
         fprintf(stdout," e [inFluent_HandleNodes]  Not enough preallocated zones (%d) \n",cas->mzone);
         return(1);
      } else {
         zp = (struct my_fluentzone *) &(cas->zones[cas->nzone]);

         zp->id   = id;  // this may have to be moved further down
         zp->type = 10;  // this may have to be moved further down
         zp->iattr = 0;
      }

      if(cas->nno <= 0) {
         zp->imem = 10;
         // Here I am speculating that there could be a possibility where
         // nodes are part of a zone but the total number of nodes was not
         // given in the case file. If true, I allocate space for the nodes
         // as if they are part of this zone, which is what happens below.
         // If false, I should just return an error for out-of-order case items

         size = (iend - istart + 1)*sizeof(double);
         zp->x = (double *) malloc(size);
         zp->y = (double *) malloc(size);
         zp->z = (double *) malloc(size);

         if(zp->x == NULL || zp->y == NULL || zp->z == NULL) {
            fprintf(stdout," e [inFluent_HandleNodes]  Could not allocate memory for nodes within a zone \n");
            if(zp->x != NULL) free(zp->x);
            if(zp->y != NULL) free(zp->y);
            if(zp->z != NULL) free(zp->z);
            zp->x = NULL;
            zp->y = NULL;
            zp->z = NULL;
            return(-1);
         } else {
            fprintf(stdout," i [inFluent_HandleNodes]  Allocated %f MBs for nodes in zone %d \n",
                    ((double) (size*3)) / ((double) (1024*1024)) ,id);
         }

         ins = 2;
      } else {
         zp->imem = 1;
         fprintf(stdout," i [inFluent_HandleNodes]  Inserting nodes in array\n");
         zp->x = (double *) &(cas->x[istart-1]);
         zp->y = (double *) &(cas->y[istart-1]);
         zp->z = (double *) &(cas->z[istart-1]);
         ins = 1;
      }

   }


   // no error-trapping during reading operations!
   if(ins == 1) {
      for(n=istart-1;n<iend;++n) {
         fgets(line,100,fp);
         *nline = *nline + 1;

         sscanf(line,"%lf %lf %lf",&x,&y,&z);

         cas->x[n] = x;
         cas->y[n] = y;
         cas->z[n] = z;
      }

   } else if(ins == 2) {

      for(n=istart-1;n<iend;++n) {
         fgets(line,100,fp);
         *nline = *nline + 1;

         sscanf(line,"%lf %lf %lf",&x,&y,&z);

         zp->x[n - istart+1] = x;
         zp->y[n - istart+1] = y;
         zp->z[n - istart+1] = z;
      }
   }

   if(ins != 0) {
      fgets(line,100,fp);
      *nline = *nline + 1;
      if(strncmp("))",line,2) != 0) {
         fprintf(stdout," e [inFluent_HandleNodes]  Could not find nodes-terminating line\n");
          return(1);
      } else {
         fprintf(stdout," i [inFluent_HandleNodes]  Read %ld nodes \n",iend-istart+1);
         zp->nstart = istart;
         zp->nend   = iend;
     //  fprintf(stdout,"          This zones's bounds %ld %ld \n",zp->nstart,zp->nend);
      }

      cas->nzone += 1;
   }

   return(0);
}


/*
 * Fucntion to handle the reading of faces from the case file
 * Ioannis Nompelis <nompelis@nobelware.com>       Created: 20110401
 * Ioannis Nompelis <nompelis@nobelware.com> Last modified: 20140516
 */

int inFluent_HandleFaces(struct my_fluentcase *cas, char *data,
                         long *nline, FILE *fp)
{
   int id,type,et;
   long istart,iend;
   size_t size;
   struct my_fluentzone *zp;
   long n;
   int ins;
   long in1,in2,in3,in4,ic1,ic2;
   char line[200];


   // Note that there are two posibilities!
   // (13 (0 first-index last-index 0))
   // (13 (zone-id first-index last-index type element-type))
   sscanf(data,"(13 (%x %lx %lx %x)",&id,&istart,&iend,&type);

   // for now die when a mixed element type is given
   if(id != 0) {
      // (13 (zone-id first-index last-index type element-type))
      sscanf(data,"(13 (%x %lx %lx %x %d)",&id,&istart,&iend,&type,&et);
      if(et == 0) {
         fprintf(stdout," e [inFluent_HandleFaces]  A face-zone with mixed elements types exists. \n");
         return(1);
      }
   }

   if(id == 0) {
      fprintf(stdout," i [inFluent_HandleFaces]  Case file provided total number of faces: %ld \n",iend);

      size = iend*sizeof(long);
      cas->ifnp = (long *) malloc(size*4);
      cas->ifep = (long *) malloc(size*2);
      if(cas->ifnp == NULL || cas->ifep == NULL) {
         fprintf(stdout," e [inFluent_HandleFaces]  Could not allocate memory for faces \n");
         if(cas->ifnp != NULL) free(cas->ifnp);
         if(cas->ifep != NULL) free(cas->ifep);
         cas->ifnp = NULL;
         cas->ifep = NULL;
      } else {
         fprintf(stdout," i [inFluent_HandleFaces]  Allocated %f MBs for faces \n",
                 ((double) (size*6)) / ((double) (1024*1024)) );

         cas->ifn = (ifn_t *) &(cas->ifnp[0]);
         cas->ife = (ife_t *) &(cas->ifep[0]);
      }

      cas->nfa = iend;
      ins = 0;
   } else {
      fprintf(stdout," i [inFluent_HandleFaces]  Found face-zone %d with bounds [%ld:%ld] \n",id,istart,iend);

      if(cas->nzone == cas->mzone) {
         fprintf(stdout," e [inFluent_HandleFaces]  Not enough preallocated zones (%d) \n",cas->mzone);
         return(1);
      } else {
         zp = (struct my_fluentzone *) &(cas->zones[cas->nzone]);

         zp->id   = id;  // this may have to be moved further down
         zp->type = 13;  // this may have to be moved further down
         zp->iattr = type;
      }

      if(cas->nfa <= 0) {
         zp->imem = 30;
         // Here I am speculating that there could be a possibility where
         // faces are part of a zone but the total number of faces was not
         // given in the case file. If true, I allocate space for the faces
         // as if they are part of this zone, which is what happens below.
         // If false, I should just return an error for out-of-order case items

         size = (iend - istart + 1);
         zp->ifnp = (long *) malloc(size*4);
         zp->ifep = (long *) malloc(size*2);

         if(zp->ifnp == NULL || cas->ifep == NULL) {
            fprintf(stdout," e [inFluent_HandleFaces]  Could not allocate memory for faces within a zone \n");
            if(zp->ifnp != NULL) free(zp->ifnp);
            if(zp->ifep != NULL) free(zp->ifep);
            zp->ifnp = NULL;
            zp->ifep = NULL;
            return(-1);
         } else {
            fprintf(stdout," i [inFluent_HandleFaces]  Allocated %f MBs for faces in zone %d \n",
                    ((double) (size*6)) / ((double) (1024*1024)) ,id);
         }

         ins = 2;
      } else {
         zp->imem = 3;
         fprintf(stdout," i [inFluent_HandleFaces]  Inserting faces in array\n");
         zp->ifnp = (long *) &(cas->ifnp[4*(istart-1)]);
         zp->ifep = (long *) &(cas->ifep[2*(istart-1)]);
         ins = 1;
      }
      zp->ifn = (ifn_t *) zp->ifnp;
      zp->ife = (ife_t *) zp->ifep;

   }


   // no error-trapping during reading operations!
   if(ins == 1) {
      for(n=istart-1;n<iend;++n) {
         fgets(line,100,fp);
         *nline = *nline + 1;

         in3 = -1;
         in4 = -1;

         if(et == 4) {
            sscanf(line,"%lx %lx %lx %lx %lx %lx",
                         &in1,&in2,&in3,&in4,&ic1,&ic2);
         } else if(et == 3) {
            sscanf(line,"%lx %lx %lx %lx %lx",
                         &in1,&in2,&in3,&ic1,&ic2);
         } else if(et == 2) {
            sscanf(line,"%lx %lx %lx %lx",
                         &in1,&in2,&ic1,&ic2);
         } else if(et == 0) {

// some decisions need to be made here before reading this line
//          sscanf(line,"%d %lx %lx %lx %lx %lx %lx",
//                       &ie,&in1,&in2,&in3,&in4,&ic1,&ic2);
         } else {
            fprintf(stdout," e [inFluent_HandleFaces]  Unknown element type! \n");
            return(1);
         }

         cas->ifn[n][0] = in1;
         cas->ifn[n][1] = in2;
         cas->ifn[n][2] = in3;
         cas->ifn[n][3] = in4;
         cas->ife[n][0] = ic1;
         cas->ife[n][1] = ic2;
/*  
printf("---> et=%d %lx %lx %lx %lx : %lx %lx \n",
et,
cas->ifn[n][0],
cas->ifn[n][1],
cas->ifn[n][2],
cas->ifn[n][3],
cas->ife[n][0],
cas->ife[n][1]);
  */
      }

   } else if(ins == 2) {

      for(n=istart-1;n<iend;++n) {
         fgets(line,100,fp);
         *nline = *nline + 1;

         in3 = -1;
         in4 = -1;

         if(et == 4) {
            sscanf(line,"%lx %lx %lx %lx %lx %lx",
                         &in1,&in2,&in3,&in4,&ic1,&ic2);
         } else if(et == 3) {
            sscanf(line,"%lx %lx %lx %lx %lx",
                         &in1,&in2,&in3,&ic1,&ic2);
         } else if(et == 2) {
            sscanf(line,"%lx %lx %lx %lx",
                         &in1,&in2,&ic1,&ic2);
         } else if(et == 0) {

// some decisions need to be made here before reading this line
//          sscanf(line,"%d %lx %lx %lx %lx %lx %lx",
//                       &ie,&in1,&in2,&in3,&in4,&ic1,&ic2);
         } else {
            fprintf(stdout," e [inFluent_HandleFaces]  Unknown element type! \n");
            return(1);
         }

         zp->ifn[n - istart+1][0] = in1;
         zp->ifn[n - istart+1][1] = in2;
         zp->ifn[n - istart+1][2] = in3;
         zp->ifn[n - istart+1][3] = in4;
         zp->ife[n - istart+1][0] = ic1;
         zp->ife[n - istart+1][1] = ic2;
/*  
printf("---> et=%d %lx %lx %lx %lx : %lx %lx \n",
et,
zp->ifn[n][0],
zp->ifn[n][1],
zp->ifn[n][2],
zp->ifn[n][3],
zp->ife[n][0],
zp->ife[n][1]);
  */
      }

   }

   if(ins != 0) {
      fgets(line,100,fp);
      *nline = *nline + 1;
      if(strncmp("))",line,2) != 0) {
         fprintf(stdout," e [inFluent_HandleFaces]  Could not find faces-terminating line\n");
          return(1);
      } else {
         fprintf(stdout," i [inFluent_HandleFaces]  Read %ld faces \n",iend-istart+1);
         zp->nstart = istart;
         zp->nend   = iend;
     //  fprintf(stdout,"          This zones's bounds %ld %ld \n",zp->nstart,zp->nend);
      }

      cas->nzone += 1;
   }

   return(0);
}


/*
 * Fucntion to read the number of dimenstions of the case file
 * Ioannis Nompelis <nompelis@nobelware.com>       Created: 20110330
 * Ioannis Nompelis <nompelis@nobelware.com> Last modified: 20110331
 */

int inFluent_HandleDims(struct my_fluentcase *cas, char *data) {

   sscanf(data,"(2 %d)",&(cas->type));

   fprintf(stdout," i [inFluent_HandleDims]  Case file is for %dD simulations \n",cas->type);

   return(0);
}


/*
 * Fucntion to determine whether this comment line is followed by a regular one
 * Ioannis Nompelis <nompelis@nobelware.com>       Created: 20160317
 * Ioannis Nompelis <nompelis@nobelware.com> Last modified: 20130618
 */

int inFluent_ShiftComment(char **data) {
   int ic = 0;
   int i;
   char *str;


//printf("CALLED \n");
   str = *data;

   // bypass the comment part
   i = 0;
   while( str[i] != ')' && str[i] != '\0' && str[i] != '\n' ) {
//printf("%c",str[i]);
      ++i;
   }
//printf("%c",str[i]);
//if(str[i] == '\n') printf("\n");
//if(str[i] == '\0') printf("\n");

   // test for another section after the comment
   while( str[i] != '(' && str[i] != '\0' && str[i] != '\n' ) {
//printf("%c",str[i]);
      ++i;
   }
//printf("%c",str[i]);
//if(str[i] == '\n') printf("\n");
//if(str[i] == '\0') printf("\n");

   // make pointer shift if another section exists
   if(str[i] == '(') {
      ic = 1;
//printf("\n");
      *data = (char *) &(str[i]);
   }

   return(ic);
}

/*
 * Fucntion to determine what a valid line in the case file describes
 * Ioannis Nompelis <nompelis@nobelware.com>       Created: 20110330
 * Ioannis Nompelis <nompelis@nobelware.com> Last modified: 20130618
 */

int inFluent_Readline(char **parse) {

   char *string;
   int len;
   int idebug = 0;
   char *data = *parse;


   string = "(0 ";
   len = 3;
   if(strncmp(data,string,len) == 0) {
      if(idebug == 1)
      printf("THIS IS A COMMENT LINE \n");
         if( inFluent_ShiftComment(parse) != 0) {
            return(inFluent_Readline(parse));
      } else {
         return(0);
      }
   }

   string = "(1 ";
   len = 3;
   if(strncmp(data,string,len) == 0) {
      if(idebug == 1)
      printf("THIS IS A HEADER LINE \n");
      return(1);
   }

   string = "(2 ";
   len = 3;
   if(strncmp(data,string,len) == 0) {
      if(idebug == 1)
      printf("THIS IS A DIMENSIONS LINE \n");
      return(2);
   }

   string = "(10 ";
   len = 4;
   if(strncmp(data,string,len) == 0) {
      if(idebug == 1)
      printf("THIS IS A NODES LINE \n");
      return(10);
   }

   string = "(12 ";
   len = 4;
   if(strncmp(data,string,len) == 0) {
      if(idebug == 1)
      printf("THIS IS A CELLS LINE \n");
      return(12);
   }

   string = "(13 ";
   len = 4;
   if(strncmp(data,string,len) == 0) {
      if(idebug == 1)
      printf("THIS IS A FACES LINE \n");
      return(13);
   }


   string = "(45 ";
   len = 4;
   if(strncmp(data,string,len) == 0) {
      if(idebug == 1)
      printf("THIS IS A PRESENTLY UNRECOGNIZED LINE \n");
      return(45);
   }


   // if we4 got this far it is an unrecognized line
   return(-1);
}


/*
 * Fucntion to sanitize a line read from the case file
 * Ioannis Nompelis <nompelis@nobelware.com>       Created: 20110330
 * Ioannis Nompelis <nompelis@nobelware.com> Last modified: 20110331
 */

int inFluent_Scanline(char *data, char *parse, int size) {

   int n,igot,m;


   memset(parse,'\0',size);

   // first sanitize the line and place it in the destination character array
   n = 0;
   m = -1;
   igot = 0;
   while(n<size && data[n] != '\n' && data[n] != '\0') {
      if(data[n] == '(') {
         igot = 1;
         if(m == -1) m = n;
      }

      if(igot == 1) {
         parse[n-m] = data[n];
      }

      ++n;
   }

   if(igot == 0) {
      return(-1);       // reached end of line but no endline character
   } else {
      // Now try to determine what type of line this is.
      // The purpose of doing this here is to do error trapping before we hand
      // over a "valid" line to the processing routines later on. This way, I
      // have only one place to worry about parsing and seg faults.
      // I changed my mind. I have a dedicated decision making routine, but I
      // will keep this here in case I want to combine the two.
      return(0);
   }
}


/*
 * Fucntion to organize the reading of the case file
 * Ioannis Nompelis <nompelis@nobelware.com>       Created: 20110330
 * Ioannis Nompelis <nompelis@nobelware.com> Last modified: 20160914
 */

int inFluent_ReadCase(struct my_fluentcase *cas, char *filename) {

   FILE *fp;
   int iterm = 0;
   char data[100],parse[100],*str;
   int ic,il,size = 100;
   int ierr;
   long nline = 0;
   struct timeval tv1,tv2;
   struct timezone tz;


   (void) gettimeofday(&tv1,&tz);

   fp = fopen(filename,"r");
   if(fp == NULL) {
      fprintf(stdout," e [inFluent_ReadCase]  Could not open file \"%s\" \n",filename);
      return(1);
   }
   fprintf(stdout," r [inFluent_ReadCase]  Reading file \"%s\" \n",filename);


   while(!feof(fp) && iterm != 1) {
      memset(data,'\0',100);
      fgets(data,100,fp);
      ++nline;

      ic = inFluent_Scanline(data,parse,size);

// The parser errors may be temporarily disabled
/**/
      if(parse[0] == '(' && parse[1] == '\n') {
         fprintf(stdout," e [inFluent_ReadCase]  The parser has failed. This is a bug or logic flaw!\n"); 
         fclose(fp);
         return(1);
      }
/**/

      if(ic == -1) {
         // this is either a blank line or a newline so we do nothing
      }
      if(ic == 0) {
         // this is a valid line
         fprintf(stdout,"   [%ld]    %s",nline,data);
         str = parse;
         il = inFluent_Readline(&str);

         if(il == 0) {
            // do nothing since this is a comment
         } else if(il == 1) {
            cas->header = (char *) malloc((strlen(str)+1)*sizeof(char));
            if(cas->header != NULL)  {
               sprintf(cas->header,"%s",str);
            } else {
               fprintf(stdout," e [inFluent_ReadCase]  Could not allocate memory for header (%ld bytes) \n", (long) (strlen(str)+1) );
               fclose(fp);
               return(-1);
            }
         } else if(il == 2) {
            inFluent_HandleDims(cas,str);
         } else if(il == 10) {   // nodes section
            ierr = inFluent_HandleNodes(cas,str,&nline,fp);
            if(ierr != 0) {
               fclose(fp);
               return(1);
            }
         } else if(il == 12) {   // cells section
            ierr = inFluent_HandleCells(cas,str,&nline,fp);
            if(ierr != 0) {
               fclose(fp);
               return(1);
            }
         } else if(il == 13) {   // faces section
            ierr = inFluent_HandleFaces(cas,str,&nline,fp);
            if(ierr != 0) {
               fclose(fp);
               return(1);
            }
         } else if(il == 45) {   // unknown (for now) section
            fprintf(stdout," i [inFluent_ReadCase]  Found the (presently) unknown section \"45\" \n");
         } else {    // this should be "-1" no other way
            fprintf(stdout," e [inFluent_ReadCase]  Unrecognized item in case file at line  %ld \n",nline);
            fclose(fp);
            return(1);
         }
      }
      if(ic == 1) {
         // this is an invalid line
         fprintf(stdout," e [inFluent_ReadCase]  Found a problematic line (# %ld) \n",nline);
         fprintf(stdout," ---> %s <----",data);
         fclose(fp);
         return(1);
      }
   }

   fclose(fp);

   (void) gettimeofday(&tv2,&tz);
   fprintf(stdout," i [inFluent_ReadCase]  Read file in %ld sec \n",
          (long) tv2.tv_sec - tv1.tv_sec);

   (void) inFluent_CaseInfo(cas);

   return(0);
}

int inFluent_ReadCase_(struct my_fluentcase *cas, char *filename) {
   return(inFluent_ReadCase(cas,filename));
}

int influent_readcase_(struct my_fluentcase *cas, char *filename) {
   return(inFluent_ReadCase(cas,filename));
}



/*
 * Fucntion to initialize the fluent case structures
 * Ioannis Nompelis <nompelis@nobelware.com>       Created: 20110330
 * Ioannis Nompelis <nompelis@nobelware.com> Last modified: 20110406
 */

int inFluent_InitCase(struct my_fluentcase *cas) {
   size_t size;


   cas->type = 0;

   cas->nno = 0;
   cas->nel = 0;
   cas->nfa = 0;

   cas->mzone = 100;
   cas->nzone = 0;

   size = cas->mzone * sizeof(struct my_fluentzone);

   cas->zones = (struct my_fluentzone *) malloc(size);
   if(cas->zones != NULL) {
      int n;

      for(n=0;n<cas->mzone;++n) {
         cas->zones[n].id = -1;
         cas->zones[n].type = -1;
      // cas->zones[n].nno = 0;
      // cas->zones[n].nel = 0;
      // cas->zones[n].nfa = 0;
         cas->zones[n].nstart = -1;
         cas->zones[n].nend = -1;
         cas->zones[n].imem = 0;
         cas->zones[n].x = NULL;
         cas->zones[n].y = NULL;
         cas->zones[n].z = NULL;
         cas->zones[n].ifn = NULL;
         cas->zones[n].ife = NULL;
      }


      fprintf(stdout," i [inFluent_InitCase]  Initialized FLUENT case structure\n");
      fprintf(stdout,"     Preallocated space for %d zones (reallocated automatically if exceeded) \n",cas->mzone);
   } else {
      cas->mzone = 0;
      fprintf(stdout," e [inFluent_InitCase]  Could not initialize FLUENT case structure (out of memory?) \n");
      return(1);
   }

   return(0);
}

int inFluent_InitCase_(struct my_fluentcase *cas) {
   return(inFluent_InitCase(cas));
}
int influent_initcase_(struct my_fluentcase *cas) {
   return(inFluent_InitCase(cas));
}


void inFluent_CaseInfo(struct my_fluentcase *cas) {

   int n,id,it,is,ia;


   fprintf(stdout,"   Summary of fluent case file contents\n");
   fprintf(stdout,"   - Total number of nodes:   %ld   (%.12lx) \n",cas->nno,cas->nno);
   fprintf(stdout,"   - Total number of cells:   %ld   (%.12lx) \n",cas->nel,cas->nel);
   fprintf(stdout,"   - Total number of faces:   %ld   (%.12lx) \n",cas->nfa,cas->nfa);
   fprintf(stdout,"   - Total number of zones:   %d \n",cas->nzone);
   fprintf(stdout,"   Zone information          \n");
   fprintf(stdout,"    Int. num.     ID-in-file        Type           Attrib \n");
   for(n=0;n<cas->nzone;++n) {
      id = cas->zones[n].id;
      it = cas->zones[n].type;
      is = cas->zones[n].type;
      if(is > 20) is = 0;
      ia = cas->zones[n].iattr;
      if(ia > 40 || ia < 0) ia = 0;

      fprintf(stdout,"     zone %d        id %d      type %d   (%s)     (%s) \n",
              n,id,it,fluent_types[is],fluent_attrib[ia]);
   }

}


/*
 * Fucntion to clear the fluent case structures
 * Ioannis Nompelis <nompelis@nobelware.com>       Created: 20160915
 * Ioannis Nompelis <nompelis@nobelware.com> Last modified: 20160915
 */

int inFluent_ClearCase(struct my_fluentcase *cas)
{
   char *FUNC = "inFluent_ClearCase";

   if(cas->zones != NULL) {
      int n;

      for(n=0;n<cas->nzone;++n) {
         if(cas->zones[n].imem == 10) {
            free(cas->zones[n].x);
            free(cas->zones[n].y);
            free(cas->zones[n].z);
            cas->zones[n].x = NULL;
            cas->zones[n].y = NULL;
            cas->zones[n].z = NULL;
         }
         if(cas->zones[n].imem == 30) {
            free(cas->zones[n].ifn);
            free(cas->zones[n].ife);
            cas->zones[n].ifn = NULL;
            cas->zones[n].ife = NULL;
            cas->zones[n].ifnp = NULL;
            cas->zones[n].ifep = NULL;
         }
         cas->zones[n].imem = 0;
         cas->zones[n].id = -1;
         cas->zones[n].type = -1;
      // cas->zones[n].nno = 0;
      // cas->zones[n].nel = 0;
      // cas->zones[n].nfa = 0;
         cas->zones[n].nstart = -1;
         cas->zones[n].nend = -1;
      }

      if(cas->zones != NULL) free(cas->zones);
   }

   if(cas->x != NULL) free(cas->x);
   cas->x = NULL;
   if(cas->y != NULL) free(cas->y);
   cas->y = NULL;
   if(cas->z != NULL) free(cas->z);
   cas->z = NULL;

   if(cas->ifn != NULL) free(cas->ifn);
   cas->ifn = NULL;
   cas->ifnp = NULL;
   if(cas->ife != NULL) free(cas->ife);
   cas->ife = NULL;
   cas->ifep = NULL;

   if(cas->header != NULL) free(cas->header);

   cas->mzone = 0;
   cas->nzone = 0;

   cas->nno = 0;
   cas->nel = 0;
   cas->nfa = 0;

   cas->type = 0;

   fprintf(stdout," i [%s]  Cleared Fluent case structure\n",FUNC);

   return(0);
}


#ifdef _USE_HDF
/******* HDF functions go here ********/

/*
 * Routine to create the HDF5 composite datatypes
 * (Taken from the parallel Fluent case file reader)
 * Ioannis Nompelis <nompelis@nobelware.com>       Created: 20140313
 * Ioannis Nompelis <nompelis@nobelware.com> Last modified: 20140324
 */

int inFluent_HDF_Startup( struct my_HDF_vars *hdfvars )
{
   char *FUNC = "inFluent_HDF_Startup";

   fprintf(stdout," i [%s]  Creating HDF5 composite datatypes\n",FUNC);


//--- creating the face HDF5 datatype

   hdfvars->h_face = H5Tcreate(H5T_COMPOUND, sizeof(diskface_t));

   H5Tinsert(hdfvars->h_face,"nc",   HOFFSET(diskface_t,nc),
             H5T_NATIVE_SHORT);

   H5Tinsert(hdfvars->h_face,"node0",   HOFFSET(diskface_t,node[0]),
             H5T_NATIVE_LONG);
   H5Tinsert(hdfvars->h_face,"node1",   HOFFSET(diskface_t,node[1]),
             H5T_NATIVE_LONG);
   H5Tinsert(hdfvars->h_face,"node2",   HOFFSET(diskface_t,node[2]),
             H5T_NATIVE_LONG);
   H5Tinsert(hdfvars->h_face,"node3",   HOFFSET(diskface_t,node[3]),
             H5T_NATIVE_LONG);

   H5Tinsert(hdfvars->h_face,"cell0",   HOFFSET(diskface_t,cell[0]),
             H5T_NATIVE_LONG);
   H5Tinsert(hdfvars->h_face,"cell1",   HOFFSET(diskface_t,cell[1]),
             H5T_NATIVE_LONG);


//--- creating the zone HDF5 datatype

   hdfvars->h_zone = H5Tcreate(H5T_COMPOUND, sizeof(diskzone_t));

   H5Tinsert(hdfvars->h_zone,"id",     HOFFSET(diskzone_t,id),
             H5T_NATIVE_INT);
   H5Tinsert(hdfvars->h_zone,"itype",  HOFFSET(diskzone_t,itype),
             H5T_NATIVE_INT);
   H5Tinsert(hdfvars->h_zone,"iattr",  HOFFSET(diskzone_t,iattr),
             H5T_NATIVE_INT);

   H5Tinsert(hdfvars->h_zone,"nstart", HOFFSET(diskzone_t,nstart),
             H5T_NATIVE_LONG);
   H5Tinsert(hdfvars->h_zone,"nend",   HOFFSET(diskzone_t,nend),
             H5T_NATIVE_LONG);


//--- creating the node HDF5 datatype

   hdfvars->h_node = H5Tcreate(H5T_COMPOUND, sizeof(disknode_t));

   H5Tinsert(hdfvars->h_node,"x",  HOFFSET(disknode_t,x),
             H5T_NATIVE_DOUBLE);
   H5Tinsert(hdfvars->h_node,"y",  HOFFSET(disknode_t,y),
             H5T_NATIVE_DOUBLE);
   H5Tinsert(hdfvars->h_node,"z",  HOFFSET(disknode_t,z),
             H5T_NATIVE_DOUBLE);

   // sizes for reads and writes
   hdfvars->isize_cio = -1;


   return(0);
}


/*
 * Fucntion to handle the conversion of nodes from the case file
 * Ioannis Nompelis <nompelis@nobelware.com>       Created: 20110401
 * Ioannis Nompelis <nompelis@nobelware.com> Last modified: 20140516
 */

int inFluent_HandleNodesHDF(struct my_fluentcase *cas, char *data,
                            long *nline, FILE *fp,
                            struct my_HDF_vars *hdfvars)
{

   char *FUNC = "inFluent_HandleNodesHDF";
   int id,nd;
   long istart,iend;
   struct my_fluentzone *zp;
   long n;
   int ins;
   double x,y,z;
   char line[200];

   hid_t    dataset, filespace, memspace;
   hsize_t  idim[1],icnt[1],ioff[1],istd[1],iblk[1];
   hsize_t  ibuf = 10000;  // throttling of writes
   herr_t   status;
   disknode_t *buf;
   int m;


   // not that it is like this:
   // (10 (zone-id first-index last-index type ND)(     OR
   // (10 (0       first-index last-index type ND))
   // where "type" is always zero
   sscanf(data,"(10 (%x %lx %lx 0 %d)",&id,&istart,&iend,&nd);

   if(id == 0) {
      fprintf(stdout," i [%s]  Case file provided total number of nodes: %ld \n",FUNC,iend);

      cas->nno = iend;

      idim[0] = cas->nno;
      filespace = H5Screate_simple(1,idim,idim);
      dataset = H5Dcreate(hdfvars->file_id, "nodes", hdfvars->h_node, filespace,
             H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
      H5Dclose(dataset);
      H5Sclose(filespace);
      printf(" i [%s]  Created nodes dataset in file \n",FUNC);

      ins = 0;
   } else {
      fprintf(stdout," i [%s]  Found node-zone %d with bounds [%ld:%ld] \n",FUNC,id,istart,iend);

      if(cas->nzone == cas->mzone) {
         fprintf(stdout," e [%s]  Not enough preallocated zones (%d) \n",FUNC,cas->mzone);
         return(1);
      } else {
         zp = (struct my_fluentzone *) &(cas->zones[cas->nzone]);

         zp->id   = id;  // this may have to be moved further down
         zp->type = 10;
         zp->iattr = 0;
      }

      if(cas->nno <= 0) {
         zp->imem = 10;
         // Here I am speculating that there could be a possibility where
         // nodes are part of a zone but the total number of nodes was not
         // given in the case file.

         // Need to create a dataset of appropriate size if it does not
         // already exist. Need to be picking up size and keep augmenting...




         ins = 2;
      } else {
         zp->imem = 1;
         ins = 1;
      }

      // allocate a buffer for read/writes
      buf = (disknode_t *) malloc(ibuf*sizeof(disknode_t));
      if(buf == NULL) {
         fprintf(stdout," e [%s]  Could not allocate node buffer \n",FUNC);
         return(-1);
      }
   }


   if(ins == 1) {
      // we need to open the dataset here
      dataset = H5Dopen(hdfvars->file_id, "nodes", H5P_DEFAULT);
      filespace = H5Dget_space(dataset);

      m = 0;
      ioff[0] = istart-1;
      for(n=istart-1;n<iend;++n) {
         fgets(line,100,fp);
         *nline = *nline + 1;

         sscanf(line,"%lf %lf %lf",&x,&y,&z);
         buf[m].x = x;
         buf[m].y = y;
         buf[m].z = z;

         m = m + 1;

         if(m == ibuf || n == iend-1) {

            istd[0] = 1;
            iblk[0] = 1;
            icnt[0] = m;

            memspace = H5Screate_simple(1,icnt,icnt);
            H5Sselect_hyperslab(filespace, H5S_SELECT_SET, ioff, istd, icnt, iblk);
            status = H5Dwrite(dataset, hdfvars->h_node, memspace, filespace,
                        H5P_DEFAULT, buf);
            H5Sclose(memspace);

            ioff[0] =  ioff[0] + icnt[0];
            m = 0;
         }
      }

      H5Sclose(filespace);
      H5Dclose(dataset);

   } else if(ins == 2) {

      for(n=istart-1;n<iend;++n) {
         fgets(line,100,fp);
         *nline = *nline + 1;

         sscanf(line,"%lf %lf %lf",&x,&y,&z);

      }
   }

   if(ins != 0) {
      fgets(line,100,fp);
      *nline = *nline + 1;
      if(strncmp("))",line,2) != 0) {
         fprintf(stdout," e [%s]  Could not find nodes-terminating line\n",FUNC);
         free(buf);
         return(1);
      } else {
         free(buf);
         fprintf(stdout," i [%s]  Read %ld nodes \n",FUNC,iend-istart+1);
         zp->nstart = istart;
         zp->nend   = iend;
     //  fprintf(stdout,"          This zones's bounds %ld %ld \n",zp->nstart,zp->nend);
      }

      cas->nzone += 1;
   }

   return(0);
}


/*
 * Fucntion to read the nodes in the case file
 * Ioannis Nompelis <nompelis@nobelware.com>       Created: 20130612
 * Ioannis Nompelis <nompelis@nobelware.com> Last modified: 20140516
 */

int inFluent_HandleCellsHDF(struct my_fluentcase *cas,
                            char *data, long *nline, FILE *fp,
                            struct my_HDF_vars *hdfvars)
{
   char *FUNC = "inFluent_HandleCellsHDF";
   int id,it;
   struct my_fluentzone *zp;
   long istart,iend;
   int ins;



   // note that it is like this:
   // (12 (0       first-index last-index type))
   // (12 (zone-id first-index last-index type XX))     OR
   sscanf(data,"(12 (%x %lx %lx %x)",&id,&istart,&iend,&it);

   if(id == 0) {
      fprintf(stdout," i [%s]  Case file provided total number of cells: %ld \n",FUNC,iend);

      cas->nel = iend;
      ins = 0;
   } else {
      fprintf(stdout," i [%s]  Found cell-zone %d with bounds [%ld:%ld] \n",FUNC,id,istart,iend);

      if(cas->nzone == cas->mzone) {
         fprintf(stdout," e [%s]  Not enough preallocated zones (%d) \n",FUNC,cas->mzone);
         return(1);
      } else {
         zp = (struct my_fluentzone *) &(cas->zones[cas->nzone]);

         zp->id   = id;  // this may have to be moved further down
         zp->type = 12;  // this may have to be moved further down
         zp->nstart = istart;
         zp->nend   = iend;
         zp->iattr  = 0;
      }

      if(cas->nel <= 0) {
         zp->imem = 20;

         ins = 2;
      } else {
         zp->imem = 2;
         ins = 1;
      }

   }


   if(ins != 0) {
      fprintf(stdout," i [%s]  Got info on %ld cells \n",FUNC,iend-istart+1);
      zp->nstart = istart;
      zp->nend   = iend;

      cas->nzone += 1;
   }

   return(0);
}



/*
 * Fucntion to handle the conversion of faces from the case file
 * Ioannis Nompelis <nompelis@nobelware.com>       Created: 20110401
 * Ioannis Nompelis <nompelis@nobelware.com> Last modified: 20140516
 */

int inFluent_HandleFacesHDF(struct my_fluentcase *cas, char *data,
                            long *nline, FILE *fp,
                            struct my_HDF_vars *hdfvars)
{

   char *FUNC = "inFluent_HandleFacesHDF";
   int id,type,et;
   long istart,iend;
   struct my_fluentzone *zp;
   long n;
   int ins;
   long in1,in2,in3,in4,ic1,ic2;
   char line[200];

   hid_t    dataset, filespace, memspace;
   hsize_t  idim[1],icnt[1],ioff[1],istd[1],iblk[1];
   hsize_t  ibuf = 10000;  // throttling of writes
   herr_t   status;
   diskface_t *buf;
   int m;


   // Note that there are two posibilities!
   // (13 (0 first-index last-index 0))
   // (13 (zone-id first-index last-index type element-type))
   sscanf(data,"(13 (%x %lx %lx %x)",&id,&istart,&iend,&type);

   // for now die when a mixed element type is given
   if(id != 0) {
      // (13 (zone-id first-index last-index type element-type))
      sscanf(data,"(13 (%x %lx %lx %x %d)",&id,&istart,&iend,&type,&et);
      if(et == 0) {
         fprintf(stdout," e [%s]  A face-zone with mixed elements types exists. \n",FUNC);
         return(1);
      }
   }

   if(id == 0) {
      fprintf(stdout," i [%s]  Case file provided total number of faces: %ld \n",FUNC,iend);

      cas->nfa = iend;

      idim[0] = cas->nfa;
      filespace = H5Screate_simple(1,idim,idim);
      dataset = H5Dcreate(hdfvars->file_id, "faces", hdfvars->h_face, filespace,
             H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
      H5Dclose(dataset);
      H5Sclose(filespace);
      printf(" i [%s]  Created faces dataset in file \n",FUNC);

      ins = 0;
   } else {
      fprintf(stdout," i [%s]  Found face-zone %d with bounds [%ld:%ld] \n",FUNC,id,istart,iend);

      if(cas->nzone == cas->mzone) {
         fprintf(stdout," e [%s]  Not enough preallocated zones (%d) \n",FUNC,cas->mzone);
         return(1);
      } else {
         zp = (struct my_fluentzone *) &(cas->zones[cas->nzone]);

         zp->id   = id;  // this may have to be moved further down
         zp->type = 13;
         zp->iattr = type;
      }

      if(cas->nfa <= 0) {
         zp->imem = 30;
         // here I am speculating that there could be a possibility where
         // faces are part of a zone but the total number of faces was not
         // given in the case file.

         // Need to create a dataset of appropriate size if it does not
         // already exist. Need to be picking up size and keep augmenting...



         ins = 2;
      } else {
         zp->imem = 3;
         ins = 1;
      }

      // allocate a buffer for read/writes
      buf = (diskface_t *) malloc(ibuf*sizeof(diskface_t));
      if(buf == NULL) {
         fprintf(stdout," e [%s]  Could not allocate face buffer \n",FUNC);
         return(-1);
      }

   }


   if(ins == 1) {
      // we need to open the dataset here
      dataset = H5Dopen(hdfvars->file_id, "faces", H5P_DEFAULT);
      filespace = H5Dget_space(dataset);

      m = 0;
      ioff[0] = istart-1;
      for(n=istart-1;n<iend;++n) {
         fgets(line,100,fp);
         *nline = *nline + 1;

         in3 = -1;
         in4 = -1;

         if(et == 4) {
            sscanf(line,"%lx %lx %lx %lx %lx %lx",
                         &in1,&in2,&in3,&in4,&ic1,&ic2);
         } else if(et == 3) {
            sscanf(line,"%lx %lx %lx %lx %lx",
                         &in1,&in2,&in3,&ic1,&ic2);
         } else if(et == 2) {
            sscanf(line,"%lx %lx %lx %lx",
                         &in1,&in2,&ic1,&ic2);
         } else if(et == 0) {

// some decisions need to be made here before reading this line
//          sscanf(line,"%d %lx %lx %lx %lx %lx %lx",
//                       &ie,&in1,&in2,&in3,&in4,&ic1,&ic2);
         } else {
            fprintf(stdout," e [%s]  Unknown element type! \n",FUNC);
            free(buf);
            return(1);
         }

         buf[m].nc = (short) et;
         buf[m].node[0] = in1;
         buf[m].node[1] = in2;
         buf[m].node[2] = in3;
         buf[m].node[3] = in4;
         buf[m].cell[0] = ic1;
         buf[m].cell[1] = ic2;

         m = m + 1;

         if(m == ibuf || n == iend-1) {

            istd[0] = 1;
            iblk[0] = 1;
            icnt[0] = m;

            memspace = H5Screate_simple(1,icnt,icnt);
            H5Sselect_hyperslab(filespace, H5S_SELECT_SET, ioff, istd, icnt, iblk);
            status = H5Dwrite(dataset, hdfvars->h_face, memspace, filespace,
                        H5P_DEFAULT, buf);
            H5Sclose(memspace);

            ioff[0] =  ioff[0] + icnt[0];
            m = 0;
         }
      }

      H5Sclose(filespace);
      H5Dclose(dataset);

   } else if(ins == 2) {
      // we need to open the dataset here
      dataset = H5Dopen(hdfvars->file_id, "faces", H5P_DEFAULT);
      filespace = H5Dget_space(dataset);

      m = 0;
      ioff[0] = istart-1;
      for(n=istart-1;n<iend;++n) {
         fgets(line,100,fp);
         *nline = *nline + 1;

         in3 = -1;
         in4 = -1;

         if(et == 4) {
            sscanf(line,"%lx %lx %lx %lx %lx %lx",
                         &in1,&in2,&in3,&in4,&ic1,&ic2);
         } else if(et == 3) {
            sscanf(line,"%lx %lx %lx %lx %lx",
                         &in1,&in2,&in3,&ic1,&ic2);
         } else if(et == 2) {
            sscanf(line,"%lx %lx %lx %lx",
                         &in1,&in2,&ic1,&ic2);
         } else if(et == 0) {

// some decisions need to be made here before reading this line
//          sscanf(line,"%d %lx %lx %lx %lx %lx %lx",
//                       &ie,&in1,&in2,&in3,&in4,&ic1,&ic2);
         } else {
            fprintf(stdout," e [%s]  Unknown element type! \n",FUNC);
            free(buf);
            return(1);
         }

         buf[m].nc = (short) et;
         buf[m].node[0] = in1;
         buf[m].node[1] = in2;
         buf[m].node[2] = in3;
         buf[m].node[3] = in4;
         buf[m].cell[0] = ic1;
         buf[m].cell[1] = ic2;

         m = m + 1;

         if(m == ibuf || n == iend-1) {

            istd[0] = 1;
            iblk[0] = 1;
            icnt[0] = m;

            memspace = H5Screate_simple(1,icnt,icnt);
            H5Sselect_hyperslab(filespace, H5S_SELECT_SET, ioff, istd, icnt, iblk);
            status = H5Dwrite(dataset, hdfvars->h_face, memspace, filespace,
                        H5P_DEFAULT, buf);
            H5Sclose(memspace);

            ioff[0] =  ioff[0] + icnt[0];
            m = 0;
         }
      }

      H5Sclose(filespace);
      H5Dclose(dataset);

   }

   if(ins != 0) {
      fgets(line,100,fp);
      *nline = *nline + 1;
      if(strncmp("))",line,2) != 0) {
         fprintf(stdout," e [%s]  Could not find faces-terminating line\n",FUNC);
          free(buf);
          return(1);
      } else {
          free(buf);
         fprintf(stdout," i [%s]  Read %ld faces \n",FUNC,iend-istart+1);
         zp->nstart = istart;
         zp->nend   = iend;
     //  fprintf(stdout,"          This zones's bounds %ld %ld \n",zp->nstart,zp->nend);
      }

      cas->nzone += 1;
   }

   return(0);
}


/*
 * Fucntion to read the Fluent Case file and immediately convert to HDF5
 * Ioannis Nompelis <nompelis@nobelware.com>       Created: 20140313
 * Ioannis Nompelis <nompelis@nobelware.com> Last modified: 20160915
 */
int inFluent_ConvertCaseHDF(struct my_fluentcase *cas, char *filename,
                            struct my_HDF_vars *hdfvars) {

   char *FUNC ="inFluent_ConvertCaseHDF";
   FILE *fp;
   int iterm = 0;
   char data[100],parse[100],*str;
   int ic,il,size = 100;
   long nline = 0;
   int ierr;

   hid_t    dataset, filespace, memspace;
   hsize_t  idim[1];
   herr_t   status;
   char hfname[256];             // for unix, infinity is set to 256
   diskzone_t *buf;
   int m;
   struct timeval tv1,tv2;
   struct timezone tz;


   (void) gettimeofday(&tv1,&tz);

   fp = fopen(filename,"r");
   if(fp == NULL) {
      fprintf(stdout," e [%s]  Could not open file \"%s\" \n",FUNC,filename);
      return(1);
   }
   fprintf(stdout," r [%s]  Reading file \"%s\" \n",FUNC,filename);

   sprintf(hfname,"%s.h5",filename);
   hdfvars->file_id = H5Fcreate(hfname, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
   // need to put error trapping here


   while(!feof(fp) && iterm != 1) {
      memset(data,'\0',100);
      fgets(data,100,fp);
      ++nline;

      ic = inFluent_Scanline(data,parse,size);

// The parser errors may be temporarily disabled
/**/
      if(parse[0] == '(' && parse[1] == '\n') {
         fprintf(stdout," e [%s]  The parser has failed. This is a bug or logic flaw!\n",FUNC); 
         H5Fclose(hdfvars->file_id);
         fclose(fp);
         return(1);
      }
/**/
      if(ic == -1) {
         // this is either a blank line or a newline so we do nothing
      }
      if(ic == 0) {
         // this is a valid line
         fprintf(stdout,"   [%ld]    %s",nline,data);
         str = parse;
         il = inFluent_Readline(&str);

         if(il == 0) {
            // do nothing since this is a comment
         } else if(il == 1) {
            cas->header = (char *) malloc((strlen(str)+1)*sizeof(char));
            if(cas->header != NULL) {
               sprintf(cas->header,"%s",str);
            } else {
               fprintf(stdout," e [inFluent_ReadCase]  Could not allocate memory for header (%ld bytes) \n", (long) (strlen(str)+1) );
               H5Fclose(hdfvars->file_id);
               fclose(fp);
               return(-1);
            }
         } else if(il == 2) {
            inFluent_HandleDims(cas,str);
         } else if(il == 10) {   // nodes section
            ierr = inFluent_HandleNodesHDF(cas,str,&nline,fp, hdfvars);
            if(ierr != 0) {
               H5Fclose(hdfvars->file_id);
               fclose(fp);
               return(1);
            }
         } else if(il == 12) {   // cells section
            ierr = inFluent_HandleCellsHDF(cas,str,&nline,fp, hdfvars);
            if(ierr != 0) {
               H5Fclose(hdfvars->file_id);
               fclose(fp);
               return(1);
            }
         } else if(il == 13) {   // faces section
            ierr = inFluent_HandleFacesHDF(cas,str,&nline,fp, hdfvars);
            if(ierr != 0) {
               H5Fclose(hdfvars->file_id);
               fclose(fp);
               return(1);
            }
         } else if(il == 45) {   // unknown (for now) section
            fprintf(stdout," i [%s]  Found the (presently) unknown section \"45\" \n",FUNC);
         } else {    // this should be "-1" no other way
            fprintf(stdout," e [%s]  Unrecognized item in case file at line  %ld \n",FUNC,nline);
            H5Fclose(hdfvars->file_id);
            fclose(fp);
            return(1);
         }
      }
      if(ic == 1) {
         // this is an invalid line
         fprintf(stdout," e [%s]  Found a problematic line (# %ld) \n",FUNC,nline);
         fprintf(stdout," ---> %s <----",data);
         return(1);
         H5Fclose(hdfvars->file_id);
         fclose(fp);
      }
   }

   fclose(fp);


   // write zone array to file
   buf = (diskzone_t *) malloc(cas->nzone*sizeof(diskzone_t));
   if(buf == NULL) {
      fprintf(stdout," e [%s]  Could not allocate zones array \n",FUNC);
      H5Fclose(hdfvars->file_id);
      fclose(fp);
      return(-1);
   }

   for(m=0;m<cas->nzone;++m) {
      buf[m].id     = cas->zones[m].id;
      buf[m].itype  = cas->zones[m].type;
      buf[m].iattr  = cas->zones[m].iattr;
      buf[m].nstart = cas->zones[m].nstart;
      buf[m].nend   = cas->zones[m].nend;
   }

   idim[0] = cas->nzone;
   filespace = H5Screate_simple(1,idim,idim);
   dataset = H5Dcreate(hdfvars->file_id, "zones", hdfvars->h_zone, filespace,
             H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
   memspace = H5Screate_simple(1,idim,idim);
   status = H5Dwrite(dataset, hdfvars->h_zone, memspace, filespace,
                        H5P_DEFAULT, buf);
   H5Sclose(memspace);
   H5Dclose(dataset);
   H5Sclose(filespace);
   printf(" i [%s]  Created zones dataset in file \n",FUNC);

   free(buf);

   H5Fclose(hdfvars->file_id);

   (void) gettimeofday(&tv2,&tz);
   fprintf(stdout," i [%s]  Read file in %ld sec \n",FUNC,
          (long) tv2.tv_sec - tv1.tv_sec);

   (void) inFluent_CaseInfo(cas);

   return(0);
}
#endif


