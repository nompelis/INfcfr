/***********************************************************************/
/* Routines to define and read a "simple" Fluent case file. This code  */
/* is not fully Fluent case format compliant. It is my adoptation of   */
/* the Fluent case format for use in my codes.                         */
/*                                                                     */
/* Version 1.9                                                         */
/* Copyright 2011-2018 Ioannis Nompelis <nompelis@nobelware.com>       */
/***********************************************************************/

/**************************************************************************
 Copyright (c) 2011-2018, Ioannis Nompelis
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

#ifndef _FLUENT_H_
#define _FLUENT_H_

/*
 * structure declarations and function prototypes for fluent.c
 * which is a set of routines for handling Fluent case files
 * Ioannis Nompelis <nompelis@nobelware.com>       Created: 20110330
 * Ioannis Nompelis <nompelis@nobelware.com> Last modified: 20180129
 */

typedef long ifn_t [4];
typedef long ife_t [2];

struct my_fluentzone {
   int id;                  // ID of zone in the Fluent case
   int imem;                // 1 = nodes, 2 = cells, 3 = faces
                            // for 10,20,30 the memory is locally allocated
   int type;                // indicates cells/nodes/faces (10,12,13, etc)
   int iattr;               // this is the "type" as it pertains to Fluent
// long nno,nel,nfa;        // cell, node and face totals for this zone
   long nstart,nend;        // starting and ending cells for this zone
   long *ifnp;              // pointer for memory handling
   long *ifep;              // pointer for memory handling
   ifn_t *ifn;              // array of node numbers (4) indexed by faces
   ife_t *ife;              // array of left/right elements indexed by faces
   double *x,*y,*z;         // nodes coefficients (physical coordinates)
};

struct my_fluentcase {
   char *header;
   int type;
   int nzone,mzone;
   struct my_fluentzone *zones;
   long nno,nel,nfa;
   long *ifnp;
   long *ifep;
   ifn_t *ifn;
   ife_t *ife;
   double *x,*y,*z;
};

// Function prototypes that represent the API

int inFluent_InitCase( struct my_fluentcase *cp );

void inFluent_CaseInfo( struct my_fluentcase *cp );

int inFluent_ClearCase( struct my_fluentcase *cp );

int inFluent_ReadCase( struct my_fluentcase *cp, char *filename );

int inFluent_OutputPlot3d(struct my_fluentcase *cas, int nblock,
                          long *im, long *jm, long *km, char *filename);

#endif

