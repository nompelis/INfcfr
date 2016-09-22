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

#ifdef _USE_HDF
#include "hdf5_struct.h"

/*
 * Routine to create the HDF5 composite datatypes
 */

int influent_HDF_Startup( struct my_HDF_vars *hdfvars )
{
   char *FUNC = "inparfluent_HDF_Startup";

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




/*
 * sizes for reads and writes
 */
   hdfvars->isize_cio = -1;


   return(0);
}

#endif

