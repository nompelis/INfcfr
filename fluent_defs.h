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

#ifndef _FLUENT_DEFS_H_
#define _FLUENT_DEFS_H_

/*
 * Types of zones that can be encountered in the file 
 */

static char *fluent_types[] =
{
 "UNKNOWN",                 //  0
 "(noname)",                //  1
 "(noname)",                //  2
 "(noname)",                //  3
 "(noname)",                //  4
 "(noname)",                //  5
 "(noname)",                //  6
 "(noname)",                //  7
 "(noname)",                //  8
 "(noname)",                //  9
 "nodes",               // 10
 "(noname)",                // 11
 "cells",               // 12
 "faces",               // 13
 "(noname)",                // 14
 "(noname)",                // 15
 "(noname)",                // 16
 "(noname)",                // 17
 "(noname)",                // 18
 "(noname)",                // 19
 "(noname)",                // 20
};


/*
 * "Attributes" of a given zone type (dictates BCs for the most part)
 */

static char *fluent_attrib[] =
{
 "UNKNOWN",                //  0
 "UNKNOWN",                //  1
 "Interior",               //  2
 "Wall",                   //  3
 "UNKNOWN",                //  4
 "UNKNOWN",                //  5
 "UNKNOWN",                //  6
 "Symmetry",               //  7
 "UNKNOWN",                //  8
 "UNKNOWN",                //  9
 "Inflow",                 // 10
 "UNKNOWN",                // 11
 "UNKNOWN",                // 12
 "UNKNOWN",                // 13
 "UNKNOWN",                // 14
 "UNKNOWN",                // 15
 "UNKNOWN",                // 16
 "UNKNOWN",                // 17
 "UNKNOWN",                // 18
 "UNKNOWN",                // 19
 "UNKNOWN",                // 20
 "UNKNOWN",                // 21
 "UNKNOWN",                // 22
 "UNKNOWN",                // 23
 "UNKNOWN",                // 24
 "UNKNOWN",                // 25
 "UNKNOWN",                // 26
 "UNKNOWN",                // 27
 "UNKNOWN",                // 28
 "UNKNOWN",                // 29
 "UNKNOWN",                // 30
 "UNKNOWN",                // 31
 "UNKNOWN",                // 32
 "UNKNOWN",                // 33
 "UNKNOWN",                // 34
 "UNKNOWN",                // 35
 "Outflow",                // 36
 "UNKNOWN",                // 37
 "UNKNOWN",                // 38
 "UNKNOWN",                // 39
 "UNKNOWN",                // 40
};



/*
 * Definitions that are used for data management
 */
#define INPAR_FLUENT_CASE_NULL           0
#define INPAR_FLUENT_CASE_FILE           1
#define INPAR_FLUENT_CASE_DIST           2

#endif

