/*********************************************************************
**
**    NAME         :  zfeatut.h
**
**       CONTAINS:
**       	DD1 tutorial definition file
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**       zfeatut.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:25
**
*********************************************************************/

#ifndef ZFEATUTH


#include "dtuto.h"
#include "ustdio.h"

static UD_TOPICS  UZ_pifeaturtxt[6] =
                       { {"zfeat1.tut", 2},
                        {"zfeat2.tut", 2},
                        {"zfeat3.tut", 1},
                        {"zfeat4.tut", 2},
                        {"zfeat5.tut", 2},
                        {"zfeat6.tut", 1} } ;
							
static UD_TUTO  UZ_pifeatur = { 6, "zfeat0.tut", 1, UZ_pifeaturtxt };

#define ZFEATUTH
#endif
