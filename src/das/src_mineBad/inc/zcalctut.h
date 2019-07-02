/*********************************************************************
**
**    NAME         :  zcalctut.h
**
**       CONTAINS:
**       	DD1 tutorial definition file
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**       zcalctut.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:24
**
*********************************************************************/

#ifndef ZCALCTUTH


#include "dtuto.h"
#include "ustdio.h"

static UD_TOPICS UZ_picalcultxt[8] =
                       { {"zcalc1.tut", 4},
                        {"zcalc2.tut", 3},
                        {"zcalc3.tut", 4},
                        {"zcalc4.tut", 3},
                        {"zcalc5.tut", 4},
                        {"zcalc6.tut", 4},
                        {"zcalc7.tut", 3},
                        {"zcalc8.tut", 1} };
							
static UD_TUTO UZ_picalcul = { 8, "zcalc0.tut", 1, UZ_picalcultxt };

#define ZCALCTUTH
#endif
