/*********************************************************************
**
**    NAME         :  zunitut.h
**
**       CONTAINS:
**       	DD1 tutorial definition file
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**       zunitut.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:26
**
*********************************************************************/

#ifndef ZUNITUTH


#include "usysdef.h"
#include "dtuto.h"
#include "ustdio.h"

#if (UU_COMP == UU_SUN)
#define PUCKTUT "zmouse.tut"
#else
#define PUCKTUT "zpuck.tut"
#endif

#if (UU_COMP == UU_SUN)
#define FKBTUT "zsunkeys.tut"
#else
#define FKBTUT "ztekkeys.tut"
#endif

static UD_TUTO UZ_pisignon = { 0, "zsignon.tut", 3, NULL };
static UD_TUTO UZ_pisignof = { 0, "zsignoff.tut", 1, NULL };
static UD_TUTO UZ_piexit = { 0, "zexit.tut", 2, NULL };

static UD_TOPICS UZ_pitutontxt[9] =
                      { {"ztuthow.tut", 2},
							  {"zmenus.tut",   3},
							  {PUCKTUT,   		 1},
							  {FKBTUT,    		 3},
							  {"zdata.tut",    3},
							  {"zmodes.tut",   4},
							  {"zforms.tut",   3},
							  {"zsubsys.tut",  2},
							  {"zcordsys.tut", 3} } ;

static UD_TUTO UZ_pituton = { 9, "ztuton.tut", 1, UZ_pitutontxt };

static UD_TOPICS UZ_pirecontxt[4] =
                      { {"zrcpb1.tut", 2},
                       {"zrcpb2.tut", 2},
                       {"zrcpb3.tut", 1},
                       {"zrcpb4.tut", 2} } ;

static UD_TUTO   UZ_pirecon = {4, "zrcpb0.tut", 1, UZ_pirecontxt };

static UD_TOPICS UZ_pirecofftxt[4] =
                      { {"zrcpb1.tut", 2},
                       {"zrcpb2.tut", 2},
                       {"zrcpb3.tut", 1},
                       {"zrcpb4.tut", 2} } ;

static UD_TUTO   UZ_pirecoff = {4, "zrcpb0.tut", 1, UZ_pirecofftxt };

static UD_TOPICS UZ_piplabcktxt[4] =
                       { {"zrcpb1.tut", 2},
                       {"zrcpb2.tut", 2},
                       {"zrcpb3.tut", 1},
                       {"zrcpb4.tut", 2} } ;

static UD_TUTO   UZ_piplabck = {4, "zrcpb0.tut", 1, UZ_piplabcktxt };

#define ZUNITUTH
#endif
