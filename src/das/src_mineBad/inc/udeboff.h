/*********************************************************************
**
**		NAME			: udeboff.h
**    CONTAINS    :  uu_denter(xTRC(us,"format",v1,v2,..));
**							 uu_denter2(xTRC,(us,"format",v1,v2,..));
**							 uu_dexit;
**
**			When included these definitions completely disable the
**		Unicad debugging system.  To enable the system, include udebon.h.
**
**    COPYRIGHT 2000 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       udeboff.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:07:03
**
*********************************************************************/


#ifndef UDEBOFFH
#include "umasks.h"

/* Define the required global variables */
#ifdef __cplusplus 
#define EXT extern "C"
#else
#define EXT extern
#endif

#ifdef UU_DEBUGMAIN
	int 		UU_stklen=0;					/* Current stack length */
	char 		ustk[UU_STKDEPTH][150];		/* Stack of strings */
	char 		*(addstk[UU_STKDEPTH]);		/* Stack of string addresses */
	int 		UU_debmask;						/* Debug mask */
#else
	EXT int  UU_stklen;					/* Current stack length */
	EXT char ustk[UU_STKDEPTH][150];	/* Stack of strings */
	EXT char *(addstk[UU_STKDEPTH]);	/* Stack of string addresses */
	EXT int  UU_debmask;					/* Debug mask */
#endif

#define UU_DEBUG 0

#define uu_denter(mask,x) 
#define uu_dexit 
#define uu_denter2(mask,x) 
#define uu_dprint(mask,x) 
#define uu_dexitstatus(routine_name, status)

#define UDEBOFFH
#endif

