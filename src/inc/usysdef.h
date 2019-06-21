/*********************************************************************
**    NAME         : usysdef.h 
**       CONTAINS:
**      all system wide defines. 
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       usysdef.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:07:06
*********************************************************************/

#ifndef USYSDEFH

/*
.....added this for this file used by C++ file
.....Yurong
*/
#undef EXTX
#ifdef __cplusplus 
#define EXTX extern "C"
#else
#define EXTX extern
#endif

/* if define following line, then when we try to
.....include <Windows.h>, <GL/gl.h> will have problem,
.....My guess is the have internal defined this variable
.....use SINGL & DOUBL instead
.....Yurong 4/6/00
*/
/*
#define	SINGLE 1
#define 	DOUBLE 2
*/
#define	SINGL 1
#define 	DOUBL 2


#ifdef UU_DOUBLE
#define	UU_PREC	DOUBL
#else
#define	UU_PREC	SINGL
#endif
#ifdef UU_SINGLE 
#include "usfltdef.h"
#else
#include "usdbldef.h"
#endif

#if (UU_COMP==UU_RIDGE) || (UU_COMP == UU_WIN2K)
#define bcopy(from,to,n) memcpy(to,from,n)
#endif

/*	-- Application specifcation Section -- */

#define UU_UNICAD 0 							/* Old Unicad Application */
#define UU_SOLIDS 1							/* DDC with Solids */
#define UU_DDC    2							/* DDC without Solids */
#define UU_UIDS   3							/* UIDS Tool */
#define UU_NCLCADD 2						/* NCLCADD - same as DDC */
#define UU_NCLCAM 4							/* NCLCAM */
#define UU_NCLNIS 5							/* NIS - NCL502 Interactive Shell */

#define UU_NCLPLUS 6
#define UU_NCLSIGNON 7
#define UU_NCLIPV 8

#define NULLKEY ((UU_KEY_ID) 0)

EXTX int UU_application;					/* application flag */
char *uu_malloc();
char *uu_toolmalloc();

EXTX char *uu_lsnew();
char *uu_lsnext();
char *uu_lsprev();
char *uu_lsend();
EXTX char *uu_lsinsrt(char *,int);
char *uu_lsdele();
char *uu_lsempty();

char *uu_uprompt0();
char *uu_uprompt1();
char *uu_uprompt2();

#define USYSDEFH
#endif
