/*********************************************************************
**    NAME         :  umoveb.h
**       CONTAINS:
**       system dependent macro definitions for uu_move_byte
**    COPYRIGHT 1986 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       umoveb.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:05
*********************************************************************/

#include <stdio.h>
#include "umath.h"
#include "usysdef.h"

/**************************************************/
/** machine dependant fast byte copy routines    **/
/**                                              **/
/**  NOTE: UU_COMP definitions found in          **/
/**        "usysdef.h"                           **/
/**************************************************/

#if (UU_COMP==UU_RIDGE)
#define uu_move_byte(from,to,n)     memcpy(to,from,n)
#endif
#if (UU_COMP==UU_TEK)
#define uu_move_byte(from,to,n)     (bcopy(from,to,n),to)
#endif
#if ((UU_COMP==UU_SUN) || (UU_COMP==UU_SUN2))
#define uu_move_byte(from,to,n)     (bcopy(from,to,n),to)
#endif
#if (UU_COMP==UU_IRIS)
#define uu_move_byte(from,to,n)     (blt(to,from,n),to)
#endif
#if (UU_COMP==UU_IRIS4D)
#define uu_move_byte(from,to,n)     (bcopy(from,to,n),to)
#endif
#if (UU_COMP==UU_HPUX)
#define uu_move_byte(from,to,n)     (bcopy(from,to,n),to)
#endif
#if (UU_COMP==UU_WINNT)
#define uu_move_byte(from,to,n)     (bcopy(from,to,n),to)
#endif
#if (UU_COMP==UU_PYRAMID)
#define uu_move_byte(from,to,n)     (bcopy(from,to,n),to)
#endif
#if (UU_COMP==UU_VAXULTRIX)
#define uu_move_byte(from,to,n)     (bcopy(from,to,n),to)
#endif
#if (UU_COMP==UU_MASSCOMP)
#define uu_move_byte(from,to,n)     (bcopy(from,to,n),to)
#endif

/* #ifdef WNT */
#if UU_COMP==UU_WIN2K
#define uu_move_byte(from,to,n)     (bcopy(from,to,n),to)
#endif
#if UU_COMP==UU_AIX64
#define uu_move_byte(from,to,n)     (bcopy(from,to,n),to)
#endif

/* add new machines here */
#ifndef uu_move_byte

	/* if uu_move_byte is not defined here the C source in umoveb.c */
	/* will be used instead. */
	extern char	*uu_move_byte();
#endif

