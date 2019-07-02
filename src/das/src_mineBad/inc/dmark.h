/*********************************************************************
**
**    NAME         :  dmark.h
**
**       CONTAINS:
**
**              MARK and UNMARK mechanism
**
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       dmark.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:06:13
**
*********************************************************************/

/*      ---------------- MARK stack maintenance ------------------*/

#ifndef DMARKH

#include "usetjmp.h"
#include "usysdef.h"
#include "udebug.h"

/*
.....changed for this file used by C++
*/
#ifdef DPGM
#define EXT 
#else
#ifdef __cplusplus 
#define EXT extern "C"
extern "C" int ud_markover();
extern "C" int ud_jmpmark(int val);
#else
#define EXT extern
#endif
#endif

/******** -- setjump and longjump section -- ********/

#define UD_MARKLEVEL 300                                /*  number of levels this system can mark */

	typedef struct
	{
		UU_LOGICAL stopflag;                            /* if UU_TRUE, then stop jumping */
		int debugsave;                                          /* save debug stack pointer */
		jmp_buf markenv;                                        /* mark environment */
	} UD_markstkrec;

#ifdef DPGM
	int UD_enablejmp = UU_FALSE;            /* do not enable jumping */
	int UD_markptr = 0;                                     /* mark stack pointer */
#else
	EXT int UD_enablejmp;
	EXT int UD_markptr;
#endif

	EXT UD_markstkrec UD_markstk[UD_MARKLEVEL];             /* mark stack */

	extern int UU_stklen;

#define UD_MARK(Val, Flag)      if(UD_markptr >= UD_MARKLEVEL) \
										ud_markover(); \
									UD_markstk[UD_markptr].stopflag = Flag; \
									UD_markstk[UD_markptr].debugsave = UU_stklen; \
					uu_dprint(UU_DTRC,(us, "in MARK, mrkptr=%d", UD_markptr)); \
									UD_markptr++; \
									Val = setjmp(UD_markstk[UD_markptr-1].markenv)

#define UD_UNMARK(Val) \
	uu_dprint(UU_DTRC,(us, "in UNMARK, mrkptr=%d, val=%d", UD_markptr, Val)); \
		ud_jmpmark(Val)

#undef EXT
#define DMARKH

#endif

