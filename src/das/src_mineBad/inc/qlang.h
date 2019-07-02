/*********************************************************************
**    NAME         :  qlang.h
**       CONTAINS:
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       qlang.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:41
*********************************************************************/

#ifndef QLANGH

#include		"usysdef.h"
#include		"uims.h"
#include		"cdef.h"

#ifdef QMAIN

#define EXT
	int  UQ_cendindex = 0;
	int  uq_calc2flag=UU_FALSE;   /* flag for second uq_calc call */
	int  UQI_clxconflag=UU_FALSE;	/* flag to detect lexicon has been read */
	int  UQI_angflag = UQ_DEG;		/* flag to show what the input angle is  */

#else

#define EXT extern
	EXT int  UQ_cendindex;
	EXT int  uq_calc2flag;   	/* flag for second uq_calc call */
	EXT int  UQI_clxconflag;	/* flag to detect whether lexicon has been read */
	EXT int  UQI_angflag;		/* flag to show what the input angle is  */

#endif

	EXT int *UQ_lexicon;
	EXT int UQ_symbol[10],		/* name of distinguished symbol, such as
											<s>, in internal langpak code */
			  uq_symlength;		/* length of symbol */
	EXT int UQ_trace;				/* 1=tracing on, 0=no tracing */
	EXT int UQ_cfnindex;
	EXT int UQ_cendflag[21];
	EXT int uq_calc2rslt;		/* flag to check whether uq_calc2 gets result */
	EXT UU_REAL  UQI_cdval[4];
	EXT int UQI_cdebug;
	EXT int UQI_sdebug;			/* debug flag for symtb.c file     */
	EXT int UQI_cer1;				/* error flag for undefined symbol */
	EXT int UQI_cer2;				/* error flag for common errors    */
	EXT int UQI_cfuncflag;		/* flag to show the function parsing is on */
	EXT int UQI_cinptr;			/* pointer to next symbol in the "in"  */
	EXT int UQI_cstkpt;			/* stack pointer                   */
	EXT int UQI_cfpar;			/* index for function name stack     */

#define	QLANGH
#undef EXT

#endif
