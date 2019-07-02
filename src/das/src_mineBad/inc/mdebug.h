
/*********************************************************************
**    NAME         :  mdebug.h
**       CONTAINS: modeling specific debugging information
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       mdebug.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:06:29
*********************************************************************/

#ifndef UM_MDEBUG

/***********************************************************************
*
*		The following 3 definitions are flags for the modeling debug
*		procedure, "p_ary"; they indicate whether to output values as
*		integers, floats, or hex respectively.
*
***********************************************************************/

#include "ustdio.h"

#define UM_PINT 1
#define UM_PFLOAT 2
#define UM_PHEX 3
#define UM_PLOGICAL 4

/***********************************************************************
*
*	The following external is used as a string for printing data.
*
***********************************************************************/
#ifdef UM_MPGM
#define EXT
#else
#define EXT extern
#endif

EXT	char	UM_sbuf[256];
EXT	FILE	*UM_psfile;

/***********************************************************************
*
*	The following macro is used to to print failure in the in the "trc"
*	file if a function fails and the modelling trace is on.
*
***********************************************************************/

#ifdef UU_DEBUGON
#define UM_IF_FAILURE_PRINT_IT                       \
	if (status != UU_SUCCESS)                         \
	{                                                 \
		char us[120];                                  \
		uu_denter2(UU_MTRC,(us, "returned FAILURE"));  \
		uu_dexit;                                      \
	}                                                 
#else
#define UM_IF_FAILURE_PRINT_IT
#endif

#undef EXT
#define UM_MDEBUG
#endif


