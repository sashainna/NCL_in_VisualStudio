/*********************************************************************
**    NAME         :  derror.h
**       CONTAINS:
**       	Help, Prompt, and Error common
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       derror.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:06:13
*********************************************************************/

#ifndef DERRORH
#ifdef EPGM
#define EXT    
#else
#ifdef __cplusplus 
#define EXT extern "C"
#else
#define EXT extern
#endif
#endif

#include "usysdef.h"

	typedef enum
	{
		UD_APPSTATE,								/* Application DAS state */
		UD_UNISTATE									/* Unicad DAS state */
	} UD_DASSTATE;

#ifdef EPGM
	int UD_syssys = 0;							/* Subsystem System number */
	int UD_sysnum = 0;							/* Subystem prompt number */
	int UD_promptsys = 0;						/* Prompt System number */
	int UD_promptnum = 0;						/* Prompt prompt number */
	int UD_errorsys = 0;							/* Error System number */
	int UD_errornum = 0;							/* Error prompt number */
	int UD_errorcnt = 0;							/* Error counter */
	int UD_dassys = 0;							/* DAS System number */
	int UD_dasnum = 0;							/* DAS prompt number */
	UD_DASSTATE UD_promptstate = {UD_UNISTATE};	/* DAS input state */
	UD_DASSTATE UD_errorstate = {UD_UNISTATE};	/* error state */
#else
	EXT int UD_syssys;							/* Subsystem System number */
	EXT int UD_sysnum;							/* Subystem prompt number */
	EXT int UD_promptsys;						/* Prompt System number */
	EXT int UD_promptnum;						/* Prompt prompt number */
	EXT int UD_errorsys;							/* Error System number */
	EXT int UD_errornum;							/* Error prompt number */
	EXT int UD_errorcnt;							/* Error counter */
	EXT int UD_dassys;							/* DAS System number */
	EXT int UD_dasnum;							/* DAS prompt number */
	EXT UD_DASSTATE UD_promptstate;			/* DAS state */
	EXT UD_DASSTATE UD_errorstate;			/* error state */
#endif

#undef EXT
#define DERRORH

#endif
