/**************************************************
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       xfsys0.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:23
***************************************************/
#ifndef XFSYS0H
#include "xenv1.h"

#define UX_FAILURE -1
#ifndef UX_EOF
#define UX_EOF -23					/* originally in xfsys1.h */
#endif
#define UX_IF_FAILURE_PRINT_IT							  \
/*{  if (status != UU_SUCCESS)								 \
	{																 \
		char us[120];											 \
		uu_denter2(UU_XTRC,(us, "returned FAILURE"));  \
		uu_dexit;												  \
	} }*/																 

#define UX_GETC0(stream,cin) \
	(((cin=getc(stream))==EOF)&&(feof(stream)==0))?UX_FAILURE:UU_SUCCESS
#define UX_PUTC0(cout,stream) ((putc(cout,stream))==EOF)?UX_FAILURE:UU_SUCCESSS
#define UX_FPRINTF0(str,numout) \
	((numout = fprintf str)==EOF)?UX_FAILURE:UU_SUCCESS
#define UX_SPRINTF0(str) sprintf str
#define UX_SSCANF0(str,nfound) (nfound =sscanf str)
#define UX_FSCANF0(str,nfound) (nfound =fscanf str)

#define MAXT 150
typedef struct 
	{
	char fname[UX_MAX_PATH_LEN];
	char rname[40];					/* name of opening calling routine */
	int fdes;
	FILE *fileptr;
	char openfor[3]; /* one of the following: r, w, a, r+, w+, a+ */	
	char format[12]; /* currently either BLOCK or STREAM */
	char interp[12]; /* currently either ASCII, TEXT, or BINARY */
	} UX_OPENF;

#ifdef UX_F0PGM
#define EXT
#else
#define EXT extern
#endif

EXT UX_OPENF otable[MAXT];
EXT int otind;	/* positon into record of greatest length used in table */
EXT int otnext; /* position into record for next available entry */

#undef EXT
#define XFSYS0H
#endif
