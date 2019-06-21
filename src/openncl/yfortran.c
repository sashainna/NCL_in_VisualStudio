/*********************************************************************
**    NAME         :  nclx.h
**       CONTAINS:
**			sysvfy(name,ierr)
**			ylanch(name,nfarg,args,ierr)
**    COPYRIGHT 1997 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       yfortran.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:10:59
*********************************************************************/
#include "usysdef.h"
#include "mfort.h"

#if (UU_COMP == UU_SUN) || (UU_COMP== UU_IRIS4D)
#ifndef UU_RS6000
#define sysvfy sysvfy_
#define ylanch ylanch_
#endif
#endif

/*********************************************************************
**		E_FUNCTION     : sysvfy(name,ierr)
**			Verify that a user defined routine exists.
**		PARAMETERS
**		INPUT  :
**			name                 Name of routine.
**		OUTPUT :
**			ierr                 Returns non-zero if routine does not
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
sysvfy(name,ierr)
UM_int2 *ierr;
UM_f77_str_ptr name;
{
	int nc;
	char *p,routin[65];
/*
.....Initialize routine
*/
	*ierr = 0;
/*
.....Convert Fortran Character string
.....to C string
*/
	p = UM_cstr_of_f77_str(name);
	strncpy(routin,p,64);
/*   IJD - strlen returns > 64 if data following routin is not NULL */
/* 	nc = strlen(routin); */
	nc = 62;
	ul_strip_blanks(routin,&nc);
/*
.....Verify this routine exists
*/
	if (NclxGetRoutine(routin) == -1) *ierr = 8;
}

/*********************************************************************
**		E_FUNCTION     : ylanch(name,nfarg,args,ierr)
**			Calls the specified user routine.
**		PARAMETERS
**		INPUT  :
**			name                 Name of routine to call.
**			nfarg                Number of arguments passed.
**			args                 Arguments to passed to called routine.
**		OUTPUT :
**			ierr                 Returns non-zero on error.
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
ylanch(name,nfarg,args,ierr)
UM_int2 *nfarg,*ierr;
UM_f77_str_ptr name;
char args[35][64];
{
	int narg,i,nc,irtn;
	char routin[64],*p;
/*
.....Initialize routine
*/
	*ierr = 0;
	narg = *nfarg;
/*
.....Convert Fortran Character string
.....to C string
*/
	p = UM_cstr_of_f77_str(name);
	nc = 62;
	strncpy(routin,p,nc);
	ul_strip_blanks(routin,&nc);
/*
.....Convert arguments
*/
	for (i=0;i<narg;i++)
	{
		nc = 62;
		ul_strip_blanks(args[i],&nc);
	}
/*
.....Call System Routine
*/
	irtn = NclxLaunchRoutine(routin,narg,args);
   if (irtn != UU_SUCCESS) *ierr = 464;
}
