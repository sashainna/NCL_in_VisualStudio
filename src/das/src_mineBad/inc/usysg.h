/*********************************************************************
**
**    NAME         :  usysg.h
**
**       CONTAINS:
**       	GKS workstation definitions for the DAS
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**       usysg.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:06
**
*********************************************************************/

#ifndef USYSGKSH

#ifdef DPGM
#define EXT    
#else
#define EXT extern
#endif


/*
				Common information for GKS routines
*/

	EXT int *UD_ksws;				/* pointer to read / write workstation */
	EXT int *UD_kswsadr;			/* address of workstation entry */
	EXT int UD_wsptr;				/* next free workstation number */
	EXT int *UD_gksws[10];		/* list of active workstations */
/*
.....The following was added to save
.....the command line arguments
.....Yurong
*/
#define COMMAX 10
	EXT char *UU_comargv[COMMAX];
	EXT int UU_comargc;

#undef EXT

#define USYSGKSH
#endif
