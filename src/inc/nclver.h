/*********************************************************************
**
**    NAME         :  nclver.h
**    COPYRIGHT 1987 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       nclver.h , 26.5
**    DATE AND TIME OF LAST MODIFICATION
**       07/23/18 , 14:28:33
**
** NOTE: the module nclc/nauth.c must be recompiled after this
**		include files has changed.  This module delcares the version
**		number used by NCL502 and all tools as well as containing
**		the authorization routines.
**
*********************************************************************/
#ifndef UM_MFORT
#include "mfort.h"
#endif

#ifdef NCLVERSION

UM_real8 NCL_version = 2019.00;
UM_real8 NCL_unibase_version = 8.201;	/* VERSION OF NCL502 WHERE UNIBASE CHANGED */
UM_real8 NCL_infile_version = 0.0;
char NCL_machine[20];		/* MACHINE TYPE CURRENTLY RUNNING ON */
char NCL_machine_type[20];  /* MACHINE UNIBASE WAS BUILT ON */
char NCL_infile_date[20];  /* MACHINE UNIBASE WAS BUILT ON */

char last_mod[] = "@(#) nclver.h 24.14 05/12/14 08:19:27";
char last_release[] = "05/12/14 08:19:27";

#else

extern UM_real8 NCL_version;
extern UM_real8 NCL_unibase_version;
extern UM_real8 NCL_infile_version;
extern char NCL_machine_type[20];
extern char NCL_machine[20];		
extern char NCL_infile_date[20];		
extern char last_mod[];
extern char last_release[];

#endif /* NCLVERSION */

