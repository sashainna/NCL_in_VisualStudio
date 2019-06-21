/*********************************************************************
**    NAME         :PWfunc.h
**		functions calls from C++ to C (Fortran) 
** 
**    CONTAINS     : 
**  
**    COPYRIGHT 2001 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			Pwfunc.h , 25.2
**    DATE AND TIME OF LAST  MODIFICATION
**			10/06/15 , 08:49:41
*********************************************************************/
#ifndef PWFUNC_H
#define PWFUNC_H


#ifdef __cplusplus 
	#define EXT extern "C"
#else
	#define EXT extern
#endif

EXT int init();
EXT int cloddat (int *kfl, char *msg, int *ierr);
EXT int cerrkil (char *msg, int *ierr);
EXT int getopcc (char *msg, int *ierr);
EXT int prsopt (char *comstr, int *num,int *ifil,char *msg, int *ierr);
EXT int pworks();
EXT int pw_saveopt();
EXT int pw_reinit();
EXT int pw_getoptions(int iopt[], double rusr[], char copt[11][256]);
EXT int pw_saveoptions(int iopt[], double rusr[], char copt[11][256]);
EXT int pw_putpfile(char *filename);
EXT int pworks_ntinit();
EXT int isquiet();

#endif
