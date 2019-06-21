/*********************************************************************
**    NAME         :Pmfunc.h
**		functions calls from C++ to C (Fortran) 
** 
**    CONTAINS     : 
**  
**    COPYRIGHT 2001 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			Pmfunc.h , 24.1
**    DATE AND TIME OF LAST  MODIFICATION
**			09/11/13 , 12:58:28
*********************************************************************/
#ifndef PMFUNC_H
#define PMFUNC_H


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
EXT int pmacro();
EXT int pm_saveopt();
EXT int pm_reinit();
EXT int pm_getoptions(int iopt[11], char copt[7][256]);
EXT int pm_saveoptions(int iopt[11], char copt[7][256]);
EXT int pm_putpfile(char *filename);
EXT int pmacro_ntinit();
EXT int isquiet();

#endif
