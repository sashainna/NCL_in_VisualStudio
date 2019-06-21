/*********************************************************************
**    NAME         :Pvfunc.h
**		functions calls from C++ to C (Fortran) 
** 
**    CONTAINS     : 
**  
**    COPYRIGHT 2011 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			Pvfunc.h , 25.2
**    DATE AND TIME OF LAST  MODIFICATION
**			09/26/17 , 11:53:26
*********************************************************************/
#ifndef PVFUNC_H
#define PVFUNC_H


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
EXT int pwconv();
EXT int pv_saveopt();
EXT int pv_reinit();
//EXT int pv_getoptions(int iopt[11], char copt[7][80]);
//EXT int pv_saveoptions(int iopt[11], char copt[7][80]);
EXT int pv_getoptions(int iopt[20], char copt[20][256]);
EXT int pv_saveoptions(int iopt[20], char copt[20][256]);
EXT int pv_putpfile(char *filename);
EXT int pwconv_ntinit();
EXT int isquiet();

#endif
