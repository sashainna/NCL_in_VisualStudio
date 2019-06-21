/*********************************************************************
**    NAME         :PCfunc.h
**				functions calls from C++ to C (Fortran) 
** 
**    CONTAINS     : 
**  
**    COPYRIGHT 2001 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			Pcfunc.h , 24.1
**    DATE AND TIME OF LAST  MODIFICATION
**			09/11/13 , 12:58:27
*********************************************************************/
#ifndef PCFUNC_H
#define PCFUNC_H


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
EXT int postcomp();
EXT int pc_saveopt();
EXT int pc_reinit();
EXT int pc_getoptions(int iopt[11], char copt[7][256]);
EXT int pc_saveoptions(int iopt[11], char copt[7][256]);
EXT int pc_putpfile(char *filename);
EXT int pcomp_ntinit();
EXT int isquiet();

#endif
