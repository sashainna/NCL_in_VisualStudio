/***********************************
c     Filename ncqcom.h
c     COPYRIGHT 2003 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c     MODULE NAME AND RELEASE LEVEL
c           ncqcom.h , 25.1
c     DATE AND TIME OF LAST  MODIFICATION
c           04/29/15 , 15:06:38
c        
********************************************/
#ifndef NCQCOM_H
#define NCQCOM_H

#include "usysdef.h"

#define MAX_PATH 1024
#define MAXOPT 20

#ifdef EXT
#undef EXT
#endif

#ifdef NCQ_MAIN
#define EXT
#else
#ifdef __cplusplus 
#define EXT extern "C"
#else
#define EXT extern
#endif
#endif
/*
.....structure for NCL information, it should be same as NCL defined
*/
typedef struct
{
	int flag;
	char ppfile[256];
	int current, highest, lines;
	char macro[64];
	int warn, error;
} NCLInfo;

/*
.....structure for version96 and earlier
*/
typedef struct
{
	int flag;
	char ppfile[256];
	int current, highest, lines;
	char macro[8];
	int warn, error;
} NCLInfo96;


EXT int UU_BATCH,ncq_idel, ncq_flen, ncq_open, ncq_ipglen;
EXT char ncq_lpri;
EXT char ncq_linbuf[20], ncq_linsav[20];

#endif
