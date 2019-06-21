
/*********************************************************************
**    NAME         :  qsymtb.h
**       CONTAINS:
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       qsymtb.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:41
*********************************************************************/

#ifndef	QSYMTBH

#include 	"calcom.h"
#include		"cdef.h"

#define		CFUNCNAME		1
#define		CFUNCARG			2

/* extern  int UT_ictodc[];	/* langpak internal code to ascii code  */
/* extern  int UT_dctoic[];	/* ascii code to langpak internal code  */
extern  char UQ_dpt[];		/* number print out format              */
extern  int UQI_sciflag;		/* flag for scientific print out        */
extern  int UQI_angflag;    /* flag for angle representation        */
extern  int  UQI_sdebug;		/* debug flag for symtb.c file          */
extern  int  UQI_cer2;		/* flag for common error 					 */
extern  int  UQI_cinptr;    /* pointer to the current symbol in input */
extern  int  uq_calc2flag;

static  TFUNC   cfunc;
static  int     cfstop;		/* flag to stop adding function name */
static  int     findex[20];
extern  int     UQI_cfpar;

static char *keywd[] =  		/* key word table */
  {     
	"quit",
   "q",
   "reset",
   "sci",
   "flt",
   "degree",
   "radian",
   "list",
   "help",
   "dot",
   "cross",
   "dist",
   "unitvc",
   "mag",
   "deg",
   "rad",
   "comp",
   "sump",
  };


#define	QSYMTBH
#endif

