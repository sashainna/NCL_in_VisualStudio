
/*********************************************************************
**    NAME         :  tstep.h
**       CONTAINS:
**    COPYRIGHT 2013 (c) NCCS.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       tstep.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:06:58
*********************************************************************/

#ifndef TSTEPH
#define TSTEPH

#include "tstepcmd.h"
#include "ulist.h"

#ifdef MAINIGES
#define EXT 
#else
#ifdef __cplusplus
#define EXT extern "C"
#else
#define EXT extern
#endif
#endif
 
typedef union
{
	int recno;
	UTP_command_type cmd;
	UU_REAL value;
	char *str;
} UTPs_step_token;

typedef struct
{
	UTP_token_type type;
	UTPs_step_token ptype;
} UTPs_step_parm;

typedef struct
{
	int recno;
	UTP_command_type command;
	int nparm;
	int used;
	UTPs_step_parm *parm;
} UTPs_step_record;

EXT UU_REAL UTP_units_cnv;
EXT int UTP_step_numrec;
EXT UU_LIST UTP_step_record;
EXT int UTP_step_214;

UU_REAL utp_convert();
UTPs_step_record *utp_get_record();

#define STEP_ACY 12

#endif
