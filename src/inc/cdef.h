
/*********************************************************************
**    NAME         :  cdef.h
**       include file for calculator
**       names of functions in file
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       cdef.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:11
*********************************************************************/

/***   cdef.h - calculator's own declaration          ***/
#ifndef 	UQ_CDEF
#include		"calcom.h"

#define UQ_MAXSTK    600					/* calculator stack size  */
#define UQ_SCI  		0
#define UQ_FLT			1
#define UQ_DEG			0
#define UQ_RAD			1
#define UQ_RADIAN    (UU_REAL) 57.2957795131
#define UQ_TBSIZE   100						/* symbol table size       */
#define UQ_FUZZ		(UU_REAL) 1.0e-10
#define MAXCALCBUF	200

typedef struct  stk				/* structure of stack element   */
	{
	 int	ftype;
	 union
	 { 
	  UU_REAL   tval;
	  UU_REAL   *tptr;
	 }  sval;
	} CSTK;

typedef struct func			/* temporary storage for func name and parameter string  */
	{
	 int	argcount;
	 char	arglist[UQ_ARGNUM][UQ_SYMSIZE];
	} TFUNC;

typedef struct pstk
	{
	 int 	tpindex;
	 int  ttype;
	 UU_REAL  tval[3];
    UQ_func	 *fptr;
	} TPSTK;

#define		UQ_CDEF
#endif
