/********************************************************************
**    NAME         :  fmill.h
** CONTAINS: FMILL definitions
**    COPYRIGHT 1997 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       fmill.h , 25.1
**     DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:06:15
*********************************************************************/

#ifndef FMILH
#define FMILH

#define TL_TO 1
#define TL_ON 2
#define TL_PAST 3

#define RETRCT_DIS 1
#define RETRCT_PLN 2
#define RETRCT_SRF 3
#define THRU 4
#define DOWN 5

#define DIRSAME 1
#define DIRCCW 2
#define DIRCW 3

typedef struct
{
	int cvtyp;
	int iup;
	int flowdir;
	int stepdir;
	int nppp;
	int npas;
	UU_REAL wst;
	UU_REAL wnd;
	UU_REAL wfrs;
	UU_REAL wlst;
	UU_REAL delw;
} NCL_fmlparam;

struct NCL_isect
{
	UM_2Dcoord pc;
	int ib;
	int jst;
	int jnd;
};

typedef struct
{
	UU_KEY_ID key;
	UU_LIST keylst;
	UU_LIST ptlist;
	UU_REAL w;
	UM_2Dcoord uvend; 
	UM_2Dcoord uvlim; 
} NCL_wline;

#endif
