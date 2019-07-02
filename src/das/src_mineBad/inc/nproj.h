/*********************************************************************
**    NAME         :  nproj.h
**       CONTAINS: This file contains C structure definitions for then
**                 geometry projection routines.
**    COPYRIGHT 2007 (c) NCCS Inc.
**     MODULE NAME AND RELEASE LEVEL 
**       nproj.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:06:39
*********************************************************************/

#include "mdcoord.h"

typedef struct
{
	UM_coord *inpt;
	UM_vector *tvec;
	UU_REAL *uprm;
	UU_KEY_ID sfkey;
	UU_KEY_ID sakey;
	int npts;
	int wrap;
	int attach;
	int ptype;
	int nrptfl;
	int vectype;
	UU_LOGICAL trimext;
	UU_LOGICAL ssplin;
	UU_LOGICAL onsrf;
	UU_REAL sang;
	UU_REAL eang;
	UU_REAL tol;
	UM_vector pvec;
	UM_coord atpt;
	UM_coord nrpt;
	UU_REAL uv[2];
	UU_REAL uva[2];
} NCL_proj_struc;
