/*********************************************************************
**    NAME         :  nclpockofs.h
**       CONTAINS: trimmed surface boundary offset structures
**
**    COPYRIGHT 2013 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       nclpockofs.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:36
*********************************************************************/

#ifndef NCLPOCKOFS

typedef struct
{
	UM_coord pt;
	UM_vector fvec;
	UM_vector nvec;
	UU_REAL cvu;
	UU_REAL sfu;
	UU_REAL sfv;
	UU_LOGICAL remove;
} ncl_bndpt_struc;

typedef struct
{
	UU_KEY_ID key;
	int no_bpts;
	UU_LIST *bpts;
	UU_REAL cvu0;
	UU_REAL cvu1;
	UU_REAL rad;
	int orient;
	UU_LOGICAL check;
	UU_LOGICAL open;
} ncl_nbr_struc;

typedef struct
{
	int start;
	int end;
	UU_LOGICAL open;
} ncl_ind_struc;

#define NCLPOCKOFS

#endif
