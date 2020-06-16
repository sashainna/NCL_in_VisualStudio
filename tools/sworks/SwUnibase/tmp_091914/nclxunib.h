/*********************************************************************
**    NAME         :  nclxunib.h
**       CONTAINS:
**       names of functions in file
**    COPYRIGHT 1997 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       nclxunib.h , 24.1
**    DATE AND TIME OF LAST  MODIFICATION
**       09/11/13 , 13:01:07
*********************************************************************/

#include "xenv1.h"
#ifndef NCLXUNIBASE

/*
typedef enum
{
	NCLX_BASE,
	NCLX_FACE
} NCLX_mdl_trim_type;
*/

typedef struct
{
	char dir[UX_MAX_PATH_LEN];
	char unibase[UX_MAX_PATH_LEN];
	int label_opts;
	int import_curves;
	int shade_surfs;
	int import_sketch;
	int sflab_type;
	char sflab[64];
	int cvlab_type;
	char cvlab[64];
	int ptlab_type;
	char ptlab[64];
	int lnlab_type;
	char lnlab[64];
	int cilab_type;
	char cilab[64];
	int kvlab_type;
	char kvlab[64];
	char unimatch[UX_MAX_PATH_LEN];
	int exact_match;
	double toler;
	int layer[6];
	int color[6];
	char mxname[128];
	int cvopt;
} NCLX_sw_options;

typedef struct
{
	int type;
	char prefix[64];
	int sub;
	char label[96];
} NCLX_label_struc;

#define NCLXUNIBASE
#endif
