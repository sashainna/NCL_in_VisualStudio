/*********************************************************************
**    NAME         :  mdpick
**       CONTAINS: definitions for picked entities
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       mdpick.h , 25.2
**     DATE AND TIME OF LAST  MODIFICATION
**       08/01/17 , 13:34:58
*********************************************************************/
#ifndef   UM_MDPICK

#include "usysdef.h"
#include "dasnog.h"
#include "nconst.h"

typedef struct				/* picked entity */
	{
	int num;					/* number of entities in pick path */
	UU_KEY_ID key[5];		/* UNIBASE keys of entities in pick path */
	} UM_PICKENT;

typedef struct
{
	UU_KEY_ID key;
	int relnum;
	char label[NCL_MAX_LABEL_AND_SUBSCRIPT];
	int color;
	int inout;
	UU_REAL thick;
	int	label_on;
} UM_sgeo;

typedef struct				/* pick location record */
	{
	UM_PICKENT pent;		/* pick path of picked entity */
	UD_NDCLOCREC ploc;		/* pick location of picked entity */
	}  UM_PLOCREC;

extern UU_KEY_ID um_get_pickkey();

#define   UM_MDPICK
#endif
