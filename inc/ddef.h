/*********************************************************************
**
**    NAME         :  ddef.h
**
**       CONTAINS:
**       	DAS input common
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**       ddef.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:13
**
*********************************************************************/

#ifndef DDEFH

#include "usysdef.h"


/***   ddef.h - dasinput's own declaration          ***/

#define UD_dmaxstk   300					/* das stack size  */
#define UD_WOUNIT		0	
#define UD_WUNIT		1
#define UD_SYMSIZE	9

								/* das input data type			 */
#define UD_DSCALAR   	  1
#define UD_DCOORDINATE    2
#define UD_DREFERENCE     3
#define UD_DNINCREMENT    4
#define UD_DQINCREMENT    5
#define UD_DELTAPOINT	  6

#define UD_PREINCREMENT     0
#define UD_POSTINCREMENT    1

#define UD_DISTANCE		0
#define UD_DANGLE			1
#define UD_UNITLESS		2

typedef struct  dstk				/* structure of stack element   */
	{
	 int	  dtype;
	 UU_REAL   dval;
	} UD_DSTK;

typedef struct	dasdata			/* structure for the return data  */
	{
	 int	dtype;			/* das input data type            */
	 UU_LOGICAL	 funcflag;	/* is there a function name?		 */
	 char	  funcname[UD_SYMSIZE];		/* function name   */
	 union
		{
		 UU_REAL dval;
		 struct	dcord
			{
			 int	cordtype;	/* coordinate type              */
			 UU_REAL	coord[3];	/* coordinate value					*/
			 int 	incnum[3];	/* incremental number				*/
			 int	prepostflag[3];
			} stcord;
		} stval;
	}UD_DASDATA;

#define DDEFH
#endif
