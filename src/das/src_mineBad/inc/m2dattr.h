/*********************************************************************
**    NAME         :  m2dattr.h
**       CONTAINS: 2D Analysis Modal Attribute Definitions
**    COPYRIGHT 1989 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m2dattr.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:06:28
*********************************************************************/

#ifndef UM_M2DATTR

struct UM_2dattr_def	{
	int	pts_per_span;		/* number of points generated along curve */
	int	width;				/* total field witdth							*/
	int	prec;					/* decimal precision								*/
	int	type;					/* representation type for values			*/
	int	disp_flag;			/* flag for display of intermediate pts	*/
	char  format[12];			/* format for displaying results				*/
	};

/*************************************************************************
*
*	define external variables
*
*************************************************************************/

#ifdef  UM_MPGM
#define EXT
	UU_LOGICAL UM_initialize_2da_attr = UU_TRUE;
#else
#define EXT extern
   extern UU_LOGICAL UM_initialize_2da_attr;
#endif

EXT struct  UM_2dattr_def	UM_2dattr;

#undef EXT

#define UM_M2DATTR
#endif
