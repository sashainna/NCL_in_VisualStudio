/*********************************************************************
**    NAME         :  mdattr.h
**       CONTAINS: attribute definitions
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       mdattr.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:06:28
*********************************************************************/

#ifndef UM_MDATTR

/*********************************************************************
*
*							Define Colors
*
*********************************************************************/
#define  UM_BACKGROUND	0
#define  UM_BLACK 		0
#define  UM_WHITE 		1
#define  UM_DARKBLUE 	2
#define  UM_DARKRED 	3
#define  UM_DARKGREEN 	4
#define  UM_MAGENTA		5
#define  UM_YELLOW		6
#define  UM_CYAN		7
#define  UM_RED 		3 
#define  UM_GREEN 		4
#define  UM_BLUE		2
#define  UM_ORANGE		12
#define  UM_PINK		13
#define  UM_LIGHTGREEN	11
#define  UM_LIGHTBLUE	10
/*********************************************************************
*
*							Define Linestyles
*
*********************************************************************/

#define  UM_SOLID_LINE		1
#define  UM_SMLDASH_LINE		2
#define  UM_DOT_LINE		3
#define  UM_CENTER_LINE		4
#define  UM_PHANTOM_LINE  	5
#define  UM_DASHED_LINE		6
#define  UM_DASHDOT_LINE		7
#define  UM_DASHSPC_LINE		8


/*********************************************************************
*
*							Define Display Attributes
*
*********************************************************************/

#define UM_DISPLAYABLE		0
#define UM_UNDISPLAYABLE	1
#define UM_NEVERDISPLAYABLE	2


/*********************************************************************
*
*							Set the maximum number of
*							pens, layers, colors, linestyles.
*
*********************************************************************/

#define UM_MAX_PENS			256
#define UM_MAX_LAYERS		10000
#define UM_MAX_COLORS		256
#define UM_MAX_LINESTYLES	16


/*********************************************************************
*
*							Declare the base layer
*
*********************************************************************/

#ifdef UM_MPGM
int	UM_BASE_LAYER;
#else
extern int	UM_BASE_LAYER;
#endif

#define UM_MDATTR
#endif
