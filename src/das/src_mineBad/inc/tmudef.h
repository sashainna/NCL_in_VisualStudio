
/*********************************************************************
**    NAME         :  tmudef.h
**       CONTAINS:
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       tmudef.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:55
*********************************************************************/

#ifndef TMUDEFH


#define	NMMENU		0		/* a normal menu file */
#define	POPMENU		1		/* a popup menu file  */

typedef 	struct	
	{
	 char		sym[80];
	 int		menuid,			/* menu's corresponding number	*/
				child,			/* the first child's index			*/
				childno,			/* how many children					*/
				parent;			/* parent's index						*/
	}	UTI_CMSTB;

typedef	struct	kptnode
	{
	 int	index, lineno;
	 struct  kptnode	*next;
	}	UTI_KPTNODE;

typedef	struct
	{
	 char	sym[80];
	 int	index;
	}	UTI_MUNAME;



#define TMUDEFH
#endif
