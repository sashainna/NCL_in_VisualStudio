/*********************************************************************
**    NAME         :  ridctdef.h
**       CONTAINS:
**       structure data dictionaries
**    COPYRIGHT 1987 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       ridctdef.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:42
*********************************************************************/

#ifndef RIDCTDEFH

struct UR_dct        				/* data dictionary structure */
{
	char  dctnm[9];   				/* dictionary name */
	int   dcttyp;     				/* dictionary type */
	struct relblk	*dctrels;		/* dictionary relations */
	int	dctnumrels;
	struct attr_def	*dctattrs;	/* dictionary attributes */
	int	dctnumattrs;
};

#define RIDCTDEFH
#endif

