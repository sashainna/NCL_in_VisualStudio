/*********************************************************************
**    NAME         :  rireldef.h
**       CONTAINS:
**       structure for relation definitions in the data dictionary
**    COPYRIGHT 1987 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       rireldef.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:47
*********************************************************************/

#ifndef RIRELDEFH

struct relblk        /* relation data block */
{
	char  relnm[9];   /* relation name */
	int   reltyp;     /* relation type */
	int   relindx;    /* index to attributes */
	int   rellen;     /* number of attributes */
};

#define RIRELDEFH
#endif

