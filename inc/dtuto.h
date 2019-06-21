/*********************************************************************
**
**    NAME         :  dtuto.h
**
**       CONTAINS:
**       names of functions in file
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**       dtuto.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:15
**
*********************************************************************/

#ifndef DTUTO

typedef struct
{
	char	filnm[82];				/* file name containing this tutorial */
	int	pages;				/* number of 24 line pages in this file */
}	UD_TOPICS;

typedef	struct
{
	int	row;					/* number of rows in one page */
	int	col;					/* column numbers */
	int	itemnu;				/* number of items in this menu */
	char	topfilnm[82];			/* file name of top level tutorial */
	int	pagenu;				/* number of 24 line pages in top level */
	UD_TOPICS	*items;		/* descriptors of sub-tutorials */
}	UD_TUTO;


#define DTUTO
#endif
