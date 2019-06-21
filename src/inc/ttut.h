
/*********************************************************************
**    NAME         :  ttut.h
**       CONTAINS:
**       names of functions in file
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       ttut.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:58
*********************************************************************/

#ifndef	UT_TTUT

#define	TITLE		0
#define	PAGE		1
#define	SPACE		2
#define	DONE		3
#define	RETURN	4

typedef	struct
	{
	 int	row, col;			/* # of rows and # of cols for display */
	 int	itemnum;				/* # of item files 	*/
	 struct iteminfo
		{
		 char itemnm[80];		/* each item's file name */
		 int	pagenum;			/* # of pages of each item file */
		}	items[12];
	}	TUT_info;




#define	UT_TTUT
#endif
