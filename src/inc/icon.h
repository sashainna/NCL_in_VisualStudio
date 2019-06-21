/*********************************************************************
**    NAME         :  icon.h
**    CONTAINS:	data type definitions for icon editor.
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       icon.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:25
*********************************************************************/

#ifndef ICONH

typedef struct {				/* specifies a segment in an archive file*/
	char fname[50];			/* name of archive file (Null means no seg) */
	int segno;					/* segment number in archive file */
} UT_archseg;

typedef struct {				/* definition of one icon in icon file */
	int xsiz,ysiz;				/* size of array that follows */
	int *pixarray;				/* pointer to 2D array of pixels */
	UT_archseg archseg;		/* archive filename and segment no. */
} UT_icon;
/* an icon file consists of an integer N, followed by an array of
	UT_icon structures, with the pixarray pointer replaced by the actual
	data. */

#define ICONH
#endif
