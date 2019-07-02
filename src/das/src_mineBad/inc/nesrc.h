
/*********************************************************************
**    NAME         :  nesrc.h
**       CONTAINS: This file contains C structure definitions for
**						storing the part program source file.
**    COPYRIGHT 2009 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       nesrc.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:06:39
*********************************************************************/
#ifndef NCLSRC_H
#define NCLSRC_H
typedef struct
{
	int nc;
	int size;
	int type;
	int lineno;
	int prev;
	int next;
	char *buf;
} NCL_source_struc;

typedef struct
{
	int first;
	int current;
	int last;
} NCL_srcptr_struc;
#endif
