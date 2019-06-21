
/*********************************************************************
**    NAME         :  	files.h
**		CONTAINS:
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       files.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:15
*********************************************************************/
#ifndef FILESH

	/* this file has the location of the parser, and the size of the progam desired */
	/* It may also contain definitions to override various defaults: for example,
	/* WORD32 tells yacc that there are at least 32 bits per int */
	/* on some systems, notably IBM, the names for the output files and tempfiles must
	/* also be changed  */

#ifndef WORD32
#define WORD32
#endif
	/* location of the parser text file */
	/* CHANGED from "/usr/lib/yaccpar" 10/3/85 PRT */
# define PARSER "tyaccpar"

	/* basic size of the Yacc implementation */
# define HUGE

#define FILESH
#endif
