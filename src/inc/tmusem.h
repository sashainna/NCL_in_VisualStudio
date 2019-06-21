
/*********************************************************************
**    NAME         :  	tmusem.h
**       CONTAINS:
**       names of functions in file
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       tmusem.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:56
*********************************************************************/

#ifndef TMUSEMH

#include "ustdio.h"
#include "tmudef.h"

extern	char	UTI_yytext[];
extern	int	UTI_yyleng, UTI_yylineno;
extern	FILE *UTI_fd, *UTI_fd1, *UTI_fd2, *UTI_fd3, *UTI_fd4, *UTI_fd6;
extern	UTI_CMSTB	 UTI_symtb[];
extern	UTI_MUNAME	UTI_mutb[];
extern	int	UTI_muindex;
extern	char	UTI_gstemp[][80];  /* temporary array to store goto string*/
extern	int	UTI_gscont;				/* goto string count              */
extern	int	UTI_strcont;		 /* total number of strings */
extern	int	UTI_itmmax;		 /* maximum number of items for a menu  */
extern	int   UTI_mugcflag;
extern	int	UTI_muerf;

static	int	pstemp[30];			/* temporary array to store path  */
static	int	pscont=0;				/* number of nodes in the path    */
static	char  itmtext[80];
static	int	itmline;




#define TMUSEMH
#endif
