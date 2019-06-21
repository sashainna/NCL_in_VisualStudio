
/*********************************************************************
**    NAME         :  tmugm.h
**       CONTAINS:
**       names of functions in file
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       tmugm.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:55
*********************************************************************/

#ifndef TMUGMH

#include "usysdef.h"

char	UTI_gstemp[10][80];
int	UTI_gscont = 0;
static 	int	row = -1;
static	int	col;
static	int	tbindex;
static	int	leaflag;		/* flag for leaf node */
static	int	ttlflag;		/* flag for menu title */
static	UU_LOGICAL 	actnflag=UU_FALSE;	/* flag for the existence 
											of actions          */
static	UU_LOGICAL	istuto = UU_FALSE;	/* is there a tutorial?  */
static	UU_LOGICAL	isicon = UU_FALSE;	/* is there a icon file? */
static	UU_LOGICAL	isarea = UU_FALSE;		/* is there a menu area? */
static	int	locind = 0;		/* index to the menu area array */
static	char	tutofil[80];			/* tutorial file name for leaves */
#if UU_COMP==UU_IRIS && UU_DOUBLE
static	long float	recpt[4];			/* the ll and ur corner */
#else
static	float	recpt[4];			/* the ll and ur corner */
#endif
			/* recpt is declared as float on purpose, since double or single
				precision doesn't make difference here but can cause problem to
				convert a text into a real number to be stored in recpt */
			/* a special case is necessary to do true double precision on Iris */
static   int	recind = 0;				/* index to the recpt */
UU_LOGICAL	UTI_locf = UU_FALSE;		/* Is a location going to be specified? */
extern	int	UTI_mugcflag;
extern	char	UTI_yytext[];
extern	int	UTI_yyleng, UTI_yylineno;




#define TMUGMH
#endif
