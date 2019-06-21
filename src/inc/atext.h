
/*********************************************************************
**    NAME         :  atext.h
**       CONTAINS:
**       names of functions in file
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       atext.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:11
*********************************************************************/

#ifndef	UA_TEXT

#define  UA_TXT_TWOPI  (UU_REAL) 6.28318530718
#define  UA_TXT_BUFSZ    8
#define  UA_TEXT_BUFSZ   6000
#define  UA_MAX_TEXT_LEN 100
#include		"atxtddl.h"

#ifdef	ATEXT
#define	EXT	
#else
#define	EXT	extern
#endif

typedef	struct	formattr
{
	int	color;
	char	fontname[32];
	int	prec;
	UU_REAL	expn;
	UU_REAL	spacing;
	UU_REAL	height;
	UU_REAL	tangle;
	int	path;
	int	align_hor;
	int	align_ver;
	int	txt_dens;
	UU_REAL	slant;
	UU_REAL	sub_sup;
	UU_REAL	line_spacing;
	int	entity_site;
}	ATXT_FRM;

typedef	struct txtind
{
 int	*prev, *next;
 char	str[257];
}	TXTLIST;

EXT	ATXT_FRM		UA_txtattr, UB_txtattr; /* global attribute bundle for 
															Drafting and Symbols          */
EXT	UU_LOGICAL	UA_TXT_ASSO;				/* the associativity action flag */

#undef   EXT
#define	UA_TEXT
#endif
