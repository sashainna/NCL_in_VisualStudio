/*********************************************************************
**    NAME         :  layout.h -- include file for screen layout stuff.
**       CONTAINS:
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       layout.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:27
*********************************************************************/

#ifndef LAYOUTH
/* define names for the 10 types of areas */
#define UD_GRAF 0
#define UD_SPRMT 1
#define UD_LPRMT 2
#define UD_ERR 3
#define UD_HLP 4
#define UD_CHC 5
#define UD_ICON 6
#define UD_SCROLL 7
#define UD_FORMS 8
#define UD_MENU 9
#define UD_WSWIND 10
#define UD_NTYPES 11

#include "go.h"
typedef struct {			/* specifies contents of a choice, menu, icon, 
									or forms area */
	char fname[60];		/* filename of the menu, icon, or forms file */
} UD_contents;

typedef struct {
	int noareas;			/* no. areas in this list */
	int color[10];			/* array of backbround color indices */
	Gdrect areas[10];		/* array of areas */
	UD_contents cont[10];	/* array of contents of the areas */
} UD_arealist;

typedef struct{
	UD_arealist	alist[UD_NTYPES];	/* array of UD_NTYPES area lists */
} UD_scrnlayout;
/* a layout file consists of a UD_scrnlayout structure */
#endif
#define LAYOUTH
