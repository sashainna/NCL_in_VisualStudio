/*********************************************************************
**
**    NAME         :  dsubcom.h
**
**       CONTAINS:
**       	common for substitute device mechanism for Tek 4115a
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**       dsubcom.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:15
**
*********************************************************************/

#ifndef SUBCOMH

#include "ustdio.h"
#include "dpipe.h"

#ifdef DPGM
#define EXT
#else
#define EXT extern
#endif

/*	-- typedef section -- */

	typedef struct
	{
		int min ;				/* minimum choice number */
		int max ;				/* maximum choice number */
		char **subchar;		/* array of string pointers */
	} UD_SUBTABLE;

/*	-- choice menu jump table -- */

#define UD_CHCTBLEN 14
#ifdef DPGM
	int UD_chctblen = UD_CHCTBLEN;			/* length of choice table */ 
	int (*UD_chctable[UD_CHCTBLEN])() = {
									NULL,				/* main keyboard */
									NULL,				/* function keyboard */
									NULL,				/* puck */
									NULL,				/* tablet */
									NULL,
									NULL,				/* icon menus */
									NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL
										};
	char *UD_chdev3t[3] = {
									"",					/* F1 - requested event trigger */
									"",					/* F2 - cycle choice, pic, loc */
									"\\\\2"				/* F3 - Done */
								 };

	char *UD_cycle2[2]  =  {
									"\\\\A",				/* pick */
									"\\\\B"				/* locator */
								};
	int UD_cycle2len = 2;							/* cycle length */
	int UD_cycle2ptr = 1;							/* cycle pointer */
	char *UD_chccycle = "\\\\E";					/* choice command */
#else
	extern int UD_chctblen;							/* length of choice table */ 
	extern int (*UD_chctable[UD_CHCTBLEN])();
	extern char *UD_chdev3t[3] ;
	extern char *UD_cycle2[2];
	extern int UD_cycle2len;						/* cycle length */
	extern int UD_cycle2ptr;						/* cycle pointer */
	extern char *UD_chccycle;						/* choice command */
	extern int UD_num_safs;							/* number of SAFs */
	extern UD_SAF UD_saf_table[]; 				/* table of SAFs */
#endif

#ifdef DPGM
	UD_SUBTABLE UD_chdev3= {1, 15, UD_chdev3t} ;
#else
	extern UD_SUBTABLE UD_chdev3 ;
#endif

#undef EXT
#define SUBCOMH

#endif
