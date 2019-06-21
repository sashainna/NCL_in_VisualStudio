/*********************************************************************
**    NAME         :  uio.h
**       CONTAINS: variables and defns for logical/physical I/O xlation.
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       uio.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:04
*********************************************************************/
#ifndef UIOH

#include "usysdef.h"
#include "ustdio.h"

#ifdef UPGM
#define EXT
#else
#define EXT extern
#endif

/******  UU_LOGICAL DEVICE TYPES ******/

/* these are used as an index into the logical device table */

#define UD_CI 0	/* Command Input, from the user by default. */
#define UD_CO 1	/* Command Output (echo area for Command Input). */
#define UD_UOSI 2 /* UOS Input (is this just one type of command input)?*/
#define UD_UOSO 3 /* UOS OUTPUT (echo area for UOS commands). */
#define UD_IGO 4	/* Interactive Graphics Output, usually to a screen.*/
#define UD_HGO 5	/* Hardcopy Graphics Output, usually to a plotter. */
#define UD_PO 6	/* Prompt Output (echo for prompt messages). */
#define UD_MO 7 	/* Menu Output (is this one type of prompt output?).*/
#define UD_MI 8 	/* Menu Input (is this a type of command input)? */
#define UD_EO 9	/* Error Output (echo area for error messages). */
#define UD_HO 10	/* Help Output. */
#define UD_LO 11	/* Listing Output. */
#define UD_SO 12	/* Status Output. */
#define UD_RO 13	/* Recording Output (usually
						(a disk file for recording the user's inputs). */

/******* PHYSICAL UNITS ******/

/* I copied these from UNICAD's design book. I'm not sure I
	see the need for a two stage lookup. We could associate a
	physical device directly with a logical unit and not have
	physical units. Here Physical units serve to group together
	one or more logical units and send them to a physical device.
	Do we really need that grouping function? (G.H.) */

#define UD_CONS 0 	/* system console */
#define UD_PLOT 1		/* system plotter */
#define UD_DISK 2		/* disk file */
#define UD_PRIN 3		/* system printer */
#define UD_TAPE 4		/* system tape */
#define UD_WIND 5		/* system window */
#define UD_NETW 6		/* system network unit (not used now) */
#define UD_NUL	 7		/* null device */
/****** I/O TABLE -- two stage translation *******/

#define UD_PUNLEN 14

/****** a PHYSICAL DEVICE *******/

typedef struct 		/*	device assoc with physical unit i. */
{
	int type;			/* 0=fdi or fdo, 1=ws */
	FILE *fdi,*fdo;	/* File pointers for non-graphical units 
								if typ==0 */
	int *ws;				/* A GKS workstation id for
								graphical units if typ!=0 */
}  Dphdev;

EXT int UD_Physunit[UD_PUNLEN];	/* physunit[i]=physical device unit 
													associated with logical device i */
EXT Dphdev UD_Physdev[8];			/* physdev[i]=the physical device
													assoc with physical unit i */

#ifdef UPGM
UU_LOGICAL UD_first = UU_TRUE;		/* first time flag */
#else
EXT UU_LOGICAL UD_first;
#endif
#define UIOH
#undef EXT
#endif
