/*********************************************************************
**    NAME         :  dasg.h
**       CONTAINS:
**       	DAS global that has gks constructs
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**       dasg.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:12
*********************************************************************/

#ifndef DASGKSH

#include "usysdef.h"
#include "ustdio.h"
#include "gistroke.h"
#include "gloc3.h"
#include "dasnog.h"

#ifdef DPGM
#define EXT    
#else
#define EXT extern
#endif

/*	-- Macro Section -- */

/* -- Convert a Gloc3 form wcs to ndc and back -- */

#define UD_GLOC3_W2NDC(to, from) { \
	(*to).transform = (*from).transform; \
	gsnormtran((*to).transform); \
	gwndc3(&(*to).position.x, &(*to).position.y, &(*to).position.z, \
		(*from).position.x, (*from).position.y, (*from).position.z); }

#define UD_GLOC3_NDC2W(to, from) { \
	(*to).transform = (*from).transform; \
	gsnormtran((*to).transform); \
	gndcw3(&(*to).position.x, &(*to).position.y, &(*to).position.z, \
		(*from).position.x, (*from).position.y, (*from).position.z); }

/*	-- Typedef Section -- */

/*	-- Pop up menu control block -- */

	typedef struct
	{
		int numchoice;						/* number of choices */
		char **text;						/* pointer to array of text strings */
		char iconfile[16];				/* icon file name */
		Gdpoint menusz;					/* size of icon array or text box */
		int devnum;							/* choice device number */
		int popet;							/* P.E.T. of this menu */
		int perm_flag;						/* Take down during pick/loc */
	} UD_POPUPREC;

/*	-- a window state save record -- */

	typedef struct
	{
		int state;						/* window state */
		Glntype oldlntype;			/* save linetype here */
		Gcolor oldlinecolr;			/* save linecolor here */
		int pet;							/* save string pet */
		int dev;							/* save string device */
	} UD_WINDOW_STATE_REC;

/* --  DAS data --  */

#ifdef DPGM
	Gdrect UD_locwin;				/* window max, min for locator */
	Gdrect UD_chcwin;				/* window max, min for choice */
	Gdrect UD_valwin;				/* window max, min for valuator */
	Gdrect UD_pickwin;			/* window max, min for pick */
	Gdrect UD_strwin;				/* window max, min for string */
	Gdrect UD_stkwin;				/* window max, min for stroke */
	Gdrect UD_menuwin;			/* window max, min for menu */
	Gloc3 UD_wxhair = {0, .5, .5, .5};		/* WCS initial crosshair location */
	Gloc3 UD_nxhair = {0, .5, .5, .5};		/* NDC initial crosshair location */
	Gdrect UD_winext = {.2,.2, .8,.7}; /* window max, min for string windows */
	Gwpoint3 UD_errpos = {0.01, .01, 0.};
	Gwpoint3 UD_pmtpos = {0.01, .0557, 0.};
	Gstroke UD_stkrec =				/* initial stroke */ 
	{
		1,						/* transform */
		1,						/* number of points */
		NULL					/* points */
	};

	Gstrokerec UD_stkinit = 			/* stroke data record */
	{
		300,0,					/* buffer size, edit posn */
		.01,.01,					/* interval */
		.1,						/* time */
		UG_CURRENT,				/* acf, aspect ctrl flag */
		UG_INDIVIDUAL,UG_INDIVIDUAL, UG_INDIVIDUAL,
		UG_INDIVIDUAL,UG_INDIVIDUAL,UG_INDIVIDUAL,
		1,							/* line index */
		1,0,0.,{0},1.,1,		/* Glnbundl: type, width, color */
		1,							/* marker index */
		1,1.,1,					/* Gmkbundl: type, size, color */
		NULL,						/* function to call on each stroke pt  */
		NULL						/* prompt */
	};
	UD_WINDOW_STATE_REC UD_winstate[UD_MAX_NUM_WIN];	/* window state stack */
	int UD_winstate_ptr = -1;									/* window stack pointer */

#else
	EXT Gdrect UD_locwin;			/* window max, min for locator */
	EXT Gdrect UD_chcwin;			/* window max, min for choice */
	EXT Gdrect UD_valwin;			/* window max, min for valuator */
	EXT Gdrect UD_pickwin;			/* window max, min for pick */
	EXT Gdrect UD_strwin;			/* window max, min for string */
	EXT Gdrect UD_stkwin;			/* window max, min for stroke */
	EXT Gdrect UD_menuwin;			/* window max, min for menu */
	EXT Gloc3 UD_wxhair;				/* WCS initial crosshair location */
	EXT Gloc3 UD_nxhair;				/* NDC initial crosshair location */
	EXT Gdrect UD_winext;			/* window max, min for string windows */
	EXT Gwpoint3 UD_errpos;			/* error message position */
	EXT Gwpoint3 UD_pmtpos;			/* prompt message position */
	EXT Gstroke UD_stkrec;			/* initial stroke */
	EXT Gstrokerec UD_stkinit;		/* stroke data record */
	EXT UD_WINDOW_STATE_REC UD_winstate[UD_MAX_NUM_WIN]; /* window state stack */
	EXT int UD_winstate_ptr;								  	/* window stack pointer */
#endif

#undef EXT
#define DASGKSH
#endif
