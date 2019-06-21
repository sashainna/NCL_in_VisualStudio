/*********************************************************************
**    NAME         :  gtblws.h
**       CONTAINS:
**       All GKS external data tables and compile time constants.
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       gtblws.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:24
*********************************************************************/
#ifndef GKSTBLWSH

#include "go.h"
#include "gi.h"

/******* WORKSTATION DESCRIPTION TABLE *********/
typedef struct {			/* entries in this sub-structure of the
									workstation description table do not exist
									for workstations of category INPUT */
	Gwsclass wsclass;		/* workstation classification (VECTOR,RASTER,OTHER)*/	
								/* dynamic modification accepted for: */
	Gmodtype modstrcont;	/* structure content (IRG,IMM) */
	Gmodtype modvrep;		/* view representation (IRG,IMM) */
	Gmodws modwsattr;		/* workstation attributes of: polyline, polymarker,
									text,fill area,  pattern, 
									color, ws transform (IRG,IMM) */
								/* IRG means implicit regeneration necessary */
								/* IMM means performed immediately */
	Gdefmode defdef;		/* default deferral mode (ASAP,BNIG,BNIL,ASTI,WAIT) */
	Gmodmode defmod;		/* default modification mode (NIVE,UWOR,UQUM,PRIN) */
	Glnfac lnfac;			/* polyline facilities: no. linetypes, list of
									avail linetypes, no. linewisths, nominal, min,
									max linewidths, no. predefined polyline bundles */
	Glnbundl *ppobpt;		/* pointer to array of predefined polyline bundles */
	Gmkfac mkfac;			/* polymarker facilities: no. marker types, list of
									avail marker types, no. marker sizes, nominal,min,
									max size, no. predefined polymarker bundles */
	Gmkbundl *ppmbpt;		/* pointer to array of predefined polymarker bundles*/
	Gtxfac txfac;			/* text facilities: no. font&precision pairs, list of
									font/precision pairs, no. of char heights, nominal,
									min,max height, no. char expansion factors, nominal,
									min,max expansion factor, no. predefined bundles */
	Gtxbundl *ptbpt;		/* pointer to array of predefined text bundles */
	Gflfac flfac;			/* fill area facilities: no. interior styles, list of
									interior styles, (no. hatch styles, list of hatch
									styles, in GKS only), no. predefined bundles */
	Gflbundl *flbundl;	/* pointer to array of predefined fill area bundles, 
									containing interior style, style index, color index */
	Gptfac ptfac;			/* pattern facilities: no. pattern types, no.
									predefined bundles */
	Gptbundl *ptbundl;	/* pointer to array of predefined pattern bundles:
									pattern array size, ptr to pattern array */
	Gcofac cofac;			/* color facilities: workstation color model, no.
									avail colors, color avail, no. predefined
									color bundles */
	Gcobundl *cobundl;	/* pointer to array of predefined color bundles:
									each a color triplet */
	Ggdpfac gdpfac;		/* gdp facilities: no. available gdp's, list of
									attributes used(gks only) */
	Ggdp *gdpavail;		/* pointer to array of available gdp's: gdp id,
									no. sets of attrs used, list of sets of attrs used*/
	Gint maxpob;			/* maxn no of polyline bundle table entries (5..n)*/
	Gint maxpmb;			/* max no of polymarker bundle table entries(5..n)*/
	Gint maxtxb;			/* max no of text bundle table entries (2..n) */
	Gint maxfab;			/* max no of fill area bundle table entries (5..n) */
	Gint maxpati;			/* max no of pattern indices (0..n) */
	Gint maxcolri;			/* max no of color indices (2..n) */
	Gint maxprio;			/* max number of segment priorities (0..n) n<=255 */
								/* Zero indicates continuous range of priorities */
	Gmodtype modcontent;	/* dynamic modification of segment content(IRG,IMM) */
	Gmodtype modprio;		/* modification of segment priority (IRG,IMM) */
	Gmodtype modunpost;	/* unpost segment(IRG,IMM) */
	Gmodtype modadd;		/* adding primitives to open segment overlapping
									segment of higher priority (IRG,IMM) */
	Gmodtype moddel;		/* modification accepted for delete segment(IRG,IMM)*/
								/* IRG: implicit regeneration necessary(may be deferred)*/
								/* IMM: performed immediately */
	Gint maxlights;		/* Max number of light source indices */
	Glighttbl *lights;	/* Pointer to table of defined light sources */
} UG_outwdt;

typedef struct {				/* entries in this sub-structure of the workstation
									description table do not exist for workstations
									of categories OUTPUT */
	Gint nloc;					/* number of input devices of class LOCATOR (1..n) */
	Gdefloc *defloc;			/* pointer to array of LOCATOR devices */
	Gint 	nstroke;				/* number of input devices of class STROKE (1..n) */
	Gdefstroke *defstk;		/* pointer to array or STROKE devices */
	Gint nval;					/* number of input devices of class VALUATOR (1..n) */
	Gdefval *defval;			/* pointer to array of VALUATOR devices */
	Gint nchoice;				/* number of input devices of class CHOICE (1..n) */
	Gdefchoice *defcho;		/* pointer to array of CHOICE devices */
	Gint npick;					/* number of input devices of class PICK (1..n) */
	Gdefpick *defpick;		/* pointer to array of PICK devices */
	Gint nstring;				/* number of devices of class STRING (1..n) */
	Gdefstring *defstr;		/* pointer to array of STRING devices */
} UG_inwdt;

/* One per workstation type. In my implementation, the table is 
in the workstation. Thus there is really one per workstation */

typedef struct {
	Gchar type[16];		/* workstation type */
	Gwscat category;		/* workstation category */
	Gdspsize dspsize;		/* device units, size in DC, size in raster */
	Gint rowmax,colmax;	/* number rows,cols of text */
	Gint nvtindex;			/* no. definable viewing table indices (20..n) */
	Gint needndc;			/* 1=need NDC segments, 0 = don't */
	Gint savescrn;			/* 1=UG_DSAVESCRN entry works, 0=doesn't */
	Gint bitsperpixel;	/* number of bit planes */
	UG_outwdt *outwdtpt;	/* pointer to OUT wdt data */
	UG_inwdt *inwdtpt;	/* pointer to IN wdt data */
} UG_wdt;


/*   WORKSTATION STATE LIST -- one for every open wkstn*/
typedef struct {			/* UG_outwssl -- output wssl stuff */
	Gdefmode defmode;		/* workstation deferral mode (ASAP, BNIG, etc) */
	Gmodmode modmode;		/* modification mode (NIVE, UWOR, UQUM, PRIN */
	Gdspsurf dspsurf;		/* display surface empty: EMPTY or NOTEMPTY */
	Gnframe nframe;		/* new frame action necessary at update: NO or YES */
	Gwstran3 reqxform;	/* requested workstation window/viewport */
	Gwstran3 curxform;	/* current workstation window/viewport */
	Gint hlhsrupdate;		/* HLHSR update state (pending/notpending) */
	Gint hlhsrmode;		/* Current hlhsr mode */
	Gint rhlhsrmode;		/* Requested hlhsr mode */
} UG_outwssl;					/* output wssl stuff */

typedef struct {			/* UG_inwssl -- input wssl stuff */
	Gint nloc;				/* number of locator devices */
	Glocst  *locdata;		/* pointer to array of locator states */
	Gint nstroke;			/* number of stroke devices */
	Gstrokest *strokedata;	/* pointer to array of stroke states */
	Gint nval;				/* number of valuator devices */
	Gvalst *valdata;		/* pointer to array of valuator states */
	Gint nchoice;			/* number of choice devices */
	Gchoicest  *choicedata;	/* pointer to array of choice states */
	Gint npick;			/* number of pick devices */
	Gpickst  *pickdata; /* pointer to array of pick states */
	Gint nstring;			/* number of string devices */
	Gstringst *stringdata;	/* pointer to array of string states */
} UG_inwssl;

typedef struct {
	Gws id;					/* workstation index into ug_gksstli.wsopen array */
	Gwsct wsct;				/* workstation connection(FILE *) and type(Gchar *) */
	Gint (**connid)();	/* connection id- pointer to ws entry table */
	UG_wdt *wdtptr;			/* pointer to wkstn's wdt */
	Gwsstate state;		/* ACTIVE or INACTIVE */
	UG_outwssl *outptr;		/* pointer to output wssl stuff */
	UG_inwssl *inptr;		/* pointer to input wssl stuff */
} UG_wssl;

typedef struct {			/* segment attributes */
	Gsegvis gvis;			/* visibility */
	Gseghi ghilit;			/* highlighted */
	Gsegpri prio;			/* display priority */
	Gsegdet gdtect;		/* detectable */
} UG_segat;
#define GKSTBLWSH
#endif
