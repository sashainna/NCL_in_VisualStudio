/********************************************************************* 
**  NAME:  gksiloc.h
**
**      GKS  file for input stuff
**
**  COPYRIGHT  1984  UNICAD, Inc.
**  MODULE NAME AND RELEASE LEVEL 
**       giloc.h , 25.1
**  DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:18
**
**  PARAMETERS   
**      INPUT:  none 
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/

#ifndef GKSILOCH

#include "gobas.h"
#include "gomisc.h"
#include "go1.h"
#include "go2.h"
#include "gi1.h"
#include "gobndl.h"

/**********		LOCATOR INPUT TYPES	**********/

/* Gloc  -  LOCator data */

	typedef struct	{
		Gint		transform;	/* normalization transformation number */
		Gwpoint	position;	/* locator position */
	} Gloc;


/* Glocrec  -  LOCator data RECord */

	typedef struct	{
		Gpfcf		pfcf;			/* polyline fill area control flag */
		Gacf		acf;			/* attribute control flag */
		Gasf		ln_type;		/* line type aspect source flag */
		Gasf		ln_width;	/* line width aspect source flag */
		Gasf		ln_color;	/* line color aspect source flag */
		Gasf		fl_inter;	/* fill area interior aspect source flag */
		Gasf		fl_style;	/* fill area style aspect source flag */
		Gasf		fl_color;	/* fill area color aspect source flag */
		Gindex	line;			/* polyline index */
		Glnbundl	lnbundl;		/* polyline bundle */
		Gindex	fill;			/* fill area index */
		Gflbundl	flbundl;		/* fill area bundle */
		Gchar		*prompt;		/* prompt */
		Gint		seg;			/* segment number to drag, for pet 21 */
		Gnpoint3 attach;		/* point in seg to attach loc device, for pet 21 */
	} Glocrec;


/* Glocst  -  LOCator STate */

	typedef struct	{
		Gimode	mode;			/* mode */
		Gesw		esw;			/* echo switch */
		Gloc		loc;			/* locator data */
		Gpet		pet;			/* prompt and echo type */
		Gdrect	e_area;		/* echo area */
		Glocrec	record;		/* locator data record */
	} Glocst;


/* Gdefloc  -  DEFault LOCator data */

	typedef struct	{
		Gwpoint	position;	/* initial position */
		Gint		n_pets;		/* number of prompt and echo types */
		Gpet		*pets;		/* list of prompt and echo types */
		Gdrect	e_area;		/* default echo area */
		Glocrec	record;		/* default locator data record */
	} Gdefloc;

/* Gqloc  -  reQuest LOCator */

	typedef struct	{
		Gstatus	status;		/* request status */
		Gloc		loc;			/* locator data */
	} Gqloc;
#define GKSILOCH
#endif
