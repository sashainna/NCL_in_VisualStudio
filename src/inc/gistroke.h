/********************************************************************* 
**  NAME:  gistroke.h
**
**      GKS  file for input stuff
**
**  COPYRIGHT  1984  UNICAD, Inc.
**  MODULE NAME AND RELEASE LEVEL 
**       gistroke.h , 25.1
**  DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:20
**
**  PARAMETERS   
**      INPUT:  none 
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/

#ifndef GISTROKEH

#include "gobas.h"
#include "gi1.h"
#include "go1.h"
#include "gobndl.h"


/**********		STROKE INPUT TYPES	**********/

/* define maximum number of points for each stroke device */
#define UG_MAXSTROKEPTS 3000

/* Gstroke  -  STROKE data */

	typedef struct	{
		Gint		transform;	/* normalization transformation number */
		Gint		n_points;	/* number of points in stroke */
		Gwpoint	*points;		/* points in stroke */
	} Gstroke;


/* Gstrokerec  -  STROKE data RECord */

	typedef struct	{
		Gint		bufsiz;		/* input buffer size */
		Gint		editpos;		/* editing position */
		Gwpoint	interval;	/* x,y interval */
		Gfloat	time;			/* time interval */
		Gacf		acf;			/* attribute control flag */
		Gasf		ln_type;		/* line type aspect source flag */
		Gasf		ln_width;	/* line width aspect source flag */
		Gasf		ln_color;	/* line color aspect source flag */
		Gasf		mk_type;		/* marker type aspect source flag */
		Gasf		mk_size;		/* marker size aspect source flag */
		Gasf		mk_color;	/* marker color aspect source flag */
		Gindex	line;			/* polyline index */
		Glnbundl	lnbundl;		/* polyline bundle */
		Gindex	mark;			/* polymarker index */
		Gmkbundl	mkbundl;		/* polymarker bundle */
		Gint		(*funct)();	/* function to call at each stroke point */
		Gchar		*prompt;		/* prompt */
	} Gstrokerec;


/* Gstrokest  -  STROKE STate */

	typedef struct	{
		Gimode	mode;			/* mode */
		Gesw		esw;			/* echo switch */
		Gstroke	stroke;		/* stroke data */
		Gpet		pet;			/* prompt and echo type */
		Gdrect	e_area;		/* echo area */
		Gstrokerec	record;	/* stroke data record */
	} Gstrokest;


/* Gdefstroke  -  DEFault STROKE data */

	typedef struct {
		Gint		bufsiz;		/* initial buffer size */
		Gint		n_pets;		/* number of prompt and echo types */
		Gpet		*pets;		/* list of prompt and echo types */
		Gdrect	e_area;		/* default echo area */
		Gstrokerec	record;	/* default stroke data record */
	} Gdefstroke;

/* Gqstroke  -  reQuest STROKE */

	typedef struct	{
		Gstatus	status;		/* request status */
		Gstroke	stroke;		/* stroke data */
	} Gqstroke;
#define GISTROKEH
#endif
