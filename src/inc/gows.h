/********************************************************************* 
**  NAME:   gksows.h
**
**		Output Type definitions for GKS. 
**
**  COPYRIGHT  1984  UNICAD, Inc.
**  MODULE NAME AND RELEASE LEVEL 
**       gows.h , 25.1
**  DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:22
**
**  PARAMETERS   
**      INPUT:  none 
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
/* From ANSI X3H34/83-12R1, the GKS C-language binding */

#ifndef GKSOWSH
#include "ustdio.h"
#include "gobas.h"
#include "go1.h"
#include "go2.h"
#include "go3.h"
/**********		WORKSTATION TYPES		**********/

/* Gwsct  -  WorkStation Connection and Type */

	typedef struct	{
		FILE		*r;			/* read connection */
		FILE		*w;			/* write connection */
		Gchar		*type;		/* workstation type */
	} Gwsct;


/* Gwsdus  -  WorkStation Deferral and Update State */

	typedef struct	{
		Gdefmode	defmode;		/* deferral mode */
		Gdspsurf	dspsurf;		/* display surface */
		Girgmode	irgmode;		/* implicit regeneration mode */
		Gnframe	nframe;		/* new frame action at update */
	} Gwsdus;


/* Gwsmax  -  WorkStation MAXimum numbers */

	typedef struct	{
		Gint		open;			/* number of open workstations */
		Gint		active;		/* number of active workstations */
		Gint		assoc;		/* number of associated workstations */
	} Gwsmax;


/* Gwstables  -  length of WorkStation TABLES */

	typedef struct	{
		Gint		line;			/* polyline tables */
		Gint		mark;			/* polymarker tables */
		Gint		text;			/* text tables */
		Gint		fill;			/* fill-area tables */
		Gint		pat;			/* pattern tables */
		Gint		color;		/* color tables */
	} Gwstables;


/* Gwstran  -  WorkStation TRANsformation */

	typedef struct	{
		Gnrect	w;				/* window */
		Gdrect	v;				/* vieport */
	} Gwstran;


/* Gwstran3  -  3-D WorkStation TRANsformation */

	typedef struct	{
		Gnrect3	w;				/* window */
		Gdrect3	v;				/* vieport */
	} Gwstran3;


/* Gwsti  -  WorkStation Transformation Information */

	typedef struct	{
		Gwstus	wstus;		/* workstation transformation update state */
		Gwstran	request;		/* requested transformation */
		Gwstran	current;		/* current transformation */
	} Gwsti;


/* Gwsti3  -  3-D WorkStation Transformation Information */

	typedef struct	{
		Gwstus	wstus;		/* workstation transformation update state */
		Gwstran3	request;		/* requested transformation */
		Gwstran3	current;		/* current transformation */
	} Gwsti3;


/* Gmodws  -  dynamic MODificiation of WorkStation attributes */

	typedef struct	{
		Gmodtype	line;			/* polyline */
		Gmodtype	mark;			/* polymarker */
		Gmodtype	fill;			/* fill area */
		Gmodtype	pat;			/* pattern */
		Gmodtype	color;		/* color */
		Gmodtype	wstran;		/* workstation transformation */
	} Gmodws;


/* Gws  -  WorkStation (implementation dependent) */

	typedef Gint	Gws;		/* MAY POSSIBLY NEED TO BE CHANGED !! */

typedef enum {
	UG_MAINTAIN,
	UG_REPLACE
} Gconres;

#define GKSOWSH
#endif
