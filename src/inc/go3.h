/********************************************************************* 
**  NAME:   gkso3.h
**
**		Output Type definitions for GKS. 
**
**  COPYRIGHT  1984  UNICAD, Inc.
**  MODULE NAME AND RELEASE LEVEL 
**       go3.h , 25.1
**  DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:21
**
**  PARAMETERS   
**      INPUT:  none 
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
/* From ANSI X3H34/83-12R1, the GKS C-language binding */

#ifndef GSEGDETH
#include "gsegdet.h"
#endif

#ifndef GKSO3H

/* Gseghi  -  SEGment highlighting */

	typedef unsigned char Gseghi;
#define	UG_NORMAL 0
#define	UG_HIGHLIGHTED 1
#define	UG_PICKLIGHTED 2

/*  Gsegvis  -  SEGment VISibility */

	typedef unsigned char Gsegvis;
#define UG_VISIBLE 0
#define UG_INVISIBLE 1

/* Gtoggle  -  TOGGLE switch */

	typedef enum	{
		UG_OFF,
		UG_ON
	} Gtoggle;


/* Gtxhor  -  TeXt alignment HORizontal component */

	typedef enum	{
		UG_TH_NORMAL,
		UG_TH_LEFT,
		UG_TH_CENTRE,
		UG_TH_RIGHT
	} Gtxhor;


/* Gtxver  -  TeXt alignment VERtical component */

	typedef enum	{
		UG_TV_NORMAL,
		UG_TV_TOP,
		UG_TV_HALF,
		UG_TV_BASE,
		UG_TV_BOTTOM
	} Gtxver;


/* Gtxpath  -  TeXt PATH */

	typedef enum	{
		UG_TP_RIGHT,
		UG_TP_LEFT,
		UG_TP_UP,
		UG_TP_DOWN
	} Gtxpath;


/* Gtxprec  -  TeXt PRECision */

	typedef enum	{
		UG_STRING,
		UG_CHAR,
		UG_STROKE
	} Gtxprec;

/* Gvtype - View Type -- not in GKS, in GKS-3D */
	
	typedef enum	{
		UG_PARALLEL,
		UG_PERSPECTIVE
	} Gvtype;

/* Gwscat  -  WorkStation CATegory */

	typedef enum	{
		UG_OUTPUT,
		UG_OUTIN,
		UG_INPUT,
		UG_MO,
		UG_MI,
		UG_WISS
	} Gwscat;


/* Gwsclass  -  WorkStation CLASSification */

	typedef enum	{
		UG_RASTER,
		UG_VECTOR,
		UG_OTHER
	} Gwsclass;

/* Gwsstate  -  WorkStation STATE */

	typedef enum	{
		UG_ACTIVE,
		UG_INACTIVE
	} Gwsstate;


/* Gwstus  -  WorkStation Transformation Update State */

	typedef enum	{
		UG_PENDING,
		UG_NOTPENDING
	} Gwstus;
#define GKSO3H
#endif
