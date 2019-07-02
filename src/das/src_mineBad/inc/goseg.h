/********************************************************************* 
**  NAME:   gksoseg.h
**
**		Output Type definitions for GKS. 
**
**  COPYRIGHT  1984  UNICAD, Inc.
**  MODULE NAME AND RELEASE LEVEL 
**       goseg.h , 25.1
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


#ifndef GKSOSEGH
#include "gobas.h"
#include "go2.h"
/**********		SEGMENT TYPES		**********/

/* Gseg  -  SEGment (implementation dependent) */

	typedef Gint	Gseg;			/* MAY POSSIBLY NEED TO BE CHANGED !! */


/* Gsegpri  -  SEGment PRIority */

	typedef unsigned char Gsegpri;

/* Gsegtran  -  SEGment TRANsformation */

	typedef struct	{
		Gfloat	m11;			/* matrix element  1, 1  */
		Gfloat	m12;			/* matrix element  1, 2  */
		Gndc		m13;			/* matrix element  1, 3  */
		Gfloat	m21;			/* matrix element  2, 1  */
		Gfloat	m22;			/* matrix element  2, 2  */
		Gndc		m23;			/* matrix element  2, 3  */
	} Gsegtran;


/* Gsegtran3  -  3-D SEGment TRANsformation */

	typedef struct	{
		Gfloat	m11;			/* matrix element  1, 1  */
		Gfloat	m12;			/* matrix element  1, 2  */
		Gfloat	m13;			/* matrix element  1, 3  */
		Gndc		m14;			/* matrix element  1, 4  */
		Gfloat	m21;			/* matrix element  2, 1  */
		Gfloat	m22;			/* matrix element  2, 2  */
		Gfloat	m23;			/* matrix element  2, 3  */
		Gndc		m24;			/* matrix element  2, 4  */
		Gfloat	m31;			/* matrix element  3, 1  */
		Gfloat	m32;			/* matrix element  3, 2  */
		Gfloat	m33;			/* matrix element  3, 3  */
		Gndc		m34;			/* matrix element  3, 4  */
	} Gsegtran3;


/* Gmodseg  -  dynamic MODification of SEGment attributes */

	typedef struct	{
		Gmodtype	transform;	/* transformation */
		Gmodtype	appear;		/* appearing (turning visible) */
		Gmodtype	disappear;	/* disappearing (turning invisible) */
		Gmodtype	hilite;		/* hilight */
		Gmodtype	priority;	/* priority */
		Gmodtype	addition;	/* addition of primitives to segment */
		Gmodtype	deletion;	/* deletion of segment */
	} Gmodseg;

/* Gtran -- transformation matrix -- */

	typedef Gfloat Gtran[4][4]; 
#define GKSOSEGH
#endif
