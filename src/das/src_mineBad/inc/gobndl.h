/********************************************************************* 
**  NAME:   gksobndl.h
**
**		Output Type definitions for GKS. 
**
**  COPYRIGHT  1984  UNICAD, Inc.
**  MODULE NAME AND RELEASE LEVEL 
**       gobndl.h , 25.1
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


#ifndef GKSOBNDLH
#include "gomisc.h"
#include "goatt.h"
/**********		BUNDLE TYPES		**********/

/* Gcobundl  -  COlor BUNDLe */

	typedef struct	{
		Ginten	red;			/* red intensity */
		Ginten	green;		/* green intensity */
		Ginten	blue;			/* blue intensity */
	} Gcobundl;


/* Gflbundl  -  FiLl area BUNDLe */

	typedef struct	{
		Gflinter	inter;		/* fill area interior style */
		Gflstyle	style;		/* fill area style index */
		Gcolor	color;		/* fill area color index */
		Gcobundl colorrep;	/* fill area color for rendering */
		Gtoggle edgeflag;		/* OFF or ON */
		Glightmodel model;	/* Type of lighting model to render with */
		Gfloat kd;				/* Phong diffuse coefficient */
		Gfloat ks;				/* Phong specular coefficient */
		Gfloat spec_exp;		/* Phong specular exponent */
	} Gflbundl;

/* Glnbundl  -  polyLiNe BUNDLe */

	typedef struct	{
		Glntype	type;			/* line type */
		Gscale	width;		/* linewidth scale factor */
		Gcolor	color;		/* polyline color index */
	} Glnbundl;


/*  Gmkbundl  -  polyMarKer BUNDLe */

	typedef struct	{
		Gmktype	type;			/* marker type */
		Gscale	size;			/* marker size scale factor */
		Gcolor	color;		/* polymarker color index */
	} Gmkbundl;

/* Gptbundl  -  PaTtern BUNDLe */

	typedef struct	{
		Gipoint	size;			/* pattern array size */
		Gcolor	*array;		/* pattern array */
	} Gptbundl;


/* Gtxbundl  -  TeXt BUNDLe */

	typedef struct	{
		Gtxfp		fp;			/* font and precision */
		Gchrexp	expn;			/* character expansion */
		Gchrsp	space;		/* character spacing */
		Gcolor	color;		/* text color */
	} Gtxbundl;
#define GKSOBNDLH
#endif
