/*********************************************************************
**    NAME         :  gtblvar8.h
**       CONTAINS:
**       All GKS external data tables and compile time constants for
**			the rendering package.
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       gtblvar8.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:24
*********************************************************************/
#ifndef GTBLVAR8H

/* Necessary include files */
#include "gtbldef.h"
#include "gobas.h"
#include "goatt.h"
 
/* Rendering modals */
typedef struct {
	Glightmodel model;				/* Lighting model to use */
	Gfloat ia;							/* Ambient intensity */
	Gfloat kd;							/* Diffuse coefficent */
	Gfloat ks;							/* Specular coefficent */
	Gfloat sex;							/* Specular exponent */
	Gint xres, yres;					/* Resolution of rendered images */
	Gnrect3 ndcbox;					/* Ndc region to map */
	char filename[120];				/* Output file name */
}	Grendermodals;


#ifdef RENDERMAIN
Grendermodals ug_rendermodals = { 
		UG_PHONG,					/* Lighting model  */
		0.20, 0.65, 0.15, 20.0,	/* Plastic */
 		128, 128, 					/* Resolution */
		0.0, 0.0, 1.0, 			/* Lower left front window corner */
		1.0, 1.0, 0.0, 			/* Upper right back window corner */
		"vfb.fb"						/* Output file name */
};
#else
extern Grendermodals ug_rendermodals;
#endif

#define GTBLVAR8H
#endif
