/*********************************************************************
**    NAME         :  gkstblopst.h
**       CONTAINS:
**       All GKS external data tables and compile time constants.
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       gtblopst.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:23
*********************************************************************/
#ifndef GKSTBLOPSTH

#include "ustdio.h"
#include "gomisc.h"
#include "goren.h"
#include "gtbldef.h"
#include "gobndl.h"
#include "gipick.h"

typedef Gint UG_elptr;			/* Element pointer. Not in standard GKS */

typedef struct {				/* UG_prat3 - primitive attributes */
	Gindex lnindex;			/* polyline index */
	Glnbundl lnbundl;			/* polyline bundle: linetype, linewidth
										scale factor, color index */
	Gindex mkindex;			/* polymarker index (1..n) */
	Gmkbundl mkbundl;			/* polymarker bundle: marker type, size scale
										factor, color index */
	Gindex txindex;			/* text index */
	Gtxbundl txbundl;			/* text bundle: font/precision, expansion 
										factor, char spacing, color index */
	Gchrht txht;				/* char height */
	Gwpoint3 txuv;				/* char up vector */
	Gtxpath txpath;			/* character path (TP_LEFT,TP_RIGHT,TP_UP,TP_DOWN)*/
	Gtxalign txalign;			/* text alignment(horiz and vert) */
	Gwpoint3 txpvec;			/* char plane vector */
	Gindex flindex;			/* fill area index */
	Gflbundl flbundl;			/* fill area bundle */
	Gwpoint  patsize;			/* pattern size */
	Gwpoint patrefpt;			/* pattern reference point */
	Gwpoint3 patpvec;			/* pattern plane vector */
	Gwpoint3 patupvec;		/* pattern up vector */
	Gpickid pickid;			/* pickid */
	Gasfs asfs;					/* aspect source flags (INDIVIDUAL,BUNDLED) */
} UG_prat3;						/* end of primitive attributes */

typedef struct {			/* UG_vtran3 -- 3D viewing transformation */
	Gwrect3 window;		/* window  (UVN) */
	Gnrect3 vport;			/* viewport (NDC) */
	Gvtype vtype;			/* viewport type (PARALLEL,PERSPECTIVE) */
	Gwpoint3 vrefpt;		/* view reference point (World )*/
	Gwpoint3 vpnorm;		/* view plane normal (World) */
	Gwpoint3 vup;			/* view up vector (World) */
	Gwpoint3 prp;			/* projection reference point (UVN) for PERSPECTIVE */
	Gclip winclip;			/* window clipping indicator(CLIP,NOCLIP)=CLIP */
	Gclip backclip;		/* back clipping indicator = CLIP */
	Gclip frontclip;		/* front clipping indicator = CLIP */
	Gint inputprio;		/* input priority (0..n) = 0 */
} UG_vtran3;

typedef struct {				/* UG_rendatt - PHIGS+ rendering attributes */
	Gcolr ffcolr;				/* Front face interior color */
	Gfloat ffambrfl;			/* Front face ambient reflection coefficient */
	Gfloat ffdifrfl;			/* Front face diffuse reflection coefficient */
	Gfloat ffspcrfl;			/* Front face specular reflection coefficient */
	Gcolr ffspccolr;			/* Front face specular color */
	Gfloat ffspcexp;			/* Front face specular exponent */
	Gfloat fftrnsp;			/* Front face transparency coefficient */
	Gint bfmode;				/* Back face distinguish mode */
	Gcull cull;					/* Face culling mode NONE, BACK, or FRONT */
	Gcolr bfcolr;				/* Back face interior color */
	Gfloat bfambrfl;			/* Back face ambient reflection coefficient */
	Gfloat bfdifrfl;			/* Back face diffuse reflection coefficient */
	Gfloat bfspcrfl;			/* Back face specular reflection coefficient */
	Gcolr bfspccolr;			/* Back face specular color */
	Gfloat bfspcexp;			/* Back face specular exponent */
	Gfloat bftrnsp;			/* Back face transparency coefficient */
	Gindex depthcue;			/* Depth cue index */
	Gindex colorapp;			/* Color approximation index */
	Gint model;					/* Rendering color model */
	Gcolr polylncolr;			/* Polyline color */
	Gcolr polymkcolr;			/* Polymarker color */
	Gcolr textcolr;			/* Text color */
	Gcolr intrcolr;			/* Interior color */
	Gcolr edgecolr;			/* Edge color */
	Gasf polylncsf;			/* Polyline CSF (direct/indexed) */
	Gasf polymkcsf;			/* Polymarker CSF (direct/indexed) */
	Gasf textcsf;				/* Text CSF (direct/indexed) */
	Gasf intrcsf;				/* Interior CSF (direct/indexed) */
	Gasf edgecsf;				/* Edge CSF (direct/indexed) */
	Gint polyshademethod;	/* Polyline shading method */
	Gint intrshademethod;	/* Interior shading method */
	Gint intrlightmethod;	/* Interior lighting method */
	Gasf polyshadeasf;		/* Polyline shading method ASF */
	Gasf intrshadeasf;		/* Interior shading method ASF */
	Gasf intrlightasf;		/* Interior lighting method ASF */
	Gindex lightstate[GMAXLIGHTS];/* list of light source states */
} UG_rendatt;						/* end of PHIGS+ rendering attributes */

/***** GKS OPERATING STATE ******/
typedef struct {
	Gos sysstate;			/* system state value (GKCL,GKOP,WSOP,WSAC,SGOP) */
	FILE *erfile;				/* error file */
} UG_gksos;						/* endof gks operating state */
#define GKSTBLOPSTH
#endif
