/********************************************************************* 
**  NAME:   gofac.h
**
**		Output Type definitions for GKS. 
**
**  COPYRIGHT  1984  UNICAD, Inc.
**  MODULE NAME AND RELEASE LEVEL 
**       gofac.h , 25.1
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


#ifndef GOFACH

#include "gobas.h"
#include "goatt.h"
#include "go1.h"

/**********		FACILITIES TYPES		**********/

/* Gcofac  -  COlor FACilities */

	typedef struct	{
		Gint		colors;		/* number of colors */
		Gcoavail	coavail;		/* color availability */
		Gint		predefined;	/* number of predefined bundles */
	} Gcofac;


/* Gflfac  -  FiLl area FACilities */

	typedef struct	{
		Gint		n_inter;		/* number of interior styles */
		Gflstyle *interiors;	/* list of available interior styles */
		Gint		n_hatch;		/* number of hatch styles */
		Gint		*hatches;	/* list of available hatch styles */
		Gint		predefined;	/* number of predefined bundles */
	} Gflfac;

/* Ggdpfac  -  GDP FACilities */

	typedef struct	{
		Gint		number;		/* number of GDPs */
		Gattrs	*attrs;		/* list of attributes used */
	} Ggdpfac;

/* Glnfac  -  polyLiNe FACilities */

	typedef struct	{
		Gint		types;		/* number of line types */
		Gint		*list;		/* list of available line types */
		Gint		widths;		/* number of line widths */
		Gdc		nom;			/* nominal width */
		Gdc		min;			/* minimum width */
		Gdc		max;			/* maximum width */
		Gint		predefined;	/* number of predefined bundles */
	} Glnfac;


/* Gmkfac  -  polyMarKer FACilities */

	typedef struct	{
		Gint		types;		/* number of marker types */
		Gint		*list;		/* list of available marker types */
		Gint		sizes;		/* number of marker sizes */
		Gdc		nom;			/* nominal size */
		Gdc		min;			/* minimum size */
		Gdc		max;			/* maximum size */
		Gint		predefined;	/* number of predefined bundles */
	} Gmkfac;

/* Gptfac  -  PaTtern FACilities */

	typedef struct	{
		Gint		types;		/* number of pattern types */
		Gint		predefined;	/* number of predefined bundles */
	} Gptfac;


/* Gtxfac  -  TeXt FACilities */

	typedef struct	{
		Gint		fps;			/* number of fonts and precisions */
		Gtxfp		*fp_list; 	/* pointer to array of available fonts and precisions */
		Gint		heights;		/* number of character heights */
		Gdc		nom_ht;		/* nominal height */
		Gdc		min_ht;		/* minimum height */
		Gdc		max_ht;		/* maximum height */
		Gint		expansions;	/* number of character expansion factors */
		Gdc		nom_ex;		/* nominal expansion factor */
		Gdc		min_ex;		/* minimum expansion factor */
		Gdc		max_ex;		/* maximum expansion factor */
		Gint		predefined;	/* number of predefined bundles */
	} Gtxfac;
#define GOFACH
#endif
