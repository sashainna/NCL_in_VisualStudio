/********************************************************************* 
**  NAME:   gksoatt.h
**
**		Output Type definitions for GKS. 
**
**  COPYRIGHT  1984  UNICAD, Inc.
**  MODULE NAME AND RELEASE LEVEL 
**       goatt.h , 25.1
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


#ifndef GKSOATTH
#include "gobas.h"
#include "go3.h"
/**********		ATTRIBUTE TYPES	**********/

/* Gpet  -  Prompt and Echo Type */

	typedef Gint	Gpet;


/* Gchrexp  -  CHaRacter EXPansion factor */

	typedef Gfloat	Gchrexp;


/* Gchrht  -  CHaRacter HeighT */

	typedef Gwc	Gchrht;


/* Gchrsp  -  CHaRacter SPacing */

	typedef Gfloat	Gchrsp;


/* Gflstyle  -  FiLl area STYLE index */

	typedef Gint	Gflstyle;

/* Glntype  -  LiNe TYPE */

	typedef struct {
				Gint typeno;			/* linetype number (1-8 or 21) */
				/* if typeno==21, user defined linestyle. Def'n follows: */
				int npatn;				/* length of user defined pattern */
				Gfloat patnlen;		/* NDC length of pattern. */
				Gchar typepatn[100];	/* user pattern, 1's and 0's. Max len 100 */
			} Glntype;


/* Gmktype  -  MarKer TYPE */

	typedef Gint	Gmktype;


/* Gscale  -  SCALE factor */

	typedef Gfloat	Gscale;


/* Gtxfont  -  TeXt FONT */

	typedef Gint	Gtxfont;


/* Gtxfp  -  TeXt Font and Precision */

	typedef struct	{
		Gtxfont	font;			/* text font */
		Gtxprec	prec;			/* text precision */
	} Gtxfp;


/* Gtxalign  -  TeXt ALIGNment */

	typedef struct	{
		Gtxhor	hor;			/* horizontal component */
		Gtxver	ver;			/* vertical component */
	} Gtxalign;

/* Glightmodel - Lighting model to render by */

typedef enum {
	UG_PHONG,
	UG_GOURAD,
	UG_FLAT
} Glightmodel;

#define GKSOATTH
#endif
