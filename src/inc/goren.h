/********************************************************************* 
**  NAME:   goren.h
**
**		Output Type definitions for DIGS rendering. 
**
**  COPYRIGHT  1984  UNICAD, Inc.
**  MODULE NAME AND RELEASE LEVEL 
**       goren.h , 25.1
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

#ifndef GORENH		
#include "gobas.h"
#include "gomisc.h"
#include "gobndl.h"

/**********		RENDERING TYPES	**********/

/* Gcolr   -  COLoR   Defines an absolute color and its model (rgb,hsi,hsv) */
	typedef struct {
		Gfloat red;
		Gfloat green;
		Gfloat blue;
		Gindex model;
	} Gcolr;

/* Glight  -  LIGHT source types */

	typedef enum	{ 
		UG_AMBIENT, 
		UG_DIRECTIONAL, 
		UG_POINT, 
		UG_SPOT
	} Glight;


/* Glsd - Light source data */

	typedef struct {
		Gcolr color;	/* Color of this light specified in current
							 * PHIGS+ color model.  
							 */
		Gwpoint3	pos;	/* Position of the light, only valid for
							 * types UG_POINT and UG_SPOT. 
							 */
		Gwpoint3 dir;	/* Direction of this light, only valid for
							 * types UG_DIRECTIONAL and UG_SPOT. 
							 */
		Gfloat conc;	/*	Concentration exponent, only valid for
							 * type UG_SPOT. 
							 */
		Gfloat spread;	/* Spread angle, only valid for type UG_SPOT.
							 * Light source has no contribution outside of 
							 * angle spread.
							 */
		Gfloat c1, c2;	/* Attenuation coefficients, only valid for
							 * types UG_POINT and UG_SPOT. 
							 */
	} Glsd;


/* Glighttbl  -  LIGHT source TaBeL */

	typedef struct	{ 
		Glight type;		/* Light source types */
		Glsd data;			/* Light source data records */
	} Glighttbl;

/* Gcull CULL mode for polygons */
	typedef enum {
		UG_NOCULL,
		UG_BACK,
		UG_FRONT
	} Gcull;

/* Gnorm NORMal vector definition */
	typedef struct {
		Gfloat x;
		Gfloat y;
		Gfloat z;
	} Gnorm;

#define GORENH
#endif
