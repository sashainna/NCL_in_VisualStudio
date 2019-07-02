/********************************************************************* 
**  NAME:  gival.h
**
**      GKS  file for input stuff
**
**  COPYRIGHT  1984  UNICAD, Inc.
**  MODULE NAME AND RELEASE LEVEL 
**       gival.h , 25.1
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

#ifndef GIVALH

#include "gobas.h"
#include "gi1.h"
#include "goatt.h"


/**********		VALUATOR INPUT TYPES		**********/

/* Gvalrec  -  VALuator data RECord */

	typedef struct	{
		Gfloat	high;			/* high range limit */
		Gfloat	low;			/* low range limit */
		Gchar		*prompt;		/* prompt */
	} Gvalrec;


/* Gvalst  -  VALuator STate */

	typedef struct	{
		Gimode	mode;			/* mode */
		Gesw		esw;			/* echo switch */
		Gfloat	val;			/* valuator data */
		Gpet		pet;			/* prompt and echo type */
		Gdrect	e_area;		/* echo area */
		Gvalrec	record;		/* valuator data record */
	} Gvalst;


/* Gdefval  -  DEFault VALuator data */

	typedef struct {
		Gfloat	value;		/* initial value */
		Gint		n_pets;		/* number of prompt and echo types */
		Gpet		*pets;		/* list of prompt and echo types */
		Gdrect	e_area;		/* default echo area */
		Gvalrec	record;		/* default valuator data record */
	} Gdefval;

/* Gqval  -  reQuest VALuator */

	typedef struct	{
		Gstatus	status;		/* request status */
		Gfloat	val;			/* valuator data */
	} Gqval;
#define GIVALH
#endif
