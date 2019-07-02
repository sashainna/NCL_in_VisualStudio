/********************************************************************* 
**  NAME:  gksimisc.h
**
**      GKS  file for input stuff
**
**  COPYRIGHT  1984  UNICAD, Inc.
**  MODULE NAME AND RELEASE LEVEL 
**       gimisc.h , 25.1
**  DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:18
**
**  PARAMETERS   
**      INPUT:  none 
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/

#ifndef GKSIMISCH

#include "gobas.h"
#include "gows.h"
#include "gi1.h"

/**********		MISC. INPUT TYPES		**********/

/* Gidevno  -  Input DEVice Number */

	typedef Gint	Gidevno;


/* Gnumdev  -  NUMber of input DEVices */

	typedef struct	{
		Gint	locator;			/* locators */
		Gint	stroke;			/* strokes */
		Gint	valuator;		/* valuators */
		Gint	choice;			/* choices */
		Gint	pick;				/* picks */
		Gint	string;			/* strings */
	} Gnumdev;


/* Gevent  -  EVENT */

#ifndef __cplusplus
	typedef struct	{
		Gws		*ws;			/* workstation */
		Gidevno	dev;			/* device number */
		Geclass	class;		/* event class */
	} Gevent;
#endif


/**********		INPUT REQUEST TYPES	**********/

/* Gqinfo  -  input Queue INFOrmation */

#ifndef __cplusplus
	typedef struct	{
		Gws		*ws;			/* workstation */
		Giclass	class;		/* input class */
		Gidevno	dev;			/* input device number */
	} Gqinfo;
#endif
#define GKSIMISCH
#endif
