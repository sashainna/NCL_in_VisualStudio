/********************************************************************* 
**  NAME:  gistr.h
**
**      GKS  file for input stuff
**
**  COPYRIGHT  1984  UNICAD, Inc.
**  MODULE NAME AND RELEASE LEVEL 
**       gistr.h , 25.1
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

#ifndef GISTRH

#include "gobas.h"
#include "gi1.h"
#include "goatt.h"

/**********		STRING INPUT TYPES	**********/

/* Gstringrec  -  STRING data RECord */

	typedef struct	{
		Gint		bufsiz;		/* buffer size */
		Gint		position;	/* position in buffer */
		Gchar 	*prompt;		/* pointer to prompt */
		Gint lins,cols;		/* lines, cols in echoarea*/
		Gint     perm_flag;	/* If 1, then leave window up for loc, pick
										else take down for pick/loc					*/
		Gint border_color;   /* border color of window */
		Gint bckgrnd_color;  /* background color of window */
	} Gstringrec;


/* Gstringst  -  STRING STate */

	typedef struct	{
		Gimode	mode;			/* mode */
		Gesw		esw;			/* echo switch */
		Gchar		*string;		/* string data */
		Gpet		pet;			/* prompt and echo type */
		Gdrect	e_area;		/* echo area */
		Gstringrec	record;	/* string data record */
		Gchar  initstring[1024];	/* initial string */
	} Gstringst;


/* Gdefstring  -  DEFault STRING data */

	typedef struct	{
		Gint		bufsiz;		/* initial buffer size */
		Gint		n_pets;		/* number of prompt and echo types */
		Gpet		*pets;		/* list of prompt and echo types */
		Gdrect	e_area;		/* default echo area */
		Gstringrec	record;	/* default string data record */
	} Gdefstring;

/* Gqstring  -  reQuest STRING */

	typedef struct	{
		Gstatus	status;		/* request status */
		Gchar		*string;		/* string data */
	} Gqstring;
#define GISTRH
#endif
