/********************************************************************* 
**  NAME:  gksipick.h
**
**      GKS  file for input stuff
**
**  COPYRIGHT  1984  UNICAD, Inc.
**  MODULE NAME AND RELEASE LEVEL 
**       gipick.h , 25.1
**  DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:19
**
**  PARAMETERS   
**      INPUT:  none 
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/

#ifndef GKSIPICKH

#include "gobas.h"
#include "gi1.h"
#include "goseg.h"
#include "goatt.h"

/**********		PICK INPUT TYPES		**********/

/* Gpickid  -  PICK IDentifier */

	typedef Gint	Gpickid;


/* Gpick  -  PICK data */

	typedef struct	{
		Gpstat	status;		/* pick status */
		Gseg		*seg;			/* pick segment */
		Gpickid	pickid;		/* pick identifier */
	} Gpick;

/* Gpicks - PICK data for PHIGS -- has array of segments */

	typedef struct {
		Gpstat status;			/* pick status */
		Gint depth;				/* depth of pick path */
		Gpickid *pickpath;	/* pointer to array of segments and pickid at end  */
	} Gpicks;

/* Gpickrec  -  PICK data RECord (implementation dependent) */

	typedef struct {		/* THIS MAY NEED TO BE CHANGED */
		Gchar *prompt;
	} Gpickrec;


/* Gpickst  -  PICK STate */

	typedef struct	{
		Gimode	mode;			/* mode */
		Gesw		esw;			/* echo switch */
		Gpicks	pick;			/* pick data */
		Gpet		pet;			/* prompt and echo type */
		Gdrect	e_area;		/* echo area */
		Gpickrec	record;		/* pick data record */
	} Gpickst;


/* Gdefpick  -  DEFault PICK data */

	typedef struct	{
		Gint		n_pets;		/* number of prompt and echo types */
		Gpet		*pets;		/* list of prompt and echo types */
		Gdrect	e_area;		/* default echo area */
		Gpickrec	record;		/* default pick data record */
	} Gdefpick;

/* Gqpick  -  reQuest PICK */

	typedef struct	{
		Gstatus status;		/* request status */
		Gseg		seg;			/* segment */
		Gpickid	pickid;		/* pick identifier */
	} Gqpick;

/* Gqpicks - reQuest PICKS - GKS LVL 3 extension. Not in GKS */

	typedef struct {
		Gstatus status;		/* request status */
		Gint depth;				/* depth of pick path */
		Gpickid *pickpath;	/* pointer to array of segments and pickid at end  */
	} Gqpicks;
#define GKSIPICKH
#endif
