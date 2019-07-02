/********************************************************************* 
**  NAME:  gichoice.h
**
**      GKS  file for input stuff
**
**  COPYRIGHT  1984  UNICAD, Inc.
**  MODULE NAME AND RELEASE LEVEL 
**       gichoice.h , 25.1
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

#ifndef GICHOICEH

#include "gobas.h"
#include "gi1.h"
#include "go3.h"
#include "goatt.h"
#include "goseg.h"
#include "gomisc.h"

/**********		CHOICE INPUT TYPES	**********/

/* Gchoicerec  -  CHOICE data RECord */

	typedef struct	{
		/* the following 4 fields are described in the ANSI GKS C binding */
		Gint		number;		/* number of choices */
		Gtoggle	*enable;		/* prompt enables. Not implemented yet. */
		Gchar		**strings;	/* choice strings. Not used for pet=5 */
		Gseg		seg;			/* for pet=5, segment defining the iconic menu */
		/* the following 4 fields are UNICAD specific for pet==5 */
		Gnrect *chposn;		/* ptr to array of positions (fractions of e_area) 
										of each choice for pet=5. */
		Gint *menupt;			/* Input: Pointer to menulist data buffer.
										Output: Pointer to menulist for writing device
													dependant menu file					*/
		Gint len;				/* Output: Len of data pointed to by menupt.	*/
		char *devfname;		/* For pet=5, Pointer to File name of device 
										specific icon file or NULL. If this file does 
										not exist, it will be created and the chposn field
										info will be written to the file. If it exists, 
										it will be used to define the menu. The chposn 
										info from the file will be used instead of the
										info in the chposn field. */
		Gint hilite[2];			/* Used as bit field for keeping track of hilighted
										choice devices.											*/
		/* the following colors are only used for text menus (i.e. pet!=5) */
		/* For pet=5, the segment defining the menu specifies the colors */
		Gcolor bordcolor;		/* menu border color */
		Gcolor txcolor;		/* menu text color */
		Gcolor bkcolor;		/* menu background color */
	} Gchoicerec;


/* Gchoicest  -  CHOICE STate */

	typedef struct	{
		Gimode	mode;			/* mode */
		Gesw		esw;			/* echo switch */
		Gint		choice;		/* choice data */
		Gpet		pet;			/* prompt and echo type */
		Gdrect	e_area;		/* echo area */
		Gchoicerec	record;	/* choice data record */
	} Gchoicest;


/* Gdefchoice  -  DEFault CHOICE data */

	typedef struct	{
		Gint		choices;		/* maximum number of choices */
		Gint		n_pets;		/* number of prompt and echo types */
		Gpet		*pets;		/* list of prompt and echo types */
		Gdrect	e_area;		/* default echo area */
		Gchoicerec	record;	/* default choice data record */
	} Gdefchoice;

/* Gqchoice  -  reQuest CHOICE */

	typedef struct	{
		Gstatus	status;		/* request status */
		Gint		choice;		/* choice data */
	} Gqchoice;
#define GICHOICEH
#endif
