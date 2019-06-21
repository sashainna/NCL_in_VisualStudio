
/********************************************************************* 
**  NAME:  file wstty.h
**
**      GKS tty workstation include file.
**
**  COPYRIGHT  1984  UNICAD, Inc.
**  MODULE NAME AND RELEASE LEVEL 
**       wstty.h , 25.1
**  DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:23
**
*********************************************************************/

#ifndef WSTTYH

#include "gcurses.h"
#include "gows.h"

typedef struct 	{ 

						Gws wid;					/* Workstation id */

						int DEVXMAX;			/* Device maximum x coordinate */
						int DEVYMAX;			/* Device maximum y coordinate */

						WINDOW *msgwin;		/* Message window */
						WINDOW *menuwin;		/* Menu window */
						WINDOW *current_win;	/* Current window */
						char line_char;		/* Character to plot with */
						int redraw;				/* True if we need to redraw */

						struct {
							Gfloat scale; Gfloat dx,dy;
						} wsxform;

						} UG_ttydat;


#define WSTTYH
#endif
