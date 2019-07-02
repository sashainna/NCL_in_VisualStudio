
/********************************************************************* 
**  NAME:  wsi24trm.h
**
**  COPYRIGHT  1984  UNICAD, Inc.
**
**	 CONTAINS
**
**  MODULE NAME AND RELEASE LEVEL 
**       wsi24trm.h , 25.1
**  DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:11
**
*********************************************************************/

#ifndef WSI24TRMH


typedef struct {
	int ll[2];				/* Lower left of screen */
	int ur[2];				/* Upper right of screen */
	int lines;				/* Number of lines in screen */
	int cols;				/* Number of columns in screen */
	int curx;				/* Current cursor column */
	int cury;				/* Current cursor row */
	int indx;				/* Current first line of screen data */
	int top;					/* Current top of screen in data */
	int vis;					/* TRUE, screen is visible */
	int border;				/* Color of border */
	int bckgrnd;			/* Color of bckgrnd */
	
	char data[48][114];	/* Screen data held here */
} UW_Term_data;

UW_Term_data *uw_iris_term_on();

#define WSI24TRMH
#endif
