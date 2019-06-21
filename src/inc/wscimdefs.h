#include "usysdef.h"
#if UU_COMP == UU_CIM
/*********************************************
**
**
**		wscimdefs.h
**
**		contains cimlinc equivalent definitions
**		to sun structures, etc.
**
**    COPYRIGHT 1988 (c) Mills Data Systems Co.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       wscimdefs.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:09
**********************************************************************/
typedef struct Cim_font{
	short w, h;
	short raster[NCH * MAXCHH];
	}cim_font;

static cim_font GFX_font;

typedef struct cim_brush 
	{
	int	width;
	} Cim_brush;

struct	cim_pos {
	int	x, y;
	};

struct	cim_size {
	int	x, y;
	};

typedef struct pixrect {
        struct  pixrectops *cim_ops;     /* operations appropriate to this pr */
        struct  cim_size cim_size;	/* pixels per dimension */
        short	*cim_data;		/* device-dependent pixel-access */
} Pixrect;

#define rect_right(rp) \
	(rp)->x + (rp)->w - 1

#define rect_bottom(rp)  \
	(rp)->y + (rp)->h - 1

#define	event_id(event) \
	((event)->ie_code)

/** following should only be temporary kluge, was sun specific stuff **/
#define BITSPERBYTE 8

/** defined in sunwindow/win_input.h **/

#define IL_ERRORMSG_SIZE	256

typedef struct
	{
	short ie_code, ie_locx, ie_locy;
	} Event;

/* A whole lot of sun specific defines related to
   keyboard device and event handling */

#define ASCII_FIRST		(0)				/*     0 */
#define ASCII_LAST		(127)				/*   127 */
#define BUT_FIRST		(VKEY_FIRSTFUNC)		/* 32544 */
#define BUT_LAST                (BUT_FIRST+9)			/* 32553 */
#define KEY_LEFTFIRST           ((BUT_LAST)+1)			/* 32554 */
#define KEY_LEFTLAST            ((KEY_LEFTFIRST)+15)		/* 32569 */
#define KEY_RIGHTFIRST          ((KEY_LEFTLAST)+1)		/* 32570 */
#define KEY_RIGHTLAST           ((KEY_RIGHTFIRST)+15)		/* 32585 */
#define KEY_TOPFIRST            (32586)				/* 32586 */
#define KEY_TOPLAST             (32601)				/* 32601 */
#define	LOC_MOVEWHILEBUTDOWN    (VKEY_FIRSTPSEUDO+4)		/* 32516 */
#define	LOC_MOVE                (VKEY_FIRSTPSEUDO+0)		/* 32512 */
#define	VKEY_FIRSTPSEUDO        (VKEY_FIRST)			/* 32512 */
#define	VKEY_FIRSTFUNC          (VKEY_LASTSHIFT+1)		/* 32544 */
#define	VKEY_LASTSHIFT          (VKEY_FIRSTSHIFT+15)		/* 32543 */
#define	VKEY_FIRSTSHIFT         (VKEY_LASTPSEUDO+1)		/* 32528 */
#define	VKEY_LASTPSEUDO         (VKEY_FIRSTPSEUDO+15)		/* 32527 */
#define	VKEY_FIRST		(32512)				/* 32512 */
#define	SHIFTMASK		0x000E
#define IM_NEGEVENT		(0x01)				/* send input negative events too */
#define	KEY_LEFT(i)		((KEY_LEFTFIRST)+(i)-1)
#define	KEY_RIGHT(i)		((KEY_RIGHTFIRST)+(i)-1)
#define	KEY_TOP(i)		((KEY_TOPFIRST)+(i)-1)
#endif
