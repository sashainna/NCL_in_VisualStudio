/********************************************************************* 
**  NAME:   gobas.h
**
**		Output Type definitions for GKS. 
**
**  COPYRIGHT  1984  UNICAD, Inc.
**    MODULE NAME AND RELEASE LEVEL 
**       gobas.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
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


#ifndef GKSOBASH
/*
.....Added so that VMS complies of MPE modules using only the include gobas.h
.....will compile without warning. - Roberta Zorzynski
*/
#include "usysdef.h"

/**********		BASIC TYPES		**********/

/* Gchar  -  CHARacter (8 bits minimum) */

	typedef char	Gchar;


/* Gint  -  INTeger (16 bits minimum) */

	typedef int		Gint;


/*  Glong  -  LONG integer (32 bits minimum) */

	typedef long	Glong;


/* Gfloat  -  FLOATing point number (single precision minimum) */

#ifdef UU_SINGLE
	typedef float	Gfloat;
#else

#if UU_COMP==UU_IRIS
	typedef long float Gfloat;
#else
	typedef double Gfloat;
#endif

#endif


/**********		COORDINATE TYPES		**********/

/* Gwc  -  World Coordinates (implementation dependent) */

	typedef Gfloat	Gwc;


/* Gwc3  -  3-D World Coordinates (implementation dependent) */

	typedef Gfloat	Gwc3;


/* Gdc  -  Device Coordinates */

	typedef Gfloat Gdc;


/* Gdc3  -  3-D Device Coordinates */

	typedef Gfloat	Gdc3;


/* Gndc  -  Normalized Device Coordinates */

	typedef Gfloat	Gndc;


/* Gndc3  -  3-D Normalized Device Coordinates */

	typedef Gfloat	Gndc3;


/**********		POINT TYPES		**********/

/* Gdpoint  -  Device coordinate POINT */

	typedef struct	{
		Gdc		x;				/* x coordinate */
		Gdc		y;				/* y coordinate */
	} Gdpoint;


/* Gdpoint3  -  3-D Device coordinate POINT */

	typedef struct	{
		Gdc		x;				/* x coordinate */
		Gdc		y;				/* y coordinate */
		Gdc		z;				/* z coordinate */
	} Gdpoint3;


/* Gipoint  -  Integer POINT */

	typedef struct	{
		Gint		x;				/* x coordinate */
		Gint		y;				/* y coordinate */
	} Gipoint;


/* Gipoint3  -  3-D Integer POINT */

	typedef struct	{
		Gint		x;				/* x coordinate */
		Gint		y;				/* y coordinate */
		Gint		z;				/* z coordinate */
	} Gipoint3;


/* Gnpoint  -  Normalized device coordinate POINT */

	typedef struct	{
		Gndc	x;					/* x coordinate */
		Gndc	y;					/* y coordinate */
	} Gnpoint;


/* Gnpoint3  -  3-D Normalized device coordinate POINT */

	typedef struct	{
		Gndc	x;					/* x coordinate */
		Gndc	y;					/* y coordinate */
		Gndc	z;					/* z coordinate */
	} Gnpoint3;


/* Gwpoint  -  World coordinate POINT */

	typedef struct	{
		Gwc		x;				/* x coordinate */
		Gwc		y;				/* y coordinate */
	} Gwpoint;


/* Gwpoint3  -  3-D World coordinate POINT */

	typedef struct	{
		Gwc		x;				/* x coordinate */
		Gwc		y;				/* y coordinate */
		Gwc		z;				/* z coordinate */
	} Gwpoint3;


/**********		RECTANGLE TYPES	**********/

/* Gdrect  -  Device coordinate RECTangle */

	typedef struct {
		Gdpoint	ll;			/* lower left-hand corner */
		Gdpoint	ur;			/* upper right-hand corner */
	} Gdrect;


/* Gdrect3  -  3-D Device coordinate RECTangle (box) */

	typedef struct {
		Gdpoint3	llf;			/* lower left-hand front corner */
		Gdpoint3	urb;			/* upper right-hand back corner */
	} Gdrect3;


/* Gnrect  -  Normalized device coordinate RECTangle */

	typedef struct	{
		Gnpoint	ll;			/* lower left-hand corner */
		Gnpoint	ur;			/* upper right-hand corner */
	} Gnrect;


/* Gnrect3  -  3-D Normalized device coordinate RECTangle (box) */

	typedef struct {
		Gnpoint3	llf;			/* lower left-hand front corner */
		Gnpoint3	urb;			/* upper right-hand back corner */
	} Gnrect3;


/* Gwrect  -  World coordinate RECTangle */

	typedef struct	{
		Gwpoint	ll;			/* lower left-hand corner */
		Gwpoint	ur;			/* upper right-hand corner */
	} Gwrect;


/* Gwrect3  -  3-D World coordinate RECTangle (box) */

	typedef struct {
		Gwpoint3	llf;			/* lower left-hand front corner */
		Gwpoint3	urb;			/* upper right-hand back corner */
	} Gwrect3;

/* Girect -- 2D integer rectangle */
	typedef struct {
		Gipoint ll;				/* lower left corner */
		Gipoint ur;				/* upper right corner */
	} Girect;

#endif
#define GKSOBASH
