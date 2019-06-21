/********************************************************************* 
**  NAME:   gkso1.h
**
**		Output Type definitions for GKS. 
**
**  COPYRIGHT  1984  UNICAD, Inc.
**  MODULE NAME AND RELEASE LEVEL 
**       go1.h , 25.1
**  DATE AND TIME OF LAST MODIFICATION
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

#ifndef GKSO1H
/**********		ENUMERATED TYPES		**********/

/* Gacf  -  Attribute Control Flag */

	typedef enum	{
		UG_CURRENT,
		UG_SPECIFIED
	} Gacf;

/* Gasf  -  Aspect Source Flag */

	typedef enum	{
		UG_INDIVIDUAL,
		UG_BUNDLED
	} Gasf;


/* Gattrs  -  ATRRibuteS used */

	typedef enum	{
		UG_POLYLINE,
		UG_POLYMARKER,
		UG_TEXT,
		UG_FILLAREA
	} Gattrs;


/* Gbis  -  Bundle Index Setting */

	typedef enum	{
		UG_DEFINED,
		UG_UNDEFINED
	} Gbis;


/* Gclip  -  CLIPping indicator */

	typedef enum	{
		UG_CLIP,
		UG_NOCLIP
	} Gclip;


/* Gclrflag  -  CLeaR control FLAG */

	typedef enum	{
		UG_CONDITIONALLY,
		UG_ALWAYS
	} Gclrflag;


/* Gcoavail  -  COlor AVAILability */

	typedef enum	{
		UG_COLOUR,
		UG_MONOCHROME
	} Gcoavail;


/* Gcovalid  -  COlor values VALID */

	typedef enum	{
		UG_VALID,
		UG_INVALID
	} Gcovalid;


/* Gcsw  -  Coordinate SWitch */

	typedef enum	{
		UG_WC,						/* world coordinates */
		UG_NDC						/* normalized device coordinates */
	} Gcsw;


/* Gdefmode  -  DEFerral MODE */

	typedef enum	{
		UG_ASAP,						/* as soon as possible */
		UG_BNIG,						/* before next interaction globally */
		UG_BNIL,						/* before next interaction locally */
		UG_ASTI,						/* at some time */
		UG_WAIT						/* wait until application requests it */
	} Gdefmode;


/* Gdevunits  -  DEVice coordinate UNITS */

	typedef enum	{
		UG_DC_METRES,				/* (meters) */
		UG_DC_OTHER
	} Gdevunits;


/* Gdspsurf  -  DiSPlay SURFace */

	typedef enum	{
		UG_EMPTY,
		UG_NOTEMPTY
	} Gdspsurf;
/*
.....Graphics buffer type
*/
	typedef enum	{
		UG_BACK_BUFFER,
		UG_FRONT_BUFFER,
		UG_BOTH_BUFFER
	} Gbuffer;
#define GKSO1H
#endif
