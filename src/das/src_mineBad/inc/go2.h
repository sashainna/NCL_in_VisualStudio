/********************************************************************* 
**  NAME:   gkso2.h
**
**		Output Type definitions for GKS. 
**
**  COPYRIGHT  1984  UNICAD, Inc.
**  MODULE NAME AND RELEASE LEVEL 
**       go2.h , 25.1
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

#ifndef GKSO2H
/* Gflinter  -  FiLl area INTERior style */

	typedef enum	{
		UG_HOLLOW,
		UG_SOLID,
		UG_PATTERN,
		UG_HATCH
	} Gflinter;

/* Girgmode  -  Implicit ReGeneration MODE */

	typedef enum	{
		UG_ALLOWED,
		UG_SUPPRESSED
	} Girgmode;

/* Gmodmode -- Modification mode */

	typedef enum {
		UG_NIVE,				/* no immediate visual effects required */
		UG_UWOR,				/* update without regeneration */
		UG_UQUM,				/* use quick update methods */
		UG_PRIN				/* perform regeneration if necessary */
	} Gmodmode;

/* Gmodtype  -  dynameic MODification TYPE */

	typedef enum	{
		UG_IRG,
		UG_IMM
	} Gmodtype;


/* Gnframe  -  New FRAME action at update */

	typedef enum	{
		UG_YES,
		UG_NO
	} Gnframe;


/* Gos  -  Gks Operating State */

	typedef enum	{
		UG_GKCL,						/* GKS closed */
		UG_GKOP,						/* GKS open */
		UG_WSOP,						/* workstation open */
		UG_WSAC,						/* workstation active */
		UG_SGOP						/* segment open */
	} Gos;


/* Gpfcf  -  Polyline / Fill-area Control Flag */

	typedef enum	{
		UG_PF_POLYLINE,
		UG_PF_FILLAREA
	} Gpfcf;

/* Gregen  -  REGENeration flag */

	typedef enum	{
		UG_PERFORM,
		UG_SUPPRESS
	} Gregen;
#define GKSO2H
#endif
