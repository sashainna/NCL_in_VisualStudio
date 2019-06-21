/********************************************************************* 
**  NAME:  gksi1.h
**
**      GKS  file for input stuff
**
**  COPYRIGHT  1984  UNICAD, Inc.
**  MODULE NAME AND RELEASE LEVEL 
**       gi1.h , 25.1
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

#ifndef GKSI1H
/* Geclass  -  Event CLASS */

	typedef enum	{
		UG_E_NONE,
		UG_E_LOCATOR,
		UG_E_STROKE,
		UG_E_VALUATOR,
		UG_E_CHOICE,
		UG_E_PICK,
		UG_E_STRING
	} Geclass;


/* Gesw  -  Echo SWitch */

	typedef enum	{
		UG_ECHO,
		UG_NOECHO
	} Gesw;


/* Giclass  -  Input CLASS */

	typedef enum	{
		UG_IC_LOCATOR,
		UG_IC_STROKE,
		UG_IC_VALUATOR,
		UG_IC_CHOICE,
		UG_IC_PICK,
		UG_IC_STRING,
		UG_IC_NONE
	} Giclass;

/* Gimode  -  Input MODE */

	typedef enum	{
		UG_REQUEST,
		UG_SAMPLE,
		UG_EVENT
	} Gimode;

/* Gpstat  -  Pick STATus */

	typedef enum	{
		UG_OKPICK,
		UG_NOPICK
	} Gpstat;

/* Gqtype  -  inQuiry TYPE */

	typedef enum	{
		UG_SET,
		UG_REALIZED
	} Gqtype;


/* Gsimultev  -  SIMULTaneous EVents */

	typedef enum	{
		UG_MORE,
		UG_NOMORE
	} Gsimultev;


/* Gstatus  -  request STATUS */

	typedef enum	{
		UG_OK,
		UG_NONE
	} Gstatus;

/* Gvpri  -  Viewport input PRIority */

	typedef enum	{
		UG_HIGHER,
		UG_LOWER
	} Gvpri;
#define GKSI1H
#endif
