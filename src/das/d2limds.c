/*********************************************************************
**
**    NAME         :  d2limds.c
**
**       CONTAINS:
**				ud_lgeo 
**				ud_inqgeo 
**				ud_lpra 
**				ud_lsin
**				ud_lnin
**				ud_leditable
**				ud_lpsh
**				ud_lpop
**				ud_unlimit
**				ud_prlimit
**				ud_prntlim
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**       d2limds.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:05:05
**
*********************************************************************/

#include "usysdef.h"
#include "umath.h"
#include "dmark.h"
#include "dasnog.h"
#include "uhep.h"
#include "udebug.h"

extern UU_LOGICAL NCL_pick_feature;
#define OL UD_limbf[UD_limbfptr-1]

/********************************************************************* 
**  E_FUNCTION:  ud_lgeo 
**      limit the geometry in the DAS
**
**  PARAMETERS   
**      input:  flag = if true then limit DAS, else unrestrict DAS 
**					 bitary = if call to limit then the geometry bit array
**
**      output: none
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
*********************************************************************/

ud_lgeo(flag, bitary)
UU_LOGICAL flag;					/* if true then limit DAS, else unrestrict DAS */
int bitary[];						/* bit array to limit geometry */
{
	int i;
	char us[100];

	if(flag == UU_TRUE)
	{
		UD_LIMIT.lsel = UU_TRUE;
		uu_dprint(UU_DTRC,(us,"lgeo UD_NMENTWD = %d, bits=%x",
			UD_NMENTWD, bitary[0]));

		for(i=0; i<UD_NMENTWD; i++)
			UD_LIMIT.lselbf[i] = bitary[i];
/*
.........Reset user defined local limiting in case of hangover
.........llsel == UU_TRUE means that there are user defined limits.
.........llselbf holds the original limit bit array while lselbf 
.........	holds the modified bit array.
.........llsel_name holds character string name of geometry limited to.
*/
		UD_LIMIT.llsel = UU_FALSE;
		for(i=0; i<UD_NMENTWD; i++)
			UD_LIMIT.llselbf[i] = 0;
		strcpy(UD_LIMIT.llsel_name, "");

	}
	else
	{
/*
.....Features are separate from
.....the geometry limit mask
*/
		NCL_pick_feature = UU_FALSE;
		UD_LIMIT.lsel = UU_FALSE;
	}
	return;
}

/********************************************************************* 
**  E_FUNCTION:  ud_inqgeo 
**      inquire limit the geometry in the DAS
**
**  PARAMETERS   
**      input:  none
**
**      output: 
**      			 flag = if UU_TRUE then DAS is limited
**					 bitary = if flag == UU_TRUE limit then the geometry bit array
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
*********************************************************************/

ud_inqgeo(flag, bitary)
UU_LOGICAL *flag;					/* if true then DAS is limited */
int bitary[];						/* bit array to limit geometry */
{
	int i;

	*flag = UD_LIMIT.lsel;
	if(*flag == UU_TRUE)
	{
		for(i=0; i<UD_NMENTWD; i++)
			bitary[i] = UD_LIMIT.lselbf[i];
	}
}

/********************************************************************* 
**  E_FUNCTION:  ud_lpra 
**      limit parameter range
**
**  PARAMETERS   
**      input:  flag = if true then limit DAS, else unrestrict DAS 
**					 low  = low parameter range
**					 hi   = high parameter range
**
**      output: none
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
*********************************************************************/

ud_lpra(flag, low, hi)
UU_LOGICAL flag;					/* if true then limit DAS, else unrestrict DAS */
UU_REAL low;						/* low parameter range */
UU_REAL hi;						/* high parameter range */
{
char us[100];

	if(flag == UU_TRUE)
	{
		UD_LIMIT.lpran = UU_TRUE;
		UD_LIMIT.lpranlo = low;
		UD_LIMIT.lpranhi = hi;
	uu_denter2(UU_DTRC,(us,"set val lim = %g, =%g",
			UD_LIMIT.lpranlo,UD_LIMIT.lpranhi));
	uu_dexit;
	}
	else
		UD_LIMIT.lpran = UU_FALSE;
	return;
}

/********************************************************************* 
**  E_FUNCTION:  ud_lsin
**      limit DAS by specific interaction
**
**  PARAMETERS   
**      input:  flag = if true then limit DAS, else unrestrict DAS 
**					 sint = specific interaction number
**					 device = device number
**					 pet = prompt and echo type
**
**      output: none
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
*********************************************************************/

ud_lsin(flag, sint, device, pet)
UU_LOGICAL flag;					/* if true then limit DAS, else unrestrict DAS */
int sint;							/* specific interaction number */
int device;							/* device number */
int pet;								/* prompt and echo type */
{
	if(flag == UU_TRUE)
	{
		UD_LIMIT.lsint = UU_TRUE;
		UD_LIMIT.fsint = sint;
		UD_LIMIT.fsdev = device;
		UD_LIMIT.fsech = pet;
	}
	else
		UD_LIMIT.lsint = UU_FALSE;
	return;
}

/********************************************************************* 
**  E_FUNCTION:  ud_lnin
**      limit DAS to specific number of interactions
**
**  PARAMETERS   
**      input:  flag = if true then limit DAS, else unrestrict DAS 
**					 lonumin = low number of interactions
**					 hinumin = high number of interactions
**
**      output: none
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
*********************************************************************/

ud_lnin(flag, lonumin, hinumin)
UU_LOGICAL flag;					/* if true then limit DAS, else unrestrict DAS */
int lonumin;						/* low number of interactions */
int hinumin;						/* high number of interactions */
{
	uu_denter(UU_DTRC,(us,"in ud_lnin, flag = %d", flag));

	if(flag == UU_TRUE)
	{
		UD_LIMIT.lnumin = UU_TRUE;
		UD_LIMIT.fntlo = lonumin;
		UD_LIMIT.fnthi = hinumin;
	}
	else
		UD_LIMIT.lnumin = UU_FALSE;

	uu_dexit;
	return;
}

/********************************************************************* 
**  E_FUNCTION:  ud_leditable
**      limit DAS to editable entities only
**
**  PARAMETERS   
**      input:  flag = if true then limit DAS, else unrestrict DAS 
**
**      output: none
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
*********************************************************************/

ud_leditable(flag)
UU_LOGICAL flag;					/* if true then limit DAS, else unrestrict DAS */
{
	uu_denter(UU_DTRC,(us,"in ud_leditable, flag = %d", flag));

	if(flag == UU_TRUE)
	{
		UD_LIMIT.editable = UU_TRUE;
	}
	else
		UD_LIMIT.editable = UU_FALSE;

	uu_dexit;
	return;
}

/**************************************************************************
**
**  E_FUNCTION         :  ud_lpsh
**      push limit state onto a stack
**
**  PARAMETERS   
**      input  : 
**          propflag = propogate flag
**
**      output :  
**          none
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
**************************************************************************/

ud_lpsh(propflag)
UU_LOGICAL propflag;				/* if TRUE, then propogate limit state */
{
	uu_denter(UU_DTRC,(us,"in ud_lpsh, ptr = %d, propflag = %d", 
				UD_limbfptr, propflag));

/*	-- check for uninitialized DAS stack -- */

	if(UD_dastkfl == UU_FALSE)
	{
		UD_dastkfl = UU_TRUE;
		UD_limbfptr = 0;		/* note - entry zero is reserved for the system */
		ud_unlimit();
	}

/* -- check for stack overflow -- */

	UD_limbfptr++;
	if(UD_limbfptr < UD_MAXENVDEPTH)
	{

/*		-- increment pointer and init new record --  */

		if(propflag == UU_FALSE)
			ud_unlimit();
		else
			ud_prlimit();
	}
	else

/*	-- stack overflow -- */

	{

/*		-- "DAS invoked too many times, returning to root" -- */

		uu_uerror0(UD_DASHEP, 68);
		UD_dastkfl = UU_FALSE;
		uu_dexit;
		uu_dprint(UU_DTRC, (us, "ud_lpsh before jump"));
		ud_jump(UD_markptr, UU_TRUE);
		uu_dprint(UU_DTRC, (us, "ud_lpsh after jump"));
	}
	uu_dexit;
}

/**************************************************************************
**
**  E_FUNCTION         :  ud_lpop
**      pop limit state from the stack
**
**  PARAMETERS   
**      input  : 
**          none
**
**      output :  
**          none
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
**************************************************************************/

ud_lpop()
{
	uu_denter(UU_DTRC,(us,"in ud_lpop"));

/*	-- if not underflow, pop stack -- */

	if(UD_limbfptr > 0)
		UD_limbfptr--;
	ud_prntlim("ud_lpop");
	uu_dexit;
}

/*********************************************************************
**
**    E_FUNCTION         :  ud_unlimit
**       reset the DAS to the unlimited state
**
**    PARAMETERS   
**       input  : 
**          none
**
**       output :  
**          none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/

ud_unlimit()
{
	uu_denter(UU_DTRC,(us,"in ud_unlimit, ptr = %d", UD_limbfptr));

/* -- set all flags to unlimited state -- */

	UD_LIMIT.lsel = UU_FALSE;			/* limit geometry flag */
	UD_LIMIT.lpran = UU_FALSE;			/* limit parameter range */
	UD_LIMIT.lsint = UU_FALSE;			/* limit specific interaction */
	UD_LIMIT.lnumin = UU_FALSE;		/* limit number of inputs */
	UD_LIMIT.editable = UU_FALSE;
/*
.....Initialize new local limits (for USER DEFINE LIMITS)
.....See LIMIT GEOMETRY TYPES popup menu and nclc/nepick.c - RAZ
*/
	UD_LIMIT.llsel = UU_FALSE;			/* local limit geometry flag */
	strcpy(UD_LIMIT.llsel_name,"");
/*
.....Features are separate from
.....the geometry limit mask
*/
	NCL_pick_feature = UU_FALSE;

	uu_dexit;
}

/*********************************************************************
**
**    E_FUNCTION         :  ud_prlimit
**       propgate the DAS limit state from the previous environment
**
**    PARAMETERS   
**       input  : 
**          none
**
**       output :  
**          none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/

ud_prlimit()
{
	uu_denter(UU_DTRC,(us,"in ud_prlimit, ptr = %d", UD_limbfptr));

/*	-- propogate all flags to form current state -- */

	ud_lgeo(OL.lsel, OL.lselbf);		 				/* limit geometry */
	ud_lpra(OL.lpran, OL.lpranlo, OL.lpranhi); 	/* limit parameter range */
	ud_lnin(OL.lnumin, OL.fntlo, OL.fnthi); 		/* limit number of inputs */
	ud_lsin(OL.lsint, OL.fsint, OL.fsdev, OL.fsech); /* specific interaction */
	ud_leditable(OL.editable);							/* editablility */
/*
.....Initialize new local limits (for USER DEFINE LIMITS)
.....See LIMIT GEOMETRY TYPES popup menu and nclc/nepick.c - RAZ
.....These flags do not get propogated.
*/
	OL.llsel = UU_FALSE;
	strcpy(OL.llsel_name,"");

	ud_prntlim("prlimit");
	uu_dexit;
}

/*********************************************************************
**
**    E_FUNCTION         :  ud_prntlim
**       print the DAS limit state
**
**    PARAMETERS   
**       input  : 
**          from = procedure name we came from
**
**       output :  
**          none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/

ud_prntlim(from)
char *from;
{
	uu_denter(UU_DTRC,(us,"from %s, ptr = %d", from, UD_limbfptr));
	uu_dprint(UU_DTRC,(us,
		"limits geom=%d, parm=%d, sint=%d, inputs=%d, edit=%d", 
			UD_LIMIT.lsel, UD_LIMIT.lpran, UD_LIMIT.lsint,
				UD_LIMIT.lnumin, UD_LIMIT.editable));

/*	-- write out limit stuff -- */

	if(UD_LIMIT.lsel == UU_TRUE)
	{
		uu_dprint(UU_DTRC,(us,"selmsk=%x %x %x ", UD_LIMIT.lselbf[0], 
				UD_LIMIT.lselbf[1], UD_LIMIT.lselbf[2]));
	}
	uu_dexit;
}
