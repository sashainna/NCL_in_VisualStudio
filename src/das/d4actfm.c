/*************************************************************************
**		NAME	:		uddactfm.c
**			Contains:		dactfm deactivates an active form
**				ud_form_invis()
**				ud_form_vis()
**
**		COPYRIGHT 2001 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       d4actfm.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION 
**       04/29/15 , 15:05:08
**************************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "udfconst.h"
#include "udforms.h"
#include "udfrmdec.h"
#include "udfmracs.h"
#include "gtbl.h"
#include "gdidd.h"
/*******************************************************************
**
**		I_FUNCTION	:		int ud_dactfm()
**
**			Synopsis:		dactfm takes down the form display, returns.
**
**			Parameters	:
**				Input		:
**				Output	:
**			Returns		:
**			Side Effects:
**					The form disappears from the display device. Some static
**					variables in the device interface pkg may change.
**			Warnings		:
********************************************************************/

void ud_dactfm()									/* close down form */
{
	/*
	**	local variable decs
	*/

	/*
	**	local function decs
	*/

	/*
	**		code
	*/
	uu_denter(UU_DTRC,(us,"entering uddactfm:"));

	/*
	**	erase screen
	*/
	/* ud_eraall(); */

	/*
	**	terminate connection
	*/
	ud_trmoff();

	/*
	**	exit
	*/
	uu_dexit;
	return;
}

/*******************************************************************
**
**		I_FUNCTION	:		void ud_form_invis()
**
**			Synopsis:		Invisibles the active form.  Please note that
**								the form is still considered active.
**
**			Parameters	:
**				Input		:
**				Output	:
**			Returns		:
**			Side Effects:
**					none
**			Warnings		:
********************************************************************/
void ud_form_invis()
{
	(*(ug_gksstli.wsopen[0].connid)[UW_FORM_INVIS])();
}

/*******************************************************************
**
**		I_FUNCTION	:		void ud_form_vis()
**
**			Synopsis:		Redisplays the active form.
**
**			Parameters	:
**				Input		:
**				Output	:
**			Returns		:
**			Side Effects:
**					none
**			Warnings		:
********************************************************************/
void ud_form_vis()
{
	(*(ug_gksstli.wsopen[0].connid)[UW_FORM_VIS])();
}


/*******************************************************************
**
**		I_FUNCTION	:		void ud_dspfrm_invis(frmid)
**
**			Synopsis:		Invisibles the display form. 
**
**			Parameters	:
**				Input	frmid: form ID
**				Output	:
**			Returns		:
**			Side Effects:
**					none
**			Warnings		:
********************************************************************/
void ud_dspfrm_invis(frmid)
int frmid;
{
	(*(ug_gksstli.wsopen[0].connid)[UW_DSPFRM_INVIS])(frmid);
}

/*******************************************************************
**
**		I_FUNCTION	:		void ud_dspfrm_vis(frmid)
**
**			Synopsis:		Redisplays the display form. 
**
**			Parameters	:
**				Input		frmid: form ID
**				Output	:
**			Returns		:
**			Side Effects:
**					none
**			Warnings		:
********************************************************************/
void ud_dspfrm_vis(frmid)
int frmid;
{
	(*(ug_gksstli.wsopen[0].connid)[UW_DSPFRM_VIS])(frmid);
}
