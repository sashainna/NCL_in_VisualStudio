/*************************************************************************
**		NAME	:		udactfrm.c
**			Contains:	actfrm is the high level form activate routine.
**
**		COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**       d4actfr.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:05:08
**************************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "gtbl.h"
#include "udfconst.h"
#include "udforms.h"
#include "udfrmdec.h"
#include "udfmracs.h"
extern int UD_ksws;
/*******************************************************************
**
**		I_FUNCTION	:		UD_FSTAT ud_actfrm()
**
**			Synopsis:	actfrm first defines the form to the terminal
**						interface package, then activates and initializes the
**						defined device. If there was no error it returns with
**						status ok else it returns with error status
**
**			Parameters	:
**				Input		:
**				Output	:
**			Returns		:
**					The value of the function is of type UD_fstat:
**						UD_ACTOK - the form is successfully activated
**						UD_BADACT - there was an init or activate error
**			Side Effects:
**				sets the values of the terminal device definitions internal
**				to the device interface package, will initialize (ie-clear)
**				the device if it gets that far.
**			Warnings		:
**				may leave the interface device in a peculiar state if there's
**				a problem
********************************************************************/

UD_FSTAT ud_actfrm()							/* interface activate routine */
{
	/*
	**	local variable decs
	*/
	UU_REAL llx, lly, urx, ury;					/* lower left, upper right coords */
	int pgc, txc, txf;							/* colors and font */
	UU_REAL txs;										/* text char height */

	/*
	**	local function decs
	*/

	/*
	**		code
	*/
	uu_denter(UU_GTRC,(us,"entering ud_actfrm:"));

	gupdatews(UD_ksws,UG_SUPPRESS);
	/*
	**	define the interface device to the interface package
	*/
	llx=ud_getfwllx();							/* pull window specs from fstruct */
	lly=ud_getfwlly();							/* corner coords: lower left, */
	urx=ud_getfwurx();							/* upper right (they are NDC) */
	ury=ud_getfwury();

	pgc=ud_getfpgc();								/* similarly, colors and font */
	txc=ud_getftxc();
	txf=ud_getftxf();
	txs=ud_getftxs();								/* and size */

	ud_tsetup(llx,lly,urx,ury,pgc,txc,txf,txs);/* define the terminal device */

	if (ud_trmon() == 0)								/* initialize ok */
	{
		uu_dexit;
		return (UD_ACTOK);
	}
	else
	{
		ud_treset();									/* else attempt reset */
		uu_dexit;
		return (UD_BADACT);
	}
}
