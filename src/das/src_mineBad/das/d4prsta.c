/*********************************************************************
**
**    NAME         :  d4prsta.c
**
**       CONTAINS:
**				UD_FSTAT ud_prsnta()
**
**    COPYRIGHT 2000 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       d4prsta.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:10
**
*********************************************************************/

#include "usysdef.h"
#include "udebug.h"
#include "udfconst.h"
#include "udforms.h"
#include "udfrmdec.h"
#include "udfmracs.h"

extern int ud_curfldno;

/*******************************************************************
**
**		I_FUNCTION	:		UD_FSTAT ud_prsnta()
**
**			prsnta will display all display all message strings
**			defined in the current form-struct, then display all
**			prompts defined in the current form-struct (masks also
**			when implemented). it returns an error status from the
**			routine that fails if there is an error, else an ok status.
**
**			PARAMETERS	:
**				INPUT		:
**				OUTPUT	:
**			RETURNS		:
**					The value of the function is of UD_fstat:
**						UD_PRSTOK - all went well, the form is now displayed.
**						UD_BADMSG - there was a message diplay error
**						UD_BADPRMT - there was a prompt display error
**						UD_BASMSK - there was a mask display error
**			SIDE EFFECTS:
**			WARNINGS		:
********************************************************************/

UD_FSTAT ud_prsnta()								/* present all module */
{
	register int r,c;									/* row, col variables */
	register int ix;									/* index */
	int num;												/* count or number */
	UD_FSTAT stat;										/* return status */
	UD_DASIN temp;										/* temp dasin data */
	int len;
	UD_FSTAT ud_prsntf();

	uu_denter(UU_DTRC,(us,"entering udprsnta:"));

	/* -- **	first display all display message strings -- */

	num=ud_getfmno();								/* get the number of messages */
	uu_dprint(UU_DTRC,(us,"ud_prsnta: num=%d",num));

	for(ix=0; ix<num; ix++)						/* = -1 if none, so this is ok */
	{
		if(UD_frm->ud_display_mask[ix] == 1)
		{
			uu_dprint(UU_DTRC,(us,"ud_prsnta: ix=%d",ix));
			r=ud_getfmr(ix);									/* getting location, */
			c=ud_getfmc(ix);
			temp.dstr=ud_getfmsg(ix);						/* get char ptr */
			ud_curfldno= -1;						/* outstr won't update default value*/
			len = strlen(temp.dstr);
			outstr(r,c,len,len,&temp);		/* display each msg */
		}
	}

	/* -- next display all prompt strings -- */

	num=ud_getffno();									/* get number of fields in form */
	uu_dprint(UU_DTRC,(us,"ud_prsnta: no of fields=%d",num));

	for (ix=0;ix<num;ix++)
	{
		if(UD_frm->ud_display_mask[ix+UD_frm->n_display_fields] == 1)
		{
			if (ud_getffmf(ix) == 1)					/* if it's mandatory, */
			{
				uu_dprint(UU_DTRC,(us,"ud_prsnta: hilite on for fno %d",ix));
				ud_attbld();									/* hilite it */
			}
			stat=ud_prsntf(ix);							/* present the field */
			if(stat != UD_PRSTOK)
			{
				break; 										/* bug out if err */
			}

			if(ud_getffmf(ix) == 1)						/* if it was mandatory, */
			{
				uu_dprint(UU_DTRC,(us,"ud_prsnta: hilite off, fno %d",ix));
				ud_atoff();										/* turn off hilite */
			}
		}
	}

	ud_pstat(stat, "ud_prsnta");
	uu_dexit;
	return (UD_PRSTOK);
}
