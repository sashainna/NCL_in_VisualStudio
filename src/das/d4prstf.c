/*********************************************************************
**
**    NAME         :  d4prstf.c
**
**       CONTAINS:
**				UD_FSTAT ud_prsntf(fno)
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL
**       d4prstf.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:10
**
*********************************************************************/

#include "ustdio.h"
#include "usysdef.h"
#include "udebug.h"
#include "udfconst.h"
#include "udforms.h"
#include "udfrmdec.h"
#include "udfmracs.h"

extern int ud_curfldno;					/* current field number */

/*******************************************************************
**
**		I_FUNCTION	:		UD_FSTAT ud_prsntf(fno)
**
**			prsntf prints the prompt (if there is one) indexed
**			by the given field number and returns a status from
**			the attempt. Later it will also display the data mask
**			indexed by a field (if there).
**
**			PARAMETERS	:
**				INPUT		:
**					fno	: the number of the field whose prompt/mask is to
**								be displayed
**				OUTPUT	:
**			RETURNS		:
**				The value of the function is of type UD_fstat:
**					UD_BADPRMT - there was an error in displaying the prompt
**					UD_BADMASK - there was an error in displaying the mask
**					UD_PRSTOK - no error, all ok
**			SIDE EFFECTS:
**			WARNINGS		:
********************************************************************/

UD_FSTAT ud_prsntf(fno)					/* display field prompt/mask */
int fno;										/* field number */
{
	register int r,c,len;				/* row/col and len variables */
	UD_DASIN temp;                   /* data storage */

	uu_denter(UU_DTRC,(us,"entering ud_prsntf: field no=%d",fno));

/* -- display the prompt -- */

	if ((temp.dstr=ud_getffp(fno)) != NULL)		/* if there is a prompt */
	{
		uu_dprint(UU_DTRC,(us,"ud_prsntf: there is a prompt at %x",temp.dstr));
		r=ud_getflpcr(fno);								/* get the field location */
		c=ud_getflpcc(fno);
		ud_curfldno= -1;						/* outstr won't update default value */
		len = strlen(temp.dstr);
		outstr(r,c,len,len,&temp);		/* print the prompt */
	}

	ud_pstat(UD_PRSTOK, "ud_prsntf");
	uu_dexit;
	return (UD_PRSTOK);
}
