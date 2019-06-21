/*********************************************************************
**
**    NAME         :  d4fildfl.c
**
**       CONTAINS:
**				UD_FSTAT ud_fildfl(fno, dno, de)
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**       d4fildf.c , 25.1 
**    DATE AND TIME OF LAST  MODIFICATION 
**       04/29/15 , 15:05:08
**
*********************************************************************/

#include "usysdef.h"
#include "udebug.h"
#include "zsysdep.h"
#include "udfconst.h"
#include "udforms.h"
#include "udfdata.h"
#include "udfrmdec.h"
#include "udfmracs.h"

#define BFRLN	128							/* length os field bfr */

static char blnkbfr[128]=					/* field blank buffer */
"                                                                \
                                                               ";
extern int ud_curfldno;						/* curr field no (see udfiopak) */

/*******************************************************************
**
**		I_FUNCTION	:		UD_FSTAT ud_fildfl(fno,de)
**			filfdl first checks the form-data element; ifthere's
**			no default data there, it checks the form-struct field
**			definition and ifthere's a default there, it copies it
**			into the form-data element. If there was no default in
**			either place, it returns with status ok. Finally, it
**			displays the default in the data-element, using the
**			precision and length specs in the form struct field
**			definition, ifthey are not each -1;
**
**			PARAMETERS	:
**				INPUT		:
**					fno	:	field number of field definition in form-struct
**					de		:	pointer to data-element structure in form-data
**				OUTPUT	:
**			RETURNS		:
**				The value of the function is of type UD_FSTAT:
**					UD_DFLTOK - ifall went well, any default properly displayed
**					UD_DFLTER - there was a default display error
**			SIDE EFFECTS:
**				May change the value contained in the data element pointed to
**			WARNINGS		:
********************************************************************/

UD_FSTAT ud_fildfl(fno, dno, de)			/* fill/display default value */
int fno;											/* field number */
int dno;											/* default number */
UD_DPIECE *de;									/* pointer to data element */
{
	int ix;										/* loop index */
	int r,c;										/* row/col field/default loc */
	int dtyp;									/* data type: in UD_DPIECE */
	int ftyp;									/*   "    " : in form struct */
	UD_DASIN *ptr;								/* ptr to das data type (default stor)*/
	UD_DASIN tmpdata;
	UD_DASIN ldfdata;							/* local dasin storage */
	int prec;									/* field length/precision */
	int len;										/* min field length */

	uu_denter(UU_DTRC,(us,"entering ud_fildfl: field no=%d, data element ptr=%x",
							fno,de));
	uu_dprint(UU_DTRC,(us,"udfildfl: default number=%d",dno));

	ftyp = ud_getffdt(fno);
	uu_dprint(UU_DTRC,(us,"ud_fildfl: fstruct data typ=%d, fdata type=%d",
								ftyp, de->dtyp));

	if(de->dtyp != ftyp)
	{
		ud_pstat(UD_DFLTER, "ud_fildfl");
		uu_dexit;
		return(UD_DFLTER);
	}

/*	-- if no default in form-data, fill from form-struct, if there -- */

	if(ud_getfft(fno) == 2)
	{
		tmpdata.dstr = ud_getffd(fno,dno)->dstr;
		ptr = &tmpdata;
	}
	else
		ptr=ud_getffd(fno,dno);					/* get ptr to fstruct dflt (ifany)*/

	uu_dprint(UU_DTRC,(us,"ud_fildfl: fstruct ptr=%x",ptr));
	uu_dprint(UU_DTRC,(us,"ud_fildfl: fdata default flag=%d",de->dflg));

/* -- if no fdata default, or choice field -- */

	if((de->dflg !=1) || (ud_getfft(fno)==2))
	{
		uu_dprint(UU_DTRC,(us,"ud_fildfl: no fdata default"));

/* 	-- and there is fstruct default, -- */

		if(ptr != (UD_DASIN *) UU_NULL)
		{
			uu_dprint(UU_DTRC,(us,"ud_fildfl: fstruct default exists"));
			dtyp = ftyp;								/* set data type */
			zbytecp(ldfdata,*ptr);              /* copy in data */
			if(ud_getfft(fno) == 2)
			{
         	*(de->ud_delem.frmint) = ud_dfindstr(fno, ldfdata.dstr);
			}
			else
				ud_todata(&ldfdata, &(de->ud_delem), de->dtyp);
			de->dflg = 1;
			de->dtyp = ftyp;
		}
		else

/* 	-- else, no default at all, -- */

		{
			uu_dprint(UU_DTRC,(us,"ud_fildfl: no fstruct default, either"));
			dtyp = UD_DASSTRING;
			ldfdata.dstr = blnkbfr;					/* after blanking the field */
			for(ix=0; ix<ud_getfprec(fno); ix++)
			{
				blnkbfr[ix]=' ';
			}
			blnkbfr[ix]='\0';
		}
	}
	else						/* else there was default in fdata, */
	{
		uu_dprint(UU_DTRC,(us,"ud_fildfl: fdata default exists"));
		if((dtyp=de->dtyp)==UD_DASSTRING) 
			ldfdata.dstr=blnkbfr;					/* set up for copy */
		if(ud_getfft(fno) == 2)
				ldfdata.dstr=ud_getffd(fno,dno)->dstr;
		else
			ud_todas(&(de->ud_delem),&ldfdata,dtyp);/* trans to dasin for outdas */
	}

/*	-- now go display the default at the appropriate location -- */

	r = ud_getflcr(fno);							/* get field entry area loc */
	c = ud_getflcc(fno);
	prec = ud_getfprec(fno);					/* and precision/length */
	len = ud_getflen(fno);
	ud_curfldno = fno;							/* set curr fld no to curr fld */

	outdas(r, c, dtyp, prec, len, &ldfdata);		/* dsp data,  rtn rslt */
	if(dtyp == UD_DASSTRING) 
	{
		uu_dprint(UU_DTRC,(us,"ud_fildfl: ldfdata.dstr=%s",ldfdata.dstr));
		if(ud_getfft(fno) == 2) 
		{
			*(de->ud_delem.frmint) = ud_dfindstr(fno, ldfdata.dstr);
		}
	}
	uu_dexit;
	return (UD_DFLTOK);
}
