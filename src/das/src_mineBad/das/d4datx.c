/*********************************************************************
**
**    NAME         :  d4datx.c
**
**       CONTAINS:
**				ud_todas(data)
**				ud_todata(das)
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**       d4datx.c , 25.1 
**    DATE AND TIME OF LAST  MODIFICATION 
**       04/29/15 , 15:05:08
**
*********************************************************************/

#include "usysdef.h"
#include "udebug.h"
#include "udfconst.h"
#include "udforms.h"
#include "udfdata.h"
#include "udfrmdec.h"
#include "udfmracs.h"

/*********************************************************************
**    I_FUNCTION :  UD_FSTAT ud_todas(data,das,dtyp)
**			transfer data at *data (which is a union of pointers)
**			to *das. dtyp tells what kind of data it is. returns XFROK
**			or BADXFR if dtype was in error.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

UD_FSTAT ud_todas(data,das,dtyp)
UD_DDATA *data;							/* ptr to a UD_DDATA (ptr union) */
UD_DASIN *das;								/* ptr to a UD_DASIN (data union) */
int dtyp;									/* type of data in *data */
{
	UD_FSTAT stat;							/* status return */
	int ix;									/* cntr/index */

	uu_denter(UU_DTRC,(us,
		"entering ud_todas: ptr union ptr=%x, das ptr=%x, typ=%d",
		data, das, dtyp));

/* -- according to data type -- */

	switch(dtyp)
	{

/* 	-- transfer coordinate vector -- */

		case UD_DASCART:
		case UD_DASNDC:
		case UD_DASVEC:
			for(ix=0; ix<3; ix++)
				(das->cord.cord)[ix] = (data->frmvec)[ix];
			stat = UD_XFROK;
			break;

/* 	-- transfer floating point number -- */

		case UD_DASVAL:						/* or float */
		case UD_DASDISTANCE:
		case UD_DASUNITLESS:
		case UD_DASANGLE:
			das->dreal = *(data->frmflt);
			stat = UD_XFROK;
			break;

/* 	-- integer -- */

		case UD_DASCHOICE:
		case UD_DASINT:
			das->dint = *(data->frmint);
			stat = UD_XFROK;
			break;

/* 	-- or string -- */

		case UD_DASSTRING:
			uu_dprint(UU_DTRC,(us,"ud_todas. data.frmstr=%s",data->frmstr));
			strcpy(das->dstr, data->frmstr);
			stat=UD_XFROK;
			break;

/* 	-- anything else is an error -- */

		default:
			stat=UD_BADXFR;
			break;
	}
	uu_dexit;
	return(stat);							/* and exit with type/status */
}

/*********************************************************************
**    I_FUNCTION :  UD_FSTAT ud_todata(das,data,dtyp)
**			transfer data at *das to *data(a union of ptrs).
**			dtyp tells what kind of data it is. returns XFROK
**			or BADXFR if dtype was in error.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

UD_FSTAT ud_todata(das, data, dtyp)
UD_DASIN *das;								/* ptr to a UD_DASIN (data union) */
UD_DDATA *data;							/* ptr to a UD_DDATA (ptr union) */
int dtyp;									/* type of data in *data */
{
	UD_FSTAT stat;							/* status return */
	int ix;									/* cntr/index */

	uu_denter(UU_DTRC,(us,
		"entering ud_todata: das ptr=%x, ptr union ptr=%x, typ=%d",
		das, data, dtyp));

/* -- according to data type -- */

	switch(dtyp)
	{

/* 	-- transfer coordinate vector -- */

		case UD_DASCART:
		case UD_DASNDC:
		case UD_DASVEC:
			for (ix=0; ix<3; ix++)
				(data->frmvec)[ix]=(das->cord.cord)[ix];
			stat=UD_XFROK;
			break;

/* 	-- transfer floating point number -- */

		case UD_DASVAL:
		case UD_DASDISTANCE:
		case UD_DASUNITLESS:
		case UD_DASANGLE:
			*(data->frmflt) = das->dreal;
			stat=UD_XFROK;
			break;

/* 	-- transfer integer -- */

		case UD_DASCHOICE:
		case UD_DASINT:
			*(data->frmint) = das->dint;
			stat = UD_XFROK;
			break;

/* 	-- transfer string -- */

		case UD_DASSTRING:
			uu_dprint(UU_DTRC,(us,"ud_todata. case DASSTRING dstr=%s",das->dstr));
			strcpy(data->frmstr, das->dstr);
			stat = UD_XFROK;
			break;

/* 	-- anything else is err --  */

		default:
			stat = UD_BADXFR;
			break;
	}
	uu_dexit;
	return(stat);							/* and exit with type/status */
}
