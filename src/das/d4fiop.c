/*********************************************************************
**
**    NAME         :  d4fiop.c
**
**       CONTAINS:
**				UD_FSTAT ud_inpdas(data,len,typ,r,c)
**				int inpcrd(data,len,r,c)
**				int inpval(data,len,r,c)
**				int inpdst(data,len,r,c)
**				int inpint(data,len,r,c)
**				int inpvec(data,len,r,c)
**				int inpstr(data,len,r,c)
**				int inpndc(data,len,r,c)
**				int inpang(data,len,r,c)
**				int inpflt(data,len,r,c)
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**       d4fiop.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION 
**       04/29/15 , 15:05:09
**
*********************************************************************/

#include "ustdio.h"
#include "usysdef.h"
#include "udebug.h"
#include "udfconst.h"
#include "udforms.h"
#include "udfrmdec.h"
#include "dinput.h"
#include "ddef.h"

#define CTRLEN 32

/*******************************************************************
**
**		I_FUNCTION	:		UD_FSTAT ud_inpdas(data,len,typ,r,c)
**
**			ud_inpdas inputs a das data item of length, type
**			specified at row and column specified.
**			types DASSELECT, DASCHOICE, DASPICK, and DASPCKLOC
**			are not currently supported (4/3/85).
**
**			PARAMETERS	:
**				INPUT		:
**				OUTPUT	:
**			RETURNS		:
**					UD_ALT - "users  alternate action"
**					UD_FWD - field forward
**					UD_BAK - field back
**					UD_DONE - "done" key hit
**					UD_PASSF - default was approved
**					UD_DASCORD - data type entered
**					UD_DASVAL -   "     "     "
**					UD_DASDISTANCE - "  "     "
**					UD_DASINT -   "     "     "
**					UD_DASVEC -   "     "     "
**					UD_DASSTRING - "    "     "
**					UD_DASNDC -    "    "     "
**					UD_DASANGLE -  "    "     "
**					UD_DASUNITLESS - "  "     "
**			SIDE EFFECTS:
**			WARNINGS		:
********************************************************************/

UD_FSTAT ud_inpdas(data,len,typ,r,c)
UD_DASIN *data;								/* ptr to UD_DASIN */
int len;											/* max length of data input in chars */
int typ;											/* data type expected */
int r,c;											/* row, col to input from */
{
	UD_FSTAT stat;

	uu_denter(UU_DTRC,(us,
	"entering ud_inpdas: dasin ptr=%x, type=%d, row=%d, col=%d",data,typ,r,c));

	switch (typ)								/* according to expected type, input: */
	{
		case UD_DASCART:						/* cartesian world coord */
			stat=(UD_FSTAT)inpcrd(data,len,r,c);
			break;

		case UD_DASSELECT:					/* select subsystem input */
			uu_dexit;
			return (UD_BADREQ);					/* select is undefined (3/18) */
			break;

		case UD_DASVAL:						/* value */
			stat=(UD_FSTAT)inpval(data,len,r,c);
			break;

		case UD_DASDISTANCE:					/* distance */
			stat=(UD_FSTAT)inpdst(data,len,r,c);
			break;

		case UD_DASINT:						/* integer */
			stat=(UD_FSTAT)inpint(data,len,r,c);
			break;

		case UD_DASVEC:						/* vector coord */
			stat=(UD_FSTAT)inpvec(data,len,r,c);
			break;

		case UD_DASPICK:						/* pick input */
			uu_dexit;
			return (UD_BADREQ);					/* pick is undefined (3/18) */
			break;

		case UD_DASPCKLOC:					/* locate input */
			uu_dexit;
			return (UD_BADREQ);					/* loc is undefined (3/18) */
			break;

		case UD_DASSTRING:					/* string */
			stat=(UD_FSTAT)inpstr(data,len,r,c);
			break;

		case UD_DASCHOICE:					/* choice input */
			uu_dexit;
			return (UD_BADREQ);					/* choice is undefined (3/18) */
			break;

		case UD_DASNDC:						/* ndc coord */
			stat=(UD_FSTAT)inpndc(data,len,r,c);
			break;

		case UD_DASANGLE:						/* angle */
			stat=(UD_FSTAT)inpang(data,len,r,c);
			break;

		case UD_DASUNITLESS:					/* unitless real */
			stat=(UD_FSTAT)inpflt(data,len,r,c);
			break;

		default:									/* or bad type */
			uu_dexit;
			return (UD_BADREQ);
	}
	uu_dexit;
	return (stat);
}

/*******************************************************************
**
**		I_FUNCTION	:		int inpcrd(data,len,r,c)
**
**			inpcrd inputs a coordinate (whose elements have
**			maximum length specified at row and column requested.
**
**			PARAMETERS	:
**				INPUT		:
**				OUTPUT	:
**			RETURNS		:
**					UD_ALT - "users  alternate action"
**					UD_FWD - field forward
**					UD_BAK - field back
**					UD_DONE - "done" key hit
**					UD_PASSF - default was approved
**					UD_DASCORD + OFFSET - good coordinate returned
**			SIDE EFFECTS:
**			WARNINGS		:
********************************************************************/

int inpcrd(data,len,r,c)
UD_DASIN *data;
int len;
int r,c;
{
	int rtn;
	UD_FSTAT frmgets();
	UD_DEVENT event;
	UD_DASTAT  ud_cart1(),status;
	UD_NDCLOCREC ret_cord;				/* coordinate to return  */
	UD_NDCLOCREC def_cord;				/* default coordinate */
	int ilen;
	int type, device, pet;			/* save locator device, pet, type */

	uu_denter(UU_DTRC,(us,"inpcrd: data ptr=%x, len=%d, r,c=%d %d",
			data,len,r,c));
	do
	{
		ud_crspos(r,c);
		ilen=len; if(ilen<0) ilen= -ilen;
		if((rtn=(int)frmgets(&event,3*ilen+4)) == (int)UD_DATAOK)
		{
			ud_qcord(&type, &device, &pet);
			status=ud_cart1(&event, &ret_cord, UU_FALSE, &def_cord, UU_TRUE);
			ud_dtcord(type, device, pet);
			(*data).cord.cord[0]=ret_cord.cord[0];
			(*data).cord.cord[1]=ret_cord.cord[1];
			(*data).cord.cord[2]=ret_cord.cord[2];
			rtn=UD_DASCART+OFFSET;
		}
		else
			break;
	} while(status==DE_AGAIN);

	ud_pstat((UD_FSTAT)rtn, "inpcrd");
	uu_dprint(UU_DTRC,(us,"inpcrd returns (*data).cord=%g %g %g",
			(*data).cord.cord[0],(*data).cord.cord[1],(*data).cord.cord[2]));
	uu_dexit;
	return(rtn);
}

/*******************************************************************
**
**		I_FUNCTION	:		int inpval(data,len,r,c)
**
**			inpval inputs a value which has maximum length
**			specified at row and column requested.
**
**			PARAMETERS	:
**				INPUT		:
**				OUTPUT	:
**			RETURNS		:
**					UD_ALT - "users  alternate action"
**					UD_FWD - field forward
**					UD_BAK - field back
**					UD_DONE - "done" key hit
**					UD_PASSF - default was approved
**					UD_DASVAL + OFFSET - good value returned
**			SIDE EFFECTS:
**			WARNINGS		:
********************************************************************/

int inpval(data,len,r,c)
UD_DASIN *data;
int len;
int r,c;
{
	int rtn;
	UD_FSTAT frmgets();
	UD_DEVENT event;
	UD_DASTAT  ud_val1(),status;
	UU_REAL ret_val;						/*   value to return  */
	UU_REAL def_val;						/*   default value */
	int ilen;

	uu_denter(UU_DTRC,(us,"inpval: data ptr=%x, len=%d r,c=%d %d",data,len,r,c));
	do
	{
		ud_crspos(r,c);
		ilen=len; if(ilen<0) ilen= -ilen;
		if((rtn=(int)frmgets(&event,ilen)) ==(int)UD_DATAOK)
		{
			status=ud_val1(&event, &ret_val, UU_FALSE, &def_val, UD_UNITLESS);
			(*data).dreal=ret_val;
			rtn=UD_DASVAL+OFFSET;
		}
		else
			break;
	} while(status==DE_AGAIN);
	ud_pstat((UD_FSTAT)rtn, "inpval");
	uu_dprint(UU_DTRC,(us,"inpval returns (*data).dreal=%g",(*data).dreal));
	uu_dexit;
	return(rtn);
}

/*******************************************************************
**
**		I_FUNCTION	:		int inpdst(data,len,r,c)
**
**			inpdst inputs a distance which has maximum length
**			specified at row and column requested.
**
**			PARAMETERS	:
**				INPUT		:
**				OUTPUT	:
**			RETURNS		:
**					UD_ALT - "users  alternate action"
**					UD_FWD - field forward
**					UD_BAK - field back
**					UD_DONE - "done" key hit
**					UD_PASSF - default was approved
**					UD_DASDISTANCE + OFFSET - good value returned
**			SIDE EFFECTS:
**			WARNINGS		:
********************************************************************/

int inpdst(data,len,r,c)
UD_DASIN *data;
int len;
int r,c;
{
	int rtn;
	UD_FSTAT frmgets();
	UD_DEVENT event;
	UD_DASTAT  ud_val1(),status;
	UU_REAL ret_val;						/*   value to return  */
	UU_REAL def_val;						/*   default value */
	int ilen;

	uu_denter(UU_DTRC,(us,"inpdst: data ptr=%x, len=%d,r,c=%d %d",data,len,r,c));
	do
	{
		ud_crspos(r,c);
		ilen=len; if(ilen<0) ilen= -ilen;
		if((rtn=(int)frmgets(&event,ilen)) == (int)UD_DATAOK)
		{
			status=ud_val1(&event, &ret_val, UU_FALSE, &def_val, UD_DISTANCE);
			(*data).dreal=ret_val;
			rtn=UD_DASDISTANCE+OFFSET;
		}
		else
			break;
	}while (status==DE_AGAIN);
	ud_pstat((UD_FSTAT)rtn, "inpdst");
	uu_dprint(UU_DTRC,(us,"inpdst returns dreal=%g",(*data).dreal));
	uu_dexit;
	return(rtn);
}

/*******************************************************************
**
**		I_FUNCTION	:		int inpint(data,len,r,c)
**
**			inpint inputs an integer which has maximum length
**			specified at row and column requested.
**
**			PARAMETERS	:
**				INPUT		:
**				OUTPUT	:
**			RETURNS		:
**					UD_ALT - "users  alternate action"
**					UD_FWD - field forward
**					UD_BAK - field back
**					UD_DONE - "done" key hit
**					UD_PASSF - default was approved
**					UD_DASINT + OFFSET - good value returned
**			SIDE EFFECTS:
**			WARNINGS		:
********************************************************************/

int inpint(data,len,r,c)
UD_DASIN *data;
int len;
int r,c;
{
	int rtn;
	UD_FSTAT frmgets();
	int def_int;
	UD_DEVENT event;
	UD_DASTAT  ud_int1(),status;
	int ilen;

	uu_denter(UU_DTRC,(us,"inpint: data ptr=%x len=%d r,c=%d %d",data,len,r,c));
	do
	{
		ud_crspos(r,c);
		ilen=len; if(ilen<0) ilen= -ilen;
		if((rtn=(int)frmgets(&event,ilen)) == (int)UD_DATAOK)
		{
			status = ud_int1(&event, &(*data).dint, UU_FALSE, &def_int);
			rtn=UD_DASINT+OFFSET;
		}
		else
			break;
	} while(status==DE_AGAIN);
	ud_pstat((UD_FSTAT)rtn, "inpint");
	uu_dprint(UU_DTRC,(us,"inpint returns dint=%d",(*data).dint));
	uu_dexit;
	return(rtn);
}

/*******************************************************************
**
**		I_FUNCTION	:		int inpvec(data,len,r,c)
**
**			inpvec inputs a coordinate (whose elements have
**			maximum length specified at row and column requested.
**
**			PARAMETERS	:
**				INPUT		:
**				OUTPUT	:
**			RETURNS		:
**					UD_ALT - "users  alternate action"
**					UD_FWD - field forward
**					UD_BAK - field back
**					UD_DONE - "done" key hit
**					UD_PASSF - default was approved
**					UD_DASVEC + OFFSET - good coordinate returned
**			SIDE EFFECTS:
**			WARNINGS		:
********************************************************************/

int inpvec(data,len,r,c)
UD_DASIN *data;
int len;
int r,c;
{
	int rtn;
	UD_FSTAT frmgets();
	UD_DEVENT event;
	UD_DASTAT  ud_vec1(),status;
	UU_REAL ret_cord[3];				/* coordinate to return  */
	UU_REAL def_cord[3];				/* default coordinate */
	int ilen;

	uu_denter(UU_DTRC,("inpvec: data ptr=%x len=%d r,c=%d %d",data,len,r,c));
	do
	{
		ud_crspos(r,c);
		ilen=len; if(ilen<0) ilen= -ilen;
		if((rtn=(int)frmgets(&event,3*ilen+4)) == (int)UD_DATAOK)
		{
			status= ud_vec1(&event, ret_cord, UU_FALSE, def_cord);
			(*data).cord.cord[0]=ret_cord[0];
			(*data).cord.cord[1]=ret_cord[1];
			(*data).cord.cord[2]=ret_cord[2];
			rtn=UD_DASVEC+OFFSET;
		}
		else
			break;
	}while (status==DE_AGAIN);
	ud_pstat((UD_FSTAT)rtn, "inpvec");
	uu_dprint(UU_DTRC,(us,"inpvec returns cord=%g %g %g",(*data).cord.cord[0],
		(*data).cord.cord[1],(*data).cord.cord[2]));
	uu_dexit;
	return(rtn);
}

/*******************************************************************
**
**		I_FUNCTION	:		int inpstr(data,len,r,c)
**
**			inpstr inputs a string which has maximum length
**			specified at row and column requested.
**
**			PARAMETERS	:
**				INPUT		:
**				OUTPUT	:
**			RETURNS		:
**					UD_ALT - "users  alternate action"
**					UD_FWD - field forward
**					UD_BAK - field back
**					UD_DONE - "done" key hit
**					UD_PASSF - default was approved
**					UD_DASSTRING + OFFSET - good value returned
**			SIDE EFFECTS:
**			WARNINGS		:
********************************************************************/

int inpstr(data,len,r,c)
UD_DASIN *data;
int len;
int r,c;
{
	int rtn;
	char def_str[80];
	UD_FSTAT frmgets();
	UD_DEVENT event;
	UD_DASTAT  ud_str1(),status;
	int l,ilen;

	uu_denter(UU_DTRC,(us,"inpstr: data ptr=%x, len=%d r,c=%d %d",
		data,len,r,c));
	do
	{
		ud_crspos(r,c);
		ilen=len; if(ilen<0) ilen= -ilen;
		if((rtn=(int)frmgets(&event,ilen)) == (int)UD_DATAOK)
		{
			def_str[0]=0;
			status= ud_str1(&event, (*data).dstr, UU_FALSE, def_str, 80, &l);
			if(status!=DE_AGAIN) {
				uu_dprint(UU_DTRC,(us,
					"inpstr. l=%d, evclass=%d, stringdata=%s, (*data).dstr=%s",
					l,event.evclass,event.indata.stringdata,(*data).dstr));
				rtn=UD_DASSTRING+OFFSET;
			}
		}
		else
			break;
	} while(status==DE_AGAIN);
	uu_dexit;
	return(rtn);
}

/*******************************************************************
**
**		I_FUNCTION	:		int inpndc(data,len,r,c)
**
**			inpndc inputs a coordinate (whose elements have
**			maximum length specified at row and column requested.
**
**			PARAMETERS	:
**				INPUT		:
**				OUTPUT	:
**			RETURNS		:
**					UD_ALT - "users  alternate action"
**					UD_FWD - field forward
**					UD_BAK - field back
**					UD_DONE - "done" key hit
**					UD_PASSF - default was approved
**					UD_DASNDC + OFFSET - good coordinate returned
**			SIDE EFFECTS:
**			WARNINGS		:
********************************************************************/

int inpndc(data,len,r,c)
UD_DASIN *data;
int len;
int r,c;
{
	int rtn;
	UD_FSTAT frmgets();
	UD_DASTAT  ud_cart1(),status;
	UD_DEVENT event;
	UD_NDCLOCREC ret_cord;				/* coordinate to return  */
	UD_NDCLOCREC def_cord;				/* default coordinate */
	int ilen;

	uu_denter(UU_DTRC,(us,"inpndc: data ptr=%x, len=%d r,c=%d %d",data,len,r,c));
	do
	{
		ud_crspos(r,c);
		ilen=len; if(ilen<0) ilen= -ilen;
		if((rtn=(int)frmgets(&event,3*ilen+4)) == (int)UD_DATAOK)
		{
			status=ud_cart1(&event, &ret_cord, UU_FALSE, &def_cord, UU_FALSE);
			(*data).cord.cord[0]=ret_cord.cord[0];
			(*data).cord.cord[1]=ret_cord.cord[1];
			(*data).cord.cord[2]=ret_cord.cord[2];
			rtn=UD_DASNDC+OFFSET;
		}
		else
			break;
	} while(status==DE_AGAIN);
	uu_dprint(UU_DTRC,(us,"inpndc returns cord=%g %g %g",(*data).cord.cord[0],
		(*data).cord.cord[1],(*data).cord.cord[2]));
	uu_dexit;
	return(rtn);
}

/*******************************************************************
**
**		I_FUNCTION	:		int inpang(data,len,r,c)
**
**			inpang inputs an angle which has maximum length
**			specified at row and column requested.
**
**			PARAMETERS	:
**				INPUT		:
**				OUTPUT	:
**			RETURNS		:
**					UD_ALT - "users  alternate action"
**					UD_FWD - field forward
**					UD_BAK - field back
**					UD_DONE - "done" key hit
**					UD_PASSF - default was approved
**					UD_DASANGLE + OFFSET - good value returned
**			SIDE EFFECTS:
**			WARNINGS		:
********************************************************************/

int inpang(data,len,r,c)
UD_DASIN *data;
int len;
int r,c;
{
	int rtn;
	UD_FSTAT frmgets();
	UD_DEVENT event;
	UD_DASTAT  ud_val1(),status;
	UU_REAL ret_val;						/*   value to return  */
	UU_REAL def_val;						/*   default value */
	int ilen;

	uu_denter(UU_DTRC,(us,"inpang: data ptr=%x, len=%d r,c=%d %d",data,len,r,c));
	do
	{
		ud_crspos(r,c);
		ilen=len; if(ilen<0) ilen= -ilen;
		if((rtn=(int)frmgets(&event,ilen)) == (int)UD_DATAOK)
		{
			status=ud_val1(&event, &ret_val, UU_FALSE, &def_val, UD_DANGLE);
			(*data).dreal=ret_val;
			rtn=UD_DASANGLE+OFFSET;
		}
		else
			break;
	} while(status==DE_AGAIN);
	uu_dprint(UU_DTRC,(us,"inpang returns dreal=%g",(*data).dreal));
	uu_dexit;
	return(rtn);
}

/*******************************************************************
**
**		I_FUNCTION	:		int inpflt(data,len,r,c)
**
**			inpflt inputs a value which has maximum length
**			specified at row and column requested.
**
**			PARAMETERS	:
**				INPUT		:
**				OUTPUT	:
**			RETURNS		:
**					UD_ALT - "users  alternate action"
**					UD_FWD - field forward
**					UD_BAK - field back
**					UD_DONE - "done" key hit
**					UD_PASSF - default was approved
**					UD_DASUNITLESS + OFFSET - good value returned
**			SIDE EFFECTS:
**			WARNINGS		:
********************************************************************/

int inpflt(data,len,r,c)
UD_DASIN *data;
int len;	
int r,c;
{
	int rtn;
	UD_FSTAT frmgets();
	UD_DEVENT event;
	UD_DASTAT  ud_val1(),status;
	UU_REAL ret_val;						/*   value to return  */
	UU_REAL def_val;						/*   default value */
	int ilen;

	uu_denter(UU_DTRC,(us,"inpflt: data ptr=%x, len=%d r,c=%d %d",data,len,r,c));
	do
	{
		ud_crspos(r,c);
		ilen=len; if(ilen<0) ilen= -ilen;
		if((rtn=(int)frmgets(&event,ilen)) == (int)UD_DATAOK)
		{
			status=ud_val1(&event, &ret_val, UU_FALSE, &def_val, UD_UNITLESS);
			(*data).dreal=ret_val;
			rtn=UD_DASUNITLESS+OFFSET;
		}
		else
			break;
	}while (status==DE_AGAIN);
	uu_dprint(UU_DTRC,(us,"inpflt returns dreal=%g",(*data).dreal));
	uu_dexit;
	return(rtn);
}
