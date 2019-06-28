/*************************************************************************
**		NAME	:	udfiopako.c
**			Contains:	
**
**		COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       d4fioko.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION 
**       04/29/15 , 15:05:09
**************************************************************************/
#include "ustdio.h"
#include "usysdef.h"
#include "udfconst.h"
#include "udforms.h"
#include "udfrmdec.h"
#include "udebug.h"
#include "driver.h"

#define CTRLEN 32
	extern int UD_ksws;
	extern int UD_formdev;				/* form device number */
   extern char **ud_curdflt;			/* current default for each field */
	int ud_curfldno;
/******************* output functions **********************/

/*---------------------- outdas ------------------------*/
/*******************************************************************
**
**		I_FUNCTION	:		UD_FSTAT ud_outdas(r,c,typ,prec,len,data)
**								UD_DASIN *data;
**								int len;
**								int typ;
**								int prec, len;
**								int r,c;
**
**			Synopsis:	ud_outdas outputs a das data item of length,
**						precision, and type specified at row and column
**						specified. Types DASSELECT, DASCHOICE, DASPICK,
**						and DASPCKLOC are not currently supported (4/3/85).
**
**			Parameters	:
**				Input		:
**				Output	:
**			Returns		:
**				UD_BADREQ - invalid or unsupported type specified
**				UD_OUTOK - successful output
**			Side Effects:
**			Warnings		:
********************************************************************/
UD_FSTAT outdas(r,c,typ,prec,len,data)
UD_DASIN *data;
int len;
int prec;
int typ;
int r,c;
{
	return(UD_OUTOK);
}

/*------------------------ outcrd ------------------------*/
/*******************************************************************
**
**		I_FUNCTION	:		int outcrd(r,c,prec,len,data)
**								int r,c;
**								int prec,len;
**								UD_DASIN *data;
**
**			Synopsis:	outcdr displays the DASCORD pointed to at the
**						row and column specified, with the length and
**						precision requested.
**
**			Parameters	:
**				Input		:
**				Output	:
**			Returns		:
**			Side Effects:
**			Warnings		:
********************************************************************/
void outcrd(r,c,prec,len,data)
UD_DASIN *data;
int prec,len;
int r,c;
{
	return;
}

/*------------------------ outval -----------------------*/
/*******************************************************************
**
**		I_FUNCTION	:		int outval(r,c,prec,len,data)
**								int r,c;
**								int prec,len;
**								UD_DASIN *data;
**
**			Synopsis:	outval displays the DASVAL pointed to at the
**						row and column specified, with the length and
**						precision requested.
**
**			Parameters	:
**				Input		:
**				Output	:
**			Returns		:
**			Side Effects:
**			Warnings		:
********************************************************************/
void outval(r,c,prec,len,data)
UD_DASIN *data;
int prec,len;
int r,c;
{
	return;
}

/*------------------------ outdst -----------------------*/
/*******************************************************************
**
**		I_FUNCTION	:		int outdst(r,c,prec,len,data)
**								int r,c;
**								int prec,len;
**								UD_DASIN *data;
**
**			Synopsis:	outdst displays the DASDISTANCE pointed to at the
**						row and column specified, with the length and
**						precision requested.
**
**			Parameters	:
**				Input		:
**				Output	:
**			Returns		:
**			Side Effects:
**			Warnings		:
********************************************************************/
void outdst(r,c,prec,len,data)
UD_DASIN *data;
int prec,len;
int r,c;
{
	return;
}

/*------------------------ outint -----------------------*/
/*******************************************************************
**
**		I_FUNCTION	:		int outint(r,c,prec,len,data)
**								int r,c;
**								int prec,len;
**								UD_DASIN *data;
**
**			Synopsis:	outint displays the DASINT pointed to at the
**						row and column specified, with the length and
**						precision requested.
**
**			Parameters	:
**				Input		:
**				Output	:
**			Returns		:
**			Side Effects:
**			Warnings		:
********************************************************************/
void outint(r,c,prec,len,data)
UD_DASIN *data;
int prec,len;
int r,c;
{
	return;
}

/*------------------------ outvec ------------------------*/
/*******************************************************************
**
**		I_FUNCTION	:		int outvec(r,c,prec,len,data)
**								int r,c;
**								int prec,len;
**								UD_DASIN *data;
**
**			Synopsis:	outvec displays the DASVEC pointed to at the
**						row and column specified, with the length and
**						precision requested.
**
**			Parameters	:
**				Input		:
**				Output	:
**			Returns		:
**			Side Effects:
**			Warnings		:
********************************************************************/
void outvec(r,c,prec,len,data)
UD_DASIN *data;
int prec,len;
int r,c;
{
	return;
}

/*------------------------- outstr ------------------------*/
/*******************************************************************
**
**		I_FUNCTION	:		int outstr(r,c,prec,len,data)
**								int r,c;
**								int prec,len;
**								UD_DASIN *data;
**
**			Synopsis:	outstr displays the DASSTRING pointed to at the
**						row and column specified, with the length and
**						precision requested.
**
**			Parameters	:
**				Input		:
**				Output	:
**			Returns		:
**			Side Effects:
**			Warnings		:
********************************************************************/
void outstr(r,c,prec,len,data)
UD_DASIN *data;
int prec,len;
int r,c;
{
	return;
}

/*------------------------ outndc ------------------------*/
/*******************************************************************
**
**		I_FUNCTION	:		int outndc(r,c,prec,len,data)
**								int r,c;
**								int prec,len;
**								UD_DASIN *data;
**
**			Synopsis:	outndc displays the DASNDC pointed to at the
**						row and column specified, with the length and
**						precision requested.
**
**			Parameters	:
**				Input		:
**				Output	:
**			Returns		:
**			Side Effects:
**			Warnings		:
********************************************************************/
void outndc(r,c,prec,len,data)
UD_DASIN *data;
int prec,len;
int r,c;
{
	return;
}

/*------------------------ outang -----------------------*/
/*******************************************************************
**
**		I_FUNCTION	:		int outang(r,c,prec,len,data)
**								int r,c;
**								int prec,len;
**								UD_DASIN *data;
**
**			Synopsis:	outang displays the DASANGLE pointed to at the
**						row and column specified, with the length and
**						precision requested.
**
**			Parameters	:
**				Input		:
**				Output	:
**			Returns		:
**			Side Effects:
**			Warnings		:
********************************************************************/
void outang(r,c,prec,len,data)
UD_DASIN *data;
int prec,len;
int r,c;
{
	return;
}

/*------------------------ outflt -----------------------*/
/*******************************************************************
**
**		I_FUNCTION	:		int outflt(r,c,prec,len,data)
**								int r,c;
**								int prec,len;
**								UD_DASIN *data;
**
**			Synopsis:	outflt displays the DASUNITLESS pointed to at the
**						row and column specified, with the length and
**						precision requested.
**
**			Parameters	:
**				Input		:
**				Output	:
**			Returns		:
**			Side Effects:
**			Warnings		:
********************************************************************/
void outflt(r,c,prec,len,data)
UD_DASIN *data;
int r,c;
{
	return;
}
