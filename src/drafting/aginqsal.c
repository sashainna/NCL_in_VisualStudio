/*********************************************************************
**    NAME      :  aginqsal.c -- SAL -> GKS inquiry interface functions
**       CONTAINS:
**		sgqtextindex() -- Inquire text index
**     sgqtextfont() -- Inquire text font number
**     sgqtextprec() -- Inquire text precision
**		sgqcharexp() -- Inquire character expansion factor
**		sgqcharspace() -- Inquire character spacing 
**		sgqtextcolor() -- Inquire text color
**		sgqcharheight() -- Inquire character height 
**		sgqtextpath() -- Inquire text path 
**     sgqtextalignh() -- Inquire text alignment- horizontal
**     sgqtextalignv() -- Inquire text alignment- vertical
**     sgqcharup() --     Inquire character up vector
**     sgqtxplane() --    Inquire character text plane normal
**
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       aginqsal.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:34
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "go.h"
#include "gtbl.h"
#include "ginq.h"

/*********************************************************************
	E_FUNCTION: gqtextindex() -- Inquire text index
#define gqtextindex() ug_gksstli.curprats.txindex
*********************************************************************/
Gindex sgqtextindex()
{
	return( gqtextindex() );
}

/*********************************************************************
	E_FUNCTION: gqtextfp() -- Inquire text font and precision 
#define gqtextfp() (&ug_gksstli.curprats.txbundl.fp)
*********************************************************************/
int sgqtextfont()
{
	Gtxfp  *pfp;
	pfp = gqtextfp();
	return( pfp->font);
}
Gtxprec sgqtextprec()
{
	Gtxfp  *pfp;
	pfp = gqtextfp();
	return( pfp->prec);
}

/*********************************************************************
	E_FUNCTION: gqcharexp() -- Inquire character expansion factor
#define gqcharexp() ug_gksstli.curprats.txbundl.expn
*********************************************************************/
Gchrexp sgqcharexp()
{
	return( gqcharexp() );
}

/*********************************************************************
	E_FUNCTION: gqcharspace() -- Inquire character spacing 
#define gqcharspace() ug_gksstli.curprats.txbundl.space
*********************************************************************/
Gchrsp sgqcharspace()
{
	return( gqcharspace() );
}

/*********************************************************************
	E_FUNCTION: gqtextcolor() -- Inquire text color
#define gqtextcolor() ug_gksstli.curprats.txbundl.color
*********************************************************************/
Gcolor sgqtextcolor()
{
	return( gqtextcolor() );
}

/*********************************************************************
	E_FUNCTION: gqcharheight() -- Inquire character height 
#define gqcharheight() ug_gksstli.curprats.txht
*********************************************************************/
Gchrht sgqcharheight()
{
	return( gqcharheight() );
}

/*********************************************************************
	E_FUNCTION: gqtextpath() -- Inquire text path 
#define gqtextpath() ug_gksstli.curprats.txpath
*********************************************************************/
Gtxpath sgqtextpath()
{
	return( gqtextpath() );
}

/*********************************************************************
	E_FUNCTION: gqtextalign() -- Inquire text alignment
*********************************************************************/
Gtxhor sgqtextalignh()
{
	Gtxalign *palign;
	palign = gqtextalign();
	return( palign->hor);
}
Gtxver sgqtextalignv()
{
	Gtxalign *palign;
	palign = gqtextalign();
	return( palign->ver);
}

/*********************************************************************
	E_FUNCTION: gqcharup() -- Inquire character up vector
*********************************************************************/
sgqcharup(coord)
UU_REAL coord[3];
{
	Gwpoint  *gqcharup();
	Gwpoint3  *tempv;

	tempv = (Gwpoint3 *)gqcharup();
	coord[0] = tempv->x;
	coord[1] = tempv->y;
	coord[2] = tempv->z;
}

/*********************************************************************
	E_FUNCTION: gqtxplane() -- Inquire character text plane normal
*********************************************************************/
sgqtxplane(coord)
UU_REAL coord[3];
{
	Gwpoint3  *gqtxplane();
	Gwpoint3  *tempv;

	tempv = gqtxplane();
	coord[0] = tempv->x;
	coord[1] = tempv->y;
	coord[2] = tempv->z;
}
