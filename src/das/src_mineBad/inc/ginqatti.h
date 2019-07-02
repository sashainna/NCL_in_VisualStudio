/*********************************************************************
**    NAME      :  ginqatti.h -- internal GKS attribute inquiry macros.
**       CONTAINS:
**		ug_qlinetypeno() -- Inquire polyline typeno 
**		ug_qlinewidth() -- Inquire polyline width
**		ug_qlinecolor() -- Inquire polyline color 
**		ug_qmarktype() -- Inquire polymarker type 
**		ug_qmarkcolor() -- Inquire polymarker color
**		ug_qtextcolor() -- Inquire text color
**		ug_qtxalignhor() -- Inquire text horizontal alignment
**		ug_qtxalignver() -- Inquire text vertical alignment
**		ug_qtextprec() -- Inquire text precision
**		ug_qcharheight() -- Inquire character height 
**		ug_qfillcolor() -- Inquire fill color
**		ug_qfilledge() -- Inquire fill edge flag
**		ug_qpickid() -- Inquire pick id
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       ginqatti.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:19
*********************************************************************/

#ifndef GINQATTIH

#include "go.h"
#include "gtbl.h"
#include "gipick.h"

/*********************************************************************
	I_FUNCTION: Gint ug_qlinetypeno() -- Inquire polyline typeno 
*********************************************************************/
#define ug_qlinetypeno() ug_gksstli.curprats.lnbundl.type.typeno

/*********************************************************************
	I_FUNCTION: Gscale gqlinewidth() -- Inquire polyline width
*********************************************************************/
#define gqlinewidth() ug_gksstli.curprats.lnbundl.width

/*********************************************************************
	I_FUNCTION: Gcolor ug_qlinecolor() -- Inquire polyline color 
*********************************************************************/
#define ug_qlinecolor() ug_gksstli.curprats.lnbundl.color

/*********************************************************************
	I_FUNCTION: Gmktype ug_qmarktype() -- Inquire polymarker type 
*********************************************************************/
#define ug_qmarktype() ug_gksstli.curprats.mkbundl.type

/*********************************************************************
	I_FUNCTION: Gcolor ug_qmarkcolor() -- Inquire polymarker color
*********************************************************************/
#define ug_qmarkcolor() ug_gksstli.curprats.mkbundl.color

/*********************************************************************
	I_FUNCTION: Gcolor ug_qtextcolor() -- Inquire text color
*********************************************************************/
#define ug_qtextcolor() ug_gksstli.curprats.txbundl.color

/*********************************************************************
	I_FUNCTION: Gwc ug_qcharheight() -- Inquire character height
*********************************************************************/
#define ug_qcharheight() ug_gksstli.curprats.txht

/*********************************************************************
	I_FUNCTION: Gtxhor ug_qtxalignhor() -- Inquire text hor. alignment
*********************************************************************/
#define ug_qtxalignhor() ug_gksstli.curprats.txalign.hor

/*********************************************************************
	I_FUNCTION: Gtxver ug_qtxalignver() -- Inquire text ver. alignment
*********************************************************************/
#define ug_qtxalignver() ug_gksstli.curprats.txalign.ver

/*********************************************************************
	I_FUNCTION: Gtxprec ug_qtextprec() -- Inquire text precision
*********************************************************************/
#define ug_qtextprec() ug_gksstli.curprats.txbundl.fp.prec

/*********************************************************************
	I_FUNCTION: Gcolor ug_qfillcolor() -- Inquire fill area color
*********************************************************************/
#define ug_qfillcolor() ug_gksstli.curprats.flbundl.color

/*********************************************************************
	I_FUNCTION: Gtoggle ug_qfilledge() -- Inquire fill area edgeflag
*********************************************************************/
#define ug_qfilledge() ug_gksstli.curprats.flbundl.edgeflag

/*********************************************************************
	I_FUNCTION: Gpickid ug_qpickid() -- Inquire pick id
*********************************************************************/
#define ug_qpickid() ug_gksstli.curprats.pickid

#define GINQATTIH
#endif
