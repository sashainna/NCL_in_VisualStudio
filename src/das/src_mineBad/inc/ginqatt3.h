/*********************************************************************
**    NAME      :  gksinqatt3.h -- simple GKS inquiry functions, macros.
**       CONTAINS:
**
**		gqfillindex      -- Inquire fill index 
**		gqfillintstyle   -- Inquire fill interior style 
**		gqfillstyleindex -- Inquire fill interior style index
**		gqfillcolor      -- Inquire fill color
**		gqfilledge       -- Inquire fill edge flag
**	   gqsavescrn       -- Inquire devices ability to save/rest scrn
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       ginqatt3.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:19
*********************************************************************/

#ifndef GKSINQATT3H
/*********************************************************************
	Gindex gqfillindex() -- Inquire fill index 
*********************************************************************/
#define gqfillindex() ug_gksstli.curprats.flindex

/*********************************************************************
	Gflinter gqfillintstyle() -- Inquire fill interior style 
*********************************************************************/
#define gqfillintstyle() ug_gksstli.curprats.flbundl.inter

/*********************************************************************
	Gflstyle gqfillstyleindex() -- Inquire fill interior style index
*********************************************************************/
#define gqfillstyleindex() ug_gksstli.curprats.flbundl.style

/*********************************************************************
	Gcolor gqfillcolor() -- Inquire fill color
*********************************************************************/
#define gqfillcolor() ug_gksstli.curprats.flbundl.color

/*********************************************************************
	Gtoggle gqfilledge() -- Inquire fill edge flag
*********************************************************************/
#define gqfilledge() ug_gksstli.curprats.flbundl.edgeflag

/***************************************************************************
	int gqsavescrn() -- Inquire devices ability to save/rest scrn
****************************************************************************/
#define gqsavescrn(ws)  ((*(ug_gksstli.wsopen[ws].wdtptr)).savescrn)

#define GKSINQATT3H
#endif
