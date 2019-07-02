/*********************************************************************
**    NAME      :  gksinqatt2.h -- simple GKS inquiry functions, macros.
**       CONTAINS:
**
**		gqtextindex  -- Inquire text index
**		gqtextfp     -- Inquire text font and precision 
**		gqcharexp    -- Inquire character expansion factor
**		gqcharspace  -- Inquire character spacing 
**		gqtextcolor  -- Inquire text color
**		gqcharheight -- Inquire character height 
**		gqtextpath   -- Inquire text path 
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       ginqatt2.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:19
*********************************************************************/

#ifndef GKSINQATT2H
/*********************************************************************
	Gindex gqtextindex() -- Inquire text index
*********************************************************************/
#define gqtextindex() ug_gksstli.curprats.txindex

/*********************************************************************
	Gtxfp gqtextfp() -- Inquire text font and precision 
*********************************************************************/
#define gqtextfp() (&ug_gksstli.curprats.txbundl.fp)

/*********************************************************************
	Gchrexp gqcharexp() -- Inquire character expansion factor
*********************************************************************/
#define gqcharexp() ug_gksstli.curprats.txbundl.expn

/*********************************************************************
	Gchrsp gqcharspace() -- Inquire character spacing 
*********************************************************************/
#define gqcharspace() ug_gksstli.curprats.txbundl.space

/*********************************************************************
	Gcolor gqtextcolor() -- Inquire text color
*********************************************************************/
#define gqtextcolor() ug_gksstli.curprats.txbundl.color

/*********************************************************************
	Gchrht gqcharheight() -- Inquire character height 
*********************************************************************/
#define gqcharheight() ug_gksstli.curprats.txht

/*********************************************************************
	Gtxpath gqtextpath() -- Inquire text path 
*********************************************************************/
#define gqtextpath() ug_gksstli.curprats.txpath
#define GKSINQATT2H
#endif
