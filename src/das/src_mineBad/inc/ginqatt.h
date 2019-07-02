/*********************************************************************
**    NAME      :  gksinqatt.h -- simple GKS inquiry functions, macros.
**       CONTAINS:
**
**		gqlineindex -- Inquire polyline index.
**		gqlinetype  -- Inquire polyline type 
**		gqlinewidth -- Inquire polyline index
**		gqlinecolor -- Inquire polyline color 
**		gqmarkindex -- Inquire polymarker index
**		gqmarktype  -- Inquire polymarker type 
**		gqmarksize  -- Inquire polymarker size 
**		gqmarkcolor -- Inquire polymarker color
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       ginqatt.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:19
*********************************************************************/

#ifndef GKSINQATTH
/*********************************************************************
	Gindex gqlineindex() -- Inquire polyline index.
*********************************************************************/
#define gqlineindex() ug_gksstli.curprats.lnindex

/*********************************************************************
	Glntype gqlinetype() -- Inquire polyline type 
*********************************************************************/
#define gqlinetype() (&(ug_gksstli.curprats.lnbundl.type))

/*********************************************************************
	Gscale gqlinewidth() -- Inquire polyline index
*********************************************************************/
#define gqlinewidth() ug_gksstli.curprats.lnbundl.width

/*********************************************************************
	Gcolor gqlinecolor() -- Inquire polyline color 
*********************************************************************/
#define gqlinecolor() ug_gksstli.curprats.lnbundl.color

/*********************************************************************
	Gindex gqmarkindex() -- Inquire polymarker index
*********************************************************************/
#define gqmarkindex() ug_gksstli.curprats.mkindex

/*********************************************************************
	E_FUNCTION: Gmktype gqmarktype() -- Inquire polymarker type 
*********************************************************************/
#define gqmarktype() ug_gksstli.curprats.mkbundl.type

/*********************************************************************
	E_FUNCTION: Gscale gqmarksize() -- Inquire polymarker size 
*********************************************************************/
#define gqmarksize() ug_gksstli.curprats.mkbundl.size

/*********************************************************************
	E_FUNCTION: Gcolor gqmarkcolor() -- Inquire polymarker color
*********************************************************************/
#define gqmarkcolor() ug_gksstli.curprats.mkbundl.color
#define GKSINQATTH
#endif
