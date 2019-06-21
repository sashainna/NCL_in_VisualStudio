/*********************************************************************
**    NAME      :  ginqxf.h -- simple GKS inquiry xform functions, macros.
**       CONTAINS:
**		gqnormtran() -- Inquire current normalization transformation
**		gqnormmat(xform) -- get normalization matrix.
**		gqwindow3(trans) -- Inquire window 3D.
**	   gqvup3(trans) -- inquire view up vector 3D.
**		gqvpn3(xform) -- Inquire view plane normal 3D.
**		gqrefpt(xform) -- Inquire view reference point. 
**		gqpickid() -- Inquire current pick id
**		gqdisplaysize(ws) -- Inquire display size
**		gqrowmax(ws) -- inquire number of hardware char rows
**		gqacharcol(ws) -- inquire number of hardware char cols
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       ginqxf.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:19
*********************************************************************/

#ifndef GKSINQXFH
/*********************************************************************
	E_FUNCTION: Gindex gqnormtran() -- Inquire curr. normalization transformation
*********************************************************************/
#define gqnormtran() ug_gksstli.curvwindex

/*********************************************************************
	E_FUNCTION: Gtran gqnormmat(xform) -- Inquire normalization matrix
*********************************************************************/
#define gqnormmat(xform) (ug_cxform[xform])

/*********************************************************************
	E_FUNCTION: Gwrect3 gqwindow3(xform) -- Inquire window 3D.
*********************************************************************/
#define gqwindow3(xform) (&(ug_gksstli.vtran[xform].window))

/*********************************************************************
	E_FUNCTION: Gnrect3 gqvport3(xform) -- Inquire viewport 3D.
*********************************************************************/
#define gqvport3(xform) (&(ug_gksstli.vtran[xform].vport))

/*********************************************************************
	E_FUNCTION: Gwpoint3 gqvup3(xform) -- Inquire view up vector 3D.
*********************************************************************/
#define gqvup3(xform) (&(ug_gksstli.vtran[xform].vup))

/*********************************************************************
	E_FUNCTION: Gwpoint3 gqvpn3(xform) -- Inquire view plane normal 3D.
*********************************************************************/
#define gqvpn3(xform) (&(ug_gksstli.vtran[xform].vpnorm))

/*********************************************************************
	E_FUNCTION: Gwpoint3 gqrefpt(xform) -- Inquire view reference point. 
*********************************************************************/
#define gqrefpt(xform) (&(ug_gksstli.vtran[xform].vrefpt))

/*********************************************************************
	E_FUNCTION: Gpickid gqpickid() -- Inquire current pick id
*********************************************************************/
#define gqpickid() ug_gksstli.curprats.pickid
#define GKSINQXFH
#endif
