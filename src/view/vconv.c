/*********************************************************************
**    NAME         :  vconv.c
**       CONTAINS: Routines to convert ndc and world coordinates back and forth
**			uv_cctondc(cc,ndc,xform)
**			uv_ndctocc(ndc,cc,xform)
**			uv_projvpln(cc,vpcc,xform)
**			int uv_projcpln(cc,cpcc,xform)
**			int uv_cpln_parallel_vpln(xform)
**  COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**   MODULE NAME AND RELEASE LEVEL 
**       vconv.c , 25.1
**  DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:11:57
**************************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "mdcoord.h"
#include "mdcpln.h"
#include "vconst.h"
#include "view.h"
#include "go.h"

/*********************************************************************
**    E_FUNCTION     : uv_cctondc(cc,ndc,xform)
**      Convert a cartesian model  coordinate to a NDC  coordinate.
**    PARAMETERS   
**       INPUT  : 
**				cc							cartesian model  coordinate
**				xform						norm transformation number
**       OUTPUT :  
**				ndc						NDC  coordinate
**    RETURNS      : UU_SUCCESS if vport is a valid active viewport
**							UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uv_cctondc(cc, ndc, xform)
	UM_coord		cc;
	UM_ndc		ndc;
	int			xform;

	{
	uu_denter(UU_MTRC,(us,"uv_cctondc(cc=(%f,%f,%f),xform=%d)",
		cc[0],cc[1],cc[2],xform));
	gsnormtran(xform);
	gwndc3(&ndc[0],&ndc[1],&ndc[2],cc[0],cc[1],cc[2]);
	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION     : uv_ndctocc(ndc,cc,xform)
**      Convert an NDC  coordinate to a cartesian model  coordinate.
**    PARAMETERS   
**       INPUT  : 
**				ndc						NDC  coordinate
**				xform						norm transformation number
**       OUTPUT :  
**				cc							cartesian model  coordinate
**    RETURNS      : UU_SUCCESS if vport is a valid active viewport
**							UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uv_ndctocc(ndc, cc, xform)
	UM_ndc		ndc;
	UM_coord		cc;
	int			xform;

	{
	uu_denter(UU_MTRC,(us,"uv_ndctocc(ndc=(%f,%f,%f),xform=%d)",
		ndc[0],ndc[1],ndc[2],xform));
	gsnormtran(xform);
	gndcw3(&cc[0],&cc[1],&cc[2],ndc[0],ndc[1],ndc[2]);
	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION     : uv_projvpln(cc,vpcc,xform)
**      Project a model  coordinate onto the view plane.
**    PARAMETERS   
**       INPUT  : 
**				cc							Cartesian model  coordinate
**				xform						norm transformation number
**       OUTPUT :  
**				vpcc						Cartesian  coordinate on view plane
**    RETURNS      : UU_SUCCESS if vport is a valid active viewport
**							UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uv_projvpln(cc, vpcc, xform)
	UM_coord		cc;
	UM_coord		vpcc;
	int			xform;

	{
	UM_vector	view_norm;
	UM_coord		ref_pt;
	Gwpoint3		vpn;
	Gwpoint3		vref;

	uu_denter(UU_MTRC,(us,"uv_projvpln(cc=(%f,%f,%f),xform=%d)",
		cc[0],cc[1],cc[2],xform));
	/* query DIGS for the view plane normal for this transformation */
	givpn3(xform, &vpn);
	view_norm[0] = vpn.x;
	view_norm[1] = vpn.y;
	view_norm[2] = vpn.z;

	/* query DIGS for the view reference point for this transformation */
	givref3(xform, &vref);
	ref_pt[0] = vref.x;
	ref_pt[1] = vref.y;
	ref_pt[2] = vref.z;

	um_nptpln(cc,ref_pt,view_norm,vpcc);
	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION     : int uv_projcpln(cc,cpcc,xform)
**      Project a cartesian model  coordinate onto the construction plane
**      along the view plane normal.
**    PARAMETERS   
**       INPUT  : 
**				cc						Cartesian model  coordinate
**				xform					norm transformation number
**       OUTPUT :  
**				cpcc					Cartesian  coordinate on construction plane
**    RETURNS      : 
**			UU_SUCCESS	if valid viewport and view normal is not perpendicular
**							to construction plane normal;
**			UU_FAILURE	otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uv_projcpln(cc,cpcc, xform)
	UM_coord cc;
	UM_coord cpcc;
	int		 xform;

	{
	int status;
	UM_vector	view_norm;				/* view normal of vport				*/
	int nint;								/* number of intersection points */
	Gwpoint3		vpn;

	uu_denter(UU_MTRC,(us,"uv_projcpln(cc=(%f,%f,%f),xform=%d)",
		cc[0],cc[1],cc[2],xform));

	/* query DIGS for the view plane normal for this transformation */
	givpn3(xform, &vpn);
	view_norm[0] = vpn.x;
	view_norm[1] = vpn.y;
	view_norm[2] = vpn.z;

	if (um_vcperp(view_norm, UM_cpln.zaxis)) 
		{
		um_vctovc(UM_zerovec, cpcc);
		status = UU_FAILURE;
		}
	else
		{
		um_ilnpln(cc,view_norm,UM_cpln.origin,UM_cpln.zaxis,&nint,cpcc);
		status = UU_SUCCESS;
		}
	uu_dexit;
	return (status);
	}
/*********************************************************************
**    E_FUNCTION     : int uv_cpln_parallel_vpln(xform)
**			Determine if the current construction plane is parallel to 
**			the view plane.
**    PARAMETERS   
**       INPUT  : 
**          xform			DIGS normtran for a view
**       OUTPUT :  
**          none
**    RETURNS      : 
**			UU_SUCCESS iff construction plane is parallel to view plane 
**			UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uv_cpln_parallel_vpln(xform)
	int xform;

	{
	int status;
	UM_vector	view_norm;
	Gwpoint3		vpn;

	uu_denter(UU_MTRC,(us,"uv_cpln_vs_vpln(xform=%d)",xform));

	/* query DIGS for the view plane normal for this transformation */
	givpn3(xform, &vpn);
	view_norm[0] = vpn.x;
	view_norm[1] = vpn.y;
	view_norm[2] = vpn.z;

	if (um_vcparall(view_norm, UM_cpln.zaxis)) 
		status = UU_SUCCESS;
	else
		status = UU_FAILURE;
	uu_dexit;
	return (status);
	}

/*********************************************************************
**    E_FUNCTION     : uv_symbol_draging_view_info(vrefpt)
**			Return the view reference point for use in draging a symbol
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          vrefpt					view reference point
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     :
**			This routine uses the view reference point in viewport 0
**			of the currently active screen.
*********************************************************************/
uv_symbol_draging_view_info(vrefpt)
	UM_coord vrefpt;

	{
	UV_vport vport;
	UV_view view;

	uu_denter(UU_MTRC,(us,"uv_symbol_draging_view_info()"));

	uv_getvpid(UV_act_screen[0].vports[0], &vport);
	uv_getvid(vport.cur_view, &view);
	um_vctovc(view.cur_ref_pt, vrefpt);

	uu_dexit;
	}

