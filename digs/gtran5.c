/*********************************************************************
**    NAME         :  gtran5.c -- transformation stuff.
**       CONTAINS:
**		Gerror gsviewmapping(xform,window,vport,type,prp) -- set view mapping.
**		Gfloat ug_ndclen(dx,dy,dz,cxform) -- convert WC length to NDC.
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       gtran5.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:26
*********************************************************************/
#include "zsysdep.h"
#include <stdio.h>
#include "umath.h"
#include "g.h"
#include "gdidd.h"
#include "udebug.h"

#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) gtran5.c 3.3 7/20/88 15:49:05 single"};
#else
static char uu_sccsident[]={"@(#) gtran5.c 3.3 7/20/88 15:49:05 double"};
#endif

/*********************************************************************
**    E_FUNCTION :  Gerror gsviewmapping(xform,window,vport,type,prp) 
**						set view mapping.
**    PARAMETERS   
**       INPUT  : 	int xform; -- which normtran to set.
**							Gwrect3 *window; -- window (UVN coords).
**							Gnrect3 *vport; -- viewport (ndc coords).
**							Gvtype	projtype; -- UG_PARALLEL or UG_PERSPECTIVE.
**							Gwpoint3 *prp; -- perspective reference point(UVN coords).
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
Gerror gsviewmapping(xform,window,vport,projtype,prp) 
int xform; 						/* which normtran to set. */
Gwrect3 *window; 				/* window (UVN coords). */
Gnrect3 *vport; 				/* viewport (ndc coords). */
Gvtype projtype; 				/* UG_PARALLEL or UG_PERSPECTIVE. */
Gwpoint3 *prp; 				/* perspective reference point(UVN coords) */
/* First, the UVN coordinate system is defined as follows: the current
	view reference point is the origin of UVN. The +N axis aligns with
	the current view plane normal. The +V axis aligns with the projection
	of the current view up vector in the UV plane.
	   A window volume is formed from the window parameter and the projectors
	used in the selected projection type (projtype).  For a PARALLEL projection,
	the parallel projectors result in a window volume which is a rectangular
	parallelopiped. The converging projectors of a PERSPECTIVE view result in a
	window volume which is a truncated frustrum. 
	    In a PARALLEL view, the projection reference point and the center of the
	window form a direction vector to which all projectors are parallel.
	An oblique view results when the U and V components of the prp are not
	the same as the center of the window. In a PERSPECTIVE view, the
	prp is the point where all projectors converge. A non-centered view
	(such as might be used for stereo presentation) is achieved by
	specifying the window such the center of the window is
	not at (0,0) on the UV plane.
	    The 3D viewport defines a rectangular parallelopiped in NDC space
	into which the 3D window volume is mapped such that the front (back) plane 
	of the window volume coincides with the front (back) plane of the viewport*/
{
	struct  { int op; Gws ws;
		int xform;
		Gwrect3 window;
		Gnrect3 vport;
		Gvtype vtype;
		Gwpoint3 prp;
	} prms;						/* for UG_DVMAP workstation entry */
		
	uu_denter(UU_GTRC,(us,"gsviewmapping(%d window=%g %g %g %g %g %g",
		xform,(*window).llf.x,(*window).llf.y,(*window).llf.z,
		(*window).urb.x,(*window).urb.y,(*window).urb.z));
	uu_dprint(UU_GTRC,(us,"       vport=%g %g %g %g %g %g, %d, prp=%g %g %g",
		(*vport).llf.x,(*vport).llf.y,(*vport).llf.z,
		(*vport).urb.x,(*vport).urb.y,(*vport).urb.z,projtype,
		(*prp).x,(*prp).y,(*prp).z));
	zbytecp(ug_gksstli.vtran[xform].window,*window);
	zbytecp(ug_gksstli.vtran[xform].vport,*vport);
	ug_gksstli.vtran[xform].vtype=projtype;
	zbytecp(ug_gksstli.vtran[xform].prp,*prp);
	ug_winviw(xform);
	/* delete segment ndc bounding boxes for those segs using this normtran */
	ug_ndcntranboxdel(xform);	
	prms.op=UG_DVMAP;
	prms.xform=xform;
	zbytecp(prms.window, *window);
	zbytecp(prms.vport, *vport);
	prms.vtype= projtype;
	zbytecp(prms.prp, *prp);
	ug_wkout (&prms,(sizeof(prms)+sizeof(int)-1)/sizeof(int));
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  Gfloat ug_ndclen(wlen,cxform)--convert WC length to NDC.
**			Convert a world coord len to ndc len.
**    PARAMETERS   
**       INPUT  :  Gfloat dx,dy,dz -- world coordinate vector whose length is to
**							be converted to NDC.
**						 Gtran cxform -- world to NDC transformation matrix.
**       OUTPUT :  
**    RETURNS      : NDC length of wlen.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
Gfloat ug_ndclen(vx,vy,vz,cxform)		/* convert a world coord vector length 
														to ndc len */
Gfloat vx,vy,vz;					/* world coordinate vector */
Gtran cxform;						/* world to NDC xformation */
{
	Gfloat nlen;					/* ndc length */
	Gfloat dx,dy,dz;
	Gnpoint3 npos1,npos2;
	char us[80];

	ug_xform(vx,vy,vz,&npos1,cxform);
	ug_xform((UU_REAL) 0.0,(UU_REAL) 0.0,(UU_REAL) 0.0,&npos2,cxform);

	/* now get dist between npos1 and npos2 */
	dx=npos1.x-npos2.x;
	dy=npos1.y-npos2.y;
	dz=npos1.z-npos2.z;
	nlen=sqrt(dx*dx+dy*dy+dz*dz);

	uu_denter2(UU_GITRC,(us,"%g=ug_ndclen(%g %g %g,cxform)",nlen,vx,vy,vz));
	uu_dexit;

	return(nlen);
}
