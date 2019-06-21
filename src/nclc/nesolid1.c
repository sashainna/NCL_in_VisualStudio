/*********************************************************************
**    NAME         :  nesolid1.c
**       CONTAINS:
**             ncl_xform_solid()
**             ncl_solid_calc_lists()
**             ncl_solid_cutter_lists()
**             ncl_calc_cyl_list()
**             ncl_calc_sphere_list()
**             ncl_calc_torus_list()
**    COPYRIGHT 2008 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       nesolid1.c , 25.2
**    DATE AND TIME OF LAST MODIFICATION
**       08/17/15 , 17:53:53
*********************************************************************/

#include "usysdef.h"
#include "lipv.h"
#include "umath.h"
#include "ulist.h"
#include "uhep.h"
#include "udebug.h"
#include "umoveb.h"
#include "class.h"
#include "mattr.h"
#include "mdcoord.h"
#include "mdgenent.h"
#include "mdpick.h"
#include "mcrv.h"
#include "msrf.h"
#include "msol.h"
#include	"mfort.h"
#include	"mdrel.h"
#include	"mdcpln.h"
#include	"mgeom.h"
#include	"nccs.h"
#include	"ncl.h"
#include	"nclfc.h"

static void S_orient_pts();
static void S_calc_tangents();

/*********************************************************************
**    E_FUNCTION :  ncl_xform_solid(type,parms,tf,store)
**       Transforms Visual Solid/NCLIPV stock data.
**    PARAMETERS   
**       INPUT  : 
**          type     Type of solid/stock to transforam.
**          parms    Solid/stock canonical data.
**          tf       Transformation matrix.
**          store    UU_TRUE = transformed entity is to be stored in
**                   Unibase.  Used for Composite Solids only.
**       OUTPUT :  
**          parms    Updated parameters.
**    RETURNS      : UU_SUCCESS if all goes well.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_xform_solid(type,parms,tf,store)
UU_REAL **parms;
UM_transf tf;
UU_LOGICAL store;
{
	int i,np,status;
	UU_REAL *p,*ptr;
	UM_coord *pts;
	UM_vector *vcs;
	UU_KEY_ID *keys;
	struct UM_srfdatabag e1;
	union {UU_REAL rval; int ival[2];} tprm;
/*
.....Initialize routine
*/
	ptr = *parms;
	status = UU_SUCCESS;
	switch (type)
	{
/*
.....Box solid
*/
	case UM_BOX_SOLID:
		um_cctmtf(&ptr[0],tf,&ptr[0]);
		um_vctmtf(&ptr[3],tf,&ptr[3]);
		um_vctmtf(&ptr[6],tf,&ptr[6]);
		um_vctmtf(&ptr[9],tf,&ptr[9]);
		np = 0;
		break;
/*
.....Extruded solid
*/
	case UM_EXTRUDED_SOLID:
		np = ptr[3];
		pts = (UM_coord *)&ptr[4];
		um_vctmtf(&ptr[0],tf,&ptr[0]);
		break;
/*
.....Cone, Cylinder solid
*/
	case UM_CONE_SOLID:
	case UM_CYLINDER_SOLID:
		um_cctmtf(&ptr[0],tf,&ptr[0]);
		um_vctmtf(&ptr[3],tf,&ptr[3]);
		np = 0;
		break;
/*
.....Sphere solid
*/
	case UM_SPHERE_SOLID:
		um_cctmtf(&ptr[0],tf,&ptr[0]);
		np = 0;
		break;
/*
.....Torus solid
*/
	case UM_TORUS_SOLID:
		um_cctmtf(&ptr[0],tf,&ptr[0]);
		um_vctmtf(&ptr[3],tf,&ptr[3]);
		np = 0;
		break;
/*
.....Revolved solid
*/
	case UM_REVOLVED_SOLID:
		np = ptr[8];
		pts = (UM_coord *)&ptr[9];
		um_cctmtf(&ptr[0],tf,&ptr[0]);
		um_vctmtf(&ptr[3],tf,&ptr[3]);
		vcs = (UM_vector *)&ptr[9+np*3];
		for (i=0;i<np;i++) um_vctmtf(vcs[i],tf,vcs[i]);
		break;
/*
.....Contour solid
*/
	case UM_CONTOUR_SOLID:
		np = ptr[5];
		pts = (UM_coord *)&ptr[6];
		um_vctmtf(&ptr[2],tf,&ptr[2]);
		break;
/*
.....STL solid
*/
	case UM_STL_SOLID:
		tprm.rval = ptr[1];
/*		if (tprm.ival[0] == 1) um_tftmtf(&ptr[2],tf,&ptr[2]);
		else um_tftotf(tf,&ptr[2]);*/
		um_tftotf(tf,&ptr[2]);
		tprm.ival[0] = 2; ptr[1] = tprm.rval;
		np = 0;
		break;
/*
.....Composite solid
*/
	case UM_COMPOS_SOLID:
		np = 0;
		keys = (UU_KEY_ID *)ptr;
		for (i=1;i<=keys[0];i++)
		{
			e1.key = keys[i];
			ncl_retrieve_data(&e1,sizeof(e1));
			status = uc_transform(&e1,tf,store);
			if (status != UU_SUCCESS) break;
		}
		break;
/*
.....Unknown stock
.....Do not transform
*/
	default:
		status = UU_FAILURE;
		np = 0;
	}
/*
......Transform points
*/
	for (i=0;i<np;i++) um_cctmtf(pts[i],tf,pts[i]);
	return(status);
}

/*********************************************************************
**    E_FUNCTION :  ncl_solid_calc_lists(solid,params,nparms,dlist,tess)
**       Calculates the display and tessallation lists for a visual
**       solid entity along with the bounding box.
**    PARAMETERS   
**       INPUT  : 
**          solid    Solid to calculate lists for.
**          params   Solid canonical data.
**          nparms   Number of reals in 'param'.
**       OUTPUT :  
**          solid    Contains the bounding box of the solid.
**          dlist    Display list.
**          tess     Tessellation list.
**    RETURNS      : UX_NO_ACCESS on file error, UU_FAILURE on other failures,
**                   otherwise UU_SUCCESS.
**    SIDE EFFECTS :
**       The lists 'dlist' and 'tess' are created in this routine
**       but must be freed in the calling routine.
**    WARNINGS     : none
*********************************************************************/
int ncl_solid_calc_lists(solid,params,nparms,dlist,tess)
struct UM_solid_rec *solid;
UU_REAL params[];
int nparms;
UU_LIST *dlist;
UM_tessellation *tess;
{
	int status,units,bin,xf,optimize,nsrf;
	char *cstr;
	union {UU_REAL rval; int ival[2];} tprm;

	switch (solid->type)
	{
/*
.....Box solid
*/
	case UM_BOX_SOLID:
		status = S_calc_box_list(solid,params,dlist,tess);
		break;
/*
.....Cone, Cylinder solid
*/
	case UM_CONE_SOLID:
	case UM_CYLINDER_SOLID:
		status = ncl_calc_cyl_list(solid,params,1,dlist,tess,UU_NULL);
		break;
/*
.....Torus solid
*/
	case UM_TORUS_SOLID:
		status = ncl_calc_torus_list(solid,params,1,dlist,tess,UU_NULL);
		break;
/*
.....Sphere solid
*/
	case UM_SPHERE_SOLID:
		status = ncl_calc_sphere_list(solid,params,1,dlist,tess,UU_NULL);
		break;
/*
.....Extruded solid
*/
	case UM_EXTRUDED_SOLID:
		status = S_calc_extrude_list(solid,params,dlist,tess);
		break;
/*
.....Contour solid
*/
	case UM_CONTOUR_SOLID:
		status = S_calc_contour_list(solid,params,dlist,tess);
		break;
/*
.....Revolved solid
*/
	case UM_REVOLVED_SOLID:
		status = S_calc_revolve_list(solid,params,dlist,tess);
		break;
/*
.....STL solid
*/
	case UM_STL_SOLID:
		tprm.rval = params[0];
		units = tprm.ival[0];
		bin = tprm.ival[1];
		tprm.rval = params[1];
		xf = tprm.ival[0];
		optimize = tprm.ival[1];
		cstr = (char *)&params[14];
		status = ncl_load_stl(solid,cstr,units,&bin,&xf,optimize,&params[2],
			dlist,tess);
		tprm.ival[0] = units;
		tprm.ival[1] = bin;
		params[0] = tprm.rval;
		tprm.rval = params[1];
		tprm.ival[0] = xf;
		params[1] = tprm.rval;
		break;
/*
.....Composite solid
*/
	case UM_COMPOS_SOLID:
		status = S_calc_compos_list(solid,params,dlist,tess);
		break;
/*
.....Unrecognized solid
*/
	default:
		status = UU_FAILURE;
		break;
	}
/*
.....End of routine
*/
	return(status);
}

/*********************************************************************
**    E_FUNCTION :  ncl_solid_cutter_lists(solid,pvo,ptlist,vclist,npts)
**       Calculates the curve points and normal vectors used for defining
**       cutter parts using a solid entity.
**    PARAMETERS   
**       INPUT  : 
**          solid    Solid to calculate lists for.
**       OUTPUT :  
**          pvo      Contains the origin point and axis vector of the
**                   solid.
**          ptlist   Point list of generating curve.
**          vclist   Tangent vector list of generating curve.
**          npts     Number of points returned, 0 if an error occured.
**    RETURNS      : none
**    SIDE EFFECTS :
**       The lists 'ptlist' and 'vclist' are created in this routine
**       but must be freed in the calling routine.
**    WARNINGS     : none
*********************************************************************/
void ncl_solid_cutter_lists(solid,pvo,ptlist,vclist,npts)
struct UM_solid_rec *solid;
UM_covec pvo;
UU_LIST *ptlist,*vclist;
int *npts;
{
	int status,i,np,nv;
	UM_coord *pts,pt0[5],pto;
	UM_vector *vcs,vcn,vc0[5];
/*
.....Initialize routine
*/
	*npts = 0;
	np = 0;
	nv = 0;
	uu_list_init0(vclist);
	status = UU_SUCCESS;
/*
.....Initialize origin & axis
*/
	pvo[0] = pvo[1] = pvo[2] = 0.;
	pvo[3] = 0.; pvo[4] = 1.; pvo[5] = 0.;
	switch (solid->type)
	{
/*
.....Box solid
*/
	case UM_BOX_SOLID:
		um_vctovc(&solid->sdata[0],pt0[0]);
		um_vcplvc(pt0[0],&solid->sdata[3],pt0[1]);
		um_vcplvc(pt0[1],&solid->sdata[6],pt0[2]);
		um_vcmnvc(pt0[2],&solid->sdata[3],pt0[3]);
		um_vctovc(pt0[0],pt0[4]);

		um_unitvc(&solid->sdata[3],vc0[0]);
		um_unitvc(&solid->sdata[6],vc0[1]);
		um_vctmsc(&solid->sdata[3],-1.,vc0[2]);
		um_unitvc(vc0[2],vc0[2]);
		um_vctmsc(&solid->sdata[6],-1.,vc0[3]);
		um_unitvc(vc0[3],vc0[3]);
		um_vctovc(vc0[3],vc0[4]);

		pts = pt0;
		vcs = vc0;
		np = 5;
		nv = 5;

		um_unitvc(&solid->sdata[9],&pvo[3]);
		break;
/*
.....Cone, Cylinder solid
*/
	case UM_CONE_SOLID:
	case UM_CYLINDER_SOLID:
		status = ncl_calc_cyl_list(solid,solid->sdata,3,ptlist,vclist,UU_NULL);
		um_unitvc(&solid->sdata[3],&pvo[3]);
		break;
/*
.....Torus solid
*/
	case UM_TORUS_SOLID:
		status = ncl_calc_torus_list(solid,solid->sdata,3,ptlist,vclist,UU_NULL);
		um_unitvc(&solid->sdata[3],&pvo[3]);
		break;
/*
.....Sphere solid
*/
	case UM_SPHERE_SOLID:
		status = ncl_calc_sphere_list(solid,solid->sdata,3,ptlist,vclist,UU_NULL);
		pts = (UM_coord *)UU_LIST_ARRAY(ptlist);
		um_vctovc(pts[0],pvo);
		break;
/*
.....Extruded solid
*/
	case UM_EXTRUDED_SOLID:
		pvo[0] = pvo[1] = pvo[2] = 0.;
		um_vctovc(&solid->sdata[0],&pvo[3]);
		np = solid->sdata[3];
		pts = (UM_coord *)&solid->sdata[4];
		S_calc_tangents(pts,vclist,np);
		break;
/*
.....Contour solid
*/
	case UM_CONTOUR_SOLID:
		um_vctovc(&solid->sdata[2],&pvo[3]);
		np = solid->sdata[5];
		pts = (UM_coord *)&solid->sdata[6];
		S_calc_tangents(pts,vclist,np);
		break;
/*
.....Revolved solid
*/
	case UM_REVOLVED_SOLID:
		np = nv = solid->sdata[8];
		pts = (UM_coord *)&solid->sdata[9];
		vcs = (UM_vector *)&solid->sdata[9+np*3];
		um_unitvc(&solid->sdata[3],&pvo[3]);
		break;
/*
.....Unrecognized solid
*/
	default:
		status = UU_FAILURE;
		break;
	}
/*
.....Store curve points and vectors
*/
	if (np != 0)
	{
		uu_list_init(ptlist,sizeof(UM_coord),np,10);
		if (nv != 0) uu_list_init(vclist,sizeof(UM_vector),nv,10);
		for (i=0;i<np;i++)
		{
			ncl_wcstomcs(0,&pts[i],pto);
			uu_list_push(ptlist,pto);
			if (nv != 0)
			{
				ncl_mcstowcs(1,&vcs[i],pto);
				uu_list_push(vclist,pto);
			}
		}
	}
/*
.....End of routine
*/
	if (status == UU_SUCCESS) *npts = UU_LIST_LENGTH(ptlist);
	return;
}

/*********************************************************************
**    E_FUNCTION :  ncl_calc_cyl_list(solid,params,flag,dlist,tess,sdata)
**       Calculates the display and tessallation lists for a CYLINDER or
**       CONE visual solid entity along with the bounding box.
**    PARAMETERS   
**       INPUT  : 
**          solid    Solid to calculate lists for.
**          params   Solid canonical data.
**          flag     1 = Calculate display lists.
**                   2 = Calculate stock parameters.
**                   3 = Calculate cutter parameters.
**       OUTPUT :  
**          solid    Contains the bounding box of the solid.
**          dlist    Display list when flag = 1.
**          tess     Tessellation list when flag = 1.
**          sdata    Contains the stock parameters when flag = 2.
**    RETURNS      : UU_FAILURE on failure, otherwise UU_SUCCESS.
**    SIDE EFFECTS : none.
**    WARNINGS     : none
*********************************************************************/
int ncl_calc_cyl_list(solid,params,flag,dlist,tess,sdata)
struct UM_solid_rec *solid;
UU_REAL params[];
int flag;
UU_LIST *dlist;
UM_tessellation *tess;
UU_REAL **sdata;
{
	int i,n,status;
	UM_int2 idx;
	UU_REAL told,*p,d,r1;
	UM_coord pts[2],cpt;
	UM_vector nvec,svec,tvecs[2],tvc;
	UU_LIST *vlist;
/*
.....Calculate line to revolve
*/
	status = UU_SUCCESS;
	um_vctovc(params,cpt);
	um_unitvc(&params[3],nvec);
	um_perpvc(nvec,svec);
	um_unitvc(svec,svec);
	pts[0][0] = params[6]; pts[0][1] = pts[0][2] = 0.;
	r1 = params[6];
	if (solid->type == UM_CONE_SOLID) r1 = params[7];
	pts[1][0] = r1; pts[1][1] = um_mag(&params[3]); pts[1][2] = 0.;
/*
.....Check for valid geometry
*/
	if (pts[0][0] < 0. || pts[1][0] < 0. || fabs(pts[1][1]) < UM_FUZZ ||
		(pts[0][0] < UM_FUZZ && pts[1][0] < UM_FUZZ)) goto failed;
/*
.....Calculate tangent vectors
*/
	um_vcmnvc(pts[1],pts[0],tvc);
	um_unitvc(tvc,tvecs[0]);
	um_vctovc(tvecs[0],tvecs[1]);
/*
.....Calculate revolved solid
*/
	if (flag == 1)
	{
		idx = 175; getsc(&idx,&told);
		n = 2;
		status = S_revolved(solid,cpt,nvec,svec,0.,360.,pts,tvecs,n,told,dlist,
			tess);
	}
/*
.....Store stock parameters
*/
	else if (flag == 2)
	{
		p = (UU_REAL *)uu_malloc(sizeof(UU_REAL)*15);
		if (p == UU_NULL) goto failed;
		um_vctovc(cpt,p);
		um_vctovc(nvec,&p[3]);
		p[6] = 0.; p[7] = 360.;
		p[8] = 2;
		S_orient_pts(pts,2,cpt,nvec,svec,&p[9],&d);
		*sdata = p;
	}
/*
.....Store cutter parameters
*/
	else
	{
		vlist = (UU_LIST *)tess;
		uu_list_init(dlist,sizeof(UM_coord),2,2);
		uu_list_init(vlist,sizeof(UM_vector),2,2);
		for (i=0;i<2;i++)
		{
			uu_list_push(dlist,pts[i]);
			uu_list_push(vlist,tvecs[i]);
		}
	}
	goto done;
/*
.....Failed to create lists
*/
failed:;
	status = UU_FAILURE;
/*
.....End of routine
*/
done:;
	return(status);
}

/*********************************************************************
**    E_FUNCTION :  ncl_calc_sphere_list(solid,params,flag,dlist,tess,sdata)
**       Calculates the display and tessallation lists for a SPHERE
**       visual solid entity along with the bounding box.
**    PARAMETERS   
**       INPUT  : 
**          solid    Solid to calculate lists for.
**          params   Solid canonical data.
**          flag     1 = Calculate display lists.
**                   2 = Calculate stock parameters.
**                   3 = Calculate cutter parameters.
**       OUTPUT :  
**          solid    Contains the bounding box of the solid.
**          dlist    Display list when flag = 1.
**          tess     Tessellation list when flag = 1.
**          sdata    Contains the stock parameters when flag = 2.
**    RETURNS      : UU_FAILURE on failure, otherwise UU_SUCCESS.
**    SIDE EFFECTS : none.
**    WARNINGS     : none
*********************************************************************/
int ncl_calc_sphere_list(solid,params,flag,dlist,tess,sdata)
struct UM_solid_rec *solid;
UU_REAL params[];
int flag;
UU_LIST *dlist;
UM_tessellation *tess;
UU_REAL **sdata;
{
	int i,n,status;
	UM_int2 idx;
	UU_REAL told,*p,d;
	UM_coord *pts,cpt;
	UM_vector nvec,svec,*tvecs;
	UM_transf tfmat;
	UU_LIST plist,vlist,*vclist;
	struct UM_circle_rec c;
/*
.....Initialize routine
*/
	status = UU_SUCCESS;
	uu_list_init0(&plist);
	uu_list_init0(&vlist);
	if (params[3] < UM_FUZZ) goto failed;
/*
.....Calculate circle to revolve
*/
	idx = 175; getsc(&idx,&told);
	um_vctovc(&params[0],cpt);
	c.center[0] = c.center[1] = c.center[2] = 0.;
	nvec[0] = nvec[1] = 0.; nvec[2] = 1.;
	svec[0] = 1.; svec[1] = svec[2] = 0.;
	c.nvec[0] = c.nvec[1] = 0.; c.nvec[2] = 1.;
	c.svec[0] = 0.; c.svec[1] = -1.; c.svec[2] = 0.;
	c.radius = params[3];
	c.dang = UM_PI;
	c.rel_num = UM_CIRCLE_REL;
/*
.....Evolve circle
*/
	uu_list_init(&plist,sizeof(UM_coord),50,20);
	if (UU_LIST_NULLPTR(&plist)) goto failed;
	uu_list_init(&vlist,sizeof(UM_coord),50,20);
	if (UU_LIST_NULLPTR(&vlist)) goto failed;
	if (solid->key != 0)
	{
		status = uc_retrieve_transf(solid->key,tfmat);
		if (status != UU_SUCCESS) goto done;
	}
	else
		um_identtf(tfmat);
	n = ncl_evolve_circ(&c,tfmat,told,&plist,&vlist,UU_NULL);
	if (n == 0) goto failed;
	pts = (UM_coord *)UU_LIST_ARRAY(&plist);
	tvecs = (UM_vector *)UU_LIST_ARRAY(&vlist);
/*
.....Calculate revolved solid
*/
	if (flag == 1)
	{
		status = S_revolved(solid,cpt,nvec,svec,0.,360.,pts,tvecs,n,told,dlist,
			tess);
	}
/*
.....Store stock parameters
*/
	else if (flag == 2)
	{
		p = (UU_REAL *)uu_malloc(sizeof(UU_REAL)*(9+n*3));
		if (p == UU_NULL) goto failed;
		um_vctovc(cpt,p);
		um_vctovc(nvec,&p[3]);
		p[6] = 0.; p[7] = 360.;
		p[8] = n;
		S_orient_pts(pts,n,cpt,nvec,svec,&p[9],&d);
		*sdata = p;
	}
/*
.....Store cutter parameters
*/
	else
	{
		vclist = (UU_LIST *)tess;
		uu_list_init(dlist,sizeof(UM_coord),n,2);
		uu_list_init(vclist,sizeof(UM_vector),n,2);
		for (i=0;i<n;i++)
		{
			uu_list_push(dlist,pts[i]);
			uu_list_push(vclist,tvecs[i]);
		}
	}
	goto done;
/*
.....Failed to create lists
*/
failed:;
	status = UU_FAILURE;
/*
.....End of routine
*/
done:;
	if (!UU_LIST_NULLPTR(&plist)) uu_list_free(&plist);
	if (!UU_LIST_NULLPTR(&vlist)) uu_list_free(&vlist);
	return(status);
}

/*********************************************************************
**    E_FUNCTION :  ncl_calc_torus_list(solid,params,flag,dlist,tess,sdata)
**       Calculates the display and tessallation lists for a TORUS
**       visual solid entity along with the bounding box.
**    PARAMETERS   
**       INPUT  : 
**          solid    Solid to calculate lists for.
**          params   Solid canonical data.
**          flag     1 = Calculate display lists.
**                   2 = Calculate stock parameters.
**                   3 = Calculate cutter parameters.
**       OUTPUT :  
**          solid    Contains the bounding box of the solid.
**          dlist    Display list when flag = 1.
**          tess     Tessellation list when flag = 1.
**          sdata    Contains the stock parameters when flag = 2.
**    RETURNS      : UU_FAILURE on failure, otherwise UU_SUCCESS.
**    SIDE EFFECTS : none.
**    WARNINGS     : none
*********************************************************************/
int ncl_calc_torus_list(solid,params,flag,dlist,tess,sdata)
struct UM_solid_rec *solid;
UU_REAL params[];
int flag;
UU_LIST *dlist;
UM_tessellation *tess;
UU_REAL **sdata;
{
	int i,n,status;
	UM_int2 idx;
	UU_REAL told,*p,d;
	UM_coord *pts,cpt;
	UM_vector nvec,svec,*tvecs;
	UM_transf tfmat;
	UU_LIST plist,vlist,*vclist;
	struct UM_circle_rec c;
/*
.....Initialize routine
*/
	status = UU_SUCCESS;
	uu_list_init0(&plist);
	uu_list_init0(&vlist);
/*
.....Check for valid geometry
*/
	if (params[6] < UM_FUZZ || params[7] < UM_FUZZ) goto failed;
	if (params[6] < params[7]) goto failed;
	if (um_mag(&params[3]) < UM_FUZZ) goto failed;
/*
.....Calculate circle to revolve
*/
	idx = 175; getsc(&idx,&told);
	um_vctovc(&params[0],cpt);
	um_unitvc(&params[3],nvec);
	um_perpvc(nvec,svec);
	um_unitvc(svec,svec);
	c.nvec[0] = c.nvec[1] = 0.; c.nvec[2] = 1.;
	c.svec[0] = 1.; c.svec[1] = c.svec[2] = 0.;
	c.radius = params[7];
	um_vctmsc(c.svec,params[6],c.center);
	c.dang = UM_TWOPI;
	c.rel_num = UM_CIRCLE_REL;
/*
.....Evolve circle
*/
	uu_list_init(&plist,sizeof(UM_coord),50,20);
	if (UU_LIST_NULLPTR(&plist)) goto failed;
	uu_list_init(&vlist,sizeof(UM_coord),50,20);
	if (UU_LIST_NULLPTR(&vlist)) goto failed;
	if (solid->key != 0)
	{
		status = uc_retrieve_transf(solid->key,tfmat);
		if (status != UU_SUCCESS) goto done;
	}
	else
		um_identtf(tfmat);
	n = ncl_evolve_circ(&c,tfmat,told,&plist,&vlist,UU_NULL);
	if (n == 0) goto failed;
	pts = (UM_coord *)UU_LIST_ARRAY(&plist);
	tvecs = (UM_vector *)UU_LIST_ARRAY(&vlist);
/*
.....Calculate revolved solid
*/
	if (flag == 1)
	{
		status = S_revolved(solid,cpt,nvec,svec,0.,360.,pts,tvecs,n,told,dlist,
			tess);
	}
/*
.....Store stock parameters
*/
	else if (flag == 2)
	{
		p = (UU_REAL *)uu_malloc(sizeof(UU_REAL)*(9+n*3));
		if (p == UU_NULL) goto failed;
		um_vctovc(cpt,p);
		um_vctovc(nvec,&p[3]);
		p[6] = 0.; p[7] = 360.;
		p[8] = n;
		S_orient_pts(pts,n,cpt,nvec,svec,&p[9],&d);
		*sdata = p;
	}
/*
.....Store cutter parameters
*/
	else
	{
		vclist = (UU_LIST *)tess;
		uu_list_init(dlist,sizeof(UM_coord),n,2);
		uu_list_init(vclist,sizeof(UM_vector),n,2);
		for (i=0;i<n;i++)
		{
			uu_list_push(dlist,pts[i]);
			uu_list_push(vclist,tvecs[i]);
		}
	}
	goto done;
/*
.....Failed to create lists
*/
failed:;
	status = UU_FAILURE;
/*
.....End of routine
*/
done:;
	if (!UU_LIST_NULLPTR(&plist)) uu_list_free(&plist);
	if (!UU_LIST_NULLPTR(&vlist)) uu_list_free(&vlist);
	return(status);
}

/*********************************************************************
**    I_FUNCTION :  S_calc_box_list(solid,params,dlist,tess)
**       Calculates the display and tessallation lists for a BOX visual
**       solid entity along with the bounding box.
**    PARAMETERS   
**       INPUT  : 
**          solid    Solid to calculate lists for.
**          params   Solid canonical data.
**       OUTPUT :  
**          solid    Contains the bounding box of the solid.
**          dlist    Display list.
**          tess     Tessellation list.
**    RETURNS      : UU_FAILURE on failure, otherwise UU_SUCCESS.
**    SIDE EFFECTS : none.
**    WARNINGS     : none
*********************************************************************/
static int S_calc_box_list(solid,params,dlist,tess)
UU_REAL params[];
struct UM_solid_rec *solid;
UU_LIST *dlist;
UM_tessellation *tess;
{
	int i,k,nv,nvp,status;
	UU_REAL ang;
	UM_coord pt0[5],pt1[5],ptx;
	UM_vector vcn,xaxis,yaxis,zaxis;
	UM_tript tript;
/*
.....Initialize display list
*/
	uu_list_init(dlist,sizeof(UM_coord),25,12);
	if (dlist->data == UU_NULL) goto failed;
/*
.....Calculate sides of box
*/
	nv = 0;
	nvp = 0;
	if (um_mag(&params[3]) < UM_FUZZ)
	{
		nv++;
		nvp = 1;
	}
	if (um_mag(&params[6]) < UM_FUZZ)
	{
		nv++;
		nvp = 2;
	}
	if (um_mag(&params[9]) < UM_FUZZ)
	{
		nv++;
		nvp = 3;
	}
	if (nv > 1) goto failed;
/*
.....Make sure axes follow right hand rule
*/
	um_vctovc(&params[0],pt0[0]);
	um_vctovc(&params[3],xaxis);
	um_vctovc(&params[6],yaxis);
	um_vctovc(&params[9],zaxis);
	if (nv == 0)
	{
		ang = um_angle2p(xaxis,yaxis,zaxis);
		if (ang > UM_PI)
		{
			um_vcplvc(pt0[0],yaxis,pt0[0]);
			um_negvc(yaxis,yaxis);
		}
	}

	um_vcplvc(pt0[0],zaxis,pt0[1]);
	um_vcplvc(pt0[1],yaxis,pt0[2]);
	um_vcmnvc(pt0[2],zaxis,pt0[3]);
	um_vctovc(pt0[0],pt0[4]);

	ncl_init_box(pt0[0],solid->box);
	for (i=0;i<5;i++)
	{
		um_vcplvc(pt0[i],xaxis,pt1[i]);
		ncl_update_box(pt0[i],solid->box);
		ncl_update_box(pt1[i],solid->box);
	}
/*
.....Store box in display list
........3-D Box
*/
	if (nv == 0)
	{
		ptx[0] = 6; ptx[1] = 0; ptx[2] = 0;
		uu_list_push(dlist,ptx);
		ptx[0] = 5; ptx[1] = 1;
		uu_list_push(dlist,ptx);
		for (i=0;i<5;i++) uu_list_push(dlist,pt0[i]);
		uu_list_push(dlist,ptx);
		for (i=0;i<5;i++) uu_list_push(dlist,pt1[i]);
		ptx[0] = 2;
		for (i=0;i<4;i++)
		{
			uu_list_push(dlist,ptx);
			uu_list_push(dlist,pt0[i]);
			uu_list_push(dlist,pt1[i]);
		}
	}
/*
........2-D Box
*/
	else
	{
		ptx[0] = 1; ptx[1] = 0; ptx[2] = 0;
		uu_list_push(dlist,ptx);
		ptx[0] = 5;
		ptx[1] = 1;
		uu_list_push(dlist,ptx);
		if (nvp == 1)
		{
			for (i=0;i<5;i++) uu_list_push(dlist,pt0[i]);
		}
		else if (nvp == 2)
		{
			uu_list_push(dlist,pt0[0]);
			uu_list_push(dlist,pt0[1]);
			uu_list_push(dlist,pt1[1]);
			uu_list_push(dlist,pt1[0]);
			uu_list_push(dlist,pt0[0]);
		}
		else
		{
			uu_list_push(dlist,pt0[0]);
			uu_list_push(dlist,pt1[0]);
			uu_list_push(dlist,pt1[3]);
			uu_list_push(dlist,pt0[3]);
			uu_list_push(dlist,pt0[0]);
		}
	}
/*
.....Initialize tessellation structure
*/
	tess->toler = 0.;
	status = uu_list_init1(&tess->vertices,sizeof(UM_coord),24,4);
	if (status != UU_SUCCESS) goto done;
	status = uu_list_init1(&tess->normals,sizeof(UM_coord),24,4);
	if (status != UU_SUCCESS) goto done;
	status = uu_list_init1(&tess->tri,sizeof(UU_REAL)*3,12,4);
	if (status != UU_SUCCESS) goto done;
/*
.....3-D Box
........Calculate vertices and normals
*/
	if (nv == 0)
	{
		tess->np = 24;
		uu_list_push(&tess->vertices,pt0[1]);
		uu_list_push(&tess->vertices,pt1[1]);
		uu_list_push(&tess->vertices,pt1[2]);
		uu_list_push(&tess->vertices,pt0[2]);
		um_unitvc(zaxis,vcn);
		for (i=0;i<4;i++) uu_list_push(&tess->normals,vcn);

		uu_list_push(&tess->vertices,pt0[2]);
		uu_list_push(&tess->vertices,pt1[2]);
		uu_list_push(&tess->vertices,pt1[3]);
		uu_list_push(&tess->vertices,pt0[3]);
		um_unitvc(yaxis,vcn);
		for (i=0;i<4;i++) uu_list_push(&tess->normals,vcn);

		uu_list_push(&tess->vertices,pt0[3]);
		uu_list_push(&tess->vertices,pt1[3]);
		uu_list_push(&tess->vertices,pt1[0]);
		uu_list_push(&tess->vertices,pt0[0]);
		um_negvc(zaxis,vcn);
		um_unitvc(vcn,vcn);
		for (i=0;i<4;i++) uu_list_push(&tess->normals,vcn);

		uu_list_push(&tess->vertices,pt0[0]);
		uu_list_push(&tess->vertices,pt1[0]);
		uu_list_push(&tess->vertices,pt1[1]);
		uu_list_push(&tess->vertices,pt0[1]);
		um_negvc(yaxis,vcn);
		um_unitvc(vcn,vcn);
		for (i=0;i<4;i++) uu_list_push(&tess->normals,vcn);

		uu_list_push(&tess->vertices,pt0[0]);
		uu_list_push(&tess->vertices,pt0[1]);
		uu_list_push(&tess->vertices,pt0[2]);
		uu_list_push(&tess->vertices,pt0[3]);
		um_negvc(xaxis,vcn);
		um_unitvc(vcn,vcn);
		for (i=0;i<4;i++) uu_list_push(&tess->normals,vcn);

		uu_list_push(&tess->vertices,pt1[3]);
		uu_list_push(&tess->vertices,pt1[2]);
		uu_list_push(&tess->vertices,pt1[1]);
		uu_list_push(&tess->vertices,pt1[0]);
		um_unitvc(xaxis,vcn);
		for (i=0;i<4;i++) uu_list_push(&tess->normals,vcn);
/*
........Store triangle list
*/
		tess->ntri = 12;
		for (i=0;i<6;i++)
		{
			tript.n1 = i*4;
			tript.n2 = i*4 + 1;
			tript.n3 = i*4 + 2; 
			uu_list_push(&tess->tri,&tript);
			tript.n1 = i*4 + 2;
			tript.n2 = i*4 + 3;
			tript.n3 = i*4; 
			uu_list_push(&tess->tri,&tript);
		}
	}
/*
.....2-D Box
........Calculate vertices and normals
*/
	else
	{
		tess->np = 4;
		if (nvp == 1)
		{
			uu_list_push(&tess->vertices,pt0[0]);
			uu_list_push(&tess->vertices,pt0[1]);
			uu_list_push(&tess->vertices,pt0[2]);
			uu_list_push(&tess->vertices,pt0[3]);
			um_negvc(xaxis,vcn);
			um_unitvc(vcn,vcn);
			for (i=0;i<4;i++) uu_list_push(&tess->normals,vcn);
		}
		else if (nvp == 2)
		{
			uu_list_push(&tess->vertices,pt0[0]);
			uu_list_push(&tess->vertices,pt1[0]);
			uu_list_push(&tess->vertices,pt1[1]);
			uu_list_push(&tess->vertices,pt0[1]);
			um_negvc(yaxis,vcn);
			um_unitvc(vcn,vcn);
			for (i=0;i<4;i++) uu_list_push(&tess->normals,vcn);
		}
		else
		{
			uu_list_push(&tess->vertices,pt0[3]);
			uu_list_push(&tess->vertices,pt1[3]);
			uu_list_push(&tess->vertices,pt1[0]);
			uu_list_push(&tess->vertices,pt0[0]);
			um_negvc(zaxis,vcn);
			um_unitvc(vcn,vcn);
			for (i=0;i<4;i++) uu_list_push(&tess->normals,vcn);
		}
/*
........Store triangle list
*/
		tess->ntri = 2;
		tript.n1 = 0;
		tript.n2 = 1;
		tript.n3 = 2; 
		uu_list_push(&tess->tri,&tript);

		tript.n1 = 2;
		tript.n2 = 3;
		tript.n3 = 0; 
		uu_list_push(&tess->tri,&tript);
	}
	status = UU_SUCCESS;
	goto done;
/*
.....Failed to create lists
*/
failed:;
	status = UU_FAILURE;
/*
.....End of routine
*/
done:;
	return(status);
}

/*********************************************************************
**    I_FUNCTION :  S_calc_contour_list(solid,params,dlist,tess)
**       Calculates the display and tessallation lists for a CONTOUR
**       visual solid entity along with the bounding box.
**    PARAMETERS   
**       INPUT  : 
**          solid    Solid to calculate lists for.
**          params   Solid canonical data.
**       OUTPUT :  
**          solid    Contains the bounding box of the solid.
**          dlist    Display list.
**          tess     Tessellation list.
**    RETURNS      : UU_FAILURE on failure, otherwise UU_SUCCESS.
**    SIDE EFFECTS : none.
**    WARNINGS     : none
*********************************************************************/
static int S_calc_contour_list(solid,params,dlist,tess)
UU_REAL params[];
struct UM_solid_rec *solid;
UU_LIST *dlist;
UM_tessellation *tess;
{
	int i,n,status;
	UM_int2 idx;
	UU_REAL told;
	UM_coord *pts;
	UM_vector nvec,*tvecs;
	UM_transf tfmat;
	struct UC_entitydatabag e;
/*
.....Initialize routine
*/
	idx = 175; getsc(&idx,&told);
	n = params[5];
	pts = (UM_coord *)&params[6];
/*
.....Calculate extruded solid
*/
	um_vctovc(&params[2],nvec);
	status = S_extrusion(solid,nvec,pts,n,told,dlist,tess);
	goto done;
/*
.....Failed to create lists
*/
failed:;
	status = UU_FAILURE;
/*
.....End of routine
*/
done:;
	return(status);
}

/*********************************************************************
**    I_FUNCTION :  S_calc_extrude_list(solid,params,dlist,tess)
**       Calculates the display and tessallation lists for an EXTRUDED
**       visual solid entity along with the bounding box.
**    PARAMETERS   
**       INPUT  : 
**          solid    Solid to calculate lists for.
**          params   Solid canonical data.
**       OUTPUT :  
**          solid    Contains the bounding box of the solid.
**          dlist    Display list.
**          tess     Tessellation list.
**    RETURNS      : UU_FAILURE on failure, otherwise UU_SUCCESS.
**    SIDE EFFECTS : none.
**    WARNINGS     : none
*********************************************************************/
static int S_calc_extrude_list(solid,params,dlist,tess)
UU_REAL params[];
struct UM_solid_rec *solid;
UU_LIST *dlist;
UM_tessellation *tess;
{
	int i,n,status;
	UM_int2 idx;
	UU_REAL told;
	UM_coord *pts;
	UM_vector nvec,*tvecs;
	UM_transf tfmat;
	struct UC_entitydatabag e;
/*
.....Initialize routine
*/
	idx = 175; getsc(&idx,&told);
	n = params[3];
	pts = (UM_coord *)&params[4];
/*
.....Calculate extruded solid
*/
	um_vctovc(&params[0],nvec);
	status = S_extrusion(solid,nvec,pts,n,told,dlist,tess);
	goto done;
/*
.....Failed to create lists
*/
failed:;
	status = UU_FAILURE;
/*
.....End of routine
*/
done:;
	return(status);
}

/*********************************************************************
**    I_FUNCTION :  S_calc_revolve_list(solid,params,dlist,tess)
**       Calculates the display and tessallation lists for a REVOLVED
**       visual solid entity along with the bounding box.
**    PARAMETERS   
**       INPUT  : 
**          solid    Solid to calculate lists for.
**          params   Solid canonical data.
**       OUTPUT :  
**          solid    Contains the bounding box of the solid.
**          dlist    Display list.
**          tess     Tessellation list.
**    RETURNS      : UU_FAILURE on failure, otherwise UU_SUCCESS.
**    SIDE EFFECTS : none.
**    WARNINGS     : none
*********************************************************************/
static int S_calc_revolve_list(solid,params,dlist,tess)
UU_REAL params[];
struct UM_solid_rec *solid;
UU_LIST *dlist;
UM_tessellation *tess;
{
	int i,n,status;
	UM_int2 idx;
	UU_REAL told,sang,eang,um_mag(),plane[4];
	UM_coord *pts,cpt,*opts,*ovcs;
	UM_vector nvec,*tvecs,svec,xaxis,yaxis;
/*
.....Initialize routine
*/
	opts = UU_NULL;
	ovcs = UU_NULL;
	idx = 175; getsc(&idx,&told);
	n = params[8];
	pts = (UM_coord *)&params[9];
	tvecs = (UM_coord *)&params[n*3+9];
/*
.....Project axis point onto
.....plane level with first curve point
*/
	um_unitvc(&params[3],nvec);
	um_nptpln(&params[0],pts[0],nvec,cpt);
	sang = params[6];
	eang = params[7];
/*
.....Check for valid geometry
*/
	if (um_mag(nvec) < UM_FUZZ) goto failed;
	if (n > 2)
	{
		if (!um_planar_curve(pts,n,plane,xaxis,yaxis)) goto failed;
		if (um_vcparall(nvec,plane)) goto failed;
	}
/*
.....Curve points must be on XY-plane
*/
	opts = (UM_coord *)uu_malloc(sizeof(UM_coord)*(n+2));
	ovcs = (UM_vector *)uu_malloc(sizeof(UM_vector)*(n+2));
	status = ul_ipv_gen_revsf(cpt,nvec,pts,tvecs,&n,opts,ovcs,svec,UU_FALSE,
		UU_FALSE,UU_FALSE);
	if (status != UU_SUCCESS) goto failed;
	um_vctovc(cpt,&params[0]);
/*
.....Calculate revolved solid
*/
	status = S_revolved(solid,cpt,nvec,svec,sang,eang,opts,ovcs,n,told,dlist,
		tess);
	goto done;
/*
.....Failed to create lists
*/
failed:;
	status = UU_FAILURE;
/*
.....End of routine
*/
done:;
	if (opts != UU_NULL) uu_free(opts);
	if (ovcs != UU_NULL) uu_free(ovcs);
	return(status);
}

/*********************************************************************
**    I_FUNCTION :  S_calc_compos_list(solid,params,dlist,tess)
**       Calculates the display and tessallation lists for a COMPOS visual
**       solid entity along with the bounding box.
**    PARAMETERS   
**       INPUT  : 
**          solid    Solid to calculate lists for.
**          params   Solid canonical data.
**       OUTPUT :  
**          solid    Contains the bounding box of the solid.
**          dlist    Display list.
**          tess     Tessellation list.
**    RETURNS      : UU_FAILURE on failure, otherwise UU_SUCCESS.
**    SIDE EFFECTS : none.
**    WARNINGS     : none
*********************************************************************/
static int S_calc_compos_list(solid,params,dlist,tess)
UU_REAL params[];
struct UM_solid_rec *solid;
UU_LIST *dlist;
UM_tessellation *tess;
{
	int i,nsrf,status;
	UM_int2 idx;
	UU_REAL box[6];
	UU_KEY_ID nclkey;
	UM_tript tript;
/*
.....Initialize display list
*/
	uu_list_init(dlist,sizeof(UM_coord),25,12);
	if (dlist->data == UU_NULL) goto failed;
/*
.....Calculate bounding box
*/
	nsrf = params[2];
	solid->box[0] = solid->box[1] = solid->box[2] = 100000.;
	solid->box[3] = solid->box[4] = solid->box[5] = -100000.;
	for (i=0;i<nsrf;i++)
	{
		idx = i + 1;
		gtsky(&idx,&nclkey);
		ncl_geo_box(nclkey,box);
		ncl_update_box(&box[0],solid->box);
		ncl_update_box(&box[3],solid->box);
	}
/*
.....Composite solids do not store
.....display or tessellation lists
.....Instead the member surfaces
.....display and tessellation lists are used
*/
	uu_list_init0(dlist);
	tess->np = 0;
	tess->ntri = 0;
	status = UU_SUCCESS;
	goto done;
/*
.....Failed to allocate memory
*/
failed:;
	status = UU_FAILURE;
/*
.....End of routine
*/
done:;
	return(status);
}

/*********************************************************************
**    I_FUNCTION :  S_extrusion(solid,nvec,pts,npts,told,dlist,tess)
**       Calculates the display and tessallation lists for an extruded
**       visual solid entity along with the bounding box.
**    PARAMETERS   
**       INPUT  : 
**          solid    Solid to calculate lists for.
**          nvec     Extrusion vector with distance.
**          pts      Points defining curve to be extruded.
**          npts     Number of points in 'pts'.
**       OUTPUT :  
**          solid    Contains the bounding box of the solid.
**          dlist    Display list.
**          tess     Tessellation list.
**    RETURNS      : UU_FAILURE on failure, otherwise UU_SUCCESS.
**    SIDE EFFECTS : none.
**    WARNINGS     : none
*********************************************************************/
static int S_extrusion(solid,nvec,pts,npts,told,dlist,tess)
struct UM_solid_rec *solid;
UM_vector nvec;
UM_coord *pts;
int npts;
UU_REAL told;
UU_LIST *dlist;
UM_tessellation *tess;
{
	int i,k,status,inc,np,n;
	UU_REAL d,um_mag(),plane[4];
	UM_coord *pte,*ptq,pt0,ptx;
	UM_vector vcn,*vcq,xaxis,yaxis;
	UM_tript tript,*t;
/*
.....Initialize display list
*/
	pte = UU_NULL;
	uu_list_init(dlist,sizeof(UM_coord),25,12);
	if (dlist->data == UU_NULL) goto failed;
	np = npts - 1;
/*
.....Check for valid geometry
*/
	if (npts < 3 || um_mag(nvec) < UM_FUZZ) goto failed;
	if (um_dcccc(pts[0],pts[npts-1]) > UM_FUZZ ||
		 !um_planar_curve(pts,npts,plane,xaxis,yaxis)) goto failed;
	if (um_vcperp(nvec,plane)) goto failed;
/*
.....Initialize bounding box
*/
	ncl_init_box(pts[0],solid->box);
/*
.....Calculate far end of extrusion
*/
	pte = (UM_coord *)uu_malloc(sizeof(UM_coord)*npts);
	if (pte == UU_NULL) goto failed;
	for (i=0;i<npts;i++)
		um_vcplvc(pts[i],nvec,pte[i]);
/*
.....Store Extrusion in display list
*/
	ptx[0] = np + 2; ptx[1] = 0; ptx[2] = 0;
	uu_list_push(dlist,ptx);
	ptx[0] = npts; ptx[1] = 1;
	uu_list_push(dlist,ptx);
	for (i=0;i<npts;i++)
	{
		uu_list_push(dlist,pts[i]);
		ncl_update_box(pts[i],solid->box);
	}
	uu_list_push(dlist,ptx);
	for (i=0;i<npts;i++)
	{
		uu_list_push(dlist,pte[i]);
		ncl_update_box(pte[i],solid->box);
	}
	ptx[0] = 2;
	for (i=0;i<np;i++)
	{
		uu_list_push(dlist,ptx);
		uu_list_push(dlist,pts[i]);
		uu_list_push(dlist,pte[i]);
	}
/*
.....Initialize tessellation structure
*/
	tess->toler = told;
	tess->np = np*2 + np*4 + 2;
	tess->ntri = np*2 + np*2 + 1;
	status = uu_list_init1(&tess->vertices,sizeof(UM_coord),tess->np,4);
	if (status != UU_SUCCESS) goto done;
	status = uu_list_init1(&tess->normals,sizeof(UM_coord),tess->np,4);
	if (status != UU_SUCCESS) goto done;
	status = uu_list_init1(&tess->tri,sizeof(UU_REAL)*3,tess->ntri,4);
	if (status != UU_SUCCESS) goto done;
/*
.....Store input polygon triangles
*/
	status = ncl_tessellate_polygon(pts,npts,told,tess);
	if (status != UU_SUCCESS) goto done;
/*
.....Store extruded polygon triangles
*/
	inc = tess->np;
	for (i=0;i<inc;i++)
	{
		ptq = (UM_coord *)UU_LIST_ARRAY(&tess->vertices);
		vcq = (UM_coord *)UU_LIST_ARRAY(&tess->normals);
		um_vcplvc(ptq[i],nvec,pt0);
		um_vctmsc(vcq[i],-1.,vcn);
		uu_list_push(&tess->vertices,pt0);
		uu_list_push(&tess->normals,vcn);
	}
	tess->np = tess->np + inc;

	n = tess->ntri;
	for (i=0;i<n;i++)
	{
		t = (UM_tript *)UU_LIST_ARRAY(&tess->tri);
		tript.n1 = t[i].n3 + inc;
		tript.n2 = t[i].n2 + inc;
		tript.n3 = t[i].n1 + inc;
		uu_list_push(&tess->tri,&tript);
	}
	tess->ntri = tess->ntri + n;
/*
.....Store sides of extrusion
*/
	inc = tess->np;
	for (i=0;i<np;i++)
	{
		k = (i < np-1) ? i+1 : 0;
		uu_list_push(&tess->vertices,pts[i]);
		uu_list_push(&tess->vertices,pte[i]);
		uu_list_push(&tess->vertices,pte[k]);
		uu_list_push(&tess->vertices,pts[k]);
		um_3pt_pln(pts[i],pte[i],pte[k],vcn,&d);
		for (k=0;k<4;k++) uu_list_push(&tess->normals,vcn);
		tess->np += 4;
	}
/*
.....Store triangle list for sides
*/
	for (i=0;i<np;i++)
	{
		tript.n1 = inc;
		tript.n2 = inc + 1;
		tript.n3 = inc + 2;
		uu_list_push(&tess->tri,&tript);
		tript.n1 = inc + 2;
		tript.n2 = inc + 3;
		tript.n3 = inc;
		uu_list_push(&tess->tri,&tript);
		inc += 4;
		tess->ntri += 2;
	}
	status = UU_SUCCESS;
	goto done;
/*
.....Failed to create lists
*/
failed:;
	status = UU_FAILURE;
/*
.....End of routine
*/
done:;
	if (pte != UU_NULL) uu_free(pte);
	return(status);
}

/*********************************************************************
**    I_FUNCTION :  S_revolved(solid,cpt,nvec,svec,ang1,ang2,pts,vecs,npts,
**                       told,dlist,tess)
**       Calculates the display and tessallation lists for a revolved
**       visual solid entity along with the bounding box.
**    PARAMETERS   
**       INPUT  : 
**          solid    Solid to calculate lists for.
**          cpt      Center point of curve being revolved.
**          nvec     Vector to revolve curve about.
**          svec     Starting vector for revolved curve.
**          ang1     Starting angle of revolved solid.
**          ang2     Ending angle of revolved solid.
**          pts      Points defining curve to be extruded.  These points
**                   are stored in the XY plane assuming the rotation
**                   vector to be +Y.
**          vecs     Tangent vectors for the 'pts' array.
**          npts     Number of points in 'pts'.
**       OUTPUT :  
**          solid    Contains the bounding box of the solid.
**          dlist    Display list.
**          tess     Tessellation list.
**    RETURNS      : UU_FAILURE on failure, otherwise UU_SUCCESS.
**    SIDE EFFECTS : none.
**    WARNINGS     : none
*********************************************************************/
static int S_revolved(solid,cpt,nvec,svec,ang1,ang2,pts,vecs,npts,told,dlist,
	tess)
struct UM_solid_rec *solid;
UM_coord cpt;
UM_vector nvec,svec;
UU_REAL ang1,ang2;
UM_coord *pts;
UM_vector *vecs;
int npts;
UU_REAL told;
UU_LIST *dlist;
UM_tessellation *tess;
{
	int i,k,n,status,inc,np,ns,nsides,nlist,sinc,is,ie,ulines,vlines;
	UU_LOGICAL redo,closed,sclosed,istop,isbot,wedge;
	UU_REAL xrad,ang,dang,sang,eang,len,minlen,dis,d,totdis,um_getpolylen();
	UM_coord *pte,*tpts,*cvpts,pt0,pt1,ptx;
	UM_vector *cvvcs,vcn,temp,vc1;
	UM_tript tript;
	UM_transf tm,tm1;
	struct UM_circle_rec c;
	struct UM_surfattr_rec attr;
/*
.....Initialize routine
*/
	tpts = UU_NULL;
	cvvcs = UU_NULL;
	cvpts = UU_NULL;
	sang = (ang1 / UM_RADIAN);
	eang = (ang2 / UM_RADIAN);
	if (sang > eang) eang = eang + UM_TWOPI;
/*
.....Get attribute record to determine
.....number of V-lines to display
*/
	status = uc_retrieve_attr(solid->key,&attr);
	if (status != UU_SUCCESS) goto failed;
	ulines = attr.numupaths;
	vlines = attr.numvpaths;
/*
.....Determine if curve and solid is closed
*/
	closed = um_cceqcc(pts[0],pts[npts-1]) ? UU_TRUE : UU_FALSE;
	sclosed = (fabs(eang-sang) > UM_TWOPI-UM_FUZZ) ? UU_TRUE : UU_FALSE;
/*
.....Determine invariant circular
.....cross-section of tool geometry
*/
	um_vctovc(cpt,c.center);
	um_vctovc(nvec,c.nvec);
	if (sang == 0.) um_unitvc(svec,c.svec);
	else
	{
		um_nullvc(pt0);
		um_rotatept(svec,nvec,pt0,sang,UU_TRUE,tm);
		um_unitvc(svec,svec);
	}
	um_vctovc(svec,c.svec);
	c.dang = eang - sang;
	if (fabs(c.dang) < UM_FUZZ) goto failed;
/*
.....Transform curve to cross section and
.....Calculate number of points
.....to generate around revolved solid
*/
	xrad = 0.;
	cvpts = (UM_coord *)uu_malloc(sizeof(UM_coord)*npts);
	if (cvpts == UU_NULL) goto failed;
	S_orient_pts(pts,npts,cpt,nvec,svec,cvpts,&xrad);
	if (xrad == 0.) goto failed;
	um_circle_nsides(xrad,c.dang,told,&nsides);
/*
.....Initialize bounding box
*/
	ncl_init_box(cvpts[0],solid->box);
/*
.....Determine if we are drawing top and bottom of solid
*/
	isbot = UU_FALSE;
	istop = UU_FALSE;
	if (fabs(pts[0][0]) > UM_FUZZ && !closed) isbot = UU_TRUE;
	if (fabs(pts[npts-1][0]) > UM_FUZZ && !closed) istop = UU_TRUE;
/*
.....Initialize display list
*/
	n = npts * nsides + nsides + 1;
	if (isbot) n = n + nsides + 1;
	if (istop) n = n + nsides + 1;
	uu_list_init(dlist,sizeof(UM_coord),n,nsides);
	if (UU_LIST_NULLPTR(dlist)) goto failed;
	tpts = (UM_coord *)uu_malloc(sizeof(UM_coord)*nsides);
	if (tpts == UU_NULL) goto failed;
	cvvcs = (UM_coord *)uu_malloc(sizeof(UM_coord)*npts);
	if (cvvcs == UU_NULL) goto failed;
	ptx[0] = npts*2 + nsides; ptx[1] = 0; ptx[2] = 0;
	uu_list_push(dlist,ptx);
	nlist = 0;
/*
.....Initialize tessellation structure
*/
	inc = 0;
	np = (sclosed) ? nsides - 1 : nsides;
	tess->toler = told;
	tess->np = 0;
	tess->ntri = 0;
	n = npts * nsides;
	if (isbot) n = n + np + 1;
	if (istop) n = n + np + 1;
	status = uu_list_init1(&tess->vertices,sizeof(UM_coord),n,npts);
	if (status != UU_SUCCESS) goto done;
	status = uu_list_init1(&tess->normals,sizeof(UM_coord),n,npts);
	if (status != UU_SUCCESS) goto done;
	n = np* (npts-1) * 2;
	if (isbot) n = n + np;
	if (istop) n = n + np;
	status = uu_list_init1(&tess->tri,sizeof(UU_REAL)*3,n,6);
	if (status != UU_SUCCESS) goto done;
/*
.....Store circular rings around solid
*/
	ptx[1] = 1;
	sinc = 0;
	len = um_getpolylen(npts,pts);
	if (ulines > npts) ulines = npts;
	minlen = len / (ulines-1);
	totdis = 0.;
	for (i=0;i<npts;i++)
	{
		if (i == 0) dis = 0.;
		else dis = um_dcccc(pts[i],pts[i-1]);
		totdis = totdis + dis;
		if (totdis >= minlen || (dis < UM_FUZZ && i != 0) ||
			((isbot || closed) && i == 0) ||
			((istop && !closed) && i == npts-1))
		{
			d = totdis-minlen;
			if (d < UM_FUZZ || i == npts-1)
			{
				um_vctovc(pts[i],pt1);
				d = 0;
			}
			else
			{
				um_vcmnvc(pts[i-1],pts[i],vc1);
				um_unitvc(vc1,vc1);
				um_translate_point(pts[i],d,vc1,pt1);
			}
			um_translate_point(cpt,pt1[1],nvec,c.center);
			c.radius = fabs(pt1[0]);
			ncl_cutter_circle(&c,tpts,nsides);
			ptx[0] = nsides;
			uu_list_push(dlist,ptx);
			uu_list_push_multiple(dlist,nsides,tpts);
			nlist++;
			totdis = d;
		}
	}
/*
.....Store bottom of solid
*/
	if (isbot)
	{
		um_vctovc(cpt,c.center);
		c.radius = fabs(pts[0][0]);
		ncl_cutter_circle(&c,tpts,nsides);
/*
		ptx[0] = nsides;
		uu_list_push(dlist,ptx);
		uu_list_push_multiple(dlist,nsides,tpts);
		nlist++;
*/

		um_unitvc(nvec,vcn);
		um_vctmsc(vcn,-1.,vcn);
		uu_list_push(&tess->vertices,cpt);
		uu_list_push(&tess->normals,vcn);
		for (i=0;i<np;i++)
		{
			uu_list_push(&tess->vertices,tpts[i]);
			uu_list_push(&tess->normals,vcn);
		}
		tess->np = tess->np + np + 1;
		inc = tess->np;
	}
/*
.....Store top of solid
*/
	if (istop)
	{
		um_translate_point(cpt,pts[npts-1][1],nvec,c.center);
		c.radius = fabs(pts[npts-1][0]);
		ncl_cutter_circle(&c,tpts,nsides);
/*
		ptx[0] = nsides;
		uu_list_push(dlist,ptx);
		uu_list_push_multiple(dlist,nsides,tpts);
		nlist++;
*/

		um_unitvc(nvec,vcn);
		uu_list_push(&tess->vertices,c.center);
		uu_list_push(&tess->normals,vcn);
		for (i=0;i<np;i++)
		{
			uu_list_push(&tess->vertices,tpts[i]);
			uu_list_push(&tess->normals,vcn);
		}
		tess->np = tess->np + np + 1;
		inc = tess->np;
	}
/*
.....Calculate curve normals
*/
	for (i=0;i<npts;i++)
	{
		vc1[0] = vecs[i][1];
		vc1[1] = -vecs[i][0];
		vc1[2] = 0.;
		um_unitvc(vc1,vc1);
		um_vctmsc(nvec,vc1[1],pt1);
		um_vctmsc(svec,vc1[0],temp);
		um_vcplvc(pt1,temp,cvvcs[i]);
	}
/*
.....Store sides of solid
........In display list
*/
	ang = 0.;
	dang = c.dang / (vlines-1);
	pt0[0] = pt0[1] = pt0[2] = 0.;
	wedge = UU_FALSE;
	if ((isbot || istop) && !sclosed) wedge = UU_TRUE;
	for (i=0;i<vlines;i++)
	{
		redo = UU_TRUE;
		if ((i == 0 || i == vlines-1) && wedge)
		{
			ptx[0] = npts + 1;
			if (isbot) ptx[0] += 1;
			if (istop) ptx[0] += 1;
			uu_list_push(dlist,ptx);

			um_vctovc(cvpts[npts-1],pt1);
			um_rotatept(pt1,nvec,cpt,ang,redo,tm);
			uu_list_push(dlist,pt1);
			if (istop)
			{
				um_translate_point(cpt,pts[npts-1][1],nvec,pt0);
				uu_list_push(dlist,pt0);
			}
			if (isbot) uu_list_push(dlist,cpt);
		}
		else
		{
			ptx[0] = npts;
			uu_list_push(dlist,ptx);
		}
		for (k=0;k<npts;k++)
		{
			um_vctovc(cvpts[k],pt1);
			um_rotatept(pt1,nvec,cpt,ang,redo,tm);
			ncl_update_box(pt1,solid->box);
			uu_list_push(dlist,pt1);
			redo = UU_FALSE;
		}
		ang = ang + dang;
		nlist++;
	}
/*
...........Store number of curves in display list
*/
	pte = (UM_coord *)UU_LIST_ARRAY(dlist);
	pte[0][0] = nlist;
/*
........In tessellation list
*/
	ang = 0.;
	dang = c.dang / (nsides-1);
	pt0[0] = pt0[1] = pt0[2] = 0.;
	for (i=0;i<nsides;i++)
	{
		redo = UU_TRUE;
		uu_list_push(dlist,ptx);
		for (k=0;k<npts;k++)
		{
			um_vctovc(cvpts[k],pt1);
			um_rotatept(pt1,nvec,cpt,ang,redo,tm);
			uu_list_push(&tess->vertices,pt1);

			um_vctovc(cvvcs[k],pt1);
			um_rotatept(pt1,nvec,pt0,ang,redo,tm1);
			uu_list_push(&tess->normals,pt1);
			tess->np++;

			redo = UU_FALSE;
		}
		ang = ang + dang;
	}
/*
.....Store triangle list
*/
	ns = nsides - 1;
	is = 0;
	ie = ns;
	if (isbot)
	{
		for (i=0;i<ns;i++)
		{
			k = (i < np-1 || !sclosed) ? i+2 : 1;
			tript.n1 = k;
			tript.n2 = i + 1;
			tript.n3 = 0; 
			uu_list_push(&tess->tri,&tript);
		}
		tess->ntri = tess->ntri + ns;
		is = np + 1;
		ie = np + ns + 1;
	}
  
	if (istop)
	{
		for (i=is;i<ie;i++)
		{
			k = (i < ie-1 || !sclosed) ? i+2 : is+1;
			tript.n1 = is;
			tript.n2 = i + 1;
			tript.n3 = k; 
			uu_list_push(&tess->tri,&tript);
		}
		tess->ntri = tess->ntri + ns;
	}

	sinc = inc;
	ie = (sclosed) ? np : np - 1;
	for (i=0;i<ie;i++)
	{
		n = (i < np-1) ? inc + npts : sinc;
		for (k=0;k<npts-1;k++)
		{
			if (istop || closed || k != npts-2)
			{
				tript.n1 = n + k + 1;
				tript.n2 = inc + 1;
				tript.n3 = inc;
				uu_list_push(&tess->tri,&tript);
				tess->ntri++;
			}
			if (isbot || closed || k != 0)
			{
				tript.n1 = inc;
				tript.n2 = n + k;
				tript.n3 = n + k + 1;
				uu_list_push(&tess->tri,&tript);
				tess->ntri++;
			}
			inc++;
		}
		inc++;
	}
/*
.....Partially rotated solid
.....Tessellate open faces of solid
*/
	if (!sclosed)
	{
/*
........Tessellate starting face
*/
		inc = tess->np - npts;
		status = S_create_face(tess,cpt,sinc,npts,nvec,pts[npts-1][1],told,
			isbot,istop,closed);
		status = S_create_face(tess,cpt,inc,npts,nvec,pts[npts-1][1],told,
			isbot,istop,closed);
		if (status != UU_SUCCESS) goto done;
	}
	status = UU_SUCCESS;
	goto done;
/*
.....Failed to create lists
*/
failed:;
	status = UU_FAILURE;
/*
.....End of routine
*/
done:;
	if (tpts != UU_NULL) uu_free(tpts);
	if (cvpts != UU_NULL) uu_free(cvpts);
	if (cvvcs != UU_NULL) uu_free(cvvcs);
	return(status);
}

/*********************************************************************
**    I_FUNCTION :  S_create_face(tess,cpt,sinc,npts,nvec,hgt,told,isbot,
**                                istop,closed)
**       Calculates the tessallation lists for the endcaps of a trimmed
**       revolved solid.
**    PARAMETERS   
**       INPUT  : 
**          tess     Current tessellation list for solid.
**          cpt      Center point of curve being revolved.
**          sinc     Starting increment of endcap curve vertices.
**          npts     Number of points in endcap curve.
**          nvec     Vector to revolve curve about.
**          hgt      Height of revolved solid.
**          told     Working tolerance.
**          isbot    UU_TRUE = Bottom face was created.
**          istop    UU_TRUE = Top face was created.
**          closed   UU_TRUE = Revolved curve is closed.
**       OUTPUT :  
**          tess     Updated tessellation list.
**    RETURNS      : UU_FAILURE on failure, otherwise UU_SUCCESS.
**    SIDE EFFECTS : none.
**    WARNINGS     : none
*********************************************************************/
static int S_create_face(tess,cpt,sinc,npts,nvec,hgt,told,isbot,istop,closed)
UM_tessellation *tess;
UM_coord cpt;
int sinc,npts;
UM_vector nvec;
UU_REAL hgt,told;
UU_LOGICAL isbot,istop,closed;
{
	int i,ns,np,status;
	UM_coord *tpts,*pts;
	UM_tript *tptr;
	UM_tessellation tess1;
/*
.....Initialize routine
*/
	ns = 0;
	np = npts + 3;
	um_init_tess(&tess1);
/*
.....Allocate memory for curve points
*/
	tpts = (UM_coord *)uu_malloc(sizeof(UM_coord)*np);
	if (tpts == UU_NULL) goto failed;
/*
.....Create outline of face curve
*/
	if (isbot) um_vctovc(cpt,tpts[ns++]);

	pts = (UM_coord *)UU_LIST_ARRAY(&tess->vertices);
	for (i=0;i<npts;i++)
	{
		if (i == 0 || um_dcccc(pts[sinc+i],pts[sinc+i-1]) > UM_FUZZ)
			um_vctovc(pts[sinc+i],tpts[ns++]);
	}

	if (istop) um_translate_point(cpt,hgt,nvec,tpts[ns++]);
	if (!closed) um_vctovc(cpt,tpts[ns++]);
/*
.....Initialize tessellation structure
*/
	tess1.toler = told;
	tess1.np = 0;
	tess1.ntri = 0;
	status = uu_list_init1(&tess1.vertices,sizeof(UM_coord),npts*3,npts);
	if (status != UU_SUCCESS) goto done;
	status = uu_list_init1(&tess1.normals,sizeof(UM_coord),npts*3,npts);
	if (status != UU_SUCCESS) goto done;
	status = uu_list_init1(&tess1.tri,sizeof(UU_REAL)*3,npts,npts);
	if (status != UU_SUCCESS) goto done;
/*
.....Tessellate the face
*/
	status = ncl_tessellate_polygon(tpts,ns,told,&tess1);
	if (status != UU_SUCCESS) goto done;
/*
.....Add face tessellation to solid tessellation
*/
	np = UU_LIST_LENGTH(&tess->vertices);
	uu_list_push_list(&tess->vertices,&tess1.vertices);
	uu_list_push_list(&tess->normals,&tess1.normals);
	tptr = (UM_tript *)UU_LIST_ARRAY(&tess1.tri);
/*
........Triangle pointers must be adjusted
*/
	for (i=0;i<tess1.ntri;i++)
	{
		tptr[i].n1 = tptr[i].n1 + np;
		tptr[i].n2 = tptr[i].n2 + np;
		tptr[i].n3 = tptr[i].n3 + np;
	}
	uu_list_push_list(&tess->tri,&tess1.tri);
	tess->np += tess1.np;
	tess->ntri += tess1.ntri;
	status = UU_SUCCESS;
	goto done;
/*
.....Failure
*/
failed:
	status = UU_FAILURE;
/*
.....Free the local tessellation
*/
done:
	um_free_tess(&tess1);
	return(status);
}

/*********************************************************************
**    I_FUNCTION :  S_orient_pts(pti,npts,nvec,svec,pto,xrad)
**       Orients a set of points in the XY-plane to its 3-D position
**       as defined by the normal and starting vectors.
**    PARAMETERS   
**       INPUT  : 
**          pti      Input point array.
**          npts     Number of points in 'pti'.
**          cpt      Center point of rotation.
**          nvec     Normal vector of rotation.
**          svec     Starting vector of rotation.
**       OUTPUT :  
**          pto      Output point array.
**          xrad     Largest calculated radius of point array.
**    RETURNS      : none.
**    SIDE EFFECTS : none.
**    WARNINGS     : none
*********************************************************************/
static void S_orient_pts(pti,npts,cpt,nvec,svec,pto,xrad)
UM_coord *pti;
int npts;
UM_coord cpt;
UM_vector nvec,svec;
UM_coord *pto;
UU_REAL *xrad;
{
	int i;
	UM_coord pt0;
/*
.....Orientate point to rotation
*/
	*xrad = 0.;
	for (i=0;i<npts;i++)
	{
		if (fabs(pti[i][0]) > *xrad) *xrad = fabs(pti[i][0]);
		um_translate_point(cpt,pti[i][0],svec,pt0);
		um_translate_point(pt0,pti[i][1],nvec,pto[i]);
	}
}

/*********************************************************************
**    I_FUNCTION :  S_calc_tangents(pts,vclist,np)
**       Calculates the tangent vectors for a polyline.
**    PARAMETERS   
**       INPUT  : 
**          pts      Point array definining polyline.
**          np       Number of points in 'pts'.
**       OUTPUT :  
**          cvlist   List of tangent vectors equal to number of input points.
**    RETURNS      : none.
**    SIDE EFFECTS : none.
**    WARNINGS     : none
*********************************************************************/
static void S_calc_tangents(pts,vclist,np)
UM_coord *pts;
UU_LIST *vclist;
int np;
{
	int i;
	UM_vector tvc;
/*
.....Initialize vector list
*/
	uu_list_init(vclist,sizeof(UM_vector),np,2);
/*
.....Calculate tangent vectors
*/
	for (i=0;i<np-1;i++)
	{
		um_vcmnvc(pts[i+1],pts[i],tvc);
		um_unitvc(tvc,tvc);
		uu_list_push(vclist,tvc);
	}
	uu_list_push(vclist,tvc);
}
