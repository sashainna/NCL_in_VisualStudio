/*********************************************************************
**    NAME         :  nesfsup1.c
**       CONTAINS:
**
**     *** Routines used by IGES and NCL***
**
**           ncl_cylinder_to_sf
**           ncl_plane_to_sf
**           ncl_sphere_to_sf
**           ncl_torus_to_sf
**           ncl_revolv_to_sf
**           nclf_revnorm
**           ncl_get_redef_params
**           ncl_face_revnorm
**           ncl_get_surface_sense
**           ncl_reverse_surface_sense
**
**    COPYRIGHT 1991 (c) Numerical Control Computer Sciences Inc.
**                       All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       nesfsup1.c , 25.2
**    DATE AND TIME OF LAST  MODIFICATION
**       08/17/15 , 17:59:21
*********************************************************************/

#include "udebug.h"
#include "uhep.h"
#include "mfort.h"
#include "mdrel.h"
#include "mcrv.h"
#include "modef.h"
#include "msol.h"
#include "mattr.h"
#include "mdeval.h"
#include "msrf.h"
#include "nccs.h"
#include "ncl.h"
#include "ulist.h"
#include "nclfc.h"
#include "class.h"
#include "mdattr.h"

static void S_adjust_torus_circle();

/*********************************************************************
**    E_FUNCTION     : int ncl_cylinder_to_sf (cyl,ln,sptr)
**       Create a cylindrical or conicular surface from a center point,
**       normal vector, angle of line, and generating line.
**    PARAMETERS
**       INPUT  :
**          cyl      - Cylinder/cone canonical data.  For a cone,
**                     cyl[8] != 0.
**          ln       - Genatrix line.
**          sang     - Starting angle of cylinder.
**          eang     - Ending angle of cylinder.
**       OUTPUT :
**          sptr     - Pointer to surface.
**    RETURNS      :
**       UU_SUCCESS if no error, UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_cylinder_to_sf (cyl,ln,sang,eang,sptr)
UU_REAL cyl[];
UM_coord ln[];
UU_REAL sang,eang;
struct NCL_revsurf_rec *sptr;
{
	int i,status,np;
	UU_REAL prm[16],fudge=0.05;
	struct UM_line_rec line;
	nclsf_prim_type sftype;
	struct UC_attributedatabag attr;
/*
.....Initialize routine
*/
	status = UU_SUCCESS;
/*
.....Initialize line record
*/
	ur_setup_data(UM_LINE_REL,&line,sizeof(struct UM_line_rec));
/*
.....Create line to revolve
*/
	strcpy(line.label,"@UN");
	line.subscr = 0;
	um_vctovc(ln[0],line.spt);
	um_vctovc(ln[1],line.ept);
/*
.....Store line in Unibase
*/
	um_create_geom(&line,UM_DEFAULT_TF,UM_CURRENT_ATTR);
	if (line.key == 0) goto failed;
	attr.key = line.key;
	ur_retrieve_attr(&attr);
	attr.displayable = UM_NEVERDISPLAYABLE;
	ur_update_attr(&attr);
/*
.....Create Revolved (cylindrical) Surface
*/
	ur_setup_data(NCL_REVSURF_REL,sptr,sizeof(struct NCL_revsurf_rec));
	sptr->rldnu = UU_TRUE;
	sptr->swapuv = UU_FALSE;
	sptr->rev_normal = UU_FALSE;
	sptr->closdinu = UU_FALSE;
	sptr->closdinv = fabs(eang-sang) >= 359.0;//360.0-fudge;//== UM_TWOPI;
	sptr->offdist = 0.;
	sptr->cvkey = line.key;
/*
........Canonical data
*/
	um_vctovc(&cyl[0],sptr->pta);
	um_vctovc(&cyl[3],sptr->vca);
	sptr->sa = sang; sptr->ta = eang;
/*
.....Store Cylinder
*/
	i = NCLI_SURF;
	status = ncl_create_entity (sptr, i);
/*
.....Store Primitive data
*/
	if (status == UU_SUCCESS)
	{
/*
........Cylinder
*/
		if (cyl[8] == 0)
		{
			for (i=0;i<8;i++) prm[i] = cyl[i];
			np = 8;
			sftype = NCLSF_CYLINDER;
		}
/*
........Cone
*/
		else
		{
			ncl_revsf_primdat(sptr->pta,sptr->vca,&line,&sftype,prm);
			np = 9;
		}
		for (i=np;i<16;i++) prm[i] = 0.;
		status = ncl_store_prim_data(sptr, sftype, prm);
	}
	goto done;
/*
.....Error creating surface
*/
failed:;
	status = UU_FAILURE;
	sptr->key = 0;
/*
.....End of routine
*/
done:;
	return (status);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_plane_to_sf (e1ptr, e2ptr, e3ptr)
**       Create a planar rbsplsrf surface from a plane limited by a 
**       bounding curve.
**    PARAMETERS
**       INPUT  :
**          e1ptr    - Pointer to plane.
**          e2ptr    - Pointer to xyz curve.
**       OUTPUT :
**          e3ptr    - Pointer to surface.
**    RETURNS      :
**       UU_SUCCESS iff no error, UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_plane_to_sf (e1ptr, e2ptr, e3ptr)
struct NCL_fixed_databag *e1ptr, *e2ptr, *e3ptr;
{
	int i,j,status,npts,n1;
	UM_real8 tol, d1, d1max;
	UU_REAL da[6], prm[16];
	UM_coord *p1, ppt; 
	UM_coord cpt[4];
	UM_vector pve,v1,v2,vt;
	UU_LIST ptlist;
	struct NCL_nclpl_rec *plptr;
	struct UM_rbsplsrf_rec *sptr;
	UM_transf tfmat;

	status = UU_SUCCESS;
	gettol(&tol);
	uu_list_init (&ptlist, sizeof(UM_coord), 200, 200);
/*
.....Evolve points on curve.
*/
	status = uc_retrieve_transf(e2ptr->key, tfmat);
	if (status == UU_SUCCESS)
	{
		npts = ncl_evolve_curve (e2ptr,tfmat,tol,&ptlist,UU_NULL,UU_NULL,0);
		if (npts < 4) status = UU_FAILURE;
	}
	if (status == UU_SUCCESS)
	{
		p1 = (UM_coord *)UU_LIST_ARRAY(&ptlist);
		plptr = (struct NCL_nclpl_rec *)e1ptr;
		um_vctovc(plptr->nvec,pve);
		um_vctovc(plptr->pt,ppt);
	}
/*
.....If plane key is zero, create a plane out of the bounding curve.
*/
	if (status == UU_SUCCESS && e1ptr->key == 0)
	{
		n1 = npts-1;

		ppt[0] = ppt[1] = ppt[2] = 0.;
		pve[0] = pve[1] = pve[2] = 0.;
		d1max = 0.;
/*
..... calculate the center point ppt
*/
		for (i=0; i<n1; i++)
		{
			ppt[0] += p1[i][0]; ppt[1] += p1[i][1]; ppt[2] += p1[i][2];
		}
		ppt[0] /= n1; ppt[1] /= n1; ppt[2] /= n1;
/*
..... find a couple of "radius-vectors" (connecting the center with a curve
..... point) that give the best defined plane normal
*/
		for (i=0; i<n1; i++)
		{
			um_vcmnvc (p1[i],ppt,v1);
			d1 = UM_MAG(v1);
			if (d1 < 2.*tol) continue;
			v1[0] /= d1; v1[1] /= d1; v1[2] /= d1;
			for (j=i+1; j<n1; j++)
			{
				um_vcmnvc (p1[j],ppt,v2);
				um_cross (v1,v2,vt);
				d1 = UM_MAG(vt);
				if (d1 > tol && d1 > d1max + UM_FUZZ)
				{
					um_vctovc (vt,pve); d1max = d1;
				}
			}
		}
		if (d1max < tol) status = UU_FAILURE;
		else
		{
/*
..... check if all the curve points are within tolerance on the plane
*/
			pve[0] /= d1max; pve[1] /= d1max; pve[2] /= d1max;
			for (i=0; i<n1 && status == UU_SUCCESS; i++)
			{
				um_vcmnvc (p1[i],ppt,v1);
				d1 = UM_DOT (pve,v1);
				if (fabs (d1) > tol) status = UU_FAILURE;
			}
			if (status == UU_SUCCESS)
			{
				um_vctovc(pve,plptr->nvec);
				um_vctovc(ppt,plptr->pt);
			}
		}
	}
	if (status == UU_SUCCESS)
	{
/*
.....Create a 2d bounding box on a plain from a set of points. Use the corner
..... points as control points.
*/
		um_box_on_plane(p1,npts,ppt,pve,cpt,UU_TRUE);
/*
.....Create surface.
*/
		sptr = (struct UM_rbsplsrf_rec *)e3ptr;
		sptr->key = 0;
		status = ncl_setup_rbsf(UM_RBSPLSRF_REL, sptr, sizeof(*sptr));
	}
	if (status == UU_SUCCESS)
	{
		sptr->ku = 2;
		sptr->kv = 2;
		sptr->nu = 1;
		sptr->nv = 1;
		i = NCLI_SURF;
		status = ncl_create_entity (sptr, i);
	}
	if (status == UU_SUCCESS)
	{
		da[0] = da[1] = 0.0;
		da[2] = da[3] = 1.0;
/*
..... Max and min u,v values get appended to knot vectors.
*/
		da[4] = 0.0;
		da[5] = 1.0;
		status = ur_update_data_varlist (sptr->key, 1, da, 1, 6);
	}
	if (status == UU_SUCCESS)
		status = ur_update_data_varlist (sptr->key, 2, da, 1, 6);
	if (status == UU_SUCCESS)
		status = ur_update_data_varlist (sptr->key, 3, cpt, 1, 4);
	if (status == UU_SUCCESS)
	{
		da[0] = da[1] = 1.0;
		status = ur_update_data_varlist (sptr->key, 4, da, 1, 4);
	}
	if (status == UU_SUCCESS)
		status = ncl_retrieve_data_fixed (sptr);
	if (status == UU_SUCCESS)
	{
		um_vctovc(pve,prm);
		prm[3] = UM_DOT(pve,ppt);
		um_vctovc(ppt,&prm[4]);
		for (i=7;i<16;i++) prm[i] = 0.;
		status = ncl_store_prim_data(sptr, NCLSF_PLANE, prm);
	}

	uu_list_free (&ptlist);
	return (status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_revolve_to_sf (cvkey,cpt,nvec,sang,eang,sptr)
**       Create a surface of revolution from a center point, normal, and
**       curve.
**    PARAMETERS
**       INPUT  :
**          cvkey    - Boundary curve key.
**          cpt      - Center of surface.
**          nvec     - Axis of revolved surface.
**          sang     - Starting angle of surface.
**          eang     - Ending angle of surface.
**       OUTPUT :
**          sptr     - Pointer to surface.
**    RETURNS      :
**       UU_SUCCESS if no error, UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_revolve_to_sf (cvkey,cpt,nvec,sang,eang,sptr)
UU_KEY_ID cvkey;
UM_coord cpt;
UM_vector nvec;
UU_REAL sang,eang;
struct NCL_revsurf_rec *sptr;
{
	int i,status;
	UM_int2 typ;
	UU_REAL prm[16];
	struct NCL_fixed_databag crv;
	struct UC_attributedatabag attr;
/*
.....Initialize routine
*/
	status = UU_SUCCESS;
/*
.....Create Revolved Surface
*/
	ur_setup_data(NCL_REVSURF_REL,sptr,sizeof(struct NCL_revsurf_rec));
	sptr->rldnu = 0;
	sptr->swapuv = 0;
	sptr->rev_normal = UU_FALSE;
	sptr->closdinu = UU_FALSE;
	sptr->closdinv = fabs(eang-sang) >= 359.0;//360.0-UM_FUZZ;UM_TWOPI-UM_FUZZ;
	sptr->offdist = 0.;
	sptr->cvkey = cvkey;
/*
........Canonical data
*/
	um_vctovc(cpt,sptr->pta);
	um_vctovc(nvec,sptr->vca);
	sptr->sa = sang; sptr->ta = eang;
/*
.....Store Surface
*/
	i = NCLI_SURF;
	status = ncl_create_entity (sptr, i);
/*
.....Store Primitive data
*/
	if (status == UU_SUCCESS)
	{
		attr.key = sptr->key;
		ur_retrieve_attr(&attr);
		attr.displayable = UM_NEVERDISPLAYABLE;
		ur_update_attr(&attr);
		crv.key = cvkey;
		ncl_retrieve_data_fixed(&crv);
		status = ncl_revsf_primdat(sptr->pta,sptr->vca,&crv,&typ,prm);
		if (typ <= 0)
			status = ncl_sf_prim_analyz(&sptr->key,&typ,prm);
		else
			status = ncl_put_sf_primdat(&sptr->key,&typ,prm);
		status = UU_SUCCESS;
	}
	goto done;
/*
.....Error creating surface
*/
failed:;
	status = UU_FAILURE;
	sptr->key = 0;
/*
.....End of routine
*/
done:;
	return (status);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_sphere_to_sf (cpt,nvec,svec,rad,sptr)
**       Create a spherical surface from a center point, plane, and
**       radius.
**    PARAMETERS
**       INPUT  :
**          cpt      - Center of sphere.
**          nvec     - Sphere plane normal vector.
**          svec     - Starting vector of sphere generating circle.
**          rad      - Radius of sphere.
**          sang     - Starting angle for surface.
**          eang     - Ending angle for surface.
**          cang     - Angle for circle.
**       OUTPUT :
**          sptr     - Pointer to surface.
**    RETURNS      :
**       UU_SUCCESS if no error, UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_sphere_to_sf (cpt,nvec,svec,csvec,rad,sang,eang,cang,sptr)
UM_coord cpt;
UM_vector nvec,csvec,svec;
UU_REAL rad,sang,eang,cang;
struct NCL_revsurf_rec *sptr;
{
	int i,status;
	UU_REAL prm[16];
	struct UM_circle_rec circle;
	struct UC_attributedatabag attr;
/*
.....Initialize routine
*/
	status = UU_SUCCESS;
/*
.....Initialize circle record
*/
	ur_setup_data(UM_CIRCLE_REL,&circle,sizeof(struct UM_circle_rec));
/*
.....Create circle to revolve
*/
	strcpy(circle.label,"@UN");
	circle.subscr = 0;
	um_vctovc(cpt,circle.center);
	um_vctovc(nvec,circle.nvec);
	um_vctovc(csvec,circle.svec);
	circle.radius = rad;
	circle.dang = cang;
/*
.....Store circle in Unibase
*/
	um_create_geom(&circle,UM_DEFAULT_TF,UM_CURRENT_ATTR);
	if (circle.key == 0) goto failed;
	attr.key = circle.key;
	ur_retrieve_attr(&attr);
	attr.displayable = UM_NEVERDISPLAYABLE;
	ur_update_attr(&attr);
/*
.....Create Revolved (spherical) Surface
*/
	ur_setup_data(NCL_REVSURF_REL,sptr,sizeof(struct NCL_revsurf_rec));
	sptr->rldnu = 0;
	sptr->swapuv = 0;
	sptr->rev_normal = UU_FALSE;
	sptr->closdinu = sptr->closdinv = UU_TRUE;
	sptr->offdist = 0.;
	sptr->cvkey = circle.key;
/*
........Canonical data
*/
	um_vctovc(circle.center,sptr->pta);
	um_vctovc(svec,sptr->vca);
	sptr->sa = sang; sptr->ta = eang;
/*
.....Store Sphere
*/
	i = NCLI_SURF;
	status = ncl_create_entity (sptr, i);
/*
.....Store Primitive data
*/
	if (status == UU_SUCCESS)
	{
		um_vctovc(circle.center,prm);
		prm[3] = circle.radius;
		for (i=4;i<16;i++) prm[i] = 0.;
		status = ncl_store_prim_data(sptr, NCLSF_SPHERE, prm);
	}
	goto done;
/*
.....Error creating surface
*/
failed:;
	status = UU_FAILURE;
	sptr->key = 0;
/*
.....End of routine
*/
done:;
	return (status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_torus_to_sf (cpt,nvec,svec,rad1,rad2,spt,sang,eang,
**                                      sptr)
**       Create a toroidal surface from a center point, vectors, and
**       radii.
**    PARAMETERS
**       INPUT  :
**          cvkey    - Boundary curve key.
**          cpt      - Center of torus.
**          nvec     - Torus plane normal vector.
**          svec     - Starting vector of torus generating circle.
**          rad1     - Major radius of torus.
**          rad2     - Minor radius of torus.
**          spt      - Starting point on minor circle of torus.
**          sang     - Starting angle of torus.
**          eang     - Ending angle of torus.
**          flag     - UU_FALSE: Adjust torus circle.
**                     UU_TRUE:  Use the given vectors,do not adjust.
**       OUTPUT :
**          sptr     - Pointer to surface.
**    RETURNS      :
**       UU_SUCCESS if no error, UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_torus_to_sf (cvkey,cpt,nvec,svec,rad1,rad2,spt,sang,eang,flag,sptr)
UU_KEY_ID cvkey;
UM_coord cpt;
UM_vector nvec,svec;
UU_REAL rad1,rad2;
UM_coord spt;
UU_REAL sang,eang;
UU_LOGICAL flag;
struct NCL_revsurf_rec *sptr;
{
	int i,status;
	UU_REAL prm[16];
	UM_coord ptx;
	struct UM_circle_rec circle;
	struct UC_attributedatabag attr;
/*
.....Initialize routine
*/
	status = UU_SUCCESS;
/*
.....Initialize circle record
*/
	ur_setup_data(UM_CIRCLE_REL,&circle,sizeof(struct UM_circle_rec));
/*
.....Create circle to revolve
*/
	strcpy(circle.label,"@UN");
	circle.subscr = 0;
	um_translate_point(cpt,rad1,svec,circle.center);
	um_cross(nvec,svec,circle.nvec);
	um_nptpln(spt,circle.center,circle.nvec,ptx);
	um_vcmnvc(ptx,circle.center,circle.svec);
	um_unitvc(circle.svec,circle.svec);
	circle.radius = rad2;
	circle.dang = UM_TWOPI;
/*
.....Adjust circle for boundary curve
.......Do not adjust circle if boundary was merged - ASF 7/2/13.
*/
	if (!flag) S_adjust_torus_circle(cvkey,&circle,cpt,nvec);
/*
.....Store circle in Unibase
*/
	um_create_geom(&circle,UM_DEFAULT_TF,UM_CURRENT_ATTR);
	if (circle.key == 0) goto failed;
	attr.key = circle.key;
	ur_retrieve_attr(&attr);
	attr.displayable = UM_NEVERDISPLAYABLE;
	ur_update_attr(&attr);
/*
.....Create Revolved (toroidal) Surface
*/
	ur_setup_data(NCL_REVSURF_REL,sptr,sizeof(struct NCL_revsurf_rec));
	sptr->rldnu = 0;
	sptr->swapuv = 0;
	sptr->rev_normal = UU_FALSE;
	sptr->closdinu = UU_TRUE;
	sptr->closdinv = fabs(eang-sang) >= 359.0;//360.0-UM_FUZZ;UM_TWOPI-UM_FUZZ;
	sptr->offdist = 0.;
	sptr->cvkey = circle.key;
/*
........Canonical data
*/
	um_vctovc(cpt,sptr->pta);
	um_vctovc(nvec,sptr->vca);
	sptr->sa = sang; sptr->ta = eang;
/*
.....Store Torus
*/
	i = NCLI_SURF;
	status = ncl_create_entity (sptr, i);
/*
.....Store Primitive data
*/
	if (status == UU_SUCCESS)
	{
		um_vctovc(cpt,prm);
		um_vctovc(nvec,&prm[3]);
		prm[6] = rad1; prm[7] = rad2;
		for (i=8;i<16;i++) prm[i] = 0.;
		status = ncl_store_prim_data(sptr, NCLSF_TORUS, prm);
	}
	goto done;
/*
.....Error creating surface
*/
failed:;
	status = UU_FAILURE;
	sptr->key = 0;
/*
.....End of routine
*/
done:;
	return (status);
}

/*********************************************************************
**    I_FUNCTION     :  S_adjust_torus_circle(cvkey,circle,cpt,nvec)
**       Recalculates the initial vector of the revolved circle based
**       on the boundary curve of the trimmed surface.
**    PARAMETERS   
**       INPUT  : 
**          cvkey    Boundary curve key.
**          circle   Revolved circle of torus.
**          cpt      Torus center point.
**          nvec     Normal vector of torus.
**       OUTPUT : 
**          circle.svec     Starting vector of vector.
**    RETURNS      : UU_FAILURE if could not calculate parameters.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_adjust_torus_circle(cvkey,circle,cpt,nvec)
UU_KEY_ID cvkey;
struct UM_circle_rec *circle;
UM_coord cpt;
UM_vector nvec;
{
	int i,npts;
	UU_LOGICAL first,init;
	UU_REAL tol,dang,sang;
	UU_REAL um_angle2p();
	UM_coord *pts,ptx,pty;
	UM_vector evec,tvec,svec;
	UM_transf tfmat;
	UU_LIST ptlist;
	struct UM_crvdatabag crv;
	struct UM_rotmatrix mx;
/*
.....Initialize routine
*/
	init = UU_FALSE;
	if (cvkey == 0) goto done;
/*
....Get boundary curve
*/
	crv.key = cvkey;
	ncl_retrieve_data_fixed(&crv);
	uc_retrieve_transf (crv.key,tfmat);
/*
.....Evolve points on the curve
*/
	gettol(&tol);
	uu_list_init(&ptlist,sizeof(UM_coord),200,200); init = UU_TRUE;
	npts = ncl_evolve_curve(&crv,tfmat,tol,&ptlist,UU_NULL,UU_NULL,0);
	if (npts < 3) goto done;
/*
.....Calculate the starting vector of the circle
*/
	um_vcmnvc(circle->center,cpt,tvec); um_unitvc(tvec,tvec);
	um_vctovc(circle->svec,svec);
/*
.....Loop through all points
.....to find the starting vector of the circle
.....that encompasses the boundary curve
*/
	sang = 0.;
	pts = (UM_coord *)UU_LIST_ARRAY(&ptlist);
	for (i=0;i<npts;i++)
	{
/*
........Calculate vector of curve point
*/
		um_nptpln(pts[i],cpt,nvec,ptx);
		um_vcmnvc(ptx,cpt,evec);
		um_unitvc(evec,evec);
/*
........Rotate the boundary curve point
........onto the plane of the circle
*/
		dang = um_angle2p(evec,tvec,nvec);
		um_vctovc(pts[i],pty);
		um_rotatept(pty,nvec,cpt,dang,UU_TRUE,&mx);
/*
........Calculate the starting angle of the circle
*/
		um_vcmnvc(pty,circle->center,evec);
//		dang = um_angle2p(tvec,evec,circle->nvec);
		dang = um_angle2p(svec,evec,circle->nvec);
		if (dang > UM_PI) dang = dang - UM_TWOPI;
		if (fabs(dang) > sang)
		{
			um_unitvc(evec,circle->svec);
			sang = fabs(dang);
		}
	}
/*
.....End of routine
*/
done:;
	if (init) uu_list_free(&ptlist);
	return;
}

/*********************************************************************
**    E_FUNCTION     : nclf_revnorm (nclkey)
**       Reverse the surface rev_normal flag.
**    PARAMETERS
**       INPUT  :
**          nclkey     - Key of surface.
**       OUTPUT :
**    RETURNS      : none
**         UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclf_revnorm (nclkey)
UM_int4 *nclkey;
{
	UU_LOGICAL update = UU_TRUE;
	int status;
	UM_int4 key;
	struct NCL_fixed_databag e1;
	struct NCL_surface_rec *nsf;
	struct NCL_revsurf_rec *vsf;
	struct NCL_meshsf_rec *msf;
	struct UM_rbsplsrf_rec *rsf;

	e1.key = *nclkey;
	status = ncl_retrieve_data_fixed(&e1);

	if (status == UU_SUCCESS)
	{
		switch (e1.rel_num)
		{
		case NCL_SURF_REL:
			nsf = (struct NCL_surface_rec *)&e1;
			nsf->rev_normal = !nsf->rev_normal;
			break;
		case NCL_REVSURF_REL:
			vsf = (struct NCL_revsurf_rec *)&e1;
			vsf->rev_normal = !vsf->rev_normal;
			break;
		case NCL_MESHSURF_REL:
			msf = (struct NCL_meshsf_rec *)&e1;
			msf->rev_normal = !msf->rev_normal;
			break;
		case UM_RBSPLSRF_REL:
			rsf = (struct UM_rbsplsrf_rec *)&e1;
			rsf->rev_normal = !rsf->rev_normal;
			break;
		case NCL_TRIMSF_REL:
			key = ((struct NCL_trimsf_rec *)&e1)->bs_key;
			nclf_revnorm (&key);
			update = UU_FALSE;
			break;
		default:
			break;
		}
		if (update) status = ur_update_data_fixed(&e1);
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_get_redef_params (eptr,iswap,revnorm)
**       Retrieve the swapuv and rev_normal flags for a surface.
**    PARAMETERS
**       INPUT  :
**          eptr      -  surface.pointer
**       OUTPUT :
**          iswap
**          revnorm
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_get_redef_params (eptr,iswap,revnorm)
struct NCL_fixed_databag *eptr;
int *iswap;
UU_LOGICAL *revnorm;
{
	struct NCL_surface_rec *nsf;
	struct NCL_revsurf_rec *vsf;
	struct NCL_meshsf_rec *msf;
	struct UM_rbsplsrf_rec *rsf;

	switch (eptr->rel_num)
	{
	case NCL_SURF_REL:
		nsf = (struct NCL_surface_rec *)eptr;
		*iswap = nsf->swapuv;
		*revnorm = nsf->rev_normal;
		break;
	case NCL_REVSURF_REL:
		vsf = (struct NCL_revsurf_rec *)eptr;
		*iswap = vsf->swapuv;
		*revnorm = vsf->rev_normal;
		break;
	case NCL_MESHSURF_REL:
		msf = (struct NCL_meshsf_rec *)eptr;
		*iswap = msf->swapuv;
		*revnorm = msf->rev_normal;
		break;
	case UM_RBSPLSRF_REL:
		rsf = (struct UM_rbsplsrf_rec *)eptr;
		*iswap = rsf->swapuv;
		*revnorm = rsf->rev_normal;
		break;
	default:
		*iswap = 0;
		*revnorm = UU_FALSE;
		break;
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_face_revnorm (nclkey)
**       Reverse the surface face normal flag (prim_param[15]).  This
**       flag is typically used to state whether the normals of a
**       revolved surface point outside or inside the revolved surface.
**    PARAMETERS
**       INPUT  :
**          nclkey     - Key of surface.
**       OUTPUT :
**    RETURNS      : none
**         UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_face_revnorm (nclkey)
UM_int4 *nclkey;
{
	UU_LOGICAL update = UU_TRUE;
	int status;
	UM_int4 key;
	struct NCL_fixed_databag e1;
	struct NCL_surface_rec *nsf;
	struct NCL_revsurf_rec *vsf;
	struct UM_rbsplsrf_rec *rsf;

	e1.key = *nclkey;
	status = ncl_retrieve_data_fixed(&e1);

	if (status == UU_SUCCESS)
	{
		switch (e1.rel_num)
		{
		case NCL_SURF_REL:
			nsf = (struct NCL_surface_rec *)&e1;
			nsf->prim_param[15] = !nsf->prim_param[15];
			break;
		case NCL_REVSURF_REL:
			vsf = (struct NCL_revsurf_rec *)&e1;
			vsf->prim_param[15] = !vsf->prim_param[15];
			break;
		case UM_RBSPLSRF_REL:
			rsf = (struct UM_rbsplsrf_rec *)&e1;
			rsf->prim_param[15] = !rsf->prim_param[15];
			break;
		case NCL_TRIMSF_REL:
			key = ((struct NCL_trimsf_rec *)&e1)->bs_key;
			ncl_face_revnorm(&key);
			update = UU_FALSE;
			break;
		default:
			break;
		}
		if (update) status = ur_update_data_fixed(&e1);
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_get_face_revnorm (surf)
**       Return the surface face normal flag (prim_param[15]);
**    PARAMETERS
**       INPUT  :
**          surf       - Surface entity to return face normal for.
**       OUTPUT :
**    RETURNS      : none
**         1 = Face normal is reversed, 0 if not.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_get_face_revnorm(surf)
struct NCL_fixed_databag *surf;
{
	int status,revnorm;
	struct NCL_fixed_databag e1;
	struct NCL_surface_rec *nsf;
	struct NCL_revsurf_rec *vsf;
	struct UM_rbsplsrf_rec *rsf;

	switch (surf->rel_num)
	{
	case NCL_SURF_REL:
		nsf = (struct NCL_surface_rec *)surf;
		revnorm = nsf->prim_param[15];
		break;
	case NCL_REVSURF_REL:
		vsf = (struct NCL_revsurf_rec *)surf;
		revnorm = vsf->prim_param[15];
		break;
	case UM_RBSPLSRF_REL:
		rsf = (struct UM_rbsplsrf_rec *)surf;
		revnorm = rsf->prim_param[15];
		break;
	case NCL_TRIMSF_REL:
		e1.key = ((struct NCL_trimsf_rec *)surf)->bs_key;
		status = ncl_retrieve_data_fixed(&e1);
		revnorm = ncl_get_face_revnorm(&e1);
		break;
	default:
		revnorm = UU_FALSE;
		break;
	}
	return(revnorm);
}

/*********************************************************************
**    E_FUNCTION     : ncl_get_surface_sense (surf)
**       Determines the direction of the surface normal.
**    PARAMETERS
**       INPUT  :
**          surf       - Surface entity to analyze the normal for.
**       OUTPUT :
**    RETURNS      :
**         -1 = Normal points to inside of closed surface.
**         0  = Cannot determine surface sense for this type of surface.
**         1  = Normal points to the outside of closed surfaces.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_get_surface_sense(surf)
struct NCL_fixed_databag *surf;
{
	int status,sense;
	UU_REAL *prims,ang;
	UM_coord pt1,pt2;
	UM_vector vc1,nvec;
	UM_transf tfmat;
	nclsf_prim_type primtyp;
	struct NCL_fixed_databag e1;
	struct NCL_surface_rec *nsf;
	struct NCL_revsurf_rec *vsf;
	struct UM_rbsplsrf_rec *rsf;
	struct UM_evsrfout evsrf;
/*
.....Initialize routine
*/
	sense = 0;
/*
.....Determine surface type
........It seems STEP only requires this flag
........for Surfaces-of-Revolution
*/
	switch (surf->rel_num)
	{
	case NCL_REVSURF_REL:
		vsf = (struct NCL_revsurf_rec *)surf;
		primtyp = vsf->primitive;
		prims = vsf->prim_param;
		break;
/*
	case NCL_SURF_REL:
		nsf = (struct NCL_surface_rec *)surf;
		primtyp = nsf->primitive;
		prims = nsf->prim_param;
		break;
	case UM_RBSPLSRF_REL:
		rsf = (struct UM_rbsplsrf_rec *)surf;
		primtyp = rsf->primitive;
		prims = rsf->prim_param;
		break;
*/
	case NCL_TRIMSF_REL:
		e1.key = ((struct NCL_trimsf_rec *)surf)->bs_key;
		status = ncl_retrieve_data_fixed(&e1);
		sense = ncl_get_surface_sense(&e1);
		goto done;
	default:
		sense = 0;
		goto done;
	}
/*
.....Determine primitive type
*/
	uc_init_evsrfout(surf,&evsrf);
	status = uc_retrieve_transf(surf->key,tfmat);
	if (status != UU_SUCCESS) goto done;
	switch (primtyp)
	{
/*
.....Cone
*/
	case NCLSF_CONE:
		uc_evsrf(UM_NORM,0.,0.,surf,tfmat,&evsrf);
		um_nptln(evsrf.sp,&prims[0],&prims[3],pt1);
		um_vcmnvc(evsrf.sp,pt1,vc1); um_unitvc(vc1,vc1);
		um_cross(evsrf.snorm,vc1,nvec);
		ang = um_angle2p(evsrf.snorm,vc1,nvec);
		sense = 1;
		if (ang > UM_PI) sense = -1;
		break;
/*
.....Cylinder
*/
	case NCLSF_CYLINDER:
		uc_evsrf(UM_NORM,0.,0.,surf,tfmat,&evsrf);
		um_translate_point(evsrf.sp,prims[6],evsrf.snorm,pt1);
		um_nptln(evsrf.sp,&prims[0],&prims[3],pt2);
		sense = 1;
		if (um_dcccc(pt1,pt2) < um_dcccc(evsrf.sp,pt2)) sense = -1;
		break;
/*
.....Sphere
*/
	case NCLSF_SPHERE:
		uc_evsrf(UM_NORM,0.,0.,surf,tfmat,&evsrf);
		um_translate_point(evsrf.sp,prims[3],evsrf.snorm,pt1);
		sense = 1;
		if (um_dcccc(pt1,&prims[0]) < um_dcccc(evsrf.sp,&prims[0])) sense = -1;
		break;
/*
.....Torus
*/
	case NCLSF_TORUS:
		uc_evsrf(UM_NORM,0.,0.,surf,tfmat,&evsrf);
		um_nptln(evsrf.sp,&prims[0],&prims[3],pt1);
		um_vcmnvc(evsrf.sp,pt1,vc1); um_unitvc(vc1,vc1);
		um_translate_point(&prims[0],prims[6],vc1,pt1);
		um_translate_point(evsrf.sp,prims[7],evsrf.snorm,pt2);
		sense = 1;
		if (um_dcccc(pt1,pt2) < prims[7]) sense = -1;
		break;
/*
.....Cannot calculate sense
.....from primitive type
*/
	default:
		sense = 0;
		break;
	}
/*
.....End of routine
*/
done:;
	return(sense);
}

/*********************************************************************
**    E_FUNCTION     : ncl_reverse_surface_sense (surf)
**       Reverses the surface normals.
**    PARAMETERS
**       INPUT  :
**          surf       - Surface entity to reverse the the normals for.
**       OUTPUT :
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_reverse_surface_sense(surf)
struct NCL_fixed_databag *surf;
{
	UU_REAL rnum;
	struct NCL_fixed_databag e1;
	struct NCL_surface_rec *nsf;
	struct NCL_revsurf_rec *vsf;
	struct UM_rbsplsrf_rec *rsf;
/*
.....Determine surface type
*/
	switch (surf->rel_num)
	{
	case NCL_SURF_REL:
/*
		nsf = (struct NCL_surface_rec *)surf;
		nsf->swapuv = !nsf->swapuv;
*/
		break;
	case NCL_REVSURF_REL:
		vsf = (struct NCL_revsurf_rec *)surf;
		um_vctmsc(vsf->vca,-1.,vsf->vca);
		rnum = vsf->sa;
		vsf->sa = 360. - vsf->ta;
		vsf->ta = 360. - rnum;
		if (vsf->primitive == NCLSF_CYLINDER || vsf->primitive == NCLSF_CONE)
		{
			um_translate_point(vsf->pta,-vsf->prim_param[7],vsf->vca,vsf->pta);
			um_vctovc(vsf->pta,vsf->prim_param);
			um_vctovc(vsf->vca,&vsf->prim_param[3]);
			e1.key = vsf->cvkey;
			ncl_retrieve_data_fixed(&e1);
			uc_reverse_curve(&e1);
			um_update_geom(&e1,UM_DEFAULT_TF);
		}
		break;
	case UM_RBSPLSRF_REL:
/*
		rsf = (struct UM_rbsplsrf_rec *)surf;
		rsf->swapuv = !rsf->swapuv;
*/
		break;
	case NCL_TRIMSF_REL:
		e1.key = ((struct NCL_trimsf_rec *)surf)->bs_key;
		ncl_retrieve_data_fixed(&e1);
		ncl_reverse_surface_sense(&e1);
		return;
	}
/*
.....Save the updated surface
*/
	ur_update_data_fixed(surf);
	if (surf->rel_num == NCL_REVSURF_REL)
	{
		vsf->key = surf->key;
		ncl_retrieve_data_fixed(vsf);
	}
	return;
}
