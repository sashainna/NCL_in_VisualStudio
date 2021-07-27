/*********************************************************************
**    NAME         :  neretrim1.c
**       CONTAINS: Fortran interface, display, and evaluator routines
**                 for trimmed surfaces which are:
**
**           int rmvget
**           void rmvput
**           void rmvfre 
**           void plnsrf
**           void extprm
**           void trmrev
**           void trmbse
**           int ncl_pol_bndry
**           int ncl_bndry_pol
**           int ncl_cvpol
**           void ncl_addto_pol
**           void ncl_addto_pol0
**           void ncl_contour_components
**           int ncl_contour_curve
**           void ncl_extend_cyl
**           void ncl_extend_con
**           void ncl_extend_sph
**           void ncl_extend_rev
**           int ncl_prim_base
**
**    COPYRIGHT 2005 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       neretrim1.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:46
*********************************************************************/
#include "udebug.h"
#include "mdeval.h"
#include "mdrel.h"
#include "mcrv.h"
#include "msrf.h"
#include "ncl.h"
#include "nclfc.h"
#include "nclvx.h"
#include "nccs.h"
#include "mattr.h"
#include "mdattr.h"
#include "mgeom.h"
#include "modef.h"
#include "class.h"
#include "nclclip.h"

static UU_LIST Srmv_list;
static int Srmv_list_init = 0;
extern UU_LOGICAL NCL_noname;
void rmvfre ();

/*********************************************************************
**    E_FUNCTION     : void rmvput (inum)
**       Push a key onto the NCL_key_list list.
**    PARAMETERS
**       INPUT  :
**          nclkey    - Key to push.
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_SUCCESS 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void rmvput (inum)
UM_int4 *inum;
{
  int n;
  if (Srmv_list_init == 0)
  {
     uu_list_init (&Srmv_list,sizeof(int),10,10);
     Srmv_list_init = 1;
  }
  n = *inum;
  uu_list_push (&Srmv_list,&n);
}

/*********************************************************************
**    E_FUNCTION     : void rmvfre ()
**       Free the key list.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_SUCCESS 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void rmvfre ()
{
	if (Srmv_list_init == 1)
	{
		uu_list_free (&Srmv_list);
		Srmv_list_init = 0;
	}
}

/*********************************************************************
**    E_FUNCTION     : int ncl_copy_surf (sf1,sf2)
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_SUCCESS 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_copy_surf (sf1,sf2)
struct NCL_fixed_databag *sf1,*sf2;
{
	int status,rel_num;
			
	rel_num = sf1->rel_num;

	if (rel_num == UM_RBSPLSRF_REL)
		status = ncl_copy_rbsf(sf1, sf2);
	else if (rel_num == NCL_SURF_REL)
		status = ncl_copy_nsf(sf1, sf2);
	else if (rel_num == NCL_TRIMSF_REL)
		status = ncl_copy_trimsf(sf1, sf2);
	else if (rel_num == NCL_REVSURF_REL)
		status = ncl_copy_revsf(sf1, sf2);
	else
		status = UU_FAILURE;

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_redef_copy (sf1,sf2)
**       Create the next auto surface label, then copy a surface.
**    PARAMETERS
*********************************************************************/
int ncl_redef_copy (sf1,sf2)
struct NCL_fixed_databag *sf1,*sf2;
{
	UM_f77_str_ptr fstr2;
	UM_int4 is2;
	struct NCL_curve_rec *leptr = (struct NCL_curve_rec *) sf2;
	int status;
			
	UM_init_f77_str(fstr2, leptr->label, NCL_MAX_LABEL);
	curnam(UM_addr_of_f77_str(fstr2), &is2);
	leptr->label[NCL_MAX_LABEL-1] = '\0';
	leptr->subscr = is2;
	um_init_lablocal (leptr);

	status = ncl_copy_surf (sf1,sf2);

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_bndry_pol (tsfp,tfmat,pol0)
**       Creates a polygon from a trimmed surface UV boundary
**    PARAMETERS
**       INPUT  :
**          tsfp      - trimmed surface.
**          tfmat     - matrix
**       OUTPUT :
**          pol0    - polygon
**    RETURNS      :
**       UU_SUCCESS iff no error, UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_bndry_pol (tsfp,tfmat,pol0)
struct NCL_trimsf_rec *tsfp;
UM_transf tfmat;
ncl_polygon *pol0;
{
	UM_real8 tol8;
	UU_REAL tol;
	UM_srf_boundary bound;
	UM_coord *pts;
	int status,ib,nb,iv,nv,c;

	gettol (&tol8);
	tol = tol8;
	ncl_set_boundary_toler (tol);
	status = ncl_get_boundary (WHOLE_BOUNDARY_LIST,tsfp,&bound);

	if (status == UU_SUCCESS)
	{
		nv = bound.cvpts->cur_cnt;
		ncl_init_polygon (pol0,nv);

		nb = bound.nb;
		pol0->box = (UM_2box *) uu_malloc (nb*sizeof(UM_2box));
		pts = (UM_coord *) UU_LIST_ARRAY (bound.uvpts);

		pol0->num_contours = nb;
		pol0->np = (int *) uu_malloc (nb*sizeof(int));
		for (ib = 0, c = 0; ib < nb; ib++, pts += (nv+1))
		{
			nv = bound.np[ib] - 1;
			iv = (ib == 0)? nv: -nv;
			pol0->np[c] = iv;
			pol0->box[c].xmin = bound.ummx[ib][0];
			pol0->box[c].xmax = bound.ummx[ib][1];
			pol0->box[c].ymin = bound.vmmx[ib][0];
			pol0->box[c].ymax = bound.vmmx[ib][1];
			c++;
			for (iv = 0; iv < nv; iv++)
				uu_list_push (pol0->contour,pts[iv]);
		}
	}
	
	um_free_boundary (&bound);

	return(status);
}

/*********************************************************************
**    E_FUNCTION     : void ncl_addto_pol0 (pts,nv,pol1,typ)
**       Add a contour to a polygon
**    PARAMETERS
**       INPUT  :
**          pts      - contour points
**          nv       - number of points
**          typ      - 2 if 2D, 3 if 3D
**       OUTPUT :
**          pol1    - updated polygon
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_addto_pol0 (pts,nv,pol1,typ)
UU_REAL *pts;
int nv,typ;
ncl_polygon *pol1;
{
	UU_REAL u,v;
	UM_2Dcoord pti;
	int iv,c;
	UU_LOGICAL havebox;

	havebox = (pol1->box != UU_NULL);

	c = pol1->num_contours;
	if (c < 1) c = 0;

	if (c == 0)
		pol1->np[c] = nv;
	else
		pol1->np[c] = -nv;

	pol1->num_contours = c+1;

	for (iv = 0; iv < nv; iv++)
	{
		u = pts[typ*iv];
		v = pts[typ*iv+1];
		if (havebox)
		{
			if (iv == 0)
				pol1->box[c].xmin = pol1->box[c].xmax = u;
			else if (u < pol1->box[c].xmin)
				pol1->box[c].xmin = u;
			else if (u > pol1->box[c].xmax)
				pol1->box[c].xmax = u;
			if (iv == 0)
				pol1->box[c].ymin = pol1->box[c].ymax = v;
			if (v < pol1->box[c].ymin)
				pol1->box[c].ymin = v;
			else if (v > pol1->box[c].ymax)
				pol1->box[c].ymax = v;
		}
		pti[0] = u; pti[1] = v;
		uu_list_push (pol1->contour,pti);
	}
}

/*********************************************************************
**    E_FUNCTION     : void ncl_addto_pol (pts,nv,pol1)
**       Add a contour to a polygon. Intersect if the input polygon is
**       not empty.
**    PARAMETERS
**       INPUT  :
**          pts      - contour points
**          nv       - number of points
**       OUTPUT :
**          pol1    - updated polygon
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_addto_pol (pts,nv,pol1)
UM_coord *pts;
int nv;
ncl_polygon *pol1;
{
	int typ = 3;

	if (pol1->num_contours < 1)
	{
		ncl_addto_pol0 ((UU_REAL *)pts,nv,pol1,typ);
		return;
	}
	else
	{
		ncl_polygon cpol;
		int cnp[2];
		UM_2box cbox[2];
		UM_real8 tol8;
		UU_REAL tol,tolsq;

		gettol (&tol8);
		tol = tol8; tolsq = tol*tol;

		ncl_init_polygon (&cpol,nv+4);
		cpol.np = cnp;
		cpol.box = cbox;
		ncl_boxpol(&cpol);
		ncl_addto_pol0 ((UU_REAL *)pts,nv,&cpol,typ);

		ncl_polygon_clip (NCL_INTOF,pol1,&cpol,pol1,tol,tolsq);

		UU_LIST_FREE (cpol.contour);
	}
}

/*********************************************************************
**    E_FUNCTION     : int ncl_cvpol (ckey,bkey,pol1)
**       Project a curve on a surface, create a list of UV-points,
**       add the result to a polygon struct as a new contour.
**    PARAMETERS
**       INPUT  :
**          skey       - Key of base surface.
**          ckey       - Key of base surface.
**       OUTPUT :
**          pol1    - updated polygon
**    RETURNS      :
**       UU_SUCCESS iff no error
**       UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_cvpol (ckey,skey,pol1)
UU_KEY_ID skey,ckey;
ncl_polygon *pol1;
{
	UM_real8 tol8;
	UU_REAL tol;
	UU_REAL uv[2];
	int status,nv;
	UU_LIST uvpts;
	UM_coord *pts;

	gettol (&tol8);
	tol = tol8;

	uv[0] = uv[1] = 0.5;
	uu_list_init (&uvpts, sizeof(UM_coord), 100, 100);

	status = ncl_cv_project_sfuv(ckey,skey,uv,tol,&uvpts,&nv);
	if (status == UU_SUCCESS)
	{
		pts = (UM_coord *) UU_LIST_ARRAY (&uvpts);
		if (UM_SQDIS_2D (pts[0],pts[nv-1]) < tol*tol)
		{
			nv--;
			uvpts.cur_cnt = nv;
		}

		if (nv < 3)
			status = UU_FAILURE;
		else
			ncl_addto_pol (pts,nv,pol1);
	}

	uu_list_free (&uvpts);
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : void ncl_contour_components (pts,nv,bps,tol)
**       Breaks up a set of uv points into uv  curve components 
**    PARAMETERS
**       INPUT  :
**          ksf       - Key of base surface.
**          ncvs      - number of inner xyz trimming curves.
**          uv        - Starting surface u,v
**       OUTPUT :
**          nclkey    - Key of trimmed surface.
**          ierr      - Non-zero if error.
**    RETURNS      :
**       UU_SUCCESS iff no error
**       UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_contour_components (pts,nv,bps,tol)
UM_2Dcoord *pts;
int nv;
UU_LIST *bps;
UU_REAL tol;
{
	int i;
	UU_REAL tolsq,d0,d1,dot,cmin;
	UM_2Dcoord v0,v1;
	UU_REAL C9 = 0.975528;
	UU_REAL C12 = 0.956773;

	tolsq = tol*tol;

	um_vcmnvc_2d (pts[1],pts[0],v0);
	d0 = UM_DOT_2D (v0,v0);

	for (i = 1; i < nv; i++)
	{
		if (i < nv-1)
			um_vcmnvc_2d (pts[i+1],pts[i],v1);
		else
			um_vcmnvc_2d (pts[0],pts[i],v1);
		d1 = UM_DOT_2D (v1,v1);

		dot = UM_DOT_2D (v0,v1);
		if (dot < 0)
			uu_list_push (bps, &i);
		else
		{
			if (d0 < 0.0005 && d1 < 0.0005)
				cmin = C12;
			else
				cmin = C9;
			if (dot*dot < cmin*d0*d1)
				uu_list_push (bps, &i);
		}

		d0 = d1;
		um_vctovc_2d (v1,v0);
	}
}

/*********************************************************************
**    E_FUNCTION     : int ncl_contour_curve (pts,nv,skey,ukey,tol)
**       Creates a boundary curve with a set of uv points
**    PARAMETERS
**       INPUT  :
**          ksf       - Key of base surface.
**          ncvs      - number of inner xyz trimming curves.
**          uv        - Starting surface u,v
**       OUTPUT :
**          nclkey    - Key of trimmed surface.
**          ierr      - Non-zero if error.
**    RETURNS      :
**       UU_SUCCESS iff no error
**       UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_contour_curve (pts,nv,skey,ukey,tol)
UM_2Dcoord *pts;
int nv;
UU_KEY_ID skey,*ukey;
UU_REAL tol;
{
	UU_LIST bps,polygon;
	UM_coord pt;
	int i,status;

	uu_list_init (&bps, sizeof(int), 10, 10);
	ncl_contour_components (pts,nv,&bps,tol);
	i = nv+1;

	uu_list_init (&polygon, sizeof(UM_coord), i, i);
	pt[2] = 0;
	for (i = 0; i < nv; i++)
	{
		pt[0] = pts[i][0]; pt[1] = pts[i][1];
		uu_list_push (&polygon, &pt);
/*
		if (i > 0) uu_list_push (&bps, &i);
*/
	}

	status = ncl_comp_bndr (skey,&polygon,&bps,ukey);

	uu_list_free (&bps);
	uu_list_free (&polygon);

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_pol_bndry (pol0,pol1,ukey,ibky,ncv)
**       Intersect the current surface boundary with a new trimming.
**       Create the outer and inner trim boundary curves as a result.
**    PARAMETERS
**       INPUT  :
**          skey      - Key of base surface.
**          pol0      - current surface boundary
**          pol1      - new surface trimming
**       OUTPUT :
**          ukey      - Key of outer boundary.
**          ibky      - Keys of inner boundaries.
**          ncv       - number of inner trimming curves.
**    RETURNS      :
**       UU_SUCCESS iff no error
**       UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_pol_bndry (skey,pol0,pol1,ukey,ibky,ncv)
ncl_polygon *pol0,*pol1;
UU_KEY_ID *ukey,**ibky;
int *ncv;
{
	UM_real8 tol8;
	UU_REAL tol,tolsq;
	int status,nc,c,numi,ip,nvp,i,nv;
	int FAILED = UU_FAILURE;
	UM_2Dcoord *pts;
	UU_KEY_ID keyi;

	gettol (&tol8);
	tol = tol8; tolsq = tol*tol;
	*ncv = 0;

	status = UU_SUCCESS;

	if (pol1 != UU_NULL)
		status = ncl_polygon_clip (NCL_INTOF,pol0,pol1,pol0,tol,tolsq);

	if (status == UU_SUCCESS)
	{
		nc = pol0->num_contours;
		if (nc < 0) return (FAILED);
		pts = (UM_2Dcoord *) UU_LIST_ARRAY (pol0->contour);
/*
..... if more than one perimeter, take the one with most points (for now)
*/
		ip = -1; nvp = 2;
		numi = 0;
		for (c = 0; c < nc; c++)
		{
			if (pol0->np[c] > nvp)
			{
				ip = c;
				nvp = pol0->np[c];
			}
			else if (pol0->np[c] < -2)
				numi++;
		}
		if (ip < 0) return (FAILED);

		if (numi > 0)
			*ibky = (UU_KEY_ID *)uu_malloc(2*numi*sizeof(UU_KEY_ID));

		for (c = 0, i = 0, nv = 0; c < nc; c++)
		{
			pts += nv;
			nv = abs (pol0->np[c]);
			if (nv > 2)
			{
				if (c == ip || pol0->np[c] < 0)
				{
					status = ncl_contour_curve (pts,nv,skey,&keyi,tol);
					if (status != UU_SUCCESS) break;
					if (pol0->np[c] > 0)
						*ukey = keyi;
					else
					{
						(*ibky)[i++] = NULLKEY;
						(*ibky)[i++] = keyi;
					}
				}
			}
		}
		if (status == UU_SUCCESS && numi > 0) *ncv = numi;
	}
	return (status);
}

/*********************************************************************
**    E_FUNCTION     : void ncl_extend_cyl
**       Creates a 'full' sphere, cylinder, or cone by the primitive data.
**    PARAMETERS
**       INPUT  :
**          sf1       - Original surface.
**          tfmat     - sf1's matrix
**          typ       - Primitive type
**          primdata  - Primitive data
**       OUTPUT :
**          skey      - Key of the full primitive surface.
**    RETURNS      :
**       UU_SUCCESS iff no error
**       UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void ncl_extend_cyl (e1,tf1,bx1,npt,pts,primdata,skey)
struct NCL_fixed_databag *e1;
UM_transf tf1;
int npt;
UM_coord *pts;
UM_real4 bx1[];
UM_real8 primdata[];
UU_KEY_ID *skey;
{
	int i,status;
	struct UM_rbsplsrf_rec sfp;
	struct UM_circle_rec ci1,ci2;
	UU_REAL t,tmin,tmax,tol;
	UM_transf rottf;
	UM_coord cen,pc0,pc1,pci,pd0;
	UM_vector axs,vr0,vr1,vri,vca;
	struct UM_evsrfout evsrf;
	UU_REAL u,v,h,r,c,d,u0,v0,u1,v1;
	UU_LOGICAL uext,vext;
	UU_REAL alp,bet;
	UU_REAL ALP = 0.9972*UM_TWOPI; /* 359 degrees */

	*skey = NULLKEY;
	uext = vext = UU_FALSE;

	tol = 0.002;

	for (i = 0; i < 3; i++)
	{
		cen[i] = primdata[i];
		axs[i] = primdata[i+3];
	}
	r = primdata[6];
	h = primdata[7];
	if (h < tol || r < tol) return;
/*
..... get points: pc0,pc1 - on the bottom circle, pd0 on the top circle above pc0
*/
	uc_init_evsrfout (e1, &evsrf);

	u = v = 0;
	status = uc_evsrf(UM_POINT,u,v,e1,tf1,&evsrf);
	if (status != UU_SUCCESS) return;
	um_vctovc (evsrf.sp,pc0);

	v = 1;
	status = uc_evsrf(UM_POINT,u,v,e1,tf1,&evsrf);
	if (status != UU_SUCCESS) return;
	um_vctovc (evsrf.sp,pd0);

	um_vcmnvc (pd0,pc0,vca);
	d = UM_DOT (vca,axs);
/*
..... check the height is correct
*/
	if (fabs (d-h) > tol)
	{
/* u-direction is along the axis */
		um_vctovc (pd0,pc1);
		u = 1; v = 0;
		status = uc_evsrf(UM_POINT,u,v,e1,tf1,&evsrf);
		if (status != UU_SUCCESS) return;
		um_vctovc (evsrf.sp,pd0);
		um_vcmnvc (pd0,pc0,vca);
		d = UM_DOT (vca,axs);
		if (fabs (d-h) > tol) return;
		u = 0; v = 0.333;
	}
	else
	{
/* v-direction is along the axis */
		u = 1; v = 0;
		status = uc_evsrf(UM_POINT,u,v,e1,tf1,&evsrf);
		if (status != UU_SUCCESS) return;
		um_vctovc (evsrf.sp,pc1);
		u = 0.333; v = 0;
	}
/* axis goes uo from bottom to top */			
	if (d < 0) um_negvc (axs,axs);

/*
..... get the 1/3 point on the bottom circle to fix the ccw orientation
*/
	status = uc_evsrf(UM_POINT,u,v,e1,tf1,&evsrf);
	if (status != UU_SUCCESS) return;
	um_vctovc (evsrf.sp,pci);
/*
..... starting and ending vectors on the bottom circle
*/
	um_vcmnvc (pc0,cen,vr0);
	d = UM_MAG (vr0);
	if (fabs(d - r) > tol)
	{
/* the primdata center was at the top, move it to the bottom */
		um_vcmnvc (cen,vca,cen);
		um_vcmnvc (pc0,cen,vr0);
		d = UM_MAG (vr0);
		if (fabs(d - r) > tol) return;
	}
	for (i = 0; i < 3; i++) vr0[i] /= d;

	um_vcmnvc (pc1,cen,vr1);
	d = UM_MAG (vr1);
	if (fabs(d - r) > tol) return;
	for (i = 0; i < 3; i++) vr1[i] /= d;
/*
..... fix the ccw orientation
*/
	um_vcmnvc (pci,cen,vri);
	um_cross (vr0,vri,vri);
	if (UM_DOT (vri,axs) < 0)
	{
		for (i = 0; i < 3; i++)
		{
			d = vr0[i]; vr0[i] = vr1[i]; vr1[i] = d;
		}
	}

	alp = um_angle2p (vr0,vr1,axs);
	uext = (alp < ALP); /* circle is not full - so extendable */

	u0 = v0 = 0; u1 = v1 = 1;

	if (uext)
	{
		ci1.dang = ALP;
		bet = (ALP-alp)/2;
		um_rotlntf (cen, axs, -bet, rottf);
		um_vctmtf (vr0, rottf, ci1.svec);
		u0 = bet/ALP; u1 = (alp+bet)/ALP;
	}
	else
	{
		ci1.dang = UM_TWOPI;
		um_vctovc (vr0,ci1.svec);
	}

	c = UM_DOT (pc0,axs);
	d = UM_DOT (pd0,axs);

	tmin = 0; tmax = h;

	for (i = 0; i < npt; i++)
	{
		t = UM_DOT (pts[i],axs);
		if (t-c < tmin ) tmin = t-c;
		if (t-d > tmax ) tmax = t-d;
	}

	vext = (tmin < - tol || tmax > h + tol);

	if (!uext && !vext) return;

	if (vext)
	{
		t = tmax - tmin;
		v0 = -tmin/t; v1 = (h-tmin)/t;
	}
	ci1.radius = ci2.radius = r;
	um_translate_point (cen,tmin,axs,ci1.center);
	um_vctovc (axs,ci1.nvec);
/*
..... second circle has its center at the other end, otherwise it is a copy
..... of the first
*/
	um_translate_point (cen,tmax,axs,ci2.center);
	um_vctovc (axs,ci2.nvec);
	um_vctovc (ci1.svec,ci2.svec);
	ci2.dang = ci1.dang;

	ci1.key = ci2.key = NULLKEY;
	ci1.rel_num = ci2.rel_num = UM_CIRCLE_REL;
	i = 0;
	status = ncl_rldgen (&ci1, &ci2, &sfp, i);

	if (status == UU_SUCCESS)
	{
		*skey = sfp.key;
		bx1[0] = u0; bx1[1] = u1;
		bx1[2] = v0; bx1[3] = v1;
	}
}

/*********************************************************************
**    E_FUNCTION     : void ncl_extend_con
**       Creates a 'full' sphere, cylinder, or cone by the primitive data.
**    PARAMETERS
**       INPUT  :
**          sf1       - Original surface.
**          tfmat     - sf1's matrix
**          typ       - Primitive type
**          primdata  - Primitive data
**       OUTPUT :
**          skey      - Key of the full primitive surface.
**    RETURNS      :
**       UU_SUCCESS iff no error
**       UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void ncl_extend_con (e1,tf1,bx1,npt,pts,primdata,skey)
struct NCL_fixed_databag *e1;
UM_transf tf1;
int npt;
UM_coord *pts;
UM_real4 bx1[];
UM_real8 primdata[];
UU_KEY_ID *skey;
{
	int i,status;
	struct UM_rbsplsrf_rec sfp;
	struct UM_circle_rec ci1,ci2;
	UU_REAL t,tmin,tmax,tol;
	UM_transf rottf;
	UM_coord cen,pc0,pc1,pci;
	UM_vector axs,vr0,vr1,vri;
	struct UM_evsrfout evsrf;
	UU_REAL u,v,u0,v0,u1,v1,b0[2],ci[2];
	UU_LOGICAL uext,vext;
	UU_REAL alp,bet;
	UU_REAL ALP = 0.9972*UM_TWOPI; /* 359 degrees */
			
	UU_REAL alpha,h,h0,h1,talp,r,ha,hb,hc,d;
	UM_coord apx;
	

	*skey = NULLKEY;
	uext = vext = UU_FALSE;

	tol = 0.002;

	for (i = 0; i < 3; i++)
	{
		apx[i] = primdata[i];
		axs[i] = -primdata[i+3];
	}
	alpha = primdata[6];
	talp = tan(alpha);
	h = primdata[7];

	h0 = primdata[8]; /* distance from apex to the top */
	h1 = h0 + h; /* distance from apex to the bottom */

	r = h1*talp; /* radius of the bottom */

	if (h < tol || r < tol) return;

	ha = UM_DOT (apx,axs);
/*
..... ha is the level of apex point on the axis, hb is the top level,
..... hc is the bottom level. Thus h1 = ha-hc, h0 = ha-hb.
..... Get points: pc0,pci,pc1 - on the bottom circle
*/
	uc_init_evsrfout (e1, &evsrf);

	u = v = 0;
	status = uc_evsrf(UM_POINT,u,v,e1,tf1,&evsrf);
	if (status != UU_SUCCESS) return;
	hc = UM_DOT (evsrf.sp, axs);
	d = fabs (ha-hc);
	if (fabs (d-h1) < tol)
	{
/*..... u=v=0 is at the base .....*/
		um_vctovc (evsrf.sp,pc0);
		if (hc > ha)
		{
			um_negvc(axs,axs);
			ha = -ha;
			hc = -hc;
		}

		u = 1;
		status = uc_evsrf(UM_POINT,u,v,e1,tf1,&evsrf);
		if (status != UU_SUCCESS) return;

		d = UM_DOT (evsrf.sp, axs);
		if (fabs (d-hc) < tol)
		{
/* base circle at v=0 */
			um_vctovc (evsrf.sp,pc1);
			b0[0] = 0; b0[1] = 1;
			ci[0] = 0.333; ci[1] = 0;
		}
		else
		{
			u = 0; v = 1;
			status = uc_evsrf(UM_POINT,u,v,e1,tf1,&evsrf);
			if (status != UU_SUCCESS) return;
			d = UM_DOT (evsrf.sp, axs);
			if (fabs (d-hc) > tol) return;
/* base circle at u=0 */
			um_vctovc (evsrf.sp,pc1);
			b0[0] = 1; b0[1] = 0;
			ci[0] = 0; ci[1] = 0.333;
		}
		status = uc_evsrf(UM_POINT,b0[0],b0[1],e1,tf1,&evsrf);
		if (status != UU_SUCCESS) return;
		hb = UM_DOT (evsrf.sp,axs);
	}
	else
	{
/*..... u=v=1 is at the base .....*/
		hb = hc;
		u = v = 1;
		status = uc_evsrf(UM_POINT,u,v,e1,tf1,&evsrf);
		if (status != UU_SUCCESS) return;
		hc = UM_DOT (evsrf.sp, axs);
		d = fabs (ha-hc);
		if (fabs (d-h1) > tol) return;
		um_vctovc (evsrf.sp,pc1);
		if (hc > ha)
		{
			um_negvc(axs,axs);
			ha = -ha;
			hb = -hb;
			hc = -hc;
		}

		u = 0;
		status = uc_evsrf(UM_POINT,u,v,e1,tf1,&evsrf);
		if (status != UU_SUCCESS) return;

		d = UM_DOT (evsrf.sp, axs);
		if (fabs (d-hc) < tol)
		{
/* base circle at v=1 */
			um_vctovc (evsrf.sp,pc0);
			ci[0] = 0.333; ci[1] = 1;
		}
		else
		{
/* base circle at u=1 */
			u = 1; v = 0;
			status = uc_evsrf(UM_POINT,u,v,e1,tf1,&evsrf);
			if (status != UU_SUCCESS) return;

			d = UM_DOT (evsrf.sp, axs);
			if (fabs (d-hc) > tol) return;
			um_vctovc (evsrf.sp,pc0);
			ci[0] = 1; ci[1] = 0.333;
		}
	}

	if (fabs (hb-hc-h) > tol) return;

	status = uc_evsrf(UM_POINT,ci[0],ci[1],e1,tf1,&evsrf);
	if (status != UU_SUCCESS) return;
	um_vctovc (evsrf.sp,pci);

/*
..... axs looks up from base center to apex
*/
	for (i = 0; i < 3; i++) cen[i] = apx[i] - h1*axs[i];

/*
..... starting and ending vectors on the bottom circle
*/
	um_vcmnvc (pc0,cen,vr0);
	d = UM_MAG (vr0);
	if (fabs(d - r) > tol) return;
	for (i = 0; i < 3; i++) vr0[i] /= d;

	um_vcmnvc (pc1,cen,vr1);
	d = UM_MAG (vr1);
	if (fabs(d - r) > tol) return;
	for (i = 0; i < 3; i++) vr1[i] /= d;
/*
..... fix the ccw orientation
*/
	um_vcmnvc (pci,cen,vri);
	um_cross (vr0,vri,vri);
	if (UM_DOT (vri,axs) < 0)
	{
		for (i = 0; i < 3; i++)
		{
			d = vr0[i]; vr0[i] = vr1[i]; vr1[i] = d;
		}
	}

	alp = um_angle2p (vr0,vr1,axs);
	uext = (alp < ALP); /* circle is not full - so extendable */

	u0 = v0 = 0; u1 = v1 = 1;

	if (uext)
	{
		ci1.dang = ALP;
		bet = (ALP-alp)/2;
		um_rotlntf (cen, axs, -bet, rottf);
		um_vctmtf (vr0, rottf, ci1.svec);
		u0 = bet/ALP; u1 = (alp+bet)/ALP;
	}
	else
	{
		ci1.dang = UM_TWOPI;
		um_vctovc (vr0,ci1.svec);
	}

	tmin = 0; tmax = h;

	if (talp > 1)
	{
		ha = ha-0.0005;
	}
	else
	{
		ha = ha - 0.0005/talp;
	}

	for (i = 0; i < npt; i++)
	{
		t = UM_DOT (pts[i],axs);
		if (t > ha) t = ha;
		if (t-hc < tmin ) tmin = t-hc;
		if (t-hc > tmax ) tmax = t-hc;
	}

	vext = (tmin < - tol || tmax > h + tol);

	if (!uext && !vext) return;

	if (vext)
	{
		t = tmax - tmin;
		v0 = -tmin/t; v1 = (h-tmin)/t;
	}

	d = h1 - tmin; 
	um_translate_point (apx,-d,axs,ci1.center);
	ci1.radius = d*talp;
	um_vctovc (axs,ci1.nvec);
/*
..... second circle has its center at the other end, otherwise it is a copy
..... of the first
*/
	d = h0 - tmax; 
	um_translate_point (apx,-d,axs,ci2.center);
	ci2.radius = d*talp;
	um_vctovc (axs,ci2.nvec);
	um_vctovc (ci1.svec,ci2.svec);
	ci2.dang = ci1.dang;

	ci1.key = ci2.key = NULLKEY;
	ci1.rel_num = ci2.rel_num = UM_CIRCLE_REL;
	i = 0;
	status = ncl_rldgen (&ci1, &ci2, &sfp, i);

	if (status == UU_SUCCESS)
	{
		*skey = sfp.key;
		bx1[0] = u0; bx1[1] = u1;
		bx1[2] = v0; bx1[3] = v1;
	}
}

/*********************************************************************
**    E_FUNCTION     : void ncl_extend_sph
**       Creates a 'full' sphere, cylinder, or cone by the primitive data.
**    PARAMETERS
**       INPUT  :
**          sf1       - Original surface.
**          tfmat     - sf1's matrix
**          typ       - Primitive type
**          primdata  - Primitive data
**       OUTPUT :
**          skey      - Key of the full primitive surface.
**    RETURNS      :
**       UU_SUCCESS iff no error
**       UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void ncl_extend_sph (e1,tf1,bx1,primdata,skey)
struct NCL_fixed_databag *e1;
UM_transf tf1;
UM_real4 bx1[];
UM_real8 primdata[];
UU_KEY_ID *skey;
{
	int i,status;
	struct UM_rbsplsrf_rec sfp;
	struct UM_circle_rec ci1;
	UU_REAL tol,tolsq,u,v,u0,v0,u1,v1,r,r2,d;
	UM_transf rottf;
	UM_coord cen,pc[3];
	UM_vector axs,vr0,vr1;
	struct UM_evsrfout evsrf;
	UU_LOGICAL uext,vext;
	UU_REAL alp,bet,sa,ea;
	UU_REAL ALP = 0.9972*UM_TWOPI; /* 359 degrees */

	*skey = NULLKEY;
	u0 = v0 = 0; u1 = v1 = 1;
	uext = vext = UU_FALSE;

	tol = 0.002;

	for (i = 0; i < 3; i++) cen[i] = primdata[i];
	r = primdata[3];
	if (r < tol) return;

	uc_init_evsrfout (e1, &evsrf);

	r2 = r*r;
	tolsq = tol*tol;

	u = 0.5;
	for (i = 0; i < 4; i++)
	{
		v = i; v /= 3;
		status = uc_evsrf(UM_POINT,u,v,e1,tf1,&evsrf);
		if (status != UU_SUCCESS) return;
		d = UM_SQDIS (evsrf.sp,cen);
		if (fabs (d-r2) > tolsq) return;
		um_vctovc (evsrf.sp,pc[i]);
	}

	um_c3_3pt(pc[0],pc[1],pc[2],&ci1);
	um_vcmnvc(pc[0],cen,vr0);
	um_unitvc (vr0,vr0);
	um_vcmnvc(pc[3],cen,vr1);
	um_unitvc (vr1,vr1);

	alp = um_angle2p (vr0,vr1,ci1.nvec);

	vext = (alp < ALP); /* circle is not full - so extendable */	
	if (vext)
	{
		bet = (ALP-alp)/2;
		sa = -bet; ea = alp+bet;
		u0 = bet/ALP; u1 = (alp+bet)/ALP;
	}
	else
	{
		sa = 0.0;
		ea = UM_TWOPI;
	}

	v = 0.5;
	for (i = 0; i < 4; i++)
	{
		u = i; u /= 3;
		status = uc_evsrf(UM_POINT,u,v,e1,tf1,&evsrf);
		if (status != UU_SUCCESS) return;
		d = UM_SQDIS (evsrf.sp,cen);
		if (fabs (d-r2) > tolsq) return;
		um_vctovc (evsrf.sp,pc[i]);
	}

	um_c3_3pt(pc[0],pc[1],pc[2],&ci1);
	um_vctovc (ci1.nvec,axs);

	ci1.key = NULLKEY;
	ci1.rel_num = UM_CIRCLE_REL;
	um_vctovc (cen, ci1.center);
	ci1.radius = r;

	um_vcmnvc(pc[0],cen,vr0);
	um_unitvc (vr0,vr0);
	um_vcmnvc(pc[3],cen,vr1);
	um_unitvc (vr1,vr1);

	alp = um_angle2p (vr0,vr1,axs);
	uext = (alp < ALP); /* circle is not full - so extendable */

	if (!uext && !vext) return;
	if (uext)
	{
		ci1.dang = ALP;
		bet = (ALP-alp)/2;
		um_rotlntf (cen, axs, -bet, rottf);
		um_vctmtf (vr0, rottf, ci1.svec);
		u0 = bet/ALP; u1 = (alp+bet)/ALP;
	}
	else
	{
		ci1.dang = UM_TWOPI;
		um_vctovc (vr0,ci1.svec);
	}
			
	status = ncl_revsrf (cen, axs, sa, ea, &ci1, &sfp);
	if (status == UU_SUCCESS)
	{
		*skey = sfp.key;
		bx1[0] = u0; bx1[1] = u1;
		bx1[2] = v0; bx1[3] = v1;
	}
}

/*********************************************************************
**    E_FUNCTION     : void ncl_extend_rev
**       Creates a 'full' sphere, cylinder, or cone by the primitive data.
**    PARAMETERS
**       INPUT  :
**          sf1       - Original surface.
**          tfmat     - sf1's matrix
**          typ       - Primitive type
**          primdata  - Primitive data
**       OUTPUT :
**          skey      - Key of the full primitive surface.
**    RETURNS      :
**       UU_SUCCESS iff no error
**       UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void ncl_extend_rev (e1,tf1,bx1,skey)
struct NCL_fixed_databag *e1;
UM_transf tf1;
UM_real4 bx1[];
UU_KEY_ID *skey;
{
	struct NCL_fixed_databag e2;
	struct NCL_revsurf_rec *rsrf;
	UU_REAL v0,v1,alp,bet;
	UU_LOGICAL vext;
	UU_REAL ALP = 359;
	int status;

	*skey = NULLKEY;
	rsrf = (struct NCL_revsurf_rec *) e1;

	alp = rsrf->ta - rsrf->sa;

	vext = (alp < ALP); /* circle is not full - so extendable */
	if (!vext) return;

	status = ncl_copy_revsf(e1,&e2);
	if (status != UU_SUCCESS) return;

	rsrf = (struct NCL_revsurf_rec *) &e2;

	bet = (ALP-alp)/2;
	v0 = bet/ALP; v1 = (alp+bet)/ALP;

	rsrf->sa = -bet; rsrf->ta = alp + bet;
	rsrf->closdinv = 0;
	status = ur_update_data_fixed(rsrf);
	if (status == UU_SUCCESS)
	{
		*skey = e2.key;
		bx1[0] = 0; bx1[1] = 1;
		bx1[2] = v0; bx1[3] = v1;
	}
}

/*********************************************************************
**    E_FUNCTION     : int ncl_prim_base (sf1,tfmat,typ,primdata,lsamesf,skey)
**       Creates a 'full' sphere, cylinder, or cone by the primitive data.
**    PARAMETERS
**       INPUT  :
**          sf1       - Original surface.
**          tfmat     - sf1's matrix
**          typ       - Primitive type
**          primdata  - Primitive data
**       OUTPUT :
**          skey      - Key of the full primitive surface.
**    RETURNS      :
**       UU_SUCCESS iff no error
**       UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int ncl_prim_base (sf1,tfmat,typ,primdata,lsamesf,skey)
struct NCL_fixed_databag *sf1;
UM_transf tfmat;
nclsf_prim_type typ;
UM_real8 primdata[];
UU_LOGICAL lsamesf;
UU_KEY_ID *skey;
{
	int i,status;
	struct UM_rbsplsrf_rec sfp;
	UM_int2 lfl_77;
	struct UM_circle_rec ci1;
	UM_coord cen;
	UU_REAL r,tol;

	*skey = NULLKEY;
	if (lsamesf)
	{
		lfl_77 = 1; stunlb (&lfl_77);
	}

	tol = 0.002;
/*
..... sphere. create an nsurf by calling ncl_revsrf
*/
	if (typ == NCLSF_SPHERE)
	{
		UU_REAL sa,ea;

		for (i = 0; i < 3; i++) cen[i] = primdata[i];
		r = primdata[3];
		if (r < tol) return (UU_FAILURE);

		ci1.key = NULLKEY;
		ci1.rel_num = UM_CIRCLE_REL;
      
		um_vctovc (&cen, ci1.center);
		ci1.radius = r;
		ci1.dang = UM_PI;
		um_vctovc (UM_yaxis, ci1.svec);
		um_vctovc (UM_zaxis, ci1.nvec);
		ci1.nvec[2] = -1.0;

		sa = 0.0;
		ea = UM_TWOPI;
		sfp.key = NULLKEY;
					
		status = ncl_revsrf (&cen, UM_yaxis, sa, ea, &ci1, &sfp);
	}
	else
	{
		UM_coord pt1,pt2;
		UM_vector axs,vr,vca;
		struct UM_evsrfout evsrf;
		UU_REAL u,v,h,r,d;
		struct UM_circle_rec ci2;
/*
..... cone or cylinder. create two circles, then a ruled nsurf
*/
		for (i = 0; i < 3; i++) axs[i] = primdata[i+3];
		h = primdata[7];
		if (h < tol) return (UU_FAILURE);
/*
..... first check the height is correct
*/
		uc_init_evsrfout (sf1, &evsrf);

		u = v = 0;
		status = uc_evsrf(UM_POINT,u,v,sf1,tfmat,&evsrf);
		if (status != UU_SUCCESS) return (UU_FAILURE);
		um_vctovc (evsrf.sp,pt1);

		v = 1;
		status = uc_evsrf(UM_POINT,u,v,sf1,tfmat,&evsrf);
		if (status != UU_SUCCESS) return (UU_FAILURE);
		um_vctovc (evsrf.sp,pt2);

		um_vcmnvc (pt2,pt1,vca);
		d = UM_DOT (vca,axs);

		if (fabs(fabs(d)-h) > tol)
		{
			u = 1; v = 0;
			status = uc_evsrf(UM_POINT,u,v,sf1,tfmat,&evsrf);
			if (status != UU_SUCCESS) return (UU_FAILURE);
			um_vctovc (evsrf.sp,pt2);
			
			um_vcmnvc (pt2,pt1,vca);
			d = UM_DOT (vca,axs);

			if (fabs(fabs(d)-h) > tol) return (UU_FAILURE);
		}
			
		ci1.key = ci2.key = NULLKEY;
		ci1.rel_num = ci2.rel_num = UM_CIRCLE_REL;
		ci1.dang = ci2.dang = UM_TWOPI;

		if (typ == NCLSF_CYLINDER)
		{
			for (i = 0; i < 3; i++) cen[i] = primdata[i];
			r = primdata[6];
			if (r < tol) return (UU_FAILURE);

			if (d < 0) um_negvc (axs,axs);

			um_vcmnvc (pt1,cen,vr);
			d = UM_MAG (vr);
			if (fabs(d - r) > tol)
			{
				um_vcmnvc (cen,vca,cen);
				um_vcmnvc (pt1,cen,vr);
				d = UM_MAG (vr);
				if (fabs(d - r) > tol) return (UU_FAILURE);
			}
			for (i = 0; i < 3; i++) vr[i] /= d;

			ci1.radius = ci2.radius = r;
			um_vctovc (cen,ci1.center);
			um_vctovc (axs,ci1.nvec);
			um_vctovc (vr,ci1.svec);
/*
..... second circle has its center at the other end, otherwise it is a copy
..... of the first
*/
			um_vcplvc (ci1.center,vca,ci2.center);
			um_vctovc (axs,ci2.nvec);
			um_vctovc (vr,ci2.svec);
		}
		else if (typ == NCLSF_CONE)
		{
			UM_angle alpha;
			UU_REAL h0,h1,talp;
			UM_coord apx;

			for (i = 0; i < 3; i++)	apx[i] = primdata[i];
			alpha = primdata[6];
			talp = tan(alpha);
			h0 = primdata[8]; /* distance from apex to the top */
			h1 = h0 + h; /* distance from apex to the bottom */

			r = h1*talp; /* radius of the bottom */

			if (r < tol) return (UU_FAILURE);

			um_vcmnvc (pt1,apx,vca);
			if (UM_DOT (vca,axs) < 0) um_negvc(axs,axs);

			for (i = 0; i < 3; i++)
				cen[i] = apx[i] + h1*axs[i];

			um_vcmnvc (pt1,cen,vr);
			d = UM_MAG (vr);
			if (fabs(d - r) > tol)
			{
				um_vcmnvc (pt2,cen,vr);
				d = UM_MAG (vr);
				if (fabs(d - r) > tol) return (UU_FAILURE);
			}

			for (i = 0; i < 3; i++) vr[i] /= d;

			ci1.radius = r;
			um_vctovc (cen,ci1.center);
			um_negvc (axs,ci1.nvec);
			um_vctovc (vr,ci1.svec);
/*
..... ncl_rldgen will not accept a point, so create a very small second circle
..... just below the apex
*/
			um_vctovc (ci1.nvec,ci2.nvec);
			um_vctovc (ci1.svec,ci2.svec);

			tol = 0.0005;
			if (talp > 1)
			{
				h = tol; r = tol*talp;
			}
			else
			{
				r = tol; h = tol/talp;
			}
		
			for (i = 0; i < 3; i++)	ci2.center[i] = apx[i] + h*axs[i];
			ci2.radius = r;
		}
		i = 0;
		status = ncl_rldgen (&ci1, &ci2, &sfp, i);
	}

	if (status != UU_SUCCESS) return (UU_FAILURE);
	*skey = sfp.key;

	if (lsamesf)
		stunlb (&lfl_77);
	else
		ncl_def_color (sfp.key);

	return (0);
}

/*********************************************************************
**    E_FUNCTION     : void plnsrf (p0,p1,p2,p3,skey,asw,ierr)
**       Create a planar rbsplsrf surface from four corner points.
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
void plnsrf (p0,p1,p2,p3,skey,asw,ierr)
UM_int4 *skey;
UM_real8 p0[],p1[],p2[],p3[],*asw;
UM_int2 *ierr;
{
	int i;
	UM_int2 primtyp = 0;
	UM_real8 primdata[16];
	struct NCL_fixed_databag s1;
	struct UM_rbsplsrf_rec *sptr;
	int status = UU_SUCCESS;
	UM_coord cpt[4];
	UU_REAL da[6],prm[16];
	UM_int2 lfl_77,isf,itype;

	status = ncl_get_sf_primdat(skey,&primtyp,primdata);

	for (i = 0; i < 3; i++)
	{
		cpt[0][i] = p0[i];
		cpt[1][i] = p1[i];
		cpt[2][i] = p2[i];
		cpt[3][i] = p3[i];
	}
/*
.....Create surface.
*/
	sptr = (struct UM_rbsplsrf_rec *)&s1;
	sptr->key = 0;
	status = ncl_setup_rbsf(UM_RBSPLSRF_REL, sptr, sizeof(*sptr));
	lfl_77 = 1;
	stunlb (&lfl_77);

	if (status == UU_SUCCESS)
	{
		sptr->ku = 2;
		sptr->kv = 2;
		sptr->nu = 1;
		sptr->nv = 1;
		i = NCLI_SURF;
		status = ncl_create_entity (sptr, i);
		ncl_store_wf1 (sptr->key);
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
				
	ur_update_displayable(sptr->key, UM_NEVERDISPLAYABLE);
	stunlb (&lfl_77);

	if (status == UU_SUCCESS)
		status = ncl_retrieve_data_fixed (sptr);
	if (status == UU_SUCCESS)
	{
		for (i=0;i<16;i++) prm[i] = primdata[i];
		status = ncl_store_prim_data(sptr, NCLSF_PLANE, prm);
	}

	*ierr = status;

	isf = 1;
	itype = 9; /* surface */
	ptdsc3(&sptr->key,&isf,&itype,asw);
	*skey = sptr->key;
}

/*********************************************************************
**    E_FUNCTION     : void extprm (ksf, ncvs, uv, nclkey, ierr)
**       Redefine a primitive type surface by creating a full primitive.
**    PARAMETERS
**       INPUT  :
**          ksf       - Key of base surface.
**          ncvs      - number of inner xyz trimming curves.
**          uv        - Starting surface u,v
**       OUTPUT :
**          nclkey    - Key of trimmed surface.
**          ierr      - Non-zero if error.
**    RETURNS      :
**       UU_SUCCESS iff no error
**       UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void extprm (ky1,ityp1,ky2,k1ext,bx1,ierr)
UM_int4 *ky1,*ky2,*k1ext;
UM_real4 bx1[];
UM_int2 *ityp1,*ierr;
{
	int status;
	UU_KEY_ID skey,nkey;
	struct NCL_fixed_databag sf1;
	UM_transf tf1;
	nclsf_prim_type typ = NCLSF_UNKNOWN;
	UM_real8 primdata[16];
	UM_int2 lfl_77;


	nkey = NULLKEY;
	sf1.key = skey = *ky1;
	status = ncl_retrieve_data_fixed (&sf1);
	if (status == UU_SUCCESS)
		status = uc_retrieve_transf(skey, tf1);
	if (status == UU_SUCCESS)
		status = ncl_get_sf_primdat(&skey,ityp1,primdata);
	if (status == UU_SUCCESS)
		typ = (nclsf_prim_type) *ityp1;
			
	if (status != UU_SUCCESS || typ < NCLSF_SPHERE || typ > NCLSF_REVOLV) return;
/*
..... set ranfile label flag so that all is created as '@UN'
*/
	lfl_77 = 1;
	stunlb (&lfl_77);

	if (typ == NCLSF_REVOLV)
		ncl_extend_rev (&sf1,tf1,bx1,&nkey);
	else if (typ == NCLSF_SPHERE)
		ncl_extend_sph (&sf1,tf1,bx1,primdata,&nkey);
	else
	{
		struct NCL_fixed_databag sf2;
		UM_real8 tol8;
		UU_REAL tol;
		UM_srf_boundary bound;
		UM_coord *pts;
		int npt;

		sf2.key = *ky2;
		status = ncl_retrieve_data_fixed (&sf2);
		if (status == UU_SUCCESS)
		{
			gettol (&tol8);
			tol = tol8;
			ncl_set_boundary_toler (tol);
			status = ncl_get_boundary (WHOLE_BOUNDARY_LIST,&sf2,&bound);
		}
		if (status == UU_SUCCESS)
		{
			pts = (UM_coord *) UU_LIST_ARRAY (bound.cvpts);
			npt = bound.np[0];
			if (typ == NCLSF_CYLINDER)
				ncl_extend_cyl (&sf1,tf1,bx1,npt,pts,primdata,&nkey);
			else if (typ == NCLSF_CONE)
				ncl_extend_con (&sf1,tf1,bx1,npt,pts,primdata,&nkey);
		}
		um_free_boundary (&bound);
	}

	if (nkey > NULLKEY)
	{					
		ur_update_displayable(nkey, UM_NEVERDISPLAYABLE);
		*k1ext = nkey;
		*ierr = 0;
	}
/*
.....Reset ranfile label flag.
*/
	stunlb (&lfl_77);
}

/*********************************************************************
**    E_FUNCTION     : void trmrev (ksf, ncvs, uv, nclkey, ierr)
**       Redefine a revolving type surface by creating a full primitive.
**    PARAMETERS
**       INPUT  :
**          ksf       - Key of base surface.
**          ncvs      - number of inner xyz trimming curves.
**          uv        - Starting surface u,v
**       OUTPUT :
**          nclkey    - Key of trimmed surface.
**          ierr      - Non-zero if error.
**    RETURNS      :
**       UU_SUCCESS iff no error
**       UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void trmrev (ksf,nclkey,inamed,ierr)
UM_int4 *ksf,*nclkey;
UM_int2 *inamed,*ierr;
{
	int status;
	UU_KEY_ID skey;
	struct NCL_fixed_databag sf1,sf2;
	UM_transf tfmat;
	struct  UC_attributedatabag attr1;
	UU_LOGICAL um_is_idmat();
	UU_LOGICAL lsamesf,lrevsf;
	int isub;
	UM_f77_str_ptr fstr1,fstr2;
	char buf1[80],buf2[80];
	UM_int4 is1,is2;
	UM_int2 primtyp = 0;
	nclsf_prim_type typ = NCLSF_UNKNOWN;
	UM_real8 primdata[16];

	UM_init_f77_str (fstr1, buf1, 80);
	UM_init_f77_str (fstr2, buf2, 80);

	*nclkey = NULLKEY;
	*ierr = 0;

	sf1.key = skey = *ksf;
	status = ncl_retrieve_data_fixed (&sf1);
	if (status == UU_SUCCESS)
		status = uc_retrieve_transf(skey, tfmat);
	if (status == UU_SUCCESS)
		status = ncl_get_sf_primdat(&skey,&primtyp,primdata);
	if (status == UU_SUCCESS)
		typ = (nclsf_prim_type) primtyp;
			
	if (status != UU_SUCCESS) goto Err;

	lsamesf = (*inamed != 1);
	lrevsf = (sf1.rel_num == NCL_REVSURF_REL);
/*
..... if not an acceptable primitive type: clone it if new sf, else do nothing
*/ 
	if ((typ < NCLSF_SPHERE || typ > NCLSF_CONE) && !lrevsf)
	{
		if (lsamesf)
		{
			*nclkey = sf1.key;
			return;
		}
		status = ncl_copy_surf(&sf1,&sf2);
		if (status == UU_SUCCESS)
			goto Fin;
		else
			goto Err;
	}
/*
..... if surface of revolution: create the 360-degrees surface
*/
	if (lrevsf)
	{
		struct NCL_revsurf_rec *rsrf;

		if (lsamesf)
			rsrf = (struct NCL_revsurf_rec *) &sf1;
		else
		{
			status = ncl_copy_revsf(&sf1,&sf2);
			rsrf = (struct NCL_revsurf_rec *) &sf2;
		}
		rsrf->sa = 0; rsrf->ta = 360;
		rsrf->closdinv = 1;
		status = ur_update_data_fixed(rsrf);
		if (status != UU_SUCCESS) goto Err;

		if (rsrf->no_tesslst != 0)
			ncl_lst_delete(TESSELLATION_LIST, &rsrf->key);
		if (rsrf->no_displst != 0)
			ncl_lst_delete(DISPLAY_LIST, &rsrf->key);
		if (rsrf->no_xyzbylst != 0)
			ncl_lst_delete(WHOLE_BOUNDARY_LIST, &rsrf->key);

		if (lsamesf)
		{
			*nclkey = rsrf->key;
			uc_display (rsrf);
			return;
		}
		goto Fin;
	}
/*
..... cone, cylinder, or sphere: create a ruled spline surface
*/
	if (lsamesf)
	{
		ncl_get_label_and_subscr (&sf1,buf1,&isub);
		is1 = isub;
		status = uc_retrieve_attr(skey, &attr1);
		if (status != UU_SUCCESS) goto Err;
	}

	status = ncl_prim_base (&sf1,tfmat,typ,primdata,lsamesf,&sf2.key);
	if (status == UU_SUCCESS)
		status = ncl_retrieve_data_fixed (&sf2);
	if (status == UU_SUCCESS)
		ncl_store_prim_data(&sf2,typ,primdata);

	if (status == UU_SUCCESS && lsamesf)
	{
		attr1.key = sf2.key;
		ur_update_attr(&attr1);

		ncl_get_label_and_subscr (&sf2,buf2,&isub);
		is2 = isub;
		ncl_rename_geom(&sf2.key, fstr2, &is2, fstr1, &is1, ierr);
		uc_delete (skey);
		status = ncl_retrieve_data_fixed (&sf2);
	}
	if (status != UU_SUCCESS) goto Err;

Fin:
	if (!um_is_idmat(tfmat))
		status = uc_transform (&sf2, tfmat, (UU_LOGICAL) UU_TRUE);
	if (status != UU_SUCCESS) goto Err;
	*nclkey = sf2.key;
	uc_display (&sf2);
	return;

Err:
	*ierr = 163;
}

/*********************************************************************
**    E_FUNCTION     : void trmbse (ksf, ncvs, uv, nclkey, ierr)
**       Creates a NCL trimmed surface in UNIBASE and return the
**       unibase key of the surface. Fortran interface routine.
**    PARAMETERS
**       INPUT  :
**          ksf       - Key of base surface.
**          ncvs      - number of inner xyz trimming curves.
**          uv        - Starting surface u,v
**       OUTPUT :
**          nclkey    - Key of trimmed surface.
**          ierr      - Non-zero if error.
**    RETURNS      :
**       UU_SUCCESS iff no error
**       UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void trmbse (ksf,nclkey,inamed,ierr)
UM_int4 *ksf,*nclkey;
UM_int2 *inamed,*ierr;
{
	int status;
	UU_KEY_ID skey;
	struct NCL_trimsf_rec *tsf;
	struct NCL_fixed_databag sf1,sf2;
	UM_transf tfmat;
	struct  UC_attributedatabag attr1;
	UU_LOGICAL um_is_idmat();
	UU_LOGICAL lsamesf;
	int isub;
	UM_int2 lfl_77;
	UM_f77_str_ptr fstr1,fstr2;
	char buf1[80],buf2[80];
	UM_int4 is1,is2;

	UM_init_f77_str (fstr1, buf1, 80);
	UM_init_f77_str (fstr2, buf2, 80);

	lsamesf = (*inamed != 1);
	*nclkey = NULLKEY;
	*ierr = 0;

	sf1.key = skey = *ksf;
	status = ncl_retrieve_data_fixed (&sf1);
	if (status == UU_SUCCESS)
	   status = uc_retrieve_transf(sf1.key, tfmat);
	if (status != UU_SUCCESS) goto Err;
			
	if (lsamesf)
	{
		ncl_get_label_and_subscr (&sf1,buf1,&isub);
		is1 = isub;
		uc_retrieve_attr(skey, &attr1);
		lfl_77 = 1;
		stunlb (&lfl_77);
	}

	tsf = (struct NCL_trimsf_rec *)&sf1;
	sf1.key = tsf->bs_key;
	status = ncl_retrieve_data_fixed (&sf1);
	if (status == UU_SUCCESS)
	{
		if (lsamesf)
			status = ncl_class_copy_nsf (&sf1, &sf2, sizeof(struct NCL_fixed_databag));
		else
			status = ncl_copy_surf(&sf1,&sf2);
	}
   
	if (status == UU_SUCCESS && !um_is_idmat(tfmat))
		status = uc_transform (&sf2, tfmat, (UU_LOGICAL) UU_TRUE);
   
	if (status == UU_SUCCESS) 
	{
		if (lsamesf)
		{
			attr1.key = sf2.key;
			ur_update_attr(&attr1);
			ncl_get_label_and_subscr (&sf2,buf2,&isub);
			is2 = isub;

			ncl_rename_geom(&sf2.key, fstr2, &is2, fstr1, &is1, ierr);
			uc_delete (skey);
			stunlb (&lfl_77);
			status = ncl_retrieve_data_fixed (&sf2);
		}
		else
			ncl_store_wf1 (sf2.key);

		*nclkey = sf2.key;
		uc_display (&sf2);
		
		return;
   }

Err:
	*ierr = 163;
	return;
}

/*********************************************************************
**    E_FUNCTION     : void trmrmv (ksf, ncvs, uv, nclkey, ierr)
**       Creates a NCL trimmed surface in UNIBASE and return the
**       unibase key of the surface. Fortran interface routine.
**    PARAMETERS
**       INPUT  :
**          ksf       - Key of base surface.
**          ncvs      - number of inner xyz trimming curves.
**          uv        - Starting surface u,v
**       OUTPUT :
**          nclkey    - Key of trimmed surface.
**          ierr      - Non-zero if error.
**    RETURNS      :
**       UU_SUCCESS iff no error
**       UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void trmrmv (ksf,nclkey,inamed,ierr)
UM_int4 *ksf,*nclkey;
UM_int2 *inamed,*ierr;
{
	UM_int2 lfl_77;
	int i,n,status;
	UU_LOGICAL mall;
	UU_KEY_ID ckey,ukey,skey;
	struct NCL_trimsf_rec *tsfp;
	struct NCL_fixed_databag sf1,sf2;
	UM_transf tfmat;
	UU_LIST tkeylst;
	UU_KEY_ID *tkey = UU_NULL;
	UU_KEY_ID *ibky = UU_NULL;
	UU_LOGICAL lsamesf,match,ltrims;
	int nb,ib;
	int *num = UU_NULL;
	int ncv = 0;

	*nclkey = NULLKEY;
	uu_list_init (&tkeylst,sizeof(UU_KEY_ID),0,10);

	skey = *ksf;
	if (skey == NULLKEY) goto Done;
	sf1.key = skey;
	status = ncl_retrieve_data_fixed (&sf1);
	if (status != UU_SUCCESS) goto Done;

	lsamesf = (*inamed != 1);

	if (Srmv_list_init == 1)
	{
		ncv = Srmv_list.cur_cnt;
		num = (int *) UU_LIST_ARRAY (&Srmv_list);
	}

	ltrims = (sf1.rel_num == NCL_TRIMSF_REL);
	if (!ltrims || ncv < 1)
	{
/*
..... nothing to remove - just copy
*/
		if (lsamesf)
			*nclkey = skey;
		else
		{
			status = ncl_redef_copy (&sf1,&sf2);

			if (status == UU_SUCCESS)
			{
				*nclkey = sf2.key;
				ncl_store_wf1 (sf2.key);
				uc_display (&sf2);
			}
			else
				if (sf2.key != NULLKEY) uc_delete (sf2.key);
		}
		goto Done;
	}

	if (lsamesf)
	{
		*nclkey = skey;
		tsfp = (struct NCL_trimsf_rec *)&sf1;
	}
	else
	{
		status = ncl_redef_copy (&sf1,&sf2);
		if (sf2.key > NULLKEY) uu_list_push (&tkeylst,&sf2.key);
		if (status == UU_SUCCESS)
			status = ncl_retrieve_data_fixed (&sf2);
		if (status != UU_SUCCESS) goto Err;
		*nclkey = sf2.key;
		ncl_store_wf1 (sf2.key);

		tsfp = (struct NCL_trimsf_rec *)&sf2;
	}
/*
.....Remove outer boundary curve
*/
	match = UU_FALSE;
	for (i = 0; i < ncv && !match; i++) match = (num[i] == 0);
	if (match)
	{
		ukey = tsfp->cv_key;
		if (ukey > NULLKEY) uc_delete(ukey);
		ukey = tsfp->uv_key;
		if (ukey > NULLKEY) uc_delete(ukey);

		skey = tsfp->bs_key;

		tsfp->uv_key = tsfp->cv_key = NULLKEY;

		lfl_77 = 1;
		stunlb (&lfl_77);
		ckey = ukey = NULLKEY;
		
		status = ncl_boxbndry(skey,tfmat,&ckey,&ukey,UU_NULL);
/*
.....Reset ranfile label flag.
*/
		stunlb (&lfl_77);

		if (ckey > NULLKEY)
		{
			uu_list_push (&tkeylst,&ckey);
			ur_update_displayable(ckey, UM_NEVERDISPLAYABLE);
			tsfp->cv_key = ckey;
		}
		if (ukey > NULLKEY)
		{
			uu_list_push (&tkeylst,&ukey);
			ur_update_displayable(ukey, UM_NEVERDISPLAYABLE);
			tsfp->uv_key = ukey;
		}
		if (status == UU_SUCCESS)
			status = ur_update_data_fixed(tsfp);
		if (status != UU_SUCCESS) goto Err;
	}
/*
.....Remove inner boundary curve
*/
	nb = tsfp->no_ibndykey/2;
	if (nb > 0)
	{
		UU_LIST iblst;

		ibky = tsfp->ibndykey;

		i = 2*nb;
		uu_list_init (&iblst,sizeof(UU_KEY_ID),i,i);

		mall = UU_FALSE;
		for (i = 0; i < ncv && !mall; i++) mall = (num[i] == -1);
		for (ib = 0; ib < nb; ib++)
		{
			match = mall;;
			for (i = 0; i < ncv && !match; i++)
				match = (num[i] == ib+1);
			if (match)
			{
				ukey = ibky[2*ib];
				if (ukey > NULLKEY) uc_delete(ukey);
				ukey = ibky[2*ib+1];
				if (ukey > NULLKEY) uc_delete(ukey);

			}
			else
			{
				uu_list_push (&iblst,&ibky[2*ib]);
				uu_list_push (&iblst,&ibky[2*ib+1]);
			}
		}
		if (iblst.cur_cnt < 2*nb)
		{
			UU_KEY_ID *keyp;

			nb = iblst.cur_cnt;
	
			ur_delete_data_varlist (tsfp->key, 1);
			ncl_retrieve_data_fixed (tsfp);

			ibky = (UU_KEY_ID *) UU_LIST_ARRAY (&iblst);
			keyp = (UU_KEY_ID *) uu_toolmalloc(nb*sizeof(UU_KEY_ID));

			for (i = 0; i < nb; i++) keyp[i] = ibky[i];

			status = ur_update_data_varlist (tsfp->key, 1, keyp, 1, nb);
			if (keyp != UU_NULL) uu_toolfree(keyp);

		}
		uu_list_free (&iblst);
	}

	if (status == UU_SUCCESS)
	{
		if (tsfp->no_tesslst!=0)
			ncl_lst_delete(TESSELLATION_LIST, &tsfp->key);
		if (tsfp->no_displst!=0)
			ncl_lst_delete(DISPLAY_LIST, &tsfp->key);
		if (tsfp->no_xyzbylst!=0)
			ncl_lst_delete(WHOLE_BOUNDARY_LIST, &tsfp->key);
	
		status = ncl_retrieve_data_fixed (tsfp);
		if (status == UU_SUCCESS)
		{
			uc_display (tsfp);
			goto Done;
		}
	}

Err:
	if (*ierr <= 0) *ierr = 163;
	*nclkey = NULLKEY;
	stunlb (&lfl_77);
	n = tkeylst.cur_cnt;
	if (n > 0)
	{
		tkey = (UU_KEY_ID *) UU_LIST_ARRAY (&tkeylst);
		for (i = 0; i < n; i++) uc_delete(tkey[i]);
	}

Done:
	rmvfre ();
	uu_list_free (&tkeylst);
}
