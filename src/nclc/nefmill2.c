/*****************************************************************************
**   NAME:  nefmill2.c
**    CONTAINS: code to compute boundary curves for fmill.
**        ncl_fmill_set_bndry
**        ncl_fmill_set_params
**        ncl_fmill_refine
**        ncl_fml_fix_bndr
**        ncl_fml_outside_box
**        ncl_fmill_project
**
**    COPYRIGHT 2002 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       nefmill2.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:33
*****************************************************************************/

#include "ncldef.h"
#include "modef.h"
#include "mgeom.h"
#include "mdeval.h"
#include "nccs.h"
#include "uminmax.h"
#include "fmill.h"
#include "nclclip.h"

static UM_int2 IPT = NCLI_POINT;
static UM_int2 IVE = NCLI_VECTOR;
static UM_int2 ISF = 4;
static UM_int2 ISF1 = 2;
static UM_real4 DIR1 = 1;
static UU_REAL BPLM[4] = {0,1,0,1};

typedef struct
{
	int tamode;
	int lpm;
	UU_LOGICAL lpast;
	UU_LOGICAL lext;
	UU_REAL told;
	UU_REAL crad;
	UU_REAL cute;
	UM_vector vta;
	UM_real8 asw;
	UM_real8 asw1;
	UM_real4 dir;
	UM_real4 u;
	UM_real4 v;
	UM_real4 u1;
	UM_real4 v1;
} fml_offset;

/*****************************************************************************
**    I_FUNCTION     : int S_on_extension (pti,u4,v4,ss,tol)
**       Determine if the point projection is on surface extension
**          pti      - projected point
**          u4,v4    - surface paramters
**          ss       - projection result: plane and surface point
**          tol      - tolerance
**       OUTPUT : none
**    RETURNS      :  -1 iff on the extension; else 0
**    SIDE EFFECTS : none
**    WARNINGS     : none
*****************************************************************************/
static int S_on_extension (pti,u4,v4,ss,tol)
UM_coord pti;
UM_real4 u4,v4,ss[];
UU_REAL tol;
{
	int k,ins;
	UM_vector vec;
	UU_REAL d,eps;

	ins = 0;

	if (u4 > UM_FUZZ && 1-u4 > UM_FUZZ && v4 > UM_FUZZ && 1-v4)
		return (ins);

	eps = 4*tol*tol;
	d = 0;
	for (k = 0; k < 3; k++)
	{
		vec[k] = pti[k] - ss[k+4];
		d += vec[k]*ss[k];
	}

	for (k = 0; k < 3; k++)
	{
		vec[k] -= d*ss[k];
	}
	d = UM_DOT (vec,vec);

	if (d > eps) ins = -1;

	return (ins);
}

/*****************************************************************************
**    I_FUNCTION     : void S_tlaxis (offs,u4,v4,pt,vta)
**       calculate current tool axis by projecting on a surface
**          pt      - projected point
**          offs    - surface parameters
**       OUTPUT :
**          vta     - tool axis vector
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*****************************************************************************/
static void S_tlaxis (offs,pt,vta)
fml_offset *offs;
UM_vector vta;
UM_coord pt;
{
	int k;
	UM_real8 pt8[3],vt8[3];
	UM_real4 ss[9];

	for (k = 0; k < 3; k++) pt8[k] = pt[k];
	from_unibase (pt8,pt8,&IPT);

	if (offs->tamode == 1)
	{
		sfpt1 (&offs->asw,pt8,&ISF,&offs->dir,&offs->u,&offs->v,ss);
	}
	else
	{
		sfpt1 (&offs->asw1,pt8,&ISF1,&DIR1,&offs->u1,&offs->v1,ss);
	}

	for (k = 0; k < 3; k++) vt8[k] = ss[k];
	to_unibase (vt8,vt8,&IVE);
	for (k = 0; k < 3; k++) vta[k] = vt8[k];
}

/*****************************************************************************
**    I_FUNCTION     : void S_ps_project (offs,pt,vps,d,vta)
**       Project a point on surface, determine surface parameters and tool
**       axis vector
**          pt      - projected point
**          offs    - surface parameters
**       OUTPUT :
**          vta     - tool axis vector
**          vps,d   - surface plane
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*****************************************************************************/
static void S_ps_project (offs,pt,vps,d,vta)
fml_offset *offs;
UM_vector vps,vta;
UM_coord pt;
UU_REAL *d;
{
	int k;
	UM_coord spt;
	UU_REAL pcon,pco,psi;
	UM_real8 pt8[3],vt8[3];
	UM_real4 ss[9];

	for (k = 0; k < 3; k++) pt8[k] = pt[k];
	from_unibase (pt8,pt8,&IPT);

	sfpt1 (&offs->asw,pt8,&ISF,&offs->dir,&offs->u,&offs->v,ss);

	for (k = 0; k < 3; k++) vt8[k] = ss[k];
	to_unibase (vt8,vt8,&IVE);
	for (k = 0; k < 3; k++) vps[k] = vt8[k];

	for (k = 0; k < 3; k++) pt8[k] = ss[k+4];
	to_unibase (pt8,pt8,&IPT);
	for (k = 0; k < 3; k++) spt[k] = pt8[k];

	pcon = UM_DOT(vps,spt);

	if (offs->tamode == 1)
		um_vctovc (vps,vta);
	else
	{
		for (k = 0; k < 3; k++) pt8[k] = pt[k];
		from_unibase (pt8,pt8,&IPT);
		sfpt1 (&offs->asw1,pt8,&ISF1,&DIR1,&offs->u1,&offs->v1,ss);
		for (k = 0; k < 3; k++) vt8[k] = ss[k];
		to_unibase (vt8,vt8,&IVE);
		for (k = 0; k < 3; k++) vta[k] = vt8[k];

		pco = UM_DOT (vps,vta);
		psi = 1. - pco*pco;
		if (psi > 0)
		{
			psi = sqrt(psi);
			pcon = pcon + offs->crad*(1.-pco) + offs->cute*psi;
		}
	}

	*d = pcon;
}

/*****************************************************************************
**    I_FUNCTION     : int S_is_offpt_inside (offs,npt,uvs,ummx,vmmx,pti,uvi,
**                                                              vlft,dis,tol)
**       Determine if a boundary point offset is inside a surface
**          offs    - surface parameters
**          npt     - number of all boundary points
**          uvs     - all boundary points
**          ummx,vmmx  - boundary box
**          pti     - boundary point (XYZ)
**          uvi     - boundary point (UV)
**          vcl     - offset vector
**          dis     - offset distance
**          tol      - tolerance
**       OUTPUT : none
**    RETURNS      :  1 iff inside, -1 iff outside; 0 if can't tell
**    SIDE EFFECTS : none
**    WARNINGS     : none
*****************************************************************************/
static int S_is_offpt_inside (offs,npt,uvs,ummx,vmmx,pti,uvi,vcl,dis,tol)
fml_offset *offs;
int npt;
UM_coord *uvs;
UM_2Dcoord ummx,vmmx;
UM_coord pti,uvi;
UM_vector vcl;
UU_REAL dis,tol;
{
	UM_coord ptl,uvl;
	int ins,k;
	UM_real8 pt8[3];
	UM_real4 u4,v4;
	UM_real4 ss[9];

	uvl[2] = 0;
	um_translate_point (pti,dis,vcl,ptl);

	u4 = (UM_real4) uvi[0];
	v4 = (UM_real4) uvi[1];

	for (k = 0; k < 3; k++) pt8[k] = ptl[k];
	from_unibase (pt8,pt8,&IPT);
	sfpt1 (&offs->asw,pt8,&ISF,&offs->dir,&u4,&v4,ss);
	uvl[0] = u4; uvl[1] = v4;

	ins = um_cshape_inschk1 (uvs,npt,uvl,ummx,vmmx);
	if (ins == 0)
	ins = S_on_extension (ptl,u4,v4,ss,tol);

	return (ins);
}

/*****************************************************************************
**    I_FUNCTION     : int S_fmill_pm1 (offs,pti,vti,uvi,ummx,vmmx,vta0,tol)
**       Determine if, at a given boundary point, the tool axis vector cross
**       the boundary tangent vector points inside the surface.
**    PARAMETERS
**       INPUT  :
**          offs    - surface parameters
**          npt     - number of all boundary points
**          uvs     - all boundary points
**          ummx,vmmx  - boundary box
**          pti     - boundary point (XYZ)
**          uvi     - boundary point (UV)
**          vti     - boundary tangent vector
**          vta0    - initial tool axis vector
**          tol      - tolerance
**       OUTPUT : none
**    RETURNS      :  1 iff inside, -1 iff outside; 0 if can't tell
**    SIDE EFFECTS : none
**    WARNINGS     : none
*****************************************************************************/
static int S_fmill_pm1 (offs,pti,vti,uvi,npt,uvs,ummx,vmmx,vta0,tol)
fml_offset *offs;
UM_coord uvi,pti;
UM_vector vti,vta0;
int npt;
UM_coord *uvs;
UM_2Dcoord ummx,vmmx;
UU_REAL tol;
{
	UM_vector vta,vlft;
	UU_REAL d;
	UU_REAL tolsq = 4*tol*tol;
	UU_REAL del = 5*tol;
	int ins,ins1;

	ins = 0;
	if (offs->tamode == 0)
		um_vctovc (vta0,vta);
	else
	{
		if (offs->tamode == 1)
		{
			offs->u = (UM_real4) uvi[0];
			offs->v = (UM_real4) uvi[1];
		}
		S_tlaxis (offs,pti,vta);
	}

	um_cross (vta,vti,vlft);
	d = UM_DOT (vlft,vlft);
	if (d < tolsq) return (ins);
	um_unitvc (vlft,vlft);

	ins = S_is_offpt_inside (offs,npt,uvs,ummx,vmmx,pti,uvi,vlft,del,tol);
	if (ins <= 0)
	{
/*
..... qar 95284 - inconclusive if both left and right offsets are outside
*/
		ins1 = S_is_offpt_inside (offs,npt,uvs,ummx,vmmx,pti,uvi,vlft,-del,tol);
		if (ins == -1 && ins1 == -1)
			ins = 0;
		else if (ins == 0)
			ins = -ins1;
	}

	return (ins);
}

/*****************************************************************************
**    I_FUNCTION     : int S_fmill_pm (offs,npt,pts,vt,uvs,ummx,vmmx,vta0,tol)
**          Set the flag that would reverse the tool axis vector vta,
**          if necessary, so that vta cross a boundary tangent vector points
**          inside the surface.
**    PARAMETERS
**          offs    - surface parameters
**          npt     - number of all boundary points
**          uvs     - all boundary points
**          ummx,vmmx  - boundary box
**          vta0    - initial tool axis vector
**          tol      - tolerance
**       OUTPUT :  offs->lpm flag
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*****************************************************************************/
static void S_fmill_pm (offs,npt,pts,vt,uvs,ummx,vmmx,vta0,tol)
fml_offset *offs;
int npt;
UM_coord *uvs,*pts;
UM_vector vta0;
UM_vector *vt;
UM_2Dcoord ummx,vmmx;
UU_REAL tol;
{
	int i,ins;

	offs->lpm = 0;

	evsf_ext_rst();

	for (i = 1; i < npt-1; i++)
	{
		ins = S_fmill_pm1 (offs,pts[i],vt[i],uvs[i],npt,uvs,ummx,vmmx,vta0,tol);

		if (ins != 0)
		{
			offs->lpm = ins; break;
		}
	}

	if (offs->lpast) offs->lpm = -offs->lpm;
}

/*****************************************************************************
**    I_FUNCTION     : int S_offset_vec (offs,vta,vds,voff)
**          Calculate the current offset vector by the tool axis and the
**          boundary tangent vectors
**    PARAMETERS
**       INPUT  :
**          offs    - surface parameters
**          vta     - tool axis vector
**          vds     - tangent vector
**       OUTPUT :
**          voff    - offset vector
**    RETURNS      : UU_SUCCESS / UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*****************************************************************************/
static int S_offset_vec (offs,vta,vds,voff)
fml_offset *offs;
UM_vector vta,vds,voff;
{
	UU_REAL d;
	int k;

	if (offs->lpm == 1)
		um_cross (vta,vds,voff);
	else
		um_cross (vds,vta,voff);

	d = UM_DOT (voff,voff);
	if (d < UM_DFUZZ)
		return (UU_FAILURE);
	else
	{
		d = sqrt(d);
		for (k = 0; k < 3; k++) voff[k] = voff[k]/d;
	}

	return (UU_SUCCESS);
}

/*****************************************************************************
**    I_FUNCTION   : int S_move_offset (offs,vds,voff,pt,poff,dis,tol)
**          Move (inside or outside) by dis from the boundary with
**          the "normal,PS" condition.
**    PARAMETERS
**       INPUT  :
**          offs    - surface parameters
**          pt      - boundary point
**          vds     - boundary tangent vector
**          dis     - offset distance
**          tol     - tolerance
**       OUTPUT :
**          poff    - offset point
**          voff    - offset vector
**    RETURNS      : UU_SUCCESS / UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*****************************************************************************/
static int S_move_offset (offs,vds,voff,pt,poff,dis,tol)
fml_offset *offs;
UM_vector vds,voff;
UM_coord pt,poff;
UU_REAL dis,tol;
{
	UM_vector vps,vta,vdel;
	int j,jmax,k,l,lmax,status;
	UU_REAL a,b,si,p,d,del;
	UU_REAL tol1 = 0.5*tol;

	lmax = 25;
	jmax = 30;
	status = UU_SUCCESS;

	um_vctovc (pt,poff);
	S_tlaxis (offs,pt,vta);

	del = dis;
	status = S_offset_vec (offs,vta,vds,voff);

	for (l = 0; l < lmax && status == UU_SUCCESS; l++)
	{
		um_translate_point (poff,del,voff,poff);
/*
..... logic from mover.f to move the point onto PS and the plane through
..... the original boundary point-vector (pt,vds)
*/
		for (j = 0; j < jmax; j++)
		{
			S_ps_project (offs,poff,vps,&d,vta);
			si = UM_DOT(vps,vds);
			p = UM_DOT(vps,poff) - d;
			a = -p/(1.-si*si); b = a*si;
			if (fabs(a) < tol1 && fabs(b) < tol1) break;
			for (k = 0; k < 3; k++)
				poff[k] += (a*vps[k] + b*vds[k]);
		}
		if (j >= jmax)
			status = UU_FAILURE;
		else
			status = S_offset_vec (offs,vta,vds,voff);
/*
..... distance from poff to (pt,vds) line should be close to the offset
..... parameter
*/
		if (status == UU_SUCCESS)
		{
			um_vcmnvc (poff,pt,vdel);
			d = UM_DOT (vdel,voff);

			del = dis - d;
			if (fabs(del) < tol) break;
		}
	}

	if (l >= lmax) status = UU_FAILURE;

	return (status);
}

/*****************************************************************************
**    I_FUNCTION     : int S_ps_offset (offs,ncc,cvpts,vlst,uvpts,dis,tol,lcheck)
**          Offset the boundary curve, inside or outside, by dis, with
**          the "normal,PS[,sf1]" condition.
**    PARAMETERS
**       INPUT  :
**          offs    - surface parameters
**          ncc     - number of boundary points
**          cvpts   - boundary points (XYZ)
**          vlst    - boundary tangent vectors
**          uvpts   - boundary points (UV)
**          dis     - offset distance
**          tol     - tolerance
**			lcheck	- check start/end flag
**       OUTPUT :
**          ncc     - number of offset points
**          uvpts   - offset points (UV)
**    RETURNS      : UU_SUCCESS / UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*****************************************************************************/
int S_ps_offset (offs,ncc,cvpts,vlst,uvpts,dis,tol,lcheck)
fml_offset *offs;
int *ncc;
UU_LIST *cvpts,*vlst,*uvpts;
UU_REAL dis,tol;
UU_LOGICAL lcheck;
{
	UM_coord *uvs,*vt,*pts;
	UM_vector vnx,voff,vds,VNUL;
	UM_coord cpt;
	UU_REAL co,co2,csmin,d,u,v;
	UU_REAL eps = 0.01*tol*tol;
	UM_real8 fac;
	int i,k,npt,tonp;
	UM_coord pti,uvi;
	int status = UU_SUCCESS;

	csmin = 0.9238795; /* corner if more than 22.5 degrees */

	vt = (UM_vector *) UU_LIST_ARRAY (vlst);
	pts = (UM_coord *) UU_LIST_ARRAY (cvpts);
	uvs = (UM_coord *) UU_LIST_ARRAY (uvpts);

	ncl_get_fml_tonp (&tonp);

	if (offs->lext)
	{
		fac = 2.;
		evsf_ext_set (&ISF,&fac);
	}
	else
		evsf_ext_rst();

	u = uvs[0][0];
	v = uvs[0][1];
	if (offs->lext)
	{
		u = 0.5*u + 0.25; v = 0.5*v + 0.25;
	}

	offs->u = (UM_real4) u;
	offs->v = (UM_real4) v;

	uvi[2] = 0;
	um_nullvc (VNUL);

	npt = *ncc;

	for (i = 0; i < npt-1; i++)
	{
		d = UM_DOT (vt[i],vt[i]);
		if (d < eps) return (UU_FAILURE);
		um_unitvc (vt[i],vt[i]);
	}

	um_vctovc (vt[0],vds);
	um_vctovc (vt[0],vt[npt-1]);

	for (i = 0; i < npt-1; i++)
	{
		um_vctovc (vt[i+1],vnx);

		status =
		S_move_offset(offs,vds,voff,pts[i],pti,dis,tol);
		if (status != UU_SUCCESS) return (status);
		uvs[i][0] = offs->u; uvs[i][1] = offs->v;
		um_vctovc (pti,pts[i]);
/*
..... offset corners
*/
		co = UM_DOT (vds,vnx);

		if (co <= csmin)
		{
			for (k = 0; k < 3; k++) cpt[k] = pts[i+1][k];

			if (UM_DOT (voff,vnx) >= 0)
			{
				status =
				S_move_offset(offs,vds,voff,cpt,pti,dis,tol);
			}
			else
			{
				um_vcplvc (vds,vnx,vds);
				d = UM_DOT (vds,vds);
				if (d < eps) return (UU_FAILURE);
				um_unitvc (vds,vds);

				co2 = sqrt ((1.+co)/2.);
				d = dis/co2;
				status =
				S_move_offset(offs,vds,voff,cpt,pti,d,tol);

				if (lcheck && i == npt-2)
				{
					uvi[0] = offs->u; uvi[1] = offs->v;
					uu_list_insert(uvpts,0,uvi);
					uu_list_insert(cvpts,0,pti);
					uu_list_insert(vlst,0,VNUL);

					vt = (UM_vector *) UU_LIST_ARRAY (vlst);
					pts = (UM_coord *) UU_LIST_ARRAY (cvpts);
					uvs = (UM_coord *) UU_LIST_ARRAY (uvpts);
					i++; npt++;
				}
			}
			if (status != UU_SUCCESS) return (status);
			uvi[0] = offs->u; uvi[1] = offs->v;
			uu_list_insert(uvpts,i+1,uvi);
			uu_list_insert(cvpts,i+1,pti);
			uu_list_insert(vlst,i+1,VNUL);

			vt = (UM_vector *) UU_LIST_ARRAY (vlst);
			pts = (UM_coord *) UU_LIST_ARRAY (cvpts);
			uvs = (UM_coord *) UU_LIST_ARRAY (uvpts);
			i++; npt++;
		}
		else if (i == npt-2)
		{
/*
..... if last point is not a corner, offset by the last tangent
*/
			i++; npt++;
			status =
			S_move_offset(offs,vds,voff,pts[i],pti,dis,tol);
			if (status != UU_SUCCESS) return (status);
			uvs[i][0] = offs->u; uvs[i][1] = offs->v;
			um_vctovc (pti,pts[i]);
		}

		um_vctovc (vnx,vds);
	}

	npt--;

	vlst->cur_cnt = cvpts->cur_cnt = uvpts->cur_cnt = npt;
	*ncc = npt;

	return (0);
}

/*****************************************************************************
**    I_FUNCTION     : void S_shuffle (ptlst,vclst,uvlst,tol,ncc)
**      Shuffle a closed curve, if necessary, so that it starts
**      away from corners.
**    PARAMETERS
**       INPUT  :
**          ncc     - number of boundary points
**          cvpts   - boundary points (XYZ)
**          vlst    - boundary tangent vectors
**          uvpts   - boundary points (UV)
**          tol     - tolerance
**       OUTPUT :
**          ncc     - fixed number of boundary points
**          cvpts   - fixed boundary points (XYZ)
**          vlst    - fixed boundary tangent vectors
**          uvpts   - fixed boundary points (UV)
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*****************************************************************************/
static void S_shuffle (ptlst,vclst,uvlst,dis,tol,ncc)
UU_LIST *ptlst,*vclst,*uvlst;
UU_REAL dis,tol;
int *ncc;
{
	int i,n1,i0,i1,ip,ic;
	UU_REAL cco,dd,dmax;
	UU_REAL csmin = 0.9238795; /* corner if more than 22.5 degrees */
	UM_coord *pp,*uv,pti,uvi;
	UM_vector *vv,vv0,vv1,vci;

	n1 = *ncc;

	pp = (UM_coord *) UU_LIST_ARRAY(ptlst);
	vv = (UM_vector *) UU_LIST_ARRAY(vclst);
	uv = (UM_coord *) UU_LIST_ARRAY (uvlst);

/*
..... qar 95284 - if there is a long segment just start at its midpoint
*/
	dmax = 15.99*dis*dis;
	i0 = i1 = -1;

	for (i = 0; i < n1-2; i++)
	{
		dd = UM_SQDIS (pp[i],pp[i+1]);
		if (dd > dmax)
		{
			dmax = dd;
			i0 = i;
			i1 = i+1;
		}
	}
	if (i0 >= 0) goto Shu;

	um_unitvc (vv[n1-1],vv1);
	um_unitvc (vv[0],vv0);

	cco = UM_DOT(vv0,vv1);
/*
..... if no corner at 0, do not shuffle
*/
	if (cco > csmin) return;


	dmax = -1;
	i0 = i1 = ip = ic = 0;

	for (i = 0; i < n1-2; i++)
	{

		um_vctovc (vv[i],vv0);
		dd = UM_DOT (vv0,vv0);
		if (dd < UM_DFUZZ) continue;

		um_vctovc (vv[i+1],vv1);
		dd = UM_DOT (vv1,vv1);
		if (dd < UM_DFUZZ) continue;

		um_unitvc (vv0,vv0);
		um_unitvc (vv1,vv1);

		cco = UM_DOT(vv0,vv1);

		if (cco > csmin) continue;

		ic = i+1;

		dd = UM_SQDIS (pp[ip],pp[ic]);
		if (dd > dmax)
		{
			dmax = dd;
			i0 = ip;
			i1 = ic;
		}
		ip = ic;
	}

	if (dmax > 0)
	{
		dd = UM_SQDIS (pp[ip],pp[n1-1]);
		if (dd > dmax)
		{
			i0 = ip;
			i1 = n1-1;
		}
	}

	if (i1 == 0) i1 = n1-1;

	if (i1 < i0+1) return;

Shu:
	if (i1 == i0+1)
	{
		um_middlept (pp[i0],pp[i1],pti);
		um_vcmnvc (pp[i1],pp[i0],vci);
		uu_list_insert (ptlst,i1,pti);
		uu_list_insert (vclst,i1,vci);

		um_middlept (uv[i0],uv[i1],uvi);
		uu_list_insert (uvlst,i1,uvi);

		pp = (UM_coord *) UU_LIST_ARRAY(ptlst);
		vv = (UM_vector *) UU_LIST_ARRAY(vclst);
		uv = (UM_coord *) UU_LIST_ARRAY (uvlst);

		n1++;
		i1++;
	}

	i0 = (i0 + i1)/2;
	n1++;

	for (i = 0; i <= i0; i++)
	{
		pp = (UM_coord *) UU_LIST_ARRAY(ptlst);
		vv = (UM_vector *) UU_LIST_ARRAY(vclst);
		uv = (UM_coord *) UU_LIST_ARRAY (uvlst);

		um_vctovc (pp[i],pti);
		um_vctovc (vv[i],vci);
		um_vctovc (uv[i],uvi);

		uu_list_push (ptlst,pti);
		uu_list_push (vclst,vci);
		uu_list_push (uvlst,uvi);
	}

	uu_list_delete (ptlst,0,i0);
	uu_list_delete (vclst,0,i0);
	uu_list_delete (uvlst,0,i0);

	*ncc = n1;
}

/*****************************************************************************
**    I_FUNCTION     : void S_shuffle (ptlst,pt,tol)
**      Shuffle a closed curve to start from pt, insert pt if neccessary.
**    PARAMETERS
**       INPUT  :
**          cvpts   - boundary points (XYZ)
**          pt      - point on the boundary
**          tol     - tolerance
**       OUTPUT :
**          cvpts   - fixed boundary points (XYZ)
=**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*****************************************************************************/
static void S_shuffle_bound_pt(ptlst,pt,tol)
UU_LIST *ptlst;
UM_coord pt;
UU_REAL tol;
{
	int i,i0,i1, npt;
	UM_coord *pp,pti;

	pp = (UM_coord *) UU_LIST_ARRAY(ptlst);
	npt = UU_LIST_LENGTH(ptlst);

	for (i = 0; i < npt-1; i++)
	{
		if (um_point_in_segment_2d(pt,pp[i],pp[i+1],tol))	
		{
			i0 = i;
			break;
		}
	}

	i1 = i0 + 1;
	uu_list_insert (ptlst,i1,pt);

	for (i = 1; i <= i1; i++)
	{
		pp = (UM_coord *) UU_LIST_ARRAY(ptlst);
		um_vctovc (pp[i],pti);
		uu_list_push (ptlst,pti);
	}

	uu_list_delete (ptlst,0,i1);
}

/*****************************************************************************
**    I_FUNCTION     : S_trim_bound_pt(ptlst,pt1,pt2,tol)
**      trim a open curve to start from pt1 and end with pt2.
**    PARAMETERS
**       INPUT  :
**          cvpts   - boundary points (XYZ)
**          pt1      - the first point on the boundary
**          pt2      - the second point on the boundary
**          tol     - tolerance
**       OUTPUT :
**          cvpts   - timmed boundary points (XYZ)
=**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*****************************************************************************/
static void S_trim_bound_pt(ptlst,pt1,pt2,tol)
UU_LIST *ptlst;
UM_coord pt1,pt2;
UU_REAL tol;
{
	int i,i0,i1, npt;
	UM_coord *pp,pti;

	pp = (UM_coord *) UU_LIST_ARRAY(ptlst);
	npt = UU_LIST_LENGTH(ptlst);
/*
.....The end point
*/
	for (i = 0; i < npt-1; i++)
	{
		if (um_point_in_segment_2d(pt2,pp[i],pp[i+1],tol))	
		{
			i0 = i;
			break;
		}
	}

	i1 = i0 + 1;
	uu_list_insert (ptlst,i1,pt2);
	uu_list_delete(ptlst,i1+1,npt-i1);
/*
.....The start point
*/
	for (i = 0; i < npt-1; i++)
	{
		if (um_point_in_segment_2d(pt1,pp[i],pp[i+1],tol))	
		{
			i0 = i;
			break;
		}
	}
	i1 = i0 + 1;
	uu_list_insert (ptlst,i1,pt1);

	uu_list_delete (ptlst,0,i1);
}

/*****************************************************************************
**    I_FUNCTION     : UU_LOGICAL ncl_selfint_bound_segements(ptlst,tol)
**          Check if there are duplicate segments on the surface boundary curve
**    PARAMETERS
**       INPUT  :
**          offs    - surface parameters
**          npt     - number of points
**          ptlst   - list of 3D points
**          vclst   - list of tangencies
**          uvlst   - list of uv points
*****************************************************************************/
UU_LOGICAL ncl_selfint_bound_segements(npts,ptlst,n1,n2)
int npts,*n1,*n2;
UU_LIST *ptlst;
{
	int i,i1,n3,n4;
	UU_REAL dis2;
	UM_coord *pts,ps,pe;
	UU_REAL um_dcccc();
	UU_LOGICAL lselfint = UU_FALSE;

	pts = (UM_coord *) UU_LIST_ARRAY (ptlst);
	um_vctovc(pts[0],ps);

	n3 = n4 = npts;
	for (i = 1; i < npts; i++)
	{
		dis2 = UM_DCCCC(ps,pts[i]);
		if (dis2 < UM_FUZZ * UM_FUZZ)
		{
			n3 = i;
			break;
		}
	}

	*n1 = n3;
	if (n3 < npts-1)
	{
		i1 = 2;
		for (i = n3+1; i< npts;i++,i1++)
		{
			dis2 = UM_DCCCC(pts[i],pts[npts-i1]);
			if (dis2 > UM_FUZZ * UM_FUZZ)
			{
				n4 = i;
				lselfint = UU_TRUE;
				break;
			}
		}
		*n2 = n4-1;
	}

	return lselfint ;
}

/*****************************************************************************
**    I_FUNCTION     : int S_fmill_cv_offset (cvpoint,cvtang,npts,n1,n2,lr,
**																	vmod,dis,tol)
**          Offset the self intersected boundary curve separately, and then 
**			make one boudnary curve
**    PARAMETERS
**       INPUT  :
**          ptlst   - list of points
**          vclst   - list of tangencies
**          npts    - number of points
**          n1      - start index of common points
**          n2		- end index of common points
**          vmod    - initial tool axis vector
**          dis     - offset distance
**          tol     - inches tolerance
**       OUTPUT :
**          uvlst  - new list of uv points
**    RETURNS      : 
**			The nuber of points
**    SIDE EFFECTS : none
**    WARNINGS     : none
*****************************************************************************/
int S_fmill_cv_offset (cvpoint,cvtang,npts,n1,n2,lr,vmod,dis,tol)
int npts,n1,n2,lr;
UM_vector vmod;
UU_LIST *cvpoint,*cvtang;
UU_REAL dis,tol;
{
	int i,nc,nc1,nc2,n0,n3,n4,npt0,npt1,npt2,tonp;
	UU_REAL dln;
	UM_2Dcoord vln;
	UM_coord *vcs,*pts,*pts0,*pts1,*pts2,*ptsi,pt1,pt2;
	UU_LIST ptlst0,vclst0,ptlst1,vclst1,ptlst2,vclst2;
	UU_LIST iobuf;
/*
.....Tool location flag
*/
	ncl_get_fml_tonp (&tonp);
	pts = (UM_coord *) UU_LIST_ARRAY (cvpoint);
	vcs = (UM_vector *) UU_LIST_ARRAY (cvtang);
/*
.....Initialize
*/
	n4 = n1+1;
	uu_list_init(&ptlst1,sizeof(UM_coord),n4,n4);
	uu_list_init(&vclst1,sizeof(UM_vector),n4,n4);

	n0 = n2-n1+1;
	uu_list_init(&ptlst0,sizeof(UM_coord),n0,n0);
	uu_list_init(&vclst0,sizeof(UM_vector),n0,n0);

	n3 = npts-n2-n0+1;
	uu_list_init(&ptlst2,sizeof(UM_coord),n3,n3);
	uu_list_init(&vclst2,sizeof(UM_vector),n3,n3);
/*
.....The first curve
*/
	uu_list_push_multiple(&ptlst1,n4,pts);
	uu_list_push_multiple(&vclst1,n4,vcs);
/*
.....The common curve
*/
	uu_list_push_multiple(&ptlst0,n0,&pts[n1]);
	uu_list_push_multiple(&vclst0,n0,&vcs[n1]);
/*
.....The second curve
*/
	uu_list_push_multiple(&ptlst2,n3,&pts[n2]);
	uu_list_push_multiple(&vclst2,n3,&vcs[n2]);
/*
.....Offset the first curve
*/
	nc1 = ncl_cv_offset(NULLKEY,&ptlst1,&vclst1,n4,lr,vmod,dis,0,0.,0.,tol,
		0,0);
	pts1 = (UM_coord *) UU_LIST_ARRAY (&ptlst1);
	npt1 = UU_LIST_LENGTH (&ptlst1);
/*
.....Offset the second curve
*/
	nc2 = ncl_cv_offset(NULLKEY,&ptlst2,&vclst2,n3,lr,vmod,dis,0,0.,0.,tol,
		0,0);
	pts2 = (UM_coord *) UU_LIST_ARRAY (&ptlst2);
	npt2 = UU_LIST_LENGTH (&ptlst2);

	pts0 = (UM_coord *) UU_LIST_ARRAY (&ptlst0);
	uu_list_init (&iobuf, sizeof(UM_pointd), 2, 2);
/*
.....Extend common boundary
*/
	if (tonp == TL_PAST)
	{
/*
.....end point
*/
		um_vcmnvc_2d(pts0[n0-1],pts0[n0-2],vln);
		dln = UM_MAG_2D (vln);
		vln[0] /= dln; vln[1] /= dln;
		um_vcplbvc_2d(pts0[n0-1], 1.1*dis, vln, pt1);
/*
.....start point
*/
		um_vcmnvc_2d(pts0[0],pts0[1],vln);
		dln = UM_MAG_2D (vln);
		vln[0] /= dln; vln[1] /= dln;
		um_vcplbvc_2d(pts0[0], 1.1*dis, vln, pt2);

		uu_list_push(&ptlst0, pt1);
		uu_list_insert(&ptlst0, 0, pt2);
	
		pts0 = (UM_coord *) UU_LIST_ARRAY (&ptlst0);
		n0 = n0 + 2;
	}

	for (i = 0; i < n0-1; i++)
	{					
		um_vcmnvc_2d(pts0[i+1],pts0[i],vln);
		dln = UM_MAG_2D (vln);
		vln[0] /= dln; vln[1] /= dln;
		nc1 = um_isect_cls_bndry1(pts0[i],pts0[i+1],vln,dln,pts1,npt1,tol,&iobuf);
		if (nc1 == 1)
		{
			ptsi = (UM_coord *) UU_LIST_ARRAY (&iobuf);
			um_vctovc(ptsi[0],pt1);
			S_shuffle_bound_pt(&ptlst1,pt1,tol);
			break;
		}
	}
	UU_LIST_EMPTY (&iobuf);

	for (i = 0; i < n0-1; i++)
	{					
		um_vcmnvc_2d(pts0[i+1],pts0[i],vln);
		dln = UM_MAG_2D (vln);
		vln[0] /= dln; vln[1] /= dln;
		nc2 = um_isect_cls_bndry1(pts0[i],pts0[i+1],vln,dln,pts2,npt2,tol,&iobuf);
		if (nc2 == 1)
		{
			ptsi = (UM_coord *) UU_LIST_ARRAY (&iobuf);
			um_vctovc(ptsi[0],pt2);
			S_shuffle_bound_pt(&ptlst2,pt2,tol);
			break;
		}
	}
/*
.....Trim the common boundary
*/
	S_trim_bound_pt(&ptlst0,pt1,pt2,tol);
	pts0 = (UM_coord *) UU_LIST_ARRAY (&ptlst0);
	npt0 = UU_LIST_LENGTH (&ptlst0);
/*
....Put into boundary
*/
	UU_LIST_EMPTY (cvpoint);
	uu_list_push_list(cvpoint, &ptlst1);

	for (i = 1; i < npt0-1; i++)
		uu_list_push(cvpoint,pts0[i]);

	uu_list_push_list(cvpoint, &ptlst2);

	for (i = npt0-2; i >= 0; i--)
		uu_list_push(cvpoint,pts0[i]);

	nc = UU_LIST_LENGTH (cvpoint);
	uu_list_free (&iobuf);

	return (nc);
}

/*****************************************************************************
**    I_FUNCTION     : void ncl_debug_uvpts(ptlist,color)
**          Debug uv points
**    PARAMETERS
**       INPUT  :
**          ptlist   - list of points
**          color   - color
**       OUTPUT :
**          none
**    RETURNS      : 
**			none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*****************************************************************************/
void ncl_debug_uvpts(ptlist,color)
UU_LIST *ptlist;
int color;
{
	int i,npts;
	UM_coord *pos;
	char tbuf[80];
	pos = (UM_coord *) UU_LIST_ARRAY (ptlist);
	npts = UU_LIST_LENGTH(ptlist);

	sprintf(tbuf,"*stop");
	NclxDbgPstr(tbuf);
	sprintf(tbuf,"draft/modify,color=%d",color);
	NclxDbgPstr(tbuf);
	for (i = 0; i < npts; i++)
	{
		sprintf(tbuf,"Pt/on,sf1,%8.5f,%8.5f", pos[i][0],pos[i][1]);
		NclxDbgPstr(tbuf);
	}
}

/*****************************************************************************
**    I_FUNCTION     : int S_fmill_ps_offset (offs,ncc,cvpts,vlst,uvpts,n1,n2,dis,tol)
**          Offset the boundary curve, inside or outside, by dis, with
**          the "normal,PS[,sf1]" condition.
**    PARAMETERS
**       INPUT  :
**          offs    - surface parameters
**          ncc     - number of boundary points
**          cvpts   - boundary points (XYZ)
**          vlst    - boundary tangent vectors
**          uvpts   - boundary points (UV)
**          n1      - start index of common points
**          n2		- end index of common points
**          dis     - offset distance
**          tol     - tolerance
**       OUTPUT :
**          ncc     - number of offset points
**          uvpts   - offset points (UV)
**    RETURNS      : UU_SUCCESS / UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*****************************************************************************/
int S_fmill_ps_offset (offs,ncc,cvpts,vlst,uvpts,n1,n2,dis,tol)
fml_offset *offs;
int *ncc,n1,n2;
UU_LIST *cvpts,*vlst,*uvpts;
UU_REAL dis,tol;
{
	int i,nc,nc1,nc2,n0,n3,n4,npt0,npt1,npt2,npts,tonp,status;
	UU_REAL dln;
	UM_2Dcoord vln;
	UM_coord *vcs,*pts,*uvs,*pts0,*uvs1,*pts1,*pts2,*ptsi,pt1,pt2;
	UU_LIST ptlst0,vclst0,uvlst0,ptlst1,vclst1,uvlst1,ptlst2,vclst2,uvlst2;

	status = UU_SUCCESS;
	npts = *ncc;
/*
.....Tool location flag
*/
	ncl_get_fml_tonp (&tonp);
	pts = (UM_coord *) UU_LIST_ARRAY (cvpts);
	vcs = (UM_vector *) UU_LIST_ARRAY (vlst);
	uvs = (UM_coord *) UU_LIST_ARRAY (uvpts);
/*
.....Initialize
*/
	n4 = n1+1;
	uu_list_init(&ptlst1,sizeof(UM_coord),n4,n4);
	uu_list_init(&vclst1,sizeof(UM_vector),n4,n4);
	uu_list_init(&uvlst1,sizeof(UM_coord),n4,n4);

	n0 = n2-n1+1;
	uu_list_init(&ptlst0,sizeof(UM_coord),n0,n0);
	uu_list_init(&vclst0,sizeof(UM_vector),n0,n0);
	uu_list_init(&uvlst0,sizeof(UM_coord),n0,n0);

	n3 = npts-n2-n0+1;
	uu_list_init(&ptlst2,sizeof(UM_coord),n3,n3);
	uu_list_init(&vclst2,sizeof(UM_vector),n3,n3);
	uu_list_init(&uvlst2,sizeof(UM_coord),n3,n3);
/*
.....The first curve
*/
	uu_list_push_multiple(&ptlst1,n4,pts);
	uu_list_push_multiple(&vclst1,n4,vcs);
	uu_list_push_multiple(&uvlst1,n4,uvs);
/*
.....The common curve
*/
	uu_list_push_multiple(&ptlst0,n0,&pts[n1]);
	uu_list_push_multiple(&vclst0,n0,&vcs[n1]);
	uu_list_push_multiple(&uvlst0,n0,&uvs[n1]);
/*
.....The second curve
*/
	uu_list_push_multiple(&ptlst2,n3,&pts[n2]);
	uu_list_push_multiple(&vclst2,n3,&vcs[n2]);
	uu_list_push_multiple(&uvlst2,n3,&uvs[n2]);
/*
.....Offset the first curve
*/
	status = S_ps_offset (offs,&n4,&ptlst1,&vclst1,&uvlst1,dis,tol,UU_FALSE);
	uvs1 = (UM_coord *) UU_LIST_ARRAY (&uvlst1);
	npt1 = UU_LIST_LENGTH (&uvlst1);
/*
.....Offset the second curve
*/
	status = S_ps_offset (offs,&n3,&ptlst2,&vclst2,&uvlst2,dis,tol,UU_FALSE);
/*
....Put into boundary
*/
	UU_LIST_EMPTY (uvpts);
	uu_list_push_list(uvpts, &uvlst1);
	uu_list_push_list(uvpts, &uvlst2);
	uu_list_push(uvpts,uvs1[0]);

	npts = UU_LIST_LENGTH (uvpts);
	*ncc = npts;

	return status;
}

/*****************************************************************************
**    I_FUNCTION     : int S_fmill_offset1 (offs,npt,ptlst,vclst,uvlst,vta,
**                                                                 dis,tol)
**          Offset and reproject one surface boundary curve
**    PARAMETERS
**       INPUT  :
**          offs    - surface parameters
**          npt     - number of points
**          ptlst   - list of 3D points
**          vclst   - list of tangencies
**          uvlst   - list of uv points
**          vta     - initial tool axis vector
**          dis     - offset distance
**          tol     - inches tolerance
**       OUTPUT :
**          uvlst  - new list of uv points
**    RETURNS      : UU_SUCCESS / UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*****************************************************************************/
int S_fmill_offset1 (offs,npt,ptlst,vclst,uvlst,vta,dis,tol)
fml_offset *offs;
int npt;
UU_LIST *ptlst,*vclst,*uvlst;
UM_vector vta;
UU_REAL dis,tol;
{
	int i,inclosed,lr,n0,n1,n2,ncc,status;
	UM_coord *pts;
	UU_LOGICAL lduplicate = UU_FALSE;

	status = UU_SUCCESS;
	ncc = npt;
/*
.....Check if there are duplciate segments in boundary curve
*/	
	lduplicate = ncl_selfint_bound_segements(ncc,ptlst,&n1,&n2);

	if (offs->tamode == 0)
	{
		if (offs->lpm == -1) um_negvc (vta,vta);
		n0 = ncc;
		lr = 1;

		if (!lduplicate)
			ncc = ncl_cv_offset (NULLKEY,ptlst,vclst,n0,lr,vta,dis,0,0.,0.,tol,
			0,0);
		else
			ncc = S_fmill_cv_offset (ptlst,vclst,n0,n1,n2,lr,vta,dis,tol);

		if (ncc < 4) status = UU_FAILURE;
/*
..... project the offset spatial curve onto surface along the tool axis,
..... and replace the UV-boundary curve with the result
*/
		if (status == UU_SUCCESS)
		{
			pts = (UM_coord *) UU_LIST_ARRAY (ptlst);
			for (i = 0; i < ncc; i++)
				from_unibase (pts[i],pts[i],&IPT);

			status = ncl_fmill_project (offs->asw,offs->dir,ncc,offs->vta,ptlst,
												uvlst,offs->told,offs->lext);
		}
	}
	else
	{
		if (!lduplicate)	
		{
			if (!ncl_setver(95))
				S_shuffle (ptlst,vclst,uvlst,dis,tol,&ncc);
		
			status = S_ps_offset (offs,&ncc,ptlst,vclst,uvlst,dis,tol,UU_TRUE);
		}
		else
			status = S_fmill_ps_offset(offs,&ncc,ptlst,vclst,uvlst,n1,n2,dis,tol);

		if (status == UU_SUCCESS)
		{
			if (ncl_setver(95))
			{
				inclosed = (offs->lpast)? 0: 1;
				status = ncl_uv_deloop1 (&ncc,uvlst,inclosed);
			}
			else
				status = ncl_uv_deloop (&ncc,uvlst);
		}
	}

	if (status != UU_SUCCESS) ncc = -1;

	return (ncc);
}

/*****************************************************************************
**    I_FUNCTION     : int S_fmill_offset (offs,bound,vlst,dis,tol)
**          Offset and reproject surface boundary curves
**    PARAMETERS
**       INPUT  :
**          offs    - surface parameters
**          bound   - surface boundary
**          vlst    - list of tangencies
**          dis     - offset distance
**          tol     - inches tolerance
**       OUTPUT :
**          bound->uvpts  - new list of uv points
**    RETURNS      : UU_SUCCESS / UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*****************************************************************************/
int S_fmill_offset (offs,bound,vlst,dis,tol)
fml_offset *offs;
UM_srf_boundary *bound;
UU_LIST *vlst;
UU_REAL dis,tol;
{
	int status,ib,nb,ncc,npt,n0,k;
	int *np;
	UM_coord *uvs,*pts;
	UM_real8 vt8[3];
	UM_vector vta;
	UM_vector *vcs;
	UM_2Dcoord *ummx, *vmmx;
	UU_LIST ptlst,vclst,uvlst;
	char tbuf[80];	

	status = UU_SUCCESS;
	nb = bound->nb; np = bound->np;
	ummx = bound->ummx; vmmx = bound->vmmx;

	for (k = 0; k < 3; k++) vt8[k] = offs->vta[k];
	to_unibase (vt8,vt8,&IVE);
	for (k = 0; k < 3; k++) vta[k] = vt8[k];

	ncc = np[0];
	for (ib = 1; ib < nb; ib++)
	{
		if (np[ib] > ncc) ncc = np[ib];
	}

	if (ncc < 3) goto err;

	uu_list_init(&ptlst,sizeof(UM_coord),ncc,ncc);
	uu_list_init(&vclst,sizeof(UM_vector),ncc,ncc);
	uu_list_init(&uvlst,sizeof(UM_coord),ncc,ncc);

	for (ib = n0 = 0; ib < nb; ib++)
	{
		pts = (UM_coord *) UU_LIST_ARRAY (bound->cvpts);
		vcs = (UM_vector *) UU_LIST_ARRAY (vlst);
		uvs = (UM_coord *) UU_LIST_ARRAY (bound->uvpts);
		npt = np[ib];

		if (ib > 0)
		{
			n0 = n0 + np[ib-1];
			if (ib == 1) offs->lpast = !offs->lpast;
		}

		uvs += n0;

		S_fmill_pm (offs,npt,pts,vcs,uvs,ummx[ib],vmmx[ib],vta,tol);
		if (offs->lpm == 0) goto err;

		UU_LIST_EMPTY (&ptlst);
		UU_LIST_EMPTY (&vclst);
		UU_LIST_EMPTY (&uvlst);
		uu_list_push_multiple (&ptlst,npt,pts);
		uu_list_push_multiple (&vclst,npt,vcs);
		uu_list_push_multiple (&uvlst,npt,uvs);

		ncc = S_fmill_offset1 (offs,npt,&ptlst,&vclst,&uvlst,vta,dis,tol);

		if (ncc < 4) goto err;

		np[ib] = ncc;
		uu_list_delete (bound->cvpts,0,npt);
		uu_list_delete (vlst,0,npt);
		uu_list_delete (bound->uvpts,n0,npt);

		uvs = (UM_coord *) UU_LIST_ARRAY (&uvlst);

		uu_list_insert_multiple (bound->uvpts,n0,ncc,uvs);
	}
	goto done;

err:
	status = UU_FAILURE;

done:
	uu_list_free (&ptlst);
	uu_list_free (&vclst);
	uu_list_free (&uvlst);

	return (status);
}

/*********************************************************************
**    I_FUNCTION     : void S_pts_to_pol (pts,nv,ummx,vmmx,pol1)
**       Add a contour to a polygon
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
static void S_pts_to_pol (pts,nv,ummx,vmmx,pol1)
UM_coord *pts;
int nv;
UM_2Dcoord ummx,vmmx;
ncl_polygon *pol1;
{
	int iv;

	pol1->num_contours = 1;
	if (pol1->np == UU_NULL)
		pol1->np = (int *) uu_malloc(sizeof(int));
	pol1->np[0] = nv;

	if (pol1->box == UU_NULL)
		pol1->box = (UM_2box *) uu_malloc(sizeof(UM_2box));

	pol1->box[0].xmin = ummx[0];
	pol1->box[0].xmax = ummx[1];

	pol1->box[0].ymin = vmmx[0];
	pol1->box[0].ymax = vmmx[1];

	for (iv = 0; iv < nv; iv++)
	{
		uu_list_push (pol1->contour,pts[iv]);
	}
}

/*********************************************************************
**    I_FUNCTION     : void S_find_outer (pol,m0,mper)
**       Find outer contour in a polygon; if several, the largest one
**       is selected (by area).
**    PARAMETERS
**       INPUT  :
**          pol      - polygon structure
**       OUTPUT :
**          m0    - perimeter contour
**          mper  - number of outer contours
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_find_outer (pol,m0,mper)
ncl_polygon *pol;
int *m0,*mper;
{
	int c,nc,nv,n0,nper;
	int *np;
	UM_2Dcoord *vtx;
	UU_REAL area,amax;
	UU_REAL ncl_closed_area();

	nc = pol->num_contours;
	np = (int *) pol->np;
	n0 = -1; nper = 0;

	for (c = 0; c < nc; c++)
	{
		if (np[c] > 3)
		{
			nper++;
			n0 = c;
		}
	}

	*mper = nper;
	if (nper >= 2)
	{
		amax = -1;
		n0 = -1;
		vtx = (UM_2Dcoord *) UU_LIST_ARRAY (pol->contour);
		for (c = 0; c < nc; c++)
		{
			nv = abs(np[c]);
			if (np[c] > 3)
			{
				area = ncl_closed_area (vtx,nv);
				if (area > amax)
				{
					n0 = c; amax = area;
				}
			}
			vtx += nv;
		}
	}

	*m0 = n0;
}

/*********************************************************************
**    I_FUNCTION     : void S_contour_to_bndry (vtx,nv,box,ib,bound,tolsq)
**       Write a contour to a boundary
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
static void S_contour_to_bndry (vtx,nv,box,ib,bound,tolsq)
UM_2Dcoord *vtx;
int nv,ib;
UM_2box *box;
UM_srf_boundary *bound;
UU_REAL tolsq;
{
	int iv,npt;
	UM_coord pti,pt0;
	UU_REAL d;

	bound->nb++;
	pti[2] = 0;
	npt = nv;

	for (iv = 0; iv < nv; iv++)
	{
		pti[0] = vtx[iv][0]; pti[1] = vtx[iv][1];
		if (iv == 0) um_vctovc (pti,pt0);
		uu_list_push (bound->uvpts,pti);
	}
	d = UM_SQDIS_2D (vtx[0],vtx[nv-1]);
	if (d > tolsq)
	{
		uu_list_push (bound->uvpts,pt0);
		npt++;
	}
	bound->np[ib] = npt;

	bound->ummx[ib][0] = box->xmin;
	bound->ummx[ib][1] = box->xmax;
	bound->vmmx[ib][0] = box->ymin;
	bound->vmmx[ib][1] = box->ymax;
}

/*********************************************************************
**    I_FUNCTION     : void S_pol_to_bndry (pol,bound)
**       Add a polygon to a boundary
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
static int S_pol_to_bndry (pol,bound,tol,tolsq)
ncl_polygon *pol;
UM_srf_boundary *bound;
UU_REAL tol,tolsq;
{
	int ib,nb,c0,nper,nv0,nv,c,nc,ins;
	int *np;
	UM_2box *box;
	UM_2Dcoord *ummx,*vmmx,*vt0,*vtx;
	int status = UU_SUCCESS;

	S_find_outer (pol,&c0,&nper);

	if (nper < 1)
	{
		status = UU_FAILURE; return (status);
	}

	nb = bound->nb;
	np = bound->np;
	ummx = bound->ummx; vmmx = bound->vmmx;
	nc = pol->num_contours;
	if (nc > nb)
	{
		UU_FREE (np);
		UU_FREE (ummx); UU_FREE (vmmx);

		np = (int *) uu_malloc((nc+1)*sizeof(int));
		ummx = (UM_2Dcoord *) uu_malloc(nc*sizeof(UM_2Dcoord));
		vmmx = (UM_2Dcoord *) uu_malloc(nc*sizeof(UM_2Dcoord));

		bound->nb = nb; bound->np = np;
		bound->ummx = ummx; bound->vmmx = vmmx;
	}

	bound->nb = ib = 0;
	UU_LIST_EMPTY (bound->uvpts);
	np = (int *) pol->np;
	box = (UM_2box *) pol->box;
	vtx = (UM_2Dcoord *) UU_LIST_ARRAY (pol->contour);

	for (c = 0; c < nc; c++)
	{
		nv = abs(np[c]);
		if (c == c0)
		{
			vt0 = vtx; nv0 = nv;
			S_contour_to_bndry (vtx,nv,&box[c],ib,bound,tolsq);

			break;
		}
		vtx += nv;
	}

	vtx = (UM_2Dcoord *) UU_LIST_ARRAY (pol->contour);
	for (c = 0; c < nc; c++)
	{
		nv = abs(np[c]);
		if (np[c] < -3)
		{
			ins = um_check_inside (vt0,nv0,vtx[0],&box[c0],tol);
			if (ins >= 0)
			{
				ib++;
				S_contour_to_bndry (vtx,nv,&box[c],ib,bound,tolsq);
			}
		}
		vtx += nv;
	}

	return (status);
}

/*****************************************************************************
**    I_FUNCTION     : int S_recalc_bndry (bound,tol)
**          Offset and reproject surface boundary curves
**    PARAMETERS
**       INPUT  :
**          offs    - surface parameters
**          bound   - surface boundary
**          vlst    - list of tangencies
**          dis     - offset distance
**          tol     - inches tolerance
**       OUTPUT :
**          bound->uvpts  - new list of uv points
**    RETURNS      : UU_SUCCESS / UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*****************************************************************************/
static int S_recalc_bndry (bound)
UM_srf_boundary *bound;
{
	int ib,nb,ncc,nv,status;
	int *np;
	UM_coord *uvs;
	UM_2Dcoord *ummx, *vmmx;
	ncl_polygon pol0,pol;
	int npi;
	UM_2box boxi;
	UU_REAL tol,tolsq;

	tol = 0.0002;
	tolsq = tol*tol;
	status = UU_SUCCESS;

	nb = bound->nb; np = bound->np;
	ummx = bound->ummx; vmmx = bound->vmmx;
	uvs = (UM_coord *) UU_LIST_ARRAY (bound->uvpts);

	ncc = np[0];
	for (ib = 1; ib < nb; ib++)
	{
		if (np[ib] > ncc) ncc = np[ib];
	}
	if (ncc < 100) ncc = 100;

	ncl_init_polygon (&pol0,ncc);
	ncl_init_polygon (&pol,ncc);

	pol.np = &npi;
	pol.box = &boxi;

	nv = np[0];
	S_pts_to_pol (uvs,nv-1,ummx[0],vmmx[0],&pol0);

	for (ib = 1; ib < nb; ib++)
	{
		uvs += nv;
		nv = np[ib];
		UU_LIST_EMPTY (pol.contour);
		S_pts_to_pol (uvs,nv-1,ummx[ib],vmmx[ib],&pol);
		status = ncl_polygon_clip (NCL_DIFF,&pol0,&pol,&pol0,tol,tolsq);
		if (status != UU_SUCCESS) goto Done;
	}

	status = S_pol_to_bndry (&pol0,bound,tol,tolsq);

Done:
	ncl_free_polygon (&pol0);
	UU_LIST_FREE (pol.contour);
	return (status);
}

/*****************************************************************************
**    I_FUNCTION     : int S_set_off_bndry (eptr,bsptr,tfmat,bound,tol)
**          Evolve, offset, and reproject surface boundary
**    PARAMETERS
**       INPUT  :
**          eptr    - pointer to surface.
**          bsptr   - pointer to base surface.
**          tfmat   - pointer to transformation mx.
**          tol     - tolerance in XYZ space.
**          noinner - ignore inner boundaries if true
**       OUTPUT :
**          bound   - surface boundary
**    RETURNS      : UU_SUCCESS / UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : bound->cvpts list is not returned
*****************************************************************************/
static int S_set_off_bndry (eptr,bsptr,tfmat,ksec,bound,tol,noinner)
struct NCL_fixed_databag *eptr,*bsptr;
UM_transf tfmat;
UU_KEY_ID ksec;
UM_srf_boundary *bound;
UU_REAL tol;
UU_LOGICAL noinner;
{
	int status,itis,nb,k,istat,tonp;
	int *np;
	UU_LIST vlst;
	UM_coord pt0;
	UM_coord *pts,*uvs;
	UM_2Dcoord *ummx,*vmmx;
	UM_int2 isub,mm,itype;
	UM_real8 diam,toolpar;
	UM_real8 tool[6];
	UU_REAL dis,fmm;
	UU_REAL bplm[4];
	fml_offset offs;
	struct NCL_fixed_databag e2;
	struct NCL_trimsf_rec *tsf;

	nb = 0;
	np = UU_NULL;
	ummx = vmmx = UU_NULL;

	uu_list_init (&vlst, sizeof(UM_vector), 100, 100);
/*
...get surface data
*/
	if (ncl_itsa_trimsrf (eptr))
	{
		itis = 1;
		ncl_trimsrf_get_fixed (eptr,&nb,bplm);
		if (noinner) nb = 1;
	}
	else
	{
		itis = 0;
		nb = 1;
	}
	np = (int *) uu_malloc((nb+1)*sizeof(int));
	ummx = (UM_2Dcoord *) uu_malloc(nb*sizeof(UM_2Dcoord));
	vmmx = (UM_2Dcoord *) uu_malloc(nb*sizeof(UM_2Dcoord));
	ummx[0][0] = vmmx[0][0] = 0.;
	ummx[0][1] = vmmx[0][1] = 1.;

	status = um_evolve_bndr (tol,eptr,bsptr,tfmat,itis,nb,np,ummx,vmmx,bplm,
											bound,&vlst,UU_NULL);
	if (status != UU_SUCCESS) goto err;

	ncl_fmill_check_edges (np,ummx,vmmx,bound,itis);
/*
..... offset the outer boundary, inside or outside, by the tool radius,
..... perpendicular to the tool axis
*/
	isub = 264;
	getifl(&isub,&mm);

	fmm = (mm == 1)? 25.4: 1;

	offs.told = fmm*tol;
	isub = 28;
	getsc(&isub,&diam);
	dis = 0.5*diam/fmm;

	isub = 2;
	gettool (&isub ,&toolpar);
	offs.crad = toolpar/fmm;

	isub = 6;
	gettool (&isub ,&toolpar);
	offs.cute = toolpar/fmm;

	getend (tool);
	for (k = 0; k < 3; k++)	offs.vta[k] = tool[k+3];

	itype = 9; /* surface */
	isub = 1;
	ptdsc3(&eptr->key,&isub,&itype,&offs.asw);

	offs.tamode = 0;
	offs.asw1 = 0;
	offs.u1 = offs.v1 = 0.5;
	if (ncl_fml_normal())
	{
		offs.tamode = 1;
		if (ksec != NULLKEY)
		{
/*
..... get secondary PS
*/
			e2.key = ksec;
			istat = ncl_retrieve_data_fixed (&e2);
			if (istat == UU_SUCCESS)
			{
				if (e2.rel_num == NCL_TRIMSF_REL)
				{
					tsf = (struct NCL_trimsf_rec *)&e2;
					ksec = tsf->bs_key;
					e2.key = ksec;
					istat = ncl_retrieve_data_fixed (&e2);
				}
			}
			if (istat == UU_SUCCESS)
			{
				offs.tamode = 2;
				ptdsc3(&ksec,&isub,&itype,&offs.asw1);
				pts = (UM_coord *) UU_LIST_ARRAY (bound->cvpts);
				from_unibase (pts[0],pt0,&IPT);
				for (k = 0; k < 3; k++)	tool[k] = pt0[k];
				sfini1 (&offs.asw1,&ISF1,&offs.u1,&offs.v1,&tool,&tool[3]);
			}
		}
	}
	ncl_get_fml_tonp (&tonp);
	offs.lpast = (tonp == TL_PAST);
	offs.lext = offs.lpast;
	offs.dir = 0;

	status = S_fmill_offset (&offs,bound,&vlst,dis,tol);
	if (status != UU_SUCCESS) goto err;

	np[nb] = 0;
	uvs = (UM_coord *) UU_LIST_ARRAY (bound->uvpts);
/*
..... recalculate the UV-boxes
*/
	um_cshape_dirchk (uvs,nb,np,ummx,vmmx);

	bound->nb = nb; bound->np = np;
	bound->ummx = ummx; bound->vmmx = vmmx;

	if (tonp == TL_TO && nb > 1) status = S_recalc_bndry (bound);

err:
/*
.....The boundary memory is freed
.....by the calling routine
*/
/*
	if (status != UU_SUCCESS)
	{
		UU_FREE (np);
		UU_FREE (ummx); UU_FREE (vmmx);
	}
*/
	uu_list_free (&vlst);

	return (status);
}

/*****************************************************************************
**    I_FUNCTION     : void S_bndr_dir (eptr,bound,avdir,tol),
**          Set CLW or CCLW flag for the surface outer boundary with respect
**          to the tool axis vector.
**    PARAMETERS
**       INPUT  :
**          eptr   - surface pointer.
**          bound  - surface boundary.
**          tol    - tolerance
**       OUTPUT :
**          avdir  - CCW or CW
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*****************************************************************************/
static void S_bndr_dir (eptr,bound,avdir,tol)
struct NCL_fixed_databag *eptr;
UM_srf_boundary *bound;
int *avdir;
UU_REAL tol;
{
	UM_vector vta,vti;
	UM_real8 tool[6];
	UM_coord *uvs,*pts;
	UM_int2 itype,nwds;
	int ins;
	int i,np0;
	fml_offset offs;

	pts = (UM_coord *) UU_LIST_ARRAY (bound->cvpts);
	uvs = (UM_coord *) UU_LIST_ARRAY (bound->uvpts);

	getend (tool);
	vta[0] = tool[3]; vta[1] = tool[4]; vta[2] = tool[5];

	itype = 9; /* surface */
	nwds = 1;
	ptdsc3(&eptr->key,&nwds,&itype,&offs.asw);
	offs.dir = 0;
	offs.tamode = 0;

	np0 = bound->np[0];

	for (i = 1; i < np0-1; i++)
	{
		um_vcmnvc (pts[i+1],pts[i],vti);
		ins = S_fmill_pm1 (&offs,pts[i],vti,uvs[i],np0,uvs,
			bound->ummx[0],bound->vmmx[0],vta,tol);

		if (ins != 0)
		{
			if (ins == -1)
			{
				if (*avdir == DIRCCW)
					*avdir = DIRCW;
				else if (*avdir == DIRCW)
					*avdir = DIRCCW;
			}
			break;
		}
	}
}

/*****************************************************************************
**    E_FUNCTION     : int ncl_fmill_set_bndry (eptr,bsptr,tfmat,bound,vlst,
**                                                                 tol)
**          Evolve, offset, and reproject surface boundary
**    PARAMETERS
**       INPUT  :
**          eptr    - pointer to surface.
**          bsptr   - pointer to base surface.
**          tfmat   - pointer to transformation mx.
**          vlst    - auxiliary list to use
**          tol     - tolerance in XYZ space.
**          noinner - ignore inner boundaries if true
**       OUTPUT :
**          bound   - surface boundary
**    RETURNS      : UU_SUCCESS / UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : bound->cvpts list is not returned
*****************************************************************************/
int ncl_fmill_set_bndry (eptr,bsptr,tfmat,ust,vst,ckey,ksec,bound,tol,noinner,
	avdir)
struct NCL_fixed_databag *eptr,*bsptr;
UM_transf tfmat;
UU_KEY_ID ckey,ksec;
UM_srf_boundary *bound;
UU_REAL ust,vst,tol;
UU_LOGICAL noinner;
int *avdir;
{
	int status,i,npts,tonp;
	UU_KEY_ID skey;
	struct NCL_uvconv cvlist;
	UM_coord *uvpts;
	UU_REAL uv[2];

	bound->uvpts = (UU_LIST *) uu_malloc (sizeof(UU_LIST));
	uu_list_init (bound->uvpts, sizeof(UM_coord), 100, 100);

	skey = bsptr->key;
/*
.....Create boundary points.
*/
	if (ckey > NULLKEY)
	{
		uv[0] = ust; uv[1] = vst;
		status = ncl_cv_project_sfuv(ckey,skey,uv,tol,bound->uvpts,&npts);
		bound->toler = tol;
		bound->nb = 1;
		bound->ummx = (UM_2Dcoord *) uu_malloc(sizeof(UM_2Dcoord));
		bound->vmmx = (UM_2Dcoord *) uu_malloc(sizeof(UM_2Dcoord));
		uvpts = (UM_coord *) UU_LIST_ARRAY (bound->uvpts);
		npts = UU_LIST_LENGTH(bound->uvpts);
		for (i = 0; i < npts; i++)
		{
			if (i==0 || bound->ummx[0][0] > uvpts[i][0])
				bound->ummx[0][0] = uvpts[i][0];
			if (i==0 || bound->ummx[0][1] < uvpts[i][0])
				bound->ummx[0][1] = uvpts[i][0];
			if (i==0 || bound->vmmx[0][0] > uvpts[i][1])
				bound->vmmx[0][0] = uvpts[i][1];
			if (i==0 || bound->vmmx[0][1] < uvpts[i][1])
				bound->vmmx[0][1] = uvpts[i][1];
		}
		bound->np = (int *)uu_malloc(sizeof(int));
		bound->np[0] = npts;
	}
	else
	{
		bound->cvpts = (UU_LIST *) uu_malloc (sizeof(UU_LIST));
		uu_list_init (bound->cvpts, sizeof(UM_coord), 100, 100);

		ncl_get_fml_tonp (&tonp);

		if (tonp == TL_TO || tonp == TL_PAST)
			status = S_set_off_bndry (eptr,bsptr,tfmat,ksec,bound,tol,noinner);
		else
		{
			status = um_uvlist_init (eptr,&cvlist);
			if (status == UU_SUCCESS)
			{
			ncl_set_boundary_toler (tol);
			bound->toler = tol;
			status = ncl_set_boundary(eptr,tfmat,bound,&cvlist);
			}
			um_uvlist_free (&cvlist);
			if (status == UU_SUCCESS)
			{
				if (noinner) bound->nb = 1;
				if (*avdir == DIRCCW || *avdir == DIRCW)
					S_bndr_dir (bsptr,bound,avdir,tol);
			}
		}

		UU_LIST_FREE (bound->cvpts);
	}

	if (status == UU_SUCCESS)
		ncl_orient_boundary(bound);

	return (status);
}

/*****************************************************************************
**    E_FUNCTION     : int S_refine (e2,tf2,rln,fuv,is,flist,ptlst,uvlst,tol)
**          Evolve a segment on the secondary PS, insert points into the
**          original segment on the primary PS.
**    PARAMETERS
**       INPUT  :
**          e2      - pointer to surface
**          tfmat   - pointer to transformation mx
**          rln     - pointer to line holding current UV segment on secondary PS
**          flist   - fmill list
**          fuv     - fmill list array
**          is      - current index in fmill list
**          ptlst   - initialized list of UM_coord's to use
**          uvlst   - initialized list of UM_coord's to use
**          tol     - tolerance in XYZ space.
**       OUTPUT :
**          flist   - fmill list refined
**    RETURNS      : UU_SUCCESS / UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*****************************************************************************/
static int S_refine (e2,tf2,rln,fuv,is,flist,ptlst,uvlst,tol)
struct NCL_fixed_databag *e2;
UM_transf tf2;
struct UM_line_rec *rln;
UU_REAL tol;
int is;
UU_LIST *flist,*ptlst,*uvlst;
UM_coord *fuv;
{
	int i,j,k,n,ins;
	UM_coord *uvs;
	UM_coord fvc,fpi;
	UU_REAL del;

	UU_LIST_EMPTY (ptlst);
	UU_LIST_EMPTY (uvlst);
	ins = 0;
	i = is;

	n = ncl_evolve_bn_crv_on_srf (e2,tf2,rln,BPLM,tol,ptlst,UU_NULL,uvlst);
	if (n > 2)
	{
	 	uvs = (UM_coord *) UU_LIST_ARRAY (uvlst);

		ins = n - 2;
		fpi[2] = fvc[2] = 0;

		for (k = 0; k < 2; k++)
		{
			fpi[k] = fuv[is-1][k];
			fvc[k] = fuv[is][k] - fuv[is-1][k];
		}

		del = n - 1;
		del = 1./del;
		um_vctmsc (fvc,del,fvc);

		for (j = 1; j <= ins; j++)
		{
			um_vcplvc (fpi,fvc,fpi);
			uu_list_insert (flist,i,fpi);
			i++;
		}
	}

	return (ins);
}

/*****************************************************************************
**    E_FUNCTION     : void ncl_fmill_refine (bsptr,tfmat,ksec,flist,retlist,
**                                                              bnd,nbnd,tol)
**          Refine fmill list by tolerancing secondary PS
**    PARAMETERS
**       INPUT  :
**          eptr    - pointer to surface.
**          bsptr   - pointer to base surface.
**          tfmat   - pointer to transformation mx.
**          tol     - tolerance in XYZ space.
**       OUTPUT :
**          bound   - surface boundary
**    RETURNS      : UU_SUCCESS / UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*****************************************************************************/
void ncl_fmill_refine (bsptr,tfmat,ksec,flist,retlist,bnd,nbnd,tol)
struct NCL_fixed_databag *bsptr;
UM_transf tfmat;
UU_KEY_ID ksec;
UU_LIST *flist,*retlist;
int *bnd,nbnd;
UU_REAL tol;
{
	int status,k,tonp,nret,npts,kret,i,ins,init,ibnd;
	int *iret;
	UM_coord pt0;
	UM_coord *uvs;
	UM_int2 isub,mm,itype;
	UM_real8 diam,toolpar;
	UM_real8 tool[6];

	UU_REAL dis,fmm,u0,v0,ui,vi,us0,vs0,usi,vsi;
	UM_vector vta;
	struct NCL_fixed_databag e2;
	struct NCL_trimsf_rec *tsf;
	UM_transf tf2;
	struct UM_evsrfout evsrf;
	fml_offset offs;
	UU_LIST ptlst,uvlst;
	struct UM_line_rec Sln;


	npts = UU_LIST_LENGTH (flist);
 	uvs = (UM_coord *) UU_LIST_ARRAY (flist);
/*
..... offset the outer boundary, inside or outside, by the tool radius,
..... perpendicular to the tool axis
*/
	isub = 264;
	getifl(&isub,&mm);

	fmm = (mm == 1)? 25.4: 1;

	offs.told = fmm*tol;
	isub = 28;
	getsc(&isub,&diam);
	dis = 0.5*diam/fmm;

	isub = 2;
	gettool (&isub ,&toolpar);
	offs.crad = toolpar/fmm;

	isub = 6;
	gettool (&isub ,&toolpar);
	offs.cute = toolpar/fmm;

	getend (tool);
	for (k = 0; k < 3; k++)	offs.vta[k] = tool[k+3];

	itype = 9; /* surface */
	isub = 1;
	ptdsc3(&bsptr->key,&isub,&itype,&offs.asw);
	u0 = uvs[0][0]; v0 = uvs[0][1];
/*
..... get secondary PS
*/
	e2.key = ksec;
	status = ncl_retrieve_data_fixed (&e2);
	if (status == UU_SUCCESS)
	{
		if (e2.rel_num == NCL_TRIMSF_REL)
		{
			tsf = (struct NCL_trimsf_rec *)&e2;
			ksec = tsf->bs_key;
			e2.key = ksec;
			status = ncl_retrieve_data_fixed (&e2);
		}
	}
	if (status == UU_SUCCESS)
	status = uc_retrieve_transf(e2.key,tf2);

	if (status == UU_SUCCESS)
	status = uc_evsrf (UM_POINT,u0,v0,bsptr,tfmat,&evsrf);

	if (status != UU_SUCCESS) return;
	offs.u = u0; offs.v = v0;
	from_unibase (evsrf.sp,pt0,&IPT);
	for (k = 0; k < 3; k++)	tool[k] = pt0[k];

	offs.tamode = 2;
	offs.u1 = offs.v1 = 0.5;
	us0 = vs0 = 0.5;

	ptdsc3(&ksec,&isub,&itype,&offs.asw1);
	sfini1 (&offs.asw1,&ISF1,&offs.u1,&offs.v1,&tool,&tool[3]);

	ncl_get_fml_tonp (&tonp);
	offs.lpast = (tonp == TL_PAST);
	offs.lext = offs.lpast;
	offs.dir = offs.lpm = 0;

	kret = 0;
	if (retlist == NULLST)
		nret = -1;
	else
	{
		nret = UU_LIST_LENGTH (retlist);
		iret = (int *) UU_LIST_ARRAY (retlist);
	}

	ibnd = 0;

	uu_list_init(&ptlst,sizeof(UM_coord),100,100);
	uu_list_init(&uvlst,sizeof(UM_coord),100,100);

	Sln.rel_num = UM_LINE_REL;
	strncpy (Sln.label,"@UN    ",7);
	Sln.no_displst = 0; Sln.displst = UU_NULL;
	um_nullvc (Sln.spt); um_nullvc (Sln.ept);
	status = ur_create_data(&Sln);
	if (status != UU_SUCCESS || Sln.key == NULLKEY) goto Done;

	init = 0;

	for (i = 0; i < npts; i++)
	{
		ui = uvs[i][0]; vi = uvs[i][1];
		status = uc_evsrf (UM_POINT,ui,vi,bsptr,tfmat,&evsrf);
		if (status != UU_SUCCESS) goto Done;

		S_tlaxis (&offs,evsrf.sp,vta);
		usi = offs.u1; vsi = offs.v1;

		if (init == 0)
		{
			us0 = usi; vs0 = vsi;
			init = 1;
			continue;
		}

		Sln.spt[0] = us0;
		Sln.spt[1] = vs0;
		Sln.ept[0] = usi;
		Sln.ept[1] = vsi;

		status = ur_update_data_fixed (&Sln);
		if (status != UU_SUCCESS) goto Done;

		ins = S_refine (&e2,tf2,&Sln,uvs,i,flist,&ptlst,&uvlst,tol);

		if (ins > 0)
		{
			i += ins; npts += ins;
		 	uvs = (UM_coord *) UU_LIST_ARRAY (flist);
			for (k = kret; k < nret; k++)
			{
				iret[k] += ins;
			}
			if (bnd != UU_NULL)
			{
				for (k = ibnd; k < nbnd; k++)
				{
					bnd[k] += ins;
				}
			}
		}

		if (bnd != UU_NULL)
		{
			if (i == bnd[ibnd]) ibnd++;
		}

		if (nret > 0 && i == iret[kret])
		{
			init = 0;
			kret++;
		}
		else
		{
			us0 = usi; vs0 = vsi;
		}
	}

Done:
	uu_list_free (&ptlst);
	uu_list_free (&uvlst);

	if (Sln.key != NULLKEY) uc_delete (Sln.key);
}

/*********************************************************************
**    I_FUNCTION     : S_fix_bndr1 (w,m,kind,bound)
**          Intersects trimmed surface boundary with an isoparametric
**          line. Add the intersection points to the boundary.
**    PARAMETERS
**       INPUT  :
**          w      - constant u or v parameter.
**          cvtyp  - 1 = u_curve (constant v), 2 = v_curve(constant u).
**          bound  - trimmed surface boundary.
**       OUTPUT :
**          bound  - trimmed surface boundary.with added points
**    RETURNS      :  none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_fix_bndr1 (w,m,kind,bound)
UU_REAL w;
int m,kind;
UM_srf_boundary *bound;
{
	int i,j,k,i0,np1;
	UM_coord *pts,uvi;
	int ib;
	UU_REAL deli,delj;

	uvi[2] = 0;
/*
..... for v=uv=const (cvtyp=1): find polyline segments whose lower end is
..... below and upper end is above, and intersect them with the line.
..... similarly for cvtyp=2.
*/
	for (ib = 0, i0 = 0; ib < bound->nb; ib++)
	{
		pts = (UM_coord *) UU_LIST_ARRAY (bound->uvpts);
		pts += i0;
		np1 = bound->np[ib];
		for (i = 1; i < np1; i++)
		{
			j = i-1;
			deli = pts[i][kind] - w;
			delj = pts[j][kind] - w;
			if (deli*delj < 0 && fabs(deli) > UM_FUZZ && fabs(delj) > UM_FUZZ)
			{
				uvi[kind] = w;
				uvi[m] = pts[i][m] + (w - pts[i][kind])*
					(pts[j][m] - pts[i][m])/(pts[j][kind]-pts[i][kind]);

				k = i + i0;

				uu_list_insert(bound->uvpts,k,uvi);
				i++; np1++;
				bound->np[ib] = np1;
				pts = (UM_coord *) UU_LIST_ARRAY (bound->uvpts);
				pts += i0;
			}
		}
		i0 += np1;
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_fml_fix_bndr (bound,param)
**          Intersects trimmed surface boundary with an isoparametric
**          line. Add the intersection points to the boundary.
**    PARAMETERS
**       INPUT  :
**          w      - constant u or v parameter.
**          cvtyp  - 1 = u_curve (constant v), 2 = v_curve(constant u).
**          bound  - trimmed surface boundary.
**       OUTPUT :
**          bound  - trimmed surface boundary.with added points
**    RETURNS      :  none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_fml_fix_bndr (bound,param)
UM_srf_boundary *bound;
NCL_fmlparam *param;
{
	int i,m,kind,npas,n;
	UU_REAL w,wst,wnd,delw;

	kind = 2 - param->cvtyp;
	m = 1 - kind;
	npas = param->npas;
	wst = param->wst;
	wnd = param->wnd;

	if (npas > 1)
	{
		delw = (wnd - wst)/(npas-1);
		w = wst + delw;
		n = npas - 2;
	}
	else
	{
		delw = 0;
		w = (wnd + wst)/2;
		n = 1;
	}

	for (i = 0; i < n; i++, w += delw)
	{
		S_fix_bndr1 (w,m,kind,bound);
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_fml_outside_box (w,ib,ifwd,bound)
**          Check if a point is outside a boundary box.
**    PARAMETERS
**       INPUT  :
**          w      - constant u or v parameter.
**          ib     - boundary number (0 iff outer)
**          ix     - check V-box if 0, U-box if 1
**          bound  - surface boundary.
**       OUTPUT : none
**    RETURNS      :  true or false
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL ncl_fml_outside_box (w,ib,ix,bound)
UU_REAL w;
int ib,ix;
UM_srf_boundary *bound;
{
	if (ix == 0)
	{
		return (bound->vmmx[ib][0] > w || bound->vmmx[ib][1] < w);
	}
	else
	{
		return (bound->ummx[ib][0] > w || bound->ummx[ib][1] < w);
	}
}

/*********************************************************************
**    E_FUNCTION     : void ncl_fmill_set_params (bound,ifwd,ust,vst,ws,wn,
**                                                                   param)
**          Set FMILL parameters by the surface boundary.
**    PARAMETERS
**       INPUT  :
**          bound   - surface boundary
**          ifwd    - 0 = scrub in u dir, 1 = scrub in v dir.
**          ust     - u value near start point
**          vst     - v value near start point
**       OUTPUT :
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_fmill_set_params (bound,ifwd,ust,vst,ws,wn,param)
UM_srf_boundary *bound;
UU_REAL ust,vst,*ws,*wn;
NCL_fmlparam *param;
int ifwd;
{
	int iup,flowdir,stepdir;
	UU_REAL w1,w2,wst,wnd;

	if (ifwd > 0)
	{
		wst = bound->ummx[0][0];
		wnd = bound->ummx[0][1];
		w1 = bound->vmmx[0][0];
		w2 = bound->vmmx[0][1];
		flowdir = 1;
		if (vst > w2 || (vst > w1 && fabs(vst-w1) > fabs(w2-vst)))
		      flowdir = -1;
		iup = 1;
		if (ust > wnd || (ust > wst && fabs(ust-wst) > fabs(wnd-ust)))
		   iup = -1;
		stepdir = -flowdir*iup;
		param->cvtyp = 2;
	}
	else
	{
		wst = bound->vmmx[0][0];
		wnd = bound->vmmx[0][1];
		w1 = bound->ummx[0][0];
		w2 = bound->ummx[0][1];
		flowdir = 1;
		if (ust > w2 || (ust > w1 && fabs(ust-w1) > fabs(w2-ust)))
		      flowdir = -1;
		iup = 1;
		if (vst > wnd || (vst > wst && fabs(vst-wst) > fabs(wnd-vst)))
		   iup = -1;
		stepdir = flowdir*iup;
		param->cvtyp = 1;
	}

	*ws = wst;
	*wn = wnd;

	param->stepdir = stepdir;
	param->flowdir = flowdir;
	param->iup = iup;
}

/*********************************************************************
**    E_FUNCTION     : int ncl_fmill_project (asw,side,npts,vta,cvpts,uvpts,
**                                                                told,lext)
**        Project points along a vector onto surface
**    PARAMETERS
**       INPUT  : 
**          asw     - the surface word
**          side    - sfpt parameter (+/- for the surface normal)
**          cvpts   - list of 3D points
**          npts    - number of points
**          vta     - projection vector
**          told    - tolerance.
**          lext    - surface extension flag
**       OUTPUT :  
**          uvpts  - list of uv points
**    RETURNS      : UU_SUCCESS / UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_fmill_project (asw,side,npts,vta,cvpts,uvpts,told,lext)
UM_real8 asw;
UM_real4 side;
int npts;
UM_vector vta;
UU_LIST *cvpts,*uvpts;
UU_REAL told;
UU_LOGICAL lext;
{
	UM_coord *pts,*uvs;
	int i,k;
	UM_coord uvi;

	UM_int2 isf;
	UM_int2 ifl,ival;
	UM_real8 fac;
	UM_real4 u4, v4;
	UM_real4 ss[9];
	UU_REAL tol;
	UM_vector vec,norm;
	UM_coord local_pt,spt;
	int status = UU_SUCCESS;

	pts = (UM_coord *) UU_LIST_ARRAY (cvpts);
	uvs = (UM_coord *) UU_LIST_ARRAY (uvpts);
	tol = told;

	if (lext)
	{
		isf = 4; fac = 2.;
		evsf_ext_set (&isf,&fac);
	}
	else
		evsf_ext_rst();

	isf = 4;
	u4 = (UM_real4) uvs[0][0];
	v4 = (UM_real4) uvs[0][1];
	if (lext)
	{
		u4 = 0.5*u4 + 0.25; v4 = 0.5*v4 + 0.25;
	}
	uvi[0] = u4; uvi[1] = v4; uvi[2] = 0;
	UU_LIST_EMPTY (uvpts);
	for (i = 0; i < npts; i++)
	{
		um_vctovc (pts[i], local_pt);
		sfpt(&asw,local_pt,&isf,&side,&u4,&v4,ss);
		ifl = 2; getifl(&ifl,&ival);
		if (ival == 128)
		{
			status = UU_FAILURE;
			goto done;
		}
		for (k = 0; k < 3; k++)
		{
			norm[k] = ss[k]; spt[k] = ss[4+k];
		}		
		um_unitvc (norm, norm);
		um_vcmnvc(spt,local_pt,vec);

		status = ncl_pt_proj_sf1 (local_pt,asw,isf,side,&u4,&v4,spt,norm,vta,tol);
		if (status != UU_SUCCESS) return (status);
		uvi[0] = u4; uvi[1] = v4;

		uu_list_push (uvpts,uvi);
	}
done:;
	return (status);
}
