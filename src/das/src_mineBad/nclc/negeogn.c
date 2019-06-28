/*********************************************************************
**    NAME         :  negeogn.c
**       CONTAINS:
**     int ncl_interp_rbsp (n, ptve,itsk, crv)
**     int ncl_interp_with_corners (n,ptve,cvpoint,crv)
**     void ncl_rbsp_data_fixed (npts, s, pts, nkt, crv)
**     int ncl_create_rbsp (npts, s, pts, crv)
**     int ncl_create_rbsp1 (k,npt,s,pt,wt, crv)
**     int bspdef (nents, keys, nclkey, ifit)
**     int ncl_cvsfsd (key, nclkey, edge, np, offset, tol)
**     int ncl_bsp_intof(numpt, nclkey)
**     int ncvsp(nclkey,delx,dely,delz,ier)
**    COPYRIGHT 1991 (c) Numerical Control Computer Sciences Inc.
**                          All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       negeogn.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:34
*********************************************************************/

#include "udebug.h"
#include "modef.h"
#include "uhep.h"
#include "mfort.h"
#include "mdrel.h"
#include "mcrv.h"
#include "nccs.h"
#include "msrf.h"
#include "mgeom.h"
#include "mdeval.h"
#include "nclfc.h"
#include "ncl.h"

extern UU_LIST NCL_uvintof;

/*********************************************************************
**    E_FUNCTION     : ncl_interp_rbsp (n, ptve, itsk, crv)
**       Interpolate a rational B-spline curve thru a list of points &
**       optional slope vectors.
**    PARAMETERS   
**       INPUT  : 
**          n            number of points.
**          ptve         list of entities.
**          itsk         = 0 - interpolate all points
**                       = 1 - fit thru points.
**       OUTPUT :  
**          crv          created B-spline
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_interp_rbsp (n, ptve, itsk, crv)
int n;
struct NCL_crvgen_rec *ptve;
int itsk;
struct UM_rbsplcrv_rec *crv;
{
   int status;
   UU_REAL *s, *pts;
   int npts;

	s = pts = UU_NULL;
/*
.....generate B-spline using ptve
*/
	status = ncl_interp_rbsp1 (n, ptve, itsk, &npts, &s, &pts);

/*
.....initialized the key label and subscript to avoid UMR in ncl_create_entity
*/
	crv->label[0] ='\0';
	crv->key = 0;
	crv->subscr =0;

	if (status == UU_SUCCESS)
		status = ncl_create_rbsp (npts, s, pts, crv);

	if (s != UU_NULL) uu_free(s);
	if (pts != 0) uu_free(pts);
	return(status);
}

/*********************************************************************
**    S_FUNCTION     : S_splice (n1,s,pts,splist,cvpoint)
**       Add curve segment data (knots and control points) to lists. If
**       previos segments already there, add new segment after a sharp
**       corner.
*********************************************************************/
static void S_splice (n1,s,pts,splist,cvpoint)
int n1;
UU_REAL *s,*pts;
UU_LIST *splist,*cvpoint;
{
	int n0,ns0,ns1,k;
	UU_REAL *t,t0,t1,s0;
	UM_coord *pp,pi,p0,p1;
	UU_REAL sk;

	n0 = cvpoint->cur_cnt;
	ns1 = n1 + 4;

	if (n0 > 0)
	{
		ns0 = n0 + 4;
		t = (UU_REAL *) UU_LIST_ARRAY (splist);
		pp = (UM_coord *) UU_LIST_ARRAY (cvpoint);

		t0 = t[ns0-5];
		t1 = t[ns0-1];

		s0 = t1 + t1 - t0;

		splist->cur_cnt = ns0 - 1;
		for (k = 1; k < ns1; k++)
		{
			sk = s[k] + s0;
			uu_list_push (splist,&sk);
		}

		for (k = 0; k < 3; k++)
		{
			p0[k] = pp[n0-4][k];
			p1[k] = pp[n0-1][k];

			pi[k] = (p0[k] + p1[k]) / 2;
		}

		um_vctovc (p1,pp[n0-2]);
		um_vctovc (pi,pp[n0-3]);

		uu_list_push (cvpoint,p1);

		for (k = 0; k < 3; k++)
		{
			pi[k] = (p1[k] + pts[k]) / 2;
		}
		uu_list_push (cvpoint,pi);

	}
	else
	{
		uu_list_push_multiple (splist,ns1,s);
	}
		
	uu_list_push_multiple (cvpoint,n1,(UM_coord *)pts);
}

/*********************************************************************
**    E_FUNCTION     : S_sort_segp(n1,ptve)
**       Sort the points with nearest distance.
**    PARAMETERS   
**       INPUT  : 
**          npts		-number of points.
**          ptve		-list of entities.
**       OUTPUT :  
**          none
**    RETURNS      : 
**			none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_sort_segp(npts,ptve)
int npts;
struct NCL_crvgen_rec *ptve;
{
	int i,j,jmin;
	UU_REAL dis,dmin;
	UM_coord p0,p1;
	struct NCL_crvgen_rec ptmin;

	for (i = 0; i < npts-1; i++)
	{
		p0[0] = ptve[i].x;
		p0[1] = ptve[i].y;
		p0[2] = ptve[i].y;
/*
.....Get next neartes segp
*/	
		jmin = i+1;
		dmin = 10000.0;
		for (j = i + 1; j < npts; j++)
		{
			p1[0] = ptve[j].x;
			p1[1] = ptve[j].y;					
			p1[2] = ptve[j].y;
			dis = 	UM_SQDIS(p0,p1);
			if (dis < dmin)
			{
				jmin = j;
				dmin = dis;
				ptmin = ptve[j];
			}
		}
/*
.....Swap if different order
*/
		if (jmin != i+1)
		{
			ptve[jmin] = ptve[i+1];
			ptve[i+1] = ptmin;
		}
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_interp_with_corners (n,ptve,cvpoint,crv)
**       Interpolate a rational B-spline curve thru a list of points &
**       optional slope vectors. Make sharp corners when indicated.
**    PARAMETERS   
**       INPUT  : 
**          n            number of points.
**          ptve         list of entities.
**       OUTPUT :  
**          crv          created B-spline
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_interp_with_corners (n,ptve,cvpoint,ssplin,sfkey,itsk,crv)
int n,itsk;
struct NCL_crvgen_rec *ptve;
UU_LIST *cvpoint;
UU_LOGICAL ssplin;
UU_KEY_ID *sfkey;
struct UM_rbsplcrv_rec *crv;
{
	int status;
	int npts,i,i0,i1,n1;
	UU_REAL *s, *pts;
	struct NCL_crvgen_rec *segp;
	UU_LIST splist;

	int i2 = 3;

	uu_list_init (&splist,sizeof(UU_REAL),0,n);

	n1 = 0;
	i0 = 0;
	segp = ptve;
	status = UU_SUCCESS;

	for (i = i0+1; i < n-3 && status == UU_SUCCESS; i++)
	{
		if (ptve[i].inv == 1 && ptve[i+1].inv == 0 && ptve[i+2].inv == 1)
		{
			i1 = i+1;
			ptve[i1].inv = 1;
			ptve[i1].a = ptve[i].a;
			ptve[i1].b = ptve[i].b;
			ptve[i1].c = ptve[i].c;

			n1 = i1-i0+1;
			s = pts = UU_NULL;
			segp = ptve + i0;
/*
.....Sort the segp
*/
			if (itsk == 2 && *sfkey > NULLKEY)
				S_sort_segp(n1,segp);

			status = ncl_interp_rbsp1 (n1, segp, 1, &npts, &s, &pts);
			if (status != UU_SUCCESS) goto done;

			S_splice (npts,s,pts,&splist,cvpoint);

			if (s != UU_NULL) uu_free(s);
			if (pts != UU_NULL) uu_free(pts);

			i0 = i1+1;
			i+=2;
		}
	}

	n1 = n - i0;
	if (n1 > 1)
	{
		s = pts = UU_NULL;
		segp = ptve + i0;
/*
.....Sort the segp
*/
		if (itsk == 2 && n1 < n && *sfkey > NULLKEY)
			S_sort_segp(n1,segp);

		status = ncl_interp_rbsp1 (n1, segp, 1, &npts, &s, &pts);
		if (status != UU_SUCCESS) goto done;

		S_splice (npts,s,pts,&splist,cvpoint);

		if (s != UU_NULL) uu_free(s);
		if (pts != UU_NULL) uu_free(pts);
	}
/*
.....initialized the key label and subscript to avoid UMR in ncl_create_entity
*/
	crv->label[0] ='\0';
	crv->key = NULLKEY;
	crv->subscr = 0;

	if (status == UU_SUCCESS)
	{
		s = (UU_REAL *) UU_LIST_ARRAY (&splist);
		pts = (UU_REAL *) UU_LIST_ARRAY (cvpoint);

		npts = cvpoint->cur_cnt;

		if (ssplin)
			status = ncl_create_ssplin(sfkey,s,pts,&npts,&crv->key);
		else
			status = ncl_create_rbsp (npts, s, pts, crv);
	}

	uu_list_free (&splist);

done:;
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : void ncl_rbsp_data_fixed (npts, s, pts, nkt, crv)
**       Fill the 'data-fixed' part of the rational B-spline struct
**    PARAMETERS   
**       INPUT  : 
**          npts         number of control points.
**          s            array of parameters.
**          pts          array of control points.
**       OUTPUT :  
**          nkt          number of knots
**          crv          B-spline struct
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/		
void ncl_rbsp_data_fixed (npts, s, pts, nkt, crv)
int npts,*nkt;
UU_REAL *s, *pts;
struct UM_rbsplcrv_rec *crv;
{
	int ns;

	if (npts == 2)
	{
		ns = 4;
		crv->planar = UU_TRUE;
		crv->k = 2;
		crv->n = 1;
	}
	else
	{
		ns = npts + 4;
		crv->planar = UU_FALSE;
		crv->k = 4;
		crv->n = npts-3;
	}

	crv->open = UU_TRUE;
	crv->closdinu = 0;
	crv->t0 = 0.0;
	crv->t1 = s[ns-1];

	*nkt = ns;
}

/*********************************************************************
**    E_FUNCTION     : ncl_create_rbsp (npts, s, pts, crv)
**       Create a rational B-spline from control points and knots
**    PARAMETERS   
**       INPUT  : 
**          npts         number of control points.
**          s            array of parameters.
**          pts          array of control points.
**       OUTPUT :  
**          crv          created B-spline
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_create_rbsp (npts, s, pts, crv)
int npts;
UU_REAL *s, *pts;
struct UM_rbsplcrv_rec *crv;
{
	int i, ns, status;
	UM_int2 i2=0;

	ur_setup_data (UM_RBSPLCRV_REL, crv, sizeof(*crv));

	ncl_rbsp_data_fixed (npts, s, pts, &ns, crv);
/* 
... Jingrong 9/23/98: create the entity with default attributes.
...
	status = uc_create_data (crv, UM_DEFAULT_TF, UM_CURRENT_ATTR);
*/
/*
.....Switching from uc_create_data to ncl_create_data caused the label not to
.....be saved, which caused problems anytime the user tried to refer to the
.....cv. Calling ncl_store_wf1 will save the necessary information.
.....JLS 2/8/99
.....But call ncl_label_wf() first to set up label. IJD 16-MAR-2000
*/
	ncl_label_wf(crv->rel_num, crv->label, &crv->subscr, crv->key, &i2);
	status = ncl_create_entity (crv, NCLI_CURVE);
	ncl_store_wf1(crv->key);
	if (status == UU_SUCCESS)
		status = ur_update_data_varlist (crv->key, 1, s, 1, ns);

	if (status == UU_SUCCESS)
		status = ur_update_data_varlist (crv->key, 2, pts, 1, npts);

	if (status == UU_SUCCESS)
	{
		UM_real8 ver;
		UM_int2 idx = 169;

		getsc (&idx, &ver);
		if (ver > 9.2)
		{
			crv->no_wt = 0; crv->wt = UU_NULL;
		}
		else
		{
			for (i=0;i<npts;i++) s[i] = 1.0;
			status = ur_update_data_varlist (crv->key, 3, s, 1, npts);
		}
	}

	if (status == UU_SUCCESS)
		status = ncl_retrieve_data_fixed (crv);

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : bspdef (nents, keys, nclkey, ifit)
**       Create B-spline curve.
**    PARAMETERS   
**       INPUT  : 
**          nents   - number of entities
**          keys    - keys of entities
**          ifit    - = 0 interpolate
**                    = 1 fit
**                    = 2 control points
**       OUTPUT :  
**          nclkey  - key of created B-spline
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
****************************************fit*****************************/
int bspdef (nclkey, ifit)
UM_int4 *nclkey;
UM_int2 *ifit;
   {
   int i, rel_num, status, num;
   int npts, itsk;
   UU_KEY_ID key;
   struct NCL_crvgen_rec seg, *segp;
   UU_LIST seglist;
   struct UM_point_rec *ptrec;
   struct NCL_nclpv_rec pvrec;
   UM_coord opt;
   struct NCL_vector_rec verec;
   struct UM_rbsplcrv_rec crv;
   UU_REAL *ptr, *pt;
	union {
		UM_real8 asn;
		UM_int4 key[2];
	} val;

   crv.key = 0;
   ptrec = (struct UM_point_rec *)&pvrec;
   npts = 0;
   status = ncl_get_asn_ptr(&ptr,&num);
   uu_list_init (&seglist, sizeof(struct NCL_crvgen_rec), num, num);

   for (i=0; i<num && status == UU_SUCCESS; i++)
     {
		val.asn = ptr[i];
		key = val.key[0];
     if (ur_retrieve_data_relnum(key, &rel_num) != 0)
       status = UU_FAILURE;
     else
       {
        if (rel_num == UM_POINT_REL || rel_num == NCL_POINTVEC_REL)
          {
           pvrec.key = key;
           if (ur_retrieve_data_fixed (&pvrec) != 0)
             status = UU_FAILURE;
           else
             {
             if (rel_num == UM_POINT_REL) pt = ptrec->pt; else pt = pvrec.pt;
              if (i>0)
                {
				 if (!um_cceqcc(opt, pt))
					 uu_list_push (&seglist, &seg);
				 else
					 npts--;
                 if (um_cceqcc(opt, pt) && *ifit == 0) status = UU_FAILURE;
                }
              um_vctovc (pt, opt);
              seg.x = pt[0];
              seg.y = pt[1];
              seg.z = pt[2];
              seg.inv = 0;
              npts++;
             }
          }
        if (rel_num == NCL_VECTOR_REL)
          {
           verec.key = key;
           if (ur_retrieve_data_fixed (&verec) != 0)
               status = UU_FAILURE;
          }
        if (status == UU_SUCCESS)
           if (rel_num == NCL_VECTOR_REL)
             {
              seg.a = verec.vec[0];
              seg.b = verec.vec[1];
              seg.c = verec.vec[2];
              seg.inv = 1;
             }
           else if (rel_num == NCL_POINTVEC_REL)
             {
              seg.a = pvrec.ve[0];
              seg.b = pvrec.ve[1];
              seg.c = pvrec.ve[2];
              seg.inv = 1;
             }
       }
     }
   uu_list_push (&seglist, &seg);
   itsk = *ifit;
   segp = (struct NCL_crvgen_rec *) UU_LIST_ARRAY (&seglist);
   if (status == UU_SUCCESS)
     status = ncl_interp_rbsp (npts, segp, itsk, &crv);
   if (status == UU_SUCCESS)
     {
     *nclkey = crv.key;
     ncl_def_color (crv.key);
     }

   uu_list_free (&seglist);

   uu_dexitstatus("bspdef ()", status);
   return(status);
   }

/*********************************************************************
**    E_FUNCTION     : ncl_cvsfsd1 (srf,edge,offset,procent,cvtype,uvoff,tol)
**       Create curve as a SF edge - preprocess.
**    PARAMETERS   
**       INPUT  : 
**          srf      - surface struct
**          edge = 1 - v=0 curve
**          edge = 2 - v=1 curve
**          edge = 3 - u=0 curve
**          edge = 4 - u=1 curve
**          offset   - edge offset
**          procent  - flag to interpret the offset as percentage
**          tol      - tolerance (=sc(27))
**       OUTPUT :  
**          cvtype  - 1 for U-line, 2 for V-line
**          uvoff   - edge offset, as parameter
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
************************************************************************/
int ncl_cvsfsd1 (srf,edge,offset,procent,cvtype,uvoff,tol)
struct NCL_fixed_databag *srf;
UM_int2 *edge;
UM_real8 *offset;
UU_LOGICAL procent;
int *cvtype;
UU_REAL *uvoff,tol;
{
	int status, type;
	UU_REAL uv,vpr[2];
	UU_REAL W0,W1;
	UM_srf_boundary b;

	status = UU_SUCCESS;

	type = *edge;
	W1 = (procent)? 100: 1;

	switch (type)
	{
		case 1:
			*cvtype = 1;
			uv = *offset;
			break;
		case 2:
			*cvtype = 1;
			uv = W1 - *offset;
			break;
		case 3:
			*cvtype = 2;
			uv = *offset;
			break;
		case 4:
			*cvtype = 2;
			uv = W1 - *offset;
			break;
		default:
			return(UU_FAILURE);
			break;
	}

	if (procent)
	{
		W0 = 0; W1 = 1;
		vpr[0] = 0; vpr[1] = 1;
		type = 3 - *cvtype;

		if (ncl_itsa_trimsrf (srf))
		{
			ncl_set_boundary_toler (tol);
			status = ncl_get_boundary (UV_BOX_LIST,srf,&b);

			if (status == UU_SUCCESS)
			{
				if (type == 1)
				{
					W0 = b.vmmx[0][0]; W1 = b.vmmx[0][1];
					vpr[0] = b.ummx[0][0]; vpr[1] = b.ummx[0][1];
				}
				else
				{
					W0 = b.ummx[0][0]; W1 = b.ummx[0][1];
					vpr[0] = b.vmmx[0][0]; vpr[1] = b.vmmx[0][1];
				}
				um_free_boundary (&b);
			}
			else
				return (status);
		}

		status = ncl_percnt_on_sf1 (type,W0,W1,&uv,vpr,srf,tol);
	}

	*uvoff = uv;

	return(status);
}

/*********************************************************************
**    E_FUNCTION     : void cvsfs1 (key, edge, offset)
**       Create NCL curve as a SF edge: compute the parameter offset from a 
**       percentage offset 
**    PARAMETERS   
**       INPUT  : 
**          key      - surface key 
**          edge = 1 - v=0 curve
**          edge = 2 - v=1 curve
**          edge = 3 - u=0 curve
**          edge = 4 - u=1 curve
**          offset   - edge offset, as percentage
**       OUTPUT :  
**          offset   - edge offset, as parameter
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
************************************************************************/
void cvsfs1 (key, edge, offset)
UM_int4 *key;
UM_int2 *edge;
UM_real8 *offset;
{
	UM_real8 tol8;
	int status,cvtype;
	UU_REAL uv,tol;
	struct NCL_fixed_databag srf;

	gettol (&tol8);
	tol = tol8;

	srf.key = *key;
	status = ncl_retrieve_data_fixed (&srf);
	if (status != UU_SUCCESS) goto done;

	status = ncl_cvsfsd1 (&srf,edge,offset,UU_TRUE,&cvtype,&uv,tol);

done:
	if (status == UU_SUCCESS)
	{
		if (*edge == 2 || *edge == 4) uv = 1 - uv;
		*offset = uv;
	}
	else
	{
		uv = *offset;
		*offset = uv/100;
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_cvsfsd (key,nclkey,edge,np,lpercnt,offset,tol8)
**       Create B-spline curve as a SF edge.
**    PARAMETERS   
**       INPUT  : 
**          key      - surface key 
**          edge = 1 - v=0 curve
**          edge = 2 - v=1 curve
**          edge = 3 - u=0 curve
**          edge = 4 - u=1 curve
**          np       - optional number of points to evolve
**          offset   - edge offset
**          lpercnt  - flag to interpret the offset as percentage
**          tol8     - tolerance (=sc(27))
**       OUTPUT :  
**          nclkey  - key of created B-spline
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
****************************************edge****************************/
int ncl_cvsfsd (key, nclkey, edge, np, lpercnt, offset, tol8)
UM_int4 *key,*nclkey;
UM_int2 *edge,*np,*lpercnt;
UM_real8 *offset,*tol8;
{
	int i, status, cvtype;
	UU_REAL uv,tol,vpr[2],*pt;
	struct NCL_fixed_databag srf;
	UM_transf tfmat,bsmat;
	UU_LIST cvpts,cvtang,seglist;
	struct UM_rbsplcrv_rec crv;
	int npts;
	UU_LOGICAL procent,lv94;
	UM_real8 ver;
	UM_int2 idx;
	UU_REAL w,dw;
	struct UM_evsrfout evsrf;
	struct NCL_crvgen_rec seg, *segp;

	uu_denter(UU_MTRC,(us,"ncl_cvsfsd ()"));

	status = UU_SUCCESS;
	*nclkey = 0;
	npts = *np;
	if (npts < 20) npts = 0;

	tol = *tol8;
	procent = (*lpercnt == 1);

	srf.key = *key;
	status = ncl_retrieve_data_fixed (&srf);
	if (status != UU_SUCCESS) return (UU_FAILURE);

	status = ncl_cvsfsd1 (&srf,edge,offset,procent,&cvtype,&uv,tol);
	if (status != UU_SUCCESS) return (UU_FAILURE);

	status = uc_retrieve_transf (srf.key, tfmat);
	if (status != UU_SUCCESS) return (UU_FAILURE);

	if (ncl_itsa_trimsrf(&srf))
	{
		srf.key = ((struct NCL_trimsf_rec *)&srf)->bs_key;
		status = ncl_retrieve_data_fixed (&srf);
		if (status != UU_SUCCESS) return (UU_FAILURE);
		status = uc_retrieve_transf (srf.key, bsmat);
		if (status != UU_SUCCESS) return (UU_FAILURE);
		um_tftmtf(tfmat,bsmat,tfmat);
	}


	if (npts >= 20)
	{
		dw = 1./(npts-1);

		uu_list_init (&cvpts, sizeof(UM_coord), npts, npts);
		for (i = 0, w = 0.; i < npts; i++, w+=dw)
		{
			if (cvtype == 1)
				status = ncl_evsrf_tf (UM_POINT,w,uv,&srf,tfmat,&evsrf);
			else
				status = ncl_evsrf_tf (UM_POINT,uv,w,&srf,tfmat,&evsrf);
			if (status != UU_SUCCESS) break;
			uu_list_push (&cvpts,evsrf.sp);
		}
	}
	else
	{
		idx = 169;
		getsc (&idx, &ver);
		lv94 = ver < 9.449;

		vpr[0] = 0.; vpr[1] = 1.;
		uu_list_init (&cvpts, sizeof(UM_coord), 200, 200);

		if (lv94)
		{
			ncl_evolve_crv_on_srf (&srf, tfmat, uv, vpr, cvtype, tol,
				&cvpts, UU_NULL, UU_NULL);
		}
		else
		{
			uu_list_init (&cvtang, sizeof(UM_vector), 200, 200);
			ncl_evolve_crv_on_srf (&srf, tfmat, uv, vpr, cvtype, tol,
				&cvpts, &cvtang, UU_NULL);
			npts = cvpts.cur_cnt;
			if (npts >= 2)
				ncl_fix_tol (&npts,tol,&cvpts,&cvtang);
			uu_list_free (&cvtang);
		}
	}
	npts = cvpts.cur_cnt;
	if (npts <= 0) goto done;
	pt = (UU_REAL *) UU_LIST_ARRAY(&cvpts);

	uu_list_init (&seglist, sizeof(struct NCL_crvgen_rec), npts, npts);

	ncl_init_seg (&seg);

	for (i = 0; i < npts; i++, pt+=3)
	{
		seg.x = pt[0];
		seg.y = pt[1];
		seg.z = pt[2];
		uu_list_push (&seglist, &seg);
	}
	segp = (struct NCL_crvgen_rec *) UU_LIST_ARRAY (&seglist);

	if (status == UU_SUCCESS)
		status = ncl_interp_rbsp (npts, segp, 1, &crv);

	if (status == UU_SUCCESS)
	{
		*nclkey = crv.key;
		ncl_def_color (crv.key);
	}

	uu_list_free (&seglist);

done:
	uu_list_free (&cvpts);

	uu_dexitstatus("ncl_cvsfsd ()", status);
	return(status);
}

/********************************************************************
**    E_FUNCTION     : ncvsp(nclkey,delx,dely,delz,ier)
**       Create a B-spline curve as a translation (by a vector) of  
**       an NCL curve 
**    PARAMETERS   
**       INPUT  : 
**          (delx,dely,delz) - translation vector
**       OUTPUT :  
**          nclkey  - key of created B-spline, =0 if fail
**          ier     - error number, if fail
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
**************** translate NCL curve ******************/
int ncvsp(nclkey,delx,dely,delz,ier)
UM_int4 *nclkey;
UM_real8 *delx,*dely,*delz;
UM_int2 *ier;
	{
	struct UM_rbsplcrv_rec crv;
	struct NCL_curve_rec e;
	int i,j,k, key, npts, ns, rel_num, status;
	UU_REAL del[3], *pts, *s, t_end, ro;
	char *uu_malloc();

	uu_denter(UU_MTRC,(us,"ncvsp ()"));

	status = UU_SUCCESS;
	key = *nclkey;
	*ier = 0;
	*nclkey = 0;

	if (ur_retrieve_data_relnum(key, &rel_num) != 0)
		goto Err;
	e.key = key;

	if (ur_retrieve_data(&e, sizeof(struct NCL_curve_rec)) != 0)
		goto Err;

	npts = 3 * e.no_segment - 2;
	t_end = e.t_end;
	ns = npts + 4;

	s = (UU_REAL *)uu_malloc(ns*sizeof(*s));
	for (k = 0; k < 4; k++)
	{
		s[k] = 0.;
		s[ns-k-1] = t_end;
	}
	for (i = 1; i < e.no_param; i++)
		for (k = 1; k < 4; k++)
			s[3*i+k] = e.param[i-1] * t_end;

	del[0] = *delx;
	del[1] = *dely;
	del[2] = *delz;

	pts = (UU_REAL *)uu_malloc(3*npts*sizeof(*pts));
	for (i = 0; i < npts; i++)
	{
		j = (i+1)/3; 

		k = i%3;
		if (k == 0)
			ro = 0.;
		else if (k == 1)
			ro = 1.;
		else if (k == 2)
			ro = - e.segment[j-1].rho;

		for (k = 0; k < 3; k++)
			pts[3*i+k] = 
				e.segment[j].point[k] + ro * e.segment[j].delta[k] + del[k];
	}

	status = ncl_create_rbsp (npts, s, pts, &crv);


	if (status != UU_SUCCESS) goto Err;

	*nclkey = crv.key;
	ncl_def_color (crv.key);

	if (status == UU_SUCCESS) goto Done;

Err:;
	if (*ier == 0) *ier = 163;
	status = UU_FAILURE;

Done:;

	if (s != UU_NULL) uu_free(s);
	if (pts != 0) uu_free(pts);
	uu_dexitstatus("ncvsp ()", status);
	return(status);
   }
/*********************************************************************
**    E_FUNCTION     : ncl_bsp_intof(numpt,nclkey,itrans,tangfl,tol8)
**       Create B-spline curve as an intersection of two surfaces
**    PARAMETERS   
**       INPUT  : 
**        numpt     - number of points
**        tol8      - tolerance
**        itrans    - transformation flag: 0 - none; 1 - units & modsys;
**                    2 - units only
**        tangfl    - tangencies in the list: 0 - none; 1 - first and last;
**                    2 - all
**       OUTPUT :  
**          nclkey  - key of created B-spline
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
***************************************intof****************************/
int ncl_bsp_intof(numpt,nclkey,itrans,tangfl,tol8)
UM_int2 *numpt,*itrans,*tangfl;
UM_int4 *nclkey;
UM_real8 *tol8;
{
	int i, npts, nn, status;
	UM_coord *pt;
	UU_LIST cvpoint,cvtang,seglist;
	struct UM_rbsplcrv_rec crv;
	struct NCL_crvgen_rec seg, *segp;
	UU_REAL fcin = 0.039370079;
	UU_REAL tol = *tol8;
	UM_int2 type = NCLI_POINT;

	uu_denter(UU_MTRC,(us,"ncl_bsp_intof ()"));

	status = UU_SUCCESS;
	*nclkey = 0;
	npts = *numpt;

	pt = (UM_coord *) UU_LIST_ARRAY (&NCL_uvintof);

	if (*tangfl == 2)
	{
		uu_list_init (&cvtang, sizeof(UM_vector), npts, npts);
		uu_list_init (&cvpoint, sizeof(UM_coord), npts, npts);
		uu_list_push_multiple (&cvpoint,npts,pt);
		pt = pt + npts;
		uu_list_push_multiple (&cvtang,npts,pt);

		ncl_fix_tol (&npts,tol,&cvpoint,&cvtang);

		uu_list_free (&cvtang);
		pt = (UM_coord *) UU_LIST_ARRAY (&cvpoint);
	}

	uu_list_init (&seglist, sizeof(struct NCL_crvgen_rec), npts, npts);

	nn = npts;
	if (*tangfl == 1) nn = npts+1;

	for (i = 0; i < nn; i++)
	{
		ncl_init_seg (&seg);
		if (*itrans == 1) 
			to_unibase (pt[i],pt[i],&type);
		else if (*itrans == 2) 
			um_vctmsc (pt[i],fcin,pt[i]);
		seg.x = pt[i][0];
		seg.y = pt[i][1];
		seg.z = pt[i][2];
		if (*tangfl == 1 && (i == 0 || i == npts))
		{
			i++;
			if (*itrans == 1) 
			{
				type = NCLI_VECTOR;
				to_unibase (pt[i],pt[i],&type);
				type = NCLI_POINT;
			}
			seg.a = pt[i][0];
			seg.b = pt[i][1];
			seg.c = pt[i][2];
			if (seg.a*seg.a + seg.b*seg.b + seg.c*seg.c > 0.0001)
				seg.inv = 1;
		}
		uu_list_push (&seglist, &seg);
	}
	segp = (struct NCL_crvgen_rec *) UU_LIST_ARRAY (&seglist);

	if (status == UU_SUCCESS)
		status = ncl_interp_rbsp (npts, segp, 1, &crv);

	if (status == UU_SUCCESS)
	{
		*nclkey = crv.key;
		ncl_def_color (crv.key);
	}

	uu_list_free (&seglist);
	if (*tangfl == 2) uu_list_free (&cvpoint);

	uu_dexitstatus("ncl_bsp_intof ()", status);
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_create_rbsp1 (k,npt,nwt,s,pt,wt, crv)
**       Create a rational B-spline from control points, knots, and weights
**    PARAMETERS   
**       INPUT  : 
**          k            spline order: 2 if linear, 3 if quadratic,...
**          npts         number of control points.
**          s            array of parameters.
**          pts          array of control points.
**          wt           array of weights.
**       OUTPUT :  
**          crv          created B-spline
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_create_rbsp1 (k,npt,nwt,s,pt,wt,t0,t1, crv)
int npt,nwt;
UU_REAL *s,*pt,*wt;
UU_REAL t0,t1;
struct UM_rbsplcrv_rec *crv;
{
	int ns, status;
	UM_int2 i2=0;

	ur_setup_data (UM_RBSPLCRV_REL, crv, sizeof(*crv));

	if (npt == 2) k = 2;

	ns = npt + k;
	crv->k = k;
	crv->n = npt - k + 1;

	if (npt <= 3) crv->planar = UU_TRUE;
	else          crv->planar = UU_FALSE;

	crv->open = UU_TRUE;
	crv->closdinu = 0;
	crv->t0 = t0;
	crv->t1 = t1;

	ncl_label_wf(crv->rel_num, crv->label, &crv->subscr, crv->key, &i2);
	status = ncl_create_entity (crv, NCLI_CURVE);
	ncl_store_wf1(crv->key);

	if (status == UU_SUCCESS)
		status = ur_update_data_varlist (crv->key, 1, s, 1, ns);

	if (status == UU_SUCCESS)
		status = ur_update_data_varlist (crv->key, 2, pt, 1, npt);

	if (status == UU_SUCCESS)
		status = ur_update_data_varlist (crv->key, 3, wt, 1, nwt);

	if (status == UU_SUCCESS)
		status = ncl_retrieve_data_fixed (crv);

	return (status);
}
