/*********************************************************************
**    NAME         :  necvlev.c
**       CONTAINS:  Routines for calculating waterline geometry
**
**        ncl_sparms
**        cvoutj
**        ncontr
**        ncl_sfs_connect
**        ncl_sfplio
**        ncl_sfsio
**        ncl_get_zlev
**        nsfsio
**        ncl_cv_isline
**        ncl_cv_isarc
**        ncl_sfs_create_geo
**        ncl_create_splcv
**        ncl_cvio_create_splcv
**
**    COPYRIGHT 2004 (c) Numerical Control Computer Sciences Inc.
**    All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       necvlev.c , 25.2
**    DATE AND TIME OF LAST MODIFICATION
**       01/20/17 , 10:34:23
*********************************************************************/
#include "nclfc.h"
#include "mdattr.h"
#include "mattr.h"
#include "uminmax.h"
#include "modef.h"
#include "mdcoord.h"
#include "nclwaterln.h"
#include "nclclip.h"
#include "class.h"
#include "udforms.h"

UU_LOGICAL NCL_allcurve = UU_FALSE;
int NCL_multi_ent = 0;
static int Sfs_created;

static char buf[120];

static UU_REAL tol = 0, tolsq = 0, rmax = 0;
static UU_LIST ptsio,nio,seglist;
static UM_int2 mm = 0;
static UM_int4 isubscr = 0;
static UM_int2 IVE = NCLI_VECTOR;
static UM_int2 IPT = NCLI_POINT;

static int sfnum = 0;
static NCL_sfs_dispmode *dispmode = UU_NULL;
static int Sfrm = -1;
static UU_LOGICAL Sactive = UU_FALSE;
static UD_LIST stat_list;
static UU_LIST strings;
static int allcv = 0;
static int sfdisp = 0;
static int sfview = 0;
static int iseg0 = 0;

typedef struct
{
	int nlinks;
	int *link;
	UU_KEY_ID lkey;
} NCL_sfs_loop;


/*********************************************************************
**    E_FUNCTION     : void ncl_sparms(irmax,itol)
**       Set up arc routine parameters.
**    PARAMETERS   
**       INPUT  : 
**          irmax - Maximum arc radius.
**          itol  - Tolerance.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_sparms(irmax,itol)
UU_REAL irmax,itol;
{
	rmax = irmax;
	tol = itol;
	tolsq = itol*itol;
}

/*********************************************************************
*********************************************************************/
void sfsini ()
{
	uu_list_init (&ptsio, sizeof(UM_coord), 0, 100);
	uu_list_init (&nio, sizeof(int), 0, 10);
	uu_list_init (&seglist,sizeof(struct NCL_crvgen_rec),0,100);
	Sfs_created = 0;
}

/*********************************************************************
*********************************************************************/
void sfsfre (ncv)
UM_int2 *ncv;
{
	uu_list_free (&ptsio);
	uu_list_free (&nio);
	uu_list_free (&seglist);
	ncl_free_nios();
	*ncv = Sfs_created;
}

/*********************************************************************
**    E_FUNCTION     : void ncl_sfs_update_saveid (lb0,il2,is2)
**       Update saveid (Fortran global string used for labeling), and advance
**       local label counters.
*********************************************************************/
void ncl_sfs_update_saveid (lb0,il2,is2)
char *lb0;
UM_int2 *il2;
UM_int4 *is2;
{
	UM_int2 ilab;
	UM_int4 isub;
	UM_f77_str_ptr str77;

	UM_init_f77_str (str77, buf, 80);

	isub = *is2;
	ilab = *il2;
/*
..... if ilab is positive, we are creating names of type ABC3, ABC4, etc.
*/
	if (ilab > 0)
	{
		ilab++;
		sprintf(buf,"%s%d",lb0,ilab);
		stsavi(str77);
	}
/*
..... if isub is positive, we are creating subscripted names,
..... such as DD(3), DD(4), etc
*/
	if (isub > 0)
	{
		isub++;
		upsubs();
	}

	*is2 = isub;
	*il2 = ilab;
}

/*********************************************************************
**    E_FUNCTION     : void cvoutj (ckey,j,ier)
**       Fortran callable function to extract a component from
**       a composite curve.
**    PARAMETERS
**       INPUT  :
**          sfkey   - key of composite curve.
**          j       - component number
**       OUTPUT :
**          ier   - 0 iff no errors
**    RETURNS      :
**         UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void cvoutj (ckey,j,llab,subscr,ier)
UM_int4 *ckey,*subscr;
UM_int2 *j,*ier;
UM_f77_str_ptr llab;
{
	int i,rev,len;
	struct UM_crvdatabag c1,c2,tcv;
	UM_transf tran;
	int status,jcv;
	UM_int2 i2,lblst;
	UM_int4 i4;
	struct NCL_nclattr_rec attr;
	int size = sizeof(struct NCL_fixed_databag);
	UU_KEY_ID keyi;
	UM_int2 ifnd;
	struct UM_compcrv_rec ccrv;
	UU_REAL t0,t1;

	UM_int2 ilab,isub,caon,isubc;
	char *lb0;
	UM_f77_str_ptr str77;

	*ier = 472;
	isubscr = *subscr;
	isub = 296; getifl(&isub,&ilab);
	isubc = 41; getifl(&isubc,&caon);
/*
.....we need lb0 is used, so we have to set it
*/
/*	if (ilab > 0) */
	{
		lb0 = UM_cstr_of_f77_str(llab);
		UM_init_f77_str (str77, buf, 80);
	}
	ccrv.key = *ckey;
	status = ncl_retrieve_data_fixed (&ccrv);
	if (status == UU_SUCCESS)
		status = uc_retrieve_transf (ccrv.key,tran);

	if (status != UU_SUCCESS) return;

	t0 = ccrv.t0;
	t1 = ccrv.t1;
	jcv = *j - 1;

	for (i=0;i<ccrv.no_cid;i++)
	{
		if (ccrv.cid[jcv].endparam < t0) jcv++;
		else break;
	}
	*j = jcv + 1;
	if (jcv > 0 && t1 < ccrv.cid[jcv-1].endparam)
	{
		*ier = 0;
		return;
	}
	status = ncl_compcrv_getelm (&ccrv, jcv, &c1, &rev);
	if (status != UU_SUCCESS || uc_super_class(c1.rel_num) != UC_CURVE_CLASS)
		return;
	
	if (t0 != 0. || t1 != 1.)
	{
		status = uc_retrieve_transf (c1.key,tran);
		um_c5_trimpart(&c1,ccrv.cid,ccrv.no_cid,t0,t1,jcv,tran);
		status = uc_retrieve_transf (ccrv.key,tran);
		tcv.key = c1.key;
		ncl_retrieve_data_fixed(&tcv);
	}
	else
	{
		tcv.key = c1.key;
		ncl_retrieve_data_fixed(&tcv);
	}

	NCL_allcurve = UU_TRUE;
	NCL_multi_ent = 1;
	i=0;

	if (ilab > 0)
	{
		buf[0]='\0';
		sprintf(buf,"%s%d",lb0,ilab);
		len = strlen(buf);
/*
.....If the next label is equal to the composite curve label,then skip this
.....label and update saveid.This prevents the components from being labeled
.....the same as the composite curve label, thus avoiding replacment of the
.....composite curve
*/
		while(ccrv.label[i]==buf[i] && ccrv.label[i]!=' ' )
			i++;
		if(i && ccrv.label[i]==' ' && len==i)
		{
			ncl_sfs_update_saveid (lb0,&ilab,&isubscr);
			setifl(&isub,&ilab);
		}
/*
.....If the canon flag is off and the next label already exists,then skip it
.....and update saveid, till you get an unsued label.This prevents the
.....components from being labeled the same as the composite curve label.
*/
		while (caon == 0)
		{
			sprintf(buf,"%s%d",lb0,ilab);
			i4 = isubscr;
			keyi = NULLKEY;
			chklab (str77, &keyi, &i4, &ifnd, &caon);
			if (ifnd == 1)
			{
				ncl_sfs_update_saveid (lb0,&ilab,&isubscr);
				setifl(&isub,&ilab);
			}
			else
				break;
		}
	}
	status = uc_copy (&tcv,&c2,size);
	ur_update_data_fixed(&c1);

	if (status == UU_SUCCESS && c2.rel_num == NCL_CURVE_REL)
	{
		status = ncl_store_wf2(c2.key,c2.rel_num,c2.label,c2.subscr);
	}

	NCL_multi_ent = 0;
	NCL_allcurve = UU_FALSE;
	if (status == UU_SUCCESS)
	{
	isub = 296; getifl(&isub,&ilab);
		ncl_sfs_update_saveid (lb0,&ilab,&isubscr);
/*
..... check if label should be on
*/
		if (ncl_get_type(c2.rel_num, &i2) == UU_SUCCESS)
		{
			lblchk(&i2,&lblst);
			if (lblst > 0)
			{
				uc_retrieve_attr(c2.key, &attr);
				attr.label_on = lblst;
				ur_update_attr(&attr);
			}
		}
		uc_transform(&c2,tran,UU_TRUE);
		ncl_def_color (c2.key);
		ur_update_displayable(c2.key, UM_DISPLAYABLE);
		uc_display (&c2);
	}

	if (status == UU_SUCCESS) *ier = 0;

	return;
}

/*********************************************************************
**    E_FUNCTION     : ncontr(off8,zlev8,tol8,ier)
**       Create a spline curve as a contour around a list of surfaces
**    PARAMETERS
**       INPUT  :
**          tol8     - tolerance
**       OUTPUT :
**          ier     - error number, if fail
**    RETURNS      :
**       nclkey  - key of created spline, 0 if fail
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncontr(off8,zlev8,nvec8,tol8,ier)
UM_real8 *tol8,*off8,*zlev8,*nvec8;
UM_int2 *ier;
{
	UU_KEY_ID *sfkey;
	UU_KEY_ID nclkey = NULLKEY;
	int i,numsf,status,n1,irot;
	UU_REAL toler,offdis,zlev;
	UM_int2 isub;
	int mcsflg = 0,NPT;
	UU_REAL zmax,zmin;
	NCL_waterline_surf *sff = UU_NULL;
	UM_transf rot,rtinv;
	UM_coord *ptio;
	UM_vector nvec,*vs;
	UU_LIST *points = UU_NULL;
	struct NCL_crvgen_rec seg,*segp;
	UU_LIST tangs;
	struct UM_rbsplcrv_rec cv;
	ncl_polygon *pol0;

	uu_list_init (&tangs,sizeof(UM_vector),0,100);
	uu_list_init (&seglist,sizeof(struct NCL_crvgen_rec),0,100);
	*ier = 358;

	ncl_getnum_listkey (0,&numsf);
	if (numsf < 1)
	{
		ncl_free_keylist(0);
		goto Err;
	}
	ncl_get_listkeys (0,&sfkey);

	*ier = 0;
	status = UU_SUCCESS;

	toler = *tol8; zlev = *zlev8; offdis = *off8;
	for (i = 0; i < 3; i++) nvec[i] = nvec8[i];

	isub = 264;
	getifl(&isub,&mm);
	tol = (mm)? toler*25.4: toler;
	tolsq = tol*tol;

	ncl_define_rotmx (mm,nvec,rot,&mcsflg);
	ncl_get_rotmx (&irot,rtinv);

	sff = (NCL_waterline_surf *) uu_malloc (numsf * sizeof (*sff));
	if (!sff) return (UU_FAILURE);

	NPT = 1;
	status = ncl_process_netsf (1,numsf,sfkey,rot,&zmax,&zmin,sff,tol,toler,
		&NPT,0.,UU_NULL,UU_NULL,UU_NULL);
	if (status != UU_SUCCESS) goto Err;

	if (NPT < 100) NPT = 100;

	status = ncl_init_wpol (0,2*NPT);
	if (status == UU_SUCCESS) ncl_init_wpol (1,NPT);

	if (status != UU_SUCCESS) goto Err;

	status = ncl_create_contour_stock (numsf,sff,rot,tol,tolsq);

	if (status > 0)
	{
		*ier = -status; status = UU_SUCCESS;
	}
/*
..... offset the stock by the expansion factor
*/
	if (fabs(offdis) < 2*tol) offdis = 0;

	if (status == UU_SUCCESS)
	{
		ncl_get_wpol (0,&pol0);
		status = ncl_offset_out0 (&points,pol0,&tangs,offdis,tol);
	}

	if (status != UU_SUCCESS || !points || points->cur_cnt < 4) goto Err;

	ptio = (UM_coord *) UU_LIST_ARRAY (points);
	vs = (UM_vector *) UU_LIST_ARRAY (&tangs);
	n1 = points->cur_cnt;

	for (i = 0; i < n1; i++)
	{
		ncl_init_seg (&seg);
		ptio[i][2] = zlev;
		if (irot > 0)
		{
			um_cctmtf (ptio[i],rtinv,ptio[i]);
		}
		seg.x = ptio[i][0];
		seg.y = ptio[i][1];
		seg.z = ptio[i][2];
		if (UM_DOT(vs[i],vs[i]) > 0.009)
		{
			if (irot > 0)
			{
				um_vctmtf (vs[i],rtinv,vs[i]);
			}
			seg.a = vs[i][0];
			seg.b = vs[i][1];
			seg.c = vs[i][2];
			seg.inv = 1;
		}
		uu_list_push (&seglist, &seg);
	}

	segp = (struct NCL_crvgen_rec *) UU_LIST_ARRAY (&seglist);

	status = ncl_interp_rbsp (n1, segp, 1, &cv);
	nclkey = cv.key;

	if (status != UU_SUCCESS || *ier > 0 || nclkey == NULLKEY) goto Err;

	ncl_def_color (nclkey);

	if (status == UU_SUCCESS) goto Done;

Err:
	if (*ier == 0) *ier = 163;
	status = UU_FAILURE;

Done:
	ncl_free_keylist(0);
	if (sff)
	{
		NCL_waterline_surf *p1;
		p1 = sff;
		for (i = 0; i < numsf && p1->key != NULLKEY; i++, p1++)
		{
			ncl_free_bound (&p1->bound);
			UU_LIST_FREE (p1->trianlist);
		}
		UU_FREE (sff);
	}
	ncl_free_wpols();
	uu_list_free (&tangs);
	uu_list_free (&seglist);

	return (nclkey);
}

/*********************************************************************
**    E_FUNCTION     : UU_LOGICAL ncl_cv_isline (npts,pts)
**       Check whether points are on a line
**    PARAMETERS
**       INPUT  :
**          npts       - number of points in list
**          pts        - points in list
**          tol        - tolerance
**          tolsq      - squared tolerance
**       OUTPUT : none
**
**    RETURNS      :
**         UU_TRUE / UU_FALSE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL ncl_cv_isline (npts,pts)
int npts;
UM_coord *pts;
{
	UM_vector uvc;
	int i;
	UU_REAL d,t;

	uvc[0] = pts[npts-1][0] - pts[0][0];
	uvc[1] = pts[npts-1][1] - pts[0][1];
	d = uvc[0]*uvc[0] + uvc[1]*uvc[1];
	if (d < tolsq) return (UU_FALSE);
	d = sqrt(d);
	uvc[0] /= d; uvc[1] /= d; uvc[2] = 0;

	for (i = 1; i < npts-1; i++)
	{
		if (!um_ptinsg(pts[0],uvc,d,pts[i],tol,&t)) return (UU_FALSE);
	}
	return (UU_TRUE);
}

/*********************************************************************
**    E_FUNCTION     : UU_LOGICAL S_fit_arc (np,pts,center,radius,ccw,ctol)
**       Do the least squares fit for a circular arc.
**    PARAMETERS
**       INPUT  :
**          np         - number of points
**          pts        - points
**          ccw        - fit a ccw arc if 1, cw arc if -1
**          eps        - squared tolerance
**       OUTPUT :
**          cen        - arc center
**          radius     - arc radius
**    RETURNS      :
**         UU_TRUE / UU_FALSE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UU_LOGICAL S_fit_arc (np,pts,center,radius,ccw,eps)
int np,ccw;
UM_coord *pts;
UM_2Dcoord center;
UU_REAL *radius,eps;
{
	int j,k;
	UM_2Dcoord va,pa,vb,vj;
	UU_REAL da,a0,cj,aj,A,B,rsq,c;

	for (k = 0; k < 2; k++)
	{
		va[k] = pts[np-1][k] - pts[0][k];
		pa[k] = (pts[np-1][k] + pts[0][k])/2;
	}

	a0 = UM_DOT_2D (va,va);
	da = sqrt(a0);

	a0 /= 4;

	if (a0 < UM_DFUZZ) return (UU_FALSE);

	for (k = 0; k < 2; k++)
	{
		va[k] /= da;
	}
	vb[0] = -va[1]*ccw;
	vb[1] = va[0]*ccw;
/*
..... pa is the midpoint of the segment (pts[0],pts[np-1]); a0 is half
..... the segment length (squared); vb is the unit vector perp to the segment
*/
	A = B = 0;
	for (j = 1; j < np-1; j++)
	{
		um_vcmnvc_2d (pts[j],pa,vj);
		cj = UM_DOT_2D (vj,vb);
		aj = UM_DOT_2D (vj,vj);
		A += cj*(aj - a0);
		B += cj*cj;
	}
	if (B < UM_DFUZZ) return (UU_FALSE);
/*
..... the c parameter is the signed distance from pa along vb - to the center.
..... We minimize Sum_j of (rj^2 - r^2)^2, where r is the distance from the
..... center to pts[0] or pts[np-1], and rj is the distance to pts[j].
.....   
*/
	c = A/(2*B);
	rsq = c*c + a0;
	*radius = sqrt(rsq);

	for (k = 0; k < 2; k++)
	{
		center[k] = pa[k] + c*vb[k];
	}

	return (UU_TRUE);
}

/*********************************************************************
**    E_FUNCTION     : UU_LOGICAL ncl_cv_isarc (npts,pts,center,radius,cclw,
**                       anglefl)
**       Check whether points are on a circular arc.
**    PARAMETERS
**       INPUT  :
**          npts       - number of points in list
**          pts        - points in list
**          tol        - tolerance
**          tolsq      - squared tolerance
**          anglefl    - Flag denotes whether to check angles
**       OUTPUT :
**          cen        - arc center
**          rad        - arc radius
**          cclw       - ccw arc if 1, cw arc if -1
**    RETURNS      :
**         UU_TRUE / UU_FALSE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL ncl_cv_isarc (npts,pts,center,radius,cclw,anglefl)
int npts,*cclw;
UM_coord *pts,center;
UU_REAL *radius;
UU_LOGICAL anglefl;
{
	int k,j,j1,j2,i1[2],i2[2],np,ccw;
	UM_2Dcoord cen,v0,v1;
	UU_REAL rad,del,rsum,rj,dj,dot,tol1,tol2,co[2];
	UU_LOGICAL lfull;

	tol2 = 100*tolsq;

	if (UM_SQDIS_2D(pts[0],pts[npts-1]) < tol2)
	{
		lfull = UU_TRUE;
		np = npts-1;
	}
	else
	{
		lfull = UU_FALSE;
		np = npts;
	}
/*
..... find good 3 points to build the initial circle
*/
	i1[0] = np/2; i2[0] = np - 1;
	i1[1] = np/3; i2[1] = 2*np/3;

	for (k = 0; k < 2; k++)
	{
		um_vcmnvc_2d (pts[0],pts[i1[k]],v0);
		um_vcmnvc_2d (pts[i2[k]],pts[i1[k]],v1);
		co[k] = um_cosang_2d (v0,v1);
	}

	k = (fabs(co[1] - 0.5) < fabs(co[0] - 0.5))? 1: 0;
	if (anglefl && fabs(co[k]) > UM_COS15) return (UU_FALSE);
	j1 = i1[k]; j2 = i2[k];

	if (um_3pt_circle (pts[0],pts[j1],pts[j2],cen,&rad,&ccw,tol2,rmax) != 0)
		return (UU_FALSE);
/*
..... accept only no less than 15 degrees
*/
	if (!lfull)
	{
		dj = um_triangle_signed_area (cen,pts[0],pts[np-1]);
/*
..... if center and endpoints are oriented same way as the arc, the arc must
..... be less than 180 degrees
*/
		if (dj*ccw > 0)
		{
			um_vcmnvc_2d (pts[0],cen,v0);
			um_vcmnvc_2d (pts[np-1],cen,v1);
			dot = um_cosang_2d (v0,v1);
			if (anglefl && dot > UM_COS15) return (UU_FALSE);
		}
	}

	if (k == 0 && !lfull && !ncl_setver(96))
	{
/*
..... qar 97141: build the least squares arc on the inner points, so that
..... the endpoints are on the circle.
*/
		if (!S_fit_arc (np,pts,cen,&rad,ccw,tolsq))
			return (UU_FALSE);
	}
	else
	{
		rsum = rad;
		tol1 = 5*tol;

		for (j = 1; j < np; j++)
		{
			if (j == j1 || j == j2)
				rj = rad;
			else
			{
				rj = UM_DIST_2D(pts[j],cen);
				if (fabs(rj - rad) > tol1) return(UU_FALSE);
			}
			rsum += rj;
		}
/*
..... use the average distance from center as the radius
*/
		rad = rsum/np;
	}

	del = 10.*rad*tol;
/*
..... del is the maximum squared length of a segment that could be put, within
..... tolerance, on this circle (the exact formula is 4*(2*rad*tol - tolsq))
*/
	tol1 = 2*tol;
/*
..... check if the arc can be accepted
*/
	for (j = 0; j < np; j++)
	{
		rj = UM_DIST_2D(pts[j],cen);
		if (fabs(rj - rad) >= tol1) return (UU_FALSE);
		if (j > 0)
		{
			dj = UM_SQDIS_2D(pts[j],pts[j-1]);
			if (dj > del) return (UU_FALSE);
		}
	}

	if (!lfull)
		*cclw = ccw;
	else
		*cclw = 0;

	*radius = rad;
	for (j = 0; j < 2; j++) center[j] = cen[j];
	center[2] = pts[0][2];

	return (UU_TRUE);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_sfs_create1 (geo, gkey, cnon, nolab, ier)
**       Create, label, and store a curve-type entity
**    PARAMETERS
**       INPUT  :
**          geo        - pointer to the curve data struct, already filled
**          cnon       - the canon/on logical: 1 iff ON
**          nolab      - @UN logical: true iff the flag is ON
**       OUTPUT :
**          gkey       - curve (line, circle) key
**          ier        - error number
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int ncl_sfs_create1 (geo, gkey, cnon, nolab, ier)
struct UC_entitydatabag *geo;
UU_KEY_ID *gkey;
UM_int2 cnon,*ier;
UU_LOGICAL nolab;
{
	int status,itype;
	UM_int2 i2,ifnd;
	UM_int4 isub4;
	struct NCL_fixed_databag *eptr;
	UU_KEY_ID keyi = NULLKEY;
	UM_f77_str_ptr str77;

	UM_init_f77_str (str77, buf, 80);

	geo->key = NULLKEY;
	eptr = (struct NCL_fixed_databag *) geo;

	i2 = 0;
	status = ncl_label_wf(eptr->rel_num,eptr->label,&eptr->subscr,eptr->key,&i2);
/*
..... output 'identifier previously defined' if ca/off and the label
..... is already used
*/
	if (cnon == 0 && status == UU_SUCCESS)
	{
		sprintf(buf,"%s",eptr->label);
		isub4 = eptr->subscr;

		chklab (str77, &keyi, &isub4, &ifnd, &cnon);
		if (ifnd == 1)
		{
			*ier = 8;
			return (UU_FAILURE);
		}
	}

	if (status == UU_SUCCESS)
	{
		ncl_get_type (geo->rel_num,&i2);
		itype = i2;
		status = ncl_create_entity (geo, itype);
		if (status == UU_SUCCESS)
		{
/*
..... store_wf1 just calls stores; store_wf2 checks if the label belongs to an
..... existing entity, deletes it if found, and then stores the current one.
*/
			if (nolab)
				ncl_store_wf1(eptr->key);
			else
				ncl_store_wf2(eptr->key,eptr->rel_num,eptr->label,eptr->subscr);
			*gkey = eptr->key;
		}
	}
	return (status);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_sfs_interp_rbsp (n,ptve,geo,gkey,cnon,nolab,ier)
**       Create a uniform rb-spline by a list of points
**    PARAMETERS
**       INPUT  :
**          n          - number of points
**          ptve       - the points
**          cnon       - the canon/on logical: 1 iff ON
**          nolab      - @UN logical: true iff the flag is ON
**       OUTPUT :
**          geo        - curve data struct
**          gkey       - curve (line, circle) key
**          ier        - error number
**
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int ncl_sfs_interp_rbsp (n, ptve, geo, gkey, cnon, nolab, ier)
int n;
struct NCL_crvgen_rec *ptve;
struct UC_entitydatabag *geo;
UU_KEY_ID *gkey;
UM_int2 cnon,*ier;
UU_LOGICAL nolab;
{
	int status;
	UU_REAL *s, *pts;
	int npts,ns;
	struct UM_rbsplcrv_rec *crv;

	s = pts = UU_NULL;
/*
.....generate B-spline using ptve
*/
	status = ncl_interp_rbsp1 (n, ptve, 1, &npts, &s, &pts);

	if (status == UU_SUCCESS)
	{
		crv = (struct UM_rbsplcrv_rec *) geo;

		ncl_rbsp_data_fixed (npts, s, pts, &ns, crv);
		crv->no_wt = 0; crv->wt = UU_NULL;

		status = ncl_sfs_create1 (geo, gkey, cnon, nolab, ier);

		if (status == UU_SUCCESS)
			status = ur_update_data_varlist (crv->key, 1, s, 1, ns);
		if (status == UU_SUCCESS)
			status = ur_update_data_varlist (crv->key, 2, pts, 1, npts);
	}

	if (s != UU_NULL) uu_free(s);
	if (pts != 0) uu_free(pts);

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : UU_KEY_ID ncl_sfs_create_geo1 (npts,pts,nolab,itsk,ier)
**       Create a curve-like entity by a list of points
**    PARAMETERS
**       INPUT  :
**          npts       - number of points in list
**          pts        - points in list
**          nolab      - @UN logical: true iff the flag is ON
**          itsk       - try to create a line or an arc if 0;
**                       splines only if 1
**       OUTPUT :
**          ier        - error number
**    RETURNS      : entity key
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_KEY_ID ncl_sfs_create_geo1 (npts,pts,nolab,itsk,ier)
int npts,itsk;
UM_coord *pts;
UU_LOGICAL nolab;
UM_int2 *ier;
{
static struct UM_line_rec ln;
static struct UM_circle_rec ci;
static struct UM_rbsplcrv_rec cv;
	int i,status,irot;
	UM_transf rtinv;
	struct UC_entitydatabag *geo = UU_NULL;
	struct NCL_crvgen_rec seg,*segp;
	UU_KEY_ID gkey = NULLKEY;
	UM_coord ce;
	UM_vector vc;
	UU_REAL ra = 0;
	UU_REAL pm = (mm == 1)? 25.4: 1;
	int ccw;
	UM_int2 i2,cnon;

	status = UU_SUCCESS;
/*
..... get the canon/on flag
*/
	i2 = 41;
	getifl (&i2,&cnon);

	NCL_allcurve = UU_TRUE;
	if (nolab)
		cnon = 1;
	else
		NCL_multi_ent = 1;

	ncl_get_rotmx (&irot,rtinv);

	if (itsk == 0)
	{
		if (ncl_cv_isline (npts,pts))
		{
			if (irot > 0)
			{
				um_cctmtf (pts[0],rtinv,ln.spt);
				um_cctmtf (pts[npts-1],rtinv,ln.ept);
			}
			else
			{
				um_vctovc (pts[0],ln.spt);
				um_vctovc (pts[npts-1],ln.ept);
			}

			ur_setup_data(UM_LINE_REL, &ln, sizeof(ln));
			geo = (struct UC_entitydatabag *) &ln;

			status = ncl_sfs_create1 (geo, &gkey, cnon, nolab, ier);
		}
		else if (rmax > 0 && ncl_cv_isarc (npts,pts,ce,&ra,&ccw,UU_TRUE))
		{
			ci.nvec[0] = ci.nvec[1] = 0;
			ci.nvec[2] = (ccw >= 0)? pm: -pm;
			um_vcmnvc (pts[0],ce,vc);

 			ci.radius = ra/pm;

			if (irot > 0)
			{
				um_cctmtf (ce,rtinv,ci.center);
				um_vctmtf (ci.nvec,rtinv,ci.nvec);
				um_vctmtf (vc,rtinv,vc);
			}
			else
				um_vctovc (ce,ci.center);

			um_unitvc (vc,ci.svec);

			if (ccw == 0)
				ci.dang = UM_TWOPI;
			else
			{
				um_vcmnvc (pts[npts-1],ce,vc);
				if (irot > 0) um_vctmtf (vc,rtinv,vc);
				um_unitvc (vc,vc);
				ci.dang = um_angle2p(ci.svec, vc, ci.nvec);
			}

			ur_setup_data(UM_CIRCLE_REL, &ci, sizeof(ci));
			geo = (struct UC_entitydatabag *) &ci;
			status = ncl_sfs_create1 (geo, &gkey, cnon, nolab, ier);
		}
	}

	if (status == UU_SUCCESS && gkey == NULLKEY)
	{
		seglist.cur_cnt = 0;
		for (i = 0; i < npts; i++)
		{
			ncl_init_seg (&seg);
			if (irot > 0) um_cctmtf (pts[i],rtinv,pts[i]);
			seg.x = pts[i][0];
			seg.y = pts[i][1];
			seg.z = pts[i][2];

			uu_list_push (&seglist, &seg);
		}

		segp = (struct NCL_crvgen_rec *) UU_LIST_ARRAY (&seglist);

		ur_setup_data (UM_RBSPLCRV_REL, &cv, sizeof(cv));
		geo = (struct UC_entitydatabag *) &cv;

		status = ncl_sfs_interp_rbsp (npts, segp, geo, &gkey, cnon, nolab, ier);
	}

	NCL_allcurve = UU_FALSE;
	NCL_multi_ent = 0;

	if (status != UU_SUCCESS || gkey == NULLKEY)
	{
		status = UU_FAILURE;
		if (gkey != NULLKEY)
		{
			uc_delete (gkey); gkey = NULLKEY;
		}
	}
	else if (!nolab)
	{
		ncl_def_color (gkey);
		ur_update_displayable(gkey, UM_DISPLAYABLE);
		uc_display (geo);
		Sfs_created++;
	}

	return (gkey);
}

/*********************************************************************
**    E_FUNCTION     : UU_KEY_ID ncl_sfs_create_geo (npts,pts,nolab,ier)
**       Create a curve-like entity by a list of points
**    PARAMETERS
**       INPUT  :
**          npts       - number of points in list
**          pts        - points in list
**          nolab      - @UN logical: true iff the flag is ON
**       OUTPUT :
**          ier        - error number
**    RETURNS      : entity key
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UU_KEY_ID ncl_sfs_create_geo (npts,pts,nolab,ier)
int npts;
UM_coord *pts;
UU_LOGICAL nolab;
UM_int2 *ier;
{
	return (ncl_sfs_create_geo1 (npts,pts,nolab,0,ier));
}

/*********************************************************************
**    E_FUNCTION     : void ncl_add_contour (curves,loop,nj,npts,pts)
**       Add contour data to the list of contours
**    PARAMETERS
**       INPUT  :
**       OUTPUT :
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void ncl_add_contour (curves,loop,nj,npts,pts)
UU_LIST *loop,*curves;
int *nj,*npts;
UM_coord *pts;
{
	NCL_sfs_loop contour;
	int *looplink;
	int ie,ib,in,i,j,n0,nnp;
	UU_REAL wmid;

	int nlinks = loop->cur_cnt;

	if (nlinks <= 0) return;

	contour.nlinks = nlinks;
	contour.lkey = NULLKEY;
	contour.link = (int *) uu_malloc (nlinks * sizeof (int));
	if (contour.link == UU_NULL) return;

	looplink = (int *) UU_LIST_ARRAY (loop);
	ib = nj[looplink[0]];
	in = looplink[nlinks-1];
	nnp = npts[in];
	ie = nj[in] + nnp - 1;
	for (j = 0; j < 2; j++)
	{
		wmid = (pts[ie][j] + pts[ib][j]) / 2;
		pts[ie][j] = pts[ib][j] = wmid;
	}

	for (i = 0; i < nlinks; i++)
	{
		in = looplink[i];
		contour.link[i] = in;
		nnp = npts[in];
		n0 = nj[in];
		if (i+1 < nlinks)
		{
			ie = n0+nnp-1;
			ib = nj[looplink[i+1]];
			for (j = 0; j < 2; j++)
			{
				wmid = (pts[ie][j] + pts[ib][j]) / 2;
				pts[ie][j] = pts[ib][j] = wmid;
			}
		}
	}

	uu_list_push (curves,&contour);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_sfs_create_contours (curves,nj,lb0,ilab,ier)
**       Create composite curves by contours made of points
**    PARAMETERS
**       INPUT  :
**       OUTPUT :
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int ncl_sfs_create_contours (curves,nj,lb0,ilab,ier)
UU_LIST *curves;
int *nj;
char *lb0;
UM_int2 ilab,*ier;
{
	int nloops,nlinks,i,l,in,n0,nnp,status;
	int *npts,*looplink;
	NCL_sfs_loop *loop;
	UM_coord *pts;
	struct UM_compcrv_rec ccvp;
	UU_LIST comkys,delkys;
	UU_KEY_ID keyi,*ky;
	UM_int2 lfl_77,i2,lblst,cnon;
	UM_int2 ifnd;
	UM_int4 i4;
	struct NCL_nclattr_rec attr;
	UM_f77_str_ptr str77;

	UM_init_f77_str (str77, buf, 80);
	status = UU_SUCCESS;

	i2 = 41;
	getifl (&i2,&cnon);

	nloops = curves->cur_cnt;
	loop = (NCL_sfs_loop *) UU_LIST_ARRAY (curves);

	npts = (int *) UU_LIST_ARRAY (&nio);
	pts = (UM_coord *) UU_LIST_ARRAY (&ptsio);

	uu_list_init (&comkys, sizeof(UU_KEY_ID),10,10);
	uu_list_init (&delkys, sizeof(UU_KEY_ID),10,10);
	if (comkys.data == UU_NULL || delkys.data == UU_NULL)
	{
		uu_list_free (&comkys);
		uu_list_free (&delkys);
		return (UU_FAILURE);
	}

	for (l = 0; l < nloops; l++)
	{
		nlinks = loop[l].nlinks;
		if (nlinks > 0)
		{
			comkys.cur_cnt = 0;

			lfl_77 = 1;
			stunlb (&lfl_77);

			looplink = loop[l].link;

			for (i = 0; i < nlinks; i++)
			{
				in = looplink[i];
				nnp = npts[in];
				n0 = nj[in];
				keyi = NULLKEY;
				keyi = ncl_sfs_create_geo(nnp,&pts[n0],UU_TRUE,ier);
				if (keyi == NULLKEY)
				{
					status = UU_FAILURE; goto Err;
				}
				uu_list_push (&comkys,&keyi);
				uu_list_push (&delkys,&keyi);
			}
			stunlb (&lfl_77);

			if (status == UU_SUCCESS)
			{
				ccvp.key = NULLKEY;
				ky = (UU_KEY_ID *) UU_LIST_ARRAY (&comkys);
  				status = um_c5_mergecrv (nlinks,ky,&ccvp);
			}

			if (status == UU_SUCCESS)
			{
				i2 = 0;
				ncl_label_wf(ccvp.rel_num,ccvp.label,&ccvp.subscr,ccvp.key,&i2);

				if (cnon == 0)
				{
					sprintf(buf,"%s",ccvp.label);
					i4 = ccvp.subscr;
					keyi = NULLKEY;
					chklab (str77, &keyi, &i4, &ifnd, &cnon);
					if (ifnd == 1)
					{
						*ier = 8;
						status = UU_FAILURE;
						goto Err;
					}
				}

  				status =
				uc_create_mtuple_data (&ccvp,UM_DEFAULT_TF,UM_CURRENT_ATTR);
				if (status == UU_SUCCESS)
				{
					ncl_store_wf2(ccvp.key,ccvp.rel_num,ccvp.label,ccvp.subscr);
					loop[l].lkey = ccvp.key;
					ncl_get_type(ccvp.rel_num, &i2);
					lblchk(&i2,&lblst);
					if (lblst > 0)
					{
						uc_retrieve_attr(ccvp.key, &attr);
						attr.label_on = lblst;
						ur_update_attr(&attr);
					}
				}
			}
			if (status != UU_SUCCESS) goto Err;

			ncl_sfs_update_saveid (lb0,&ilab,&isubscr);

			ncl_def_color (ccvp.key);
			ur_update_displayable(ccvp.key, UM_DISPLAYABLE);
			uc_display ((struct UC_entitydatabag *)&ccvp);
		}
	}

	if (status == UU_SUCCESS)
	{
		Sfs_created += nloops;
		goto Done;
	}

Err:
	for (l = 0; l < nloops; l++)
	{
		if (loop[l].lkey != NULLKEY) uc_delete(loop[l].lkey);
	}

Done:
	uu_list_free (&comkys);
	in = delkys.cur_cnt;
	if (in > 0)
	{
		ky = (UU_KEY_ID *) UU_LIST_ARRAY (&delkys);
		for (i = 0; i < in; i++)
		{
			if (ky[i] != NULLKEY) uc_delete(ky[i]);
		}
	}
	uu_list_free (&delkys);

	return (status);
}

/*********************************************************************
**    S_FUNCTION     :  Sfdisp0()
**       Retrieve and save display data for all surfaces, then erase.
*********************************************************************/
static void Sfdisp0()
{
	UU_KEY_ID *sfkey;

	dispmode = (NCL_sfs_dispmode *) uu_malloc (sfnum*sizeof(NCL_sfs_dispmode));
	if (dispmode == UU_NULL) return;
	ncl_get_listkeys (0,&sfkey);

	if (sfkey != UU_NULL) ncl_get_dispmodes (sfkey,sfnum,dispmode);
}

/*********************************************************************
**    S_FUNCTION     :  Sfdisp1()
**       Redisplay all surfaces as they were originally.
*********************************************************************/
static void Sfdisp1()
{
	UU_KEY_ID *sfkey;

	ncl_get_listkeys (0,&sfkey);
	if (sfkey != UU_NULL) ncl_redisp_sfs (sfkey,sfnum,dispmode);
}

/*********************************************************************
**    S_FUNCTION     :  OnSfDisp()
**       Display the layer surfaces in the way selected by user.
*********************************************************************/
static UD_FSTAT OnSfdisp()
{
	UU_KEY_ID *sfkey;

	if (sfview == sfdisp) return ((UD_FSTAT)0);
	sfview = sfdisp;

	ncl_get_listkeys (0,&sfkey);

	if (sfkey != UU_NULL) ncl_disp_sfs (sfkey,sfnum,sfdisp);

	return ((UD_FSTAT)0);
}

/*********************************************************************
**    S_FUNCTION     :  OnAllcv()
**       Display or erase all surface intersections.
*********************************************************************/
static UD_FSTAT OnAllcv()
{
	int i,nseg;
	Gseg *psg;
	Gsegvis vis;

	if (allcv == 1)
		vis = UG_VISIBLE;
	else
		vis = UG_INVISIBLE;

	ncl_get_psegs (&nseg,&psg);
/*
..... the segments up to iseg0 are displayed always. iseg0 is set
..... in ncl_disp_connect_error
*/
	for (i = iseg0; i < nseg; i++)
		gssegvis(psg[i],vis);

	return ((UD_FSTAT)0);
}

/*********************************************************************
**    S_FUNCTION     :  OnFit()
**       Zoom in on the problem area.
**    PARAMETERS
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnFit()
{
	uz_extrema_zoom();
	return ((UD_FSTAT)0);
}

/*********************************************************************
**    S_FUNCTION     :  OnView()
**       Routine to enter dynamic viewing from the Current Status form.
*********************************************************************/
static UD_FSTAT OnView()
{
	if (Sfrm==-1)
		return (UD_FLDOK);
	ud_dspfrm_invis(Sfrm);
	ud_form_invis();
	uz_dyn_mouse();
	ud_form_vis();
	ud_dspfrm_vis(Sfrm);

	return (UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnClose()
**       Method called when the Current Status form is closed.
*********************************************************************/
static UD_FSTAT OnClose()
{
/*
..... redisplay the surfaces
*/
	if (dispmode != UU_NULL) Sfdisp1();
/*
.....Free the lists
*/
	ncl_free_keylist(0);
	UU_FREE (dispmode);
	ncl_free_segs();
	iseg0 = 0;
	ud_free_flist(&stat_list);
	uu_list_init0 (&strings);
	Sactive = UU_FALSE;
	uz_repaint(0);
	return ((UD_FSTAT)0);
}

/*********************************************************************
*********************************************************************/
static void S_put_list(list,sbuf)
UD_LIST *list;
char *sbuf;
{
	char *buf;
	int strlen();

	buf = (char *) uu_malloc((strlen(sbuf)+1)*sizeof(char));
	strcpy (buf,sbuf);
	uu_list_push (&strings,&buf);

	list->item = (char **) UU_LIST_ARRAY(&strings);
	list->num_item++;
}

/*********************************************************************
*********************************************************************/
static void S_msg_list(buf)
char *buf;
{
	if (Sfrm==-1)
		return;
	S_put_list(&stat_list,buf);
	strcpy(stat_list.answer,buf);
	ud_dispfrm_update_answer(Sfrm,0,(int *)&stat_list);
	ud_update_form(Sfrm);
}

/*********************************************************************
**    E_FUNCTION   : ncl_connect_msg (lnj,closenj,htop0,sff,lpt,closept)
**
**  Output the size of the gap, and between which surfaces and
**  which points it occured.
**    PARAMETERS
**       INPUT  :
**          htop0      - current level
**          sff        - data for each surface
**          lpt        - last connected point, if couldn't connect all
**          closept    - nearest (unconnected) curve endpoint to lpt
**          lnj        - position of lpt in the points list
**          closenj    - position of closept in the points list
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void ncl_connect_msg (lnj,closenj,htop0,sff,lpt,closept)
int lnj,closenj;
UU_REAL htop0;
NCL_waterline_surf *sff;
UM_coord lpt,closept;
{
	UU_KEY_ID key0,key1;
	char lab0[80],lab1[80];
	UU_REAL d;

	buf[0] = '\0';

	ncl_find_2keys(sff,sfnum,lnj,closenj,&key0,&key1);

	if (key0 != NULLKEY)
		ncl_get_label_with_key(key0, lab0);
	else
		sprintf(lab0,"???");

	if (key1 != NULLKEY)
		ncl_get_label_with_key(key1, lab1);
	else
		sprintf(lab1,"???");

	ncl_get_gap_data (UU_NULL,lpt,closept,htop0,&d);

	buf[0] = '\0'; S_msg_list (buf);
	sprintf(buf,"***** ERROR *****");
	S_msg_list (buf);

	sprintf(buf,"Gap of %g between:",d);
	S_msg_list (buf);

	sprintf(buf,"%s at <%g,%g,%g> and",
				lab0,lpt[0],lpt[1],lpt[2]);
	S_msg_list (buf);

	sprintf(buf,"%s at <%g,%g,%g>.",
				lab1,closept[0],closept[1],closept[2]);
	S_msg_list (buf);
}

/*********************************************************************
*********************************************************************/
static void ncl_disp_connect_error (curves,ncur,loop,pts,npts,nj,in1)
UU_LIST *curves,*ncur,*loop;
int *npts,*nj,in1;
UM_coord *pts;
{
	UM_coord *cpts;
	int in,i,k,nlinks,icolor,np,ip,n0,nnp;
	int *looplink,*ns;
	NCL_sfs_loop *cvs;

	nlinks = loop->cur_cnt;
	looplink = (int *) UU_LIST_ARRAY (loop);

	for (k = np = 0; k < nlinks; k++)
	{
		in = looplink[k];
		np += (npts[in]-1);
	}

	np++;
	if (np >= 3)
	{
/*
..... draw the connected part
*/
		cpts = (UM_coord *) uu_malloc (np*sizeof(UM_coord));
		if (cpts != UU_NULL)
		{
			ip = 0;
			for (i = 0; i < nlinks; i++)
			{
				in = looplink[i];
				nnp = npts[in];
				n0 = nj[in];
				for (k = (i > 0); k < nnp; k++)
				{
					um_vctovc (pts[n0+k],cpts[ip]);
					ip++;
				}
			}
			icolor = 12;
			ncl_draw_polyline (np,cpts,icolor,1);
			uu_free (cpts);
		}
	}
/*
..... draw the nearest 'loose' segment - in red
*/
	if (in1 >= 0)
	{
		icolor = 3;
		nnp = npts[in1]; n0 = nj[in1];
		ncl_draw_polyline (nnp,&pts[n0],icolor,1);
	}
/*
..... draw the rest of 'loose' segments
*/
	ncl_getnum_psegs (&iseg0);
	ns = (int *) UU_LIST_ARRAY (ncur);
	icolor = 14;
	for (i = 0; i < ncur->cur_cnt; i++)
	{
		in = ns[i];
		if (ns[i] == in1) continue;
		nnp = npts[in]; n0 = nj[in];
		ncl_draw_polyline (nnp,&pts[n0],icolor,allcv);
	}
/*
..... draw the complete contours - in green
*/
	if (curves->cur_cnt > 0)
	{
		cvs = (NCL_sfs_loop *) UU_LIST_ARRAY (curves);
		icolor = 4;

		for (k = 0; k < curves->cur_cnt; k++)
		{
			nlinks = cvs[k].nlinks;
			if (nlinks > 0)
			{
				looplink = cvs[k].link;

				for (i = 0; i < nlinks; i++)
				{
					in = looplink[i];
					nnp = npts[in];
					n0 = nj[in];
					ncl_draw_polyline (nnp,&pts[n0],icolor,allcv);
				}
			}
		}
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_connect_error (igap,htop0,sff,numsf,nio,ptsio,
**										lpt,closept,GUI,buf,str77,errout,ier)
**
**  Output the size of the gap, and between which surfaces and
**  which points it occured.
**    PARAMETERS
**       INPUT  :
**          igap       - the position of the gap in the point list
**          htop0      - current level
**          numsf      - number of surfaces
**          sff        - data for each surface
**          nio        - list of lengths of pieces
**          ptsio      - list of points representing the curves
**          lpt        - last connected point, if couldn't connect all
**          closept    - nearest (unconnected) curve endpoint to lpt
**          GUI        - GUI flag
**       OUTPUT :
**          ier        - error number
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void ncl_connect_error (nj,lnj,in1,zlev,sff,lpt,closept)
int *nj;
int lnj,in1;
UU_REAL zlev;
NCL_waterline_surf *sff;
UM_coord lpt,closept;
{
	int closenj;
	static char traverse[] = {1,1,1,1,1};
	static UD_METHOD methods[] =
	{UU_NULL,OnAllcv,OnSfdisp,OnView,OnFit,OnClose};
	static char called[] = {6,6,6,6,6,6};
	static char display[] = {1,1,1,1,1};
	static int *ans[] = {(int *)&stat_list,&allcv,&sfdisp,UU_NULL,UU_NULL};

	stat_list.num_item = 0;
	stat_list.item = UU_NULL;
	stat_list.answer = UU_NULL;
	uu_list_init0 (&strings);

	if (Sactive)
	{
/*
.....why close the form when ative?
.....the change just make sure there is no memory error
.....but why close the form when form is open?
.....Yurong
*/
		if (Sfrm!=-1)
			ud_close_dispfrm (Sfrm); 
		Sfrm = -1;
		Sactive = UU_FALSE;
		return;
	}

	allcv = sfdisp = sfview = 0;
	Sactive = UU_TRUE;
	uu_list_init (&strings,sizeof(char *),100,100);
	stat_list.item = (char **) UU_LIST_ARRAY(&strings);
	stat_list.answer = (char *) uu_malloc(120*sizeof(char));
	strcpy(stat_list.answer," ");
	Sfrm = ud_form_display1("cvlev.frm", ans, ans, methods, called, display,
			traverse);
	if (Sfrm==-1)
		return;

	closenj = (in1 >= 0)? nj[in1]: -1;

	ncl_connect_msg (lnj,closenj,zlev,sff,lpt,closept);
}

/*********************************************************************
**    E_FUNCTION     : ncl_sfs_connect (nio,nj,ptsio,lpt,closept,
**                                                         dsec,curves)
**
**  For a collection of curves, connect the pieces into closed loops.
**    PARAMETERS
**       INPUT  :
**          nio        - list of lengths of pieces
**          ptsio      - list of points representing the curves
**          nj         - pointers to where each piece starts in ptsio
**       OUTPUT :
**          curves     - list of loops
**          lpt        - last connected point, if couldn't connect all
**          closept    - nearest (unconnected) curve endpoint to lpt
**          dsec       - the gap (distance between lpt and closept)
**    RETURNS      :
**         UU_SUCCESS if no error; a position of the gap in the point list,
**         if can be determined; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int ncl_sfs_connect (nj,sff,zlev,mxtol2,lb0,ilab,ier)
int *nj;
NCL_waterline_surf *sff;
UU_REAL zlev,mxtol2;
char *lb0;
UM_int2 ilab;
UM_int2 *ier;
{
	int in,j,status,*npts,nc,*ns,j0;
	int in1;
	UM_coord *pts;
	UU_REAL dis,told,dis0;
	UU_LIST ncur,loop,curves;
	UM_coord fpt,fpt1,lpt,lpt1,closept;
	int nnp;
	UU_LOGICAL closed,closed0,found,zig;
	int plist0;
	UM_int2 isub,lbatch;

	status = UU_SUCCESS;
	nc = nio.cur_cnt;
	if (nc < 1) return (status);

	npts = (int *) UU_LIST_ARRAY (&nio);
	pts = (UM_coord *) UU_LIST_ARRAY (&ptsio);

	uu_list_init (&ncur,sizeof(int),nc,nc);
	for (j = 0; j < nc; j++)
		uu_list_push(&ncur,&j);

	uu_list_init (&loop,sizeof(int),nc,nc);
	uu_list_init (&curves,sizeof(NCL_sfs_loop),10,10);

	closed = UU_TRUE;
	in1 = -1;
	plist0 = nj[0];
	j = j0 = 0;
	told = 2.25*tolsq;
	dis0 = 1.e12;
	zig = UU_FALSE;
	ns = (int *) UU_LIST_ARRAY (&ncur);
	while (ncur.cur_cnt > 0 && j < ncur.cur_cnt)
	{
		in = ns[j];
		if (closed)
		{
			loop.cur_cnt = 0;
			uu_list_push(&loop,&in);
			uu_list_delete (&ncur,0,1); ns = (int *) UU_LIST_ARRAY (&ncur);
			nnp = npts[in];

			in1 = -1;
			plist0 = nj[in];
			um_vctovc (pts[nj[in]],fpt);
			um_vctovc (pts[nj[in]+nnp-1],lpt);
			dis = UM_SQDIS(fpt,lpt);
			closed = (dis < told);
			if (!closed)
			{
				dis0 = dis; um_vctovc (fpt,closept);
				closed0 = UU_TRUE;
				if (ncur.cur_cnt < 1)
				{
					if (dis0 < mxtol2)
					{
						closed = UU_TRUE;
						ncl_add_contour (&curves,&loop,nj,npts,pts);
					}
					break;
				}
			}
		}
		if (!closed)
		{
			in = ns[j];
			nnp = npts[in];
			um_vctovc (pts[nj[in]],fpt1);
			um_vctovc (pts[nj[in]+nnp-1],lpt1);
			dis = UM_SQDIS(lpt,fpt1);
			found = (dis < told);
			if (!found)
			{
				if (dis < dis0)
				{
					closed0 = UU_FALSE;
					um_vctovc (fpt1,closept);
					dis0 = dis; j0 = j; in1 = in;
				}
				dis = UM_SQDIS(lpt,lpt1);
				found = (dis < told);
				if (found)
				{
					ncl_revers1_list (nnp,nj[in],pts,1);
					lpt1[0] = fpt1[0]; lpt1[1] = fpt1[1]; lpt1[2] = fpt1[2];
				}
				else if (dis < dis0)
				{
					closed0 = UU_FALSE;
					um_vctovc (lpt1,closept);
					dis0 = dis; j0 = j; in1 = in;
				}
			}
			if (found)
			{
				if (zig) ncl_zigzag (&loop,in,npts,nj,&ptsio,tol);
				j0 = 0;
				dis0 = 1.e12;
				told = 2.25*tolsq;
				zig = UU_FALSE;
				uu_list_push(&loop,&in);
				uu_list_delete (&ncur,j,1); ns = (int *) UU_LIST_ARRAY (&ncur);
				dis = UM_SQDIS(fpt,lpt1);
				closed = (dis < told);
				if (!closed)
				{
					if (ncur.cur_cnt < 1 && dis < mxtol2)
					{
						closed = UU_TRUE; goto CLSD;
					}
					dis0 = dis;
					closed0 = UU_TRUE;
					lpt[0] = lpt1[0]; lpt[1] = lpt1[1]; lpt[2] = lpt1[2];
					if (ncur.cur_cnt < 1) um_vctovc (fpt,closept);
					in1 = -1;
					plist0 = nj[in];
					j = 0;
				}
			}
			else
			{
				if (j < ncur.cur_cnt - 1 || dis0 >= mxtol2)
					j++;
				else
				{
					zig = UU_TRUE;
					closed = closed0;
					if (closed) goto CLSD;
					told = 1.0001*dis0;
					j = j0;
				}
			}
		}
CLSD:;
		if (closed)
		{
			ncl_add_contour (&curves,&loop,nj,npts,pts);
			zig = UU_FALSE;
			j = 0;
		}
	}

	if (!closed)
	{
		status = UU_FAILURE;
		*ier = 503;
		isub = 35; getifl(&isub,&lbatch);
		if (lbatch != 1)
		{
			allcv = 0;
			ncl_disp_connect_error (&curves,&ncur,&loop,pts,npts,nj,in1);
			ncl_connect_error (nj,plist0,in1,zlev,sff,lpt,closept);
		}
	}
	else
		status = ncl_sfs_create_contours (&curves,nj,lb0,ilab,ier);

	uu_list_free (&ncur);
	uu_list_free (&loop);
	uu_list_free (&curves);

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_sfs_contours (sff,zlev,mxtol2,lb0,ilab,GUI,ier)
**
**  For a collection of curves, connect the pieces into closed loops.
**  Collect some data and pass to ncl_sfs_connect, which does the work.
**    PARAMETERS
**       INPUT  :
**          sff      - waterline-type surface data
**          mxtol2   - tolerance-based parameter used when connecting pieces
**          zlev     - horizontal level (in the current plane-based coodinates)
**          lb0      - label constant prefix
**          ilab     - label subscript (numerical part of labels)
**          GUI      - GUI-type error output, iff 1
**       OUTPUT :
**          ier     - error number, if fail
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int ncl_sfs_contours (sff,zlev,mxtol2,lb0,ilab,GUI,ier)
NCL_waterline_surf *sff;
UU_REAL zlev,mxtol2;
char *lb0;
UM_int2 ilab,GUI,*ier;
{
	int j,nn,nnl,status;
	UU_LIST njlist;
	int *nnp,*nj;

	status = UU_SUCCESS;

	nn = nio.cur_cnt;
	if (nn < 1)
	{
/*
..... no intersections at this level
*/
		if (GUI == 1)
			ud_wrerr("No intersections.");
		else
			*ier = 502;
		return (UU_FAILURE);
	}

	nnl = MAX2 (nn,10);
	uu_list_init (&njlist, sizeof(int), nnl, nnl);

	nnp = (int *) UU_LIST_ARRAY (&nio);
	nnl = 0;

	uu_list_push (&njlist,&nnl);
	for (j = 0; j < nn-1; j++)
	{
		nnl += nnp[j];
		uu_list_push (&njlist,&nnl);
	}
	nnl += nnp[j];

	nj = (int *) UU_LIST_ARRAY (&njlist);

	status = ncl_sfs_connect (nj,sff,zlev,mxtol2,lb0,ilab,ier);

	uu_list_free (&njlist);
	return (status);
}

/*********************************************************************
**    E_FUNCTION     : UU_KEY_ID ncl_sfs_create_cvio (ptlst,itsk,ier)
**       Create a curve-like entity by a list of points
**    PARAMETERS
**       INPUT  :
**          ptlst      - list of points
**          itsk       - try to create a line or an arc if 0;
**                       splines only if 1
**       OUTPUT :
**          ier        - error number
**    RETURNS      : entity key
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UU_KEY_ID ncl_sfs_create_cvio (ptlst,itsk,ier)
UU_LIST *ptlst;
int itsk;
UM_int2 *ier;
{
	int npts;
	UM_coord *pts;

	npts = ptlst->cur_cnt;
	if (npts < 2) return (NULLKEY);
	pts = (UM_coord *) UU_LIST_ARRAY (ptlst);

	return (ncl_sfs_create_geo1 (npts,pts,UU_FALSE,itsk,ier));
}

/*********************************************************************
**    I_FUNCTION     : S_psds_dist (pte,pvec,pcon,dvec,dcon,da,db)
**       Get distances from a point to two planes
**    PARAMETERS
**       INPUT  :
**             pte         - original point
**             pvec,pcon   - first plane
**             dvec,dcon   - second plane
**       OUTPUT :
**             da          - distane to the first plane
**             db          - distane to the second plane
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_psds_dist (pte,pvec,pcon,dvec,dcon,dis)
UM_coord pte;
UM_vector pvec,dvec;
UU_REAL pcon,dcon,*dis;
{
	UU_REAL p,q;

	p = fabs (UM_DOT (pvec,pte) - pcon);
	q = fabs (UM_DOT (dvec,pte) - dcon);

	*dis = MAX2 (p,q);

	return;
}

/*********************************************************************
**    I_FUNCTION     : S_psds_nest (pte,pvec,pcon,dvec,dcon,da,db)
**       Project point to the intersection of two planes
**    PARAMETERS
**       INPUT  :
**             pte         - original point
**             pvec,pcon   - first plane
**             dvec,dcon   - second plane
**       OUTPUT :
**             pte         - projected point
**             da          - repositioning move alond first plane normal
**             db          - repositioning move alond second plane normal
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_psds_nest (pte,pvec,pcon,dvec,dcon,da,db)
UM_coord pte;
UM_vector pvec,dvec;
UU_REAL pcon,dcon,*da,*db;
{
	UU_REAL p,q,si,a,b,co2;
	int k;

	p = UM_DOT (pvec,pte) - pcon;
	q = UM_DOT (dvec,pte) - dcon;

	si = UM_DOT (pvec,dvec);
	co2 = 1 - si*si;
	if (co2 < UM_DFUZZ)
	{
		a = p/2;
		b = q/2;
	}
	else
	{
		a = (p-si*q)/co2;
		b = q - a*si;
	}

	for (k = 0; k < 3; k++)
	{
		pte[k] = pte[k] - a*pvec[k] - b*dvec[k];
	}

	*da = fabs(a); *db = fabs(b);

	return;
}

/*********************************************************************
**    I_FUNCTION     : S_reproj_pt (pte,sn,nvec,dpl,asw,isf,tol,dir,u,v)
**       Nest a point to the surface and the plane.
**    PARAMETERS
**       INPUT  :
**             pte         - original point
**             nvec,dpl    - plane data
**             asw         - surface "word"
**             isf         - surface number
**			   tol		   - tolerance
**             dir         - surface init flag
**             u,v         - surface parameters
**       OUTPUT :
**             pte         - projected point
**             dir         - surface init flag
**             u,v         - surface parameters
**             sn          - surface vector
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int S_reproj_pt (pte,sn,nvec,dpl,asw,isf,tol,dir,u,v)
UM_coord pte;
UM_vector nvec,sn;
UU_REAL dpl,tol;
UM_real8 asw;
UM_real4 *u,*v,*dir;
UM_int2 isf;
{
	static UM_real4 ss[9];
	int j,k,status;
	UM_coord ptj,tpt;
	UU_REAL dtol,pcon,a,b,d,d1;
	UM_int2 idx = 169;
	UM_real8 ver;
	UU_LOGICAL lv101;
	char tbuf[80];
	getsc(&idx, &ver);
	lv101 = (ver >= 10.050);
#define MAX_ITER 25
	
	status = UU_SUCCESS;
	dtol = 0.1*tol;

	um_vctovc (pte,ptj);

	for (j = 0; j < MAX_ITER; j++)
	{
		sfpt1 (&asw,ptj,&isf,dir,u,v,ss);
		for (k = 0; k < 3; k++)
		{
			sn[k] = ss[k];
		}
		pcon = ss[3];

		if (j == 0) S_psds_dist (pte,sn,pcon,nvec,dpl,&d);
		S_psds_nest (ptj,sn,pcon,nvec,dpl,&a,&b);

		if (a < dtol && b < dtol) goto done;
	}

	S_psds_dist (ptj,sn,pcon,nvec,dpl,&d1);

	if (d1 < d)
	{
		d = d1;
		goto done;
	}

	if (d > tol) return (UU_FAILURE);
done:
	if (lv101)
	{
/*
.....Make sure the uv parameters are in line with the new point - ASF 8/22/13.
*/
		tpt[0] = ss[4]; tpt[1] = ss[5]; tpt[2] = ss[6];
		if (!um_cceqcc_tol(ptj,tpt,tol))
		{
			d1 = um_dcccc(tpt,ptj);
//			sprintf(tbuf,"$$ DIST = %lf",d1);
//			NclxDbgPstr(tbuf);
			sfpt1 (&asw,ptj,&isf,dir,u,v,ss);
			ptj[0] = ss[4]; ptj[1] = ss[5]; ptj[2] = ss[6];
		}
	}
	um_vctovc (ptj,pte);
	return (status);
}

/*********************************************************************
**    I_FUNCTION     : Sfpl_reproj (sf,tfmat,pl,irot,rot,u0,v0,npts,pp,
**                                  ptlst,vclst)
**       Nest points to the surface and the plane, then weed.
**    PARAMETERS
**       INPUT  :
**          sfkey      - surface key
**          pl         - plane struct
**          irot       - transformation flag
**          rot        - transformation matrix (world to part)
**          u0,v0      - initial surface parameters
**          ptlst      - initialized (UM_coord) list to use
**          vclst      - initialized (UM_coord) list to use
**          npts       - number of points
**          pp         - points (in part coordinates)
**          tol        - tolerance
**       OUTPUT :
**          ptlst      - list of points (in world coordinates)
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int Sfpl_reproj (sfkey,nvec,dpl,plflg,plmx,plinv,u0,v0,npts,pp,tol,ptlst,
								vclst)
UU_KEY_ID sfkey;
UM_vector nvec;
UU_REAL dpl,tol;
UM_transf plmx,plinv;
UU_REAL u0,v0;
UU_LOGICAL plflg;
int npts;
UM_coord *pp;
UU_LIST *ptlst,*vclst;
{
	int i,status,n1;
	UU_LOGICAL ldovecs,lrev;
	UM_coord ppi,pp0,*pts;
	UM_vector vvi,vcur,sni,sn0;
	UU_REAL co;
	UM_real8 asw;
	UM_real4 dir;
	UM_real4 u,v;
	UM_int4 skey;
	UM_int2 isub,itype,isf;

	status = UU_SUCCESS;
	UU_LIST_EMPTY (ptlst);
	UU_LIST_EMPTY (vclst);

	itype = 9; /* surface */
	isub = 1;
	skey = sfkey;
	ptdsc3 (&skey,&isub,&itype,&asw);

	isf = 1;
	dir = 0;
	u = u0; v = v0;

	ldovecs = (npts > 2);
	lrev = UU_FALSE;

	for (i = 0; i < npts; i++)
	{
		if (plflg)
			um_cctmtf (pp[i],plinv,ppi);
		else
			um_vctovc (pp[i],ppi);

		status = S_reproj_pt (ppi,sni,nvec,dpl,asw,isf,tol,&dir,&u,&v);
		if (status != UU_SUCCESS) return (status);

		uu_list_push (ptlst,ppi);

		if (i > 0 && ldovecs)
		{
/*
..... use cross-product of plane normal (PS) and surface normal (DS) as
..... the tangent vector
*/
			if (i == 1)
			{
/*
..... make sure the tangent vector is along the curve by comparing it to
..... the first delta-vector
*/
				um_cross (nvec,sn0,vvi);
				um_vcmnvc (ppi,pp0,vcur);
				co = UM_DOT (vvi,vcur);
				if (co < 0)
				{
					lrev = UU_TRUE;
					um_negvc (vvi,vvi);
				}
				uu_list_push (vclst,vvi);
			}
			if (lrev)
			um_cross (sni,nvec,vvi);
			else
			um_cross (nvec,sni,vvi);

			uu_list_push (vclst,vvi);
		}

		if (i == 0)
		{
			um_vctovc (ppi,pp0);
			um_vctovc (sni,sn0);
		}
	}

	if (ldovecs)
	{
		n1 = npts;
		ncl_fix_tol (&n1,tol,ptlst,vclst);
	}

	if (plflg)
	{
		n1 = ptlst->cur_cnt;
		pts = (UM_coord *) UU_LIST_ARRAY (ptlst);
		for (i = 0; i < n1; i++)
			um_cctmtf (pts[i],plmx,pts[i]);
	}

	return (status);
}

/*********************************************************************
**    I_FUNCTION     : Sfplio_err (sf,str77,GUI,ier,errno)
**       Output error for a given surface.
**    PARAMETERS
**       INPUT  :
**          sf      - surface data
**          errno   - error number
**          GUI      - GUI-type error output, iff 1
**       OUTPUT :
**          ier     - error number
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void Sfplio_err (sf,str77,GUI,ier,errno)
struct NCL_fixed_databag *sf;
UM_f77_str_ptr str77;
UM_int2 GUI,*ier;
int errno;
{
	char errout[80];

	*ier = errno;
	ncl_get_label (sf, buf);
	if (GUI == 1)
	{
		if (errno == 502)
			sprintf(errout,"Could not intersect surface %s.",buf);
		else
			sprintf(errout,"Could not process surface %s.",buf);
		ud_wrerr(errout);
	}
	else
	{
		uerror2(str77);
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_sfplio (sfnum,sfkey,pl,irpt,ptx,tol,lb0,ilab,GUI,ier)
**       For each surface on the list, create intersection(s) with a plane.
**    PARAMETERS
**       INPUT  :
**          sfkey    - surface keys
**          sfnum    - number of surface keys
**          pl       - plane
**          irpt     - 0 - create all individual intersections;
**                     1,2,3 - create a single spline:
**                             1 - closest to the sf center
**                             2 - closest to initial u,v provided
**                             3 - closest to the near point
**          ptx      - near point or initial surface U,V when irpt = 2 or 3
**          tolu     - Unibase tolerance (inches)
**          lb0      - label constant prefix
**          ilab     - label subscript (numerical part of labels)
**          GUI      - GUI-type error output, iff 1
**       OUTPUT :
**          ier     - error number, if fail
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int ncl_sfplio (sfnum,sfkey,plvec,pl,rot,irpt,ptx,tolu,lb0,ilab,GUI,ier)
UU_KEY_ID *sfkey;
int sfnum,irpt;
UM_real8 *ptx;
UM_vector plvec;
struct NCL_nclpl_rec *pl;
UM_transf rot;
UU_REAL tolu;
char *lb0;
UM_int2 ilab,GUI,*ier;
{
#define SI2 (UU_REAL) 3.046171e-6

	int isf,i,ib,k,nc,npts,ntri,status;
	UU_REAL tol1,tolu1,dpl;
	UU_KEY_ID keyi,*ky;
	UU_LOGICAL lsolid;
	struct NCL_fixed_databag sf;
	UM_transf tfmat,plmx,plinv;

	UM_coord *pts;
	UU_REAL d,umin,umax,vmin,vmax,u,v;
	UM_int2 primtyp;
	UM_real8 primdata[16];

	UM_srf_boundary bndr;
	UM_vector vec,vcross;
	int npo,*npi,*np;
	int istat,trilst_init,itsk,irot;
	UU_LOGICAL plflg;
	struct UM_evsrfout evsrf;

	UM_tessellation tess;
	UU_LIST cvlst,uvlst,trilst,delkys,dlist;
	UM_trian *ptri;

	nclsf_prim_type typ;

	UU_REAL zmax,zmin;
	UU_REAL xmmx[2],ymmx[2];

	UM_f77_str_ptr str77;

	UM_init_f77_str (str77, buf, 80);
	buf[0] = '\0';

	status = UU_SUCCESS;

	tol1 = 0.1*tol;
	tolu1 = (tolu > 0.0005)? 2.*tolu: 0.001;

	ncl_get_rotfl (&irot);
	plflg = ncl_newzaxis_rot (plvec,plmx,plinv);

	dpl = UM_DOT (pl->pt,pl->nvec);

	ncl_set_boundary_toler (tolu);
	um_set_tess_toler (tolu);
	um_init_boundary (&bndr);
	uu_list_init (&cvlst, sizeof(UM_coord), 100, 200);
	uu_list_init (&uvlst, sizeof(UM_coord), 100, 200);
	bndr.uvpts = &uvlst;
	bndr.cvpts = &cvlst;
	um_init_tess (&tess);
	uu_list_init0 (&trilst); uu_list_init0(&dlist);
	trilst_init = 0;
	uu_list_init (&delkys, sizeof(UU_KEY_ID),0,10);

	for (isf = 0; isf < sfnum; isf++)
	{
		ncl_nul_uv();
		ncl_nul_nios();

		sf.key = sfkey[isf];
		status = ncl_retrieve_data_fixed (&sf);
		if (status == UU_SUCCESS)
		{
			typ = NCLSF_UNKNOWN;
			lsolid = (sf.rel_num == UM_SOLID_REL);
			status = uc_retrieve_transf (sf.key, tfmat);
		}
		if (status == UU_SUCCESS)
		{
			if (lsolid) goto Tess;
			ncl_free_bndry (&bndr);
			UU_LIST_EMPTY (bndr.uvpts); UU_LIST_EMPTY (bndr.cvpts);
			status = ncl_get_bndry (&sf,tfmat,&bndr,tolu,UU_TRUE);
			if (bndr.nb < 1) status = UU_FAILURE;
		}

		if (status != UU_SUCCESS)
		{
			Sfplio_err (&sf,str77,GUI,ier,163);
			goto Done;
		}

		if (irot > 0)
		{
			pts = (UM_coord *) UU_LIST_ARRAY (bndr.cvpts);
			for (ib = 0; ib < bndr.nb; ib++)
			{
				npts = bndr.np[ib];
				for (i = 0; i < npts; i++)
					um_cctmtf(pts[i],rot,pts[i]);
				pts += npts;
			}
		}

		istat = ncl_get_sf_primdat(&sf.key,&primtyp,primdata);
		if (istat == UU_SUCCESS && (primtyp <= 1 || primtyp > 7))
		ncl_wat_check_prim (&sf,tfmat,&primtyp,primdata,tolu1);
		if (istat == UU_SUCCESS) typ = (nclsf_prim_type) primtyp;

Tess:
		if (typ == NCLSF_PLANE)
		{
			if (irot == 1)
				ncl_transform_primdat (&typ,primdata,rot);

			for (k = 0; k < 3; k++) vec[k] = primdata[k];
			um_cross (vec,pl->nvec,vcross);

			d = UM_DOT (vcross,vcross);
			if (d < SI2)
				istat = -1;
			else
			{
				um_unitvc (vcross,vcross);
				ncl_set_ldir (vcross);
				npo = bndr.np[0];
				npi = (bndr.nb > 1)? (bndr.np + 1): UU_NULL;
				istat =
				ncl_bndrpln_io (bndr.nb,npo,npi,bndr.cvpts,pl->nvec,dpl,tol1);
			}

			rmax = 0;

			if (istat != 0 && irpt > 0)
			{
				Sfplio_err (&sf,str77,GUI,ier,502);
				goto Done;
			}
		}
		else
		{
			if (lsolid)
			{
				status = ncl_get_tesslst (&sf,&tess);
				if (status != UU_SUCCESS)
				status = ncl_get_solid_tess (&sf,&dlist,&tess,tolu);
			}
			else
			{
				status =
				ncl_tess_surf (&sf,tfmat,&bndr,&tess,0.8*tolu,UM_TESS_WATRLN,0,0);
			}

			if (status == UU_SUCCESS)
			{
				if (trilst_init == 0)
				{
					uu_list_init (&trilst,sizeof (UM_trian),200,200);
					trilst_init = 1;
				}
				else
					UU_LIST_EMPTY (&trilst);

				status = ncl_get_tess_triangles (&tess,&trilst,2,1);

				ntri = trilst.cur_cnt;
				if (ntri < 1) status = UU_FAILURE;
			}
			if (status != UU_SUCCESS)
			{
				Sfplio_err (&sf,str77,GUI,ier,163);
				goto Done;
			}
			um_clean_tess (&tess);

			ptri = (UM_trian *)UU_LIST_ARRAY(&trilst);
			zmax = -1000000.; zmin = 1000000.;
			xmmx[0] = ymmx[0] = 1000000.;
			xmmx[1] = ymmx[1] = -1000000.;

			for (i = 0; i < ntri; i++)
			{
				if (irot > 0) um_cctmtf(ptri[i].p1,rot,ptri[i].p1);
				ncl_waterline_minmax(ptri[i].p1,&zmin,&zmax,xmmx,ymmx);
				if (irot > 0) um_cctmtf(ptri[i].p2,rot,ptri[i].p2);
				ncl_waterline_minmax(ptri[i].p2,&zmin,&zmax,xmmx,ymmx);
				if (irot > 0) um_cctmtf(ptri[i].p3,rot,ptri[i].p3);
				ncl_waterline_minmax(ptri[i].p3,&zmin,&zmax,xmmx,ymmx);
			}

			rmax = MAX2((xmmx[1]-xmmx[0]),(ymmx[1]-ymmx[0]));
			rmax = 4*rmax*rmax;

			if (dpl > zmin - tol && dpl < zmax + tol)
			{
				for (i = 0; i < ntri; i++)
					ncl_tripln_io(&ptri[i],pl,tol1,tolsq);

				ncl_arrange_segs (tol);
				if (lsolid)
				{
					UU_LIST_EMPTY (&cvlst);
					ncl_separate_components (&cvlst,&uvlst,tol);
					npts = cvlst.cur_cnt;
					if (npts < 4) continue;
					pts = (UM_coord *) UU_LIST_ARRAY (&cvlst);
					nc = 1;
					ncl_get_nios (&nc,&np);
					goto Build;
				}
			}
		}

		ncl_getpts_uv (&npts,&pts);
		if (npts <= 1)
		{
			if (irpt > 0)
			{
				Sfplio_err (&sf,str77,GUI,ier,502);
				goto Done;
			}
			else
				continue;
		}

		nc = 1;
		ncl_get_nios (&nc,&np);

		umin = bndr.ummx[0][0];	umax = bndr.ummx[0][1];
		vmin = bndr.vmmx[0][0];	vmax = bndr.vmmx[0][1];

		if (irpt == 2)
		{
			u = ptx[0]; v = ptx[1];
			if (u < umin || u > umax) u = (umin + umax)/2;
			if (v < vmin || v > vmax) v = (vmin + vmax)/2;
		}
		else
		{
			u = (umin + umax)/2; v = (vmin + vmax)/2;
		}

		if (nc > 1 && irpt > 0)
		{
			if (irpt == 1)
			{
				if (plflg) um_cctmtf (ptx,plmx,ptx);
				itsk = 0;
			}
			else if (irpt == 2)
			{
				status = uc_evsrf (UM_POINT,u,v,&sf,tfmat,&evsrf);
				if (status != UU_SUCCESS)
				{
					Sfplio_err (&sf,str77,GUI,ier,163);
					goto Done;
				}
				if (irot > 0)
					um_cctmtf (evsrf.sp,rot,ptx);
				else
					um_vctovc (evsrf.sp,ptx);

				itsk = 0;
			}
			else if (typ == NCLSF_PLANE)
				itsk = 2;
			else
				itsk = 1;

			ncl_select_component1 (itsk,ptx,&npts,tol,UU_FALSE);
			nc = 1;
			ncl_getpts_uv (&npts,&pts);
		}

Build:
		itsk = (irpt > 0)? 1: 0;

		for (i = 0; i < nc; i++)
		{
			if (nc > 1) npts = np[i];

			keyi = NULLKEY;
			if (lsolid)
			{
				keyi = ncl_sfs_create_geo1 (npts,pts,UU_FALSE,itsk,ier);
			}
			else
			{
				status = Sfpl_reproj (sf.key,plvec,dpl,plflg,plmx,plinv,u,v,npts,
					pts,tol,&cvlst,&uvlst);
				if (status == UU_SUCCESS)
				{
					keyi = ncl_sfs_create_cvio (&cvlst,itsk,ier);
				}
			}
			if (keyi == NULLKEY)
			{
				Sfplio_err (&sf,str77,GUI,ier,163);
				goto Done;
			}
			else
			{
				uu_list_push (&delkys,&keyi);
				ncl_sfs_update_saveid (lb0,&ilab,&isubscr);
			}

			pts += npts;
		}
	}

Done:
	if (status != UU_SUCCESS || *ier > 0)
	{
		nc = delkys.cur_cnt;
		if (nc > 0)
		{
			ky = (UU_KEY_ID *) UU_LIST_ARRAY (&delkys);
			for (i = 0; i < nc; i++)
			{
				if (ky[i] != NULLKEY) uc_delete(ky[i]);
			}
		}
	}
	uu_list_free (&delkys);
	um_free_tess (&tess);

	if (trilst_init == 1) uu_list_free (&trilst);
	ncl_free_bndry (&bndr);
	uu_list_free (&uvlst);
	uu_list_free (&cvlst);

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_sfsio (sff,plvec,pl,tol,mxtol2,conpoc,lb0,ilab,GUI,ier)
**       Create curves at the intersection of preprocessed surface data
**       (boundaries and tessellations) with a plane.
**
**    PARAMETERS
**       INPUT  :
**          sfi      - waterline-type surface data
**          pl       - plane
**          rot      - transformation matrix (from Unibase to part coordsys)
**          conpoc   - 0 - create all individual intersections;
**                     1 - create closed (composite) curves;
**          tol      - Unibase tolerance (inches)
**          mxtol2   - tolerance-based parameter used when connecting pieces
**          lb0      - label constant prefix
**          ilab     - label subscript (numerical part of labels)
**          GUI      - GUI-type error output, iff 1
**       OUTPUT :
**          ier     - error number, if fail
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : For single intersections this routine is OBSOLETE;
**                   starting with 9.6 the new code is ncl_sfplio
*********************************************************************/
static int ncl_sfsio (sff,plvec,pl,rot,tol,mxtol2,conpoc,lb0,ilab,GUI,ier)
NCL_waterline_surf *sff;
UM_transf rot;
int conpoc;
UM_vector plvec;
struct NCL_nclpl_rec *pl;
UU_REAL tol,mxtol2;
char *lb0;
UM_int2 ilab,GUI,*ier;
{
	int isf,i,nc,npts,status;
	NCL_waterline_surf *sfi;
	UU_REAL zlev,tol2d;
	int *np;
	UM_coord *ptio;
	UU_LIST delkys;
	UU_KEY_ID keyi,*ky;
	char errout[80];
	UM_f77_str_ptr str77;
	UU_LOGICAL lv97;

	UM_init_f77_str (str77, buf, 80);
	buf[0] = '\0';
	status = UU_SUCCESS;
	tol2d = tol;

	if (conpoc == 0)
	{
		i = (sfnum > 10)? sfnum: 10;
		uu_list_init (&delkys, sizeof(UU_KEY_ID),i,i);
		if (delkys.data == UU_NULL) return (UU_FAILURE);
	}

	lv97 = ncl_setver(97);

	zlev = pl->pt[2];
	sfi = sff;
	ptsio.cur_cnt = nio.cur_cnt = 0;
	for (isf = 0; isf < sfnum; isf++,sfi++)
	{
		sfi->ncvs = 0; sfi->nlist0 = sfi->plist0 = -1;
		if (sfi->key == NULLKEY || sfi->sf_flag == HORZPL) continue;
		if (sfi->zmin > zlev + tol || sfi->zmax < zlev + 0.1*tol) continue;

		sfi->nlist0 = nio.cur_cnt; sfi->plist0 = ptsio.cur_cnt;
		if (lv97)	
			nc = ncl_sf_pln_io (sfi,pl,&nio,&ptsio,zlev,tol2d,tol,1);				
		else			
			nc = ncl_sf_pln_io2 (sfi,pl,plvec,rot,&nio,&ptsio,
								zlev,tol2d,tol,0);
		if (nc < 0)
		{
			ncl_get_label_with_key(sfi->key, buf);
			if (GUI == 1)
			{
				sprintf(errout,"Could not intersect surface %s.",buf);
				ud_wrerr(errout);
			}
			else
			{
				*ier = 502;
				uerror2(str77);
			}
			status = UU_FAILURE;
			goto Done;
		}
		if (nc == 0) continue;
		if (conpoc == 0)
		{
			np = (int *) UU_LIST_ARRAY (&nio);
			ptio = (UM_coord *) UU_LIST_ARRAY (&ptsio);
			ptio += sfi->plist0;
			for (i = 0; i < nc; i++)
			{
				npts = np[sfi->nlist0 + i];
				keyi = ncl_sfs_create_geo(npts,ptio,UU_FALSE,ier);

				if (keyi == NULLKEY)
				{
					status = UU_FAILURE;
					goto Done;
				}
				else
				{
					uu_list_push (&delkys,&keyi);
					ncl_sfs_update_saveid (lb0,&ilab,&isubscr);
				}

				ptio += npts;
			}
		}
	}

Done:
	if (conpoc == 1 && status == UU_SUCCESS)
		status = ncl_sfs_contours (sff,zlev,mxtol2,lb0,ilab,GUI,ier);

	if (conpoc == 0)
	{
		if (status != UU_SUCCESS || *ier > 0)
		{
			nc = delkys.cur_cnt;
			if (nc > 0)
			{
				ky = (UU_KEY_ID *) UU_LIST_ARRAY (&delkys);
				for (i = 0; i < nc; i++)
				{
					if (ky[i] != NULLKEY) uc_delete(ky[i]);
				}
			}
		}
		uu_list_free (&delkys);
	}

	return (status);
}

/*********************************************************************
**    I_FUNCTION     : ncl_get_zlev (key4,d8,mod2,nvec,pl)
**       Get the plane data into a plane struct.
**    PARAMETERS
**       INPUT  :
**          pl       - plane struct to use
**          key4     - key of the plane (or planar surface)
**          d8       - plane offset (in world coordsys)
**          mod2     - plane offset direction modifier
**       OUTPUT :
**          nvec     - plane normal in world coordsys
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int ncl_get_zlev (key4,d8,nvec8,mod2,nvec,pl)
UM_int4 *key4;
UM_real8 *d8, *nvec8;
UM_int2 *mod2;
UM_vector nvec;
struct NCL_nclpl_rec *pl;
{
	struct NCL_fixed_databag e;
	struct NCL_nclpl_rec *pln;
	UM_real8 primdata[16];
	UM_int2 typ;
	UU_REAL zlev,dsec,co;
	UM_vector dir;
	int i,j;
	int status = UU_SUCCESS;

	if (*key4 > 0)
	{
		e.key = *key4;
		status = ncl_retrieve_data_fixed (&e);
		if (status != UU_SUCCESS) return (status);

		if (e.rel_num == NCL_PLN_REL)
		{
			pln = (struct NCL_nclpl_rec *)&e;
			for (i=0;i<3;i++)
			{
				nvec[i] = pln->nvec[i];
				primdata[4+i] = pln->pt[i];
			}
		}
		else
		{
			status = ncl_get_sf_primdat(&e.key,&typ,primdata);
			if ((nclsf_prim_type) typ != NCLSF_PLANE) status = UU_FAILURE;
			if (status != UU_SUCCESS) return (status);
			for (i=0;i<3;i++) nvec[i] = primdata[i];
		}
		fr_unbs(nvec,nvec,&IVE);
		fr_unbs(&primdata[4],&primdata[4],&IPT);
		zlev = primdata[4]*nvec[0] + primdata[5]*nvec[1] + primdata[6]*nvec[2];
		j = *mod2;
		dsec = *d8;
		if (j >= 638 && j <= 643 && dsec != 0)
		{
			um_nullvc (dir);
			if (j == 638) dir[0] = 1;
			else if (j == 641) dir[0] = -1;
			else if (j == 639) dir[1] = 1;
			else if (j == 642) dir[1] = -1;
			else if (j == 640) dir[2] = 1;
			else if (j == 643) dir[2] = -1;
			co = UM_DOT(dir,nvec);
			if (co < -0.0001)
				zlev -= dsec;
			else if (co > 0.0001)
				zlev += dsec;
		}
	}
	else
	{
		nvec[0] = nvec8[0];
		nvec[1] = nvec8[1]; 
		nvec[2] = nvec8[2];
		zlev = *d8;
	}

	pl->key = NULLKEY;
	pl->pt[0] = pl->pt[1] = 0.; pl->pt[2] = zlev;
	pl->nvec[0] = 0.; pl->nvec[1] = 0.; pl->nvec[2] = 1.;
	pl->rel_num = NCL_PLN_REL;

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : nsfsio (key4,d8,nvec8,ptx,mmod,tol8,itsk,llab,subscr,ier)
**       Create curves at the intersection of a list of surfaces with a plane
**    PARAMETERS
**       INPUT  :
**          key4     - key of the plane (or planar surface)
**          itsk     - 0 - create all individual intersections;
**                     1 - create closed (composite) curves;
**                     2 - create a single spline;
**          d8       - plane offset
**          mmod     - plane offset direction modifier when itsk = 0,1;
**                     near point type when itsk = 2
**          ptx      - near point or initial surface U,V when itsk = 2,
**                     else ignored
**          tol8     - Unibase tolerance (inches)
**          llab     - label prefix (constant part of labels)
**          subscr   - label subscript (numerical part of labels)
**       OUTPUT :
**          ier     - error number, if fail
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nsfsio (key4,d8,nvec8,ptx,mmod,tol8,itsk,llab,subscr,ier)
UM_int4 *key4,*subscr;
UM_real8 *tol8,*d8,*ptx,*nvec8;
UM_int2 *mmod,*itsk,*ier;
UM_f77_str_ptr llab;
{
	UU_KEY_ID *sfkey;
	int isf,status,conpoc,irpt;
	UU_LOGICAL lsfplio;
	UU_REAL zmax,zmin,toler,mxtol2,zlev;
	UU_REAL xmmx[2],ymmx[2];

	UM_int2 ilab,isub,GUI;
	char *lb0;

	int mcsflg = 0,NPT;
	struct NCL_nclpl_rec pl;
	UM_transf rot;
	UM_vector nvec;
	UU_LOGICAL lv97;

	NCL_waterline_surf *sff = UU_NULL;

	*ier = 358;

	ncl_getnum_listkey (0,&sfnum);
	if (sfnum < 1)
	{
		ncl_free_keylist(0);
		goto Err;
	}
	ncl_get_listkeys (0,&sfkey);

	conpoc = *itsk;
	*ier = 0;
	status = UU_SUCCESS;
	isubscr = *subscr;
	isub = 296; getifl(&isub,&ilab);
	if (ilab > 0)
		lb0 = UM_cstr_of_f77_str(llab);

	toler = *tol8;

	isub = 343; getifl(&isub,&GUI);

	isub = 264;	getifl(&isub,&mm);
	tol = (mm)? toler*25.4: toler;
	tolsq = tol*tol;

	lsfplio = (conpoc == 0 || conpoc == 2) && !ncl_setver(95);

/*
..... get the plane normal for the vertical direction, and the plane level
*/
	status = ncl_get_zlev (key4,d8,nvec8,mmod,nvec,&pl);

	if (status != UU_SUCCESS) goto Err;

	ncl_define_rotmx (mm,nvec,rot,&mcsflg);

	if (lsfplio)
	{
		irpt = 0;
		if (conpoc == 2) irpt = *mmod;

		lv97 = ncl_setver(97);

		if (lv97)
		{
			status = ncl_sfplio (sfnum,sfkey,nvec,&pl,rot,
				irpt,ptx,toler,lb0,ilab,GUI,ier);
		}
		else
		{
			status = um_cvio_sfplio(sfnum,sfkey,nvec,&pl,rot,
				irpt,ptx,toler,lb0,ilab,GUI,ier);
		}
	}
	else
	{
		mxtol2 = 1024*tolsq;

		zlev = pl.pt[2];
		sff = (NCL_waterline_surf *) uu_malloc (sfnum * sizeof (*sff));
		if (!sff) goto Err;

		NPT = 1;
		status = ncl_process_netsf (0,sfnum,sfkey,rot,&zmax,&zmin,sff,tol,toler,
			&NPT,tolsq,xmmx,ymmx,buf);
		if (status != UU_SUCCESS ||
			zlev > zmax + tol || zlev < zmin - tol) goto Err;

		rmax = MAX2((xmmx[1]-xmmx[0]),(ymmx[1]-ymmx[0]));
		rmax = 4*rmax*rmax;

		status = ncl_sfsio (sff,nvec,&pl,rot,tol,mxtol2,conpoc,lb0,ilab,GUI,ier);
	}

	if (status == UU_SUCCESS) goto Done;

Err:
	if (*ier == 0) *ier = 163;

Done:
	if (Sactive)
		Sfdisp0();
	else
		ncl_free_keylist(0);
	ncl_free_uv();
	if (sff != UU_NULL)
	{
		NCL_waterline_surf *p1;
		p1 = sff;
		for (isf = 0; isf < sfnum && p1->key != NULLKEY; isf++, p1++)
		{
			ncl_free_bound (&p1->bound);
			UU_LIST_FREE (p1->trianlist);
		}
		UU_FREE (sff);
	}
}

/*
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
stuff below is not active in 9.5
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
*/

/*********************************************************************
**    E_FUNCTION     : nsfsi1 (tol8,ier)
*********************************************************************/
void nsfsi1 (tol8,ier)
UM_real8 *tol8;
UM_int2 *ier;
{
/*
	int i,npts;
	UU_REAL toler,zlev;

	toler = *tol8;
	zlev = 0;

	npts = NCL_uvintof.cur_cnt/6;

	for (i = 0; i < npts; i++)
		uu_list_delete (&NCL_uvintof,3*i,3);


	ncl_sf_pln_io (UU_NULL,UU_NULL,&nio,&ptsio,zlev,toler,1);

	NCL_uvintof.cur_cnt = 0;
	if (nios_init == 1) nios.cur_cnt = 0;
*/
	return;
}

/*********************************************************************
**    E_FUNCTION     : sfscre (tol8,llab,subscr,ier)
*********************************************************************/
void sfscre (tol8,llab,subscr,ier)
UM_real8 *tol8;
UM_int2 *subscr,*ier;
UM_f77_str_ptr llab;
{
	return;
}

/*********************************************************************
**    E_FUNCTION     : nsfsi2 (tol8,llab,subscr,ier)
*********************************************************************/
void nsfsi2 (tol8,llab,subscr,ier)
UM_real8 *tol8;
UM_int2 *subscr,*ier;
UM_f77_str_ptr llab;
{
	return;
}

/*********************************************************************
**    E_FUNCTION     : void offwf1 (keyin, dist)
**       Offset a (not NCL) surface dispatching routine.
**    PARAMETERS
**       INPUT  :
**          keyin      - key of entity
**          dist       - offset distance
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void offwf1 (keyin, dist)
UM_int4 *keyin;
UM_real8 *dist;
{
/*
	struct NCL_fixed_databag sf1;
	float cdis;

	cdis = *dist;
	sf1.key = *keyin;
	if (ncl_retrieve_data_fixed (&sf1) == 0)
	{
		if (sf1.rel_num == UM_RBSPLSRF_REL)
			ncl_offset_rbsf(&sf1, cdis);
		else if (sf1.rel_num == NCL_TRIMSF_REL)
			ncl_offset_trimsf(&sf1, cdis);
		else if (sf1.rel_num == NCL_EVALSF_REL)
			ncl_offset_evalsf(&sf1, cdis);
		else if (sf1.rel_num == NCL_REVSURF_REL)
			ncl_offset_revsf(&sf1, cdis);
	}
*/
	return;
}

/*********************************************************************
**    E_FUNCTION     : int ncl_create_splcv (npts,pts,nolab,itsk,ier)
**       Create a spline curve by a list of points
**    PARAMETERS
**       INPUT  :
**          npts       - number of points in list
**          pts        - points in list
**          itsk       - try to create a line or an arc if 0;
**                       splines only if 1
**       OUTPUT :
**          ier        - error number
**    RETURNS      : UU_SUCESS if succed, or UU_FAILURE if fail
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_create_splcv (npts,itsk,tol,ier)
UM_int2 *npts,*itsk;
UM_real8 *tol;
UM_int2 *ier;
{
	int status;
	int i,j,j1,n,npt;
	UM_coord *pt;
	UM_coord *ptio;
		
	int chunks;
	int *np;

	UU_LIST delkys;
	UU_KEY_ID keyi;
	
	uu_list_init (&delkys, sizeof(UU_KEY_ID),0,10);
	keyi = NULLKEY;

	status = UU_SUCCESS;
		
	uu_list_init (&seglist,sizeof(struct NCL_crvgen_rec),0,100);

	chunks = 1;
	ncl_get_nios (&chunks,&np);

	ncl_getpts_uv (&npt,&pt);

	if (chunks <= 1)
	{
		if (npt > 1)	
			keyi = ncl_sfs_create_geo(npt,pt,UU_FALSE,ier);
		if (keyi == NULLKEY)
		{
			status = UU_FAILURE;
			goto Done;
		}
		uu_list_push (&delkys,&keyi);
	}
	else
	{
		j1 = 0;
		for (i = 0; i < chunks; i++)
		{
			n = np[i];

			ptio = (UM_coord *) uu_malloc (n*sizeof(UM_coord));
			for (j = 0; j < n; j++) um_vctovc (pt[j1+j],ptio[j]);

			if (n > 1)		
				keyi = ncl_sfs_create_geo(n,ptio,UU_FALSE,ier);

			if (keyi == NULLKEY)
			{
				status = UU_FAILURE;
				goto Done;
			}
			uu_list_push (&delkys,&keyi);

			j1 +=n;

			if (ptio)
				uu_free(ptio);
		}
	}

Done:
	uu_list_free (&seglist);
	uu_list_free (&delkys);

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : int um_cvio_create_splcv (itsk,tol,ier)
**       Create a spline curve by the points in the active Scvio list
**    PARAMETERS
**       INPUT  :
**          itsk     - try to create a line or an arc if 0;
**                     splines only if 1
*			tol		 - tolerance
**       OUTPUT :
**          ier        - error number
**    RETURNS      : UU_SUCESS if succed, or UU_FAILURE if fail
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_cvio_create_splcv (itsk,tol,rmax0,lb0,ilab,ier)
UM_int2 itsk,*ilab;
UM_real8 tol,rmax0;
char *lb0;
UM_int2 *ier;
{
	int status;
	int i,npts,nutcv,ncv,created;
	UM_coord *pts;

	UU_LIST delkys;
	UU_KEY_ID keyi;
	
	uu_list_init (&delkys, sizeof(UU_KEY_ID),0,10);
	keyi = NULLKEY;

	rmax = rmax0;
	status = UU_SUCCESS;
	created = 0;
	uu_list_init (&seglist,sizeof(struct NCL_crvgen_rec),0,100);

/*
.....Get the number of untrimmed curve and total curves
*/
	nutcv = um_cvio_getnutcv();
	ncv = um_cvio_getncv();
	if (ncv <= 0) goto Done;

/*
.....Create spline of untimmed curve
*/
 	if (ncv-nutcv == 0)
	{
		for (i = 0; i < nutcv; i++)
		{
			um_cvio_getpts(i,&npts,&pts);
			if (npts < 2) 
				continue;

			keyi = ncl_sfs_create_geo1(npts,pts,UU_FALSE,itsk,ier);
			if (keyi == NULLKEY)
			{
				status = UU_FAILURE;
				goto Done;
			}
			created++;
			uu_list_push (&delkys,&keyi);					
			ncl_sfs_update_saveid (lb0,ilab,&isubscr);
		}
	}
	else
	{
/*
.....Create spline of individual timmed curve
*/
		for (i = nutcv; i < ncv; i++)
		{
			um_cvio_getpts(i,&npts,&pts);
			if (npts < 2)
				continue;

			keyi = ncl_sfs_create_geo1(npts,pts,UU_FALSE,itsk,ier);

			if (keyi == NULLKEY)
				continue;

			created++;
			uu_list_push (&delkys,&keyi);						
			ncl_sfs_update_saveid (lb0,ilab,&isubscr);
		}
	}

Done:
	if (created == 0)
	{
		status = UU_FAILURE;
		if (*ier == 0) *ier = 502;
	}
	uu_list_free (&seglist);
	uu_list_free (&delkys);

	return (status);
}

