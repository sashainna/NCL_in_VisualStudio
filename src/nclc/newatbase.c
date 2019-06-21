/*********************************************************************
**    NAME         :  newatbase.c
**       CONTAINS:  Routines for calculating waterline geometry
**
**        ncl_wbase_init
**        ncl_wpar_init
**        ncl_init_wbase_lists
**        ncl_free_wbase_lists
**        ncl_wat_setpln
**        ncl_offset_wat_botsf
**        ncl_create_wat_botsf
**        ncl_copy_wat_botsf
**        ncl_create_wat_botpl
**        ncl_get_pln_level
**        ncl_get_gap_data
**        ncl_wat_levels
**        ncl_wat_sortpt
**        ncl_wat_baseproj
**        ncl_wat_set_trad
**        ncl_refine_trad
**        ncl_create_2box1
**        ncl_offset_out1
**        ncl_wat_set_pokgeo
**        ncl_waterline_create_geo
**        ncl_waterline_create_geo1
**        print_pol1
**
**    COPYRIGHT 2007 (c) Numerical Control Computer Sciences Inc.
**                          All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       newatbase.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:58
*********************************************************************/
#include "mdattr.h"
#include "go.h"
#include "gerrorst.h"
#include "uminmax.h"
#include "mattr.h"
#include "nclwaterln.h"
#include "nclclip.h"

#define STOCK 0
#define PLANE 1
#define DIST 2
#define DEPTH 2
#define ZLEV 3

#define NEGX 1
#define POSX 2
#define NEGY 3
#define POSY 4
#define NEGZ 5
#define POSZ 6
#define NEARPT 7

static UU_LIST ptlst,snlst,vclst,nplst;
static UM_srf_boundary bound;
static struct UM_evsrfout Sevsrf;

static struct UM_line_rec Sln;
static UU_REAL BPLM[4] = {0,1,0,1};

static UM_int2 IPT = NCLI_POINT;
static UM_int2 IVE = NCLI_VECTOR;

static UM_int2 ISF = 1;
static UM_real4 DIR = 1;

/*********************************************************************
**    E_FUNCTION     :  ncl_wbase_init (wbase)
**       Initialize the struct that holds the base surface data
*********************************************************************/
void ncl_wbase_init (wbase)
NCL_w_base *wbase;
{
	wbase->key0 = NULLKEY;
	wbase->srf = UU_NULL;
	um_identtf (wbase->tfmat);
	wbase->lrev = UU_FALSE;
	wbase->asw = 0;
	wbase->u = wbase->v = 0.5;
}

/*********************************************************************
**    E_FUNCTION     :  ncl_wpar_init (wpar)
**       Initialize the struct that holds the waterline roughing parameters
*********************************************************************/
void ncl_wpar_init (wpar)
NCL_w_param *wpar;
{
	wpar->conpoc = wpar->poctype = wpar->frmtype = 0;
	wpar->havestk = wpar->finish = UU_FALSE;
	wpar->z0 = wpar->z1 = wpar->dz = 0;
	wpar->rsq = 0;
	wpar->sortpt[0] = wpar->sortpt[1] = 0;
}

/*********************************************************************
**    E_FUNCTION     :  ncl_init_wbase_lists()
**       Initialize the local static lists.
*********************************************************************/
int ncl_init_wbase_lists()
{
	int status;

	uu_list_init (&ptlst, sizeof(UM_coord), 100, 100);
	uu_list_init (&snlst, sizeof(UM_vector), 100, 100);
	uu_list_init (&vclst, sizeof(UM_vector), 100, 100);
	uu_list_init (&nplst, sizeof(int), 10, 10);

	um_init_boundary (&bound);

	Sln.rel_num = UM_LINE_REL;
	strncpy (Sln.label,"@UN    ",7);
	Sln.no_displst = 0; Sln.displst = UU_NULL;
	um_nullvc (Sln.spt);
	um_nullvc (Sln.ept);
	status = ur_create_data(&Sln);

	if (status == UU_SUCCESS && Sln.key == NULLKEY)
		status = (UU_FAILURE);

	return (status);
}

/*********************************************************************
**    E_FUNCTION     :  ncl_free_wbase_lists()
**       Free the local static lists.
*********************************************************************/
void ncl_free_wbase_lists()
{
	uu_list_free (&ptlst);
	uu_list_free (&snlst);
	uu_list_free (&vclst);
	uu_list_free (&nplst);

	uc_delete (Sln.key);
}

/*********************************************************************
**    E_FUNCTION     :  ncl_wat_setpln (pl)
**       Get a plane level re given vertical direction
**    PARAMETERS
**       INPUT  :
**          key    - the Unibase key
**          vert   - the vertical direction
**          GUI    - flag for error output
**          buf    - buffer for error output
**       OUTPUT :
**          dlev   - the level
**          ier    - 0 if no error; error number else
**    RETURNS      : UU_SUCCESS / UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_wat_setpln (pl)
struct NCL_nclpl_rec *pl;
{
	int status = UU_SUCCESS;
	UU_REAL z;

	ncl_get_wlev (&z);
	pl->pt[0] = pl->pt[1] = 0.; pl->pt[2] = z;
	pl->nvec[0] = 0.; pl->nvec[1] = 0.; pl->nvec[2] = 1.;

	if (!ncl_is_watrev())
		status = ur_update_data_fixed (pl);

	return (status);
}

/*********************************************************************
**    E_FUNCTION     :  ncl_offset_wat_botsf(cbase)
**       Get a plane level re given vertical direction
**    PARAMETERS
**       INPUT  :
**          key    - the Unibase key
**          vert   - the vertical direction
**          GUI    - flag for error output
**          buf    - buffer for error output
**       OUTPUT :
**          wbase      - base surface data
**    RETURNS      : UU_SUCCESS / UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_offset_wat_botsf(cbase,dis)
NCL_w_base *cbase;
UU_REAL dis;
{
	UU_LOGICAL lcopy;
	int status;
	UU_REAL cdis;

	lcopy = UU_FALSE;
	if (cbase->lrev == 1)
		cdis = -dis;
	else
		cdis = dis;

	status = ncl_sf_offset (lcopy,cbase->srf,cdis);

	return (status);
}

/*********************************************************************
**    E_FUNCTION     :  ncl_create_wat_botsf(key,tpos,GUI,buf,wbase,pl)
**       Get a plane level re given vertical direction
**    PARAMETERS
**       INPUT  :
**          key    - the Unibase key
**          tpos   - the initial tool position
**          GUI    - flag for error output
**          buf    - buffer for error output
**       OUTPUT :
**          wbase      - base surface data
**    RETURNS      : UU_SUCCESS / UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_create_wat_botsf(lnum,key,tpos,GUI,buf,wbase,pl)
int lnum;
UU_KEY_ID key;
UM_real8 *tpos;
UM_int2 GUI;
char *buf;
NCL_w_base *wbase;
struct NCL_nclpl_rec *pl;
{
	struct NCL_fixed_databag e;
	struct NCL_trimsf_rec *tsf;
	UU_KEY_ID skey;
	int status,k,layer,dum;
	UM_transf tfmat;
	UM_real8 ta[3],te[3],vt8[3];
	UM_int4 ksec;
	UM_int2 lfl_77;
	UM_int2 isub,itype;

	UM_f77_str_ptr str77;

	struct UM_transf_rec tran;
	UU_REAL u,v,co;

	if (key == NULLKEY) goto Err;
	skey = NULLKEY;
	e.key = key;
	status = ncl_retrieve_data_fixed (&e);

	if (status == UU_SUCCESS)
		status = uc_retrieve_transf(key, tfmat);

	wbase->key0 = NULLKEY;

	if (status == UU_SUCCESS)
	{
		if (e.rel_num == NCL_TRIMSF_REL)
		{
			tsf = (struct NCL_trimsf_rec *) &e;
			e.key = tsf->bs_key;
			status = ncl_retrieve_data_fixed (&e);
		}
		else
		{
			um_get_attrib (&key,&dum,&dum,&dum,&dum,&layer);
			if (layer == lnum) wbase->key0 = key;
		}
	}

	if (status != UU_SUCCESS || e.rel_num != NCL_REVSURF_REL) goto Err;

/* set ranfile label flag to temp unknown */
	lfl_77 = 1;
	stunlb (&lfl_77);

	status = uc_copy (&e, wbase->srf, sizeof(struct NCL_fixed_databag));

	stunlb (&lfl_77);

	if (status == UU_SUCCESS)
	{
		skey = wbase->srf->key;
		if (skey == NULLKEY) status = UU_FAILURE;

		if (status == UU_SUCCESS)
		{
/* get its transform */
			tran.key = skey;
			tran.rel_num = UM_TRANSFORM_REL;
			status = ur_retrieve_transf(&tran);
			if (status == UU_SUCCESS)
			{
				um_tftmtf(tfmat, tran.tfmat, tran.tfmat);
				status = ur_update_transf(&tran);
				if (status == UU_SUCCESS)
					status = uc_retrieve_transf(skey, wbase->tfmat);
			}
		}
	}

	if (status != UU_SUCCESS) goto Err;

	for (k = 0; k < 3; k++)
	{
		te[k] = tpos[k];
		ta[k] = tpos[k+3];
	}

	ksec = skey;
	itype = 9; /* surface */
	isub = 1;

	ptdsc3(&ksec,&isub,&itype,&wbase->asw);
	sfini1 (&wbase->asw,&ISF,&wbase->u,&wbase->v,te,ta);

	u = wbase->u; v = wbase->v;
	uc_evsrf (UM_NORM,u,v,wbase->srf,wbase->tfmat,&Sevsrf);

	for (k = 0; k < 3; k++) vt8[k] = Sevsrf.snorm[k];
	from_unibase (vt8,vt8,&IVE);

	for (k = 0, co = 0; k < 3; k++)
	{
		co += vt8[k]*ta[k];
	}
	wbase->lrev = (co < 0);

	ncl_wat_setpln (pl);

	goto Done;

Err:
	strcpy (buf,"Bottom Surface Undefined.");
	if (status == UU_SUCCESS) status = UU_FAILURE;
	if (GUI == 1)
		ud_wrerr(buf);
	else
	{
		UM_init_f77_str (str77, buf, 80);
		status = 19; uerror2(str77);
	}

Done:
	return (status);
}

/*********************************************************************
**    E_FUNCTION     :  ncl_copy_wat_botsf(wbase,cbase)
**       Copy a base surface
**    PARAMETERS
**       INPUT  :
**          wbase      - base surface data
**       OUTPUT :
**          cbase      - base surface data
**    RETURNS      : UU_SUCCESS / UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_copy_wat_botsf(wbase,cbase)
NCL_w_base *wbase,*cbase;
{
	UU_KEY_ID ckey;
	int status;
	UM_int4 ksec;
	UM_int2 lfl_77;
	UM_int2 isub,itype;
	struct UM_transf_rec tran;

	status = UU_SUCCESS;

/* set ranfile label flag to temp unknown */
	lfl_77 = 1;
	stunlb (&lfl_77);

	status = uc_copy (wbase->srf, cbase->srf, sizeof(struct NCL_fixed_databag));

	stunlb (&lfl_77);

	if (status == UU_SUCCESS)
	{
		ckey = cbase->srf->key;
		if (ckey == NULLKEY) status = UU_FAILURE;

		if (status == UU_SUCCESS)
		{
/* get its transform */
			tran.key = ckey;
			tran.rel_num = UM_TRANSFORM_REL;
			um_tftotf(wbase->tfmat, tran.tfmat);
			status = ur_update_transf(&tran);
			if (status == UU_SUCCESS)
				status = uc_retrieve_transf(ckey, cbase->tfmat);
		}
	}

	if (status != UU_SUCCESS) goto Done;

	cbase->lrev = wbase->lrev;

	ksec = ckey;
	itype = 9; /* surface */
	isub = 1;

	ptdsc3(&ksec,&isub,&itype,&cbase->asw);

	cbase->u = wbase->u; cbase->v = wbase->v;

Done:
	return (status);
}

/*********************************************************************
**    E_FUNCTION     :  ncl_create_wat_botpl (pl)
**       Create a horizontal plane in Unibase
**    PARAMETERS
**       INPUT  : none
**       OUTPUT :
**          pl     - the plane
**    RETURNS      : UU_SUCCESS / UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_create_wat_botpl (pl)
struct NCL_nclpl_rec *pl;
{
	int status;

	pl->rel_num = NCL_PLN_REL;
	strncpy (pl->label,"@UN    ",7);
	pl->no_displst = 0; pl->displst = UU_NULL;
	status = ur_create_data(pl);
	if (pl->key == NULLKEY)
		return (UU_FAILURE);
	else
		return (UU_SUCCESS);
}

/*********************************************************************
**    I_FUNCTION     : void S_sfpt (wbase,pt,lpt,spt,snorm,itsk)
**       Local sfpt interface: project a point on the base surface
**    PARAMETERS
**       INPUT  :
**          wbase      - base surface data
**          pt         - projection point
**          itsk       - transform from unibase iff 1
**       OUTPUT :
**          spt        - surface point
**          snorm      - surface normal
**          lpt        - projection point, in world coordinates
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_sfpt (wbase,pt,lpt,spt,snorm,itsk)
NCL_w_base *wbase;
UM_coord pt,lpt,spt;
UM_vector snorm;
{
	int k;
	UM_real8 pt8[3];
	UM_real4 ss[9];

	for (k = 0; k < 3; k++) pt8[k] = pt[k];
	if (itsk == 1) from_unibase (pt8,pt8,&IPT);

	sfpt1 (&wbase->asw,pt8,&ISF,&DIR,&wbase->u,&wbase->v,ss);
	for (k = 0; k < 3; k++)
	{
		lpt[k] = pt8[k];
		snorm[k] = ss[k];
		spt[k] = ss[k+4];
	}
}

/*********************************************************************
**    E_FUNCTION     : void ncl_wat_baseproj (wbase,pt,spt)
**       Project a point on a surface.
**    PARAMETERS
**       INPUT  :
**          wbase      - base surface data
**          pt         - point to project
**       OUTPUT :
**          spt        - u,v,h coordinates of the point
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_wat_baseproj (wbase,pt,spt)
NCL_w_base *wbase;
UM_coord pt,spt;
{
	UM_coord lpt;
	UM_vector snorm,vec;
	UU_REAL h;

	S_sfpt (wbase,pt,lpt,spt,snorm,1);
	um_vcmnvc (lpt,spt,vec);
	h = UM_DOT (snorm,vec);

	spt[0] = wbase->u;
	spt[1] = wbase->v;
	spt[2] = h;
}

/*****************************************************************************
**    I_FUNCTION     : void S_evsrf (flg,wbase,u,v,evsrf)
**       Local uc_evsrf interface: evaluate on the base surface
**    PARAMETERS
**       INPUT  :
**          wbase      - base surface data
**          u,v        - surface parameters
**          flg        - point if -1, point and normal if 0, derivatives if 1
**       OUTPUT :
**          evsrf      - surface evaluator
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*****************************************************************************/
static void S_evsrf (flg,wbase,u,v,evsrf)
int flg;
NCL_w_base *wbase;
UU_REAL u,v;
struct UM_evsrfout *evsrf;
{
	int k;
	UM_real8 t[3];

	if (flg == -1)
	uc_evsrf (UM_POINT,u,v,wbase->srf,wbase->tfmat,evsrf);
	else
	uc_evsrf (UM_NORM,u,v,wbase->srf,wbase->tfmat,evsrf);

	for (k = 0; k < 3; k++) t[k] = evsrf->sp[k];
	from_unibase (t,t,&IPT);
	for (k = 0; k < 3; k++) evsrf->sp[k] = t[k];

	if (flg == -1) return;

	if (wbase->lrev)
	{
		for (k = 0; k < 3; k++) t[k] = -evsrf->snorm[k];
	}
	else
	{
		for (k = 0; k < 3; k++) t[k] = evsrf->snorm[k];
	}
	from_unibase (t,t,&IVE);
	for (k = 0; k < 3; k++) evsrf->snorm[k] = t[k];
	um_unitvc (evsrf->snorm,evsrf->snorm);

	if (flg == 1)
	{
		for (k = 0; k < 3; k++) t[k] = evsrf->dsdu[k];
		from_unibase (t,t,&IVE);
		for (k = 0; k < 3; k++) evsrf->dsdu[k] = t[k];
		um_unitvc (evsrf->dsdu,evsrf->dsdu);

		for (k = 0; k < 3; k++) t[k] = evsrf->dsdv[k];
		from_unibase (t,t,&IVE);
		for (k = 0; k < 3; k++) evsrf->dsdv[k] = t[k];
		um_unitvc (evsrf->dsdv,evsrf->dsdv);
	}
}

/*********************************************************************
**    I_FUNCTION     : UU_REAL S_rad_aver (wbase,u0,v0,r0)
**       Calculate the average 2D offset in the base surface UV-space that
**       corresponds to the 3D offset by r0 in the surface tangent plane
**    PARAMETERS
**       INPUT  :
**          wbase      - base surface data
**          u0,v0      - surface parameters
**          r0         - 3D offset parameter
**       OUTPUT : none
**    RETURNS      : average 2D offset
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UU_REAL S_rad_aver (wbase,u0,v0,r0)
NCL_w_base *wbase;
UU_REAL u0,v0,r0;
{
	int i,k,n;
	UM_real8 pti[3];
	UM_real4 ss[9];
	UM_coord sx,sy;
	UU_REAL r,du,dv,ri,alp,dal,cx,cy,dn;

	S_evsrf (0,wbase,u0,v0,&Sevsrf);

	um_2perpvc (Sevsrf.snorm,sx,sy);

	n = 8;
	dn = 1./n;
	dal = UM_TWOPI * dn;
	wbase->u = u0; wbase->v = v0;

	r = 0;
	for (i = 0, alp = 0; i < n; i++, alp += dal)
	{
		cx = cos (alp); cy = sin (alp);
		for (k = 0; k < 3; k++) pti[k] = Sevsrf.sp[k] + r0*(cx*sx[k] + cy*sy[k]);

		sfpt1 (&wbase->asw,pti,&ISF,&DIR,&wbase->u,&wbase->v,ss);

		du = wbase->u - u0; dv = wbase->v - v0;
		ri = sqrt (du*du + dv*dv);
		r = r + ri;
	}

	r = r*dn;

	return (r);
}

/*********************************************************************
**    E_FUNCTION     : void ncl_wat_set_trad (wbase,te,trad)
**       Calculate the average 2D offset in the base surface UV-space that
**       corresponds to the 3D offset by trad in the surface tangent plane
**    PARAMETERS
**       INPUT  :
**          wbase      - base surface data
**          trad       - 3D offset parameter
**       OUTPUT :
**          trad       - 2D offset parameter
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_wat_set_trad (wbase,te,trad)
NCL_w_base *wbase;
UM_real8 te[];
UU_REAL *trad;
{
	UU_REAL r0,u0,v0;

	r0 = *trad;

	u0 = v0 = 0.5;
	*trad = S_rad_aver (wbase,u0,v0,r0);
}

/*********************************************************************
**    E_FUNCTION     : void ncl_refine_trad (wbase,xmmx,ymmx,trad,tol2d,eps,tol)
**       Calculate the average 2D offset in the base surface UV-space that
**       corresponds to the 3D offset by tool radius in the surface tangent
**       plane. Use the calculated 3D-2D factor to get 2D tolerance parameters.
**       The part bounding box is used to make a better estimate.
**    PARAMETERS
**       INPUT  :
**          wbase      - base surface data
**          xmmx,ymmx  - 2D bounding box for all layer surfaces
**          trad       - 3D offset parameter
**          tol2d      - 3D tolerance
**          eps        - 3D squared maximum gap
**       OUTPUT :
**          trad       - 2D offset parameter
**          tol2d      - 2D tolerance
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_refine_trad (wbase,xmmx,ymmx,trad,tol2d,tol)
NCL_w_base *wbase;
UU_REAL xmmx[],ymmx[];
UU_REAL *trad,*tol2d,tol;
{
	UU_REAL r0,u0,v0,r,fac;

	r0 = *trad;

	u0 = (xmmx[0] + xmmx[1])/2;
	v0 = (ymmx[0] + ymmx[1])/2;

	r = S_rad_aver (wbase,u0,v0,r0);

	*trad = r;
	fac = r/r0;

	*tol2d = MAX2 (tol*fac,UM_FUZZ);;
}

/*********************************************************************
**    I_FUNCTION     : int S_proj_offset (wbase,pt0,del,vec,vpn,tol)
**       Offset a point by a vector, then project on the base surface.
**    PARAMETERS
**       INPUT  :
**          wbase      - base surface data
**          pt         - starting point
**          del        - offset distance
**          vec        - offset vector
**          tol        - 3D tolerance
**       OUTPUT :
**          wbase->u,wbase->v  - parameters at projected point
**          vpn                - normal at projected point
**
**    RETURNS      : UU_SUCCESS / UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int S_proj_offset (wbase,pt0,del,vec,vpn,tol)
NCL_w_base *wbase;
UM_coord pt0;
UM_vector vpn,vec;
UU_REAL del,tol;
{
	int status;
	UM_coord lpt,spt,pt;
	UM_vector snorm,vc;
	UU_REAL d,eps,u,v,gdir;

	eps = 9*tol*tol;

	um_translate_point (pt0,del,vec,pt);

	S_sfpt (wbase,pt,lpt,spt,snorm,0);

	gdir = DIR;
	status = ncl_pt_proj_sf(lpt,wbase->srf,&wbase->u,&wbase->v,&gdir,1,vpn,tol,
		spt,snorm);

	if (status == UU_SUCCESS)
	{
		um_vcmnvc(spt,pt,vc);
		d = UM_DOT (vc,vc);
		if (d > eps)
		{
			u = wbase->u; v = wbase->v;
			if (u < UM_FUZZ || u > 1. - UM_FUZZ ||
				u < UM_FUZZ || u > 1. - UM_FUZZ)
/*
..... qar 96043: point projects on an extension return error.
*/
				status = 522;
		}
	}

	return (status);
}

/*********************************************************************
**    I_FUNCTION     : S_get_uv_offset (wbase,xmmx,ymmx,offdis,i,j,du,dv,tol)
**       Offset a surface point along U and V directions in 3D, and get the
**       corresponding U and V offset values.
**    PARAMETERS
**       INPUT  :
**          wbase      - base surface data
**          xmmx,ymmx  - UV range for all layer surfaces
**          i,j        - indices defining the corner
**                       ((0,0) is the bottom left, etc)
**          offdis     - offset distance
**          tol        - tolerance
**       OUTPUT :
**          du,dv      - u and v offset at the corner
**    RETURNS      : UU_SUCCESS / UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_get_uv_offset (wbase,xmmx,ymmx,offdis,i,j,du,dv,tol)
int i,j;
NCL_w_base *wbase;
UU_REAL xmmx[],ymmx[],du[],dv[],offdis,tol;
{
	int l,status;
	UU_REAL u,v;
	UM_vector vec;

	u = xmmx[i]; v = ymmx[j];

	S_evsrf (1,wbase,u,v,&Sevsrf);

	if (i == 0)
		um_negvc (Sevsrf.dsdu,vec);
	else
		um_vctovc (Sevsrf.dsdu,vec);

	wbase->u = u; wbase->v = v;

	status = S_proj_offset (wbase,Sevsrf.sp,offdis,vec,Sevsrf.snorm,tol);
	if (status != UU_SUCCESS) return (status);

	l = 2*i+j;
	if (i == 0)
	{
		du[l] = u - wbase->u;
	}
	else
		du[l] = wbase->u - u;

	if (j == 0)
		um_negvc (Sevsrf.dsdv,vec);
	else
		um_vctovc (Sevsrf.dsdv,vec);

	wbase->u = u; wbase->v = v;

	status = S_proj_offset (wbase,Sevsrf.sp,offdis,vec,Sevsrf.snorm,tol);
	if (status != UU_SUCCESS) return (status);

	if (j == 0)
	{
		dv[l] = v - wbase->v;
	}
	else
		dv[l] = wbase->v - v;

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_create_2box1 (wbase,xmmx,ymmx,offdis,bbox,tol)
**       Create a UV-box as a pseudo-stock for a collection of surfaces.
**    PARAMETERS
**       INPUT  :
**          wbase      - base surface data
**          xmmx,ymmx  - UV range for all layer surfaces
**          offdis     - offset distance
**          tol        - tolerance
**       OUTPUT :
**          bbox       - 2D offset box
**    RETURNS      : UU_SUCCESS / UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_create_2box1 (wbase,xmmx,ymmx,offdis,bbox,tol)
NCL_w_base *wbase;
UU_REAL xmmx[],ymmx[],offdis,tol;
UM_2box *bbox;
{
	int i,j,status;
	UU_REAL du[4],dv[4],dw,w;

	status = UU_SUCCESS;
	if (fabs(offdis) < tol)
	{
		bbox->xmin = xmmx[0];
		bbox->xmax = xmmx[1];
		bbox->ymin = ymmx[0];
		bbox->ymax = ymmx[1];
		return (status);
	}
/*
..... get u and v offsets at the four corners, then average
*/
	for (i = 0; i < 2; i++)
	{
		for (j = 0; j < 2; j++)
		{
			status = S_get_uv_offset (wbase,xmmx,ymmx,offdis,i,j,du,dv,tol);
			if (status != UU_SUCCESS) return (status);
		}
	}

	dw = (du[0] + du[1]) / 2;
	w = xmmx[0] - dw;
	if (w < 0) w = 0;
	bbox->xmin = w;

	dw = (du[2] + du[3]) / 2;
	w = xmmx[1] + dw;
	if (w > 1) w = 1;
	bbox->xmax = w;

	dw = (dv[0] + dv[1]) / 2;
	w = ymmx[0] - dw;
	if (w < 0) w = 0;
	bbox->ymin = w;

	dw = (dv[2] + dv[3]) / 2;
	w = ymmx[1] + dw;
	if (w > 1) w = 1;
	bbox->ymax = w;

	return (status);
}

/*****************************************************************************
**    I_FUNCTION     : void S_set_pm (wbase,uv0,uv1,sp0,sn0,vc0,lr,tol)
**          Determine whether surface normal cross contour forward vector is
**          directed inside or outside the contour
**    PARAMETERS
**       INPUT  :
**          wbase      - base surface data
**          uv0        - first 2D point
**          sp0        - surface point at uv0
**          sn0        - surface normal at uv0
**          vc0        - 3D forward vector at uv0
**          uv1        - second 2D point
**          tol      - tolerance
**       OUTPUT :
**          lr      - 1 iff inside, -1 iff outside
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*****************************************************************************/
static void S_set_pm (wbase,uv0,uv1,sp0,sn0,vc0,lr,tol)
NCL_w_base *wbase;
UM_coord uv0,uv1,sp0;
UM_vector sn0,vc0;
int *lr;
UU_REAL tol;
{
	UU_REAL x,y,x1,y1,du,dv,co,del;
	UM_coord spt,pt,lpt;
	UM_vector snorm,voff;

	um_cross (sn0,vc0,voff);
	um_unitvc (voff,voff);

	del = 5*tol;
	um_translate_point (sp0,del,voff,pt);

	wbase->u = uv0[0]; wbase->v = uv0[1];
	S_sfpt (wbase,pt,lpt,spt,snorm,0);

	x = wbase->u - uv0[0];
	y = wbase->v - uv0[1];

	du = uv1[0] - uv0[0];
	dv = uv1[1] - uv0[1];

	if (*lr == -1)
	{
		x1 = dv; y1 = -du;
	}
	else
	{
		x1 = -dv; y1 = du;
	}

	co = x*x1 + y*y1;

	if (co < 0)
		*lr = -1;
	else
		*lr = 1;
}

/*********************************************************************
**    I_FUNCTION     : void S_offset_vec (pm,sn,vc,voff)
**          Calculate the current offset vector on the base surface.
**          if necessary, so that vta cross a boundary tangent vector points
**          inside the surface.
**    PARAMETERS
**       INPUT  :
**          pm      - flag, reverse the offset iff -1
**          sn      - surface normal
**          vc      - 3D forward vector
**       OUTPUT :
**          vc      - 3D offset vector
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_offset_vec (pm,sn,vc,voff)
int pm;
UM_vector sn,vc,voff;
{
	if (pm == 1)
		um_cross (sn,vc,voff);
	else
		um_cross (vc,sn,voff);
	um_unitvc (voff,voff);
}

/*********************************************************************
**    I_FUNCTION     : int S_add_offset (wbase,uvpts,pt,dis,voff,sn,tol)
**          Offset a 2D surface point, reproject on the base surface.
**    PARAMETERS
**       INPUT  :
**          wbase      - base surface data
**          dis        - offset parameter
**          pt         - 3D point to offset
**          voff       - 3D offset vector
**          sn         - surface normal
**          tol       - tolerance
**       OUTPUT :
**          uvpts     - updated list
**    RETURNS      : UU_SUCCESS / UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_add_offset (wbase,uvpts,pt,dis,voff,sn,tol)
NCL_w_base *wbase;
UU_LIST *uvpts;
UM_coord pt;
UM_vector sn,voff;
UU_REAL dis,tol;
{
	int status;
	UM_coord uvo;

	status = S_proj_offset (wbase,pt,dis,voff,sn,tol);
	if (status == UU_SUCCESS)
	{
		uvo[0] = wbase->u; uvo[1] = wbase->v;
		uvo[2] = 0;
		uu_list_push (uvpts,uvo);
	}

	return (status);
}

/*********************************************************************
**    I_FUNCTION     : int S_wat_offset1 (wbase,uvpts,pt,sn,vc0,vc,pm,dis,tol)
**       Offset a 2D-polygon point by dis, possibly as a corner point.
**    PARAMETERS
**       INPUT  :
**          wbase    - base surface data
**          lr       - flag, reverse the offset direction iff -1
**          dis      - the offset distance
**          pt       - point to offset
**          sn       - current surface normal
**          vc0      - previous contour forward
**          vc       - current contour forward
**          tol      - tolerance
**       OUTPUT :
**          uvpts    - the polygon contour updated
**
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_wat_offset1 (wbase,uvpts,pt,sn,vc0,vc,pm,dis,tol)
NCL_w_base *wbase;
UU_LIST *uvpts;
UM_coord pt;
UM_vector sn,vc0,vc;
int pm;
UU_REAL dis,tol;
{
	UU_REAL co,co2,csmin,d;
	UM_vector vc1,vof,vof0;
	int status;

	csmin = 0.9238795; /* corner if more than 22.5 degrees */

	S_offset_vec (pm,sn,vc,vof);
/*
..... offset corners
*/
	co = UM_DOT (vc0,vc);

	if (co <= csmin)
	{
		if (UM_DOT (vof,vc0) > 0)
		{
			um_vcplvc (vc0,vc,vc1);
			S_offset_vec (pm,sn,vc1,vof);

			co2 = sqrt ((1.+co)/2.);
			d = dis/co2;

			status = S_add_offset (wbase,uvpts,pt,d,vof,sn,tol);
			return (status);
		}
		else
		{
			S_offset_vec (pm,sn,vc0,vof0);

			status = S_add_offset (wbase,uvpts,pt,dis,vof0,sn,tol);
			if (status != UU_SUCCESS) return (status);
		}
	}

	status = S_add_offset (wbase,uvpts,pt,dis,vof,sn,tol);

	return (status);
}

/*********************************************************************
**    I_FUNCTION     : S_wat_offset (wbase,cvpoint,lr,dis,tol2d,tol)
**       Expand a 2D-surface polygon by dis in 3D, that is determine the offset
**       direction which would make the polygon larger, and do the offseting
**    PARAMETERS
**       INPUT  :
**          wbase    - base surface data
**          lr       - flag, reverse the offset direction iff -1
**          uvpts    - the polygon contour
**          dis      - the offset distance
**          tol      - tolerance
**       OUTPUT :
**          uvpts    - the offset polygon contour
**
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_wat_offset (wbase,uvpts,lr,dis,tol)
NCL_w_base *wbase;
UU_LIST *uvpts;
int lr;
UU_REAL dis;
UU_REAL tol;
{
	int i,npts,n0,i0,pm;
	int status = UU_SUCCESS;
	UU_REAL u,v;
	UM_coord *uv,*pt;
	UM_coord pprev,p0;
	UM_vector *sn,*vc;
	UM_vector vec;

	uv = (UM_coord *) UU_LIST_ARRAY (uvpts);
	npts = uvpts->cur_cnt;
	n0 = npts-1;

	UU_LIST_EMPTY (&ptlst);
	UU_LIST_EMPTY (&snlst);
	UU_LIST_EMPTY (&vclst);

	for (i = 0; i < n0; i++)
	{
		u = uv[i][0]; v = uv[i][1];
		S_evsrf (0,wbase,u,v,&Sevsrf);
		uu_list_push (&ptlst,Sevsrf.sp);
		uu_list_push (&snlst,Sevsrf.snorm);
		if (i == 0)
		{
			um_vctovc (Sevsrf.sp,p0);
			um_vctovc (p0,pprev);
		}
		else
		{
			um_vcmnvc (Sevsrf.sp,pprev,vec);
			um_unitvc (vec,vec);
			uu_list_push (&vclst,vec);

			if (i < n0 - 1)
				um_vctovc (Sevsrf.sp,pprev);
			else
			{
				um_vcmnvc (p0,Sevsrf.sp,vec);
				um_unitvc (vec,vec);
				uu_list_push (&vclst,vec);
			}
		}
	}

	pm = lr;
	pt = (UM_coord *) UU_LIST_ARRAY (&ptlst);
	sn = (UM_vector *) UU_LIST_ARRAY (&snlst);
	vc = (UM_vector *) UU_LIST_ARRAY (&vclst);

	S_set_pm (wbase,uv[0],uv[1],pt[0],sn[0],vc[0],&pm,tol);

	for (i = 0; i < n0 && status == UU_SUCCESS; i++)
	{
		i0 = (i > 0)? i-1: n0-1;
		status = S_wat_offset1 (wbase,uvpts,pt[i],sn[i],vc[i0],vc[i],pm,dis,tol);
	}

	uu_list_delete (uvpts,0,npts);

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : S_evolve (wbase,pgn0,uvpts,tol,eps)
**       Expand a 2D surface polygon by dis in 3D.
**    PARAMETERS
**       INPUT  :
**          wbase    - base surface data
**          pgn0     - the polygon contour (closed)
**          cvpoint  - initialized list to use
**          dis      - the offset distance
**          tol2d    - 2D tolerance
**          tol      - tolerance
**       OUTPUT :
**          stk      - the offset polygon
**
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_evolve (wbase,pgn0,uvpts,tol,eps)
NCL_w_base *wbase;
UU_LIST *pgn0,*uvpts;
UU_REAL tol,eps;
{
	int i,j,k,n,npts;
	int status = UU_SUCCESS;
	UM_coord *pp;
	UU_REAL d;
	UM_2Dcoord *vtx,vti;

	npts = pgn0->cur_cnt;
	vtx = (UM_2Dcoord *) UU_LIST_ARRAY (pgn0);

	while (npts >= 4)
	{
		d = UM_SQDIS_2D(vtx[0],vtx[npts-1]);
		if (d > eps) break;
		npts--; pgn0->cur_cnt--;
	}

	vti[0] = vtx[0][0];
	vti[1] = vtx[0][1];
	uu_list_push (pgn0,vti);

	vtx = (UM_2Dcoord *) UU_LIST_ARRAY (pgn0);


	for (i = 0; i < npts; i++)
	{
		for (k = 0; k < 2; k++)
		{
			Sln.spt[k] = vtx[i][k];
			Sln.ept[k] = vtx[i+1][k];
		}
		status = ur_update_data_fixed (&Sln);
		if (status != UU_SUCCESS) return (status);


		UU_LIST_EMPTY (&ptlst);
		UU_LIST_EMPTY (uvpts);
		n = ncl_evolve_bn_crv_on_srf (wbase->srf,wbase->tfmat,&Sln,BPLM,tol,
										&ptlst,UU_NULL,uvpts);

		if (n > 2)
		{
			pp = (UM_coord *) UU_LIST_ARRAY (uvpts);
			for (j = 1; j < n-1; j++)
			{
				uu_list_insert (pgn0,++i,&pp[j]);
			}
			npts += (n-2);
			vtx = (UM_2Dcoord *) UU_LIST_ARRAY (pgn0);

		}

	}

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_box_stklst (wbase,stk,bpt,uvpts,tol)
**       Evolve a UV box boundary and store uv-points in stklst
**    PARAMETERS
**       INPUT  :
**          wbase    - base surface data
**          bbox     - the UV box on the base surface
**          uvpts    - initialized list to use
**          tol      - tolerance
**       OUTPUT :
**          stk      - the evolved polygon
**
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_box_stklst (wbase,stk,bbox,uvpts,tol)
NCL_w_base *wbase;
UU_LIST *stk,*uvpts;
UM_2box *bbox;
UU_REAL tol;
{
	int j,n,status;
	UU_REAL ur[2],vr[2];
	UM_coord *pp;
	UM_coord ppt;
	UM_vector pve;

	uu_list_init (stk, sizeof(UM_2Dcoord), 200, 200);

	ur[0] = bbox->xmin;
	vr[0] = bbox->ymin;
	ur[1] = bbox->xmax;
	vr[1] = bbox->ymax;

	n = um_evolve_boxbndr (wbase->srf,wbase->tfmat,ur,vr,tol,
		&ptlst,NULLST,uvpts,(struct NCL_uvconv *)UU_NULL);
	if (n < 4) return (UU_FAILURE);

/*
..... calculate the contour average plane - to use at each cutting level.
*/
	pp = (UM_coord *) UU_LIST_ARRAY (&ptlst);
	status = um_ptstopln (n-1,pp,ppt,pve,tol);
	if (status != UU_SUCCESS) return (status);

	ncl_wat_set_plane (ppt,pve);

	pp = (UM_coord *) UU_LIST_ARRAY (uvpts);
	for (j = 0; j < n; j++)
	{
		uu_list_push (stk,pp[j]);
	}

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_offset_out1 (wbase,stk,pgn0,cvpoint,dis,tol2d,tol)
**       Expand a 2D surface polygon by dis in 3D.
**    PARAMETERS
**       INPUT  :
**          wbase    - base surface data
**          pgn0     - the polygon contour (closed)
**          cvpoint  - initialized list to use
**          dis      - the offset distance
**          tol2d    - 2D tolerance
**          tol      - tolerance
**       OUTPUT :
**          stk      - the offset polygon
**
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_offset_out1 (wbase,stk,pgn0,cvpoint,dis,tol2d,tol)
NCL_w_base *wbase;
UU_LIST *pgn0,*stk,*cvpoint;
UU_REAL dis;
UU_REAL tol2d,tol;
{
	int i,npts,lr,status;
	UM_coord *pp,pti;
	UM_vector dm;
	UU_REAL eps;
	UM_2Dcoord *vtx;
	UM_coord ppt;
	UM_vector pve;
	UU_REAL u,v;

	eps = tol2d*tol2d;

	status = S_evolve (wbase,pgn0,cvpoint,tol,eps);

	npts = pgn0->cur_cnt;
	vtx = (UM_2Dcoord *) UU_LIST_ARRAY (pgn0);

	if (status != UU_SUCCESS || npts < 4) return (UU_FAILURE);

	if (fabs(dis) < tol2d) /* no offset */
	{
		uu_list_init (stk, sizeof(UM_2Dcoord), npts, npts);
		uu_list_push_multiple (stk,npts,vtx);
		goto Pln;
	}

	UU_LIST_EMPTY (cvpoint);

	pti[2] = 0.;
	for (i = 0; i < npts; i++)
	{
		pti[0] = vtx[i][0];
		pti[1] = vtx[i][1];
		uu_list_push (cvpoint,pti);
	}

	status = ncl_out_dm (cvpoint,NULLST,dm,&lr);

	if (status == UU_SUCCESS)
	{
		npts = cvpoint->cur_cnt;
		ncl_shuffle (cvpoint,NULLST,npts,10000*eps);

		status = S_wat_offset (wbase,cvpoint,lr,dis,tol);

		if (status == UU_SUCCESS)
		{
			npts = cvpoint->cur_cnt;
			status = ncl_uv_deloop (&npts,cvpoint);
		}
	}
	if (npts < 4) status = UU_FAILURE;
	if (status != UU_SUCCESS) return (status);

	pp = (UM_coord *) UU_LIST_ARRAY (cvpoint);

	uu_list_init (stk, sizeof(UM_2Dcoord), npts, npts);
	for (i = 0; i < npts; i++)
	{
		pti[0] = pp[i][0]; pti[1] = pp[i][1];
		uu_list_push(stk,pti);
	}
	vtx = (UM_2Dcoord *) UU_LIST_ARRAY (stk);

Pln:
/*
..... calculate the contour average plane - to use at each cutting level.
*/
	UU_LIST_EMPTY (cvpoint);

	for (i = 0; i < npts-1; i++)
	{
		u = vtx[i][0]; v = vtx[i][1];
		S_evsrf (-1,wbase,u,v,&Sevsrf);
		uu_list_push (cvpoint,Sevsrf.sp);
	}

	pp = (UM_coord *) UU_LIST_ARRAY (cvpoint);
	status = um_ptstopln (npts-1,pp,ppt,pve,tol);
	if (status == UU_SUCCESS) ncl_wat_set_plane (ppt,pve);

	return (status);
}

/*********************************************************************
**    E_FUNCTION     :  ncl_get_pln_level (key,vert,dlev,GUI,buf)
**       Get a plane level re given vertical direction
**    PARAMETERS
**       INPUT  :
**          key    - the Unibase key
**          vert   - the vertical direction
**          GUI    - flag for error output
**          buf    - buffer for error output
**       OUTPUT :
**          dlev   - the level
**    RETURNS      : UU_SUCCESS / UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_get_pln_level (key,vert,dlev,GUI,buf)
UU_KEY_ID key;
UM_real8 *vert;
UU_REAL *dlev;
UM_int2 GUI;
char *buf;
{
	struct NCL_fixed_databag e;
	struct NCL_nclpl_rec *pln;
	UM_int2 primtyp;
	UM_real8 primdata[16];
	int status;
	UM_f77_str_ptr str77;

	if (key == NULLKEY) goto Err;
	e.key = key;
	status = ncl_retrieve_data_fixed (&e);
	if (status != UU_SUCCESS) goto Err;

	if (e.rel_num == NCL_PLN_REL)
	{
		pln = (struct NCL_nclpl_rec *)&e;
		primdata[4] = pln->pt[0];
		primdata[5] = pln->pt[1];
		primdata[6] = pln->pt[2];
	}
	else
	{
		status = ncl_get_sf_primdat(&key,&primtyp,primdata);
		if (status != UU_SUCCESS || (nclsf_prim_type) primtyp != NCLSF_PLANE)
			goto Err;
	}
	fr_unbs(&primdata[4],&primdata[4],&IPT);
	*dlev = primdata[4]*vert[0] + primdata[5]*vert[1] + primdata[6]*vert[2];

	return (UU_SUCCESS);

Err:
	strcat (buf,"Plane Undefined.");
	if (status == UU_SUCCESS) status = UU_FAILURE;
	if (GUI == 1)
		ud_wrerr(buf);
	else
	{
		UM_init_f77_str (str77, buf, 80);
		status = 19; uerror2(str77);
	}

	return (status);
}

/*********************************************************************
**    E_FUNCTION     :  ncl_wat_levels (wset,wpar,ntim,zmin,zmax,tol,vert,diam,
**                                                                      GUI,buf)
**       Calculate the levels data for waterline roughing
**    PARAMETERS
**       INPUT  :
**          wset       - waterline settings struct
**          wpar       - waterline parameters struct
**          zmin,zmax  - the global vertical range
**          vert       - the vertical direction
**          GUI        - flag for error output
**          buf        - buffer for error output
**          diam       - tool diameter
**          tol        - tolerance
**       OUTPUT :
**          ntim   - the number of level
**          wpar   - the levels data stored
**    RETURNS      : UU_SUCCESS / UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_wat_levels (wset,wpar,ntim,zmin,zmax,tol,vert,diam,GUI,buf)
NCL_w_setup *wset;
NCL_w_param *wpar;
int *ntim;
UU_REAL zmin,zmax,tol;
UM_real8 *vert,diam;
UM_int2 GUI;
char *buf;
{
	UU_REAL dtop,dbot,dtim,del,step;
	int ntimes,i;
	UM_f77_str_ptr str77;
	int status = UU_SUCCESS;

	dbot = 1.e+12; dtop = 1.e-12;

	if (wset->bottype == STOCK)
		dbot = zmin + wset->zbot;
	else if (!ncl_is_watrev() && wset->bottype == PLANE)
	{
		strcpy(buf,"Bottom ");
		status = ncl_get_pln_level(wset->botkey,vert,&dbot,GUI,buf);
		if (status != UU_SUCCESS) goto Done;
		dbot += wset->zbot;
	}
	else if (wset->bottype == ZLEV)
		dbot = wset->zbot;

	if (wset->toptype == STOCK)
		dtop = zmax + wset->ztop;
	else if (!ncl_is_watrev() && wset->toptype == PLANE)
	{
		strcpy(buf,"Top ");
		status = ncl_get_pln_level(wset->topkey,vert,&dtop,GUI,buf);
		if (status != UU_SUCCESS) goto Done;
		dtop += wset->ztop;
	}
	else if (wset->toptype == ZLEV)
		dtop = wset->ztop;

	if (wset->bottype == DEPTH) dbot = dtop - wset->zbot;
	else if (wset->toptype == DIST) dtop = dbot + wset->ztop;

	if (dbot < zmin) dbot = zmin;

	if (dtop < dbot - 0.5*tol)
	{
		strcpy (buf,"Top Undefined.");
		goto Err;
	}
	del = dtop - dbot;

	step = wset->zstep;

	if (step < 0)
		dtim = -step;
	else if (step == 0)
		dtim = (2.*del)/diam;
	else if (wset->steptype == 1)
	{
		ntimes = del/step;
		if (step*ntimes < del - UM_DFUZZ) ntimes++;
	}
	else
		dtim = del/step;

	if (wset->steptype == 0 || step <= 0) ntimes = dtim + 0.5;
	if (ntimes < 1 || del <= tol) ntimes = 1;
	del = del / ntimes;
	if (dtop - del > zmax + tol/2 && !wpar->havestk)
	{
/*
..... if need real intersections (no stock pocketing), do not go higher than zmax
*/
	  if (del > tol)
		{
			i = (dtop - zmax - tol/2)/del;
			dtop -= i*del; ntimes -= i;
		}
		else
			ntimes = 0;
		if (ntimes < 1)
		{
			strcpy (buf,"Top Undefined.");
			goto Err;
		}
	}

	*ntim = ntimes;
	wpar->z0 = dbot;
	wpar->z1 = dtop;
	wpar->dz = del;

	ncl_set_wlev (dtop);

	goto Done;

Err:
	if (status == UU_SUCCESS) status = UU_FAILURE;
	if (GUI == 1)
		ud_wrerr(buf);
	else
	{
		UM_init_f77_str (str77, buf, 80);
		status = 19; uerror2(str77);
	}

Done:
	return (status);
}

/*********************************************************************
**    E_FUNCTION     :  ncl_wat_sortpt (wset,wpar,rot,GUI)
**       Get a near point used for ordering
**    PARAMETERS
**       INPUT  :
**          wset   - waterline settings struct
**          rot    - rotation matrix
**          GUI    - flag for error output
**       OUTPUT :
**          wpar   - waterline parameters struct, with the near point if needed
**    RETURNS      : UU_SUCCESS / UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_wat_sortpt (wbase,wset,wpar,xmmx,ymmx,rot,GUI,tol)
NCL_w_base *wbase;
NCL_w_setup *wset;
NCL_w_param *wpar;
UU_REAL xmmx[],ymmx[],tol;
UM_transf rot;
UM_int2 GUI;
{
	int status,k,irot,frmtype;
	struct UM_point_rec frmpt;
	UM_coord pt,lpt,spt;
	UM_vector vec,snorm;
	UU_REAL u0,v0,du,dv,d;
	UM_real8 t[3];

	status = UU_SUCCESS;
	frmtype = wpar->frmtype;

	if (frmtype == NEARPT )
	{
		frmpt.key = wset->frmkey;
		status = ncl_retrieve_data_fixed (&frmpt);
		if (status != UU_SUCCESS)
		{
			if (GUI == 1)
				ud_wrerr("Near point undefined.");
			else
				status = 520;
		}
		else
		{
			if (ncl_is_watrev())
			{
				S_sfpt (wbase,frmpt.pt,lpt,spt,snorm,1);
				for (k = 0; k < 2; k++)
				{
					wpar->sortpt[k] = spt[k];
				}
			}
			else
			{
				ncl_get_rotfl (&irot);

				if (irot > 0) um_cctmtf(frmpt.pt,rot,frmpt.pt);
				for (k = 0; k < 2; k++)
				{
					wpar->sortpt[k] = frmpt.pt[k];
				}
			}
		}
	}
	else if (frmtype >= NEGX && frmtype <= POSZ)
	{
		um_nullvc (vec);
		if (frmtype == NEGX) vec[0] = -1;
		else if (frmtype == POSX) vec[0] = 1;
		else if (frmtype == NEGY) vec[1] = -1;
		else if (frmtype == POSY) vec[1] = 1;
		else if (frmtype == NEGZ) vec[2] = -1;
		else if (frmtype == POSZ) vec[2] = 1;
		if (ncl_is_watrev())
		{
			for (k = 0; k < 3; k++) t[k] = vec[k];
			to_unibase (t,t,&IVE);

			u0 = (xmmx[0] + xmmx[1])/2;
			v0 = (ymmx[0] + ymmx[1])/2;

			uc_evsrf (UM_POINT,u0,v0,wbase->srf,wbase->tfmat,&Sevsrf);

			for (k = 0; k < 3; k++) pt[k] = Sevsrf.sp[k] + 5*tol*t[k];

			S_sfpt (wbase,pt,lpt,spt,snorm,0);

			du = wbase->u - u0;
			dv = wbase->v - v0;
			d = du*du + dv*dv;

			if (d < UM_DFUZZ)
			{
				status = 520;
				if (GUI == 1)
					ud_wrerr("Direction modifier undefined.");
			}
			else
			{
				d = sqrt(d);
				wpar->sortpt[0] = du/d;
				wpar->sortpt[1] = dv/d;
			}
		}
		else
		{
			ncl_get_rotfl (&irot);

			if (irot > 0) um_vctmtf(vec,rot,vec);
			for (k = 0; k < 2; k++)
			{
				wpar->sortpt[k] = vec[k];
			}
		}
	}

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_waterline_create_geo (wbase,w,npts,pts)
**       Create geometry out of a polygon contour.
**    PARAMETERS
**       INPUT  :
**          wbase      - base surface data
**          npts,pts   - a 2D contour
**          w          - the current cutting level
**       OUTPUT : none
**
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_waterline_create_geo (wbase,w,npts,pts)
NCL_w_base *wbase;
UU_REAL w;
int npts;
UM_coord *pts;
{
	int i,status,np;
	UM_coord pt,*pp;
	UU_REAL u,v;

	if (ncl_is_watrev())
	{
		UU_LIST_EMPTY (&ptlst);

		for (i = 0; i < npts; i++)
		{
			u = pts[i][0]; v = pts[i][1];
			S_evsrf (0,wbase,u,v,&Sevsrf);
			um_translate_point (Sevsrf.sp,w,Sevsrf.snorm,pt);
			uu_list_push (&ptlst,pt);
		}
		np = ptlst.cur_cnt;
		pp = (UM_coord *) UU_LIST_ARRAY (&ptlst);

		status = ncl_waterline_create_geo0 (np,pp);
	}
	else
		status = ncl_waterline_create_geo0 (npts,pts);

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_waterline_create_geo1 (wbase,w,tolsq,curpock)
**       Create geometry out of the current level polygon.
**    PARAMETERS
**       INPUT  :
**          wbase      - base surface data
**          curpock    - the current polygon
**          w          - the current cutting level
**          tol        - squared tolerance
**       OUTPUT : none
**
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_waterline_create_geo1 (wbase,w,tolsq,curpock)
NCL_w_base *wbase;
UU_REAL w,tolsq;
ncl_polygon *curpock;
{
	int i,k,iv,nv,status;
	UM_coord *pts;
	UM_coord pti;
	UM_2Dcoord *vtx;

	status = UU_SUCCESS;
	k = curpock->num_contours;
	if (k > 0)
	{
		pti[2] = w;
		vtx = (UM_2Dcoord *) UU_LIST_ARRAY (curpock->contour);
		for (i = 0; i < k && status == UU_SUCCESS; i++)
		{
			nv = abs(curpock->np[i]);
			if (nv > 2)
			{
				UU_LIST_EMPTY (&ptlst);
				for (iv = 0; iv < nv; iv++)
				{
					pti[0] = vtx[iv][0]; pti[1] = vtx[iv][1];
					uu_list_push (&ptlst,pti);
				}
				if (UM_SQDIS_2D(vtx[0],vtx[nv-1]) > tolsq)
				{
					pti[0] = vtx[0][0]; pti[1] = vtx[0][1];
					uu_list_push (&ptlst,pti);
				}
				pts = (UM_coord *) UU_LIST_ARRAY (&ptlst);
				iv = ptlst.cur_cnt;
				status = ncl_waterline_create_geo (wbase,w,iv,pts);
			}
			vtx += nv;
		}
	}

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : void ncl_wat_evsrf (wbase,lpt,htop,spt)
**       Evaluate a point on the base surface
**    PARAMETERS
**       INPUT  :
**          wbase     - base surface data
**          htop      - current level
**          lpt       - point in UVH coordinates
**       OUTPUT :
**          spt       - 3D point
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_wat_evsrf (wbase,lpt,htop,spt)
NCL_w_base *wbase;
UU_REAL htop;
UM_coord lpt,spt;
{
	S_evsrf (0,wbase,lpt[0],lpt[1],&Sevsrf);
	um_translate_point (Sevsrf.sp,htop,Sevsrf.snorm,spt);
}

/*********************************************************************
**    E_FUNCTION     : void ncl_get_gap_data (lpt,lpt1,htop,dis)
**       Calculate the squared 3D distance between points on the base surface
**    PARAMETERS
**       INPUT  :
**          wbase     - base surface data
**          htop      - current level
**          lpt,lpt1  - points
**       OUTPUT : squared distance
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_wat_sqdis (wbase,lpt,lpt1,htop,dis)
NCL_w_base *wbase;
UU_REAL htop,*dis;
UM_coord lpt,lpt1;
{
	UM_coord spt,spt1;

	S_evsrf (0,wbase,lpt[0],lpt[1],&Sevsrf);
	um_translate_point (Sevsrf.sp,htop,Sevsrf.snorm,spt);
	S_evsrf (0,wbase,lpt1[0],lpt1[1],&Sevsrf);
	um_translate_point (Sevsrf.sp,htop,Sevsrf.snorm,spt1);

	*dis = um_sqdis(spt,spt1);
}

/*********************************************************************
**    E_FUNCTION     : void ncl_get_gap_data (lpt,lpt1,htop,dis)
**       Calculate the 'real world' gap distance between two points
*********************************************************************/
void ncl_get_gap_data (wbase,lpt,lpt1,htop,dis)
NCL_w_base *wbase;
UU_REAL htop,*dis;
UM_coord lpt,lpt1;
{
	int irot;
	UM_transf rtinv;

	if (ncl_is_watrev())
	{
		S_evsrf (0,wbase,lpt[0],lpt[1],&Sevsrf);
		um_translate_point (Sevsrf.sp,htop,Sevsrf.snorm,lpt);
		S_evsrf (0,wbase,lpt1[0],lpt1[1],&Sevsrf);
		um_translate_point (Sevsrf.sp,htop,Sevsrf.snorm,lpt1);
	}
	else
	{
		lpt[2] = lpt1[2] = htop;
		ncl_get_rotmx (&irot,rtinv);

		if (irot > 0)
		{
			um_cctmtf(lpt,rtinv,lpt);
			um_cctmtf(lpt1,rtinv,lpt1);
			fr_unbs(lpt,lpt,&IPT);
			fr_unbs(lpt1,lpt1,&IPT);
		}
	}

	*dis = um_dcccc (lpt,lpt1);
}

/*********************************************************************
**    E_FUNCTION     : void print_pol1 (wbase,w,q,buf)
**   Debug routine - print polygon contours. The polygon q is in the base
**   surface UV space; the points are evaluated and placed on the current
**   cutting level w.
*********************************************************************/
void print_pol1 (wbase,w,q,buf)
NCL_w_base *wbase;
UU_REAL w;
ncl_polygon *q;
char *buf;
{
	int nc,c,nv,iv;
	UU_REAL u,v,dd;
	UM_2Dcoord *pp;
	UM_coord pt,pt0;

	nc = q->num_contours;
	printf ("Num_contours =  %d\n",nc);

	if (nc > 0)
	{
		pp = (UM_2Dcoord *) UU_LIST_ARRAY (q->contour);
		for (c = 0; c < nc; c++)
		{
			UU_LIST_EMPTY (&ptlst);
			nv = abs(q->np[c]);

			sprintf(buf," %d points",nv);

			for (iv = 0; iv < nv; iv++)
			{
				u=pp[iv][0];
				v=pp[iv][1];
				S_evsrf (0,wbase,u,v,&Sevsrf);
				um_translate_point (Sevsrf.sp,w,Sevsrf.snorm,pt);
				if (iv == 0) um_vctovc (pt,pt0);
				uu_list_push (&ptlst,pt);
			}

			dd = UM_SQDIS (pt0,pt);
			if (dd > 0.000001)
				uu_list_push (&ptlst,pt0);

			ncl_print_curve (&ptlst,0,buf);

			pp += nv;
		}
		printf ("\n");
	}

	return;
}
/*********************************************************************
**    E_FUNCTION     : S_add_contour (itsk,wbase,np0,uvs,tol,tolsq)
**       Map a UV contour to the current base surface, put the result
**       into a list.
**    PARAMETERS
**       INPUT  :
**          itsk       - stock contour if 0; else 1
**          wbase      - base surface data
**          np0        - number contour points
**          uvs        - contour points
**          tol        - tolerance
**          tolsq      - squared tolerance
**       OUTPUT :
**          global lists ptlst,nplst updated
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_add_contour (itsk,wbase,np0,uvs,tol,tolsq)
NCL_w_base *wbase;
UM_2Dcoord *uvs;
int itsk,np0;
UU_REAL tol,tolsq;
{
	int i,j,k,npt,np1,n;
	int status = UU_SUCCESS;
	UU_REAL u,v,dd;
	UM_coord *pp,pt,pt0;

	dd = UM_SQDIS_2D(uvs[0],uvs[np0-1]);
	if (dd < tolsq)
		np1 = np0 - 1;
	else
		np1 = np0;
	npt = np1 + 1;

	for (i = 0; i < np1; i++)
	{
		u = uvs[i][0];
		v = uvs[i][1];
		S_evsrf (0,wbase,u,v,&Sevsrf);
		um_vctovc (Sevsrf.sp,pt);
		if (i == 0) um_vctovc (pt,pt0);

		uu_list_push (&ptlst,pt);

		if (itsk == 1)
		{
			for (k = 0; k < 2; k++)
			{
				Sln.spt[k] = uvs[i][k];
				if (i == np1-1)
					Sln.ept[k] = uvs[0][k];
				else
					Sln.ept[k] = uvs[i+1][k];
			}
			status = ur_update_data_fixed (&Sln);
			if (status != UU_SUCCESS) return (status);

			UU_LIST_EMPTY (&snlst);
			UU_LIST_EMPTY (&vclst);
			n = ncl_evolve_bn_crv_on_srf (wbase->srf,wbase->tfmat,&Sln,BPLM,tol,
										&snlst,UU_NULL,&vclst);
			if (n > 2)
			{
				pp = (UM_coord *) UU_LIST_ARRAY (&snlst);
				for (j = 1; j < n-1; j++)
				{
					uu_list_push (&ptlst,&pp[j]);
					npt++;
				}
			}
		}
	}
	uu_list_push (&ptlst,pt0);

	uu_list_push (&nplst,&npt);

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_wat_set_pokgeo (wbase,curpock,ccur,cnxt,c0,tolsq)
**       Map UV-polygon contours to the current base surface, put the result
**       into a boundary structure.
**    PARAMETERS
**       INPUT  :
**          wbase      - base surface data
**          ccur       - current perimeter contour
**          cnxt       - next perimeter contour
**          c0         - stock contour
**          tolsq      - squared tolerance
**       OUTPUT :
**          bound      - contours written into the boundary struct
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_wat_set_pokgeo (wbase,curpock,ccur,cnxt,c0,tol,tolsq)
NCL_w_base *wbase;
ncl_polygon *curpock;
int ccur,cnxt,c0;
UU_REAL tol,tolsq;
{
	int c,nv,npt,status,nb,nc;
	UM_2Dcoord *vtx,*uvs;
	UU_LIST *stock;

	status = UU_SUCCESS;
	nc = curpock->num_contours;
	if (cnxt < nc) nc = cnxt;
	nb = cnxt - ccur;
	if (nb < 1) goto Err;

	UU_LIST_EMPTY (&ptlst);
	UU_LIST_EMPTY (&nplst);
	bound.nb = 0;
	bound.np = UU_NULL;

	if (ccur == c0)
		ncl_wat_set_planeflg (UU_TRUE);
	else
		ncl_wat_set_planeflg (UU_FALSE);

	vtx = (UM_2Dcoord *) UU_LIST_ARRAY (curpock->contour);
	for (c = 0; c < nc && status == UU_SUCCESS; c++)
	{
		nv = abs(curpock->np[c]);
		if (c >= ccur)
		{
			if (nv < 3) goto Err;
			if (c == c0)
			{
				ncl_get_stklst (&stock);
				if (stock == NULLST) goto Err;
				uvs = (UM_2Dcoord *) UU_LIST_ARRAY (stock);
				npt = stock->cur_cnt;
				if (uvs == UU_NULL || npt < 3) goto Err;
				status = S_add_contour (0,wbase,npt,uvs,tol,tolsq);
			}
			else
			{
				status = S_add_contour (1,wbase,nv,vtx,tol,tolsq);
			}
			if (status != UU_SUCCESS) return (status);
		}
		vtx += nv;
	}

	bound.nb = nb;
	bound.np = (int *) UU_LIST_ARRAY (&nplst);
	bound.cvpts = &ptlst;

	ncl_wat_set_boundary (&bound);

	goto Done;

Err:
	if (status == UU_SUCCESS) status = UU_FAILURE;

Done:
	return (status);
}
