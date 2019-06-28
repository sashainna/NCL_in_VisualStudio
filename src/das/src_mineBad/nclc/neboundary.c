/*********************************************************************
**    NAME         :  neboundary.c
**       CONTAINS:  Routines for projecting on trimmed surface boundaries.
**
**           ncl_debug_bndry
**           ncl_debug_shared
**           ncl_pick_bndry
**           tbyini
**           tbdini
**           ncl_comm_bndr_ini
**           ncl_comm_bndr
**           ncl_2sfbnry
**           ncl_copy_bndr
**           ncl_2sfbnry_free
**           ncl_ttcssf
**           cbdist
**           bndprj
**           gtmmuv
**           ncl_cvonsf_find
**           ncl_cvonsf_store
**           ncl_cvonsf_list_init
**           ncl_cvonsf_free
**           norm_to_cvonsf
**           norm_to_cvonsf1
**           norm_to_lnonsf1
**           norm_to_sfatcv
**           ncl_cvonsf_init
**           ncl_fix_cvonsf
**           ncl_cvonsf_init_mult
**           norm_to_sfatcv
**           ncl_norm_to_sfatcv
**           ncl_load_cvonsf
**           ncl_reproj_on_trimsf
**           ncl_2sf_tanto
**           ncl_proj_csds_bndr
**           ncl_proj_on_srf_bndry
**           ncl_common_bndry_proj
**           ncl_get_common_bndry
**
**    COPYRIGHT 1995 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       neboundary.c , 25.2
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 17:12:33
*********************************************************************/

#include "nccs.h"
#include "mdeval.h"
#include "mdrel.h"
#include "mcrv.h"
#include "modef.h"
#include "mdcpln.h"
#include "zsysdep.h"
#include "uminmax.h"
#include "mfort.h"
#include "mgeom.h"
#include "nclfc.h"
#include "ycom.h"
#include "ncl.h"
#include "nclpsmult.h"

extern int NCLX_internal_geom;
/*
... used for finding intersection of boundaries of 2 srf. in TANTO,sf mode
*/
static UM_int4 kk1=0, kk2=0;
static UM_srf_boundary surf_bound[2];
UU_LIST cbxyz, cbnum;
static int NCL_comm_bndr_init1 = 0;
static int NCL_straight;
static UU_REAL Scosq = 0.998;
/*
..... set in ncl_comm_bndr, used in cbdist
*/
static UM_coord p1str,p2str;
static UM_vector vustr;
/*
... used in ncl_ttcssf
*/
int NCL_cpn;
static UU_REAL NCL_tol,NCL_tolsq = 0;
/*
.... used in driving trimmed srf. as trimmed
*/
static UM_2Dcoord NCL_ummx[3];
static UM_2Dcoord NCL_vmmx[3];
/*
... used for CVonSF as PS
*/
static UU_LIST NCL_cvonsf_list;
static int NCL_cvonsf_list_init1 = 0;
struct NCL_cvonsf_rec *NCL_cvonsf[3] = {UU_NULL, UU_NULL, UU_NULL};
/*
..... flag used in norm_to_cvonsf1
*/
static UU_LOGICAL Ibndpt = UU_FALSE;
extern UU_LOGICAL NCL_lv93;

char *uu_malloc();

/*********************************************************************
**    E_FUNCTION     : void ncl_debug_bndry(key,ix)
**       Print surface boundary points.
**    PARAMETERS
**       INPUT  :
**          key - Key of surface entity.
**          ix  - Index of surface in data.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_debug_bndry(key,ix)
UU_KEY_ID key;
int ix;
{
	int i;
	UM_coord *pts;
	char tbuf[80];
	
	sprintf(tbuf,"$$ KEY = %d",key);
	NclxDbgPstr(tbuf);
	pts = (UM_coord *) UU_LIST_ARRAY (surf_bound[ix].cvpts);
	for (i=0;i<surf_bound[ix].np[0]-1;i++)
	{
		sprintf(tbuf,"PT/%lf,%lf,%lf",pts[i][0],pts[i][1],pts[i][2]);
		NclxDbgPstr(tbuf);
		if (um_dcccc(pts[i],pts[i+1]) > 0.001)
		{
			sprintf(tbuf,"LN/%lf,%lf,%lf,%lf,%lf,%lf",
				pts[i][0],pts[i][1],pts[i][2],
				pts[i+1][0],pts[i+1][1],pts[i+1][2]);
			NclxDbgPstr(tbuf);
		}
	}
	sprintf(tbuf,"PT/%lf,%lf,%lf",pts[i][0],pts[i][1],pts[i][2]);
	NclxDbgPstr(tbuf);
}

/*********************************************************************
**    E_FUNCTION     : void ncl_debug_shared()
**       Print shared boundary points.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_debug_shared()
{
	int i;
	UM_coord *pts;
	char tbuf[80];
	
	pts = (UM_coord *) UU_LIST_ARRAY (&cbxyz);
	for (i=0;i<cbxyz.cur_cnt-1;i++)
	{
		if (UM_MAG(pts[i]) > 1.e8 || UM_MAG(pts[i+1]) > 1.e8)
		{
			sprintf(tbuf,"$$ BAD POINT DATA");
			NclxDbgPstr(tbuf);
			continue;
		}
		if (um_dcccc(pts[i],pts[i+1]) > 0.001)
		{
			sprintf(tbuf,"LN/%lf,%lf,%lf,%lf,%lf,%lf",
				pts[i][0],pts[i][1],pts[i][2],
				pts[i+1][0],pts[i+1][1],pts[i+1][2]);
		}
		else
		{
			sprintf(tbuf,"$$ BAD LINE DATA");
			NclxDbgPstr(tbuf);
			sprintf(tbuf,"\tPT/%lf,%lf,%lf",pts[i][0],pts[i][1],pts[i][2]);
			NclxDbgPstr(tbuf);
			sprintf(tbuf,"\tPT/%lf,%lf,%lf",pts[i+1][0],pts[i+1][1],pts[i+1][2]);
		}
		NclxDbgPstr(tbuf);
	}
	if (cbxyz.cur_cnt > 0 && (cbxyz.cur_cnt < 2))
	{
		if (UM_MAG(pts[i]) < 1.e8)
			sprintf(tbuf,"PT/%lf,%lf,%lf",pts[i][0],pts[i][1],pts[i][2]);
		else
			sprintf(tbuf,"$$ BAD POINT DATA");
		NclxDbgPstr(tbuf);
	}
}

/*********************************************************************
**    E_FUNCTION     : int ncl_pick_bndry(sfinx,binx)
**       Modify boundary data so only the points for the desired
**       boundary index are stored.
**    PARAMETERS
**       INPUT  :
**          sfinx - Surface index.
**          binx  - Boundary index.
**       OUTPUT :
**          none
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_pick_bndry(sfinx,binx)
int sfinx,binx;
{
	int i,npts;
	UU_LIST ptlist,uvlist;
	UM_coord *pts,*upts;

	npts = i = 0;
	if (sfinx > 1 || binx >= surf_bound[sfinx].nb) return(UU_FAILURE);
	if (surf_bound[sfinx].nb == 1) return(UU_SUCCESS);
	while(i<binx && i<surf_bound[sfinx].nb) npts += surf_bound[sfinx].np[i++];

	uu_list_init(&ptlist,sizeof(UM_coord),surf_bound[sfinx].np[binx],10);
	uu_list_init(&uvlist,sizeof(UM_coord),surf_bound[sfinx].np[binx],10);
	pts = (UM_coord *)UU_LIST_ARRAY (surf_bound[sfinx].cvpts);
	upts = (UM_coord *)UU_LIST_ARRAY (surf_bound[sfinx].uvpts);
	for (i=0;i<surf_bound[sfinx].np[binx];i++)
	{
		uu_list_push(&ptlist,&(pts[i+npts]));
		uu_list_push(&uvlist,&(upts[i+npts]));
	}

	UU_LIST_EMPTY(surf_bound[sfinx].cvpts);
	UU_LIST_EMPTY(surf_bound[sfinx].uvpts);
	uu_list_push_list(surf_bound[sfinx].cvpts,&ptlist);
	uu_list_push_list(surf_bound[sfinx].uvpts,&uvlist);
	surf_bound[sfinx].np[0] = surf_bound[sfinx].np[binx];
	for (i=1;i<surf_bound[sfinx].nb;i++) surf_bound[sfinx].np[i] = 0;
	surf_bound[sfinx].nb = 1;
	upts = (UM_coord *)UU_LIST_ARRAY (surf_bound[sfinx].uvpts);
	um_cshape_dirchk (upts,1,&(surf_bound[sfinx].np[0]),surf_bound[sfinx].ummx,
		surf_bound[sfinx].vmmx);
	uu_list_free(&ptlist);
	uu_list_free(&uvlist);
	return(UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : int tbyini (nclkey,tol,bnix)
**       Initialization routine.
**    PARAMETERS
**       INPUT  :
**          nclkey   - Key of trimmed surface entity.
**          tol      - tolerance for curve evolver
**       OUTPUT :
**          none
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int tbyini (nclkey, tol, bnix)
UM_int4 *nclkey;
int bnix;
UU_REAL tol;
{
	int status,ix;
	struct NCL_fixed_databag sf, *psf;
/*
... aak 13-feb-1998: modified this routine a lot, for now the boundary is
... retrieved/stored in the Unibase; see the previous version for differences
*/
	ix = bnix;

	psf = &sf;
	psf->key = *nclkey;
	if (ncl_retrieve_data_fixed (psf) != UU_SUCCESS)
		return (UU_FAILURE);

	ncl_set_boundary_toler (tol);
	status = ncl_get_boundary (WHOLE_BOUNDARY_LIST,psf,&surf_bound[ix]);

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_comm_bndr_ini (key1,key2,tol)
**       Returns common boundary points of two surfaces.
**       May require preceding call of tbyini.
**    PARAMETERS
**       INPUT  :
**       OUTPUT :
**          none
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_comm_bndr_ini (key1,key2,tol)
UM_int4 *key1, *key2;
UU_REAL tol;
{
	int n, status = UU_SUCCESS;
/*
...we assume that tbyini for 'key1' may be
...called before
*/
	n = 0;
	if (*key1 > 0) status = tbyini (key1, tol, n);
	if (status == UU_SUCCESS)
	{
		n = 1;
		if (*key2 > 0) status = tbyini (key2, tol, n);
		if (status == UU_SUCCESS)
		{
			kk1 = *key1; kk2 = *key2;
	 		uu_list_init (&cbxyz, sizeof(UM_coord), 100, 100);
			uu_list_init (&cbnum, sizeof(int), 100, 100);
			n = 0;
			uu_list_push (&cbnum,&n);
			NCL_comm_bndr_init1 = 1;
		}
	}
	NCL_straight = 0;

	return(status);
}

/*********************************************************************
**    E_FUNCTION     : Sget_strseg(ve0)
**       Get endpoints of the common boundary segment.
*********************************************************************/
static void Sget_strseg(ve0)
UM_vector ve0;
{
	int i1,i2,npt,i,j;
	UM_coord *xyz;
	UU_REAL dot,d1,d2;

/*
..... recalculate the 'straight' comm_bndr as one segment. it is not
..... currently used for a comm_bndr projection;it is used in cbdist
..... to decide whether to accept an ending position.
*/
	npt = cbxyz.cur_cnt;
	xyz = (UM_coord *) UU_LIST_ARRAY (&cbxyz);
	d1 = d2 = UM_DOT (xyz[0],ve0);
	i1 = i2 = 0;
	for (i = 1; i < npt; i++)
	{
		dot = UM_DOT (xyz[i],ve0);
		if (dot < d1)
		{
			d1 = dot;
			i1 = i;
		}
		if (dot > d2)
		{
			d2 = dot;
			i2 = i;
		}
	}
	for (j = 0; j < 3; j++)
	{
		vustr[j] = ve0[j];
		p1str[j] = xyz[i1][j];
		p2str[j] = xyz[i2][j];
	}
}

/*********************************************************************
**    E_FUNCTION     : Sget_strvec (sf1,sf2,s1,s2,xyz,npt,ve0,tolsq)
**       Determine if the common boundary points of two surfaces belong to a
**       single segment.
**       INPUT  :
**          tolsq - squared tolerance
**       OUTPUT :
**          ve0   - unit vector along the segment.
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int Sget_strvec (sf1,sf2,s1,s2,xyz,npt,ve0,tolsq)
struct NCL_fixed_databag *sf1,*sf2;
NCL_surf_struct *s1,*s2;
UM_coord *xyz;
int npt;
UM_vector ve0;
UU_REAL tolsq;
{
	int i,j,ix,status;
	UU_REAL dot,d1,d2,co,len,len1;
	UM_vector ve1;

	if (npt < 2) return (UU_FAILURE);
			
	status = ncl_proj_on_srf_bndry (xyz[0],sf1,&surf_bound[0],s1,UU_TRUE);
	if (status == UU_SUCCESS)
		status = ncl_proj_on_srf_bndry(xyz[0],sf2,&surf_bound[1],s2,UU_TRUE);
	if (status == UU_SUCCESS)
	{
		d1 = UM_DOT (s1->normal,s1->normal);
		d2 = UM_DOT (s2->normal,s2->normal);
		if (d1 > 1.e-6 && d2 > 1.e-6)
		{
			dot = UM_DOT (s1->normal,s2->normal);
			co = sqrt ((dot*dot)/(d1*d2));
			if ((1.0 - co) < 0.001)
			{
				NCL_straight = 1;
			}
		}
	}

	if (NCL_straight == 1)
	{
		len = 0.;
		ix = 0;
/*
..... find the largest segment with xyz[0] at one end
*/
		for (i = 1; i < npt; i++)
		{
			len1 = UM_SQDIS (xyz[i],xyz[0]);
			if (len1 > len)
			{
				ix = i; len = len1;
			}
		}

		um_nullvc (ve0);
		if (len >= tolsq)
		{
			len = sqrt(len);
			for (j = 0; j < 3; j++)
				ve0[j] = (xyz[ix][j] - xyz[0][j])/len;
/*
..... if all points are no farther than tolerance from the big segment,
..... the common boundary is straight
*/
			for (i = 1; i < npt && NCL_straight==1; i++)
			{
				if (i == ix) continue;
				um_vcmnvc (xyz[i],xyz[0], ve1);
				len1 = UM_DOT (ve1,ve1);
				dot = UM_DOT (ve0,ve1);
				len = len1 - dot*dot;
				if (len > tolsq)
					NCL_straight = 0;
			}
		}
	}

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : S_retry_match (sf1,sf2,s1,s2,xyz,npt,len,tol)
**       Determine if the current one-piece common boundary is not quite good
**       and we should attempt to recalculate it in S_match
**       INPUT  :
**          sf1   - first surface
**          sf2   - second surface
**          s1,s2 - structs to use for ncl_proj_on_srf_bndry calls
**          xyz - common boundary 
**          npt - number of xyz-points
**          tol   - tolerance
**       OUTPUT :
**          len   - total length
**    RETURNS      : UU_TRUE iff should be reattempted
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UU_LOGICAL S_retry_match (sf1,sf2,s1,s2,xyz,npt,len,tol)
struct NCL_fixed_databag *sf1,*sf2;
NCL_surf_struct *s1,*s2;
UM_coord *xyz;
int npt;
UU_REAL *len,tol;
{
	int i,status;
	UU_REAL dot,d1,d2,co,len1,csmin;

	csmin = 1;
/*
..... calculate the min squared cos of the angle between surface normals at xyz-points
*/
	for (i = 0; i < npt; i++)
	{
		status = ncl_proj_on_srf_bndry (xyz[i],sf1,&surf_bound[0],s1,UU_TRUE);
		if (status == UU_SUCCESS)
		status = ncl_proj_on_srf_bndry (xyz[i],sf2,&surf_bound[1],s2,UU_TRUE);
		if (status == UU_SUCCESS)
		{
			d1 = UM_DOT (s1->normal,s1->normal);
			d2 = UM_DOT (s2->normal,s2->normal);
			if (d1 > 1.e-6 && d2 > 1.e-6)
			{
				dot = UM_DOT (s1->normal,s2->normal);
				co = (dot*dot)/(d1*d2);

				if (co < csmin) csmin = co;
			}
		}

		if (i > 0)
		{
			len1 = um_dcccc (xyz[i],xyz[i-1]);
			*len += len1;
		}
	}
/*
..... if the common boundary is not long, and some surface normals are at not
..... parallel - try to recalculate
*/
	if (csmin < Scosq && *len < 100.*tol)
		return (UU_TRUE);
	else
		return (UU_FALSE);
}

/*********************************************************************
**    I_FUNCTION     : Smatch_pt (p,asw,isf,dir,pte,u,v,tolsq)
**       Project a point onto a surface, determine if the projection is within
**       tolerance to the original point and within surface boundary.
**        
**       INPUT  :
**          ltrim - trimsf flag
**          pte   - point to consider
**          asw   - surface 'word' - for sfpt calls
**          isf   - surface index
**          dir   - surface normal direction, zero if uninitialized
**          u,v   - initial UV
**          p     - surface boundary
**          tolsq - squared tolerance
**       OUTPUT :
**          u,v     - projection point UV
**          spt     - projection point
**    RETURNS      : UU_TRUE if point matches
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UU_LOGICAL Smatch_pt (ltrim1,p,asw1,isf1,dir1,u1,v1,asw2,isf2,dir2,pt2,uv2,spt1,tolsq)
UU_LOGICAL ltrim1;
UM_srf_boundary *p;
UM_real8 *asw1,*asw2;
UM_real4 *dir1,*dir2;
UM_int2 *isf1,*isf2;
UM_real4 *u1,*v1;
UM_coord pt2,uv2,spt1;
UU_REAL tolsq;
{
	int k,insf;
	UM_real8 pe[3];
	UM_real4 ss[7];
	UM_real4 u2,v2;
	UU_REAL dis,d1,d2,dot,co;
	UM_coord uvs,*uvptr;
	UM_vector snorm1,snorm2;
	UU_LOGICAL lmatch = UU_FALSE;


	for (k = 0; k < 3; k++) pe[k] = pt2[k];
	sfpt1 (asw1,pe,isf1,dir1,u1,v1,ss);
	for (k = 0; k < 3; k++)
	{
		spt1[k] = ss[4+k];
		snorm1[k] = ss[k];
	}
	d1 = UM_DOT (snorm1,snorm1);
	if (d1 < 1.e-6) return (UU_FALSE);

	dis = UM_SQDIS(pt2,spt1);
	if (dis < tolsq)
	{
		if (ltrim1)
		{
			uvs[0] = *u1; uvs[1] = *v1; uvs[2] = 0;
			uvptr = (UM_coord *) UU_LIST_ARRAY (p->uvpts);
			insf = um_cshape_inschk (uvptr,p->nb,p->np,uvs,p->ummx,p->vmmx);
			lmatch = (insf >= 0);
		}
		else
			lmatch = UU_TRUE;
	}

	if (lmatch)
	{
		u2 = uv2[0]; v2 = uv2[1];
		sfpt1 (asw2,pe,isf2,dir2,&u2,&v2,ss);
		for (k = 0; k < 3; k++)
		{
			snorm2[k] = ss[k];
		}
		d2 = UM_DOT (snorm2,snorm2);
		if (d2 < 1.e-6) return (UU_FALSE);
	
		dot = UM_DOT (snorm1,snorm2);
		co = (dot*dot)/(d1*d2);

		lmatch = (co > Scosq);
	}

	return (lmatch);
}

/*********************************************************************
**    I_FUNCTION     : S_copy_bndr (bndr,cvnew,uvnew,tolsq)
**       Copy outer boundary of a surface, inserting points in long segments
**       INPUT  :
**          bndr         - boundary struct surface flag
**          uvnew,cvnew  - initialized lists to copy to.
**          tolsq        - squared tolerance
**       OUTPUT :
**          uvnew,cvnew  - copy of the outer boundary.
**    RETURNS      : number of points
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_copy_bndr (bndr,cvnew,uvnew,tolsq)
UM_srf_boundary *bndr;
UU_LIST *uvnew,*cvnew;
UU_REAL tolsq;
{
	int i,j,npt,ntot,Nins;
	UM_coord *pts,*uvs;

	UU_REAL Dlarge,Dins,di;
	UM_vector vci,vwi;
	UM_coord pti,uvi;

	Dlarge = 4.e4*tolsq;
	Nins = 12;
	Dins = 1./Nins;

	pts = (UM_coord *) UU_LIST_ARRAY (bndr->cvpts);
	uvs = (UM_coord *) UU_LIST_ARRAY (bndr->uvpts);
	npt = ntot = bndr->np[0] - 1;

	for (i = 0; i < npt; i++)
	{
		uu_list_push (uvnew,uvs[i]);
		uu_list_push (cvnew,pts[i]);
		um_vcmnvc (pts[i+1],pts[i],vci);
		di = UM_DOT (vci,vci);
		if (di > Dlarge)
		{
			um_vctovc (pts[i],pti);
			um_vctovc (uvs[i],uvi);
			uvi[2] = -1;
			um_vctmsc (vci,Dins,vci);
			um_vcmnvc (uvs[i+1],uvs[i],vwi);
			um_vctmsc (vwi,Dins,vwi);

			for (j = 1; j < Nins; j++)
			{
				um_vcplvc (pti,vci,pti);
				uu_list_push (cvnew,pti);
				um_vcplvc (uvi,vwi,uvi);
				uu_list_push (uvnew,uvi);
				ntot++;
			}
		}
	}
	return (ntot);
}

/*********************************************************************
**    I_FUNCTION     : S_remove_inserted (cvnew,uvnew,npt,nxyz0,ninp0,nn)
**       Remove inserted points from the common boundary
**       INPUT  :
**          npt          - starting index of new surface boundary points
**          uvnew,cvnew  - new surface boundary points.
**          nxyz0        - starting index of new common boundary in cbxyz
**          ninp0        - starting index of new common boundary in cbnum
**          nn           - current number of new common boundary points
**       OUTPUT :
**          uvnew,cvnew  - updated lists.
**          nn           - updated number of new common boundary points
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_remove_inserted (cvnew,uvnew,npt,nxyz0,ninp0,nn)
UU_LIST *uvnew,*cvnew;
int npt,nxyz0,ninp0,*nn;
{
	int i,i0,idel,nn1,*inpt;
	UM_coord *uvs;

	nn1 = *nn;
	uvs = (UM_coord *) UU_LIST_ARRAY (uvnew);
	uvs += npt;

	i0 = idel = 0;
	for (i = 1; i < nn1-1; i++)
	{
		if (uvs[i][2] == -1)
		{
			if (i0 == 0) i0 = i;
			idel++;
		}
		else if (idel > 0)
			break;
	}

	if (i0 > 0 && idel > 0)
	{
		uu_list_delete (uvnew,npt+i0,idel);
		uu_list_delete (cvnew,npt+i0,idel);
		uu_list_delete (&cbxyz,nxyz0+i0,idel);
		uvs = (UM_coord *) UU_LIST_ARRAY (uvnew);
		uvs += npt;
		nn1 -= idel;
		inpt = (int *) UU_LIST_ARRAY (&cbnum);
		inpt[ninp0] = nn1;
	}

	for (i = 0; i < nn1; i++)
	{
		if (uvs[i][2] == -1) uvs[i][2] = 0;
	}

	*nn = nn1;
}

/*********************************************************************
**    I_FUNCTION     : Smatch_bndry1 (ix,ltrim1,asw1,isf1,dir1,asw2,isf2,dir2,
**                                   uvnew,cvnew,len,tolsq)
**       Try to match boundary points of a surface (sf2) to the inside of
**       another surface (sf1)
**       INPUT  :
**          ix      - which surface flag
**          ltrim1  - trimsf flag of sf1
**          asw1    - surface 'word' of sf1
**          isf1    - surface index of sf1
**          dir1    - surface normal direction (zero if uninitialized) of sf1
**          asw2    - surface 'word' of sf2
**          isf2    - surface index of sf2
**          dir2    - surface normal direction (zero if uninitialized) of sf2
**          len     - total length to exceed
**          uvnew,cvnew  - initialized lists to use.
**          tolsq - squared tolerance
**       OUTPUT :
**          cbxyz,cbnum   - "common boundary" lists.
**    RETURNS      :
**         number of common boundary components; 0 if none found
**    SIDE EFFECTS : if successful, the "common boundary" replaces the real
**                   boundary of one of the surfaces. 
**    WARNINGS     : none
*********************************************************************/
static int Smatch_bndry1 (ix,ltrim1,asw1,isf1,dir1,asw2,isf2,dir2,uvnew,cvnew,
						  len,tolsq)
int ix;
UU_LOGICAL ltrim1;
UM_real8 *asw1,*asw2;
UM_real4 *dir1,*dir2;
UM_int2 *isf1,*isf2;
UU_REAL len,tolsq;
UU_LIST *uvnew,*cvnew;
{
	int i,i0,i1,npt,nn,nc,ix1,*inpt,ninp0,nxyz0;
	UM_srf_boundary *bndr1,*bndr2;
	UM_real4 u,v;
	UM_coord *pts,*uvs;

	UU_LOGICAL lmatch,laccept;

	UU_REAL di,dtot;
	UM_coord uvi,spt;

	nc = 0;

	ix1 = 1 - ix;
	bndr1 = &surf_bound[ix];
	bndr2 = &surf_bound[ix1];

	u = (bndr1->ummx[0][0] + bndr1->ummx[0][1])/2;
	v = (bndr1->vmmx[0][0] + bndr1->vmmx[0][1])/2;

	UU_LIST_EMPTY (uvnew);
	UU_LIST_EMPTY (cvnew);

	npt = S_copy_bndr (bndr2,cvnew,uvnew,tolsq);

	pts = (UM_coord *) UU_LIST_ARRAY (cvnew);
	uvs = (UM_coord *) UU_LIST_ARRAY (uvnew);
/*
..... find a point not on the common boundary - as a proper start
*/
	i0 = -1;
	for (i = 0; i < npt; i++)
	{
		if (!Smatch_pt (ltrim1,bndr1,asw1,isf1,dir1,&u,&v,
									asw2,isf2,dir2,pts[i],uvs[i],spt,tolsq))
		{
			i0 = i; break;
		}
	}
	if (i0 < 0) goto done;

	if (len > 0)
	{
/*
..... if some common boundary exists we'll restore it if this attempt fails
*/
		nxyz0 = UU_LIST_LENGTH (&cbxyz);
		ninp0 = UU_LIST_LENGTH (&cbnum);
	}
	else
	{
		UU_LIST_EMPTY (&cbxyz);
		UU_LIST_EMPTY (&cbnum);
		nxyz0 = ninp0 = 0;
	}

	lmatch = UU_FALSE;
	nn = 0;

	for (i = 1; i <= npt; i++)
	{
		i1 = (i0 + i)%npt; 
		if (i1 != i0 && Smatch_pt (ltrim1,bndr1,asw1,isf1,dir1,&u,&v,
									asw2,isf2,dir2,pts[i1],uvs[i1],spt,tolsq))
		{
			uu_list_push (cvnew,spt);

			uvi[2] = 0;
			if (uvs[i1][2] < 0) uvi[2] = -1;
			uvi[0] = u; uvi[1] = v;
			uu_list_push (uvnew,uvi);

			um_middlept (pts[i1],spt,spt);
			uu_list_push (&cbxyz,spt);
			nn++;
			lmatch = UU_TRUE;
		}
		else
		{
			if (lmatch)
			{
		 		if (nn == 1)
				{
					uvnew->cur_cnt--;
					cvnew->cur_cnt--;
					cbxyz.cur_cnt--;
				}
				else
				{
					uu_list_push (&cbnum,&nn);
					nc++;
					break;
				}
				nn = 0;
				lmatch = UU_FALSE;
			}
		}
	}

	if (nc > 0)
	{
		S_remove_inserted (cvnew,uvnew,npt,nxyz0,ninp0,&nn);

		pts = (UM_coord *) UU_LIST_ARRAY (cvnew);
		pts += npt;
		uvs = (UM_coord *) UU_LIST_ARRAY (uvnew);
		uvs += npt;

		if (len > 0)
		{
			laccept = UU_FALSE;
			dtot = 0;
			for (i = 0; i < nn-1 && !laccept; i++)
			{
				di = um_dcccc (pts[i],pts[i+1]);
				dtot += di;
				laccept = (dtot > len);
			}
			if (!laccept)
			{
/*
..... restore the existing common boundary if this attempt is not accepted
*/
				UU_LIST_LENGTH (&cbxyz) = nxyz0;
				UU_LIST_LENGTH (&cbnum) = ninp0;
				nc = 0;
				goto done;
			}
/*
..... delete the existing common boundary if this attempt is good
*/
			uu_list_delete (&cbxyz,0,nxyz0);
			uu_list_delete (&cbnum,0,ninp0);
		}

		inpt = (int *) UU_LIST_ARRAY (&cbnum);
		bndr1->nb = 1;
		bndr1->np[0] = inpt[0];

		UU_LIST_EMPTY (bndr1->uvpts);
		uu_list_push_multiple (bndr1->uvpts,nn,uvs);

		UU_LIST_EMPTY (bndr1->cvpts);
		uu_list_push_multiple (bndr1->cvpts,nn,pts);

		nn = 0;
		uu_list_push (&cbnum,&nn);
	}

done:
	return (nc);
}

/*********************************************************************
**    I_FUNCTION     : Smatch_bndry (ltrim1,ltrim2,len,tol)
**       Try to match boundary points of one of two surfaces to the inside
**       of the other surface.
**       INPUT  :
**          ltrim1,ltrim2 - trimsf flags for surfaces
**          len - current total length to exceed
**          tol - tolerance
**       OUTPUT :
**          cbxyz,cbnum   - "common boundary" lists.
**    RETURNS      :
**         number of common boundary components; 0 if none found
**    SIDE EFFECTS : if successful, the "common boundary" replaces the real
**                   boundary of one of the surfaces. 
**    WARNINGS     : none
*********************************************************************/
static int Smatch_bndry (ltrim1,ltrim2,len,tol)
UU_LOGICAL ltrim1,ltrim2;
UU_REAL len,tol;
{
	int ix,nc;
	UM_int2 i2,ier2,ifl2,itype,isub,isf1,isf2;
	UU_LIST uvnew,cvnew;
	UU_REAL tolsq = tol*tol;
	UM_real8 asw1,asw2;
	UM_real4 dir1,dir2;

/*
..... save current error, reset error before going into Smatch_bndry1
*/
	i2 = 2;
	getifl (&i2,&ifl2);
	ier2 = 0;
	setifl (&i2,&ier2);

	uu_list_init (&uvnew, sizeof(UM_coord), 0, 100);
	uu_list_init (&cvnew, sizeof(UM_coord), 0, 100);

	tolsq = 0.25*tol*tol;

	dir1 = dir2 = 0; /* initialize the surface */
	itype = 9; /* surface */
	isub = 1;
	isf1 = 2;
	ptdsc3(&kk1,&isub,&itype,&asw1);
	isf2 = 3;
	ptdsc3(&kk2,&isub,&itype,&asw2);

	ix = 0;
	nc = Smatch_bndry1 (ix,ltrim1,&asw1,&isf1,&dir1,&asw2,&isf2,&dir2,&uvnew,&cvnew,len,tolsq);
	if (nc <= 0)
	{
		ix = 1;
/*
..... reset error before going into Smatch_bndry1 again
*/
		ier2 = 0;
		setifl (&i2,&ier2);
		nc = Smatch_bndry1 (ix,ltrim2,&asw2,&isf2,&dir2,&asw1,&isf1,&dir1,&uvnew,&cvnew,len,tolsq);
	}
		
	uu_list_free (&uvnew);
	uu_list_free (&cvnew);

	if (nc > 0)
	{
		getifl (&i2,&ier2);
		if (ier2 != 0) nc = 0;
	}
/*
..... reset error to the original value
*/
	setifl (&i2,&ifl2);

	return (nc);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_comm_bndr (tolm)
**       Returns common boundary points of two surfaces.
**       Needs preceding call of ncl_comm_bndr_ini
**    PARAMETERS
**       INPUT  :
**          tolm   - tolerance (max distance between 2 curves to
**                   assume that they match)
**          skipfl - Skip extra matching steps flag.
**       OUTPUT :
**          none
**    RETURNS      :
**         number of curve segments on the common edge
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_comm_bndr (tolm,skipfl)
UU_REAL tolm;
UU_LOGICAL skipfl;
{
	UM_coord *pts1, *pts2, *xyz, tpt;
	int nc,*inpt,npt,ix,i,j,nc1,ix1,ix2,ix3,lst;
	UU_REAL len,len1,d1,d2,lmax,dd0,dd1,dmin,dmax;
	int status;
	int click = 0;
	UM_vector ve0;
	UU_REAL tolsq = tolm*tolm;
	struct NCL_fixed_databag sf1,sf2;
	NCL_surf_struct s1,s2;
	UU_LOGICAL lsmatched;
	UU_LOGICAL ltrim1,ltrim2;

	pts1 = (UM_coord *) UU_LIST_ARRAY (surf_bound[0].cvpts);
	pts2 = (UM_coord *) UU_LIST_ARRAY (surf_bound[1].cvpts);

	sf1.key = kk1; sf2.key = kk2;
	status = ncl_retrieve_data_fixed (&sf1);
	if (status == UU_SUCCESS)
		status = ncl_retrieve_data_fixed (&sf2);
	if (status != UU_SUCCESS) goto Done;

	ltrim1 = (ncl_itsa_trimsrf(&sf1));
	ltrim2 = (ncl_itsa_trimsrf(&sf2));

Retry:;
	lsmatched = UU_FALSE;
	um_match_bndry (pts1,surf_bound[0].np,pts2,surf_bound[1].np,tolm,
		&cbnum,&cbxyz);
	nc = 0;
	inpt = (int *) UU_LIST_ARRAY (&cbnum);
	while (inpt[nc] > 0) nc++;

	len = 0;
		
	if (nc < 1 && click == 0 && !skipfl)
	{
		nc = Smatch_bndry (ltrim1,ltrim2,lmax,tolm);
		lsmatched = UU_TRUE;
		if (nc > 0) inpt = (int *) UU_LIST_ARRAY (&cbnum);
	}
	if (nc < 1) goto Done;

	npt = cbxyz.cur_cnt;
	if (npt < 2) goto Done;
	xyz = (UM_coord *) UU_LIST_ARRAY (&cbxyz);


/*
..... qatest/catia/NCL306.cpp: the flag Ibndpt is used in norm_to_cvonsf1,
..... it means ncl_proj_on_srf_bndry can project onto boundary points
*/
	if (!NCL_lv93) Ibndpt = UU_TRUE;

	if (nc == 1 && click == 0 && !lsmatched && !skipfl)
	{
		if (S_retry_match (&sf1,&sf2,&s1,&s2,xyz,npt,&len,tolm))
		{
			nc1 = Smatch_bndry (ltrim1,ltrim2,9*len,tolm);
			lsmatched = UU_TRUE;
			if (nc1 > 0)
			{
				nc = nc1;
				inpt = (int *) UU_LIST_ARRAY (&cbnum);
				npt = cbxyz.cur_cnt;
				if (npt < 2) goto Done;
				xyz = (UM_coord *) UU_LIST_ARRAY (&cbxyz);
			}
		}
	}

	if (npt == 2)
	{
/*
..... if just 2 points: divide tol by 2 if the segment is too short;
..... set NCL_straight if normals at endpoints match
*/
		if (nc == 1)
		{
			len = UM_SQDIS (xyz[0],xyz[1]);
			if (len < tolsq && len > 0.25*tolsq)
			{
				tolm *= 0.5; tolsq = tolm*tolm;
			}
			status = Sget_strvec (&sf1,&sf2,&s1,&s2,xyz,npt,ve0,tolsq);
		}
		goto Done;
	}

	lmax = 0.; /* this will be the longest segment of the whole comm_bndr */
/*
..... the following for-loop finds the max segment length along the
..... common boundary; it also deletes 'queer' points far from one of the
..... surfaces (or both) if there are 'good' segments much closer to both
..... surfaces. the latter is intended as a temporary  fix - need to look at
..... um_match_bndry. CATIA NCL 215
*/
	for (i=0, ix=0; i<nc; i++)
	{
		status = ncl_proj_on_srf_bndry (xyz[0],&sf1,&surf_bound[0],&s1,UU_TRUE);
		if (status != UU_SUCCESS) break;
		status = ncl_proj_on_srf_bndry (xyz[0],&sf2,&surf_bound[1],&s2,UU_TRUE);
		if (status != UU_SUCCESS) break;
		d1 = UM_SQDIS (xyz[0],s1.pt); d2 = UM_SQDIS (xyz[0],s2.pt);
		dd0 = MAX2(d1,d2);

		dmin = 1.e20; dmax = dd0;

		len = 0.; /* this will be the longest segment of the i-th component */
/*
..... go over the segments of the i-th comm_bndr component; find the longest,
..... also find the best match between a segment and surface boundaries.
*/
		for (j=1; j<inpt[i]; j++)
		{
			status = ncl_proj_on_srf_bndry(xyz[j],&sf1,&surf_bound[0],&s1,UU_TRUE);
			if (status != UU_SUCCESS) break;
			status = ncl_proj_on_srf_bndry(xyz[j],&sf2,&surf_bound[1],&s2,UU_TRUE);
			if (status != UU_SUCCESS) break;
			d1 = UM_SQDIS (xyz[j],s1.pt); d2 = UM_SQDIS (xyz[j],s2.pt);
			dd1 = MAX2(d1,d2);
			if (dd1 > dmax) dmax = dd1;
			len1 = UM_SQDIS (xyz[j-1],xyz[j]);
			if (len1 > len) len = len1;
/*
..... consider only long enough segments; for such we estimate the match
..... as the maximum distance from a segment endpoint to a surface boundary
*/
			if (len1 >= tolsq)
			{
				d1 = MAX2(dd0,dd1);
				if (d1 < dmin) dmin = d1;
			}
			dd0 = dd1;
		}
		if (status != UU_SUCCESS) break;
		if (len > lmax) lmax = len;

		dmin = MAX2 (dmin,0.0025*tolsq);
		dmin *= 4.;
/*
..... if the best match (the smallest segment-to-boundaries distance for
..... this component) is quite good (less than half the tolerance), we delete
..... segments for which the match is a lot worse.
*/
		if (dmin < tolsq && dmax > dmin)
		{
			for (j=0; j<inpt[i]; j++)
			{
				ncl_proj_on_srf_bndry (xyz[j],&sf1,&surf_bound[0],&s1,UU_TRUE);
				ncl_proj_on_srf_bndry (xyz[j],&sf2,&surf_bound[1],&s2,UU_TRUE);
				d1 = UM_SQDIS (xyz[j],s1.pt); d2 = UM_SQDIS (xyz[j],s2.pt);
				dd0 = MAX2(d1,d2);
				if (dd0 > dmin)
				{
					uu_list_delete(&cbxyz,ix+j,1);
					j--; inpt[i]--;
				}
			}
		}
		xyz += inpt[i];
		ix += inpt[i]; /* ix is the start of the i-th component in cbxyz */
	}
/*
.....Get rid of short segment at beginning or end
.....of boundary list.
.....It is most likely an unnecessary tail at the
.....edge of a common boundary
.....Pocket/edge_loop.pp
.....Bobby - 07/14/14
*/
	if (nc == 2 && inpt[0] == 2 || inpt[1] == 2)
	{
		lst = inpt[0] + inpt[1] - 1;
		if (inpt[0] == 2)
		{
			ix1 = 0; ix2 = 1; ix3 = lst;
		}
		else
		{
			ix1 = lst; ix2 = lst-1; ix3 = 0;
		}

		xyz = (UM_coord *)UU_LIST_ARRAY(&cbxyz);
		if (UM_SQDIS(xyz[ix1],xyz[ix2]) < tolsq*3 &&
			UM_SQDIS(xyz[ix1],xyz[ix3]) < UM_DFUZZ)
		{
			uu_list_delete(&cbxyz,ix1,1);
			if (ix1 == 0)
			{
				uu_list_delete(&cbxyz,ix1,1);
				uu_list_delete(&cbnum,0,1);
			}
			else
			{
				uu_list_delete(&cbxyz,ix2,1);
				uu_list_delete(&cbnum,lst,1);
			}
			xyz = (UM_coord *)UU_LIST_ARRAY(&cbxyz);
			inpt = (int *)UU_LIST_ARRAY(&cbnum);
		}
	}

/*
..... in ncl_ttcssf only segments longer than NCL_tol are considered; so
..... if none are longer try to divide tolm (and NCL_tol) by 2 and recalc
..... the common boundary
*/
	if (lmax < tolsq)
	{
		if (lmax > 0.25*tolsq && click == 0)
		{
			click = 1;
			tolm *= 0.5; tolsq = tolm*tolm;
			cbxyz.cur_cnt = cbnum.cur_cnt = 0;
			nc = 0;
			uu_list_push (&cbnum,&nc);
			goto Retry;
		}
		else
		{
			nc = 0; goto Done;
		}
	}
/*
..... Determine whether the common boundary is on a straight line within
..... tolerance - to use in ncl_ttcssf. The flag NCL_straight is set to zero
..... in ncl_comm_bndr_ini, if the common boundary is on a straight line,
..... the flag is set to one here.
*/
	xyz = (UM_coord *) UU_LIST_ARRAY (&cbxyz);
	npt = cbxyz.cur_cnt;
	status = Sget_strvec (&sf1,&sf2,&s1,&s2,xyz,npt,ve0,tolsq);

Done:;
	Ibndpt = UU_FALSE;

	if (NCL_straight == 1 && status == UU_SUCCESS)
		Sget_strseg (ve0);

	NCL_tol = tolm;
	return (nc);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_copy_bnry ()
**			Returns the first surface boundary as the common
**			boundary points of two surfaces.
**			Needs preceding call of ncl_comm_bndr_ini
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      :
**         number of curve segments
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_copy_bndr ()
{
	UM_coord *pts1;
	int i, nc, *inpt;

	pts1 = (UM_coord *) UU_LIST_ARRAY (surf_bound[0].cvpts);
	nc   = UU_LIST_LENGTH(surf_bound[0].cvpts);

	for (i=0; i<nc; i++)
	{
		uu_list_push (&cbxyz,pts1); pts1++;
	}
	inpt = (int *) surf_bound[0].np;
	nc   = 0;
	while (inpt[nc] > 0) nc++;
	cbnum.cur_cnt = 0;
	for (i=0; i<nc; i++)
	{
		uu_list_push (&cbnum,inpt); inpt++;
	}
	i = 0;
	uu_list_push (&cbnum,&i);

	inpt = (int *) UU_LIST_ARRAY (&cbnum);

	return(nc);
}

/*********************************************************************
**    E_FUNCTION     : ncl_2sfbnry (key1,key2,told,tolm)
**       Returns common boundary points of two surfaces.
**    PARAMETERS
**       INPUT  :
**       OUTPUT :
**          none
**    RETURNS      :
**         UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_2sfbnry (key1,key2,told,tolm,ierr)
UM_int4 *key1, *key2;
UM_real8 *told, *tolm;
UM_int2 *ierr;
{
	UU_REAL t;
	int status;
	UM_int2 idx = 169;
	UM_real8 ver;

	*ierr = 0;
	getsc(&idx, &ver);
	NCL_lv93 = (ver < 9.349);

	if (NCL_comm_bndr_init1 == 1 && kk1 == *key1 && kk2 == *key2)
		return (0);

	*ierr = 466;
	t = *told;
	status = ncl_comm_bndr_ini (key1,key2,t);
	if (status == UU_SUCCESS)
	{
		t = *tolm;
		if (*key1 != *key2)
		{
			NCL_cpn = ncl_comm_bndr (t,UU_FALSE);
		}
		else
		{
			NCL_cpn = ncl_copy_bndr ();
			NCL_tol = *tolm;
		}
		if (NCL_cpn > 0 || NCL_lv93) *ierr = 0;
		getsct (&t);
		NCL_tolsq = t*t;
	}

	return (0);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_2sfbnry_free ()
**       Free memory allocated for common boundary of 2 surfaces.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_2sfbnry_free ()
{
   int status = UU_SUCCESS,i;

	uu_list_free (&cbxyz);
	uu_list_free (&cbnum);
	NCL_cpn = 0;

	for (i=0;i<2;i++) um_free_boundary (&surf_bound[i]);

	NCL_comm_bndr_init1 = 0;
	kk1 = kk2 = 0;
	NCL_tolsq = 0;

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_ttcssf (pext0, dis, pt, ve, ierr, fwd)
**       Calculate nearpt at ds & cs tangency.
**    PARAMETERS
**       INPUT  :
**          pext0 - Tool end point
**       OUTPUT :
**          dis       - Distance from tool end point to tangency.
**          pt        - Point of tangency.
**          ve        - Slope of boundary at point of tangency
**          ierr      - 1 iff error else 0
**    RETURNS      :
**         UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_ttcssf (pext0, dis, pt, ve, ierr, fwd)
UM_real8 pext0[], *dis, pt[], ve[], fwd[];
UM_int2 *ierr;
{
	UM_coord *phld,*p2hld,*p1,*p2,p3,pext;
	UM_vector v1,v2,vu,v1hld,v2hld,tfwd;
	UU_REAL d1, d2, d3, u, co;
	UU_REAL d1hld = 1.0e9, d2hld = 1.0e9;
	int i, j, *knpt;
	UM_int2 type;
	UU_LOGICAL lgood = UU_FALSE, lgood2 = UU_FALSE;


	type = NCLI_POINT;
	to_unibase (pext0,pext,&type);
	type = NCLI_VECTOR;
	to_unibase (fwd,tfwd,&type);
	um_unitvc(tfwd,tfwd);

	*ierr = 1;
	if (cbxyz.cur_cnt == 0) return (UU_FAILURE);
	phld = p1 = p2 = (UM_vector *) UU_LIST_ARRAY (&cbxyz);
	knpt = (int *) UU_LIST_ARRAY (&cbnum);
	p2hld = UU_NULL;
	um_nullvc (v1hld);
	um_nullvc (v2hld);

	for (i=0; i<NCL_cpn; i++)
	{
		for (j=1; j<knpt[i]; j++)
		{
			p2++;
			um_vcmnvc (p2, p1, v1);
			if (um_mag(v1) > NCL_tol)
			{
				*ierr = 0;
				um_unitvc (v1, vu);
				d1 = um_dot (p1, vu);
				d2 = um_dot (p2, vu);
				d3 = um_dot (pext, vu);
				if (d3 < d1) d3 = d1;
				if (d3 > d2) d3 = d2;
				u = 0.0;
				if ((d2-d1) > UM_FUZZ) u = (d3-d1) / (d2-d1);
				um_vctmsc (v1, u, v1);
				um_vcplvc (p1, v1, p3);
				um_vcmnvc (p3, pext, v2);
				d3 = um_mag (v2);
/*
... jingrong 01/28/2000. if the tangent vector is parellel to fwd vector
... hold on to see if there's another better choice.
.....Check for 180 vector also
.....Bobby  -  3/6/00
*/
				co = fabs(um_dot(vu,tfwd));
				if (d3 < d1hld && co < 0.9)
				{
					lgood = UU_TRUE;
					d1hld = d3;
					phld = p1;
					p2hld = p2;
					um_vctovc (vu, v1hld);
				}
				else if (!lgood && d3 < d2hld && co >= 0.9)
				{
					lgood2 = UU_TRUE;
					d2hld = d3;
					p2hld = p1;
					um_vctovc(vu, v2hld);
				}
			}
			p1 = p2;
		}
		p2++;
		p1 = p2;
   }

	if (!lgood)
	{
		if (*ierr > 0 || !lgood2) return (UU_FAILURE);
		phld = p2hld;
		um_vctovc(v2hld,v1hld);
	}

	type = NCLI_VECTOR;
	from_unibase (v1hld, ve, &type);
	type = NCLI_POINT;
			
	if (lgood && *dis > NCL_tol)
	{
		UM_coord pt1,pt2;

		d2 = *dis;
/*
..... if called from csrel near the end of the motion, try to return
..... a better point projection - MFGNC303
*/
		from_unibase (phld, pt1, &type);
		from_unibase (p2hld, pt2, &type);
		d3 = UM_SQDIS (pt1,pt2);
		um_vcmnvc (pext0,pt1,v1);
		co = UM_DOT(v1,ve);

		d1 = UM_DOT (v1,v1) - co*co;
		*dis = (d1 > UM_DFUZZ)? sqrt(d1): 0;

		if (co <= 0 || *dis > d2)
			um_vctovc (pt1,pt);
		else if (co*co >= d3)
			um_vctovc (pt2,pt);
		else
		{
			um_vctmsc (ve,co,vu);
			um_vcplvc (pt1,vu,pt);
		}
	}
	else
	{
   		from_unibase (phld, pt, &type);
   		um_nptln (pext0, pt, ve, v1);
   		*dis = um_dcccc (pext0, v1);
	}
/*
..... ierr=-1 is a flag to use in endpas - it should not make any
..... difference (from  ierr=0) elsewhere
*/
	if (*ierr == 0 && NCL_straight) *ierr = -1;

	return (UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : cbdist (pext0,pext1,dis,cbpt,bar,fwd)
**       Check if the tool is far from the current 'straight' common
**       boundary.
**    PARAMETERS
**       INPUT  :
**          pext0 - Tool end point
**          pext1 - Tool top point
**          bar   - current 'far' parameter (decision is made if the
**                  distance is twice bar)
**          fwd   - Current tool forward
**       OUTPUT :
**          dis       - distance parameter either unchaged or
**                      reset as 'too far'.
**          cbpt      - near point on common boundary
**    RETURNS      :
**         UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void cbdist (pext0,pext1,dis,cbpt,bar,fwd)
UM_real8 pext0[],pext1[],cbpt[],*dis,fwd[];
UM_real4 *bar;
{
	UM_vector tfwd,v1;
	UM_coord p0,p1;
	UU_REAL d1,d2,d30,d31,u0,u1,u,co;
	UM_int2 type;
	UU_LOGICAL lsec;

/*
..... This is a yes-or-no routine, it does not calculate any actual distance.
.....
..... Calculate projections of the tool top and bottom onto the CB-line.
..... If both projections are on the same extension and far from the line
..... endpoints, reset the distance
*/
	type = NCLI_POINT;
	to_unibase (pext0,p0,&type);
	to_unibase (pext1,p1,&type);
	type = NCLI_VECTOR;
	to_unibase (fwd,tfwd,&type);
	um_unitvc(tfwd,tfwd);

	if (cbxyz.cur_cnt == 0) return;

	co = fabs(um_dot(vustr,tfwd));
	if (co >= 0.9) return;

	d1 = um_dot (p1str, vustr);
	d2 = um_dot (p2str, vustr);
	d30 = um_dot (p0, vustr);
	d31 = um_dot (p1, vustr);

	if ((d30-d1)*(d30-d2) > NCL_tol && (d31-d1)*(d31-d2) > NCL_tol &&
			(d30-d1)*(d31-d1) > 0)
	{
		lsec = ((d30-d1)/(d30-d2) > 1.); /* second point is closer */
		if (lsec)
		{
			u0 = d30-d2;
			u1 = d31-d2;
		}
		else
		{
			u0 = d30-d1;
			u1 = d31-d1;
		}
		
		if (u1/u0 > 1.)
			u = u0;
		else
			u = u1;

		um_vctmsc (vustr, u, v1);
		type = NCLI_VECTOR;
		from_unibase (v1, v1, &type);

		co = UM_DOT (v1,v1);
		u = *bar + 5*NCL_tol;

		if (co > 4*u*u)
		{
			*dis = u;
			type = NCLI_POINT;
			if (lsec)
				from_unibase (p2str, cbpt, &type);
			else
				from_unibase (p1str, cbpt, &type);
		}
	}

	return;
}

/*********************************************************************
**    E_FUNCTION     : void bndprj (pt0,u0,v0,sdata)
**       If a CS surface projection of a common CS-DS boundary point
**       is not near the point, try to project it onto the CS boundary.
**       If the result is better, replace all surface projection data.
**    PARAMETERS
**       INPUT  :
**          pt0   - common boundary point
**          u0,v0 - current surface projection parameters
**          sdata - current surface projection: 0-3 is the plane,
**                                              4-6 is the surface point
**       OUTPUT :
**          u0,v0 - new surface projection parameters
**          sdata - new surface projection
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void bndprj (pt0,u0,v0,sdata)
UM_real8 pt0[];
UM_real4 *u0,*v0,sdata[];
{
	UM_coord bpt;
	UU_REAL d,pm;
	struct NCL_fixed_databag sf;
	NCL_surf_struct s;
	int i,status;

/*
..... check if the surface point is within 2*tol to pt0 - exit if it is
*/
	for (i = 0, d = 0.; i < 3; i++)
	{
		bpt[i] = pt0[i];
		d += (pt0[i] - sdata[4+i])*(pt0[i] - sdata[4+i]);
	}
	if (d < 4*NCL_tolsq) return;

	sf.key = kk2;
	status = ncl_retrieve_data_fixed (&sf);
	if (status != UU_SUCCESS) return;
	status = ncl_proj_on_srf_bndry (bpt,&sf,&surf_bound[1],&s,UU_FALSE);
	if (status == UU_SUCCESS)
	{
		d = UM_SQDIS(bpt,s.pt);
		if (d > NCL_tolsq) return;
		if (fabs(*u0 - s.uv[0]) < 0.01 && fabs(*v0 - s.uv[1]) < 0.01)
			return;
/*
..... if the boundary projection of pt0 is within tol and the new surface
..... parameters are "very" different from the input - replace the surface
..... projection with the boundary projection
*/
		*u0 = s.uv[0]; *v0 = s.uv[1];

		pm = (sdata[0]*s.normal[0] + sdata[1]*s.normal[1] +
									sdata[2]*s.normal[2] < 0)? -1: 1;
		for (i = 0; i < 3; i++)
		{
			sdata[i] = pm*s.normal[i]; sdata[4+i] = s.pt[i];
		}
		sdata[3] = pm*s.distance;
	}

	return;
}

/*********************************************************************
**    E_FUNCTION     : int tbdini (nclkey, isrf, iflg)
**       Initialization routine for driving a trimmed surface as trimmed.
**       Trimm is limited to the minimum u,v box of trimmed SF.
**    PARAMETERS
**       INPUT  :
**          nclkey   - Key of trimmed surface entity.
**          isrf     - PS, DS, CS index.
**          iflg     - =0, only initialize if surface trimmed flag is on
**                   - =1, initialize any trimmed surface.
**       OUTPUT :
**          iflg     - 1 iff initialized successfully; else 0.
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int tbdini (nclkey, isrf, tol, iflg)
UM_int4 *nclkey;
UM_int2 *isrf;
UM_int2 *iflg;
UU_REAL *tol;
{
	int isf, irslt, status;
	struct NCL_fixed_databag sf, *psf;
/*
... aak 13-feb-1998: modified this routine a lot, for now the boundary is
... retrieved/stored in the Unibase; see the previous version for differences
*/
	irslt = *iflg;
	isf = *isrf-1;

	psf = &sf;
	psf->key = *nclkey;

	if (ncl_retrieve_data_fixed (psf) == UU_SUCCESS)
		status = ncl_tsf_get_trimmed (psf, &irslt);

	if (irslt)
	{
		UM_srf_boundary b;

		ncl_set_boundary_toler (*tol);
		status = ncl_get_boundary (UV_BOX_LIST,psf,&b);

		if (status == UU_SUCCESS)
		{
			um_vctovc_2d (b.ummx,NCL_ummx[isf]);
			um_vctovc_2d (b.vmmx,NCL_vmmx[isf]);
/*
.....um_free_boundary should only be called if ncl_get_boundary
.....returns UU_SUCCESS.  JLS 11/8/99
*/
			um_free_boundary (&b);
		}
	}

	*iflg = (irslt && (status == UU_SUCCESS) ) ? 1 : 0;

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : int gtmmuv(isf,umin,vmin,umax,vmax)
**       Return min & max u & v on a trimmed sf boundary.  Outside
**       boundary only are returned here, but can be used for all
**       inner boudaries as well.  This is F77 interface.
**    PARAMETERS
**       INPUT  :
**          isf        - Fortran call sf index
**       OUTPUT :
**          umin       - minimum u value
**          umax       - maximum u value
**          vmin       - minimum u value
**          vmax       - maximum u value
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int gtmmuv(isf,umin,vmin,umax,vmax)
UM_int2 *isf;
UM_real8 *umin,*vmin,*umax,*vmax;
{
	*umin = NCL_ummx[*isf-1][0];
	*umax = NCL_ummx[*isf-1][1];
	*vmin = NCL_vmmx[*isf-1][0];
	*vmax = NCL_vmmx[*isf-1][1];
	return (0);
}

/*********************************************************************
**    E_FUNCTION     : int stmmuv(isf,umin,vmin,umax,vmax)
**       Sets min & max u & v on a trimmed sf boundary.  Outside
**       boundary only are set here, but can be used for all
**       inner boudaries as well.  This is F77 interface.
**    PARAMETERS
**       INPUT  :
**          isf        - Fortran call sf index
**          umin       - minimum u value
**          umax       - maximum u value
**          vmin       - minimum u value
**          vmax       - maximum u value
**       OUTPUT :
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void stmmuv(isf,umin,vmin,umax,vmax)
UM_int2 *isf;
UM_real8 *umin,*vmin,*umax,*vmax;
{
   NCL_ummx[*isf-1][0] = *umin;
   NCL_ummx[*isf-1][1] = *umax;
   NCL_vmmx[*isf-1][0] = *vmin;
   NCL_vmmx[*isf-1][1] = *vmax;
}

/*********************************************************************
**    E_FUNCTION     : int ncl_cvonsf_find(cvkey,sfkey,listpt)
**       Search common list for specified key with CV on SF data.
**    PARAMETERS
**       INPUT  :
**          cvkey      - key of CVonSF.
**          sfkey      - key of associated surface.
**       OUTPUT :
**          listpt     - pointer to evaluated CV_on_SF data record
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_cvonsf_find (cvkey,sfkey,listpt)
UU_KEY_ID cvkey,sfkey;
struct NCL_cvonsf_rec **listpt;
{
	int i, n, status = UU_FALSE,relnum;
	UU_KEY_ID bskey;
	static UU_KEY_ID Skey=0,Bkey=0;
	struct NCL_cvonsf_rec *p2;
	struct NCL_trimsf_rec tsf;
	if (NCL_cvonsf_list_init1)
	{
		p2 = (struct NCL_cvonsf_rec *) UU_LIST_ARRAY (&NCL_cvonsf_list);
		n = UU_LIST_LENGTH(&NCL_cvonsf_list);
/*
.....Check against trim surface key &
.....base surface key
*/
		bskey = 0;
		ur_retrieve_data_relnum(sfkey,&relnum);
		if (relnum == NCL_TRIMSF_REL)
		{
			tsf.key = sfkey;
			ncl_retrieve_data_fixed(&tsf);
			bskey = tsf.bs_key;
		}
		for (i=0; i<n && !status; i++)
		{
			if (p2->cvkey == cvkey && (p2->sfkey == sfkey || p2->sfkey == bskey))
			{
				if (Bkey != p2->cvkey || Skey != p2->sfkey)
				{
					Bkey = p2->cvkey;
					Skey = p2->sfkey;
					psreset();
				}
				*listpt = p2;
				status = UU_TRUE;
			}
			else p2++;
		}
	}
	return (status);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_cvonsf_store(cvkey,sfkey,cvlen,uvlst,
**                                xyzlst)
**       Store boundary box in the list with the surf key.
**    PARAMETERS
**       INPUT  :
**          cvkey     - CV key
**          sfkey     - SF key where CV lies on.
**          cvlen     - CV length
**          uvlst     - pointer to list of uv points
**          xyzlst    - pointer to list of xyz points
**       OUTPUT : none
**    RETURNS      :
**         UU_SUCCESS if no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_cvonsf_store (cvkey,sfkey,cvlen,uvlst,xyzlst)
UU_KEY_ID cvkey, sfkey;
UU_LIST *uvlst, *xyzlst;
UM_real8 cvlen;
{
	int status = UU_SUCCESS;
	struct NCL_cvonsf_rec cvonsf;

	if ( !NCL_cvonsf_list_init1) ncl_cvonsf_list_init();
	cvonsf.cvkey = cvkey;
	cvonsf.sfkey = sfkey;
	cvonsf.cvlen = cvlen;
	cvonsf.uvlst = uvlst;
	cvonsf.xylst = xyzlst;
	uu_list_push (&NCL_cvonsf_list, &cvonsf);
	return (status);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_cvonsf_list_init()
**       Initialize list to store boundary boxes.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_cvonsf_list_init()
{
	uu_list_init (&NCL_cvonsf_list, sizeof (struct NCL_cvonsf_rec),100,100);
	NCL_cvonsf_list_init1 = 1;

	return (0);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_cvonsf_free ()
**       Dealocate memory used for lists with curves on surface
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      :
**         UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_cvonsf_free()
{
	int n, i;
	struct NCL_cvonsf_rec *p2;

	if (NCL_cvonsf_list_init1)
	{
		p2 = (struct NCL_cvonsf_rec *) UU_LIST_ARRAY (&NCL_cvonsf_list);
		n = UU_LIST_LENGTH(&NCL_cvonsf_list);
		for (i=0; i<n; i++)
		{
			uu_list_free (p2->uvlst);
			uu_list_free (p2->xylst);  p2++;
		}
		uu_list_free (&NCL_cvonsf_list);
		NCL_cvonsf_list_init1 = 0;
	}
	return(0);
}

/**********************************************************************
**    E_FUNCTION     : int norm_to_cvonsf (pnt,gpar,jsf,pt,vc)
**        Find projection of the external point on CV using evolved
**        CVonSF data stored in the list.  Interface function.
**    PARAMETERS
**       INPUT  :
**            pnt  - point to project on cv and PS
**            gpar - initial value of curve parameter (previous location)
**            jsf  - sf index in global motion common
**       OUTPUT :
**            gpar   - parameter value at calc'd point
**            pt   - point on curve (projection of pnt)
**            vc   - vector tangent to curve at pt.
**    RETURNS      :
**         UU_SUCCESS if any projection found, othervise 1.
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
int norm_to_cvonsf (pnt,gpar,jsf,pt,vc)
UM_coord *pnt, *pt, *vc;
UM_real8 *gpar;
UM_int4 *jsf;
{
	struct NCL_cvonsf_rec *p2;
	UM_coord *cvpt;
	int npt, nn, status;

	status = 0;
/*
.....get evolved curve data
*/
	p2 = (struct NCL_cvonsf_rec *) NCL_cvonsf[*jsf];
	npt = UU_LIST_LENGTH (p2->xylst);
	cvpt = (UM_coord *) UU_LIST_ARRAY (p2->xylst);
	if (npt == 2)
		nn = norm_to_lnonsf1(pnt,gpar,cvpt,p2->cvlen,pt,vc);
	else
		nn = norm_to_cvonsf1 (pnt,gpar,npt,cvpt,p2->cvlen,pt,vc);
/*
.....if projected point is outside curve scope use curve
.....extension to find projection (future step)
*/
	if (nn == 0) status = 1;

	return (status);
}

/**********************************************************************
**    E_FUNCTION     : int norm_to_cvonsf1 (pnt,gpar,jsf,cvlen,pt,vc)
**        Find projection of the external point on CV using evolved
**        CVonSF data stored in the list.  Calculates curve parameter
**        where projected point is located, point coordinates and
**        vector tangent to the CV at the point.  Returned point is
**        closest to the expected in terms of parameter value.
**    PARAMETERS
**       INPUT  :
**         pnt   - point to project on cv and PS
**         gpar  - initial value of curve parameter (previous location)
**         npt   - number of points in cvpt array
**         cvpt  - evolved curve data (points)
**         cvlen - curve length
**       OUTPUT :
**         gpar   - parameter value at calc'd point
**         pt   - point on curve (projection of pnt)
**         vc   - vector tangent to curve at pt.
**    RETURNS      :
**         number of projection points on curve.
**    SIDE EFFECTS : none
**    WARNINGS     : none

**********************************************************************/
int norm_to_cvonsf1 (pnt,gpar,npt,cvpt,cvlen,pt,vc)
UM_coord pnt, *cvpt, pt, vc;
UM_real8 *gpar, cvlen;
UM_int4 npt;
{
	UM_coord pt2, vl2, *ptl, *vtl, vsg;
	UU_REAL d, s, s0, dsg, dt, *ddl;
	int i, j, nn, isit, fwd, flg, i1,i2,i3;
	UU_LOGICAL do_corner, closed;
	UM_plane plane;

	ptl  = (UM_coord *) uu_malloc (npt*sizeof(UM_coord));
	vtl  = (UM_coord *) uu_malloc (npt*sizeof(UM_coord));
	ddl  = (UU_REAL *)  uu_malloc (npt*sizeof(UU_REAL));
	dt  = 0;
	nn  = 0;
	flg = fwd = 0;
	s0 = 0.;
	closed = (UM_SQDIS(cvpt[0],cvpt[npt-1]) < UM_DFUZZ);
/*
.....find all projections of point on CV
*/
	for (i=1; i<npt; i++)
	{
	 	um_vcmnvc (cvpt[i],cvpt[i-1],vsg);
		dsg = um_mag (vsg);
		dt += dsg;
		if (dsg < UM_FUZZ) continue;
		for (j = 0; j< 3; j++) vl2[j] = vsg[j]/dsg;
		isit = um_nptsg (pnt,cvpt[i-1],vl2,dsg,pt2,&d);
/*
.....check if projection is in segment
*/
		switch (isit)
		{
			case 0:	
				do_corner = (fwd == 1) || ( (i == 1) && !closed);
				break;
			case 1:	
				fwd = 0;
				do_corner = UU_FALSE;
				if (d > UM_FUZZ)
				{
					if (flg == 1)
					{
						s = UM_SQDIS (pnt,pt2);
						if (NCL_lv93 || s < 100*s0 || s < 0.01) nn--;
					}
					flg = 0; s0 = 0.;
				}
				else
				{
/*
... jingrong 11/02/99 for ncl_2sf_tanto, consider the boundary point.
*/
					if (!Ibndpt)
					{		
						s0 = UM_SQDIS (pnt,pt2);
						flg = 1;
					}
				}
/*
.....calculate parameter value for projection
*/
				um_vctovc (pt2,ptl[nn]);
				um_vctovc (vl2,vtl[nn]);
				ddl[nn] = (dt - d) / cvlen;
				nn++;
				break;
/*
.....if not in segment, check if it is corner point where
.....previous projection was in front of segment but current
.....projection is behind segment
*/
			case 2:
				fwd = 1;
				do_corner = (i == npt-1);
				if (do_corner) i++;
				break;
		}  /* switch(isit) */

		if (do_corner)
		{
			um_vctovc (cvpt[i-1],ptl[nn]);
/*
... aak: if corner, tangent vector is perp. to vector connecting
... corner and projection of ext.point on the corner plane
*/
			if ( !closed && ((i==1) || (i==npt)) )
				um_vctovc(vl2,vtl[nn]);
			else
			{
/*
... aak: in fact, one has to take i1,i2 and look for i3 such
... that i1,i2,i3 are not collinear
*/
				if( (i==1) || (i==npt) )
				{
					i1 = npt-2; i2 = 0; i3 = 1;
				}
				else
				{
					i1 = i-2; i2 = i-1; i3 = i;
				}
				if (um_plane1 (cvpt[i1],cvpt[i2],cvpt[i3], &plane))
				{
					um_nptpln(pnt,plane.p0,plane.n,vc);
					um_vcmnvc(vc,cvpt[i2],vc);
					um_cross(vc,plane.n,vc);
					um_unitvc(vc,vc);

					um_vcmnvc(cvpt[i2],cvpt[i1],vl2);
					if (um_dot(vc,vl2) < 0.) um_vctmsc(vc, (UU_REAL) -1.0, vc);
					um_vctovc (vc,vtl[nn]);
				}
				else
					um_vctovc(vl2,vtl[nn]);
			}
/*
...jingrong 11/12/98 problems with out of boundary points
...force it to 0 or 1
*/
			if (i==1) ddl[nn] = 0.;
			else if (i==npt) ddl[nn] = 1.0;
			else ddl[nn] = (dt - dsg) / cvlen;
			nn++;
			fwd = 0;
		}
	}     /* for (); */
	d = 1.e20;
	for (j=0; j<nn; j++)
	{
		s = UM_SQDIS (pnt,ptl[j]);
		if (s < d)
		{
			d = s;  i = j;
		}
	}
/*
.....set output values
*/
	if (nn > 0)
	{
		*gpar = MIN2(1.0,ddl[i]);
		*gpar = MAX2(.0,*gpar);
		um_vctovc (ptl[i],pt);
		um_vctovc (vtl[i],vc);
	}
/*
.....free memory
*/
	uu_free (ptl);
	uu_free (vtl);
	uu_free (ddl);

	return (nn);
}

/*******************************************************************
**    E_FUNCTION     : int ncl_cvonsf_init (sfkey,cvkey,sfix,tol,ierr)
**       Initialization routine to evolve CVonSF and store data in
**       the list and motion common storage as a single curve.
**    PARAMETERS
**       INPUT  :
**          sfkey   - Key of surface entity where CV is located.
**          cvkey   - Key of CV on surface
**          sfix    - Index in common motion storage where data
**                    will be stored
**          tol     - tolerance for curve evolver
**       OUTPUT :
**          ierr    - 0 if success or 466 if failure.
**    RETURNS      :
**         UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_cvonsf_init (sfkey,cvkey,sfix,tol,ierr)
UU_KEY_ID *cvkey, *sfkey;
UM_real8 *tol;
UM_int2 *sfix, *ierr;
{
	int status = UU_SUCCESS;
	int npt,ix,ncv,i,np,rev,itsk;
	UU_LIST *xyzls, *uvsls;
	struct NCL_fixed_databag cssf;
	struct NCL_fixed_databag ccv, uvcvi;
	struct UM_uvcvonsf_rec *uvcv;
	UM_real8 d, um_getpolylen();
	UM_transf tfmat,*tf;
/*
.....Initialize routine
*/
	ix = *sfix - 1;
/*
.....Surface curve list is not initialized yet
*/
	if (!ncl_cvonsf_find(*cvkey,*sfkey,&NCL_cvonsf[ix]))
	{
		xyzls = (UU_LIST *) uu_malloc (sizeof(UU_LIST));
		uu_list_init (xyzls, sizeof(UM_coord), 100, 100);
		uvsls = (UU_LIST *) uu_malloc (sizeof(UU_LIST));
		uu_list_init (uvsls, sizeof(UM_coord), 100, 100);
		cssf.key = *sfkey;
/*
... aak 13-nov-1997: modified for composite uv-curve on surface
*/
		ccv.key = *cvkey;
		status = ncl_retrieve_data_fixed (&ccv);
		if (status != UU_SUCCESS) goto Done;
/*
.....get transformation matrix of curve and evolve
.....curve on surface
.....(Used to be transformation matrix of base surface)
.....Bobby  -  2/14/00
*/
		tf = &tfmat;
		status = ncl_trimsrf_get_tf (&ccv,&tf);
		if (status != UU_SUCCESS) goto Done;
/*
..... special evolving for polylines to avoid shaky tool paths
..... Dassault MFGNC252
*/
		if (!ncl_itsa_compcrv(&ccv))
		{
			if (NCLX_internal_geom && ccv.rel_num == NCLX_MDL_POLYLINE)
				status = nclx_polyln_on_sf(&cssf,tf,&ccv,*tol,&npt,uvsls,xyzls);
			else
				status = um_pre_srf_curve(&cssf,tf,&ccv,*tol,&npt,uvsls,xyzls);
			if (status != UU_SUCCESS) goto Done;
			itsk = 2;
			if (npt > 2)
				ncl_fix_cvonsf (0,&npt,uvsls,xyzls,itsk);
		}
		else
		{
			status = ncl_compcrv_getnents(&ccv,&ncv);
			if (status != UU_SUCCESS) goto Done;
			for (i=0, npt=0; i<ncv && status == UU_SUCCESS; i++)
			{
				status = ncl_compcrv_getelm(&ccv,i,&uvcvi,&rev);
				if (status != UU_SUCCESS) goto Done;

				if (NCLX_internal_geom && uvcvi.rel_num == NCLX_MDL_POLYLINE)
					status = nclx_polyln_on_sf(&cssf,tf,&uvcvi,*tol,&np,uvsls,xyzls);
				else
				{
					uvcv = (struct UM_uvcvonsf_rec *)&uvcvi;
/*
........Curve is on a different surface
........Output CVonSF data for previous surface
*/
					if (npt != 0 && uvcv->bskey != cssf.key)
					{
						npt++;
						uvsls->cur_cnt++;
						xyzls->cur_cnt++;
						S_push_cvonsf(*cvkey,cssf.key,ix,uvsls,xyzls);
						xyzls = (UU_LIST *) uu_malloc (sizeof(UU_LIST));
						uu_list_init (xyzls, sizeof(UM_coord), 100, 100);
						uvsls = (UU_LIST *) uu_malloc (sizeof(UU_LIST));
						uu_list_init (uvsls, sizeof(UM_coord), 100, 100);
						npt = 0;
					}
					cssf.key = uvcv->bskey;
					status = um_pre_srf_curve(&cssf,tf,uvcv,*tol,&np,uvsls,xyzls);
				}
				if (status != UU_SUCCESS) goto Done;

				if (rev) ncl_revers_list(np,npt,uvsls,xyzls,UU_NULL);

				if (ncv == 1)
					itsk = 2;
				else if (npt == 0)
					itsk = 0;
				else if (i == ncv-1)
					itsk = 1;
				else
					itsk = -1;
				if (np > 2 && itsk >= 0)
					ncl_fix_cvonsf (npt,&np,uvsls,xyzls,itsk);

				npt += np;
/*
... drop last point if more curves follow
*/
				if (i < ncv-1)
				{
					npt--;
					uvsls->cur_cnt--;
					xyzls->cur_cnt--;
				}
			}
		}
/*
.....store evolved curve data in common list and
.....motion data list
*/
Done:;
		if (status == 466) *ierr = 466;
		else if (status != UU_SUCCESS) *ierr = 122;
		else
		{
			status = S_push_cvonsf(*cvkey,cssf.key,ix,uvsls,xyzls);
			if (status == 0)
				ncl_cvonsf_find(*cvkey,*sfkey,&NCL_cvonsf[ix]);
			*ierr = status;
		}
	}

	return (UU_SUCCESS);
}

/*******************************************************************
**    E_FUNCTION     : void ncl_fix_cvonsf (npt,np,uvsls,xyzls,itsk)
**       Fix CVonSF if it has a little "hook" at the end - DASSAULT MFGNC240
**    PARAMETERS
**       INPUT  :
**          npt     - start of the current piece in the list
**          np      - number of points in the current piece
**          uvsls   - list of uv-coordinates of points
**          xyzls   - list of xyz-coordinates of points
**          itsk    - 0 if first, and not only, composite component
**                  - 1 if last, and not only, composite component
**                  - -1 if neither first nor last composite component
**                  - 2 if not composite
**       OUTPUT :
**          np      - number of points in the current piece
**          uvsls   - list of uv-coordinates of points
**          xyzls   - list of xyz-coordinates of points
**    RETURNS      :
**         UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_fix_cvonsf (npt,np,uvsls,xyzls,itsk)
int npt,*np,itsk;
UU_LIST *xyzls, *uvsls;
{
	UM_coord *cvpt;
	UM_vector v0,v1,v2;
	UU_REAL d0,d1,d2,co12;
	int nn = *np;

	if (uvsls->cur_cnt < npt+nn || xyzls->cur_cnt < npt+nn)
   	return (UU_SUCCESS);

	cvpt = (UM_coord *) UU_LIST_ARRAY (uvsls);
	cvpt += npt;
	if (itsk != 1 && nn > 2)
	{
		um_vcmnvc (cvpt[1],cvpt[0],v0);
		um_vcmnvc (cvpt[2],cvpt[1],v1);
		d0 = um_mag(v0); d1 = um_mag(v1);
		um_unitvc (v0,v0); um_unitvc (v1,v1);
		if (nn > 3)
		{
			um_vcmnvc (cvpt[nn-1],cvpt[2],v2);
			d2 = um_mag(v2);
			um_unitvc (v2,v2);
			co12 = um_dot (v1,v2);
			if (co12 > 0.99985) d1 += co12*d2;
		}
		if (d1 > 0.5 && d0 < 0.01*d1 && um_dot (v0,v1) < 0.087155743)
		{
				uu_list_delete (uvsls,npt,1);
				uu_list_delete (xyzls,npt,1);
				nn--;
		}
	}
	if (itsk != 0 && nn > 2)
	{
		cvpt = (UM_coord *) UU_LIST_ARRAY (uvsls);
		cvpt += npt;
		um_vcmnvc (cvpt[nn-1],cvpt[nn-2],v0);
		um_vcmnvc (cvpt[nn-2],cvpt[nn-3],v1);
		d0 = um_mag(v0); d1 = um_mag(v1);
		um_unitvc (v0,v0); um_unitvc (v1,v1);
		if (nn > 3)
		{
			um_vcmnvc (cvpt[nn-3],cvpt[0],v2);
			d2 = um_mag(v2);
			um_unitvc (v2,v2);
			co12 = um_dot (v1,v2);
			if (co12 > 0.99985) d1 += co12*d2;
		}
		if (d1 > 0.5 && d0 < 0.01*d1 && um_dot (v0,v1) < 0.087155743)
		{
				uvsls->cur_cnt--;
				xyzls->cur_cnt--;
				nn--;
		}
	}
	*np = nn;
	return (UU_SUCCESS);
}

/*******************************************************************
**    E_FUNCTION     : int ncl_cvonsf_init_mult (sfkey,cvkey,tol,ierr)
**       Initialization routine to evolve CVonSF and store data in
**       the list and motion common storage as a multiple PS.
**    PARAMETERS
**       INPUT  :
**          sfkey   - Key of surface entity where CV is located.
**          cvkey   - Key of CV on surface
**          tol     - tolerance for curve evolver
**       OUTPUT :
**          ierr    - 0 if success or 466 if failure.
**    RETURNS      :
**         UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_cvonsf_init_mult(sfkey,cvkey,sfix,tol,ierr)
UU_KEY_ID *cvkey, *sfkey;
UM_real8 *tol;
UM_int2 *ierr,*sfix;
{
	int status = UU_SUCCESS,ncv,i,np,rev;
	UU_LIST xyzls, uvsls;
	struct NCL_fixed_databag cssf;
	struct NCL_fixed_databag compcv, uvcv;
	UM_real8 d, um_getpolylen();
	UM_transf tfmat,*tf;

	cssf.key = *sfkey;
/*
.....get transformation matrix of base surface and evolve
.....curve on surface
*/
	tf = &tfmat;
	status = ncl_trimsrf_get_tf (&cssf, &tf);
	if (status != UU_SUCCESS) goto Done;
	compcv.key = *cvkey;
	status = ncl_retrieve_data_fixed (&compcv);
	if (status != UU_SUCCESS) goto Done;

	if (!ncl_itsa_compcrv(&compcv)) goto Done;

	status = ncl_compcrv_getnents (&compcv,&ncv);
	if (ncv <= 1) goto Done;

	uu_list_init (&xyzls, sizeof(UM_coord), 100, 100);
	uu_list_init (&uvsls, sizeof(UM_coord), 100, 100);

	for (i=0; i<ncv && status == UU_SUCCESS; i++)
	{
		status = ncl_compcrv_getelm (&compcv,i,&uvcv,&rev);
		status = um_pre_srf_curve (&cssf,tf,&uvcv,*tol,&np,&uvsls,&xyzls);
/*
.....store evolved curve data in common list and motion data list
*/
		if (status == UU_SUCCESS)
		{
			if (rev) ncl_revers_list(np,0,&uvsls,&xyzls,UU_NULL);
			d = um_getpolylen (np,UU_LIST_ARRAY(&xyzls));
			status = ncl_cvonsf_store_mult(i,uvcv.key,*sfkey,d,&xyzls,&uvsls);
			xyzls.cur_cnt = uvsls.cur_cnt = 0;
		}
	}

Done:;
	*ierr = status * 466;
	uu_list_free(&xyzls);
	uu_list_free(&uvsls);
	return(UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : int norm_to_sfatcv (gpar,jsf,pt,vttcv,vnorm)
**        Calculates plane normal to the SF at the point on
**        CVonSF defined by CV parameter value, and tangent to
**        the CV at this point.
**    PARAMETERS
**       INPUT  :
**          gpar   - parameter value locating point on CV
**          jsf    - surface index in motion block storage (ISRF)
**                   This is surface simulated by plane (not the
**                   one where CVonSF is laying).
**       OUTPUT :
**          pt      - point on CV defined by parameter value
**          vttcv   - calc'd vector tangent to the CV at gppt
**          vnorm   - calc'd vector normal to the SF at pt.
**    RETURNS      : UU_SUCCESS if no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int norm_to_sfatcv (gpar,jsf,pt,vttcv,vnorm)
UM_coord *pt, *vttcv, *vnorm;
UM_real8 *gpar;
UM_int4 *jsf;
{
	struct NCL_cvonsf_rec *p2;
	struct NCL_fixed_databag eptr, bs, *bsptr, cv;
	struct NCL_fixed_databag rbcv, *cvptr;
	struct UM_evcrvout evout, uvcv;
	struct UM_evsrfout evsrf;
	struct UM_uvcvonsf_rec *cvsf;
	UM_transf tfm, *tf;
	UU_REAL uu, vv, bplm[4], t;
	int nb, status;

	status = UU_FAILURE;
	t = *gpar;
	cvptr = &cv;
	bsptr = &eptr;

	p2 = (struct NCL_cvonsf_rec *) NCL_cvonsf[*jsf];
	bplm[0] = bplm[2] = 0;
	bplm[1] = bplm[3] = 1;
/*
...get surface data
*/
	eptr.key = p2->sfkey;
	if (ncl_retrieve_data_fixed (&eptr) != 0) goto Done;
	if (ncl_itsa_trimsrf (&eptr))
	{
		ncl_trimsrf_get_fixed (&eptr,&nb,bplm);
		bsptr  = (struct NCL_fixed_databag *) &bs;
		status = ncl_trimsrf_get_bs (&eptr,&bsptr);
	}
	else
	{
		cv.key = p2->cvkey;
		if(ncl_retrieve_data_fixed (&cv)) return(UU_FAILURE);
		cvptr = &rbcv;
		if (ncl_itsa_uvcv_onsf(&cv))
		{
			cvsf = (struct UM_uvcvonsf_rec *) &cv;
			ncl_cp_struct_uvcv_rbcv (cvsf,&cvptr);
		}
/*
... aak 13-nov-1997: added case of composite uv-curves
*/
		else if(ncl_itsa_compcrv(&cv))
		{
			struct NCL_fixed_databag uvcvi;
			struct UM_compcrv_rec *compcv;
			UU_REAL t0,t1;
			int i,ncv,rev;

			compcv = (struct UM_compcrv_rec *)&cv;
			status = ncl_compcrv_getnents(compcv,&ncv);
			for(i=0; i<ncv-1 && t >= compcv->cid[i].endparam; i++);
			status = ncl_compcrv_getelm(compcv,i,&uvcvi,&rev);
			cvsf = (struct UM_uvcvonsf_rec *)&uvcvi;
			ncl_cp_struct_uvcv_rbcv (cvsf,&cvptr);

			if (i==0) t0 = 0.0; else t0 = compcv->cid[i-1].endparam;
		   t1 = compcv->cid[i].endparam;
   		t  = (t - t0) / (t1 - t0);
   		if (rev) t = 1. - t;
		}
		else return (UU_FAILURE);
	}
	tf = &tfm;
	status = ncl_trimsrf_get_tf (&eptr, &tf);
/*
.....get curve data and evaluate (u,v) point
*/
	if (status == UU_SUCCESS)
	{
		uc_init_evcrvout (cvptr,&uvcv);
		status =
			um_ev7_crv_on_surf (UM_FRSTDERIV,bsptr,cvptr,bplm,t,tf,&evout,&uvcv);
		if (status == UU_SUCCESS)
		{
			uc_init_evsrfout (bsptr, &evsrf);
			uu = MAX2(0.,uvcv.cp[0]);
			uu = MIN2(1.,uu);
			vv = MAX2(0.,uvcv.cp[1]);
			vv = MIN2(1.,vv);
/*
.....evaluate surface at (u,v)
*/
			status = uc_evsrf (UM_NORM, uu, vv, bsptr, tfm, &evsrf);
			if (status == UU_SUCCESS)
			{
				um_vctovc (evsrf.sp,pt);	
				um_vctovc (evsrf.snorm,vnorm);	
				um_unitvc (evout.dcdu,vttcv);
			}
		}
	}

Done:
	return (status);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_norm_to_sfatcv (gpar,jsf,vnorm)
**        Calculates normal to the base SF at the point on
**        CVonSF defined by CV parameter value.
**        CV is treated as a polyline
**    PARAMETERS
**       INPUT  :
**          gpar   - parameter value locating point on CV
**          jsf    - surface index in motion block storage (ISRF)
**                   This is surface simulated by plane (not the
**                   one where CVonSF is laying).
**       OUTPUT :
**          vnorm   - calc'd vector normal to the SF.
**    RETURNS      : UU_SUCCESS if no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_norm_to_sfatcv (gpar,jsf,vnorm)
UM_real8 *gpar;
UM_int4 *jsf;
UM_coord *vnorm;
{
	struct NCL_cvonsf_rec *p2;
	struct NCL_fixed_databag eptr, bs, *bsptr, cvx;
	struct UM_evsrfout evsrf;
	UM_transf tfm, *tf;
	UM_coord *cvpt, *uvpt;
	UU_REAL uu, vv, bplm[4], t, d, t1,t0;
	int nb, status,npt, i;

	status = UU_FAILURE;
	t = *gpar;
	bsptr = &eptr;

	p2   = (struct NCL_cvonsf_rec *) NCL_cvonsf[*jsf];
	npt  = UU_LIST_LENGTH (p2->xylst);
	uvpt = (UM_coord *) UU_LIST_ARRAY (p2->uvlst);
	cvpt = (UM_coord *) UU_LIST_ARRAY (p2->xylst);
	d    = p2->cvlen;

	eptr.key = p2->sfkey;
/*
...get surface data
*/
	bplm[0] = bplm[2] = 0;
	bplm[1] = bplm[3] = 1;
	if (ncl_retrieve_data_fixed (&eptr) != 0) goto Done;
	if (ncl_itsa_trimsrf (&eptr))
	{
		ncl_trimsrf_get_fixed (&eptr,&nb,bplm);
		bsptr  = (struct NCL_fixed_databag *) &bs;
		status = ncl_trimsrf_get_bs (&eptr,&bsptr);
	}
/*
.....Get the transformation from the curve entity
.....Bobby  -  2/14/00
*/
	tf = &tfm;
	cvx.key = p2->cvkey;
	status = ncl_trimsrf_get_tf (&cvx, &tf);
/*
.....evaluate (u,v) point
*/
	if (status == UU_SUCCESS)
	{
		uc_init_evsrfout (bsptr, &evsrf);

		for(i=0, t1 = 0.; i < npt-1 && t >= t1; i++)
		{
			t0 = t1;
			t1 += um_dcccc(cvpt[i],cvpt[i+1])/d;
		}

		d = t1 - t0;
		if(d == 0.) {status = UU_FAILURE; goto Done;}
		uu = ((t1-t)*uvpt[i-1][0] + (t-t0)*uvpt[i][0])/d;
		vv = ((t1-t)*uvpt[i-1][1] + (t-t0)*uvpt[i][1])/d;

		uu   = MIN2(1.,MAX2(0.,uu));
		vv   = MIN2(1.,MAX2(0.,vv));
/*
.....evaluate surface at (u,v)
*/
		status = uc_evsrf (UM_NORM, uu, vv, bsptr, tf, &evsrf);
		if (status == UU_SUCCESS) um_vctovc (evsrf.snorm,vnorm);	
	}

Done:
	return (status);
}

/*********************************************************************
**    FUNCTION : int ncl_load_cvonsf (sfix,cvkey,sfkey,cvlen,uvlst,xyzlst)
**      Used to convert a piece of a composite CVonSF curve from
**      multi-PS format to "one curve" format: writes the data for
**      the piece into the global structure  "NCL_cvonsf[sfix]"
**    PARAMETERS
**       INPUT  :
**          sfix   - number of motion surface (PS,DS,CS)
**          cvkey  - key of the CVonSF to be loaded
**          sfkey  - key of the base surface
**          cvlen  - length of the CVonSF
**          uvlst  - list of the (u,v) values of the curve points
**          xyzlst - list of the (x,y,z) values of the curve points
**       OUTPUT :
**          "NCL_cvonsf[sfix]" is updated
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_load_cvonsf(sfix,cvkey,sfkey,cvlen,uvlst,xyzlst)
UM_int2 sfix;
UU_KEY_ID cvkey, sfkey;
UU_REAL cvlen;
UU_LIST *uvlst, *xyzlst;
{
	if (!ncl_cvonsf_find (cvkey,sfkey,&NCL_cvonsf[sfix-1]))
	{
		ncl_cvonsf_store (cvkey,sfkey,cvlen,uvlst,xyzlst);
		ncl_cvonsf_find (cvkey,&NCL_cvonsf[sfix-1]);
	}
	return (UU_SUCCESS);
}

/****************************************************************
**    E_FUNCTION     : int norm_to_lnonsf1 (pnt,gpar,jsf,cvlen,pt,vc)
**        Find projection of the external point on a 2-point curve.
**        Calculates curve parameter where projected point is located,
**        point coordinates and vector tangent to the CV at the point.
**    PARAMETERS
**       INPUT  :
**         pnt   - point to project on cv
**         cvpt  - evolved curve data (2 points)
**         cvlen - curve length
**       OUTPUT :
**         gpar   - parameter value at calc'd point
**         pt   - point on curve (projection of pnt)
**         vc   - vector tangent to curve at pt.
**    RETURNS      :
**         number of projection points on curve.
**    SIDE EFFECTS : none
**    WARNINGS     : none

**********************************************************************/
int norm_to_lnonsf1 (pnt,gpar,cvpt,cvlen,pt,vc)
UM_coord pnt, *cvpt, pt, vc;
UM_real8 *gpar, cvlen;
{
        UM_coord pt2, vl2, vsg;
        UU_REAL d, dsg, dt, ddl;
        int isit;

        dt  = 0;
/*
.....find all projections of point on CV
*/
        um_vcmnvc (cvpt[1],cvpt[0],vsg);
        dsg = um_mag (vsg);
        dt += dsg;
        um_unitvc (vsg,vl2);
        isit = um_nptsg (pnt,cvpt[0],vl2,dsg,pt2,&d);
/*
.....check if projection is in segment
*/
        if (isit == 1)
        {
               um_vctovc (pt2,pt);
/*
.....calculate parameter value for projection
*/
               ddl = (dt - d) / cvlen;
        }
        else if (isit == 0)
        {
               um_vctovc (cvpt[0],pt);
               ddl = 0.;
        }
        else
        {
               um_vctovc (cvpt[1],pt);
               ddl = 1.;
        }
/*
.....set output values
*/
        *gpar = MIN2(1.0,ddl);
        *gpar = MAX2(.0,*gpar);
        um_vctovc (vl2,vc);

        return (1);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_2sf_tanto (key1,key2,itanto,icnct,ierr)
**
**       Determine if two surfaces are tangent to each other at their
**       common boundary.
**
**    PARAMETERS
**       INPUT  :
**		key1: 	key to surface 1;
**		key2:	key to surface 2;	
**
**       OUTPUT :
**		itanto:	1 - if two surface are tangent to each other at
**                          common boundary;
**			0 - the two surface are not tangent to each other.
**              icnct:  1 - two surfaces have common boundary
**                      0 - no common boundary
**              ierr  : error flag.
**
**    RETURNS      : UU_SUCCESS if no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_2sf_tanto (key1,key2,itanto,icnct,ierr)
UM_int4 *key1,*key2;
UM_int2 *itanto,*icnct,*ierr;
{
	UU_REAL tol1,tol2,cos;
	int i,npt,status;
	UM_coord *pt;
	struct NCL_fixed_databag srf1,srf2;
	NCL_surf_struct s1,s2;

	*itanto = 0;
	*icnct = 0;
	srf1.key = *key1;
	srf2.key = *key2;
	status = ncl_retrieve_data_fixed(&srf1);
	status += ncl_retrieve_data_fixed(&srf2);

	if (status == UU_SUCCESS)
	{
		gettol(&tol1);
		tol2 = tol1*5.0;
		status = ncl_2sfbnry(key1, key2, &tol1, &tol2, ierr);
	
		if (*ierr == 0)
		{
	 		pt = (UM_coord *) UU_LIST_ARRAY (&cbxyz);
			npt = UU_LIST_LENGTH (&cbxyz);

			Ibndpt = UU_TRUE;
			if (npt > 0)
			{
				*itanto = 1;
				*icnct = 1;
			}
	 		for (i=0; (i<npt) && (*itanto==1); i++, pt++)
			{
				status =
					ncl_proj_on_srf_bndry (pt[0],&srf1,&surf_bound[0],&s1,UU_TRUE);
				status +=
					ncl_proj_on_srf_bndry (pt[0],&srf2,&surf_bound[1],&s2,UU_TRUE);
				if (status != UU_SUCCESS) break;
				cos = um_dot(s1.normal,s2.normal);
				*itanto = (1.0 - fabs(cos)) < 0.001;
			}
		}
	}
	*ierr = status;

	Ibndpt =  UU_FALSE;
	ncl_2sfbnry_free();

	return (0);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_reproj_on_trimsf (key,s)
**
**      Project to trimmed surface boundary if the input surface projection
**      point is out of the surface boundary.
**
**    PARAMETERS
**
**       INPUT  :
**              key:   key to the trimmed surface
**                s:   surface projection info
**
**       OUTPUT :
**                s:   modified surface projection info
**
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_reproj_on_trimsf (key,s)
UM_int4 *key;
NCL_surf_struct *s;
{
	struct NCL_fixed_databag srf;
	UM_srf_boundary p;
	UM_coord sn,*uvptr;
	int status,insf;

	srf.key = *key;
	status = ncl_retrieve_data_fixed (&srf);
	if (status == UU_SUCCESS)
	{
	 	if (ncl_itsa_trimsrf(&srf))
		{
			ncl_get_boundary_toler();
			status = ncl_get_boundary(WHOLE_BOUNDARY_LIST,&srf,&p);
			
			if (status == UU_SUCCESS)
			{
				uvptr = (UM_coord *) UU_LIST_ARRAY (p.uvpts);
  				insf = um_cshape_inschk(uvptr,1,p.np,s->uv,p.ummx, p.vmmx);

				if (insf < 0)
				{
					um_vctovc(s->normal,sn);
					status = ncl_proj_on_srf_bndry (s->pt,&srf,&p,s,UU_FALSE);
					if (um_dot(sn,s->normal) < 0.)
						um_vctmsc (s->normal,(UU_REAL) -1.,s->normal);
				}
			}
			
			if (&p) um_free_boundary (&p);
		}
	}
	return (0);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_proj_csds_bndr (isf,spti,spto,ier)
**       Project a point onto a boundary stored in surf_bound.
**       Called from csavoid (csrelx.f)
**    PARAMETERS
**       INPUT  :
**          isf  - number of the stored boundary structure
**          spti - point to project
**       OUTPUT :
**          spto - projection point
**          ier  - error flag
**    RETURNS      :
**         UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_proj_csds_bndr (isf,spti,spto,ier)
UM_real8 spti[],spto[];
UM_int2 *isf,*ier;
{
	UM_coord pts;
	struct NCL_fixed_databag sf;
	NCL_surf_struct s;
	int i,status,jsf;

	for (i=0;i<3;i++) pts[i] = spti[i];

	*ier = 0;
	jsf = *isf;
	if (jsf == 0)
		sf.key = kk1;
	else
		sf.key = kk2;
	status = ncl_retrieve_data_fixed (&sf);
	if (status == UU_SUCCESS)
		status = ncl_proj_on_srf_bndry (pts,&sf,&surf_bound[jsf],&s,UU_FALSE);
	if (status == UU_SUCCESS)
		for (i=0;i<3;i++) spto[i] = s.pt[i];

	if (status != UU_SUCCESS)
		*ier = 163;

	return (0);
}

/*********************************************************************
**    FUNCTION     : int ncl_get_common_bndry (netsf,bndry)
**
**    Calculates common boundaries of sub-surfaces of a net surface
**
**    PARAMETERS
**       INPUT  :
**           netsf   - pointer to a net surf.
**       OUTPUT :
**          bndry    - common bndry of all sub-surfaces of the
**                     net surface
**    RETURNS      :
**
**         UU_SUCCESS/UU_FAILURE
**
**    SIDE EFFECTS : allocates lists in the bndry structure;
**                   must be deallocated after use;
**    WARNINGS     : none
*********************************************************************/
int ncl_get_common_bndry (netsf,bndry)
struct NCL_netsf_rec *netsf;
UM_netsf_common_bndry *bndry;
{
	UU_LIST tmp_num, tmp0,tmp1,tmp2;
	struct NCL_fixed_databag s1,s2,*psf1,*psf2;
	UM_srf_boundary b1,b2;
	int res,isf1,isf2,status = UU_SUCCESS,npt,nsf,
	    *knpt,i,nseg;
	UU_REAL tol,blen, ncl_toler,d1,d2,um_getpolylen(),um_vcdir();
	UM_coord *pts1, *pts2, *pt0, *pt;
	UM_vector vec;
	UM_int2 idx = 169;
	UM_real8 ver;

	uu_list_init (&tmp0, sizeof(UM_coord), 100, 100);
	uu_list_init (&tmp1, sizeof(UM_coord), 100, 100);
	uu_list_init (&tmp2, sizeof(UM_coord), 10, 10);
	uu_list_init (&tmp_num, sizeof(int), 100, 100);
	npt = 0;
	uu_list_push (&tmp_num, &npt);
/*
... Fortran routine: returns the current value of sc(27)
*/
	gettol (&ncl_toler);
	getsc(&idx, &ver);
	NCL_lv93 = (ver < 9.349);

	bndry->key = netsf->key;	
	bndry->num = 0;	
	bndry->surfaces = (UU_LIST *) uu_malloc (sizeof(UU_LIST));
	bndry->np       = (UU_LIST *) uu_malloc (sizeof(UU_LIST));
	bndry->lengths  = (UU_LIST *) uu_malloc (sizeof(UU_LIST));
	bndry->pts      = (UU_LIST *) uu_malloc (sizeof(UU_LIST));
	uu_list_init (bndry->surfaces, sizeof(int), 50, 50);
	uu_list_init (bndry->np, sizeof(int), 50, 50);
	uu_list_init (bndry->lengths, sizeof(UU_REAL), 50, 50);
	uu_list_init (bndry->pts, sizeof(UM_coord), 200, 200);
	
	ncl_netsf_getnents (netsf,&nsf);
	if (nsf < 2) return (UU_FAILURE);

	for (isf1=1; isf1< nsf && status == UU_SUCCESS; isf1++)
	{
		psf1 = &s1;
		status = ncl_netsf_getelm (netsf,isf1,&psf1);
		if (status != UU_SUCCESS) goto Done;
		if (NCLX_internal_geom) UY_ps = (NCLX_mdl_struct *) psf1;
		status = ncl_get_boundary (WHOLE_BOUNDARY_LIST,psf1, &b1);
		if (status != UU_SUCCESS) goto Done;
		pts1 = (UM_coord *) UU_LIST_ARRAY (b1.cvpts);

		for (isf2=0; isf2 < isf1 && status == UU_SUCCESS; isf2++)
		{
			psf2 = &s2;
			status = ncl_netsf_getelm (netsf,isf2,&psf2);
			if (NCLX_internal_geom) UY_ps = (NCLX_mdl_struct *) psf2;
			if (status != UU_SUCCESS) goto Done;
			status = ncl_get_boundary (WHOLE_BOUNDARY_LIST,psf2, &b2);
			if (status != UU_SUCCESS) goto Done;
			pts2 = (UM_coord *) UU_LIST_ARRAY (b2.cvpts);

			tol = MAX3(b1.toler,b2.toler,ncl_toler);
			tmp0.cur_cnt = tmp1.cur_cnt = tmp2.cur_cnt = 0;
			tmp_num.cur_cnt = 1;

			res = um_match_bndry (pts1,b1.np,pts2,b2.np,tol,&tmp_num,&tmp0);

			if (res > 0)
			{
				pt0 = (UM_coord *) UU_LIST_ARRAY (&tmp0);				
				knpt = (int *) UU_LIST_ARRAY (&tmp_num);

				for (nseg=0;knpt[nseg]>0;nseg++);

				for (i=0; i<nseg;i++)
				{
					if ( um_getpolylen (knpt[i],pt0) > tol)
						uu_list_push_multiple (&tmp1,knpt[i],pt0);
					else
/*
... these are "strange points" - bunch of points generated by um_match_bndry
... which are very close together
*/
						uu_list_push (&tmp2, pt0);

					pt0 += knpt[i];
				}
				if (UU_LIST_LENGTH (&tmp1) < 2) continue; /* goto next isf2 */
/*
... decide what to do with the strange points;
*/
				nseg = UU_LIST_LENGTH (&tmp2);
				pt = (UM_coord *) UU_LIST_ARRAY (&tmp2);

				for (i=0; i<nseg;i++, pt++)
				{
					pt0 = (UM_coord *) UU_LIST_ARRAY (&tmp1);
					npt = UU_LIST_LENGTH (&tmp1);
/*
... closed loop; don't insert
*/
					if (um_dcccc (pt0[0],pt0[npt-1]) < tol) continue;

					d1 = um_dcccc (pt,pt0);
					d2 = um_dcccc (pt,pt0[npt-1]);
/*
... decide to which end the new point is closer; insert if it lies outside
... of the existing polyline
*/
					if ( d1 < d2 && d1 > tol)
					{
						um_vcmnvc (pt0[0],pt0[1],vec);
						if (um_vcdir (pt,pt0[0],vec) >= 0.)
							uu_list_insert (&tmp1,0,pt);
					}
					else if ( d2 < d1 && d2 > tol)
					{
						um_vcmnvc (pt0[npt-1],pt0[npt-2],vec);
						if (um_vcdir (pt,pt0[npt-1],vec) >= 0.)
							uu_list_push (&tmp1,pt);
					}
				}

				pt0 = (UM_coord *) UU_LIST_ARRAY (&tmp1);
				tmp0.cur_cnt = 0;
				uu_list_push (&tmp0,pt0);

				npt = 1 + ncl_weed_list_atol (&tmp1,UU_NULL,UU_NULL,
								&tmp0,UU_NULL,UU_NULL,tol);
						
				if (npt > 1)
				{
					uu_list_push (bndry->np, &npt);
					pt0 = (UM_coord *) UU_LIST_ARRAY (&tmp0);
					uu_list_push_list (bndry->pts, &tmp0);

					bndry->num++;	
					uu_list_push (bndry->surfaces, &isf1);
					uu_list_push (bndry->surfaces, &isf2);

					pt0 = (UM_coord *) UU_LIST_ARRAY (&tmp0);
					blen = um_getpolylen (npt, pt0);
					uu_list_push (bndry->lengths, &blen);
				}
			}
			um_free_boundary (&b2);
		}
			um_free_boundary (&b1);
	}

Done:;
	uu_list_free (&tmp_num);
	uu_list_free (&tmp0);
	uu_list_free (&tmp1);
	uu_list_free (&tmp2);

   return (status);
}

/*********************************************************************
**    FUNCTION     : int ncl_proj_on_srf_bndry (p,srf,bound,s,unibas)
**    Projects a point to the boundary of a surface
**    PARAMETERS
**       INPUT  :
**           p      - point to be projected
**           srf    - surface
**           bound  - boundary structure of srf
**           unibas - no to_unibase/from_unibase calls needed iff true
**       OUTPUT :
**           s     - surface structure containing projection point
**                   & surface normal at projection
**    RETURNS      :
**         UU_SUCCESS/UU_FAILURE
**    SIDE EFFECTS :
**                   none
**    WARNINGS     : none
*********************************************************************/
int ncl_proj_on_srf_bndry (p,srf,bound,s,unibas)
UM_coord p;
struct NCL_fixed_databag *srf;
UM_srf_boundary *bound;
NCL_surf_struct *s;
UU_LOGICAL unibas;
{
	struct NCL_fixed_databag bs, *bsptr;
	struct UM_evsrfout evsrf;
	UM_coord pt,*pts,proj, *uv,*uv0, *pt0;
	UM_transf tfm, *tf;
	UM_vector vec;
	UM_real8 t, t0,t1,tmin, len, len0;
	UU_REAL um_getpolylen();
	UU_REAL dmin,d;
	int status,i,*np,imin;
	UM_int2 type;
	int trimd;

	trimd = 0;

	type = NCLI_POINT;
	if (!unibas)
		to_unibase (p,pt,&type);
	else
		um_vctovc (p,pt);
/*
...get surface data
*/
	if (ncl_itsa_trimsrf (srf))
	{
		trimd = 1;
		bsptr  = (struct NCL_fixed_databag *) &bs;
		status = ncl_trimsrf_get_bs (srf,&bsptr);
		if (status != UU_SUCCESS) return (UU_FAILURE);
	}
	else
		bsptr = srf;

	tf = &tfm;
	status = ncl_trimsrf_get_tf (srf, (UU_REAL **)&tf);
	if (status != UU_SUCCESS) return (UU_FAILURE);

	pts = (UM_coord *) UU_LIST_ARRAY (bound->cvpts);
	np = bound->np;
	uv = (UM_coord *) UU_LIST_ARRAY (bound->uvpts);
	imin = -1;

	for (i=0, dmin=1.e+10; i<bound->nb; pts += np[i], uv += np[i], i++)
	{
		len = um_getpolylen (np[i],pts);
		norm_to_cvonsf1 (pt,&t,np[i],pts,len,proj,vec);
		d = um_dcccc (pt,proj);

		if (d < dmin)
		{
			dmin = d;
			imin = i;	

			tmin = t;
			pt0  = pts;
			uv0  = uv;
			len0 = len;
			um_vctovc (proj,s->pt);
		}
	}
	if (imin < 0) return (UU_FAILURE);

	t = tmin;
	for (i = 0, t1 = 0.; i < np[imin]-1 && t >= t1; i++)
	{
		t0 = t1;
		t1 += um_dcccc(pt0[i],pt0[i+1])/len0;
	}

	len = t1 - t0;
	if (len <= 0.) return (UU_FAILURE);

	s->uv[2] = 0.;
	s->uv[0] = ((t1-t)*uv0[i-1][0] + (t-t0)*uv0[i][0])/len;
	s->uv[1] = ((t1-t)*uv0[i-1][1] + (t-t0)*uv0[i][1])/len;

	s->uv[0] = MIN2(1.,MAX2(0.,s->uv[0]));
	s->uv[1] = MIN2(1.,MAX2(0.,s->uv[1]));

	uc_init_evsrfout (bsptr, &evsrf);
	status = uc_evsrf (UM_NORM, s->uv[0], s->uv[1], bsptr, tf, &evsrf);
	if (status != UU_SUCCESS) return (status);

	if (!unibas)
	{
		type = NCLI_POINT;
		from_unibase (s->pt,s->pt,&type);
		type = NCLI_VECTOR;
		from_unibase (evsrf.snorm,s->normal,&type);
	}
	else
		um_vctovc (evsrf.snorm,s->normal);
	s->distance = um_dot (s->normal,s->pt);

	return (status);
}

/*********************************************************************
**    FUNCTION     : int ncl_common_bndry_proj (isf,bndry,p,proj,vec,isf1)
**    Projects given point on common boundary of a net PS
**    PARAMETERS
**       INPUT  :
**              isf   - number of sub-surface of a net surface
**              bndry - commmon boundary of the net surface
**              p  - given point
**       OUTPUT :
**              proj - projection point
**              vec  - tangent vector of boundary at projection point
**              isf1  - number of the corresponding adjacent subsurface
**    RETURNS      :
**          UU_SUCCESS normally;
**          UU_FAILURE if sub-surf. isf does't have common boundaries
**                                 with other surfaces.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_common_bndry_proj (with_ext,isf,bndry,p,proj,vec,isf1)
UU_LOGICAL with_ext;
int isf,*isf1;
UM_netsf_common_bndry *bndry;
UM_coord p,proj,vec;
{
	UM_coord pt,*points,*pi,*vi;
	UU_REAL t,*cvlen,dmin,dist;	
	int i,nb,*np,res,*surf,status;
	UM_int2 type;
/*
... convert point to the Unibase representation
... (units, modsys, refsys etc.)
*/
	*isf1 = -1;
	type = NCLI_POINT;
	to_unibase (p,pt,&type);

	nb = bndry->num;
	if (nb < 1) return (UU_FAILURE);
	np = (int *) UU_LIST_ARRAY (bndry->np);
	cvlen = (UU_REAL *) UU_LIST_ARRAY (bndry->lengths);
	points = (UM_coord *) UU_LIST_ARRAY (bndry->pts);
	surf = (int *) UU_LIST_ARRAY (bndry->surfaces);

	pi = (UM_coord *) uu_malloc (nb*sizeof(UM_coord));
	vi = (UM_coord *) uu_malloc (nb*sizeof(UM_coord));
/*
... project the point on each of the common boundaries and
... output the closest projection to the given point
*/
	dmin = 1.e10;

	for (i=0, res = -1; i<nb; i++, surf +=2)
	{
/*
... skip if surf. #isrf is not related to this common bndry
*/
		if (surf[0] == isf || surf[1] == isf)
		{
			norm_to_cvonsf1 (pt,&t,np[i],points,cvlen[i],pi[i],vi[i]);	

			dist =  um_dcccc (pt,pi[i]);
			if ( dist < dmin )
			{
				res = i;
				dmin = dist;
				*isf1 = (surf[0] == isf)? surf[1]+1: surf[0]+1;
			}
		}
		points += np[i];
	}

	status = (res >= 0) ? UU_SUCCESS : UU_FAILURE;

	if (status == UU_SUCCESS)
	{
		um_vcmnvc (pt,pi[res],vec);
		um_unitvc (vec,vec);
/*
... if pt projects to an endpoint, project on extension of the curve
*/
		if ( with_ext == UU_TRUE && um_dot (vi[res],vec) > UM_FUZZ )
			um_nptln (pt, pi[res], vi[res],proj);
		else
			um_vctovc (pi[res], proj);

		type = NCLI_POINT;
		from_unibase (proj,proj,&type);
		type = NCLI_VECTOR;
		from_unibase (vi[res],vec,&type);
	}

	if (pi) uu_free (pi);
	if (vi) uu_free (vi);

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : S_push_cvonsf(cvkey,sfkey,cvlen,uvlst,
**                                xyzlst)
**       Store curve points in the list with the surf and curve keys.
**    PARAMETERS
**       INPUT  :
**          cvkey     - CV key
**          sfkey     - SF key where CV lies on.
**          ix        - list to store curve points on.
**          uvlst     - pointer to list of uv points
**          xyzlst    - pointer to list of xyz points
**       OUTPUT : none
**    RETURNS      :
**         UU_SUCCESS if no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_push_cvonsf(cvkey,sfkey,ix,uvsls,xyzls)
UU_KEY_ID cvkey,sfkey;
int ix;
UU_LIST *uvsls,*xyzls;
{
	int npt,status;
	UU_REAL d;
/*
.....Get length of curve
*/
	npt = UU_LIST_LENGTH(xyzls);
	if (npt > 0 )	
		d = um_getpolylen(npt,UU_LIST_ARRAY(xyzls));
	if (npt <= 0 || d < UM_DFUZZ) status = 122;
/*
.....Push curve record onto lists
*/
	else
	{
		status = 0;
		ncl_cvonsf_store(cvkey,sfkey,d,uvsls,xyzls);
		ncl_cvonsf_find(cvkey,sfkey,&NCL_cvonsf[ix]);
	}
	return(status);
}
