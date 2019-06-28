/*********************************************************************
**    NAME         :  nepocket4.c
**       CONTAINS: Routines for generating offset pocket perimeter from
**                 single trimmed surface.
**
**             ncl_get_nbrlst
**             ncl_genpocket_addindex
**             ncl_genpocket_check_index
**             ncl_genpocket_reset
**             ncl_genpocket_setall
**             ncl_genpocket_storegeo
**             nclf_genpocket_open
**             nclf_genpocket_offthk
**             nclf_genpocket_get_offthk
**             nclf_clear_ptlst
**             nclf_genpocket_getpt
**             ncl_genpocket_getnbrpt
**             ncl_genpocket_mark_open
**             nclf_genpocket_netsf
**             nclf_genpocket_cvs
**             nclf_genpocket_bound
**
**    COPYRIGHT 2013 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       nepocket4.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:41
*********************************************************************/
#include "mcrv.h"
#include "msrf.h"
#include "ncl.h"
#include "nccs.h"
#include "mgeom.h"
#include "umath.h"
#include "ulist.h"
#include "nclvx.h"
#include "ngeom.h"
#include "mdeval.h"
#include "mdrel.h"
#include "class.h"
#include "nclpockofs.h"

#define DEBUGON 0

extern UU_LIST cbxyz, cbnum, *NCL_uvlst;
extern int NCL_cpn, NCL_ubcopy;
extern struct NCL_cvonsf_rec *NCL_cvonsf[3];
static UU_KEY_ID Sbot_key;
static UU_LIST Sptlst,Sinds,Sends;
static UU_LOGICAL Spt_init = UU_FALSE, Sind_init = UU_FALSE;
static UU_LOGICAL Snbr_init = UU_FALSE;
static UU_LOGICAL Send_init = UU_FALSE;
static UU_REAL Soffthk = 0.;
static char Slabel[NCL_MAX_LABEL+1];
static int Stype,Snumpts;
static UU_KEY_ID Skey;
static UU_LOGICAL Sallfl;
static UU_LIST Snbrlst;
static UM_int2 IPT = NCLI_POINT;
static UM_int2 IVE = NCLI_VECTOR;

static void S_debug_nbr();
static void S_debug_nbr_pts();
static int S_sortind();
static int S_sortbnd();
static void S_sort_bpts();
static void S_add_bpts();
static void S_set_fvecs();
static void S_fix_cids();
static int S_neighbor_sfkeys();
static void S_buildnbr();
static int S_checknbrs();
static int S_create_netsf();
static int S_create_polyln();
static int S_calcrad();
static void S_trimslice();
static int S_pocket_offset();
static void S_debug_connect();
static int S_pocket_connect();
static void S_copy_nbr();
static void S_pocket_defold();
static int S_cvio_getpts();

/*********************************************************************
**    I_FUNCTION     : ncl_get_nbrlst(nbrlst)
**       Copy neighor list to given list.
**    PARAMETERS
**       INPUT  :
**          nbr - Neighbor boudnary data.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_get_nbrlst(nbrlst)
UU_LIST *nbrlst;
{
	uu_list_init(nbrlst,sizeof(ncl_nbr_struc),Snbrlst.cur_cnt,10);
	uu_list_push_list(nbrlst,&Snbrlst);
}

/*********************************************************************
**    E_FUNCTION     : ncl_genpocket_addindex(index)
**       Add an index to the list that defines open boundary regions.
**    PARAMETERS   
**       INPUT  : 
**          index - index of point or component in open region
**       OUTPUT : 
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_genpocket_addindex(start,end)
UM_int4 *start,*end;
{
	ncl_ind_struc range;

	if (!Sind_init)
	{
		Sind_init = UU_TRUE;
		uu_list_init(&Sinds,sizeof(ncl_ind_struc),25,25);
	}
	range.start = *start - 1; range.end = *end - 1; range.open = UU_TRUE;
	uu_list_push(&Sinds,&range);
}

/*********************************************************************
**    E_FUNCTION     : ncl_genpocket_check_index(ind)
**       Determine if an index defines an open boundary region.
**    PARAMETERS   
**       INPUT  : 
**          index - index of point or component in open region
**       OUTPUT : 
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL ncl_genpocket_check_index(ind)
int ind;
{
	int i;
	ncl_ind_struc *range;

	if (Sind_init)
	{
		range = (ncl_ind_struc *) UU_LIST_ARRAY(&Sinds);
		for (i=0;i<Sinds.cur_cnt;i++)
		{
			if ((ind >= range[i].start && ind <= range[i].end &&
				range[i].start <= range[i].end) ||
				(range[i].end < range[i].start && 
				(ind >= range[i].start || ind <= range[i].end)))
					return (UU_TRUE);
		}
	}
	return(UU_FALSE);
}

/*********************************************************************
**    E_FUNCTION     : ncl_genpocket_reset()
**       Empty lists and reset flags.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT : 
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_genpocket_reset()
{
	if (Sind_init)
	{
		Sind_init = UU_FALSE;
		uu_list_free(&Sinds);
	}
	if (Snbr_init)
	{
		Snbr_init = UU_FALSE;
		uu_list_free(&Snbrlst);
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_genpocket_setall()
**       Set all sides open flag.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT : 
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_genpocket_setall()
{
	Sallfl = UU_TRUE;
	Sind_init = UU_TRUE;
	uu_list_init(&Sinds,sizeof(ncl_ind_struc),1,1);
}

/*********************************************************************
**    E_FUNCTION  : ncl_genpocket_storegeo(inlabel,inkey,intyp,numpts)
**       Store the boundary geometry used to define pocket boundary.
**       The geometry will be used later when defining the open sides
**       of the pocket.
**    PARAMETERS   
**       INPUT  : 
**          inlabel - Label of input geometry.
**          inkey   - Key of input geometry (equals zero if point)
**          intyp   - Type of geometry given.
**                    8:  Composite curve
**                    14: Point.
**          numpts  - Number of points to use.
**       OUTPUT : 
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_genpocket_storegeo(inlabel,inkey,intyp,numpts)
UM_f77_str_ptr inlabel;
UM_int4 *inkey;
UM_int2 *intyp,*numpts;
{
	int nc;
	char *p;

	p = UM_cstr_of_f77_str(inlabel);
	nc = NCL_MAX_LABEL+1;
	strncpy(Slabel,p,nc);
	ul_strip_blanks(Slabel,&nc);
	Stype = *intyp;
	Skey = *inkey;
	Snumpts = *numpts;
	Sallfl = UU_FALSE;
}

/*********************************************************************
**    E_FUNCTION     : nclf_genpocket_open(tax,npts,ifl,ier)
**       Generate pocket boundary with open regions offset.
**    PARAMETERS   
**       INPUT  : 
**          ta0  - Tool axis vector.
**          vfl  - 0 = Don't use .5 diameter offset for open boundaries
**                 (VoluMill).
**       OUTPUT : 
**          npts - Number of boundary points generated.
**          ier  - Error flag.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclf_genpocket_open(ta0,npts,vfl,ier)
UM_real8 *ta0;
UM_int2 *npts,*vfl,*ier;
{
	int i,j,k,ninds,index,ifnd,maxpts,added,nopts,orient,status,ptcnt;
	struct NCL_nclpt_rec ptt;
	struct UM_compcrv_rec ccv;
	struct UM_crvdatabag crv;
	struct UM_evcrvout evout;
	UM_transf tf;
	UU_REAL trad,tol,fact;
	UM_vector zvec,dvec,*vcs;
	UM_coord pt,*pts,*uvs;
	ncl_bndpt_struc bpt,*bpts;
	ncl_nbr_struc nbr,*nbrs;
	ncl_ind_struc *inds,tind;
	UU_LIST ptlst,pptr,vptr,nbrlst;
	UM_int2 ncltype,nwds,ietype,ix,ifl;
	UM_int4 nclsub,isub,nclkey,ipg,iel;
	UM_real8 corner,diam;
	UM_f77_str ncllabel;
	char tbuf[80],sbuf[80];

	*ier = *npts = 0;
	gettol(&tol);
	ix = 264; getifl(&ix,&ifl);
	if (ifl == 1) fact = 25.4;
	else fact = 1.;
	UM_init_f77_str(ncllabel,Slabel,NCL_MAX_LABEL);
	uu_list_sort(&Sinds,S_sortind);
/*
.....Set up offset distance
*/
	isub = 28; getsc(&isub,&diam);
	if (*vfl == 1) trad = diam/2.;
	else trad = 0.;
	if (fabs(Soffthk) > UM_FUZZ) trad += Soffthk;
	trad /= fact;
	zvec[0] = ta0[0]; zvec[1] = ta0[1]; zvec[2] = ta0[2];
	to_unbs(zvec,zvec,&IVE);
/*
.....Fill in missing regions or define whole region if Sallfl is true.
*/
	ninds = Sinds.cur_cnt;
	if (Sallfl)
	{
		tind.start = 0;
		if (Stype == 8)
		{
			ccv.key = Skey;
			ncl_retrieve_data_fixed(&ccv);
			tind.end = ccv.no_cid-1;
		}
		else
			tind.end = Snumpts;
		tind.open = UU_TRUE;
		uu_list_push(&Sinds,&tind);
		ninds++;
	}
	else
	{
		inds = (ncl_ind_struc *)UU_LIST_ARRAY(&Sinds);
		added = 0;
		for (i=0;i<ninds;i++)
		{
			index = (i+1)%ninds;
			if (abs(inds[index].start-inds[i].end) > 1)
			{
/*
.....Make sure points or components will overlap properly.
*/
				if (Stype == 14)
				{
					tind.start = inds[i].end;
					tind.end = inds[index].start;
				}
				else
				{
					tind.start = inds[i].end+1;
					tind.end = inds[index].start-1;
				}
				tind.open = UU_FALSE;
				added++;
				uu_list_push(&Sinds,&tind);
				inds = (ncl_ind_struc *)UU_LIST_ARRAY(&Sinds);
			}
			else if (abs(inds[index].start-inds[i].end) == 1 &&
				Stype == 14 && Snumpts > 2)
			{
				tind.start = inds[i].end;
				tind.end = inds[index].start;
				tind.open = UU_FALSE;
				added++;
				uu_list_push(&Sinds,&tind);
				inds = (ncl_ind_struc *)UU_LIST_ARRAY(&Sinds);
			}
		}
		if (added == 0 && ninds == 1 && Stype != 14)
		{
			ccv.key = Skey;
			ncl_retrieve_data_fixed(&ccv);
			if (ccv.no_cid > 1)
			{
				if (inds[0].end < inds[0].start)
					j = inds[0].end + ccv.no_cid - inds[0].start;
				else
					j = inds[0].end - inds[0].start;
				if (j < ccv.no_cid-1)
				{
					tind.start = inds[0].end+1;
					tind.end = inds[0].start-1;
					tind.open = UU_FALSE;
					added++;
					uu_list_push(&Sinds,&tind);
				}
			}
		}
		ninds += added;
		uu_list_sort(&Sinds,S_sortind);
	}
	inds = (ncl_ind_struc *)UU_LIST_ARRAY(&Sinds);
	uu_list_init(&Snbrlst,sizeof(ncl_nbr_struc),ninds,10);
	Snbr_init = UU_TRUE;
	if (Stype == 14)
	{
/*
.....Find all points matching the given label and store.
*/
		uu_list_init(&ptlst,sizeof(UM_coord),Snumpts,10);
		for (i=1;i<=Snumpts;i++)
		{
			nclsub = i;
			ifnd = vxchk(UM_addr_of_f77_str(ncllabel),&nclsub,&nclkey,
				&ipg,&iel,&nwds,&ietype);
			if (ifnd == UU_SUCCESS)
			{
				ptt.key = nclkey;
				ncl_retrieve_data_fixed(&ptt);
				um_vctovc(ptt.pt,pt);
				uu_list_push(&ptlst,&pt);
			}
			else
				goto error;
		}
		pts = (UM_coord *)UU_LIST_ARRAY(&ptlst);
		orient = um_polygon_orient3D(UU_NULL,&ptlst,1,zvec,UU_NULL);
		pts = (UM_coord *)UU_LIST_ARRAY(&ptlst);
/*
.....Build neighbor record for the range of points.
*/
		for (i=0;i<ninds;i++)
		{
			if (inds[i].end < inds[i].start)
				maxpts = Snumpts + inds[i].start - inds[i].end  + 1;
			else
				maxpts = inds[i].end - inds[i].start + 1;
			nbr.bpts = (UU_LIST *) uu_malloc(sizeof(UU_LIST));
			uu_list_init(nbr.bpts,sizeof(ncl_bndpt_struc),maxpts,25);
			nbr.no_bpts = 0;
			if (inds[i].end < inds[i].start) inds[i].end += Snumpts;
			for (j=inds[i].start;j<=inds[i].end;j++)
			{
				index = j;
				if (index >= Snumpts) index -= Snumpts;
				um_vctovc(pts[index],bpt.pt);
				um_vctovc(zvec,bpt.nvec);
				bpt.remove = UU_FALSE;
				uu_list_push(nbr.bpts,&bpt);
				nbr.no_bpts++;
			}
			if (nbr.no_bpts == 1) goto error;
			if (inds[i].open) nbr.rad = trad;
			else nbr.rad = 0.;
			nbr.open = inds[i].open;
			uu_list_push(&Snbrlst,&nbr);
		}
		uu_list_free (&ptlst);
/*
.....Set forward vectors.
*/
		nbrs = (ncl_nbr_struc *)UU_LIST_ARRAY(&Snbrlst);
		for (i=0;i<Snbrlst.cur_cnt;i++)
		{
			bpts = (ncl_bndpt_struc *)UU_LIST_ARRAY(nbrs[i].bpts);
			for (j=0;j<nbrs[i].no_bpts-1;j++)
			{
				um_vcmnvc(bpts[j+1].pt,bpts[j].pt,bpts[j].fvec);
				um_unitvc(bpts[j].fvec,bpts[j].fvec);
			}
			um_unitvc(bpts[j-1].fvec,bpts[j].fvec);
		}
	}
	else
	{
/*
.....Evolve points along the composite curve. Evolve each component
.....separately so the points cover the entire component and each
.....component can be handled separately.
*/
		ccv.key = Skey;
		ncl_retrieve_data_fixed(&ccv);
		orient = um_polygon_orientation3D(&ccv,zvec);
		uu_list_init(&pptr,sizeof(UM_coord),100,100);
		uu_list_init(&vptr,sizeof(UM_coord),100,100);
		for (i=0;i<ninds;i++)
		{
/*
.....Switched it so every component is treated like a separate
.....neighbor - ASF 2/14/13.
			nbr.bpts = (UU_LIST *) uu_malloc(sizeof(UU_LIST));
			uu_list_init(nbr.bpts,sizeof(ncl_bndpt_struc),100,100);
			nbr.no_bpts = 0;
*/
			if (inds[i].end < inds[i].start || inds[i].end < 0)
				inds[i].end += ccv.no_cid;
/*
.....Build neighbor record for the range of compnents.
*/
			for (j=inds[i].start;j<=inds[i].end;j++)
			{
				nbr.bpts = (UU_LIST *) uu_malloc(sizeof(UU_LIST));
				uu_list_init(nbr.bpts,sizeof(ncl_bndpt_struc),100,100);
				nbr.no_bpts = 0;
				index = j;
				if (index >= ccv.no_cid) index -= ccv.no_cid;
				crv.key = ccv.cid[index].crvid;
				ncl_retrieve_data_fixed(&crv);
				uc_retrieve_transf(crv.key, tf);
				nopts = ncl_evolve_curve(&crv,tf,tol,&pptr,&vptr,UU_NULL,0);
				if (nopts < 2)
				{
					uu_list_free(&pptr);
					uu_list_free(&vptr);
					goto error;
				}
/*
.....Switch the direction so the points move in line with the original
.....curve. This ensures the offset is out.
*/
				if (ccv.cid[index].reverse)
				{
					ncl_revers1_list (nopts,0,(UU_LIST_ARRAY(&pptr)),1);
					ncl_revers1_list (nopts,0,(UU_LIST_ARRAY(&vptr)),2);
				}
				pts = (UM_coord *)UU_LIST_ARRAY(&pptr);
				vcs = (UM_vector *)UU_LIST_ARRAY(&vptr);
				for (k=0;k<nopts;k++)
				{
					bpt.remove = UU_FALSE;
					um_vctovc(pts[k],bpt.pt);
					um_vctovc(zvec,bpt.nvec);
					um_unitvc(vcs[k],bpt.fvec);
					uu_list_push(nbr.bpts,&bpt);
					nbr.no_bpts++;
				}
				UU_LIST_EMPTY(&pptr);
				UU_LIST_EMPTY(&vptr);
				if (inds[i].open) nbr.rad = trad + tol;
				else nbr.rad = 0.;
				nbr.open = inds[i].open;
				uu_list_push(&Snbrlst,&nbr);
			}
/*
			if (inds[i].open) nbr.rad = trad + tol;
			else nbr.rad = 0.;
			nbr.open = inds[i].open;
			uu_list_push(&Snbrlst,&nbr);
*/
		}
		uu_list_free(&pptr);
		uu_list_free(&vptr);
	}
/*
.....Offset and connect boundary points
*/
	nopts = 0;
	nbrs = (ncl_nbr_struc *)UU_LIST_ARRAY(&Snbrlst);
	for (i=0;i<Snbrlst.cur_cnt;i++)
	{
		if (nbrs[i].no_bpts <= 0) goto error;
		nopts += nbrs[i].no_bpts;
#if 0
	S_debug_nbr(&nbrs[i],UU_FALSE);
#endif
		if (nbrs[i].open)
		{ 
			bpts = (ncl_bndpt_struc *)UU_LIST_ARRAY(nbrs[i].bpts);
			for (j=0;j<nbrs[i].no_bpts;j++)
			{
				um_cross(bpts[j].nvec,bpts[j].fvec,dvec); um_unitvc(dvec,dvec);
				if (orient > 0) um_negvc(dvec,dvec);
				um_translate_point(bpts[j].pt,nbrs[i].rad,dvec,bpts[j].pt);
			}
		}
#if 0
	S_debug_nbr(&nbrs[i],UU_FALSE);
#endif
	}
/*
.....Pushing the Neighbor list onto another list
.....Does not work, since the Bpt list inside the Neighbor list
.....points to the same memory in the static Snbrlst and
.....local nbrlst arrays
.....This causes the count of points to be incorrect
*/
/*
	uu_list_init(&nbrlst,sizeof(ncl_nbr_struc),Snbrlst.cur_cnt,5);
	uu_list_push_list(&nbrlst,&Snbrlst);
*/
	S_pocket_connect (0,&Snbrlst,Snbrlst.cur_cnt,zvec,2,&nopts);
#if 0
	nbrs = (ncl_nbr_struc *)UU_LIST_ARRAY(&Snbrlst);
	for (i=0;i<Snbrlst.cur_cnt;i++) S_debug_nbr(&nbrs[i],UU_FALSE);
#endif
/*	uu_list_free(&nbrlst);*/
	*npts = nopts;
	goto done;
error:
	*ier = 1;
	ncl_genpocket_reset();
done:
	return;
}

/*********************************************************************
**    E_FUNCTION     : nclf_genpocket_offthk(thk)
**       Set the open boundary offset thick value for pocket boundary.
**    PARAMETERS   
**       INPUT  : 
**          thk - Open boundary offset thick value.
**       OUTPUT : 
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclf_genpocket_offthk(thk)
UM_real8 *thk;
{
	Soffthk = *thk;
}

/*********************************************************************
**    E_FUNCTION     : nclf_genpocket_get_offthk(thk)
**       Get the open boundary offset thick value for pocket boundary.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT : 
**          thk - Open boundary offset thick value.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclf_genpocket_get_offthk(thk)
UM_real8 *thk;
{
	*thk = Soffthk;
}

/*********************************************************************
**    E_FUNCTION     : nclf_clear_ptlst()
**       Free pocket boundary point list memory.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT : 
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclf_clear_ptlst()
{
	if (Spt_init)
	{
		Spt_init = UU_FALSE;
		uu_list_free(&Sptlst);
	}
}

/*********************************************************************
**    E_FUNCTION     : nclf_genpocket_getpt(ind,buf)
**       Retrieve pocket boundary point at given index.
**    PARAMETERS   
**       INPUT  : 
**          ind - Index of point.
**       OUTPUT : 
**          buf - Point data.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclf_genpocket_getpt(ind,buf)
UM_int2 *ind;
UM_real8 *buf;
{
	int ix = (*ind) - 1;
	UM_coord *pts = (UM_coord *)UU_LIST_ARRAY(&Sptlst);
	buf[0] = pts[ix][0]; buf[1] = pts[ix][1]; buf[2] = pts[ix][2];
}

/*********************************************************************
**    E_FUNCTION     : ncl_genpocket_getnbrpt(ix1,ix2,buf,open)
**       Retrieve pocket boundary point from original neighbor lists
**       that contain open flag.
**    PARAMETERS   
**       INPUT  : 
**          ix1      - Index of span to retrieve point from.  Should be
**                     set to 0 on first call and then not changed by the
**                     calling routine.
**          ix2      - Index into span to retrieve point from.  Should be
**                     set to 0 on first call and then not changed by the
**                     calling routine.
**       OUTPUT : 
**          ix1      - Updated pointer.
**          ix2      - Updated pointer.
**    RETURNS      : UU_FAILURE if there are no more points.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_genpocket_getnbrpt(ix1,ix2,buf,open)
int *ix1,*ix2;
UU_REAL *buf;
UU_LOGICAL *open;
{
	ncl_nbr_struc *nbrs;
	ncl_bndpt_struc *bpts;
/*
.....Initialize routine
*/
	nbrs = (ncl_nbr_struc *)UU_LIST_ARRAY(&Snbrlst);
	if (*ix1 >= Snbrlst.cur_cnt) return(UU_FAILURE);
/*
.....Point to correct span
*/
	if (*ix2 >= nbrs[*ix1].no_bpts)
	{
		*ix1 = *ix1 + 1;
		if (*ix1 >= Snbrlst.cur_cnt) return(UU_FAILURE);
		*ix2 = 0;
	}
/*
.....Return point
*/
	bpts = (ncl_bndpt_struc *)UU_LIST_ARRAY(nbrs[*ix1].bpts);
	um_vctovc(bpts[*ix2].pt,buf);
	*open = nbrs[*ix1].open;
	*ix2 = *ix2 + 1;
/*
.....End of routine
*/
	return(UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : ncl_genpocket_mark_open()
**       Sets the open indexes for the open sides in the Sptlst array.
**    PARAMETERS  none
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_genpocket_mark_open()
{
	int i,j,k,ist,ien,kend,no_nbr,nbix0,nbix1;
	UU_LOGICAL found,flag;
	UM_coord *pts,prev;
	ncl_nbr_struc *nbrs;
	ncl_bndpt_struc *bpts;
	char sbuf[80];
	UU_LOGICAL first = UU_TRUE;
	typedef int SPT[3];
	SPT *nbpt;
/*
.....Initialize routine
*/
	nbrs = (ncl_nbr_struc *)UU_LIST_ARRAY(&Snbrlst);
	pts = (UM_coord *)UU_LIST_ARRAY(&Sptlst);
	no_nbr = UU_LIST_LENGTH(&Snbrlst);
	ist = -1; ien = -1;
	kend = UU_LIST_LENGTH(&Sptlst);
	UU_LIST_EMPTY(&Sinds);
	nbix0 = nbix1 = 0;
	bpts = (ncl_bndpt_struc *)UU_LIST_ARRAY(nbrs[0].bpts);
	nbpt = (SPT *)uu_malloc(no_nbr*sizeof(SPT));
	for (i=0;i<no_nbr;i++)
	{
		nbpt[i][0] = nbpt[i][1] = -1;
		nbpt[i][2] = 0;
	}
#if 0
	S_debug_nbr_pts(nbrs);
#endif
/*
.....Find the points in the final point list
.....that mark the start and end of each neighbor
*/
	nbpt[0][0] = 0; nbpt[0][2] = 1;
	nbpt[no_nbr-1][1] = kend - 1; nbpt[no_nbr-1][2] = 2;
	um_vctovc(bpts[0].pt,prev);
	for (k=1;k<kend-1;k++)
	{
		found = UU_FALSE;
		do
		{
			flag = UU_FALSE;
			bpts = (ncl_bndpt_struc *)UU_LIST_ARRAY(nbrs[nbix0].bpts);
			for (j=nbix1;j<UU_LIST_LENGTH(nbrs[nbix0].bpts);j++)
			{
				flag = um_cceqcc(pts[k],bpts[j].pt);
				if (flag)
				{
					if (j == 0) nbpt[nbix0][2] = nbpt[nbix0][2] | 1;
					if (j == UU_LIST_LENGTH(nbrs[nbix0].bpts)-1)
						nbpt[nbix0][2] = nbpt[nbix0][2] | 2;
				}
				else
				{
					flag = um_point_in_segment(pts[k],prev,bpts[j].pt);
					if (flag)
					{
						if (j == UU_LIST_LENGTH(nbrs[nbix0].bpts)-1)
							nbpt[nbix0][2] = nbpt[nbix0][2] | 4;
					}
				}

				um_vctovc(bpts[j].pt,prev);
				if (flag)
				{
					found = UU_TRUE;
					if (nbpt[nbix0][0] == -1)
					{
						nbpt[nbix0][0] = k;
						if (nbix0 == no_nbr-1) break;
					}
					if (nbix0 != no_nbr-1) nbpt[nbix0][1] = k;
					nbix1 = j;
					break;
				}
			}
			if (found) break;
			nbix0++; nbix1 = 0;
		} while (!found && nbix0 < no_nbr);
		if (nbix0 >= no_nbr) nbix0 = nbix1 = 0;
	}
/*
.....Make sure all neighbors are assigned end points
*/
				
	do
	{
		found = UU_FALSE;
		for (i=0;i<no_nbr;i++)
		{
			if (nbpt[i][0] == -1)
			{
				if (nbpt[i-1][2] & 2) nbpt[i][0] = nbpt[i-1][1] + 1;
				else nbpt[i][0] = nbpt[i-1][1];
				found= UU_TRUE;
			}
			if (nbpt[i][1] == -1)
			{
				if (nbpt[i+1][2] & 1)
				{
					j = nbpt[i+1][0] - 1;
					if (j == nbpt[i][0]) j++;
					nbpt[i][2] = nbpt[i][2] | 2;
				}
				else j = nbpt[i+1][0];

				if (j == -1) j = nbpt[i][0] + 1;

				if (j != nbpt[i][0])
				{
					nbpt[i][1] = j;
					found= UU_TRUE;
				}
			}
		}
	} while (found);

	for (i=0;i<no_nbr-1;i++)
	{
		if (nbpt[i][1] != nbpt[i+1][0])
		{
			k = nbpt[i+1][0] - nbpt[i][1];
			bpts = (ncl_bndpt_struc *)UU_LIST_ARRAY(nbrs[i].bpts);
			um_vctovc(bpts[UU_LIST_LENGTH(nbrs[i].bpts)],prev);
			bpts = (ncl_bndpt_struc *)UU_LIST_ARRAY(nbrs[i+1].bpts);
			if (um_cceqcc(prev,bpts[0]))
				nbpt[i+1][0] = nbpt[i][1];
			else if (k >= 2)
				nbpt[i][1] = nbpt[i+1][0] = nbpt[i][1] + k/2;
			else if (nbpt[i][2] & 2 || nbpt[i][2] & 4 ||
					nbpt[i+1][0] == nbpt[i+1][1])
				nbpt[i+1][0] = nbpt[i][1];
			else
				nbpt[i][1] = nbpt[i+1][0];
		}
	}
/*
.....Mark open sides (point indexes)
*/
	for (i=0;i<no_nbr;i++)
	{
		if (nbrs[i].open)
		{
			if (ist == -1)
			{
				if (i == 0) ist = nbpt[i][0];
				else ist = nbpt[i-1][1];
			}
			ien = nbpt[i][1];
		}
		else if (ist != -1)
		{
			ist++; ien++;
			ncl_genpocket_addindex(&ist,&ien);
			ist = ien = -1;
		}
	}
	if (ist != -1)
	{
		ist++; ien++;
		ncl_genpocket_addindex(&ist,&ien);
	}
	uu_free(nbpt);
}

/*********************************************************************
**    E_FUNCTION     : int nclf_genpocket_netsf(sfkey,errfl)
*********************************************************************/
void nclf_genpocket_netsf(sfkey,errfl)
UM_int4 *sfkey;
UM_int2 *errfl;
{
	int i,status,nkeys,no_keys,no_pts,color,err;
	struct NCL_trimsf_rec sf;
	UU_LIST nbr_list;
	UU_KEY_ID key;
	UM_coord *ppts,pts[2];
/*
.....Find neighbors of given surface.
*/
	*errfl = 0;
	sf.key = key = *sfkey;
	ncl_retrieve_data_fixed(&sf);
	nkeys = ((sf.no_ibndykey+1)/2) + 1;
	no_keys = 0;
	uu_list_init(&nbr_list,sizeof(ncl_nbr_struc),10,10);
	for (i=0;i<nkeys;i++)
	{
		no_keys += S_neighbor_sfkeys (sfkey,0,&nbr_list,2,i,&err);
		if (err != 0) goto failed;
	}
	if (no_keys == 0) goto failed;
	status = S_create_netsf(&key,&nbr_list,no_keys);
	*sfkey = key;
	if (status != UU_SUCCESS) goto failed;
	goto done;
failed:;
	*errfl = 466;
done:;
	uu_list_free(&nbr_list);
}

/*********************************************************************
**    I_FUNCTION     : int nclf_genpocket_cvs(sfkey,errfl)
*********************************************************************/
void nclf_genpocket_cvs(sfkey,vkey,errfl)
UM_int4 *sfkey,*vkey;
UM_int2 *errfl;
{
	int i,j,status,nkeys,total,no_keys,npts,err,rel;
	struct NCL_trimsf_rec sf;
	ncl_nbr_struc *nbr;
	UU_LOGICAL openfl;
	UU_LIST nbr_list,pt_list;
	UU_KEY_ID key,cvkey;
	struct NCL_vector_rec vec;
	struct NCL_nclpv_rec pv;
	UM_vector zvec;
	UM_coord *ppts,pts[2];
	UM_int2 lfl_77;
/*
.....Find neighbors of given surface.
*/
	*errfl = 0;
	sf.key = key = *sfkey;
	ncl_retrieve_data_fixed(&sf);
	ur_retrieve_data_relnum(*vkey, &rel);
	if (rel == NCL_VECTOR_REL)
	{
		vec.key = *vkey;
		ncl_retrieve_data_fixed(&vec);
		um_vctovc(vec.vec,zvec);
	}
	else
	{
		pv.key = *vkey;
		ncl_retrieve_data_fixed(&pv);
		um_vctovc(pv.ve,zvec);
	}
	fr_unbs(zvec,zvec,&IVE);
	nkeys = ((sf.no_ibndykey+1)/2) + 1;
	total = 0;
	for (i=0;i<nkeys;i++)
	{
		lfl_77 = 1;
		stunlb (&lfl_77);
		no_keys = 0;
		no_keys += S_neighbor_sfkeys (sfkey,zvec,&nbr_list,1,i,&cvkey,&err);
		if (err != 0) goto failed;
		no_keys += S_checknbrs(key,zvec,cvkey,&nbr_list,i,&err);
		if (err != 0) goto failed;
		total += no_keys;
		if (no_keys == 0) continue;
/*
.....Generate polylines based on new curve.
*/
		status = S_create_polyln(key,&nbr_list,no_keys);
		*sfkey = key;
		if (status != UU_SUCCESS) goto failed;
		nbr = (ncl_nbr_struc *)UU_LIST_ARRAY(&nbr_list);
		status = S_pocket_offset(key,&nbr_list,i,UU_TRUE,&openfl);
		if (status != UU_SUCCESS) goto failed;
		stunlb (&lfl_77);
		if (i > 0 && openfl) continue;
		status = S_pocket_connect(key,&nbr_list,no_keys,zvec,0,&npts);
		uu_list_free(&nbr_list);
	}
	if (total == 0) goto failed;
	goto done;
failed:;
	*errfl = 466;
done:;
	uu_list_free(&nbr_list);
}
/*********************************************************************
**    I_FUNCTION     : void nclf_genpocket_bound(sfkey,ta0,binx,npts,ifl,errfl)
**      Calculate the pocket boundary based on the neighboring 
**      surfaces.
**    PARAMETERS
**       INPUT  :
**          sfkey - Key of surface to modify.
**          ta0   - Tool axis vector parameters.
**          binx  - Surface boudnary index.
**          ifl   - 0 = Don't use .5 diameter offset for open boundaries
**                  (VoluMill).
**       OUTPUT :
**          npts  - Number of boundary points.
**          errfl - Returns non-zero if an error is encountered.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclf_genpocket_bound(sfkey,ta0,binx,npts,ifl,errfl)
UM_int4 *sfkey;
UM_real8 *ta0;
UM_int2 *binx,*npts,*ifl,*errfl;
{
	int i,status,ix,no_keys,nopts,err,ofl;
	ncl_nbr_struc *snbrs,*nbrs,nbr;
	UU_LIST nbr_list,pt_list,*list;
	UU_KEY_ID key,cvkey;
	UM_coord *ppts,pts[2];
	UM_vector zvec;
	UM_int2 lfl_77;
	UU_LOGICAL openfl,init = UU_FALSE;
/*
.....Find neighbors of pocket bottom.
*/
	*errfl = 0;
	ix = (*binx) - 1;
	key = *sfkey;
	lfl_77 = 1;
	stunlb (&lfl_77);
	*npts = no_keys = 0;
/*
.....Put the tool axis into the same orientation
.....as the part
*/
	to_unbs(ta0,zvec,&IVE);
	if (ix == 0)
	{
		no_keys += S_neighbor_sfkeys (sfkey,zvec,&Snbrlst,1,ix,&cvkey,&err);
		list = &Snbrlst;
		Snbr_init = UU_TRUE;
	}
	else
	{
		no_keys += S_neighbor_sfkeys (sfkey,zvec,&nbr_list,1,ix,&cvkey,&err);
		list = &nbr_list;
		init = UU_TRUE;
	}
	if (err != 0) goto failed;
#if 0
status = S_create_polyln(key,list,no_keys);
#endif
	no_keys += S_checknbrs(key,zvec,cvkey,list,ix,&err);
	if (err != 0) goto failed;
/*
.....Offset boundary.
*/
	ofl = *ifl == 1;
	status = S_pocket_offset(key,list,ix,ofl,&openfl);
	if (ix == 0)
	{
		init = UU_TRUE;
		snbrs = (ncl_nbr_struc *) UU_LIST_ARRAY(&Snbrlst);
		uu_list_init(&nbr_list,sizeof(ncl_nbr_struc),Snbrlst.cur_cnt,10);
		for (i=0;i<Snbrlst.cur_cnt;i++)
		{
			S_copy_nbr(&snbrs[i],&nbr);
			uu_list_push(&nbr_list,&nbr);
		}
	}
	if (openfl && ix > 0) goto done;
	if (status != UU_SUCCESS) goto failed;
/*
.....Make offset boundary connections.
*/
	status = S_pocket_connect(key,&nbr_list,no_keys,zvec,2,&nopts);
	if (status != UU_SUCCESS) goto failed;
	*npts = nopts;
	goto done;
failed:;
	*errfl = 466;
done:;
	stunlb (&lfl_77);
	if(init) uu_list_free(&nbr_list);
}

/*********************************************************************
**    I_FUNCTION     : S_debug_nbr (sfkey,nbr_list)
**       Prints all data stored for a surface in its entry in the
**       neighbor list.
**    PARAMETERS
**       INPUT  :
**          nbr - Neighbor boudnary data.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_debug_nbr(nbr,flag)
ncl_nbr_struc *nbr;
int flag;
{
	int i;
	ncl_bndpt_struc *bpt,*bpt2;
	char sbuf[80];

	if (flag)
	{
		sprintf(sbuf,"$$ Key = %d",nbr->key);
		NclxDbgPstr(sbuf);
		sprintf(sbuf,"$$ Number of points = %d",nbr->no_bpts);
		NclxDbgPstr(sbuf);
		sprintf(sbuf,"$$ cvu0 = %lf",nbr->cvu0);
		NclxDbgPstr(sbuf);
		sprintf(sbuf,"$$ cvu1 = %lf",nbr->cvu1);
		NclxDbgPstr(sbuf);
		sprintf(sbuf,"$$ rad = %lf",nbr->rad);
		NclxDbgPstr(sbuf);
		if (nbr->orient == 1)
			sprintf(sbuf,"$$ CCLW Orientation");
		else
			sprintf(sbuf,"$$ CLW Orientation");
		NclxDbgPstr(sbuf);	
	}
	bpt = (ncl_bndpt_struc *)UU_LIST_ARRAY(nbr->bpts);
	bpt2 = (ncl_bndpt_struc *)UU_LIST_ARRAY(nbr->bpts);
	bpt2++;
	for (i=0;i<nbr->no_bpts;i++,bpt++,bpt2++)
	{
		if (UM_MAG(bpt->pt) > 1.e7) continue;
		if (flag)
		{
			sprintf(sbuf,"$$ cvu = %lf",bpt->cvu);
			NclxDbgPstr(sbuf);
			sprintf(sbuf,"$$ sfu,sfv = %lf,%lf",bpt->sfu,bpt->sfv);
			NclxDbgPstr(sbuf);
		}
		sprintf(sbuf,"PT/%lf,%lf,%lf",bpt->pt[0],bpt->pt[1],bpt->pt[2]);
		NclxDbgPstr(sbuf);
		if (UM_MAG(bpt->fvec) < 1.e7 && UM_MAG(bpt->fvec) > UM_FUZZ)
		{
			sprintf(sbuf,"PV/%lf,%lf,%lf,%lf,%lf,%lf",bpt->pt[0],bpt->pt[1],
				bpt->pt[2],bpt->fvec[0],bpt->fvec[1],bpt->fvec[2]);
			NclxDbgPstr(sbuf);
		}
		else
			NclxDbgPstr("$$ BAD FORWARD VECTOR");
		if (UM_MAG(bpt->nvec) < 1.e7 && UM_MAG(bpt->nvec) > UM_FUZZ)
		{
			sprintf(sbuf,"PV/%lf,%lf,%lf,%lf,%lf,%lf",bpt->pt[0],bpt->pt[1],
				bpt->pt[2],bpt->nvec[0],bpt->nvec[1],bpt->nvec[2]);
			NclxDbgPstr(sbuf);
		}
		else
			NclxDbgPstr("$$ BAD FORWARD VECTOR");
		if (i < nbr->no_bpts-1 && UM_MAG(bpt2->pt) < 1.e7 &&
			um_dcccc(bpt->pt,bpt2->pt) > 0.001)
		{
			sprintf(sbuf,"LN/%lf,%lf,%lf,%lf,%lf,%lf",bpt->pt[0],bpt->pt[1],
				bpt->pt[2],bpt2->pt[0],bpt2->pt[1],bpt2->pt[2]);
			NclxDbgPstr(sbuf);
		}
	}
	NclxDbgPstr("\n");
}

/*********************************************************************
**    I_FUNCTION     : S_debug_nbr_pts(nbr)
**       Prints the neighbor number, open flag, and list of points in
**       the provided neighbor list.
**    PARAMETERS
**       INPUT  :
**          nbr - Neighbor boundary data.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_debug_nbr_pts(nbrs)
ncl_nbr_struc *nbrs;
{
	int n,j;
	char sbuf[80];
	ncl_bndpt_struc *bpts;

	for (n=0;n<UU_LIST_LENGTH(&Snbrlst);n++)
	{
		bpts = (ncl_bndpt_struc *) UU_LIST_ARRAY(nbrs[n].bpts);
		sprintf(sbuf,"Neighbor %d - %d points - open = %d",n,nbrs[n].no_bpts,
			nbrs[n].open);
		NclxDbgPstr(sbuf);
		for (j=0;j<nbrs[n].no_bpts;j++)
		{
			sprintf(sbuf,"pt/%8.3f,%8.3f,%8.3f",bpts[j].pt[0],bpts[j].pt[1],
				bpts[j].pt[2]);
			NclxDbgPstr(sbuf);
		}
	}
}

/*********************************************************************
**    I_FUNCTION     : S_sortind(dat1,dat2)
**       Comparison routine for open region indices.
**    PARAMETERS
**       INPUT  :
**          dat1,dat2 - Data to compare.
**       OUTPUT :
**          none
**    RETURNS      : Result from comparing start/end index values.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_sortind(dat1,dat2)
ncl_ind_struc *dat1,*dat2;
{
	if (dat1->start < dat2->start) return(-1);
	else if (dat1->start > dat2->start) return(1);
	else return(0);
}

/*********************************************************************
**    I_FUNCTION     : S_sortbnd(dat1,dat2)
**       Comparison routine for boundary point structure ncl_bndpt_struc.
**    PARAMETERS
**       INPUT  :
**          dat1,dat2 - Data to compare.
**       OUTPUT :
**          none
**    RETURNS      : Result from comparing u parameters.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_sortbnd(dat1,dat2)
ncl_bndpt_struc *dat1,*dat2;
{
	if (dat1->cvu < dat2->cvu) return(-1);
	else if (dat1->cvu > dat2->cvu) return(1);
	else return(0);
}

/*********************************************************************
**    I_FUNCTION     : S_sortnbr(dat1,dat2)
**       Comparison routine for neighbor surface record structure
**       ncl_nbr_struc.
**    PARAMETERS
**       INPUT  :
**          dat1,dat2 - Data to compare.
**       OUTPUT :
**          none
**    RETURNS      : Result from comparing u0 parameters.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_sortnbr(dat1,dat2)
ncl_nbr_struc *dat1,*dat2;
{
	UU_LOGICAL same = (fabs(dat1->cvu0-dat2->cvu0) < UM_DFUZZ);
	if (dat1->cvu0 < dat2->cvu0 || (same && dat1->no_bpts == 1)) return(-1);
	else if (dat1->cvu0 > dat2->cvu0 || (same && dat2->no_bpts == 1)) return(1);
	else return(0);
}

/*********************************************************************
**    I_FUNCTION     : S_sort_bpts(nbr_list)
**       Sort the boundary points near the ends of the closed boundary
**       curve.  Points not near the ends should already be in order
**       when they are returned from the boundary matching routines.
**       The points near the end could be on either side and should be
**       in increasing order wrt to their u parameters. This will put
**       the points out of order wrt to the surface boundary.
**    PARAMETERS
**       INPUT  :
**          nbr_list - List of surface neighbor data.
**       OUTPUT :
**          nbr_list - Updated list of surface neighbor data.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_sort_bpts(nbr_list)
UU_LIST *nbr_list;
{
	int i,j,k,ind,ind2,nbr_ind[2],no_nbrs,no_bpts;
	UU_REAL tu;
	UM_vector tvec,tvec2;
	ncl_bndpt_struc *bpt,*bpt1,*bpt2,*bpts;
	ncl_nbr_struc *nbrs,*nbr;
	UU_LOGICAL check,set0,set1,checks[] = {UU_FALSE,UU_FALSE};
	UU_LIST tbpt_list;
	char tbuf[80];
/*
.....Currently only check for out of order points when there are points
.....near the end of the curve in the list.
*/
	no_nbrs = nbr_list->cur_cnt;
	nbrs = (ncl_nbr_struc *) UU_LIST_ARRAY(nbr_list);
	ind = 0;
	for (i=0;i<no_nbrs && ind<2;i++)
	{
		if (nbrs[i].no_bpts < 2) continue;
		if (nbrs[i].cvu0 < UM_FUZZ || nbrs[i].cvu1 > 1.-UM_FUZZ)
		{
			checks[ind] = UU_TRUE;
			nbr_ind[ind++] = i;
		}
	}
	if (!checks[0] && !checks[1]) return;
/*
.....Fix cvu values if necessary.
*/
	if (ind > 1)
	{
		for (i=0;i<2;i++)
		{
			nbr = &nbrs[nbr_ind[i]];
			bpts = (ncl_bndpt_struc *)UU_LIST_ARRAY(nbr->bpts);
			no_bpts = nbr->no_bpts;
			set0 = set1 = UU_FALSE;
			for (j=0;j<no_bpts;j++)
			{
				if (bpts[j].cvu > UM_FUZZ && bpts[j].cvu < 0.25)
					set0 = UU_TRUE;
				else if (bpts[j].cvu < 1.-UM_FUZZ && bpts[j].cvu > 0.75)
					set1 = UU_TRUE;
				else if (bpts[j].cvu < UM_FUZZ) ind = j;
				else if (bpts[j].cvu > 1.-UM_FUZZ) ind = j;
			}
			if ((!set0 && set1) || (set0 && !set1))
			{
				if (set0 && !set1) bpts[ind].cvu = 0.;
				else if (!set0 && set1) bpts[ind].cvu = 1.;
				uu_list_sort(nbr->bpts,S_sortbnd);
				bpts = (ncl_bndpt_struc *)UU_LIST_ARRAY(nbr->bpts);
				nbr->cvu0 = bpts[0].cvu;
				nbr->cvu1 = bpts[no_bpts-1].cvu;
			}
		}
	}
/*
....Loop through neighbors and search for an overlap of u parameters.
*/
	for (k=0;k<2;k++)
	{
		if (!checks[k]) continue;
		nbr = &nbrs[nbr_ind[k]];
		bpts = (ncl_bndpt_struc *)UU_LIST_ARRAY(nbr->bpts);
		no_bpts = nbr->no_bpts;
		for (i=1;i<no_bpts;i++)
		{
			ind = i;	ind2 = i-1;
/*
.....Check for overlap.
*/
			check = UU_FALSE;
			tu = (bpts[ind].cvu + bpts[ind2].cvu)/2.;
			for (j=0;j<no_nbrs;j++)
			{
				if (j == nbr_ind[k]) continue;
				if (tu > nbrs[j].cvu0 && tu < nbrs[j].cvu1)
				{
					check = UU_TRUE;
					ind = ind2;
					break;
				}
			}
/*
.....An overlap exists so the points must be out of order.  The mismatch
.....should be across the end of the curve, so the points just need to 
.....be shifted to the beginning starting at the mismatch.
*/
			if (check)
			{
				uu_list_init(&tbpt_list,sizeof(ncl_bndpt_struc),no_bpts,5);
				bpt = (ncl_bndpt_struc *)UU_LIST_ARRAY(nbr->bpts);
				for (j=0;j<no_bpts;j++)
				{
					ind = (j+i)%no_bpts;
					uu_list_push(&tbpt_list,&(bpt[ind]));
				}
				UU_LIST_EMPTY(nbr->bpts);
				uu_list_push_list(nbr->bpts,&tbpt_list);
				uu_list_free(&tbpt_list);
				nbr->cvu0 = bpt[0].cvu;
				nbr->cvu1 = bpt[no_bpts-1].cvu;
				break;
			}
		}
	}
}

/*********************************************************************
**    I_FUNCTION     : S_add_bpts(nbr_list,sf,sftf,cv,cvtf,nzvec,tol)
**       Add points to boundary if the current points do not follow the
**       boudnary within tolerance.
**    PARAMETERS
**       INPUT  :
**          nbr_list - List of surface neighbor data.
**          sf       - Pocket bottom surface.
**          sftf     - Tranformation matrix for surface.
**          cv       - Boundary curve data.
**          cvtf     - Boundary curve transformation matrix.
**          nzvec    - Upward direction vector.
**          tol      - tolerance.
**       OUTPUT :
**          nbr_list - Update list of surface neighbor data.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_add_bpts(nbr_list,sf,sftf,cv,cvtf,nzvec,tol)
UU_LIST *nbr_list;
struct UM_trimsf_rec *sf;
struct UM_compcrv_rec *cv;
UM_transf sftf,cvtf;
UM_vector nzvec;
UU_REAL tol;
{
	int i,j,ind,ind2,nbr_ind,no_nbrs,no_bpts,count;
	UU_REAL u,v,unew,uold,umax,side,dis,dis2,tolsq;
	UM_coord pt,pto;
	UM_vector nvec,tvec;
	ncl_bndpt_struc bpt,*bpts,*bpts2;
	ncl_nbr_struc *nbrs,*nbr;
	UU_LIST tbpt_list;
	struct UM_evcrvout evout;
	UM_int2 errfl;
	UM_transf sftf2;
	struct NCL_fixed_databag sf2;
	char tbuf[80];
	
	tolsq = tol*tol;
	no_nbrs = nbr_list->cur_cnt;
	nbrs = (ncl_nbr_struc *) UU_LIST_ARRAY(nbr_list);
	uc_init_evcrvout (cv,&evout);
	for (i=0;i<no_nbrs;i++)
	{
		nbr = &nbrs[i];
		if (nbr->check == UU_FALSE) continue;
		sf2.key = nbr->key;
		ncl_retrieve_data_fixed(&sf2);
		uc_retrieve_transf(sf2.key,sftf2);
		bpts = (ncl_bndpt_struc *)UU_LIST_ARRAY(nbr->bpts);
		if (i < no_nbrs-1)
			bpts2 = (ncl_bndpt_struc *)UU_LIST_ARRAY(nbrs[i+1].bpts);
		else
			bpts2 = (ncl_bndpt_struc *)UU_LIST_ARRAY(nbrs[0].bpts);
		no_bpts = nbr->no_bpts;
		for (j=0;j<no_bpts;j++)
		{
			unew = uold = bpts[j].cvu;
			uc_evcrv (UM_ALL,unew,cv,cvtf,&evout);
			if (j < no_bpts-1)
			{
				umax = bpts[j+1].cvu;
				um_vctovc(bpts[j+1].pt,pt);
			}
			else
			{
				if (i < no_nbrs-1) umax = nbrs[i+1].cvu0;
				else umax = nbrs[0].cvu0;
				if (umax > nbr->cvu1) umax = nbr->cvu1;
				um_vctovc(bpts2[0].pt,pt);
			}
			count = 0;
			while (uold < umax && count++ < 500)
			{
				ncl_crv_nextpt_at_tol (cv,cvtf,tol,uold,&unew,umax,
					&evout,1);
				um_cctmtf(evout.cp,sftf,evout.cp);
				dis = um_sqdis(evout.cp,pt);
				dis2 = um_sqdis(evout.cp,bpts[j].pt);
				if ((unew < umax && dis > 25.*tolsq && dis2 > 25.*tolsq) ||
					no_bpts == 2)
				{
/*
.....Add middle point in case the ends are not shaped for easy radius
.....calculation so there is likely a "good" representation at at least
.....one point.
*/
					bpt.remove = UU_FALSE;
					if (unew >= umax || dis <= 25.*tolsq || dis2 <= 25.*tolsq)
					{
						unew = (uold + umax)/2.;
						uc_evcrv (UM_ALL,unew,cv,cvtf,&evout);
						dis = um_sqdis(evout.cp,pt);
						dis2 = um_sqdis(evout.cp,bpts[j].pt);
						if (dis <= 25.*tolsq || dis2 <= 25.*tolsq)
							break;
						bpt.remove = UU_TRUE;
					}
					u = 0.5; v = 0.5; side = 0.;
					ncl_pt_project_sf(evout.cp,sf,sftf,&u,&v,&side,0,UU_NULL,0,
						UU_NULL,tolsq,pto,nvec,&errfl);
					um_vctovc(pto,bpt.pt); um_vctovc(pto,pt);
					if (side < 0) um_negvc(nvec,nvec);
					um_unitvc(nvec,bpt.nvec);
					if (UM_DOT(nzvec,bpt.nvec) < 0.)
						um_negvc(bpt.nvec,bpt.nvec);
					bpt.cvu = unew;
					u = 0.5; v = 0.5; side = 0.;
					ncl_pt_project_sf(evout.cp,&sf2,sftf2,&u,&v,&side,0,UU_NULL,0,
						UU_NULL,tolsq,pto,nvec,&errfl);
					bpt.sfu = u;
					bpt.sfv = v;
					uu_list_insert(nbr->bpts,j+1,&bpt);
					bpts = (ncl_bndpt_struc *)UU_LIST_ARRAY(nbr->bpts);
					no_bpts++; j++;
				}
				uold = unew;
			}
		}
		if (no_nbrs == 1 && bpts[no_bpts-1].cvu - bpts[no_bpts-2].cvu > 0.01)
		{
			dis = (bpts[no_bpts-1].cvu-bpts[no_bpts-2].cvu)/10.;
			unew = bpts[no_bpts-2].cvu+dis;
			for (j=0;j<8;j++)
			{
				uc_evcrv (UM_ALL,unew,cv,cvtf,&evout);
				u = 0.5; v = 0.5; side = 0.;
				ncl_pt_project_sf(evout.cp,sf,sftf,&u,&v,&side,0,UU_NULL,0,
					UU_NULL,tolsq,pto,nvec,&errfl);
				um_vctovc(pto,bpt.pt); um_vctovc(pto,pt);
				if (side < 0) um_negvc(nvec,nvec);
				um_unitvc(nvec,bpt.nvec);
				if (UM_DOT(nzvec,bpt.nvec) < 0.)
					um_negvc(bpt.nvec,bpt.nvec);
				bpt.cvu = unew;
				u = 0.5; v = 0.5; side = 0.;
				ncl_pt_project_sf(evout.cp,&sf2,sftf2,&u,&v,&side,0,UU_NULL,0,
					UU_NULL,tolsq,pto,nvec,&errfl);
				bpt.sfu = u;
				bpt.sfv = v;
				bpt.remove = UU_FALSE;
				uu_list_insert(nbr->bpts,no_bpts-1,&bpt);
				bpts = (ncl_bndpt_struc *)UU_LIST_ARRAY(nbr->bpts);
				no_bpts++;
				unew += dis;
			}
		}
		nbr->cvu0 = bpts[0].cvu;
		nbr->cvu1 = bpts[no_bpts-1].cvu;
		if ((nbr->cvu0 < UM_DFUZZ && nbr->cvu1 < UM_DFUZZ) ||
			(fabs(nbr->cvu0-1.) < UM_DFUZZ && fabs(nbr->cvu1-1.) < UM_DFUZZ))
		{
			nbr->cvu0 = 0.; nbr->cvu1 = 1.;
		}
		nbr->no_bpts = no_bpts;
	}
}

/*********************************************************************
**    I_FUNCTION     : S_set_fvecs(nbr_list,bcv,cvtf,sftf)
**       Set the forward vector for each point based on its neighboring
**       point.
**    PARAMETERS
**       INPUT  :
**          nbr_list - List of surface neighbor data.
**          bcv      - Boundary curve of pocket bottom.
**          cvtf     - Xform matrix associated with boundary curve.
**          sftf     - Xform matrix associated with pocket bottom.
**       OUTPUT :
**          nbr_list - Updated list of surface neighbor data.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_set_fvecs(nbr_list,bcv,cvtf,sftf)
UU_LIST *nbr_list;
struct UM_crvdatabag *bcv;
UM_transf cvtf,sftf;
{
	int i,j,ind,ind2,no_nbrs,no_bpts;
	UM_vector tvec,tvec2;
	ncl_bndpt_struc *bpt,*bpt1,*bpt2;
	ncl_nbr_struc *nbrs,*nbr;
	struct UM_evcrvout evcrv;
	
	no_nbrs = nbr_list->cur_cnt;
	nbrs = (ncl_nbr_struc *) UU_LIST_ARRAY(nbr_list);
/*
.....Use the forward vector already associated with the point on
.....the curve unless it is the end of an interval.  The ends of
.....the interval will use the forward vector from the curve if
.....it agrees with the direction of the next point.  This should
.....keep corners from causing problems.
*/
	uc_init_evcrvout(bcv, &evcrv);
	for (i=0;i<no_nbrs;i++)
	{
		nbr = &nbrs[i];
		if (nbr->check == UU_FALSE) continue;
		bpt = (ncl_bndpt_struc *)UU_LIST_ARRAY(nbr->bpts);
		for (j=0;j<nbr->no_bpts;j++)
		{
/*
.....Get the curve point and use it for now to make sure the point is
.....exactly on the boundary curve.
*/
			uc_evcrv(UM_FRSTDERIV,bpt[j].cvu,bcv,cvtf,&evcrv);
			um_cctmtf(evcrv.cp,sftf,bpt[j].pt);
			if (nbr->no_bpts > 2)
			{
				if (j < nbr->no_bpts-1)
				{
					um_unitvc(evcrv.dcdu,bpt[j].fvec);
					um_vctmtf(bpt[j].fvec,sftf,bpt[j].fvec);
					um_unitvc(bpt[j].fvec,bpt[j].fvec);
				}
				else
					um_vctovc(bpt[j-1].fvec,bpt[j].fvec);
				um_vcmnvc(bpt[1].pt,bpt[0].pt,tvec); um_unitvc(tvec,tvec);
				if (UM_DOT(bpt[0].fvec,tvec) < 0.9 && UM_MAG(tvec) > UM_FUZZ)
					um_vctovc(tvec,bpt[0].fvec);
			}
			else
			{
				if (j == 0)
				{
					uc_evcrv(UM_FRSTDERIV,bpt[j+1].cvu,bcv,cvtf,&evcrv);
					um_cctmtf(evcrv.cp,sftf,evcrv.cp);
					um_vcmnvc(evcrv.cp,bpt[j].pt,tvec);
				}
				else
					um_vcmnvc(bpt[j].pt,bpt[j-1].pt,tvec);
				um_unitvc(tvec,bpt[j].fvec);
			}
		}
		nbr->check = UU_FALSE;
	}
}

/*********************************************************************
**    I_FUNCTION     : S_fix_cids(cv)
**       The routine makes sure the reverse flag is correct for each
**       component so the cctou routine will give the correct result.
**       Note that the changes are not stored in the Unibase.  This
**       prevents the changes from affecting any later routines that
**       may already compensate for this.
**    PARAMETERS
**       INPUT  :
**          cv - Curve to check/fix.
**       OUTPUT :
**          cv - Updated curve.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_fix_cids(cv)
struct UM_compcrv_rec *cv;
{
	int i,status;
	UM_coord spt,ept,lpt,pt;
	UU_LOGICAL rev;
	struct NCL_fixed_databag e1temp;
	UM_transf tfmat;

	for (i=0;i<cv->no_cid;i++)
	{
		e1temp.key = cv->cid[i].crvid;
		status = ncl_retrieve_data_fixed(&e1temp);
		if (status != UU_SUCCESS) return;
		rev = 0;
		uc_retrieve_transf(e1temp.key, tfmat);
		um_get_endpts(&e1temp,tfmat,spt,ept);
		if (cv->cid[i].reverse)
		{
			um_vctovc (spt,pt);
			um_vctovc (ept,spt);
			um_vctovc (pt,ept);
		}
		if (i > 0)
		{
			if (um_sqdis(spt,lpt) > 5.e-3 && um_sqdis(ept,lpt) < 1.e-3)
			rev = 1;
		}
		if (rev)
			um_vctovc (spt,lpt);
		else
			um_vctovc (ept,lpt);
		if (rev)
			cv->cid[i].reverse = 1 - cv->cid[i].reverse;
	}
}

/*********************************************************************
**    I_FUNCTION     : S_neighbor_sfkeys (sfkey,nzvec,nbr_list,stype)
**       Finds all surfaces that share an edge with the given
**       surface.  Builds records of neighbors for later use.
**    PARAMETERS
**       INPUT  :
**          sfkey    - Key of surface to modify.
**          nvzvec   - Pocket normal vector.
**          stype    - Search type:
**                      1: Shared bndry pts.
**                      2: Neighboring surfaces only.
**       OUTPUT :
**          nbr_list - List of data for neighbor surfaces.
**          cvkey    - Key of boundary curve used.
**    RETURNS      : Number of keys in nbr_list.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_neighbor_sfkeys (sfkey,nzvec,nbr_list,stype,binx,cvkey,err)
UU_KEY_ID *sfkey,*cvkey;
UU_LIST *nbr_list;
UM_vector nzvec;
int stype,binx,*err;
{
	int i,j,status,stat,no_pts,no_keys,entnum,relnum,no_sfs,orient,updown;
	int bnum,ibnum,ienum,idir,maxpts,tmaxpts,tmaxind,maxind,no_rels = 3;
	int rels[] = {11,99,100};
	UU_REAL xmm1[2],ymm1[2],zmm1[2],xmm2[2],ymm2[2],zmm2[2];
	UU_REAL u,v,tol,tolsq,dist,fact,side,*upts,box1[6],box2[6];
	struct NCL_trimsf_rec sf1,sf2;
	struct UM_crvdatabag bcv;
	struct NCL_fixed_databag upvec;
	struct NCL_vector_rec *ve;
	struct NCL_nclpv_rec *pv;
	UU_LIST key_list,u_list;
	UM_coord *pts,pto;
	UM_vector nvec,tvec,tvec2;
	UU_LOGICAL isect,del;
	UU_KEY_ID *keys,ckey;
	UM_transf tfmat,sftf,sftf2,ttf;
	ncl_nbr_struc nbr,*nbrs;
	ncl_bndpt_struc bpt,*bpts;
	UM_int2 errfl,type,iret,isub,mm;
	static int cnt = 0;
	char tbuf[80];

	*err = no_sfs = no_keys = maxpts = 0;
	isub = 264;	getifl(&isub,&mm);
	if (mm) fact = 25.4;
	else fact = 1.;
	gettol(&tol);
	tol = tol*2;
	tolsq = tol*tol;
	Sbot_key = sf1.key = *sfkey;
	ncl_retrieve_data_fixed(&sf1);
	bcv.key = 0;
	if (sf1.rel_num == NCL_TRIMSF_REL)
	{
/*
.....Only use xyz curve keys.  Generate a new curve if no xyz curve
.....exists.
*/
		if (binx == 0) bcv.key = sf1.cv_key;
		else bcv.key = sf1.ibndykey[2*binx-2];
		if (bcv.key > 0) uc_retrieve_transf(sf1.key,sftf);
	}
	if (bcv.key < 1)
	{
		ibnum = ienum = 0;
		bnum = binx;
		idir = -1;
		status = ncl_cvonsf_func (sfkey,&bnum,&ckey,&type,&ibnum,&ienum,&idir);
		if (status != UU_SUCCESS) goto failed;
		bcv.key = ckey;
		um_tftotf(UM_idmat,sftf);
	}
	*cvkey = bcv.key;
	ncl_retrieve_data_fixed(&bcv);
	if (bcv.rel_num == UM_COMPCRV_REL) S_fix_cids(&bcv);
	uc_retrieve_transf(bcv.key,tfmat);
/*
.....This is now done by the calling routine
*/
/*	to_unbs(nzvec,tvec,&IVE);*/
	um_vctovc(nzvec,tvec);
	orient = um_polygon_orient3D(&bcv,UU_NULL,0,tvec,sftf);
	if (binx > 0) orient = 1 - orient;
/*
.....Find surfaces whose bounding box intersects the given surface's box.
*/
	sf2.key = 0;
	status = ncl_sf_box(&sf1,box1);
	uu_list_init(&key_list,sizeof(UU_KEY_ID),10,10);
	for (i=0;i<no_rels;i++)
	{
		entnum =0;
		relnum = rels[i];
		while(status == UU_SUCCESS)
		{
			entnum++;
			status = ur_get_next_data_key(relnum, &entnum,&sf2.key);
			if (status == UU_SUCCESS)
			{
				if (sf2.key == sf1.key) continue;
				ncl_retrieve_data_fixed(&sf2);
				if (sf2.label[0] == '@')
					continue;
				stat = ncl_sf_box(&sf2,box2);
				if (stat == UU_SUCCESS)
				{
					xmm1[0] = box1[0]; xmm1[1] = box1[3];
					ymm1[0] = box1[1]; ymm1[1] = box1[4];
					zmm1[0] = box1[2]; zmm1[1] = box1[5];
					xmm2[0] = box2[0]; xmm2[1] = box2[3];
					ymm2[0] = box2[1]; ymm2[1] = box2[4];
					zmm2[0] = box2[2]; zmm2[1] = box2[5];
					isect = um_isect_3dbox(xmm1,ymm1,zmm1,xmm2,
						ymm2,zmm2,tol);
					if (isect)
					{
						uu_list_push(&key_list,&sf2.key);
					}
				}
			}
		}
		status = UU_SUCCESS;
	}
/*
.....Find the common boundary.
*/
	i = 0;
	keys = (UU_KEY_ID *)UU_LIST_ARRAY(&key_list);
	no_keys = UU_LIST_LENGTH(&key_list);
	uu_list_init(nbr_list,sizeof(ncl_nbr_struc),no_keys,10);
	while (i < no_keys)
	{
if (keys[i] == 280)
{
	sf2.key = keys[i];
}
		sf2.key = keys[i];
		ncl_retrieve_data_fixed(&sf2);
		uc_retrieve_transf(sf2.key,sftf2);
		status = ncl_comm_bndr_ini (&(sf1.key),&(sf2.key),0.9*tol);
		ncl_pick_bndry(0,binx);
		if (status == UU_SUCCESS)
			NCL_cpn = ncl_comm_bndr (tol,UU_FALSE);
		no_pts = cbxyz.cur_cnt;
		if (no_pts < 2 || status != UU_SUCCESS) goto delcon2;
		no_pts = UU_LIST_LENGTH(&cbxyz);
		pts = (UM_coord *) UU_LIST_ARRAY (&cbxyz);
		if (no_pts > 1)
		{
/*
.....Sort surface data along boundary and fill neighbor record.
*/
			um_tftmtf(sftf,tfmat,ttf);
			uu_list_init(&u_list,sizeof(UU_REAL),no_pts+1,10);
			for (j=0;j<no_pts;j++)
			{
				del = UU_FALSE;
				if (j > 0 && um_sqdis(pts[j-1],pts[j]) < 4.5*tolsq)
					del = UU_TRUE;
				if (!del)
				{
					uc_cctou(&bcv,ttf,pts[j],&u,&dist);
					if ((u < -0.0001 || u > 1.0001) || dist > tol)
/*
.....The following IF does not support
.....closed boundaries, i.e. vmill_2axis.pp
*/
/*						(j == no_pts-1 && um_sqdis(pts[j],pts[0]) < 4.5*tolsq))*/
						del = UU_TRUE;
				}
				if (del)
				{
					uu_list_delete(&cbxyz,j,1);
					j--;
					pts = (UM_coord *) UU_LIST_ARRAY (&cbxyz);
					no_pts--;
					continue;
				}
				uu_list_push(&u_list,&u);
			}
/*
.....Currently ignoring surfaces that only share a single bndry point.
*/
			if (no_pts < 2) goto delcon1;
			upts = (UU_REAL *)UU_LIST_ARRAY(&u_list);
			if (upts[0] > 1. - UM_FUZZ) upts[0] = 0.;
			else if (upts[no_pts-1] < UM_FUZZ) upts[no_pts-1] = 1.;
			nbr.key = sf2.key;
/*
.....Skip calculating the points if only surfaces desired.
*/
			if (stype == 2)
			{
				no_sfs++;
				uu_list_free(&u_list);
				uu_list_push(nbr_list,&nbr);
				goto skip;
			}
			upts = (UU_REAL *)UU_LIST_ARRAY(&u_list);
			nbr.bpts = (UU_LIST *) uu_malloc(sizeof(UU_LIST));
			uu_list_init(nbr.bpts,sizeof(ncl_bndpt_struc),no_pts,25);
/*
.....Calculate boundary point data.
*/
			nbr.no_bpts = 0;
			for (j=0;j<no_pts;j++)
			{
				u = upts[j];
				side = 0.;
				bpt.cvu = u;
				u = 0.5; v = 0.5;
				ncl_pt_project_sf(pts[j],&sf1,sftf,&u,&v,&side,0,UU_NULL,0,
					UU_NULL,tol,pto,nvec,&errfl);
				if (side < 0) um_negvc(nvec,nvec);
				um_unitvc(nvec,bpt.nvec);
				if (UM_DOT(tvec,bpt.nvec) < 0.)
					um_negvc(bpt.nvec,bpt.nvec);
				um_vctovc(pto,bpt.pt);
				u = 0.5; v = 0.5; side = 0.;
				ncl_pt_project_sf(pts[j],&sf2,sftf2,&u,&v,&side,0,UU_NULL,0,
					UU_NULL,tol,pto,nvec,&errfl);
				bpt.sfu = u;
				bpt.sfv = v;
				bpt.remove = UU_FALSE;
				uu_list_push(nbr.bpts,&bpt);
				nbr.no_bpts++;
			}
/*
.....Make sure cctou routine didn't give any bad points
*/
			bpts = (ncl_bndpt_struc *) UU_LIST_ARRAY(nbr.bpts);
			for (j=1;j<no_pts-1;j++)
			{
				if (bpts[j].cvu > bpts[j-1].cvu && bpts[j].cvu > bpts[j+1].cvu)
				{
					uu_list_delete(nbr.bpts,j,1);
					bpts = (ncl_bndpt_struc *) UU_LIST_ARRAY(nbr.bpts);
					j--; no_pts--;
				}
			}
			if (no_pts < 2) goto delcon1;
			if (no_pts > maxpts)
			{
				maxpts = no_pts;
				maxind = no_sfs;
			}
			nbr.no_bpts = no_pts;
			uu_list_sort(nbr.bpts,S_sortbnd);
			bpts = (ncl_bndpt_struc *) UU_LIST_ARRAY(nbr.bpts);
			nbr.cvu0 = bpts[0].cvu;
			nbr.cvu1 = bpts[no_pts-1].cvu;
			nbr.orient = orient;
			nbr.check = UU_TRUE;
			uu_list_push(nbr_list,&nbr);
			uu_list_free(&u_list);
			no_sfs++;
		}
skip:
		ncl_2sfbnry_free ();
		i++;
		continue;
delcon1:
		uu_list_free(&u_list);
delcon2:
		uu_list_delete(&key_list,i,1);
		keys = (UU_KEY_ID *)UU_LIST_ARRAY(&key_list);
		no_keys--;
		ncl_2sfbnry_free ();
	}
/*
.....Fix and refine neighbor records. Sort and add boundary points.
*/
	if (no_keys > 0)
	{
		if (stype == 1)
		{
			S_sort_bpts(nbr_list);
			if (no_sfs > 1)
				uu_list_sort(nbr_list,S_sortnbr);
			S_add_bpts(nbr_list,&sf1,sftf,&bcv,tfmat,tvec,tol);
			S_set_fvecs(nbr_list,&bcv,tfmat,sftf);
#if 0
	if (stype == 1 && binx == 0)
	{
		nbrs = (ncl_nbr_struc *)UU_LIST_ARRAY(nbr_list);
		for (i=0;i<nbr_list->cur_cnt;i++) S_debug_nbr(&nbrs[i],0);
	}
#endif
		}
	}
	uu_list_free (&key_list);
	ncl_2sfbnry_free ();
	goto done;
failed:
	*err = 1;
done:
	return(no_sfs);
}

/*********************************************************************
**    I_FUNCTION     : S_buildnbr (sf,sftf,crv,cvtf,u0,u1,
**                          binx,nzvec,tol,orient,nbr)
**       Calculate boudnary points and build a neighbor record for the
**       given surface and curve.
**    PARAMETERS
**       INPUT  :
**          sf     - Surface to project to.
**          sftf   - Transformation matrix for surface.
**          crv    - Boundary curve of surface to evaluate.
**          cvtf   - Transformation matrix for curve.
**          u0,u1  - Range for u params to use for curve evaluation.
**          binx   - Boundary index of surface.
**          nzvec  - Normal vector for pocket.
**          tol    - Evaluation tolerance.
**          orient - Curve orientation with respect to pocket normal.
**       OUTPUT :
**          nbr    - Generated neighbor record.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_buildnbr(sf,sftf,crv,cvtf,u0,u1,binx,nzvec,tol,orient,nbr)
struct NCL_fixed_databag *sf;
struct UM_crvdatabag *crv;
UM_transf sftf,cvtf;
UU_REAL u0,u1,tol;
int binx,orient;
UM_vector nzvec;
ncl_nbr_struc *nbr;
{
	int i,npts,status;
	UM_int2 errfl;
	UU_REAL uu,tu,u,v,side;
	UM_coord pto;
	UM_vector nvec;
	ncl_bndpt_struc bpt,bpt2;
	struct UM_evcrvout evout;

	uu = u0; npts = 25;
	tu = u1-u0;
	if (tu < 0.) tu += 1.;
	tu /= 25.;
	nbr->no_bpts = 0;
	nbr->bpts = (UU_LIST *) uu_malloc(sizeof(UU_LIST));
	uu_list_init(nbr->bpts,sizeof(ncl_bndpt_struc),npts,25);
	status = uc_init_evcrvout (crv, &evout);
	for (i=0;i<npts-1;i++)
	{
		status = uc_evcrv (UM_FRSTDERIV,uu,crv,cvtf,&evout);
		u = 0.5; v = 0.5;
		um_cctmtf(evout.cp,sftf,evout.cp);
		ncl_pt_project_sf(evout.cp,sf,sftf,&u,&v,&side,0,UU_NULL,0,
			UU_NULL,tol,pto,nvec,&errfl);
		if (side < 0) um_negvc(nvec,nvec);
		um_unitvc(nvec,bpt.nvec);
		if (UM_DOT(nzvec,bpt.nvec) < 0.)
			um_negvc(bpt.nvec,bpt.nvec);
		um_vctovc(pto,bpt.pt);
		bpt.cvu = uu;
		bpt.sfu = u;
		bpt.sfv = v;
		bpt.remove = UU_FALSE;
		if (i == 0)
		{
			um_vctovc(bpt.pt,bpt2.pt); um_vctovc(bpt.nvec,bpt2.nvec);
			bpt2.cvu = u1;
			bpt2.sfu = bpt.sfu;
			bpt2.sfv = bpt.sfv;
		}
		uu_list_push(nbr->bpts,&bpt);
		nbr->no_bpts++;
		uu += tu;
		if (uu > 1.) uu -= 1.;
	}
	uu_list_push(nbr->bpts,&bpt2);
	nbr->no_bpts++;
	nbr->cvu0 = u0;
	nbr->cvu1 = u1;
	nbr->orient = orient;
	nbr->key = sf->key;
	nbr->check = UU_TRUE;
}

/*********************************************************************
**    I_FUNCTION     : S_checknbrs (sfkey,nzvec,&nbr_list,1,ix,&err)
**       Check for regions of the surface boundary that have no neighboring
**       surface and create a nbr record that will be treated as open.
**    PARAMETERS
**       INPUT  :
**          sfkey   - Key of surface to modify.
**          nzvec   - Pocket bottom normal vector.
**          nbr_list - List of surface data and boundary points.
**          num_sfs - Number of entries in nbr_list.
**          ix      - Boundary index.
**       OUTPUT :
**          err     - Error flag.
**    RETURNS      : UU_SUCCESS iff no failure.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_checknbrs (sfkey,nzvec,cvkey,nbr_list,binx,err)
UU_KEY_ID sfkey,cvkey;
UU_LIST *nbr_list;
UM_vector nzvec;
int binx,*err;
{
	int i,j,k,npts,numsf,orient,status,numgen;
	UU_REAL tol,tolsq,u0,u1,dis,fact;
	ncl_nbr_struc nbr,*nbrs;
	ncl_bndpt_struc *bpts,*bpts2;
	struct UM_crvdatabag crv;
	struct NCL_fixed_databag sf,upvec;
	struct NCL_vector_rec *ve;
	struct NCL_nclpv_rec *pv;
	UM_transf tfmat,sftf;
	UM_int2 isub,mm;
	UU_LIST bpt_list;
	
	isub = 264;	getifl(&isub,&mm);
	if (mm) fact = 25.4;
	else fact = 1.;
	gettol(&tol);
	tol = tol * 2.;
	tolsq = tol*tol;
	
	numsf = nbr_list->cur_cnt;
	sf.key = sfkey;
	ncl_retrieve_data_fixed(&sf);
	uc_retrieve_transf(sf.key,sftf);
	crv.key = cvkey;
	ncl_retrieve_data_fixed(&crv);
	uc_retrieve_transf(crv.key,tfmat);
	orient = um_polygon_orient3D(&crv,UU_NULL,0,nzvec,sftf);
/*
.....If there are no neighbors, build a single neighbor and treat it as open.
.....Otherwise, check for regions of the surface boundary that do not
.....have a neighbor.
*/
	numgen = 0;
	if (numsf == 0)
	{
		S_buildnbr(&sf,sftf,&crv,tfmat,0.,1.,binx,nzvec,tol,orient,&nbr);
		uu_list_push(nbr_list,&nbr);
		numgen++;
	}
	else
	{
		nbrs = (ncl_nbr_struc *)UU_LIST_ARRAY(nbr_list);
		for (i=0;i<numsf+numgen;i++)
		{
			j = (i+1)%(numsf+numgen);
			if (i == (numsf+numgen-1))
			{
				if (fabs(nbrs[i].cvu1-1.) < UM_FUZZ) u0 = 0.;
				else u0 = nbrs[i].cvu1;
			}
			else
				u0 = nbrs[i].cvu1;
			u1 = nbrs[j].cvu0;
			if (fabs(u0-u1) > 5.*UM_FUZZ && u0 < u1)
			{
				bpts = (ncl_bndpt_struc *) UU_LIST_ARRAY(nbrs[i].bpts);
				bpts2 = (ncl_bndpt_struc *) UU_LIST_ARRAY(nbrs[j].bpts);
				k = nbrs[i].no_bpts - 1;
				dis = um_sqdis(bpts[k].pt,bpts2[0].pt);
				if (dis < 4.*tolsq) continue;
				S_buildnbr(&sf,sftf,&crv,tfmat,u0,u1,binx,nzvec,tol,orient,&nbr);
				uu_list_insert(nbr_list,j,&nbr);
				nbrs = (ncl_nbr_struc *)UU_LIST_ARRAY(nbr_list);
				numgen++;
			}
		}
	}
	if ((numgen > 0 && binx == 0) || (numgen > 1 && binx > 0))
	{
		numsf += numgen;
		S_sort_bpts(nbr_list);
		if (numsf > 1)
			uu_list_sort(nbr_list,S_sortnbr);
		S_add_bpts(nbr_list,&sf,sftf,&crv,tfmat,nzvec,tol);
		S_set_fvecs(nbr_list,&crv,tfmat,sftf);
	}
	return(numgen);
}

/*********************************************************************
**    I_FUNCTION     : int S_create_netsf (sfkey,nbr_list,no_keys)
**       Create a net surface from a surface and its neighbors. 
**    PARAMETERS
**       INPUT  :
**          sfkey    - Key of surface to modify.
**          nbr_list - List of surface data and boundary points
**          no_keys  - Number of entries in nbr_list
**       OUTPUT :
**          none
**    RETURNS      : UU_SUCCESS iff no failure.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_create_netsf (sfkey,nbr_list,no_keys)
UU_KEY_ID *sfkey;
UU_LIST *nbr_list;
int no_keys;
{
	int i,j,status,rel_num,num_keys;
	struct UC_attributedatabag attr;
	UU_KEY_ID key1,key2;
	struct NCL_netsf_rec surf;
	UM_int2 i2;
	ncl_nbr_struc *nbr;
/*
.....Generate net surface.
*/
	ur_setup_data(NCL_NETSF_REL, &surf, sizeof(surf));
	surf.surf_type = NCLI_NETSF;
	surf.no_netkey = 0;
	surf.key = 0;
/*
.....Currently set these valuse to zero.  Not sure what they are for.
*/
	for (i=0;i<40;i++)
		for(j=0;j<4;j++)
			surf.bndsfs[i][j] = 0;
	status = ncl_create_entity((struct UC_entitydatabag *)&surf, 9);
	if (status != UU_SUCCESS) return(status);
	i = 0;
	i2 = 1;
	stunlb(&i2);
	num_keys = no_keys + 1;
	nbr = (ncl_nbr_struc *)UU_LIST_ARRAY(nbr_list);
	while (i<num_keys && status == UU_SUCCESS)
	{
		if (i == 0) key1 = *sfkey;
		else
		{
			key1 = nbr->key;
			nbr++;
		}
		ur_retrieve_data_relnum(key1, &rel_num);
		switch (rel_num)
			{
			case NCL_SURF_REL:
				status = ncl_net_nclsf(key1, &key2);
				break;
			case NCL_REVSURF_REL:
				status = ncl_net_revsf(key1, &key2);
				break;
			case NCL_MESHSURF_REL:
				status = ncl_net_mshsf(key1, &key2);
				break;
			case NCL_EVALSF_REL:
				status = ncl_net_evalsf(key1, &key2);
				break;
			case UM_RBSPLSRF_REL:
				status = ncl_net_rbsf(key1, &key2);
				break;
			case UM_AGSRF_REL:
				status = ncl_net_agsrf(key1, &key2);
				break;
			case NCL_TRIMSF_REL:
				status = ncl_net_trimsf(key1, &key2);
				break;
			case NCL_NETSF_REL:
				status = ncl_net_netsf(key1, &key2);
				break;
			default:
				status = UU_FAILURE;
				break;
			}
		if (ur_update_data_varlist(surf.key, 1, &key2,
								i+1, 1) != 0)
			status = UU_FAILURE;
		i++;
   }
	attr.key = *sfkey = surf.key;
	ur_retrieve_attr(&attr);
	attr.color = 6;
	ur_update_attr(&attr);
	stunlb(&i2);
	return(status);
}

/*********************************************************************
**    I_FUNCTION     : int S_create_polyln (sfkey,nbr_list,num_sfs)
**     Generate polylines from the points in the neighbor records.
**    PARAMETERS
**       INPUT  :
**          sfkey   - Key of surface to modify.
**          nbr_list - List of surface data and boundary points
**          num_sfs - Number of entries in nbr_list
**       OUTPUT : none
**    RETURNS      : UU_SUCCESS iff no failure.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_create_polyln (sfkey,nbr_list,num_sfs)
UU_KEY_ID sfkey;
UU_LIST *nbr_list;
int num_sfs;
{
	int i,j,status,color;
	struct UC_attributedatabag attr;
	UU_LIST tpt_list;
	UM_coord *tpts;
	ncl_nbr_struc *nbr;
	ncl_bndpt_struc *bpt;
	char sbuf[80];

	attr.key = sfkey;
	color = 1;
	ur_retrieve_attr(&attr);
	attr.line_weight = 2;
	attr.line_style = 3;
	nbr = (ncl_nbr_struc *)UU_LIST_ARRAY(nbr_list);
	uu_list_init(&tpt_list,sizeof(UM_coord),nbr->no_bpts,20);
	for (i=0;i<num_sfs;i++,nbr++)
	{
		if (nbr->no_bpts < 2) continue;
		bpt = (ncl_bndpt_struc *)UU_LIST_ARRAY(nbr->bpts);
		for (j=0;j<nbr->no_bpts;j++,bpt++) uu_list_push(&tpt_list,&(bpt->pt));
		tpts = (UM_coord *)UU_LIST_ARRAY(&tpt_list);
		attr.color = color;
		attr.key = 0;
		status = um_c42_genpoly(tpts,nbr->no_bpts,&attr);
		if (status != UU_SUCCESS) goto failed;
		color = ((color+1)%15) + 1;
		UU_LIST_EMPTY(&tpt_list);
	}
failed:
	uu_list_free(&tpt_list);
	return(status);
}

/*********************************************************************
**    I_FUNCTION     : int S_calcrad (npts,pts,nbr,tf,first)
**     Calculate the offset radius for a given set of points.
**    PARAMETERS
**       INPUT  :
**          npts    - Number of input points.
**          pts     - Input points.
**          nbr     -   Neighbor surface data.
**          tf      - Transformation matrix to move points to xy-plane.
**          times   - Call count. Used for averaging radius
**          tol     - tolerance.
**       OUTPUT :
**          nbr     - Updated neighbor surface data.
**          ocenter - First arc center calculated.
**    RETURNS      : UU_SUCCESS iff no failure.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_calcrad (npts,pts,nbr,bpt,tf,times,tol,ocenter)
int npts,*times;
UM_coord *pts,ocenter;
ncl_nbr_struc *nbr;
ncl_bndpt_struc *bpt;
UM_transf tf;
UU_REAL tol;
{
	int i,add,nopts,cclw,status;
	UM_coord center,pt,*mpts;
	UU_REAL rad,avgr,tolsq,dis,dis2;
	UU_LIST pt_list;
	char tbuf[80];

	uu_list_init(&pt_list,sizeof(UM_coord),npts+1,5);
	status = UU_SUCCESS;
	dis = dis2 = rad = 0.;
	nopts = 0;
	avgr = (UU_REAL)(*times);
	tolsq = tol*tol;
	add = 0;
/*
.....Make sure slice includes boundary point.
*/
	dis = um_sqdis(pts[0],bpt->pt); dis2 = um_sqdis(pts[npts-1],bpt->pt);
	if (dis < dis2 && dis > UM_DFUZZ)
	{
		um_cctmtf(bpt->pt,tf,pt); uu_list_push(&pt_list,&pt);
		nopts++;
	}
	else if (dis2 > UM_DFUZZ)
		add = 1;
	for (i=0;i<npts;i++,nopts++)
	{
		um_cctmtf(pts[i],tf,pt);
		uu_list_push(&pt_list,&pt);
	}
	if (add)
	{
		um_cctmtf(bpt->pt,tf,pt); uu_list_push(&pt_list,&pt);
		nopts++;
	}
	mpts = (UM_coord *)UU_LIST_ARRAY(&pt_list);
	if (nopts > 2 && ncl_cv_isarc(nopts,mpts,center,&rad,&cclw,UU_FALSE))
	{
		if (*times == 0)
		{
			nbr->rad = rad;
			um_vctovc(center,ocenter);
		}
		else if (fabs(rad - nbr->rad) > 10.*tol)
		{
/*
.....This check helps avoid failures on small circle portions.  Small
.....sections can have a large variance in radius.
*/
			for (i=0;i<npts;i++)
			{
				dis = um_dcccc(mpts[i],ocenter);
				if (fabs(dis - nbr->rad) > 4.*tol) goto failed;
			}
		}
		else nbr->rad = (avgr*nbr->rad + rad)/(avgr+1.);
		(*times)++;
	}
	else if (nopts > 3 && ncl_cv_isline(npts,mpts))
		goto failed;
	goto done;

failed:
	status = UU_FAILURE;
	nbr->rad = 0.;
done:
	uu_list_free(&pt_list);
	return(status);
}

/*********************************************************************
**    I_FUNCTION     : void S_trimslice (sf,pts,uvs,tol,ptlst)
**     Retrieve and filter slice points from surface-plane intersection.
**     Fit slice points to bounding uv box.
**    PARAMETERS
**       INPUT  :
**          sf    - Surface data.
**          tol   - Tolerance.
**          pts   - Points in curve slice.
**          uvs   - Corresponding UV points in curve slice.
**          npts  - Number of points in curve slice.
**       OUTPUT :
**          ptlst - Pointer to point list.
**    RETURNS      : UU_SUCCESS iff no failure.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_trimslice(sf,tol,pts,uvs,npts,ptlst)
UU_LIST *ptlst;
UU_REAL tol;
UM_coord *pts,*uvs;
int npts;
struct NCL_fixed_databag *sf;
{
	int i,j,ncv,num,nutcv,status;
	UM_srf_boundary bndr;
	UM_transf tfmat;
	UU_LIST cvlst,uvlst;
	char tbuf[80];

	ncl_set_boundary_toler (tol);
	uc_retrieve_transf(sf->key,tfmat);
	um_init_boundary (&bndr);
	uu_list_init (&cvlst, sizeof(UM_coord), 500, 500);
	uu_list_init (&uvlst, sizeof(UM_coord), 500, 500);
	bndr.uvpts = &uvlst;
	bndr.cvpts = &cvlst;
	ncl_free_bndry (&bndr);
	UU_LIST_EMPTY (bndr.uvpts);
	UU_LIST_EMPTY (bndr.cvpts);
	status = ncl_get_bndry (sf,tfmat,&bndr,tol,UU_FALSE);
	if (status != UU_SUCCESS || bndr.nb < 1)
		goto done;

	uu_list_init(ptlst,sizeof(UM_coord),50,25);
	if (npts < 2) goto done;
	for (j=0;j<npts;j++)
	{
		if (uvs[j][0]+UM_FUZZ >= bndr.ummx[0][0] &&
			uvs[j][0]-UM_FUZZ <= bndr.ummx[0][1] &&
			uvs[j][1]+UM_FUZZ >= bndr.vmmx[0][0] &&
			uvs[j][1]-UM_FUZZ <= bndr.vmmx[0][1])
			uu_list_push(ptlst,&(pts[j]));
	}
done:
	uu_list_free (&uvlst);
	uu_list_free (&cvlst);
	ncl_free_bndry (&bndr);
}

/*********************************************************************
**    I_FUNCTION     : int S_pocket_offset (sfkey,nbr_list,binx,ofl,openfl)
**     Generate offset points for given boundary points.  The maximum
**     offset distance is based on the cutter and surface dimensions.
**    PARAMETERS
**       INPUT  :
**          sfkey    - Key of pocket bottom surface.
**          nbr_list - List of neighbor records.
**          binx     - Boundary index.
**          ofl      - UU_TRUE = Use .5 diameter to offset open boundaries.
**                     UU_FALSE = Don't (VoluMill).
**       OUTPUT :
**          openfl  - Open boundary flag.
**                    UU_TRUE: Neighbor is below surface.
**                    UU_FALSE: Otherwise.
**    RETURNS      : UU_SUCCESS iff no failure.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_pocket_offset (sfkey,nbr_list,binx,ofl,openfl)
UU_KEY_ID sfkey;
UU_LIST *nbr_list;
int binx;
UU_LOGICAL ofl,*openfl;
{
	int i,j,k,ind,status,times,mcsflg,plflg,pflg;
	int step,npts,irot,indfl,irpt = 0;
	struct NCL_fixed_databag sf;
	struct NCL_nclpl_rec pl;
	struct UM_evsrfout evsrf;
	UU_REAL tol,tolsq,crad,trad,ptol,mtol,tradsq;
	UU_REAL ttol,maxrad,dis,dis2,fact,ttol1;
	UM_int4 key,igui;
	UM_real8 corner,diam;
	UM_int2 ier,isub,mm,ncvs;
	UM_coord *pts,*uvs,pt,center,tpt;
	UM_vector dvec,dvec2,xvec;
	ncl_nbr_struc *nbr,*nbr2,*nbrs;
	ncl_bndpt_struc *bpt,*bpt2,*bpts;
	UU_LOGICAL open,above,below,first,trimfl;
	UM_transf tf,tfi,rot,plmx,plinv,rtinv,sftf;
	UU_LIST pt_list;
	char tbuf[80],sbuf[80];
	int count = 0;
	
	gettol(&tol);
	isub = 2; gettool (&isub ,&corner);
	isub = 28; getsc(&isub,&diam);
	trad = diam/2.;
	crad = corner;
	status = UU_SUCCESS;
	isub = 264;	getifl(&isub,&mm);
	if (mm) fact = 25.4;
	else fact = 1.;
	trad /= fact;
	crad /= fact;
	tolsq = tol*tol;
	ptol = (1. + (1./tolsq));
	mtol = ((1./tolsq) - 1.);
	tradsq = trad*trad;
	maxrad = trad*(ptol*tradsq + mtol*trad + 0.25*ptol);
	maxrad = sqrt(maxrad);
	ncl_sparms(maxrad,tol);
	ttol = 1.*tol;
	nbr = nbrs = (ncl_nbr_struc *)UU_LIST_ARRAY(nbr_list);
/*
.....Intersect surface with planes.  Determine whether neighbor
.....is a fillet so offset can be determined.  Check for open
.....pocket is made as well.
*/
	for (i=0;i<nbr_list->cur_cnt;i++,nbr++)
	{
if (nbr->key == 645)
{
	sf.key = nbr->key;
}
		sf.key = nbr->key;
		ncl_retrieve_data_fixed(&sf);
		uc_retrieve_transf(sf.key,sftf);
		trimfl = (sf.rel_num == NCL_TRIMSF_REL);
		bpt = (ncl_bndpt_struc *)UU_LIST_ARRAY(nbr->bpts);
		nbr->rad = 0.;
		open = UU_FALSE;
		first = UU_TRUE;
		times = 0;
		if (nbr->key == sfkey)
		{
			open = (binx > 0);
			goto setrad;
		}
/*
.....Use only up to a maximum number of slices.
*/
		step = (nbr->no_bpts-1)/10;
		if (nbr->no_bpts < 5) step = 1;
		else if (step < 3) step = (nbr->no_bpts/3) + 1;
		else if (step > 30) step = (nbr->no_bpts/30) + 1;
		else step = (nbr->no_bpts/step) + 1;
		for (j=0;j<nbr->no_bpts;j+=step)
		{
			uc_evsrf(UM_FRSTDERIV,bpt[j].sfu,bpt[j].sfv,&sf,sftf,&evsrf);
			um_unitvc(evsrf.dsdu,dvec); um_unitvc(evsrf.dsdv,dvec2);
			if (fabs(um_dot(dvec,bpt[j].fvec)) > 0.975)
			{
				if (um_dot(dvec,bpt[j].fvec) < 0.)
					um_negvc(dvec,pl.nvec);
				else
					um_vctovc(dvec,pl.nvec);
			}
			else if (fabs(um_dot(dvec2,bpt[j].fvec)) > 0.975)
			{
				if (um_dot(dvec2,bpt[j].fvec) < 0.)
					um_negvc(dvec2,pl.nvec);
				else
					um_vctovc(dvec2,pl.nvec);
			}
			else
				um_vctovc(bpt[j].fvec,pl.nvec);
			um_vctovc(bpt[j].pt,pl.pt); um_vctovc(pl.nvec,dvec);
			fr_unbs(dvec,dvec,&IVE); fr_unbs(pl.pt,pt,&IPT);
			um_perpvc (pl.nvec,xvec); um_unitvc(xvec,xvec);
			um_ptzx_tf(pl.pt,pl.nvec,xvec,tf); um_inverttf(tf,tfi);
#if 0
			if (nbr->key == 645)
			{
				struct NCL_nclpl_rec tpl;
				fr_unbs(pl.pt,tpl.pt,&IPT);
				fr_unbs(pl.nvec,tpl.nvec,&IVE);
				sprintf(sbuf,"PLANE/(PT/%10.4f,%10.4f,%10.4f),PE,(VE/%10.4f,%10.4f,%10.4f)",
				tpl.pt[0],tpl.pt[1],tpl.pt[2],tpl.nvec[0],tpl.nvec[1],tpl.nvec[2]);
				NclxDbgPstr(sbuf);
			}
#endif
			ncl_define_rotmx (mm,dvec,rot,&mcsflg);
			pl.pt[2] = um_dot(dvec,pt);
			pl.pt[0] = pl.pt[1] = 0.;
			pl.nvec[0] = pl.nvec[1] = 0.; pl.nvec[2] = 1.;
/*
.....Move the point slightly to avoid being directly on the surface boundary.
.......Increased the step size to reduce the risk of problems related to the
.......slice being too close to the surface boundary - ASF 2/25/14.
*/
			if (j==0) pl.pt[2] += 5.*UM_FUZZ*fact;
			else if (j == nbr->no_bpts-1) pl.pt[2] -= 5.*UM_FUZZ*fact;
			um_cvio_new();
			ncvs = irpt = 1;
			igui = 0;
			key = sf.key;
			plflg = ncl_newzaxis_rot (dvec,plmx,plinv);
			if (trimfl) pflg = 1;
			else pflg = 0;
			ttol1 = ttol / 2.;
			status = um_cvio_create_intersection(&key,dvec,&pl,
											rot,&ttol1,&irpt,&ncvs,&igui,&pflg,&ier);
/*
			um_cvio_push_ncluv();
			ncl_getpts_uv (&npts,&pts);
*/
			um_cctmtf(bpt[j].pt,rot,pt);
			npts = S_cvio_getpts(pt,&pts,&uvs);
#if 0
			if (key == 645)
			{
				for (k=0;k<npts;k++)
				{
					sprintf(sbuf,"pto(%d) =pt/%10.4f,%10.4f,%10.4f",k+1,pts[k][0],
						pts[k][1], pts[k][2]);
					NclxDbgPstr(sbuf);
				}
			}
#endif
/*
.....Check for open pocket.
*/
			if (npts > 1)
			{
				ncl_get_rotmx (&irot,rtinv);
				if (first || open)
				{
					open = UU_TRUE;
					first = above = below = UU_FALSE;
					for (k=0;k<npts;k++)
					{
						if (irot > 0) um_cctmtf(pts[k],rtinv,tpt);
						um_nptpln(tpt,bpt[j].pt,bpt[j].nvec,pt);
						um_vcmnvc(tpt,pt,dvec); um_unitvc(dvec,dvec);
						dis = um_sqdis(tpt,pt); dis2 = um_sqdis(bpt[j].pt,pt);
						if (dis > tolsq  && UM_MAG(dvec) > UM_FUZZ)
						{
							if (um_dot(dvec,bpt[j].nvec) > UM_DFUZZ)
							{
								if (dis2 < 1.e4)
									above = UU_TRUE;
							}
							else below = UU_TRUE;
							if (above && below) break;
						}
					}
/*
.....Ignore above and below if at a corner of the pocket bottom.
*/
					if (above && below)
					{
						if (j == 0 || j == nbr->no_bpts-1)
						{
							if (j == 0)
							{
								if (i == 0) ind = nbr_list->cur_cnt-1;
								else ind = i - 1;
							}
							else
							{
								if (i == nbr_list->cur_cnt-1) ind = 0;
								else ind = i + 1;
							}
							nbr2 = &nbrs[ind];
							bpts = (ncl_bndpt_struc *)UU_LIST_ARRAY(nbr2->bpts);
							if (j == 0) ind = nbr2->no_bpts-1;
							else ind = 0;
						}
						else
						{
							ind = j - 1;
							bpts = (ncl_bndpt_struc *)UU_LIST_ARRAY(nbr->bpts);
						}
						bpt2 = &bpts[ind];
						dis = um_dot(bpt->fvec,bpt2->fvec);
						if (dis > 0.95) open = UU_FALSE;
					}
					else if (above) open = UU_FALSE;
				}
				if (open)
				{
					ncl_free_uv();
					goto skip;
				}
				if (trimfl)
				{
					S_trimslice(&sf,tol,pts,uvs,npts,&pt_list);
					npts = pt_list.cur_cnt;
					pts = (UM_coord *) UU_LIST_ARRAY(&pt_list);
					if (irot > 0)
					{
						for (k=0;k<npts;k++) um_cctmtf(pts[k],rtinv,pts[k]);
					}
				}
				status = S_calcrad (npts,pts,nbr,&bpt[j],tfi,&times,tol,center);
				if (trimfl)
					uu_list_free(&pt_list);
			}
skip:
			ncl_free_uv();
			um_cvio_free();
			if (status != UU_SUCCESS)
			{
				if (npts > 3) break;
			}
		}
setrad:
#if 0
		S_debug_nbr(nbr,0);
#endif
		*openfl = open;
		nbr->open = open;
		if (open && binx > 0) continue;
		if (open)
		{
			if (ofl) nbr->rad = trad + (Soffthk/fact + 2.*tol);
			else nbr->rad = Soffthk;
		}
		else if (nbr->rad > crad) nbr->rad = crad;
		if (nbr->rad > tol && ofl) nbr->rad -= tol;
		bpt = (ncl_bndpt_struc *)UU_LIST_ARRAY(nbr->bpts);
		if (fabs(nbr->rad) > tol)
		{ 
			for (j=0;j<nbr->no_bpts;j++,bpt++)
			{
				um_cross(bpt->nvec,bpt->fvec,dvec); um_unitvc(dvec,dvec);
				if (nbr->orient > 0) um_negvc(dvec,dvec);
				um_translate_point(bpt->pt,nbr->rad,dvec,bpt->pt);
			}
#if 0
		S_debug_nbr(nbr,0);
#endif
		}
	}
	return(UU_SUCCESS);
}

/*********************************************************************
**    I_FUNCTION     : S_debug_connect(pt11,pt12,pt21,pt22,ipt,pl,vc1,vc2,type)
**      Output debug data for connection routine.
**    PARAMETERS
**       INPUT  :
**          pt11,pt12,pt21,pt22 - Neighbor boundary points
**          ipt                 - Intersection point
**          pl                  - Plane parallel to extension
**          vc1,vc2             - Extension vectors
**          type                - Type of intersection data to print
**                                0: Intersecting segments found for two
**                                   neighbors
**                                1: Extension of first neighbor intersects
**                                   segment of second neighbor
**                                2: Extension of second neighbor intersects
**                                   segment of first neighbor
**                                3: Extensions of both neighbors intersect
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_debug_connect(pt11,pt12,pt21,pt22,ipt,pl,vc1,vc2,type)
UM_coord pt11,pt12,pt21,pt22,ipt;
UM_vector vc1,vc2;
UM_plane pl;
int type;
{
	char tbuf[80];

	if (type == 0)
	{
		NclxDbgPstr("$$ SEGMENTS INTERSECT");
		sprintf(tbuf,"LN/%lf,%lf,%lf,%lf,%lf,%lf",pt11[0],pt11[1],pt11[2],
			pt12[0],pt12[1],pt12[2]);
		NclxDbgPstr(tbuf);
		sprintf(tbuf,"LN/%lf,%lf,%lf,%lf,%lf,%lf\n",pt21[0],pt21[1],pt21[2],
			pt22[0],pt22[1],pt22[2]);
		NclxDbgPstr(tbuf);
	}
	else if (type == 1 || type == 2)
	{
		if (type == 1) NclxDbgPstr("$$ EXTENSION OF FIRST INTERSECTS SECOND");
		if (type == 2) NclxDbgPstr("$$ EXTENSION OF SECOND INTERSECTS FIRST");
		sprintf(tbuf,"PL/(PV/%lf,%lf,%lf,%lf,%lf,%lf)",pl.p0[0],pl.p0[1],
			pl.p0[2],pl.n[0],pl.n[1],pl.n[2]);
		NclxDbgPstr(tbuf);
		sprintf(tbuf,"LN/%lf,%lf,%lf,%lf,%lf,%lf\n",pt11[0],pt11[1],pt11[2],
			pt12[0],pt12[1],pt12[2]);
		NclxDbgPstr(tbuf);
	}
	else if (type == 3)
	{
		NclxDbgPstr("$$ EXTENSIONS INTERSECT");
		sprintf(tbuf,"PT/%lf,%lf,%lf",ipt[0],ipt[1],ipt[2]);
		NclxDbgPstr(tbuf);
		sprintf(tbuf,"PV/%lf,%lf,%lf,%lf,%lf,%lf",pt11[0],pt11[1],
			pt11[2],vc1[0],vc1[1],vc1[2]);
		NclxDbgPstr(tbuf);
		sprintf(tbuf,"PV/%lf,%lf,%lf,%lf,%lf,%lf",pt12[0],pt12[1],
			pt12[2],vc2[0],vc2[1],vc2[2]);
		NclxDbgPstr(tbuf);
	}
}

/*********************************************************************
**    I_FUNCTION     : S_pocket_connect (sfkey,nbr_list,num_sfs,nzvec,
**                                              flag,nopts)
**      Create a single, connected, polyline through the offset
**      boundary points.
**    PARAMETERS
**       INPUT  :
**          sfkey    - Key of pocket bottom surface.
**          nbr_list - List of surface data and boundary points
**          num_sfs  - Number of entries in nbr_list
**          nzvec    - Pocket bottom normal vector.
**          flag     - 0: Generate polyline only.
**                     1: Generate points and display polyline.
**                     2: Generate points only.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_pocket_connect (sfkey,nbr_list,num_sfs,nzvec,flag,nopts)
UU_KEY_ID sfkey;
UU_LIST *nbr_list;
UM_vector nzvec;
int num_sfs,*nopts,flag;
{
	int i,j,k,ind,ind2,ind3,status,npts;
	int diff,start,lstind,color,nint,n1,nsfs;
	UU_LOGICAL iadd,idid;
	struct UC_attributedatabag attr;
	UU_REAL tol,tolsq,minrad;
	UM_coord *pts,pt1,pt2,ipt;
	UM_vector fvec1,fvec2,nfvec2,nvec,pvec,*tans;
	UU_LIST pt_list,tan_list;
	UU_LOGICAL done;
	ncl_nbr_struc *nbrs;
	ncl_bndpt_struc *bpt,*bpt2,tbpt;
	UM_plane pl;
	UM_int2 idx,ifl;
	int count = 1,pvcount = 1;
	char tbuf[80];

	gettol(&tol);
	tolsq = tol*tol;
	status = UU_SUCCESS;
	npts = lstind = color = 0;
	nbrs = (ncl_nbr_struc *)UU_LIST_ARRAY(nbr_list);
	for (i=0;i<num_sfs;i++) npts += nbrs[i].no_bpts;
	uu_list_init(&pt_list,sizeof(UM_coord),npts+num_sfs,10);
	uu_list_init(&tan_list,sizeof(UM_coord),npts+num_sfs,10);
	npts = 0;
	minrad = 1.e12;
	nsfs = num_sfs;
	idid = UU_FALSE;
/*
.....Remove points from boundary that will be outside the connected boundary
*/
	for (i=0;i<nsfs;i++)
	{
#if 0
		if (nbrs[i].key == 1280)
		{
			nint = 0;
		}
#endif
		if (nsfs == 1) break;
		nint = 0;
		ind = (i+1)%num_sfs;
		if (nbrs[i].rad > 0. && nbrs[i].rad < minrad) minrad = nbrs[i].rad;
		bpt = (ncl_bndpt_struc *) UU_LIST_ARRAY(nbrs[i].bpts);
		bpt2 = (ncl_bndpt_struc *) UU_LIST_ARRAY(nbrs[ind].bpts);
		if (nbrs[i].rad < UM_DFUZZ && nbrs[ind].rad < UM_DFUZZ) continue;
		idid = UU_TRUE;
		ind2 = nbrs[i].no_bpts-1; ind3 = 0;
/*
.....Check to see if neighbors intersect
.......Note that a CHAMFR style connection will be made if no connection
.......is found
*/
		done = UU_FALSE;
/*
.....Added protection against trying to reconnect to the first isect point
*/
		if (nsfs <= 2 && i == nsfs-1)
		{
			start = 1;
			diff = 2;
		}
		else
		{
			start = 0;
			diff = 1;
		}
/*
.....Check to see if the neighbor segments intersect
*/
#if 0
		if (nbrs[i].key == 1280)
		{
			S_debug_nbr(&nbrs[i],UU_FALSE);
		}
#endif
		for (j=start;j<nbrs[i].no_bpts-1 && !done;j++)
		{
			for (k=0;k<nbrs[ind].no_bpts-diff && !done;k++)
			{
				um_isegseg (bpt[j].pt,bpt[j+1].pt,bpt2[k].pt,bpt2[k+1].pt,
					&nint,ipt,2.*tol);
				if (nint > 0)
				{
#if 0
	S_debug_connect(bpt[j].pt,bpt[j+1].pt,bpt2[k].pt,bpt2[k+1].pt,
		ipt,pl,UU_NULL,UU_NULL,0);
#endif
					done = UU_TRUE;
				}
			}
		}
/*
.....Check to see if extension intersects other neighbor
*/
		if (nint == 0)
		{
			j = -1;
			um_cross(bpt[ind2].nvec,bpt[ind2].fvec,pl.n); um_unitvc(pl.n,pl.n);
			um_vctovc(bpt[ind2].pt,pl.p0);
			for (k=0;k<nbrs[ind].no_bpts-diff;k++)
			{
				nint = um_iSegPlane(bpt2[k].pt,bpt2[k+1].pt,pl,ipt);
				if (nint > 0)
				{
#if 0
	S_debug_connect(bpt2[k].pt,bpt2[k+1].pt,UU_NULL,UU_NULL,
		ipt,pl,UU_NULL,UU_NULL,1);
#endif
					break;
				}
			}
		}
/*
.....Check to see if extension intersects other neighbor
*/
		if (nint == 0)
		{
			k = -1;
			um_cross(bpt2[ind3].nvec,bpt2[ind3].fvec,pl.n); um_unitvc(pl.n,pl.n);
			um_vctovc(bpt2[ind3].pt,pl.p0);
			for (j=start;j<nbrs[i].no_bpts-1;j++)
			{
				nint = um_iSegPlane(bpt[j].pt,bpt[j+1].pt,pl,ipt);
				if (nint > 0)
				{
#if 0
	S_debug_connect(bpt[j].pt,bpt[j+1].pt,UU_NULL,UU_NULL,
		ipt,pl,UU_NULL,UU_NULL,2);
#endif
					break;
				}
			}
		}
/*
.....Connect at intersection and remove points on wrong side of intersection
*/
		if (nint > 0)
		{
/*
.....Beginning of first curve and tail end of last curve intersect
*/
			if (i == 0 && ind == nsfs-1)
			{
				iadd = UU_FALSE;
				if (j >= 1)
				{
					if (um_sqdis(ipt,bpt[j].pt) > tol)
					{
						iadd = UU_TRUE;
						um_vctovc(ipt,tbpt.pt);
						um_vcmnvc(bpt[j].pt,ipt,tbpt.fvec);
						um_unitvc(tbpt.fvec,tbpt.fvec);
						um_vctovc(bpt[j].nvec,tbpt.nvec);
						uu_list_delete(nbrs[i].bpts,0,j);
					}
				}
				else
				{
					if (um_sqdis(ipt,bpt[ind3].pt) > tol)
					{
						iadd = UU_TRUE;
						um_vctovc(ipt,tbpt.pt);
						um_vcmnvc(bpt[ind3].pt,ipt,tbpt.fvec);
						um_unitvc(tbpt.fvec,tbpt.fvec);
						um_vctovc(bpt[ind3].nvec,tbpt.nvec);
					}
				}
				if (iadd)
				{
					uu_list_insert(nbrs[i].bpts,0,&tbpt);
					nbrs[i].no_bpts = UU_LIST_LENGTH(nbrs[i].bpts);
/*
........Add isect point to second neighbor and delete points after isect point
*/
					if (k >= 0)
					{
						um_vcmnvc(ipt,bpt2[k-1].pt,tbpt.fvec);
						um_unitvc(tbpt.fvec,tbpt.fvec);
						um_vctovc(bpt2[k-1].nvec,tbpt.nvec);
						uu_list_delete(nbrs[ind].bpts,k,nbrs[i].no_bpts-k);
					}
					else
					{
						um_vctovc(ipt,tbpt.pt);
						um_vcmnvc(ipt,bpt[ind2].pt,tbpt.fvec);
						um_unitvc(tbpt.fvec,tbpt.fvec);
						um_vctovc(bpt[ind2].nvec,tbpt.nvec);
					}
					uu_list_push(nbrs[ind].bpts,&tbpt);
					nbrs[ind].no_bpts = UU_LIST_LENGTH(nbrs[ind].bpts);
				}
			}
/*
.....Add isect point to first neighbor and delete points beyond isect point
*/
			else
			{
				iadd = UU_TRUE;
				if (j >= 0)
				{
					if (um_sqdis(ipt,bpt[j].pt) < tol)
						iadd = UU_FALSE;
					else
					{
						um_vctovc(ipt,tbpt.pt);
						um_vcmnvc(ipt,bpt[j].pt,tbpt.fvec);
						um_unitvc(tbpt.fvec,tbpt.fvec);
						um_vctovc(bpt[j].nvec,tbpt.nvec);
						uu_list_delete(nbrs[i].bpts,j+1,nbrs[i].no_bpts-j-1);
					}
				}
				else
				{
					if (um_sqdis(ipt,bpt[ind2].pt) < tol)
						iadd = UU_FALSE;
					else
					{
						um_vctovc(ipt,tbpt.pt);
						um_vcmnvc(ipt,bpt[ind2].pt,tbpt.fvec);
						um_unitvc(tbpt.fvec,tbpt.fvec);
						um_vctovc(bpt[ind2].nvec,tbpt.nvec);
					}
				}
				if (iadd)
				{
					uu_list_push (nbrs[i].bpts,&tbpt);
					nbrs[i].no_bpts = UU_LIST_LENGTH(nbrs[i].bpts);
/*
.....Add isect point to second neighbor and delete points before isect point
*/
					if (k >= 0)
					{
						um_vcmnvc(bpt2[k].pt,ipt,tbpt.fvec);
						um_unitvc(tbpt.fvec,tbpt.fvec);
						um_vctovc(bpt2[k].nvec,tbpt.nvec);
						uu_list_delete(nbrs[ind].bpts,0,k+1);
					}
					else
					{
						um_vctovc(ipt,tbpt.pt);
						um_vcmnvc(bpt2[ind3].pt,ipt,tbpt.fvec);
						um_unitvc(tbpt.fvec,tbpt.fvec);
						um_vctovc(bpt2[ind3].nvec,tbpt.nvec);
					}
					uu_list_insert (nbrs[ind].bpts,0,&tbpt);
					nbrs[ind].no_bpts = UU_LIST_LENGTH(nbrs[ind].bpts);
				}
			}
/*
.....Update values and pointers
*/
			bpt = (ncl_bndpt_struc *) UU_LIST_ARRAY(nbrs[i].bpts);
			bpt2 = (ncl_bndpt_struc *) UU_LIST_ARRAY(nbrs[ind].bpts);
		}
/*
.....Check to see if extension of both has intersection
*/
		else
		{
			nint = 0;
			um_vctovc(bpt[ind2].pt,pt1); um_vctovc(bpt[ind2].fvec,fvec1);
			um_vctovc(bpt2[ind3].pt,pt2); um_vctovc(bpt2[ind3].fvec,fvec2);
			if (fabs(UM_DOT(fvec1,fvec2)) < 0.975)
			{
				um_cross(fvec1,fvec2,nvec); um_unitvc(nvec,nvec);
				um_cross(nvec,fvec1,pvec); um_unitvc(pvec,pvec);
				um_negvc(fvec2,nfvec2);
				um_ilnpln(pt2,nfvec2,pt1,pvec,&nint,ipt);
			}
			if (nint > 0)
			{
#if 0
	S_debug_connect(pt1,pt2,UU_NULL,UU_NULL,
		ipt,pl,fvec1,fvec2,3);
#endif
				um_vctovc(ipt,tbpt.pt);
				um_cross(fvec1,fvec2,nvec); um_unitvc(nvec,nvec);
				um_vctovc(nvec,tbpt.nvec);
				um_vcmnvc(ipt,pt1,tbpt.fvec);
				um_unitvc(tbpt.fvec,tbpt.fvec);
				uu_list_push(nbrs[i].bpts,&tbpt);
				um_vcmnvc(pt2,ipt,tbpt.fvec);
				um_unitvc(tbpt.fvec,tbpt.fvec);
				uu_list_insert(nbrs[ind].bpts,0,&tbpt);
				nbrs[i].no_bpts++;
				nbrs[ind].no_bpts++;
				bpt = (ncl_bndpt_struc *) UU_LIST_ARRAY(nbrs[i].bpts);
				bpt2 = (ncl_bndpt_struc *) UU_LIST_ARRAY(nbrs[ind].bpts);
			}
		}
	}
/*
.....Push connected points onto combined list
*/
	for (i=0;i<nsfs;i++)
	{
		bpt = (ncl_bndpt_struc *) UU_LIST_ARRAY(nbrs[i].bpts);
		for (j=0;j<nbrs[i].no_bpts;j++)
		{
			npts++;
			uu_list_push(&pt_list,&bpt[j].pt);
			uu_list_push(&tan_list,&bpt[j].fvec);
		}
	}
/*
.....Clean up complete list of points.
.....Delete any remaining loops and weed the final list.
*/
	pts = (UM_coord *) UU_LIST_ARRAY(&pt_list);
	tans = (UM_vector *) UU_LIST_ARRAY(&tan_list);
	if (um_sqdis(pts[0],pts[npts-1]) > tolsq)
	{
		um_vctovc(pts[0],ipt);
		um_vctovc(tans[0],pvec);
		uu_list_push(&pt_list,&ipt);
		uu_list_push(&tan_list,&pvec);
		npts++;
	}
#if 0
	ncl_debug_pts(&pt_list,0);
#endif
	ncl_cv_weed (&pt_list,&tan_list,1.*tol,UU_FALSE,&npts);
#if 0
	ncl_debug_pts(&pt_list,0);
#endif
/*
.....Only fix curves if there was an offset
*/
	if (idid)
	{
		minrad = minrad/(30.*tol);
/*
.....This call is not needed per nevmill_offset.pp
*/
/*		ncl_wcstomcs(1,nzvec,nzvec);*/
		ncl_cv_deloop(&pt_list,&tan_list,UU_TRUE,2,nzvec,minrad,tol,tolsq,&npts);
#if 0
		ncl_debug_pts(&pt_list,0);
#endif
/*
.....Added new fold delete routine that does not depend on the direction
.....of the forward vectors - ASF 11/4/13.
		ncl_cv_defold (&pt_list,&tan_list,UU_TRUE,10.*tolsq,&npts);
*/
		S_pocket_defold (&pt_list,&tan_list,UU_TRUE,10.*tolsq,&npts);
#if 0
		ncl_debug_pts(&pt_list,0);
#endif
		n1 = npts;
		ncl_fix_corner0 (&pt_list,&tan_list,tol,&n1);
#if 0
		ncl_debug_pts(&pt_list,0);
#endif
		if (n1 > npts)
		{
			npts = n1;
			ncl_cv_deloop0 (&pt_list,&tan_list,nzvec,minrad,tol,tolsq,&npts);
		}
	}
#if 0
		ncl_debug_pts(&pt_list,0);
#endif
	*nopts = npts;
	if (flag < 2)
	{
		attr.key = sfkey;
		color = 4;
		ur_retrieve_attr(&attr);
		attr.line_weight = 1;
		attr.color = color;
		attr.key = 0;
		attr.line_weight = 2;
		pts = (UM_coord *) UU_LIST_ARRAY(&pt_list);
		status = um_c42_genpoly(pts,npts,&attr);
	}
	if (flag > 0)
	{
		if (!Spt_init)
		{
			uu_list_init(&Sptlst,sizeof(UM_coord),npts,10);
			Spt_init = UU_TRUE;
		}
		uu_list_push_list(&Sptlst,&pt_list);
	}
	uu_list_free(&tan_list);
	uu_list_free(&pt_list);
	return(status);
}

/*********************************************************************
**    I_FUNCTION     : S_copy_nbr (nbr1,nbr2)
**      Create a single, connected, polyline through the offset
**      boundary points.
**    PARAMETERS
**       INPUT  :
**          sfkey    - Key of pocket bottom surface.
**          nbr_list - List of surface data and boundary points
**          num_sfs  - Number of entries in nbr_list
**          nzvec    - Pocket bottom normal vector.
**          flag     - 0: Generate polyline only.
**                     1: Generate points and display polyline.
**                     2: Generate points only.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_copy_nbr (nbr1,nbr2)
ncl_nbr_struc *nbr1,*nbr2;
{
	int i;
	ncl_bndpt_struc *bpts,bpt;

	nbr2->key = nbr1->key;
	nbr2->no_bpts = nbr1->no_bpts;
	nbr2->bpts = (UU_LIST *) uu_malloc(sizeof(UU_LIST));
	uu_list_init(nbr2->bpts,sizeof(ncl_bndpt_struc),nbr2->no_bpts,50);
	bpts = (ncl_bndpt_struc *)UU_LIST_ARRAY(nbr1->bpts);
	for (i=0;i<nbr2->no_bpts;i++)
	{
		um_vctovc(bpts[i].pt,bpt.pt);
		um_vctovc(bpts[i].fvec,bpt.fvec);
		um_vctovc(bpts[i].nvec,bpt.nvec);
		bpt.cvu = bpts[i].cvu;
		bpt.sfu = bpts[i].sfu;
		bpt.sfv = bpts[i].sfv;
		bpt.remove = bpts[i].remove;
		uu_list_push(nbr2->bpts,&bpt);
	}
	nbr2->cvu0 = nbr1->cvu0;
	nbr2->cvu1 = nbr1->cvu1;
	nbr2->rad = nbr1->rad;
	nbr2->orient = nbr1->orient;
	nbr2->check = nbr1->check;
	nbr2->open = nbr1->open;
}

/*********************************************************************
**    E_FUNCTION     : S_pocket_defold (cvpoint,cvtang,closed,tol,tolsq,n1)
**       Get rid of zigzags in the offset boundary. Note that the
**       defold routine in necvfix.c would not work for this case
**       since the forward vectors will not show there is a fold.  All
**       forward vectors will likely be in a similar direction in any
**       created folds.
**    PARAMETERS
**       INPUT  :
**         cvpoint - list of evolved pts
**         cvtang  - list of tangent vectors
**         tol     - tolerance
**         npts    - number of evolved pts
**       OUTPUT :
**         cvpoint - fixed list of evolved pts
**         cvtang  - fixed list of tangent vectors
**         npts    - fixed number of pts
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_pocket_defold (cvpoint,cvtang,closed,tolsq,n1)
UU_REAL tolsq;
UU_LIST *cvpoint,*cvtang;
int closed,*n1;
{
	int ipts,npts,k;
	UM_coord *pp;
	UM_vector *vv;
	UM_vector vvp,vv1,vv2;
	UU_REAL dd,co;
	UU_REAL tolsq0 = tolsq/4.;

	npts = *n1;

	for (ipts = 0; ipts < npts-1; ipts++)
	{
		pp = (UM_coord *) UU_LIST_ARRAY(cvpoint);
		vv = (UM_vector *) UU_LIST_ARRAY(cvtang);
		um_vcmnvc (pp[ipts+1],pp[ipts],vvp);
		dd = UM_DOT (vvp,vvp);
		if (dd < tolsq0)
		{
			uu_list_delete (cvpoint,ipts+1,1);
			uu_list_delete (cvtang,ipts+1,1);
			npts--;
			ipts--;
			continue;
		}
		if (dd < tolsq) continue;
		dd = sqrt (dd);
		for (k = 0; k < 3; k++)	vvp[k] = vvp[k]/dd;

		k = (ipts+2)%npts;
		um_vcmnvc(pp[ipts+1],pp[ipts],vv1); um_unitvc(vv1,vv1);
		um_vcmnvc(pp[k],pp[ipts+1],vv2); um_unitvc(vv2,vv2);

		co = UM_DOT (vv1,vv2);
		if (co < -0.99 && closed == 1)
		{
			uu_list_delete (cvpoint,ipts,1);
			uu_list_delete (cvtang,ipts,1);
			uu_list_delete (cvpoint,ipts,1);
			uu_list_delete (cvtang,ipts,1);
			npts-=2;
			ipts-=2;
			continue;
		}
	}

	*n1 = npts;

	return;

}

/*********************************************************************
**    E_FUNCTION     : S_cvio_getpts(nearpt,pts,uvs)
**       Return the points from the surface slice.  If the slice created
**       multiple curves, then return the points from the curve closest 
**       to the specified near point.
**    PARAMETERS
**       INPUT  :
**         nearpt  - Near point used to determine which slice to use.
**       OUTPUT :
**         pts     - List of points in slice.
**         uvs     - List of corresponding UV points.  If set to UU_NULL,
**                   then the UV points are not returned.
**    RETURNS      : Number of points in 'pts'.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_cvio_getpts(nearpt,pts,uvs)
UM_coord nearpt,**pts,**uvs;
{
	int i,inc,ncv,npts;
	UU_REAL d0,d1,dis;
	UM_coord *tpts,*tuvs;
/*
.....Get number of intersection curves
*/
	ncv = um_cvio_getncv();
/*
.....No curves defined
*/
	if (ncv == 0)
		npts = 0;
/*
.....A single curve defined
*/
	else if (ncv == 1)
		um_cvio_get(0,&npts,pts,&tuvs);
/*
.....Multiple curves defined
*/
	else
	{
		dis = 10000.;
		inc = 0;
		for (i=0;i<ncv;i++)
		{
			um_cvio_get(i,&npts,&tpts,&tuvs);
			d0 = um_dcccc(tpts[0],nearpt); d1 = um_dcccc(tpts[npts-1],nearpt);
			if (d1 < d0) d0 = d1;
			if (d0 < dis)
			{
				dis = d0;
				inc = i;
			}
		}
		um_cvio_get(inc,&npts,pts,&tuvs);
	}
/*
.....End of routine
*/
	if (uvs != UU_NULL) *uvs = tuvs;
	return(npts);
}
