/********************************************************************* 
**    NAME         :  newaterln.c
**       CONTAINS:  Routines for calculating waterline geometry
**
**        wgtnpt
**        wgtpt
**        wgtci
**        ncl_rotate_plane
**        ncl_find_bound_stock
**        ncl_find_stock
**        ncl_add_stk
**        ncl_loop_to_contour
**        ncl_loops_to_pol
**        ncl_dp_ins
**        ncl_add_node
**        ncl_find_2keys
**
**    COPYRIGHT 2001 (c) Numerical Control Computer Sciences Inc.
**                          All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       newaterln.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:59
*********************************************************************/
#include "nclwaterln.h"
#include "nclclip.h"

static NCL_w_arc *wcirc = UU_NULL;

/*********************************************************************
**    E_FUNCTION     : ncl_rotate_plane (pl,mm)
*********************************************************************/
int ncl_rotate_plane (pl,mm)
struct NCL_nclpl_rec *pl;
UM_int2 mm;
{
	int i,irot;
	UM_transf rtinv;
	int status = UU_SUCCESS;

	ncl_get_rotmx (&irot,rtinv);
	if (irot <= 0) return (status);
							
	um_cctmtf(pl->pt,rtinv,pl->pt);
	if (irot == 1)
	{
		um_vctmtf(pl->nvec,rtinv,pl->nvec);
		for (i = 0; i < 3 && mm == 1; i++) pl->nvec[i] *= 25.4;
	}
	if (!ncl_is_watrev())
		status = ur_update_data_fixed (pl);

	return (status);
}

/*********************************************************************
*********************************************************************/
void ncl_set_wcirc (head)
NCL_w_arc *head;
{
	wcirc = head;
}

/*********************************************************************
**    E_FUNCTION     : void wgtnpt(ix, npts, ier)
**       Return the number of points in the current ix-th loop
**    PARAMETERS
**       INPUT  :
**          ix                current loop index
**       OUTPUT :
**          npts              number of points
**          lcirc             full circle flag
**          ier               error (zero if none)
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void wgtnpt(ix,npts,lcirc,ier)
UM_int2 *ix,*npts,*lcirc,*ier;
{
	int c,c0,cstk;
	ncl_polygon *cpol;
	UU_LIST *clst = NULLST;

	ncl_get_current_contour (&c0);
	ncl_get_current_pol (&cpol);
	ncl_get_cstk (&cstk);
	ncl_get_stklst (&clst);

	*ier = *lcirc = 0;
	c = c0 + *ix - 1;
	if (c < 0 || c >= cpol->num_contours)
	{
		*ier = 1;
		return;
	}
	if (wcirc && c == wcirc->c && wcirc->ccw == 0)
	{
		*lcirc = 1;
		*npts = 0;
	}
	else if (c == cstk && clst != NULLST)
		*npts = clst->cur_cnt;
	else	
		*npts = abs(cpol->np[c]) + 1;
}

/*********************************************************************
**    E_FUNCTION     : void wgtci (buf,ix,ier)
**       Return the data of the current full-circle contour
**    PARAMETERS
**       INPUT  :
**          ix                current loop index
**       OUTPUT :
**          buf               buffer to place the data
**          ier               error (zero if none)
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void wgtci (buf,ix,ier)
UM_int2  *ix,*ier;
UM_real8 buf[];
{
	int c,c0;
	UU_REAL z;
	ncl_polygon *cpol;

	ncl_get_current_contour (&c0);
	ncl_get_current_pol (&cpol);

	*ier = 0;
	c = c0 + *ix - 1;
	if (c < 0 || c >= cpol->num_contours || 
		!wcirc || c != wcirc->c || wcirc->ccw != 0)
	{
		*ier = 1;
		return;
	}

	ncl_get_wlev (&z);
	buf[6] = wcirc->rad;
	buf[0] = wcirc->center[0]; buf[1] = wcirc->center[1];
	buf[2] = z;

	wcirc = wcirc->next;
}

/*********************************************************************
**    E_FUNCTION     : void wgtpt (buf,ix,jx,ier)
**       Return the jx-th point in the current ix-th loop
**    PARAMETERS
**       INPUT  :
**          ix                current loop index
**          jx                current point index
**       OUTPUT :
**          buf               buffer to place the data
**          lcirc             circular arc flag
**          jnext             the last arc point, if ix is the first one 
**          ier               error (zero if none)
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void wgtpt (buf,ix,jx,ier,lcirc,jnext)
UM_int2  *ix,*jx,*ier,*lcirc,*jnext;
UM_real8 buf[];
{
	int c,c0,cstk,j,np;
	int *nj;
	UM_2Dcoord *vtx;
	UU_REAL z;
	ncl_polygon *cpol;
	UU_LIST *clst = NULLST;

	ncl_get_current_contour (&c0);
	ncl_get_current_pol1 (&cpol,&nj);
	ncl_get_cstk (&cstk);
	ncl_get_stklst (&clst);

	np = 0;

	*ier = *lcirc = 0;
	c = c0 + *ix - 1;
	if (c < 0 || c >= cpol->num_contours)
	{
		*ier = 1;
		return;
	}
	j = *jx - 1;
	if (c == cstk && clst != NULLST)
	{
		np = clst->cur_cnt - 1;
	}
	else
		np = abs(cpol->np[c]);
	if (np <= 0 || j < 0 || j > np)
	{
		*ier = 1;
		return;
	}
	if (j == np) j = 0;
	if (c == cstk && clst != NULLST)
	{
		vtx = (UM_2Dcoord *) UU_LIST_ARRAY (clst);
	}
	else
	{
		vtx = (UM_2Dcoord *) UU_LIST_ARRAY (cpol->contour);
		vtx += nj[c];
	}
		
	ncl_get_wlev (&z);

	buf[0] = vtx[j][0]; buf[1] = vtx[j][1];
	buf[2] = z;

	if (wcirc && c == wcirc->c && (j == wcirc->j0 || j-1 == wcirc->j0))
	{
		if (j-1 == wcirc->j0)
		{
			j--;
			buf[0] = vtx[j][0]; buf[1] = vtx[j][1];
			*lcirc = -1;
		}
		else
			*lcirc = 1;

		*jnext = wcirc->j1 + 1;
		buf[3] = wcirc->rad;
		buf[4] = wcirc->ccw;
		buf[5] = wcirc->center[0]; buf[6] = wcirc->center[1];
		buf[7] = z;
		buf[8] = vtx[wcirc->j1][0]; buf[9] = vtx[wcirc->j1][1];
		buf[10] = z;
		wcirc = wcirc->next;
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_find_2keys(sff,numsf,nj0,nj1,key0,key1)
**  Find surface keys by the intersection list indices.
**    PARAMETERS
**       INPUT  :
**          numsf      - number of surfaces
**          sff        - data for each surface
**          nj0,nj1    - starting indices in the intersection list
**       OUTPUT :
**          key0,key1        - the keys
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_find_2keys(sff,numsf,nj0,nj1,key0,key1)
int numsf,nj0,nj1;
NCL_waterline_surf *sff;
UU_KEY_ID *key0,*key1;
{
	UU_LOGICAL found[2];
	int i;
	NCL_waterline_surf *sfi;

	*key0 = *key1 = NULLKEY;
	found[0] = found[1] = UU_FALSE;
	if (nj0 < 0) found[0] = UU_TRUE;
	if (nj1 < 0) found[1] = UU_TRUE;
	for (i = 0, sfi = sff; i < numsf && (!found[0] || !found[1]); i++,sfi++)
	{
		if (sfi->slist == LIST_C) continue;
		if (!found[0])
		{
			if (sfi->plist0 == nj0)
			{
				found[0] = UU_TRUE;
				*key0 = sfi->key;
			}
			else if (sfi->plist0 > nj0 && key0 != NULLKEY)
				found[0] = UU_TRUE;
			else if (sfi->plist0 >= 0)
				*key0 = sfi->key;
		}
		if (!found[1])
		{
			if (sfi->plist0 == nj1)
			{
				found[1] = UU_TRUE;
				*key1 = sfi->key;
			}
			else if (sfi->plist0 > nj1 && key0 != NULLKEY)
				found[1] = UU_TRUE;
			else if (sfi->plist0 >= 0)
				*key1 = sfi->key;
		}
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_find_boundstock (box0,c0,eps)
**       In the current polygon: find the contour number for the 
**       stock generated by bounding surfaces.
**    PARAMETERS
**       INPUT  : 
**                box0   - box structure for the box stock
**                eps    - tolerance
**       OUTPUT :
**                c0     - contour number
**    RETURNS      : none
**         UU_SUCCESS iff found; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_find_bound_stock (box0,c0,eps)
UM_2box *box0;
int *c0;
UU_REAL eps;
{
	int i,nc;
	ncl_polygon *cpol;

	ncl_get_current_pol (&cpol);
	
	nc = cpol->num_contours;
	if (nc < 1) return(-1);

	for (i = 0; i < nc; i++)
	{
		if (fabs(cpol->box[i].xmin - box0->xmin) < eps &&
			fabs(cpol->box[i].ymin - box0->ymin) < eps &&
			fabs(cpol->box[i].xmax - box0->xmax) < eps &&
			fabs(cpol->box[i].ymax - box0->ymax) < eps)
		{
			*c0 = i; return(0);
		}
	}

	return(-1);
}

/*********************************************************************
**    E_FUNCTION     : ncl_find_stock (box0,c0,eps)
**       In the current polygon: find the contour number for the box stock.
**    PARAMETERS
**       INPUT  : 
**                box0   - box structure for the box stock
**                eps    - tolerance
**       OUTPUT :
**                c0     - contour number
**    RETURNS      : none
**         UU_SUCCESS iff found; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_find_stock (box0,c0,eps)
UM_2box *box0;
int *c0;
UU_REAL eps;
{
	int i,nc;
	ncl_polygon *cpol;

	ncl_get_current_pol (&cpol);
	
	nc = cpol->num_contours;
	if (nc < 1) return(-1);

	for (i = 0; i < nc; i++)
	{
		if (cpol->np[i] == 4)
		{
			if (fabs(cpol->box[i].xmin - box0->xmin) < eps &&
				fabs(cpol->box[i].ymin - box0->ymin) < eps &&
				fabs(cpol->box[i].xmax - box0->xmax) < eps &&
				fabs(cpol->box[i].ymax - box0->ymax) < eps)
			{
				*c0 = i; return(0);
			}
		}
	}

	return(-1);
}

/*********************************************************************
**    E_FUNCTION     : ncl_add_stk (poctype,curves,njlist)
**       Add the box stock if there is a stock specified, put the
**       contour depth numbers into the list - for reordering.
*********************************************************************/
void ncl_add_stk (poctype,curves,njlist,stkbox)
int poctype;
UU_LIST *curves,*njlist,*stkbox;
{
	int i,nnl;
	NCL_w_geo *cvs;
	int ncvs;
				
	if (poctype != 1 && poctype != 2) /* no stock */
	{
		cvs = (NCL_w_geo *) UU_LIST_ARRAY (curves);
		ncvs = curves->cur_cnt;
		njlist->cur_cnt = 0;
		for (i = 0; i < ncvs; i++)
		{
			nnl = cvs[i].depth;
			uu_list_push (njlist,&nnl);
		}
	}
	else
	{
/*
..... Add box stock as the outer contour; if the contour stock is specified (PART),
..... it will replace the box later. Reason - polygon operations are much easier
..... with a box.
*/		
		uu_list_push (curves,stkbox);
		cvs = (NCL_w_geo *) UU_LIST_ARRAY (curves);
		ncvs = curves->cur_cnt;
		njlist->cur_cnt = 0;
		for (i = 0; i < ncvs-1; i++)
		{
			nnl = cvs[i].depth + 1;
			uu_list_push (njlist,&nnl);
			if (cvs[i].inside == -1) cvs[i].inside = ncvs-1;
		}
		nnl = 0;
		uu_list_push (njlist,&nnl);
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_dp_ins (vti,nv,bpol,bnum,bnv0)
**       Determine whether a 2D curve is inside a 2D pocket
**    PARAMETERS
**       INPUT : 
**                vti      - the curve points
**                bpol     - polygon structure containing the pocket geo
**                bnum     - the contour number for the pocket perimeter
**                bnv0     - starting of the pocket perimeter in the polygon's
**                           array of points
**       OUTPUT:  1 iff inside, 0 iff on boundary, -1 iff outside
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int ncl_dp_ins (vti,bpol,bnum,bnv0,tol)
UM_2Dcoord *vti;
int bnum,bnv0;
ncl_polygon *bpol;
UU_REAL tol;
{
	int ins,ins0,i,npt,*nps;
	UM_2Dcoord *btx;

	btx = (UM_2Dcoord *) UU_LIST_ARRAY (bpol->contour);
	btx += bnv0;
	nps = (int *) bpol->np;

	npt = nps[bnum];
	ins0 = um_check_inside(btx,npt,vti[0],&bpol->box[bnum],tol);
	if (ins0 < 0) return (ins0);
	btx += npt;

	for (i = bnum+1; i < bpol->num_contours && nps[i] < 0; i++)
	{
		npt = -nps[i];
		ins = um_check_inside(btx,npt,vti[0],&bpol->box[i],tol);
		ins = -ins;
		if (ins < 0) return (ins);
		ins0 = ins0*ins;

		btx += npt;
	}
	return (ins0);
}

/*********************************************************************
*********************************************************************/
int ncl_add_node (levbetween,itim,ncvs,cstk,njlist,pok,njpok,head,tol)
UU_LOGICAL levbetween;
int itim,ncvs,cstk;
UU_LIST *njlist,*njpok;
ncl_polygon *pok;
NCL_dpnode **head;
UU_REAL tol;
{
	ncl_polygon *bpol,*cpol;
	int i,nc,c,ins,bnum,bnv0,*nj,*bnj;
	NCL_dpnode *dpnew,*cprev,*cp0,*bcur,*bnx;
	UM_2Dcoord *vtx,*vti;
	UU_REAL z;
	static NCL_dpnode *bp0 = UU_NULL;

	if (itim > 0)
	{
		bpol = &pok[itim-1];
		bnj = (int *) UU_LIST_ARRAY (&njpok[itim-1]);
	}

	nc = njlist->cur_cnt;

	ncl_get_current_pol (&cpol);

	uu_list_init (&njpok[itim], sizeof(int), nc, nc);
	uu_list_push_list (&njpok[itim],njlist);

	nj = (int *) UU_LIST_ARRAY (njlist);
	vtx = (UM_2Dcoord *) UU_LIST_ARRAY (cpol->contour);

	ncl_get_wlev (&z);
/*
..... cprev is the previous node on the current level
..... cp0 is the first node on the current level
*/
	cprev = cp0 = UU_NULL;

	for (i = c = 0; i < ncvs && c < cpol->num_contours; i++, c++)
	{
		if (cpol->np[c] < 0) continue;
		vti = vtx + nj[c];

		dpnew = (NCL_dpnode *) uu_malloc (sizeof (NCL_dpnode));
		if (!dpnew) return (-1);

		dpnew->itime = itim;
		dpnew->cnum = c;
		dpnew->cstk = cstk;
		dpnew->levbetween = levbetween;
		dpnew->htop = z;
		dpnew->next = dpnew->lnext = UU_NULL;

		if (*head == UU_NULL)
			*head = dpnew;
		if (cp0 == UU_NULL)
			cp0 = dpnew;

		if (cprev)
		{
			cprev->lnext = dpnew;
			if (itim == 0) cprev->next = dpnew;
		}

		if (itim > 0)
		{
/*
..... bp0 is the first node on the previous level
*/
			ins = -1;

			for (bcur = bp0; bcur != UU_NULL; bcur = bcur->lnext)
			{
				bnum = bcur->cnum; bnv0 = bnj[bnum];
				ins = ncl_dp_ins (vti,bpol,bnum,bnv0,tol);
				if (ins >= 0) break;
			}

			if (ins == -1) return (-1);

			bnx = bcur->next;

			if (bnx != UU_NULL && bnx == cprev)
			{
				bnx = cprev->next;
				cprev->next = dpnew;
			}
			else
				bcur->next = dpnew;

			dpnew->next = bnx;
		}
		cprev = dpnew;
	}

	bp0 = cp0;
	return (0);
}

/*********************************************************************
**    E_FUNCTION     : void ncl_loop_to_contour(pock,c,cvsi,inner)
**       Write an intersection contour into a polygon structure.
*********************************************************************/
static void ncl_loop_to_contour(pock,c,cvsi,inner)
int c,inner;
ncl_polygon *pock;
NCL_w_geo *cvsi;
{
	int iv,nv;		
	
	nv = cvsi->np;
	pock->np[c] = (inner == 0)? nv: -nv;
	pock->box[c].xmin = cvsi->xrange[0];
	pock->box[c].xmax = cvsi->xrange[1];
	pock->box[c].ymin = cvsi->yrange[0];
	pock->box[c].ymax = cvsi->yrange[1];
	for (iv = 0; iv < nv; iv++)
		uu_list_push (pock->contour,cvsi->pt[iv]);
}

/*********************************************************************
**    E_FUNCTION     : void ncl_loop_to_contour(pock,c,cvsi,inner)
**       Write an intersection contour into a polygon structure.
*********************************************************************/
static void ncl_stklst_to_contour(pock,c,stklst,bbox,inner)
int c,inner;
ncl_polygon *pock;
UM_2box *bbox;
UU_LIST *stklst;
{
	int iv,nv;	
	UM_2Dcoord *vtx;
	
	nv = stklst->cur_cnt;
	vtx = (UM_2Dcoord *) UU_LIST_ARRAY (stklst);

	pock->np[c] = (inner == 0)? nv: -nv;

	pock->box[c].xmin = bbox->xmin;
	pock->box[c].xmax = bbox->xmax;
	pock->box[c].ymin = bbox->ymin;
	pock->box[c].ymax = bbox->ymax;

	uu_list_push_multiple(pock->contour,nv,vtx);
}

/*********************************************************************
**    E_FUNCTION     : void ncl_loops_to_pol (dep,curves,pock,njlist)
**       Put intersection contours data into a polygon structure.
*********************************************************************/
void ncl_loops_to_pol (wbase,dep,curves,pock,njlist,poctype,offdis,tol2d,tol)
NCL_w_base *wbase;
int dep,poctype;
ncl_polygon *pock;
UU_LIST *curves,*njlist;
UU_REAL offdis,tol2d,tol;
{
	int i,iv,j,c,ncvs,nn,nv,status,NPT;
	UU_LIST stklst,contour,ptsio;
	UU_REAL toldsq;
	UM_2box bbox;

	NCL_w_geo *cvs = (NCL_w_geo *) UU_LIST_ARRAY (curves);
	int *nj = (int *) UU_LIST_ARRAY (njlist);
	toldsq = tol*tol;
	ncvs = curves->cur_cnt;
	NPT = 100;

	pock->np = (int *) uu_malloc (ncvs*sizeof(int));
	pock->box = (UM_2box *) uu_malloc (ncvs*sizeof(UM_2box));
	if (pock->np == UU_NULL || pock->box == UU_NULL) return;

	for (nn = ncvs, c = 0; nn > 0 && dep < ncvs;)
	{
		for (i = 0; i < ncvs && nn > 0; i++)
		{
			if (nj[i] == dep)
			{					
				if (poctype == STK_BOUNDSF)
				{
					uu_list_init0 (&stklst);
					uu_list_init (&ptsio, sizeof(UM_coord), 100, 100);	
					status = ncl_contour_on_bounding_surfaces(&cvs[i],tol);
					if (status == UU_SUCCESS)
					{						
						nv = cvs[i].np;
						uu_list_init (&contour, sizeof(UM_2Dcoord), nv, nv);			
						for (iv = 0; iv < nv; iv++)
							uu_list_push (&contour,cvs[i].pt[iv]);

						if (ncl_is_watrev())
						{
							status = ncl_offset_out1 (wbase,&stklst,&contour,
								&ptsio,offdis,tol2d,tol);
						}
						else
						{
							status = ncl_offset_out (&stklst,&contour,&ptsio,
								offdis,tol,toldsq);
						}
						bbox.xmin = cvs[i].xrange[0] - offdis;
						bbox.xmax = cvs[i].xrange[1] + offdis;
						bbox.ymin = cvs[i].yrange[0] - offdis;
						bbox.ymax = cvs[i].yrange[1] + offdis;
						ncl_stklst_to_contour(pock,c,&stklst,&bbox,0);
					}
					else
						ncl_loop_to_contour(pock,c,&cvs[i],0);
				}
				else
					ncl_loop_to_contour(pock,c,&cvs[i],0);

				c++;
				nj[i] = -1; nn--;
				for (j = 0; j < ncvs && nn > 0; j++)
				{
					if (nj[j] >= 0 && cvs[j].inside == i)
					{
						ncl_loop_to_contour(pock,c,&cvs[j],1);
						c++;
						nj[j] = -1; nn--;
					}
				}
			}
		}
		dep += 2;
	}
	pock->num_contours = c;
}

/*********************************************************************
*********************************************************************/
void ncl_free_nodes(head)
NCL_dpnode **head;
{
	NCL_dpnode *nod;

	while (*head != UU_NULL)
	{
		nod = *head; *head = nod->next; UU_FREE (nod);
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_pt_on_surface(pt,sfi,tol)
**		Check if point(pt) is on surface(sfi) or not
**    PARAMETERS
**       INPUT  :
**          pt		- the poin to check
**          sfi		- surface to check
**			tol  	- tol
**       OUTPUT : none
**    RETURNS      :
**       UU_SUCCESS if on the surface, UU_FAILURE if not
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_pt_on_surface(pt,sfi,tol)
UM_coord pt;
NCL_waterline_surf *sfi;
UU_REAL tol;
{
	int i,j,status,ncvs,n1,unflg,ierr;
	UU_REAL d1,du,dir,u,v;
	UM_trian *ptri;
	NCL_xtriangle *xt1;
	UM_coord npt1;
	UM_vector vc1;
	UU_REAL ncl_pttridis();
	
	status = UU_FAILURE;			
	du = 10000.;
	unflg=1;
/*
.....The surface has triangles
*/		
	if (sfi->trianlist)
	{
		n1 = sfi->trianlist->cur_cnt;
		ptri = (UM_trian *)UU_LIST_ARRAY(sfi->trianlist);
		xt1 = (NCL_xtriangle *)uu_malloc(n1*sizeof(*xt1));
		ncl_setup_xtriangles (n1,ptri,xt1);
/*
.....The minimum distance from pt
*/
		for (i=0;i<n1;i++)
		{
			if (!xt1->valid) continue;
			d1 = ncl_pttridis(pt,&xt1[i],npt1);
			if (d1 < du)
				du = d1;
		}
	}
	else
	{
		for (i=0;i<=10;i++)
		{
			for (j=0;j<=10;j++)
			{
				dir = 0.;
				u = (UU_REAL)i/10.;
				v = (UU_REAL)j/10.;
				sfpt2(&(sfi->key),pt,&u,&v,&unflg,&dir,npt1,vc1,&ierr);
				d1 = um_dcccc(pt,npt1);
				if (d1 < du && ierr <= 0)
					du = d1;
			}
		}
	}

	if (d1 < tol)
		status = UU_SUCCESS;

	return status;
}

