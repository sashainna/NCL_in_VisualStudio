/*********************************************************************
**    NAME         : m4cvio.c
**       CONTAINS: Surface/Surface intersection routines.
**
**           um_cvio_new()
**           um_cvio_create()
**           um_cvio_remove()
**           um_cvio_empty()
**           um_cvio_empty_all()
**           um_cvio_push()
**           um_cvio_push_at()
**           um_cvio_insert()
**           um_cvio_insert_at()
**           um_cvio_get()
**           um_cvio_getpts()
**           um_cvio_getncv()
**           um_cvio_getnutcv()
**           um_cvio_display()
**           um_cvio_intersection_srfbound()
**           um_cvio_free()
**           um_cvio_push_ncluv()
**           um_cvio_trim()
**           um_cvio_sort_points()
**           um_cvio_get_uv_point
**           um_cvio_project_point()
**           um_cvio_project_point_uv()
**           um_cvio_project_points()
**           um_cvio_intersection_tesses()
**           um_cvio_intersection_triangles()
**           um_cvio_intersection_boundary()
**           um_cvio_bndrpln()
**           um_cvio_inside_boundary()
**           um_cvio_sphere_srf(()
**           um_cvio_srf_sortvec()
**           um_cvio_displayboundry()
**           um_cvio_create_intersection()
**           um_cvio_sfplio()
**
**    COPYRIGHT 2009 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       m4cvio.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:01
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "class.h"
#include "mdrel.h"
#include "modef.h"
#include "mdeval.h"
#include "nccs.h"
#include "ncl.h"
#include "nclfc.h"
#include "mgeom.h"
#include "uminmax.h"

static char buf[120];
extern ncl_ioseg *head;
extern UU_LOGICAL NCL_waterline;
UU_LOGICAL lfirstime = UU_FALSE;
static UU_REAL rmax = 0.0;
static UU_REAL umin=0.0;
static UU_REAL umax=1.0;
static UU_REAL vmin=0.0;
static UU_REAL vmax=1.0;
static UM_int4 isubscr = 0;

struct {
	int ncv;          /*total number of curves*/
	int nutcv;        /*total number of untrimmed curves*/
	UU_LIST ptlist;   /*point list */
	UU_LIST uvlist;   /*uv point list */
} Scvio;

/**********************************************************************
**    E_FUNCTION     : um_cvio_new()
**       Allocate memory and initialize list of points used for 
**       creation of curves on surface.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_SUCCESS 
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
void um_cvio_new()
{
	Scvio.ncv = 0;
	Scvio.nutcv = 0;
	uu_list_init(&Scvio.ptlist, sizeof(UU_LIST), 20, 20);
	uu_list_init(&Scvio.uvlist, sizeof(UU_LIST), 20, 20);
}

/**********************************************************************
**    E_FUNCTION     : um_cvio_create()
**       Allocate memory and initialize list of points used for 
**       creation of new curves on surface.
**    PARAMETERS
**       INPUT  :
**          btrm   - Trim curve flag 
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_SUCCESS 
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
void um_cvio_create(btrm)
UU_LOGICAL btrm;
{
	UU_LIST ptlist,uvlist;

	Scvio.ncv++;
	if (!btrm)
		Scvio.nutcv++;
	uu_list_init(&ptlist, sizeof(UM_coord), 100, 50);
	uu_list_push(&Scvio.ptlist, &ptlist);
	uu_list_init(&uvlist, sizeof(UM_coord), 100, 50);
	uu_list_push(&Scvio.uvlist, &uvlist);
}

/**********************************************************************
**    E_FUNCTION     : um_cvio_remove()
**       Remove the last curve in the active Scvio list.
**    PARAMETERS
**       INPUT  :
**          btrm   - Trim curve flag
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_SUCCESS 
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
void um_cvio_remove(btrm)
UU_LOGICAL btrm;
{
	Scvio.ncv--;
	if (!btrm)
		Scvio.nutcv--;
	uu_list_delete(&Scvio.ptlist, Scvio.ncv, 1);
	uu_list_delete(&Scvio.uvlist, Scvio.ncv, 1);
}

/**********************************************************************
**    E_FUNCTION     : um_cvio_empty(cvn)
**       Empty the last curve in the active Scvio list.
**    PARAMETERS
**       INPUT  :
**          btrm   - Trim curve flag 
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_SUCCESS 
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
void um_cvio_empty(cvn)
UM_int2 cvn;
{
	UU_LIST *ptlist;
	ptlist = (UU_LIST *) UU_LIST_ARRAY (&Scvio.ptlist);	
	ptlist[cvn].cur_cnt = 0;
}

/**********************************************************************
**    E_FUNCTION     : um_cvio_empty_all()
**       Empty all curves in the active Scvio list.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_SUCCESS 
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
void um_cvio_empty_all()
{
	int icv;
	for (icv = 0; icv < Scvio.nutcv; icv++)
		um_cvio_empty(icv);

	Scvio.nutcv = 0;
	Scvio.ncv = 0;
}

/**********************************************************************
**    E_FUNCTION     : um_cvio_push(icv,pt,uv)
**       Push point and UV-point onto the icv-th curve in the Scvio list
**    PARAMETERS
**       INPUT  : 
**           icv  - The curve index in Scvio list
**           pt   - Point to store on list.
**           uv   - Associated UV-point to store on list.
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_SUCCESS 
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
int um_cvio_push_at(icv,pt,uv)
UM_int2 icv;
UM_coord *pt, *uv;
{	
	int status;
	UU_LIST *ptlist;
/*
.....Push coordinates onto lists
*/
	ptlist = (UU_LIST *) UU_LIST_ARRAY (&Scvio.ptlist);
	if (pt)
		status = uu_list_push(&ptlist[icv], pt);
	ptlist = (UU_LIST *) UU_LIST_ARRAY (&Scvio.uvlist);
	if (uv)
		status = uu_list_push(&ptlist[icv], uv);
/*
.....End of routine
*/
	return(status);
}

/**********************************************************************
**    E_FUNCTION     : um_cvio_push(pt,uv)
**       Push point, UV-point onto the active Scvio curve point list
**    PARAMETERS
**       INPUT  : 
**           pt   - Point to store on list.
**           uv   - Associated UV-point to store on list.
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_SUCCESS 
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
int um_cvio_push(pt,uv)
UM_coord *pt, *uv;
{	
	return um_cvio_push_at(Scvio.ncv-1,pt,uv);
}

/**********************************************************************
**    E_FUNCTION     : um_cvio_insert(pt,uv,ind)
**       Insert point and UV-point onto the active Scvio curve point list
**    PARAMETERS
**       INPUT  :
**           pt   - Point to store on list.
**           uv   - Associated UV-point to store on list.
**          ind   - Index at which to insert
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_SUCCESS 
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
int um_cvio_insert_at(cvn,pt,uv,ind)
UU_REAL *pt,*uv;
UM_int2 cvn,ind;
{
	int status;
	UU_LIST *ptlist;
/*
.....Push coordinates onto lists
*/
	ptlist = (UU_LIST *) UU_LIST_ARRAY (&Scvio.ptlist);
	if (pt)
		status = uu_list_insert(&ptlist[cvn], ind, pt);
	ptlist = (UU_LIST *) UU_LIST_ARRAY (&Scvio.uvlist);
	if (uv)
		status = uu_list_insert(&ptlist[cvn], ind, uv);
/*
.....End of routine
*/
	return(status);
}

/**********************************************************************
**    E_FUNCTION     : um_cvio_insert(pt,uv,vc,ind)
**       Insert point, UV-point, and tangent vector onto the active Scvio
**       curve point list
**    PARAMETERS
**       INPUT  :
**           pt   - Point to store on list.
**           uv   - Associated UV-point to store on list.
**          ind   - Index at which to insert
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_SUCCESS 
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
int um_cvio_insert(pt,uv,ind)
UU_REAL *pt,*uv;
UM_int2 ind;
{
	return um_cvio_insert_at(Scvio.ncv-1,pt,uv,ind);
}

/**********************************************************************
**    E_FUNCTION     : um_cvio_get(cvn,ptn,pt,uv)
**       Get point and UV-point in the active Scvio curve point list
**    PARAMETERS
**       INPUT  :
**          cvn    - the curve number 
**                 0: untimmed curve
**                 1: trimmed curve
**       OUTPUT :
**          ptn    - number of points in the list
**          pt     - Pointer to the array of points in the list
**          uv     - Pointer to the array of UV-points in the list
**    RETURNS      :
**       UU_SUCCESS 
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
void um_cvio_get(cvn,ptn,pt,uv)
int cvn, *ptn;
UM_coord **pt,**uv;
{	
	UU_LIST *ptlist;
	ptlist = (UU_LIST *) UU_LIST_ARRAY (&Scvio.ptlist);
	*ptn = UU_LIST_LENGTH(&ptlist[cvn]);
	*pt = (UM_coord *) UU_LIST_ARRAY (&ptlist[cvn]);
	ptlist = (UU_LIST *) UU_LIST_ARRAY (&Scvio.uvlist);
	*uv = (UM_coord *) UU_LIST_ARRAY (&ptlist[cvn]);
}

/**********************************************************************
**    E_FUNCTION     : um_cvio_get(cvn,ptn,pt)
**       Get the number of points and points in the active Scvio curve list
**    PARAMETERS
**       INPUT  :
**          ncv    - the curve number
**       OUTPUT :
**          ptn    - number of points in the list
**          pt     - the point in the list
**    RETURNS      :
**       UU_SUCCESS 
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
void um_cvio_getpts(cvn,ptn,pt)
int cvn, *ptn;
UM_coord **pt;
{	
	UU_LIST *ptlist;
	ptlist = (UU_LIST *) UU_LIST_ARRAY (&Scvio.ptlist);
	*ptn = UU_LIST_LENGTH(&ptlist[cvn]);
	*pt = (UM_coord *) UU_LIST_ARRAY (&ptlist[cvn]);
}

/**********************************************************************
**    E_FUNCTION     : um_cvio_getncv()
**       Get total number of trimmed curves in the active Scvio curve list
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      :
**         the total number of trimmed curves
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
int um_cvio_getncv()
{	
	return Scvio.ncv;
}

/**********************************************************************
**    E_FUNCTION     : um_cvio_getncv()
**       Get total number of untrimmed curves in the active Scvio curve list
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      :
**         the total number of untrimmed curves
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
int um_cvio_getnutcv()
{	
	return Scvio.nutcv;
}

/**********************************************************************
**    E_FUNCTION     : um_cvio_free()
**       Free the active Scvio curve list
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_SUCCESS 
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
void um_cvio_free()
{		
	Scvio.ncv =0;
	uu_list_free (&Scvio.ptlist);
	uu_list_free (&Scvio.uvlist);
}

/**********************************************************************
**    E_FUNCTION     : um_cvio_push_ncluv()
**       Put active Scvio curve list into NCL_uvintof
**       for waterline function
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_SUCCESS 
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
void um_cvio_push_ncluv()
{
	int icv,ncv,nutcv,npts,num;
	UM_int2 npt2;
	UM_coord *pts;

/*
.....Get the number of untrimmed curve and total curves
*/
	nutcv = um_cvio_getnutcv();
	ncv = um_cvio_getncv();
	if (ncv < 0) return;
	num = ncv-nutcv;
	if (num < 0) return;

	if (num > 0)
		num = nutcv;

	for(icv = num; icv <ncv; icv++)
	{
		um_cvio_getpts(icv,&npts,&pts);
		if (npts < 2)
			continue;
		npt2 = npts;
		ncl_put_uv(pts,&npt2);
		ncl_put_nios(npts);
	}
}

/*********************************************************************
**    E_FUNCTION     : um_cvio_display(itsk,icolor,idisp)
**      Display the curve function
**    PARAMETERS
**       INPUT  :
**          itsk   - curve flag
**                 -1 - all curves except CV0
**                  0 - CV0
**                  n - CVn
**         icolor  - color flag
**         idisp   - dislpy flag
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void um_cvio_display(itsk,icolor,idisp)
int itsk,icolor,idisp;
{
	int i,i1,npt,ncv,nutcv; 
	UM_coord *pts,*uvs;		
	char tbuf[80];

/*
.....Get the number of untrimmed curve and total curves
*/
	nutcv = um_cvio_getnutcv();
	ncv = um_cvio_getncv();

	sprintf(tbuf,"draft/modify,color=%d",icolor);
	NclxDbgPstr(tbuf);
	if (itsk == -1 && ncv-nutcv > 0)
	{
		for (i1 = nutcv; i1 < ncv; i1++)
		{
			um_cvio_get (i1,&npt,&pts,&uvs);

			if (npt > 1)
			{
				ncl_draw_polyline (npt,pts,icolor,idisp);
#if 1
				for (i=0;i<npt-1;i++)
				{
					sprintf(tbuf,"LN/%8.5f,%8.5f,%8.5f,%8.5f,%8.5f,%8.5f",
						pts[i][0],pts[i][1],pts[i][2],pts[i+1][0],pts[i+1][1],pts[i+1][2]);
					NclxDbgPstr(tbuf);
				}
/*
				if (icolor > 4)
				{
					for (i=0;i<npt;i++)
					{
						sprintf(tbuf,"pt/on, sf19, %10.6f,%10.6f",uvs[i][0],uvs[i][1]);
						NclxDbgPstr(tbuf);
					}
				}
*/
#endif
			}
		}
	}
	else if (ncv > 0)
	{
		for (i1 = 0; i1 < nutcv; i1++)
		{
			um_cvio_get (i1,&npt,&pts,&uvs);

			if (npt > 1)
			{
				ncl_draw_polyline (npt,pts,icolor,idisp);
#if 1
				for (i=0;i<npt/*-1*/;i++)
				{
					sprintf(tbuf,"PT/%8.5f,%8.5f,%8.5f",pts[i][0],pts[i][1],pts[i][2]);
//					sprintf(tbuf,"LN/%8.5f,%8.5f,%8.5f,%8.5f,%8.5f,%8.5f",
//						pts[i][0],pts[i][1],pts[i][2],pts[i+1][0],pts[i+1][1],pts[i+1][2]);
					NclxDbgPstr(tbuf);
				}
/*
				if (icolor > 4)
				{
					for (i=0;i<npt;i++)
					{
						sprintf(tbuf,"pt/on, sf19, %10.6f,%10.6f",uvs[i][0],uvs[i][1]);
						NclxDbgPstr(tbuf);
					}
				}
*/
#endif
			}
		}
	}
}

/*********************************************************************
**    E_FUNCTION     : um_cvio_intersection_srfbound()
**       Get intersection of untrimed curve with surface boundary
**       insert the intersection points into the curve
**    PARAMETERS
**      INPUT  :
**          srf      - pointer to surface entity
**          bound    - machine tolerance surface boundary
**          bound1   - tight tolerance surface boundary
**          tfmat    - surface matrix
**			pl		 - plane 
**			irot	 - tranformation flag((from Unibase to part)
**			luv		 - flag to check in uv space or not
**          itsk
**          tol      - tolerance
**      OUTPUT :
**          ier      - error number, if fail
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_cvio_intersection_srfbound(srf,bound,bound1,tfmat,pl,irot,luv,tol,inner)
struct NCL_fixed_databag *srf;
UM_srf_boundary *bound,*bound1;
struct NCL_nclpl_rec *pl;
UM_transf tfmat;
UU_LOGICAL luv;
UM_int2 irot,*inner;
UM_real8 *tol;
{
	int status,i,j,insf,imin,k,counting,n,ix,jx,nuv;
	UU_REAL dtol,dtol1,tolsq,d,dmin,lmt;
	UM_coord uvi,nearpt;
	UM_2Dcoord vln;
	UU_LIST iobuf,ptbuf;
	UM_pointd *uvpd;
	UM_coord *ptpd;
	UM_cvio *ciopd;
	UM_int2 type = NCLI_POINT;
	struct UM_evsrfout evsrf;
	UU_LOGICAL insert,linsert,lopen;

	int icv,ncv,npt,nutcv,insf1,insf2;
	UM_coord *pt,*uv;
	UM_vector *vc;
	UM_coord pp0;
	UU_REAL u0,v0,u,uscl,vscl;

	UM_coord pt1,pt2,*uvs;
	UM_vector vc1;
	UM_int2 iwf;
	int iflg,ierr,unflg = 1;
	iwf = 1;
	iflg = 20;
	u = .5;

	if (*tol <= 0.) return (UU_FAILURE);
	dtol = *tol;
	dtol1 = 0.1 * dtol;
	tolsq = dtol*dtol;
	insert = UU_FALSE;
	linsert = UU_FALSE;
	lopen = UU_TRUE;
	status = UU_SUCCESS;
	uscl = vscl = 1.;
	lmt = UM_FUZZ;
	if (bound->ummx[0][1]-bound->ummx[0][0] < dtol*5 ||
		bound->vmmx[0][1]-bound->vmmx[0][0] < dtol*5)
	{
		if (bound->ummx[0][1]-bound->ummx[0][0] < dtol*5) uscl = 500.;
		if (bound->vmmx[0][1]-bound->vmmx[0][0] < dtol*5) vscl = 500.;
		bound->ummx[0][0] *= uscl; bound->ummx[0][1] *= uscl;
		bound->vmmx[0][0] *= vscl; bound->vmmx[0][1] *= vscl;
		uvs = (UM_coord *)UU_LIST_ARRAY(bound->uvpts);
		for (i=0;i<UU_LIST_LENGTH(bound->uvpts);i++)
		{
			uvs[i][0] *= uscl;
			uvs[i][1] *= vscl;
		}
		bound1->ummx[0][0] *= uscl; bound1->ummx[0][1] *= uscl;
		bound1->vmmx[0][0] *= vscl; bound1->vmmx[0][1] *= vscl;
		uvs = (UM_coord *)UU_LIST_ARRAY(bound1->uvpts);
		for (i=0;i<UU_LIST_LENGTH(bound1->uvpts);i++)
		{
			uvs[i][0] *= uscl;
			uvs[i][1] *= vscl;
		}
	}
/*
.....Get the number of untrimmed curve and total curves
*/
	nutcv = um_cvio_getnutcv();

	for (icv = 0; icv < nutcv; icv++)
	{
/*
.....Get the untrimmed curve points, uv-points
*/	
		um_cvio_get (icv,&npt,&pt,&uv);
		n = npt;

/*
.....Get the intersections of curve with surface boundary,
.....insert the intersections into the list
*/
		uu_list_init (&iobuf, sizeof(UM_cvio), 20, 20);
			
		for (j = 0; j < npt; j++)
		{
			to_unbs (pt[j],pt[j],&type);
			uv[j][0] *= uscl; uv[j][1] *= vscl;
		}

		for (j = 0; j < n-1; j++)
		{
			um_vcmnvc_2d (uv[j+1],uv[j],vln);
			d = UM_MAG_2D (vln);
			if (d < lmt)
				ix = 0;
			else
			{
				vln[0] /= d; vln[1] /= d;
				ix = um_isect_cls_bndry0(uv[j],uv[j+1],pt[j],pt[j+1],vln,d,
									bound,bound1,pl,dtol1,&iobuf);
			}
/*
.....Found intersection points, insert if possible
*/
			if (ix > 0)
			{
				ciopd = (UM_cvio *) UU_LIST_ARRAY (&iobuf);

				for (jx = 0; jx < ix; jx++)
				{
					insert = (ciopd[jx].uv.param >= 0.0 && d - ciopd[jx].uv.param >= 0.0);
					if (insert)
					{
						if (irot > 0)
							to_unbs (ciopd[jx].pt,pp0,&type);
						else						
							um_vctovc (ciopd[jx].pt,pp0);						
						
						j++; n++;
						ciopd[jx].uv.point[2] = -1;

						um_cvio_insert_at(icv,pp0,ciopd[jx].uv.point,j);

						um_cvio_get (icv,&npt,&pt,&uv);
						linsert = UU_TRUE;
					}
					else
					{
						if (jx == ix-1 && j == n-2 && d - ciopd[jx].uv.param < 1.e-6)
							uv[j+1][2] = -1;
					}

/*
.....for a multi-point intersection: check if the middle 
.....between a pair of intersection points falls outside; 
.....if it does insert this middle into the list
*/
					if (jx < ix-1)
					{
						um_middlept_2d (ciopd[jx].uv.point,ciopd[jx+1].uv.point,uvi);

						status = uc_evsrf (UM_POINT,uvi[0],uvi[1],srf,tfmat,&evsrf);
						if (status != UU_SUCCESS) goto XYZ;

						insf = um_inside_bndr (uvi,evsrf.sp,bound,&dtol1,luv);
						if (insf == -1)
						{
							j++; n++;
							uvi[2] = 1;
							um_cvio_insert_at(icv,evsrf.sp,uvi,j);
							um_cvio_get (icv,&npt,&pt,&uv);
						}
					}
					else if (j >= 2 && uv[j-2][2] == -1 && uv[j][2] == -1)
					{
						insf = um_inside_bndr (uv[j-1],pt[j-1],bound,&dtol1,luv);
						if (insf == -1)
						{
							uv[j-1][2] = 1;
						}
					}
					else if (j == 1 && j == n-2 && uv[j-1][2] != -1 && 
							 uv[j][2] == -1 && uv[j+1][2] != -1)
					{											
						um_middlept_2d (uv[j-1],uv[j],uvi);
						status = uc_evsrf (UM_POINT,uvi[0],uvi[1],srf,tfmat,&evsrf);
						if (status != UU_SUCCESS) goto XYZ;
						insf1 = um_inside_bndr (uvi,evsrf.sp,bound,&dtol1,luv);
											
						um_middlept_2d (uv[j+1],uv[j],uvi);
						status = uc_evsrf (UM_POINT,uvi[0],uvi[1],srf,tfmat,&evsrf);
						if (status != UU_SUCCESS) goto XYZ;										
						insf2 = um_inside_bndr (uvi,evsrf.sp,bound,&dtol1,luv);

						if (insf1 == -1)
							uv[j+1][2] = -1;
						else if (insf2 == -1)
							uv[j-1][2] = -1;
					}
				}
			}
			iobuf.cur_cnt = 0;
		}

XYZ:
		um_cvio_get (icv,&npt,&pt,&uv);
		for (j = 0; j < npt; j++)
		{
			fr_unbs (pt[j],pt[j],&type);
			uv[j][0] /= uscl; uv[j][1] /= vscl;
		}

		uu_list_free (&iobuf);
	}

	if (uscl != 1. || vscl != 1.)
	{
		bound->ummx[0][0] /= uscl; bound->ummx[0][1] /= uscl;
		bound->vmmx[0][0] /= vscl; bound->vmmx[0][1] /= vscl;
		uvs = (UM_coord *)UU_LIST_ARRAY(bound->uvpts);
		for (i=0;i<UU_LIST_LENGTH(bound->uvpts);i++)
		{
			uvs[i][0] /= uscl;
			uvs[i][1] /= vscl;
		}
		bound1->ummx[0][0] /= uscl; bound1->ummx[0][1] /= uscl;
		bound1->vmmx[0][0] /= vscl; bound1->vmmx[0][1] /= vscl;
		uvs = (UM_coord *)UU_LIST_ARRAY(bound1->uvpts);
		for (i=0;i<UU_LIST_LENGTH(bound1->uvpts);i++)
		{
			uvs[i][0] /= uscl;
			uvs[i][1] /= vscl;
		}
	}

	if (!linsert)
		return UU_FAILURE;

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : um_cvio_trim(key1,key2,ier)
**       Trim the curves to surface boundary
**    PARAMETERS
**       INPUT  :
**          key1     - key of the surface
**          key2     - key of the surface (or plane surface)
**          itsk     - 0 - create all individual intersections;
**                     1 - create a single spline;
**          tol      - tolerance (=sc(27))
**          irpt     - 0 - create all individual intersections;
**                     1 - create a single spline,closest to near point
**          ptx      - near point
**          inner    - trim boundary flag
**                     0 - only untrimed CV0
**                     1 - trim to outer boundary
**                     2 - multi-trim to all boundaries
**       OUTPUT :
**          ier      - error number, if fail
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_cvio_trim(srf,bound,tfmat,luv,itsk,tol,irpt,nrpt,inner)
struct NCL_fixed_databag *srf;
UM_srf_boundary *bound;
UM_transf tfmat;
UU_LOGICAL luv;
UM_int2 *itsk,*irpt,*inner;
UM_real8 *tol, *nrpt;
{
	int status,i,j,insf,imin,k,counting,n,ix,jx,nuv;
	UU_REAL dtol,d,dmin,uscl,vscl;
	UM_coord uvi,nearpt,*uvs;
	UU_LOGICAL lopen;
	UM_int2 type = NCLI_POINT;

	int icv,cvn,npt,nutcv;
	UM_coord *pt,*uv;

	if (*tol <= 0.) return (UU_FAILURE);
	status = UU_SUCCESS;
	lopen = UU_TRUE;
	dtol = 0.1 * (*tol);
/*
.....If trimmed portion is a small percentage of surface
.....then scale the UV parameters
.....so the matching tolerance is valid
*/
	uscl = vscl = 1.;
	if (bound->ummx[0][1]-bound->ummx[0][0] < dtol*5 ||
		bound->vmmx[0][1]-bound->vmmx[0][0] < dtol*5)
	{
		if (bound->ummx[0][1]-bound->ummx[0][0] < dtol*5) uscl = 500.;
		if (bound->vmmx[0][1]-bound->vmmx[0][0] < dtol*5) vscl = 500.;
		bound->ummx[0][0] *= uscl; bound->ummx[0][1] *= uscl;
		bound->vmmx[0][0] *= vscl; bound->vmmx[0][1] *= vscl;
		uvs = (UM_coord *)UU_LIST_ARRAY(bound->uvpts);
		for (i=0;i<UU_LIST_LENGTH(bound->uvpts);i++)
		{
			uvs[i][0] *= uscl;
			uvs[i][1] *= vscl;
		}
	}
/*
.....Find all curves segments inside the boundary
.....Get the number of untrimmed curves in the list
*/
	nutcv = um_cvio_getnutcv();
	cvn = nutcv-1;
	ix = 0;

	for (icv = 0; icv < nutcv; icv++)
	{
/*
.....Get the pt points and uv points for each curve
*/
		um_cvio_get (icv,&npt,&pt,&uv);
		n = npt;

		counting = 0;
		for (j = 0; j < n; j++)
		{
			if (uv[j][2] == -1)
				insf = 0;
			else if (uv[j][2] == 1)
				insf = -1;
			else
			{
				uv[j][0] *= uscl; uv[j][1] *= vscl;
				insf = um_inside_bndr (uv[j],pt[j],bound,&dtol,luv);
				uv[j][0] /= uscl; uv[j][1] /= vscl;
			}

			if (counting == 0)
			{
				if (insf != -1)
				{
/*
.....Create a new Scvio list
*/
					um_cvio_create(UU_TRUE);
					cvn++;
/*
.....Put point and uv-point into the cvn-th Scvio list
*/
					um_cvio_push_at(cvn,pt[j],uv[j]);
					counting = 1;
				}
			}
			else
			{
				ix++;
/*
.....Put point and uv-point into the cvn-th Scvio list
*/
				if (insf != -1 && UM_SQDIS(pt[j-1],pt[j]) > UM_DFUZZ )
					um_cvio_push_at(cvn,pt[j],uv[j]);
/*
.....Check if end point or not
*/
				if (insf == -1 || j == n-1)
				{
					counting = 0;
					ix = 0;
				}
			}
		}
	}
/*
.....Restore boundary UV points
*/
	if (uscl != 1. || vscl != 1.)
	{
		bound->ummx[0][0] /= uscl; bound->ummx[0][1] /= uscl;
		bound->vmmx[0][0] /= vscl; bound->vmmx[0][1] /= vscl;
		uvs = (UM_coord *)UU_LIST_ARRAY(bound->uvpts);
		for (i=0;i<UU_LIST_LENGTH(bound->uvpts);i++)
		{
			uvs[i][0] /= uscl;
			uvs[i][1] /= vscl;
		}
	}

	return (status);
}

/*********************************************************************
**    E_FUNCTION   : int  um_cvio_sortmerge_segs (tol)
**       Sort and merge the intersection points, and put the sorted points
**       into the active Scvio lists.
**    PARAMETERS
**       INPUT  :
**          tol     - tolerance
**          global head points at the first segment
**       OUTPUT :
**          Scvio contain the sorted intersection data
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void um_cvio_sort_points (tol)
UU_REAL tol;
{
	UU_REAL eps = 0.01*tol*tol,d,d1,dmin;
	int found,i,n,ncvio,npts;
	ncl_ioseg *cur,*next,*before,*prev;
	UM_coord pt1,*pts;
	UU_LOGICAL lfirst;
/*
.....Delete duplicate points with distance less than tol
*/
	for (cur = head; cur != UU_NULL; cur = cur->next)
	{
		prev = cur;
		for (next = cur->next; next != UU_NULL; )
		{
			d = UM_SQDIS (cur->pt2,next->pt1);
			d1 = UM_SQDIS (cur->pt1,next->pt2);
			if (d < eps && d1 < eps)
			{
				prev->next = next->next; 
				uu_free (next);
				next = prev->next;
				break;
			}
			d = UM_SQDIS (cur->pt1,next->pt1);
			d1 = UM_SQDIS (cur->pt2,next->pt2);
			if (d < eps && d1 < eps)
			{
				prev->next = next->next;
				uu_free (next);
				next = prev->next;
				break;
			}
			prev = next; next = next->next;
		}
	}

	ncvio = 0;
/*
.....Sort points and Create new Scvio lists
*/
	for (cur = head, n = 2; cur != UU_NULL; )
	{
		before = prev = cur;
		found = 0;
		dmin = eps;
		next = cur->next;
		while (next != UU_NULL)
		{
			d = UM_SQDIS (cur->pt2,next->pt1);
			if (d < dmin)
			{
				found = 1; before = prev;
				if (d < UM_DFUZZ) break;
				dmin = d;
			}
			d = UM_SQDIS (cur->pt2,next->pt2);
			if (d < dmin)
			{
				found = 2; before = prev;
				if (d < UM_DFUZZ) break;
				dmin = d;
			}
			d = UM_SQDIS (head->pt1,next->pt2);
			if (d < dmin)
			{
				found = -1; before = prev;
				if (d < UM_DFUZZ) break;
				dmin = d;
			}
			d = UM_SQDIS (head->pt1,next->pt1);
			if (d < dmin)
			{
				found = -2; before = prev;
				if (d < UM_DFUZZ) break;
				dmin = d;
			}
			prev = next; next = next->next;
		}
		if (found != 0)
		{
			n++;
			next = before->next;
			if (found == 2 || found == -2)
			{
				for (i = 0; i < 3; i++)
				{
					d = next->pt1[i];
					next->pt1[i] = next->pt2[i];
					next->pt2[i] = d;
				}
			}
			before->next = next->next;
			if (found > 0)
			{
/*
.....Insert next after current end
*/
				next->next = cur->next;
				cur->next = next;
				cur = next;
			}
			else
			{
/*
.....Insert next before current start
*/
				next->next = head;
				head = next;
			}
		}
		else
		{
/*
.....Create a new Scvio list
*/
			if (ncvio == 0)
			{
				um_cvio_create(UU_FALSE);
				ncvio++;
				lfirst = UU_TRUE;
			}
			else
			{		
/*
.....Check if need to merge with previous untrimed curve
*/
				um_cvio_getpts(ncvio-1,&npts,&pts);
				d = UM_SQDIS (head->pt1,pts[npts-1]);
				d1 = UM_SQDIS (head->pt1,pts[0]);
				if (d > 100.0 * eps && d1 > 100. * eps)
				{
					um_cvio_create(UU_FALSE);
					ncvio++;
					lfirst = UU_TRUE;
				}
				else						
					lfirst = UU_FALSE;
			}

			if (lfirst)
			   um_cvio_push(head->pt1,UU_NULL);
			else
			{
			  if (d < d1)
				  um_cvio_push(head->pt1,UU_NULL);
			  else 			  
				  um_cvio_insert(head->pt1,UU_NULL,0);
			}
			um_vctovc (head->pt1,pt1);

			for (i = 0; i < n; i++)
			{
				if (UM_SQDIS (pt1,head->pt2) > eps || (n > 2 && i == n-1))
				{
					if (lfirst)
						um_cvio_push(head->pt2,UU_NULL);
					else
					{
						if (d < d1)
							um_cvio_push(head->pt2,UU_NULL);
						else 			  
							um_cvio_insert(head->pt2,UU_NULL,0);
					}
					um_vctovc (head->pt2,pt1);
				}
				else
				{
					i--; n--;
				}
				next = head->next;
				if (head == cur) i = n-1;
				uu_free (head);
				head = next;
			}

			if (n < 2)
				um_cvio_remove(UU_FALSE);

			cur = head;
			n = 2;
		}
	}
	while (head != UU_NULL)
	{
		next = head->next;
		uu_free (head);
		head = next;
	}
	return;
}

/*********************************************************************
**    E_FUNCTION     : um_cvio_get_uv_point (sfkey,pp,u0,v0)
**       Project point into surfaces
**    PARAMETERS
**       INPUT  :
**          sfkey   - key of the surface 
**          pp      - point to project
**          u0,v0   - initial surface parameters
**       OUTPUT :
**          u0,v0   - projected u,v
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_cvio_get_uv_point (sfkey,pp,u0,v0)
UU_KEY_ID sfkey;
UM_coord pp;
UU_REAL *u0,*v0;
{
	int i,icv,status,n1,irot;
	UM_coord ppi,uvi;
	UM_vector vvi,vcur,sni,sn0;
	UU_REAL co;
	UM_real8 asw;
	UM_real4 dir;
	UM_real4 u,v;
	UM_int4 skey;
	UM_int2 isub,itype,isf;
	int npts,nutcv;
	static UM_real4 ss[9];
	UM_coord ptj;

	status = UU_SUCCESS;
	itype = 9; /* surface */
	isub = 1;
	skey = sfkey;
	ptdsc3 (&skey,&isub,&itype,&asw);

	isf = 3;
	dir = 0;
	u = *u0; v = *v0;
	
	um_vctovc (pp,ptj);
	sfpt1 (&asw,ptj,&isf,&dir,&u,&v,ss);

	*u0 = u;
	*v0 = v;

	return status;
}

/*********************************************************************
**    E_FUNCTION     : um_cvio_project_point (sfkey,
**                          nvec,dpl,plflg,plmx,plinv,pp,u0,v0)
**       Project point into surfaces
**    PARAMETERS
**       INPUT  :
**          sfkey   - key of the surface 
**          plvec   - plane vector
**           dpl    - plane level
**          plflag  - plane flag
**          plmx    - tranformation matrix
**          plinv   - tranformation matrix
**          pp      - point to project
**          u0,v0   - initial surface parameters
**       OUTPUT :
**          pp      - projected point
**          u0,v0   - projected u,v
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_cvio_project_point (sfkey,nvec,dpl,plflg,plmx,plinv,tol,pp,u0,v0)
UU_KEY_ID sfkey;
UM_vector nvec;
UU_REAL dpl,tol;
UU_LOGICAL plflg;
UM_transf plmx,plinv;
UM_coord pp;
UU_REAL *u0,*v0;
{
	int i,icv,status,n1;
	UM_coord ppi,uvi;
	UM_vector vvi,vcur,sni,sn0;
	UU_REAL co;
	UM_real8 asw;
	UM_real4 dir;
	UM_real4 u,v;
	UM_int4 skey;
	UM_int2 isub,itype,isf;
	int npts,nutcv;

	status = UU_SUCCESS;
	itype = 9; /* surface */
	isub = 1;
	skey = sfkey;
	ptdsc3 (&skey,&isub,&itype,&asw);

	isf = 3;
	dir = 0;
	u = *u0; v = *v0;

	if (plflg)
		um_cctmtf (pp,plinv,ppi);
	else
		um_vctovc (pp,ppi);

	status = S_reproj_pt (ppi,sni,nvec,dpl,asw,isf,tol,&dir,&u,&v);
	if (status != UU_SUCCESS) return (status);

	um_vctovc (ppi,pp);
	*u0 = u;
	*v0 = v;

	return status;
}

/*********************************************************************
**    E_FUNCTION     : um_cvio_project_point_uv (sfkey,
**                          nvec,dpl,plflg,plmx,plinv,pp,u0,v0)
**       Project point into surfaces
**    PARAMETERS
**       INPUT  :
**          sfkey   - key of the surface 
**          plvec   - plane vector
**           dpl    - plane level
**          plflag  - plane flag
**          plmx    - tranformation matrix
**          plinv   - tranformation matrix
**          u,v     - uv point to  project
**       OUTPUT :
**          pp      - projected point
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_cvio_project_point_uv (srf,bound,tfmat,nvec,rot,irot,
							  dpl,plflg,plmx,plinv,tol,u0,v0,pp0)
struct NCL_fixed_databag *srf;
UM_srf_boundary *bound;
UM_transf tfmat, rot;
UM_vector nvec;
UM_int2 irot;
UU_REAL dpl,tol;
UU_LOGICAL plflg;
UM_transf plmx,plinv;
UU_REAL u0,v0;
UM_coord pp0;
{
	int status;

	UU_REAL u1,v1;
	UM_int2 type = NCLI_POINT;
	struct UM_evsrfout evsrf;

	status = uc_evsrf (UM_POINT,u0,v0,srf,tfmat,&evsrf);
	if (status != UU_SUCCESS)
		return status;

/*
.....Project insert point to dpl plane
*/
	u1 = (bound->ummx[0][0] + bound->ummx[0][1]) * 0.5; 
	v1 = (bound->vmmx[0][0] + bound->vmmx[0][1]) * 0.5;

	if (irot > 0)
		um_cctmtf(evsrf.sp,rot,pp0);
	else						
		um_vctovc (evsrf.sp,pp0);
	status = um_cvio_project_point(srf->key,nvec,
				dpl,plflg,plmx,plinv,tol,pp0,&u1,&v1);
	if (status == UU_SUCCESS)					
		to_unbs (pp0,pp0,&type);

	return status;
}

/*********************************************************************
**    E_FUNCTION     : um_cvio_project_points(sfkey,
**                          nvec,dpl,plflg,flatflg,plmx,plinv,tol,u0,v0)
**        Project intersection points into surfaces
**    PARAMETERS
**       INPUT  :
**          sfkey   - key of the surface 
**          plvec   - plane vector
**          plflg   - A rotation matrix is provided.
**          flatflg - Add more points along flat edge of curve.
**          plmx    - Rotation matrix.
**          u0,v0   - initial surface parameters
**       OUTPUT :
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_cvio_project_points (sfkey,nvec,dpl,plflg,flatflg,plmx,plinv,tol,u0,v0)
UU_KEY_ID sfkey;
UM_vector nvec;
UU_REAL dpl,tol;
UM_transf plmx,plinv;
UU_REAL *u0,*v0;
UU_LOGICAL plflg,flatflg;
{
	int i,icv,status,n1,ntim,j,inc,nn;
	UM_coord ppi,uvi,*pts,pv,svppi;
	UM_vector sni,vc1;
	UU_REAL co,dis,svdis,dis1,tu,tv,disary[5];
	UM_real8 asw;
	UM_real4 u,v,dir;
	UM_int4 skey;
	UM_int2 isub,itype,isf;
	UM_coord *pp,*uv;
	int npts,nutcv;
	char sbuf[80];

	status = UU_SUCCESS;
	itype = 9; /* surface */
	isub = 1;
	skey = sfkey;
	ptdsc3 (&skey,&isub,&itype,&asw);

	isf = 3;
	dir = 0;
	u = *u0; v = *v0;
#if 0
	if (sfkey == 242)
	{
		skey = skey;
	}
#endif

/*
.....Get the number of untrimmed curves
*/
	nutcv = um_cvio_getnutcv();

	for (icv = 0; icv < nutcv; icv++)
	{
		um_cvio_getpts(icv,&npts,&pp);
		if (flatflg)
		{
			pts = (UM_coord *)uu_malloc(sizeof(UM_coord)*npts);
			uu_move_byte(pp,pts,sizeof(UM_coord)*npts);
			pp = pts;
		}
		um_cvio_empty(icv);
		dir = 0;
		u = *u0; v = *v0;

		for (i = 0; i < npts; i++)
		{
			if (plflg)
				um_cctmtf (pp[i],plinv,ppi);
			else
				um_vctovc (pp[i],ppi);
/*
.....The projection onto an (almost) flat patch
.....is more accurate when using the previous
.....UV parameters, but the projection routine
.....does not compensate for a closed surface when
.....the edge of the surface is reached, so use
.....the old logic
.....QAR 99136, 2sidea.pp
.....Bobby - 07/03/12
*/
			if (u == 0. || u == 1. || v == 0. || v == 1.)
			{
				dir = 0;
				u = *u0; v = *v0;
			}
/*
.....Project point to surface to get the coresponding uv-point
*/
#if 0
//			if (sfkey == 2659)
			{
				sprintf(sbuf,"puv(%d)=pt/%10.6f,%10.6f",i+1,u,v);
				NclxDbgPstr(sbuf);
				sprintf(sbuf,"   ppi(%d)=pt/%10.4f,%10.4f,%10.4f",i+1,ppi[0],ppi[1],
					ppi[2]);
				NclxDbgPstr(sbuf);
			}
#endif
			status = S_reproj_pt (ppi,sni,nvec,dpl,asw,isf,tol,&dir,&u,&v);
#if 0
//			if (sfkey == 2659)
			{
//				sprintf(sbuf," ouv(%d)=pt/%10.6f,%10.6f",i+1,u,v);
//				NclxDbgPstr(sbuf);
				sprintf(sbuf,"   ppo(%d)=pt/%10.4f,%10.4f,%10.4f",i+1,ppi[0],ppi[1],
					ppi[2]);
				NclxDbgPstr(sbuf);
			}
#endif
			if (status != UU_SUCCESS)
				continue;

/*
.....Handle large flat areas
.....next to curved areas
.....By adding more points along flat
.....Bobby - 04/20/15
*/
			if (flatflg)
			{
				if (i == 0)
					um_vctovc(ppi,svppi);
				else
				{
					dis = um_dcccc(ppi,svppi);
					if (i == 1) svdis = dis;
					if (i+1 < npts)
					{
						dis1 = um_dcccc(ppi,pp[i+1]);
						if (dis1 < svdis) svdis = dis1;
					}
					if (svdis < tol*15.) svdis = tol*15;
					if (dis > 10*svdis)
					{
						ntim = (dis / (10*svdis) + .5);
						inc = 0;
						disary[inc++] = svdis;
						if (ntim > 3) disary[inc++] = dis * .25;
						disary[inc++] = dis * .5;
						if (ntim > 3) disary[inc++] = dis * .75;
						disary[inc++] = dis - svdis;
			
						um_vcmnvc(ppi,svppi,vc1); um_unitvc(vc1,vc1);
						tu = u; tv = v;

						for (j=0;j<inc;j++)
						{
							um_translate_point(svppi,disary[j],vc1,pv);
							status = S_reproj_pt(pv,sni,nvec,dpl,asw,isf,tol,&dir,
								&tu,&tv);
							if (plflg) um_cctmtf(pv,plmx,pv);
							uvi[0] = tu;
							uvi[1] = tv;
							uvi[2] = 0.0;
							um_cvio_push_at(icv,&pv,&uvi);
						}
/*						um_cvio_getpts(icv,&nn,&pp);
						i += inc;*/
					}
					svdis = dis;
					um_vctovc(ppi,svppi);
				}
			}
/*
.....Store point
*/
			if (plflg) um_cctmtf (ppi,plmx,ppi);
			uvi[0] = u;
			uvi[1] = v;
			uvi[2] = 0.0;
			um_cvio_push_at(icv,&ppi,&uvi);
		}

/*
.....Check if slices curve is closed
*/
/*		um_cvio_display(0,3,1);*/
		um_cvio_get (icv,&npts,&pp,&uv);
		if (UM_SQDIS (pp[0], pp[npts-1]) < UM_DFUZZ)
		{
/*
.....Check if start and end uv point the same
*/
			if (fabs(uv[0][0] - uv[npts-1][0]) < UM_DFUZZ)
			{	
				if (fabs(uv[0][0] - 1.0) < 1.0e-5)
					uv[0][0] = 0.0;
				else if (fabs(uv[0][0]) < 1.0e-5)
					uv[npts-1][0] = 1.0;
			}	
		}
		if (flatflg) uu_free(pts);
	}

	return status;
}

/*********************************************************************
**  FUNCTION : int um_cvio_intersection_tesses 
**                     (srf,plvec,pl,rot,tess,irot,lsolid,tol,itsk,igui,ier)
**     Intersect plane with triangles
**  PARAMETERS
**     INPUT  :
**        srf   - pointer to surface entity
**        rot   - tranfomration matrix
**        tess  - tessellation
**        irot  - transformation flag
**      lsoild  - Solid or not
**         tol  - tolerance
**        itsk  - 0 - create all individual intersections;
**                1 - create a single spline;
**        igui  - GUI-type error output, if 1
**     OUTPUT :
**          ier - error number, if fail
**  RETURNS      :
**     UU_SUCCESS iff no error; else UU_FAILURE
**  SIDE EFFECTS : none
**  WARNINGS     : none
*********************************************************************/
int um_cvio_intersection_tesses (srf,plvec,pl,rot,irot,tess,
									lsolid,tol,itsk,igui,ier)
struct NCL_fixed_databag *srf;
UM_vector plvec;
struct NCL_nclpl_rec *pl;
UM_transf rot;
UM_tessellation *tess;
UU_LOGICAL lsolid;
UU_REAL tol;
UM_int2 itsk,irot,igui,*ier;
{
	int status,i,ntri,ier1;
	UU_REAL tol1,tolsq,dpl;
	UU_LIST trilst;
	UM_trian *ptri;
	UU_REAL zmax,zmin;
	UU_REAL xmmx[2],ymmx[2];
	UM_transf plmx,plinv;
	int npts,nc,*np;
	UM_coord *pts,*uvs,*vcs;
	
	UM_f77_str_ptr str77;
	UM_init_f77_str (str77, buf, 80);
	buf[0] = '\0';

	status = UU_SUCCESS;
	zmax = -1000000.; zmin = 1000000.;
	xmmx[0] = ymmx[0] = 1000000.;
	xmmx[1] = ymmx[1] = -1000000.;

	tol1 = 0.1 * tol;
	tolsq = tol * tol;
	dpl = UM_DOT (pl->pt,pl->nvec);

/*
.....Get the traingles
*/
	uu_list_init (&trilst,sizeof (UM_trian),200,200);
	status = ncl_get_tess_triangles (tess,&trilst,2,1);

	ntri = trilst.cur_cnt;
	if (ntri < 1) status = UU_FAILURE;

	if (status != UU_SUCCESS)
	{
		Sfplio_err (srf,str77,igui,ier,163);
		goto Done;
	}

	ptri = (UM_trian *)UU_LIST_ARRAY(&trilst);
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

	if (dpl <= zmin - tol || dpl >= zmax + tol)
		goto Done;

	lfirstime = UU_FALSE;
	for (i = 0; i < ntri; i++)
		ncl_tripln_io(&ptri[i],pl,tol1,tolsq);

Done:
	uu_list_free (&trilst);

	return status;
}

/*********************************************************************
**  FUNCTION : int um_cvio_intersection_triangles (pl,trilst,tol)
**     Intersect plane with triangles
**  PARAMETERS
**     INPUT  :
**        pl	 - plane
**        trilst - tessellation
**         tol   - tolerance
**     OUTPUT :
**          none
**  RETURNS      :
**     UU_SUCCESS iff no error; else UU_FAILURE
**  SIDE EFFECTS : none
**  WARNINGS     : none
*********************************************************************/
int um_cvio_intersection_triangles (pl,trilst,tol)
struct NCL_nclpl_rec *pl;
UU_LIST *trilst;
UU_REAL tol;
{
	int status,i,ntri;
	UU_REAL tol1,tolsq,dpl;
	UM_trian *ptri;

	status = UU_SUCCESS;

	tol1 = 0.1 * tol;
	tolsq = tol * tol;
	dpl = UM_DOT (pl->pt,pl->nvec);

/*
.....Get the traingles
*/
	ntri = trilst->cur_cnt;
	if (ntri < 1)
		status = UU_FAILURE;
	ptri = (UM_trian *)UU_LIST_ARRAY(trilst);

	for (i = 0; i < ntri; i++)
		ncl_tripln_io(&ptri[i],pl,tol1,tolsq);

	return status;
}

/*********************************************************************
**    E_FUNCTION:  um_cvio_intersection_boundary(nb,npo,np,cvpts,nvec,d,tol)
**          Intersects surface boundary with a plane - in 3D
**    PARAMETERS
**       INPUT  :
**          nb     - number of boundary curves
**          npo    - number of points in outer boundary
**          np     - number of points in inner boundaries
**			cvpts  - boundary points list
**          nvec   - plane normal vector
**          d      - plane distance parameter
**          tol    - tolerance
**       OUTPUT :
**          Scvio list from the sorted intersection points
**    RETURNS      :  UU_SUCCESS / UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_cvio_intersection_boundary (nb,npo,np,cvpts,nvec,d,tol)
int nb,npo,*np;
UU_LIST *cvpts;
UU_REAL d,tol;
UM_vector nvec;
{
	int i, ix, ni, n, npt, icv, j, cvn;
	UM_coord *pts;
/*
.....sort condition routines
*/
	int cmp();

	if (npo < 4) return (-1);
	pts = (UM_coord *) UU_LIST_ARRAY (cvpts);

/*
.....Intersect plane with all closed boundaries
*/
	i = ni = 0;
	n = npo;

/*
.....Create a new Scvio list
*/
	um_cvio_create(UU_FALSE);

	ni = um_cvio_bndrpln (pts,n,nvec,d,tol,i);
	if (ni%2 == 1) return (-1);
	ix = n;

	for (i = 1; i < nb; i++)
	{
		n = abs(np[i-1]);
		ni += um_cvio_bndrpln (pts[ix],n,nvec,d,tol,i);
		if (ni%2 == 1) return (-1);
		ix += n;
	}
/*
.....Sort all points in buffer
*/
	if (ni >= 3 || (NCL_waterline == UU_FALSE && ni == 2))
	{
		icv = 0;
		um_cvio_getpts(icv,&npt,&pts);

		uu_qsort (pts,ni,sizeof(UM_coord),cmp);

		um_cvio_getpts(icv,&npt,&pts);
		n = npt;

		cvn = 0;
		for (j = 0; j < n; j++)
		{
			if (j%2 == 0)
			{
				um_cvio_create(UU_TRUE);
				cvn++;
				um_cvio_push_at(cvn,pts[j],UU_NULL);
			}
			else	
				um_cvio_push_at(cvn,pts[j],UU_NULL);
		}
	}

	return (0);
}

/*********************************************************************
**    E_FUNCTION:  um_cvio_bndrpln(pts,npt,nvec,d,tol,ib)
**       Intersects surface boundary component  with a plane - in 3D
**    PARAMETERS
**       INPUT  :
**          pts    - array of points representing closed curve
**          npt    - number of points in pts array.
**          nvec   - plane normal vector
**          d      - plane distance parameter
**          tol    - tolerance
**          ib     - outer boundary if 0; else inner
**       OUTPUT :
**          Scvio list contains the intersection points
**    RETURNS      :  total number of intersection points
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_cvio_bndrpln(pts,npt,nvec,d,tol,ib)
UM_coord pts[];
int npt,ib;
UM_vector nvec;
UU_REAL d,tol;
{
	int i,j,j0,j1,k,l,np1,num;
	UU_REAL d0,d1,del;
	UM_coord pt1,pt2;

	np1 = npt-1;
	if (np1 < 3) return (0);
	k = 0;
	num = 0;

/*
.....Find the first point not on the plane
*/
	d0 = UM_DOT (pts[0],nvec) - d;
	if (fabs (d0) <= tol)
	{
		for (j = np1-1; j > 0; j--)
		{
			d1 = UM_DOT (pts[j],nvec) - d;
			if (fabs(d1) > tol) break;
		}
		if (j < 1) return (0);
		if (ib == 0)
			k = np1 - j;
		else
			k = np1 - j + 1;
	}

	for (i = 0; i < np1; i++)
	{
		j = um_mod(i-k,np1);
		d0 = UM_DOT (pts[j],nvec) - d;
		if (fabs (d0) <= tol)
		{
/*
.....Current point is on the plane.
*/
			um_vctovc (pts[j],pt1);
			d1 = UM_DOT (pts[j+1],nvec) - d;
			if (i > 0 && fabs(d1) > tol)
			{
/*
.....If the next is not, we add the current point if the previous
.....and the next are on different sides of the plane.
*/
				j0 = um_mod(i-1-k,np1);
				del = UM_DOT (pts[j0],nvec) - d;
				if (fabs(del) > tol && del * d1 < 0.)
				{
					num++;
					um_cvio_push(pt1,UU_NULL);
				}
				continue;
			}
			while (fabs(d1) <= tol)
			{
				i++; j = um_mod(i-k,np1); d1 = UM_DOT (pts[j+1],nvec) - d;
			}
			um_vctovc (pts[j],pt2);
			del = UM_DCCCC(pt1,pt2);
			if (del > tol)
			{
				UM_vector vv;
				UM_coord ptt;
				int idel,iret;
/*
.....The next point(s) is also on the plane. We have an edge (pt1,pt2) on
.....the plane. We add an edge endpoint if the 'just beyond' extention will
.....be 'outside' the polygon
*/
				for (l = 0; l < 3; l++)
					vv[l] = (pt2[l] - pt1[l])/del;

				for (idel = 5, iret = 0; iret == 0; idel += 5)
				{
					for (l = 0; l < 3; l++)	ptt[l] = pt1[l] - idel*vv[l]*tol;
					iret = ncl_pt_in_contour (pts,npt,vv,nvec,ptt,tol);
				}
				if (iret == -1)
				{
					num++; 
					um_cvio_push(pt1,UU_NULL);
				}

				for (idel = 5, iret = 0; iret == 0; idel += 5)
				{
					for (l = 0; l < 3; l++)	ptt[l] = pt2[l] + idel*vv[l]*tol;
					iret = ncl_pt_in_contour (pts,npt,vv,nvec,ptt,tol);
				}
				if (iret == -1)
				{
					num++; 
					um_cvio_push(pt2,UU_NULL);
				}
			}
		}
		else
		{
			j1 = -1;

			j0 = j;
			d1 = UM_DOT (pts[j+1],nvec) - d;
			while (ib > 0 && i < np1 && fabs(d1) <= tol)
			{
				if (fabs(d1) < 0.1*tol)
				{
					j1 = j+1; um_vctovc (pts[j1],pt1);
				}
				i++;
				j = um_mod(i-k,np1);
				d1 = UM_DOT (pts[j+1],nvec) - d;
			}
			if (fabs (d1) > tol && d0 * d1 < 0.)
			{
				num++;
				if (ib > 0 && j1 >= 0)
				{
					UM_vector v0,v1;
					for (l = 0; l < 3; l++)
					{
						v0[l] = pts[j1][l] - pts[j0][l];
						v1[l] = pts[j1][l] - pts[j+1][l];
					}
					del = UM_DOT(v0,v1) - UM_DOT(v0,nvec)*UM_DOT(v1,nvec);
/*
.....If a vertex is exactly on the plane and forms a convex angle, use it
*/
					if (del > tol*tol)
					{
						um_cvio_push(pt1,UU_NULL); continue;
					}
				}
				del = d1 - d0;				
				um_avcplbvc (-d0/del, pts[j+1], d1/del, pts[j0], pt1);
				um_cvio_push(pt1,UU_NULL);
			}
		}
	}

	return (num);
}

/*********************************************************************
**  FUNCTION : S_cvio_DisplayBoundry(srf,bound,tfmat,icolor,ldisp)
**         Debug disply surface boundary
**  PARAMETERS
**     INPUT  :
**         srf  - pointer to surface entity
**       bound  - pointer to surface boundary
**       tfmat  - surface matrix
**      icolor  - display color
**       ldisp  - dosplay flag
**     OUTPUT :
**          none
**  RETURNS      :
**     none
**  SIDE EFFECTS : none
**  WARNINGS     : none
*********************************************************************/	
void um_cvio_displayboundry(srf,tfmat,bound,icolor,ldisp)
struct NCL_fixed_databag *srf;
UM_transf tfmat;
UM_srf_boundary *bound;
int icolor,ldisp;
{
	UM_coord *pts,*uvs;
	int ib,nb,npts,*np;
	int i;
	char tbuf[80];	

	nb = bound->nb;
	np = bound->np;

	for (ib = 0; ib < nb; ib++)
	{
		if (bound->cvpts)	
			pts = (UM_coord *) UU_LIST_ARRAY (bound->cvpts);
		npts = np[ib];
		if (npts > 0)
		{
			if (bound->cvpts)
				ncl_draw_polyline (npts,pts,icolor,ldisp);
#if 0
			sprintf(tbuf,"$$srf->key/%d",srf->key);
			NclxDbgPstr(tbuf);
			for (i=0;i<npts-1;i++)
			{
				sprintf(tbuf,"LN/%8.5f,%8.5f,%8.5f,%8.5f,%8.5f,%8.5f",
					pts[i][0],pts[i][1],pts[i][2],
					pts[i+1][0],pts[i+1][1],pts[i+1][2]);
				NclxDbgPstr(tbuf);
			}
			sprintf(tbuf,"*stop");
			NclxDbgPstr(tbuf);
#endif

#if 0
			uvs = (UM_coord *) UU_LIST_ARRAY (bound->uvpts);
			for (i = 0; i < npts-1; i++)
			{						
				sprintf(tbuf,"LN/%8.5f,%8.5f,%8.5f,%8.5f",					
					uvs[i][0],uvs[i][1],uvs[i+1][0],uvs[i+1][1]);
				NclxDbgPstr(tbuf);
			}
#endif
		}
	}
}

/*********************************************************************
**  FUNCTION : um_cvio_inside_boundary(bound,tol,luv)
**         Check if the untrimmed curve is inside boundary or not
**  PARAMETERS
**     INPUT  :
**			none
**     OUTPUT :
**          none
**  RETURNS      :
**     TRUE if open untrimmed curve, FALSE if closed untrimmed curve
**  SIDE EFFECTS : none
**  WARNINGS     : none
*********************************************************************/	
int um_cvio_inside_boundary(bound,tol,luv)
UM_srf_boundary *bound;
UU_LOGICAL luv;
UM_real8 *tol;
{
	int status;
	UU_REAL dtol;
	int insf, icv, j, npt, nutcv;
	UM_coord *pt,*uv;

	status = UU_FAILURE;
	dtol = 0.1 * (*tol);

	nutcv = um_cvio_getnutcv();

	for (icv = 0; icv < nutcv; icv++)
	{
		um_cvio_get (icv,&npt,&pt,&uv);
		for (j = 0; j < npt; j++)
		{		
			insf = um_inside_bndr (uv[j],pt[j],bound,&dtol,luv);
			if (insf != -1)
				return UU_SUCCESS;
		}
	}

	return (status);
}

/*********************************************************************
**  FUNCTION : um_cvio_sphere_srf(srf,tfmat,tol)
**         Check if the surface is non-spherical surface or not
**  PARAMETERS
**     INPUT  :
**         srf  - pointer to surface entity
**       tfmat  - surface matrix
**         tol  - tolerance
**     OUTPUT :
**          none
**  RETURNS      :
**     TRUE if non-spherical surface, FALSE if sphere surface
**  SIDE EFFECTS : none
**  WARNINGS     : none
*********************************************************************/	
UU_LOGICAL um_cvio_sphere_srf(srf,tfmat,tol)
struct NCL_fixed_databag *srf;
UM_transf tfmat;
UU_REAL tol;
{
	UU_LOGICAL luv = UU_TRUE;
	int status = UU_SUCCESS;

	UU_REAL tolsq = tol*tol;
	UM_coord pt1,pt2,pt3,pt4;

	UU_REAL dissq;
	UU_REAL ut1,ut2,vt1,vt2;

	struct UM_evsrfout *evsrf;
	evsrf = (struct UM_evsrfout *) uu_malloc(sizeof(struct UM_evsrfout));
	
	status = uc_init_evsrfout(srf, evsrf);
	if (status != UU_SUCCESS)
		return (status);

	ut1 = 0.0;
	vt1 = 0.0;
	ut2 = 1.0;
	vt2 = 1.0;

	status = uc_evsrf(UM_POINT, ut1, vt1, srf, tfmat, evsrf);
	um_vctovc (evsrf->sp, pt1);
	status = uc_evsrf(UM_POINT, ut1, vt2, srf, tfmat, evsrf);
	um_vctovc (evsrf->sp, pt2); 
	status = uc_evsrf(UM_POINT, ut2, vt1, srf, tfmat, evsrf);
	um_vctovc (evsrf->sp, pt3); 
	status = uc_evsrf(UM_POINT, ut2, vt2, srf, tfmat, evsrf);
	um_vctovc (evsrf->sp, pt4);
	uu_free(evsrf);

/*
.....Check if points within tolerance
*/
	dissq = um_sqdis(pt1,pt2);
	if (dissq < tolsq)
		luv = UU_FALSE;

	dissq = um_sqdis(pt2,pt3);
	if (dissq < tolsq)
	{
		if (!luv)
			luv = UU_TRUE;
		else	
			luv = UU_FALSE;
	}
		
	dissq = um_sqdis(pt3,pt4);
	if (dissq < tolsq)
	{
		if (!luv)
			luv = UU_TRUE;
		else	
			luv = UU_FALSE;
	}

	dissq = um_sqdis(pt4,pt1);
	if (dissq < tolsq)
	{
		if (!luv)
			luv = UU_TRUE;
		else	
			luv = UU_FALSE;
	}
	return luv;
}

/*********************************************************************
**  FUNCTION : um_cvio_srf_sortnorm(
**        Get the surface normal which is not parraell to plane vector
**  PARAMETERS
**     INPUT  :
**         srf  - pointer to surface entity
**       tfmat  - surface matrix
**       plvec  - plane vector to check
**     OUTPUT :
**       vcross - surface cross vector used to sort
**  RETURNS      :
**     TRUE if non-spherical surface, FALSE if sphere surface
**  SIDE EFFECTS : none
**  WARNINGS     : none
*********************************************************************/	
int um_cvio_srf_sortvec(srf,tfmat,plvec,vcross)
struct NCL_fixed_databag *srf;
UM_transf tfmat;
UM_vector plvec;
UM_vector vcross;
{
	int status,iu,iv;
	UU_REAL u,v,du,dv,d;
	UM_vector nvec;

	struct UM_evsrfout *evsrf;
	evsrf = (struct UM_evsrfout *) uu_malloc(sizeof(struct UM_evsrfout));

	status = UU_SUCCESS;

	status = uc_init_evsrfout(srf, evsrf);
	if (status != UU_SUCCESS)
		return (status);

	u = 0.5;
	v = 0.5;
	for (iv = 0; iv <= 2; iv++)
	{
		if (iv == 2)
			dv = -iv * 0.4999;
		else
			dv = iv * 0.4999;
		for (iu = 0; iu <= 2; iu++)
		{
			if (iu == 2)
				du = -iu * 0.4999;
			else
				du = iu * 0.4999;

			status = uc_evsrf(UM_NORM, u+du, v+dv, srf, tfmat, evsrf);
			if (status == UU_SUCCESS)
			{	
				um_vctovc (evsrf->snorm, nvec); 
				um_cross (nvec,plvec,vcross);
				um_unitvc (vcross,vcross);
				d = UM_DOT (vcross,vcross);
				if (d > 0.01 * UM_FUZZ)
					goto Done;
			}
		}
	}

Done:
	uu_free(evsrf);

	return status;
}

/*********************************************************************
**    E_FUNCTION     : um_cvio_create_intersection(sfkey,
**                           plvec,pl,rot,told,flag,ncvs,igui,ier)
**       Create surface and surface(plane) intersections
**    PARAMETERS
**       INPUT  :
**          sfkey1   - key of the surface 
**          plvec    - plane normal vector
**          pl       - plane 
**          rot      - transformation matrix (from Unibase to part coordsys)
**          flag     - 0 = Create untrimmed curve only, 1 = Create
**                     trimmed curves if any
**       OUTPUT :
**          ncvs     - Number of curves created.  If set to 1, then
**                     only the untrimmed curve is created, for
**                     example, when the two input surfaces are not
**                     trimmed.
**          ier      - error number, if fail
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_cvio_create_intersection(sfkey,plvec,pl,rot,told,flag,ncvs,igui,
	pflg,ier)
UM_int4 *sfkey,*flag,*pflg;
UM_vector plvec;
struct NCL_nclpl_rec *pl;
UM_transf rot;
UM_real8 *told;
UM_int2 *ncvs,igui,*ier;
{
static int qfirst = UU_TRUE;
	int i,ib,irot,k,npts,status,icolor,insert,inside;
	struct NCL_fixed_databag sf;
	UM_srf_boundary bndr,bound;
	UM_srf_boundary *sfbndr;
	UM_vector vec,vcross;
	UM_transf tfmat,plmx,plinv;
	UU_LOGICAL lsolid,plflg,ltrmsf,ldebug,luv;
	UU_REAL d,dpl,u,v,dtol,toler,wtol;
	UU_REAL box1[6],dx,dy,dz,dmax;
		
	UM_int2 primtyp;
	UM_real8 primdata[16];
	nclsf_prim_type typ;
	int ncv,npo,*npi;
	UM_int2 isub, mm;

	UM_tessellation tess;
	UU_LIST cvlst,uvlst,dlist,cvlst1,uvlst1;
	UM_coord *pts, *uvs;
	int istat,inner,irpt,plnr,ierpl,cvflg;
	UM_coord ptx,plnvec;
	UU_REAL plbuf[6];		

	UM_f77_str_ptr str77;
	UM_init_f77_str (str77, buf, 80);
	buf[0] = '\0';

	status = UU_SUCCESS;
	insert = UU_FAILURE;
	inside = UU_FAILURE;
/*
.....Calculate bounding box 
*/
	dmax = 1.0;
	status = ncl_geo_box(*sfkey,box1);
	dx = box1[3] - box1[0];
	dy = box1[4] - box1[1];
	dz = box1[5] - box1[2];
	dmax = MAX3(dx,dy,dz);

	isub = 264;	
	getifl(&isub,&mm);

/*
.....toler: Inch tolerance 
.....dtol:  boundary tolerance
.....wtol: current unit tolerance
*/
	toler =*told;
	dtol = toler/ dmax;
	dtol = MAX2(dtol/5.0,0.1*UM_FUZZ);
	wtol = (mm == 1) ? toler*25.4 : toler;

	ncl_set_boundary_toler (dtol);
	um_set_tess_toler (toler);
	ncl_get_rotfl (&irot);
	plflg = ncl_newzaxis_rot (plvec,plmx,plinv);
	dpl = UM_DOT (pl->pt,pl->nvec);

/*
.....Initialize surface, sf boundary and Tesselation
*/	
	um_init_boundary (&bndr);
	uu_list_init (&cvlst, sizeof(UM_coord), 500, 500);
	uu_list_init (&uvlst, sizeof(UM_coord), 500, 500);
	bndr.uvpts = &uvlst;
	bndr.cvpts = &cvlst;
		
	um_init_boundary (&bound);
	uu_list_init (&cvlst1, sizeof(UM_coord), 100, 100);
	uu_list_init (&uvlst1, sizeof(UM_coord), 100, 100);
	bound.uvpts = &uvlst1;
	bound.cvpts = &cvlst1;
	um_init_tess (&tess);
	uu_list_init0(&dlist);

/*
.....Get surface and tight tolerance surface boundary
*/
	sf.key = *sfkey;
	status = ncl_retrieve_data_fixed (&sf);
	if (status == UU_SUCCESS)
	{		
		lsolid = (sf.rel_num == UM_SOLID_REL);
		status = uc_retrieve_transf (sf.key, tfmat);
	}

	if (!lsolid && status == UU_SUCCESS)
	{
		ncl_free_bndry (&bndr);
		UU_LIST_EMPTY (bndr.uvpts);
		UU_LIST_EMPTY (bndr.cvpts);
		status = ncl_get_bndry (&sf,tfmat,&bndr,dtol,UU_FALSE);
		if (bndr.nb < 1) status = UU_FAILURE;
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
/*
.....Debug Display boundary curves
*/
#if 0
		if (sf.key == 1280 && qfirst)
			um_cvio_displayboundry(&sf,tfmat,&bndr,4,1);
#endif
	}

/*
.....Get surface boundary at machine tolerance
*/
	if (!lsolid && status == UU_SUCCESS)
	{
		ncl_free_bndry (&bound);
		UU_LIST_EMPTY (bound.uvpts);
		UU_LIST_EMPTY (bound.cvpts);
		status = ncl_get_bndry (&sf,tfmat,&bound,toler,UU_TRUE);
		if (bound.nb < 1) status = UU_FAILURE;
		if (irot > 0)
		{
			pts = (UM_coord *) UU_LIST_ARRAY (bound.cvpts);
			for (ib = 0; ib < bound.nb; ib++)
			{
				npts = bound.np[ib];
				for (i = 0; i < npts; i++)
					um_cctmtf(pts[i],rot,pts[i]);
				pts += npts;
			}
		}
	}

	if (status != UU_SUCCESS)
	{
		Sfplio_err (&sf,str77,igui,ier,163);
		goto Done;
	}

/*
.....Check if surface is planar
*/
	istat = ncl_get_sf_primdat(&sf.key,&primtyp,primdata);
	if (istat == UU_SUCCESS && (primtyp <= 1 || primtyp > 7))
		ncl_wat_check_prim (&sf,tfmat,&primtyp,primdata,toler);
	if (istat == UU_SUCCESS) typ = (nclsf_prim_type) primtyp;

	if (typ == NCLSF_PLANE)
	{
		if (irot == 1)
			ncl_transform_primdat (&typ,primdata,rot);

		for (k = 0; k < 3; k++)
			vec[k] = primdata[k];
		um_cross (vec,pl->nvec,vcross);

		d = UM_DOT (vcross,vcross);
		if (d < 0.01 * UM_FUZZ)
			istat = -1;
		else
		{
			um_unitvc (vcross,vcross);
			ncl_set_ldir (vcross);
			npo = bndr.np[0];
			npi = (bndr.nb > 1)? (bndr.np + 1): UU_NULL;
			istat = um_cvio_intersection_boundary (bndr.nb,npo,npi,
								bndr.cvpts,pl->nvec,dpl,dtol);
			if (*pflg == 1)
			{
				u = (bndr.ummx[0][0] + bndr.ummx[0][1]) * 0.5; 
				v = (bndr.vmmx[0][0] + bndr.vmmx[0][1]) * 0.5;
				um_cvio_project_points(*sfkey,plvec,dpl,plflg,UU_FALSE,plmx,plinv,
					wtol,&u,&v);
			}
/*
.....Display the intersection lines
*/
			icolor = 4;
			ldebug = UU_FALSE;
			if (ldebug)
				um_cvio_display(-1,icolor,1);
		}
	}
	else
	{
/*
.....Tesselate surface without trimmed boundary
*/
		if (lsolid)
		{
			status = ncl_get_tesslst (&sf,&tess);
			if (status != UU_SUCCESS)
				status = ncl_get_solid_tess (&sf,&dlist,&tess,toler);
		}
		else
		{
/*
.....Get surface cross vector to sort the first triangle slice
*/
		status = um_cvio_srf_sortvec(&sf,tfmat,pl->nvec,vcross);
		if (status == UU_SUCCESS)
			ncl_set_ldir (vcross);
			sfbndr = (UM_srf_boundary *) uu_malloc (sizeof(UM_srf_boundary));
			um_init_boundary (sfbndr);
			ncl_copy_sfboundary(&bndr,&sfbndr);

			status = ncl_tess_surfbase (&sf,tfmat,sfbndr,&tess,toler,
							UM_TESS_WATRLN,0,0,0,0,UU_NULL);
/*
.....Debug Display boundary curves
*/					
			ldebug = UU_FALSE;
			if (ldebug)
				um_cvio_displayboundry(&sf,tfmat,&bndr,8,1);

			um_free_boundary (sfbndr);
		}

/*
.....Intersect plane with triangles
*/
		status = um_cvio_intersection_tesses (&sf,plvec,pl,rot,irot,&tess,
			lsolid,toler,*flag,igui,ier);

/*
.....Create Scvio list from the sorted intersection points
*/
		um_cvio_sort_points (toler);
/*
.....Display the untrimmed curve before project	
*/
#if 0
		icolor = 4;
		um_cvio_display(0,icolor,1);
#endif

/*
.....Trim curves to trimmed surface boundarys
*/
		ltrmsf = (ncl_itsa_trimsrf(&sf));
		if (ltrmsf)
		{
/*
.....Project intersection points, and update the projected points,uv-points,
.....into the active Scvio list
*/
			u = (bndr.ummx[0][0] + bndr.ummx[0][1]) * 0.5; 
			v = (bndr.vmmx[0][0] + bndr.vmmx[0][1]) * 0.5;
			um_cvio_project_points(*sfkey,plvec,dpl,plflg,UU_TRUE,plmx,plinv,wtol,
				&u,&v);
#if 0
		if (sf.key == 1280 && qfirst)
		{
			int qn;
			UM_coord *qp,*qu;
			char qbuf[80];
			um_cvio_get(0,&qn,&qp,&qu);
			NclxDbgPstr("DRAFT/MODIFY,COLOR=red");
			for (i=0;i<qn-1;i++)
			{
				sprintf(qbuf,"LN/%8.5f,%8.5f,%8.5f,%8.5f",
					qu[i][0],qu[i][1],qu[i+1][0],qu[i+1][1]);
				NclxDbgPstr(qbuf);
			}
		}
#endif

/*
.....Display the untrimmed curve after project	
*/
			icolor = 6;
			ldebug = UU_FALSE;
			if (ldebug)
				um_cvio_display(0,icolor,1);

/*
.....Check if spherical surface or not
*/
			luv = UU_TRUE;
			luv = um_cvio_sphere_srf(&sf,tfmat,toler);
 
/*
.....Get the intersection of curve with surface boundary
.....Insert the intersection points
*/
			insert = um_cvio_intersection_srfbound(&sf,&bound,&bndr,tfmat,pl,
										irot,luv,&toler,&inner);
			if (insert != UU_SUCCESS)
			{
				inside = um_cvio_inside_boundary(&bndr,&toler,luv);
				if (inside != UU_SUCCESS)
					um_cvio_empty_all();
			}
/*
.....Trim curves within the boundary
*/
			irpt = 0;
			inner = 2;
			if (insert == UU_SUCCESS)
				um_cvio_trim(&sf,&bndr,tfmat,luv,flag,&toler,&irpt,&ptx,&inner);

#if 0
		if (sf.key == 1280 && qfirst)
		{
			int qn;
			UM_coord *qp,*qu;
			char qbuf[80];
			qfirst = UU_FALSE;
			um_cvio_get(0,&qn,&qp,&qu);
			NclxDbgPstr("DRAFT/MODIFY,COLOR=red");
			for (i=0;i<qn-1;i++)
			{
				sprintf(qbuf,"LN/%8.5f,%8.5f,%8.5f,%8.5f",
					qu[i][0],qu[i][1],qu[i+1][0],qu[i+1][1]);
				NclxDbgPstr(qbuf);
			}
		}
#endif
/*
.....Display the trimmed curve
*/
			icolor = 5;
			ldebug = UU_FALSE;
			if (ldebug)
				um_cvio_display(-1,icolor,1);
		}
		else
		{
/*
.....Project intersection points, and update the projected points,uv-points,
.....into the active Scvio list
*/
			u = v = .5;
			um_cvio_project_points(*sfkey,plvec,dpl,plflg,UU_TRUE,plmx,plinv,wtol,
				&u,&v);
/*
.....Display the untrimmed curve
*/
			icolor = 4;
			ldebug = UU_FALSE;
			if (ldebug)
				um_cvio_display(0,icolor,1);
		}
	}
/*
.....Check if intersection curve exist or not
*/	
	ncv = um_cvio_getncv();
	if (ncv <= 0)
	{
		if (*flag > 0)
			Sfplio_err (&sf,str77,igui,ier,502);
	}
	*ncvs = ncv;
/*
.....Free memory allocated
*/
Done:
	um_free_tess (&tess);
	ncl_free_bndry (&bndr);
	uu_list_free (&uvlst);
	uu_list_free (&cvlst);
	uu_list_free (&dlist);	
	ncl_free_bndry (&bound);
	uu_list_free (&uvlst1);
	uu_list_free (&cvlst1);

	return status;
}

/*********************************************************************
**    E_FUNCTION  : um_cvio_sfplio(sfnum,sfkey,pl,irpt,ptx,tol,lb0,ilab,GUI,ier)
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
**          igui      - GUI-type error output, iff 1
**       OUTPUT :
**          ier     - error number, if fail
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_cvio_sfplio (sfnum,sfkey,plvec,pl,rot,irpt,ptx,tolu,lb0,ilab,igui,ier)
UU_KEY_ID *sfkey;
UM_vector plvec;
struct NCL_nclpl_rec *pl;
UM_transf rot;
int sfnum,irpt;
UM_real8 *ptx;
UU_REAL tolu;
char *lb0;
UM_int2 ilab,igui,*ier;
{
	int status,nc,ncv,npts,i,idx,isf,itsk,irot,istat,created;
	int *np;
	UM_coord *pts; 
	UM_int4 key,pflg = 0;
	UM_int2 primtyp,isub, mm,ier1,ncvs;
	UM_real8 primdata[16];
	UU_REAL dval,dval0,u,v,tol;
	UU_LOGICAL plflg,lsolid;
	UU_KEY_ID keyi;
	nclsf_prim_type typ;
	struct NCL_fixed_databag sf;
	struct UM_evsrfout evsrf;
	UM_transf tfmat,plmx,plinv;
	UU_LIST cvlst,uvlst,delkys;
	UM_f77_str_ptr str77;

	UM_init_f77_str (str77, buf, 80);
	buf[0] = '\0';

	idx = 27;
	status = UU_SUCCESS;
	lsolid = UU_FALSE;
	created = 0;
		
	isub = 264;
	getifl(&isub,&mm);
	tol = (mm)? tolu*25.4: tolu;

	ncl_get_rotfl (&irot);
	plflg = ncl_newzaxis_rot (plvec,plmx,plinv);
	
	uu_list_init (&delkys, sizeof(UU_KEY_ID),0,10);

	for (isf = 0; isf < sfnum; isf++)
	{
/*
.....Initialize Scvio list
*/
		um_cvio_new();
/*
.....Intersect surface with plane
*/
		key = sfkey[isf];
		status = um_cvio_create_intersection(&key,plvec,pl,
			                     rot,&tolu,&irpt,&ncvs,igui,&pflg,ier);
/*
.....Check if intersection curve exist or not
*/
		ncv = um_cvio_getncv();
		if (ncv <= 0)
		{
			um_cvio_free();
			continue;
		}
/*
.....Put active Scvio curve list into NCL_uvintof
*/
		if (irpt == 1 || irpt == 2 || irpt == 4)
		{
			um_cvio_push_ncluv();
/*
.....Free Scvio list
*/
			um_cvio_free();
/*
.....Get curves based on irpt condition
*/
			ncl_getpts_uv (&npts,&pts);
			if (npts == 0) continue;
			nc = 1;
			ncl_get_nios (&nc,&np);

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
				sf.key = sfkey[isf];
				status = ncl_retrieve_data_fixed (&sf);
				if (status == UU_SUCCESS)
				{
					typ = NCLSF_UNKNOWN;
					lsolid = (sf.rel_num == UM_SOLID_REL);
					status = uc_retrieve_transf (sf.key, tfmat);
				}
				istat = ncl_get_sf_primdat(&sf.key,&primtyp,primdata);		
				if (istat == UU_SUCCESS) typ = (nclsf_prim_type) primtyp;

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
						Sfplio_err (&sf,str77,igui,ier,163);
						goto Done;
					}
					if (irot > 0)
						um_cctmtf (evsrf.sp,rot,ptx);
					else
						um_vctovc (evsrf.sp,ptx);

					itsk = 0;
				}
				else if (irpt == 4)
					itsk = 3;
				else if (typ == NCLSF_PLANE)
					itsk = 2;
				else
					itsk = 1;

				if (irpt == 1 || irpt == 2 || irpt == 4)
				{
					ncl_select_component1 (itsk,ptx,&npts,tol,UU_FALSE);
					nc = 1;
					ncl_getpts_uv (&npts,&pts);
				}
			}
		}
/*
.....Set tolerance for spline interploration
*/
		getsc (&idx,&dval0);
    	dval = 0.1 * dval0;
	    setscv(&idx,&dval);
/*
.....Create spline
*/
		itsk = (irpt > 0)? 1: 0;
		if (irpt != 1 && irpt != 2 && irpt != 4)
		{
			ncl_cvio_create_splcv(itsk,tolu,rmax,lb0,&ilab,ier);
			if (*ier == 0) created++;
		}
		else
		{
			for (i = 0; i < nc; i++)
			{
				if (nc > 1) npts = np[i];
				ier1 = 0;
				keyi = NULLKEY;	
				keyi = ncl_sfs_create_geo1 (npts,pts,UU_FALSE,itsk,ier);
				if (*ier != UU_SUCCESS || keyi == NULLKEY)
				{
					Sfplio_err (&sf,str77,igui,ier,163);
					goto Done;
				}
				else
				{
					created++;
					uu_list_push (&delkys,&keyi);
					ncl_sfs_update_saveid (lb0,&ilab,&isubscr);
				}
				pts += npts;
			}
		}

/*
.....Set default tolerance back
*/
		setscv(&idx,&dval0);

/*
.....Free Scvio list
*/
		if (irpt != 1 && irpt != 2)	
			um_cvio_free();
	}
Done:				
	if (created > 0 && *ier == 502) *ier = 0;
	if (created == 0 && *ier == 0) Sfplio_err (&sf,str77,igui,ier,502);
	uu_list_free (&delkys);

	return status;
}
