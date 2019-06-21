/********************************************************************* 
**    NAME         :  newatcir.c
**       CONTAINS:  Routines for calculating waterline geometry
**
**        ncl_close_contour
**        ncl_npt_arc
**        ncl_find_arcs
**        ncl_find_circles
**        ncl_find_circles0
**        ncl_find_circles1
**        ncl_free_circ
**        ncl_get_whead
**
**    COPYRIGHT 2001 (c) Numerical Control Computer Sciences Inc.
**                          All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       newatcir.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:59
*********************************************************************/
#include "uminmax.h"
#include "nclwaterln.h"
#include "nclclip.h"

static NCL_w_arc *whead = UU_NULL, *whead0 = UU_NULL;
static NCL_w_arc *wc0 = UU_NULL, *wc1 = UU_NULL;

#define MINARC 12


/*********************************************************************
*********************************************************************/
void S_free_circ (head)
NCL_w_arc **head;
{
	NCL_w_arc *circ;

	while (*head != UU_NULL)
	{
		circ = *head; *head = circ->next; UU_FREE (circ);
	}
}

/*********************************************************************
*********************************************************************/
void ncl_free_circ (itsk)
int itsk;
{
	if (itsk == 1)
	{
		S_free_circ (&whead);
		wc0 = wc1 = UU_NULL;
	}
	else
		S_free_circ (&whead0);
}

/*********************************************************************
*********************************************************************/
void ncl_get_whead (itsk,head)
int itsk;
NCL_w_arc **head;
{
	if (itsk == 1)
		*head = whead;
	else
		*head = whead0;
}

/*********************************************************************
*********************************************************************/
void ncl_set_whead (itsk,head)
int itsk;
NCL_w_arc *head;
{
	if (itsk == 1)
		whead = head;
	else
		whead0 = head;
}

/*********************************************************************
*********************************************************************/
void ncl_debug_print_whead(ptlist,itsk)
UU_LIST *ptlist;
int itsk;
{
	int i,np;
	UM_2Dcoord *pts;
	char sbuf[80];
	NCL_w_arc *head;

	if (itsk == 1)
		head = whead;
	else
		head = whead0;
	pts = (UM_2Dcoord *)UU_LIST_ARRAY(ptlist);
	np = UU_LIST_LENGTH(ptlist);
	while (head != UU_NULL)
	{
		for (i=head->j0;i<=head->j1;i++)
		{
			sprintf(sbuf,"pt/%.4f,%.4f",pts[i][0],pts[i][1]);
			NclxDbgPstr(sbuf);
		}
		sprintf(sbuf,"j0 = %d",head->j0);
		NclxDbgPstr(sbuf);
		sprintf(sbuf,"j1 = %d",head->j1);
		NclxDbgPstr(sbuf);
		sprintf(sbuf,"ci/%.4f,%.4f,%.4f",head->center[0],head->center[1],
			head->rad);
		NclxDbgPstr(sbuf);
		head = head->next;
	} (head != UU_NULL);
}

/*********************************************************************
**    E_FUNCTION     : void ncl_close_contour (pol,tol)
**       Close the contour, if not closed already
**    PARAMETERS   
**       INPUT  :
**                   pol  polygon
**                   tol  tolerance
**       OUTPUT : 
**                   pol  updated polygon
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void ncl_close_contour (pol,tol)
UU_LIST *pol;
UU_REAL tol;
{
	UU_REAL eps = 4.*tol*tol;
	int nv;
	UM_2Dcoord *vtx,vti;

	vtx = (UM_2Dcoord *) UU_LIST_ARRAY (pol);
	nv = pol->cur_cnt;
	if (UM_SQDIS_2D(vtx[0],vtx[nv-1]) > eps)
	{
		vti[0] = vtx[0][0]; vti[1] = vtx[0][1];
		uu_list_push (pol,vti);
	}
	else
	{
		vtx[nv-1][0] = vtx[0][0]; vtx[nv-1][1] = vtx[0][1];
	}
	return;
}

/***************************************************************************
**    E_FUNCTION     : UU_LOGICAL ncl_npt_arc (vtx,nv,is,np,c,rmax,tol,itsk)
**     Fit a circular arc through np points of a closed contour. If np is
**     equal to the number of contour points, we are checking if the
**     contour is a full circle.
**     The linked list of arcs is global: if itsk = 0 it starts at whead0,
**     else at whead. The list is ordered: nodes corresponding to a higher
**     contour number come later; nodes with the same contour number are 
**     ordered by the order of contour vertices.
**    PARAMETERS   
**       INPUT  :
**                   vtx  - array of points 
**                   nv   - number of points in contour
**                   is   - index of first point
**                   np   - number of points to fit
**                   c    - contour index
**                   rmax - maximal acceptable radius
**                   tol  - tolerance
**                   itsk - 0 for the list whead0 (stock contour)
**                          1 for the list whead (current polygon)
**       OUTPUT : 
**    RETURNS      : 
**                   UU_TRUE iff circle data defined; else UU_FALSE
**    SIDE EFFECTS : none
**    WARNINGS     : none
***************************************************************************/
static UU_LOGICAL ncl_npt_arc (vtx,nv,is,np,c,rmax,tol,itsk)
UM_2Dcoord *vtx;
int nv,is,np,c,itsk;
UU_REAL rmax,tol;
{
	int i0,j,i1,i2,ccw;
	UM_2Dcoord cen;
	UU_REAL rad,d,del,rr,tol1,tolsq = 100.*tol*tol;
	NCL_w_arc *newarc = UU_NULL,*cur = UU_NULL, *prev = UU_NULL;

	tol1 = 5*tol;

	if (3*np < 2*nv)
	{
		i1 = (is + np/2)%nv; i2 = (is + np-1)%nv;
	}
	else
	{
		i1 = (is + nv/3)%nv; i2 = (is + 2*nv/3)%nv;
	}

	if (um_3pt_circle (vtx[is],vtx[i1],vtx[i2],cen,&rad,&ccw,tolsq,rmax) != 0)
		return (UU_FALSE);
/*
..... if we have many points to fit, try to make another circle by a
..... different triplet, and average the two centers
*/
	if (4*np >= 3*nv)
	{
		UM_2Dcoord cen1;

		i0 = (is + np/2)%nv; i1 = (i0 + nv/3)%nv; i2 = (i0 + 2*nv/3)%nv;
		if (um_3pt_circle (vtx[i0],vtx[i1],vtx[i2],cen1,&rr,&ccw,tolsq,rmax) != 0)
			return (UU_FALSE);
		if (fabs (rr - rad) > tol1) return (UU_FALSE);
		d = UM_SQDIS_2D(cen,cen1);
		if (d > tolsq) return (UU_FALSE);
		for (j = 0; j < 2; j++) cen[j] = (cen[j] + cen1[j])/2.;
	}

	del = 12.*rad*tol;
/*
..... del is the largest segment that could be put, within tolerance,
..... on a circle of this radius
*/
	rr = rad;
	for (j = 1, i0 = is; j < np; j++, i0 = i1)
	{
		i1 =  (is + j)%nv;
		d = UM_DIST_2D(vtx[i1],cen);
		rr += d;
		d = fabs(d - rad);
		if (d > tol1) return(UU_FALSE);
		d = UM_SQDIS_2D(vtx[i1],vtx[i0]);
		if (d > del) return(UU_FALSE);
	}

	if (np == nv) ccw = 0; /* full circle flag */
/*
..... rr is the average distance from center, use it as radius
*/
	rad = rr/np;
	tol1 = 2*tol;

	for (j = 0; j < np; j++)
	{
		i1 = (is + j)%nv;
		d = UM_DIST_2D(vtx[i1],cen);
		if (fabs(d - rad) >= tol1) return (UU_FALSE);
	}
/*
..... accept only no less than 15 degrees
*/
	if (ccw != 0)
	{
		UU_REAL dang,um_angle_2d();
		UM_2Dcoord v0,v1;

		um_vcmnvc_2d (vtx[is],cen,v0); um_vcmnvc_2d (vtx[i1],cen,v1);
		dang = um_angle_2d (v0,v1);
		if (dang < 0.2618) return (UU_FALSE);
	}
	newarc = (NCL_w_arc *) uu_malloc (sizeof (NCL_w_arc));
	if (!newarc) return (UU_FALSE);
	newarc->c = c;
	if (ccw != 0)
	{
		newarc->j0 = is;
		newarc->j1 = i1;
	}
	else
	{
		newarc->j0 = is-1;
		newarc->j1 = np;
	}
	newarc->rad = rad;
	newarc->ccw = ccw;
	newarc->center[0] = cen[0]; newarc->center[1] = cen[1];
	newarc->next = UU_NULL;

	if (itsk == 0 || itsk == 2)
	{
		if (whead0 == UU_NULL) 
		{
			whead0 = newarc;
		}
		else if (itsk == 2 && is < whead0->j0)
		{
			newarc->next = whead0;
			whead0 = newarc;
		}
		else
		{
			prev = whead0;
			for (cur = whead0->next; cur != UU_NULL; cur = cur->next)
			{
				if (cur->j0 > is) break;
				prev = cur;
			}
			prev->next = newarc; newarc->next = cur;
		}
	}
	else
	{
/*
..... wc0 is the first arc for the current c, wc1 - is the last one
*/
		if (whead == UU_NULL)
		{
			whead = newarc; wc0 = wc1 = newarc;
		}
		else if (c > wc1->c)
		{
			wc1->next = newarc;
			wc0 = wc1 = newarc;
		}
		else
		{
			prev = wc0;
			for (cur = wc0->next; cur != UU_NULL; cur = cur->next)
			{
				if (cur->j0 > is) break;
				prev = cur;
			}
			prev->next = newarc; newarc->next = cur;
			if (!cur) wc1 = newarc;
		}
	}

	return (UU_TRUE);
}
	
/*********************************************************************
**    E_FUNCTION     : void ncl_find_arcs (vtx,nv,is,ie,c,rmax,tol,itsk)
**     Fit circular arcs on an array of points between is and ie.
**     The linked list of arcs is global.
**    PARAMETERS   
**       INPUT  :
**                   vtx  - array of points, NOT closed here 
**                   nv   - number of points in contour
**                   is   - index of first point
**                   ie   - index of last point
**                   c    - contour index
**                   rmax - maximum radius allowed
**                   tol  - tolerance
**                   itsk - 0 for the list whead0 (stock contour)
**                          1 for the list whead (current polygon)
**                          2 for the list whead0, the arcs are ordered
**                            from the array start.
**       OUTPUT : 
**    RETURNS      : 
**                   0 iff circle data defined; else -1
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_find_arcs (vtx,nv,is,ie,c,rmax,tol,itsk)
UM_2Dcoord *vtx;
int nv,is,ie,c,itsk;
UU_REAL rmax,tol;
{
	int i,np,js,je,minarc;
	UU_LOGICAL found = UU_FALSE;

	if (itsk == 2)
		minarc = 4;
	else
		minarc = MINARC;

/*
..... find the largest possible arc
*/
	for (np = ie-is+1; np >= minarc && !found; np--)
	{
		for (i = is; i+np-1 <= ie && !found; i++)
		{
			found = ncl_npt_arc (vtx,nv,i,np,c,rmax,tol,itsk);
			if (found)
			{
				js = i; je = i+np-1;
			}
		}
	}
	if (!found) return;
/*
..... the routine calls itself recursively to try the remaining segments
..... on the left and on the right
*/
	ncl_find_arcs (vtx,nv,is,js,c,rmax,tol,itsk);
	ncl_find_arcs (vtx,nv,je,ie,c,rmax,tol,itsk);

	return;
}

/*********************************************************************
**    E_FUNCTION     : ncl_find_circles0 (pol,box,tol,NPT)
**       Find circular arcs for a closed contour (the stock). Put the
**       result into the ordered global list whead0.
**    PARAMETERS
**       INPUT  : 
**                pol    - polygon structure
**                box    - bounding box
**                tol    - tolerance
**                NPT    - the number of points to initialize a list, if needed
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : the contour is renumbered to start with the largest arc;
**                   it is also closed
**    WARNINGS     : none
*********************************************************************/
void ncl_find_circles0 (pol,box,tol,NPT)
UU_LIST *pol;
UM_2box *box;
UU_REAL tol;
int NPT;
{
	int i,nv,np,c,itsk;
	UM_2Dcoord *vtx;
	UU_LOGICAL found = UU_FALSE;
	UU_REAL rmax;

	if (ncl_is_watrev ()) return;

	vtx = (UM_2Dcoord *) UU_LIST_ARRAY (pol);
	nv = pol->cur_cnt;
	if (nv < MINARC) return;
	rmax = MAX2((box->xmax - box->xmin),(box->ymax - box->ymin));
	rmax = 4*rmax*rmax;
	c = itsk = 0;
/*
..... try to fit full circle
*/
	if (ncl_npt_arc (vtx,nv,0,nv,c,rmax,tol,itsk))
	{
		ncl_close_contour (pol,tol);
		return;
	}
/*
..... try to fit the largest possible arc
*/
	for (np = nv-1; np >= MINARC && !found; np--)
	{
		for (i = 0; i < nv && !found; i++)
			found = ncl_npt_arc (vtx,nv,i,np,c,rmax,tol,itsk);
	}
	if (!found)
	{
		ncl_close_contour (pol,tol);
		return;
	}
/*
..... renumber contour, if necessary, so that it starts with the largest arc
*/
	if (whead0->j0 > 0)
	{
		UU_LIST *ptlst;
		UM_2Dcoord *pp;
		int j,k;

		ncl_reset_aux_ptlist (&ptlst,NPT);
		if (ptlst->data == UU_NULL) return;

		uu_list_push_multiple (ptlst,nv,vtx);
		pp = (UM_2Dcoord *) UU_LIST_ARRAY(ptlst);

		i = whead0->j0;
		for (j = 0; j < nv; j++)
		{
			k = (j + i)%nv;
			vtx[j][0] = pp[k][0]; vtx[j][1] = pp[k][1];
		}
		whead0->j0 = 0;
		j = whead0->j1 - i; if (j < 0) j += nv;
		whead0->j1 = j;
	}
	ncl_close_contour (pol,tol);
	vtx = (UM_2Dcoord *) UU_LIST_ARRAY (pol);
	nv = pol->cur_cnt;

	i = whead0->j1;
	ncl_find_arcs (vtx,nv,i,nv-1,c,rmax,tol,itsk);

	return;
}

/*********************************************************************
**    E_FUNCTION     : ncl_find_circles (pol,tol,NPT,c0)
**       In a list of closed polylines find circular arcs. Put the
**       result into the ordered global list whead. Insert the arcs for
**       the contour stock from the list whead0, if needed.
**    PARAMETERS
**       INPUT  : 
**                pol    - polygon structure
**                tol    - tolerance
**                NPT    - the number of points to initialize a list, if needed
**                c0     - the contour number for the stock
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : the contours are renumbered to start with the largest arc.
**    WARNINGS     : none
*********************************************************************/
void ncl_find_circles (pol,tol,NPT,c0)
ncl_polygon *pol;
UU_REAL tol;
int NPT,c0;
{
	int i,nv,nc,c,itsk,np;
	UM_2Dcoord *vtx;
	UU_LOGICAL found;
	UU_REAL rmax;

	if (ncl_is_watrev ()) return;

	nc = pol->num_contours;
	itsk = 1;
	if (nc <= 0) return;
	vtx = (UM_2Dcoord *) UU_LIST_ARRAY (pol->contour);

	for (c = 0, nv = 0; c < nc; c++, vtx += nv)
	{
		nv = abs(pol->np[c]);
		if (nv < MINARC) continue;
		rmax = MAX2((pol->box[c].xmax - pol->box[c].xmin),
				(pol->box[c].ymax - pol->box[c].ymin));
		rmax = 4*rmax*rmax;
/*
..... try to fit full circle
*/
		if (ncl_npt_arc (vtx,nv,0,nv,c,rmax,tol,itsk)) continue;
/*
..... try to fit the largest possible arc
*/
		found = UU_FALSE;
		for (np = nv-1; np >= MINARC && !found; np--)
		{
			for (i = 0; i < nv && !found; i++)
				found = ncl_npt_arc (vtx,nv,i,np,c,rmax,tol,itsk);
		}

		if (!found) continue;

/*
..... renumber contour, if necessary, so that it starts with the largest arc
*/
		if (wc0->j0 > 0)
		{
			UU_LIST *ptlst;
			UM_2Dcoord *pp;
			int j,k;

			ncl_reset_aux_ptlist (&ptlst,NPT);
			if (ptlst->data == UU_NULL) return;

			uu_list_push_multiple (ptlst,nv,vtx);
			pp = (UM_2Dcoord *) UU_LIST_ARRAY(ptlst);

			i = wc0->j0;
			for (j = 0; j < nv; j++)
			{
				k = (j + i)%nv;
				vtx[j][0] = pp[k][0]; vtx[j][1] = pp[k][1];
			}
			wc0->j0 = 0;
			j = wc0->j1 - i; if (j < 0) j += nv;
			wc0->j1 = j;
		}
/*
..... try to fit more arcs, if there is room
*/
		i = wc0->j1;
		ncl_find_arcs (vtx,nv,i,nv-1,c,rmax,tol,itsk);
	}

	if (c0 >= 0 && whead0)
	{
		NCL_w_arc *newarc = UU_NULL,*cur = UU_NULL,*prev = UU_NULL;
/*
..... create a copy of stock arcs
*/
		for (cur = whead0; cur != UU_NULL; cur = cur->next)
		{
			newarc = (NCL_w_arc *) uu_malloc (sizeof (NCL_w_arc));
			if (!newarc) return;
			newarc->c = c0;
			newarc->j0 = cur->j0; newarc->j1 = cur->j1;
			newarc->rad = cur->rad; newarc->ccw = cur->ccw;
			newarc->center[0] = cur->center[0];
			newarc->center[1] = cur->center[1];
			newarc->next = UU_NULL;
			if (prev)
				prev->next = newarc;
			else
				wc0 = newarc;
			prev = newarc;
		}
		wc1 = newarc;
/*
..... insert stock arcs into the current arc list
*/
		if (!whead)
			whead = wc0;
		else if (whead->c > c0)
		{
			wc1->next = whead; whead = wc0;
		}
		else
		{
			prev = whead;
			for (cur = whead->next; cur != UU_NULL; prev = cur, cur = cur->next)
			{
				if (cur->c > c0) break;
			}
			prev->next = wc0; wc1->next = cur;
		}
	}

	if (whead) ncl_set_wcirc (whead);

	return;
}

/*********************************************************************
**    E_FUNCTION     : ncl_find_circles1 (pol,ccur,nv0,tol,NPT,c0)
**       In a list of closed polylines find circular arcs. Put the
**       result into the ordered global list whead. Insert the arcs for
**       the contour stock from the list whead0, if needed.
**    PARAMETERS
**       INPUT  : 
**                pol    - polygon structure
**                tol    - tolerance
**                NPT    - the number of points to initialize a list, if needed
**                c0     - the contour number for the stock
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : the contours are renumbered to start with the largest arc.
**    WARNINGS     : none
*********************************************************************/
void ncl_find_circles1 (pol,ccur,nv0,tol,NPT,c0)
ncl_polygon *pol;
UU_REAL tol;
int NPT,ccur,nv0,c0;
{
	int i,nv,nc,c,itsk,np;
	UM_2Dcoord *vtx;
	UU_LOGICAL found;
	UU_REAL rmax;

	if (ncl_is_watrev ()) return;

	if (ccur == c0 && whead0)
	{
		NCL_w_arc *newarc = UU_NULL,*cur = UU_NULL,*prev = UU_NULL;
/*
..... create a copy of stock arcs
*/
		for (cur = whead0; cur != UU_NULL; cur = cur->next)
		{
			newarc = (NCL_w_arc *) uu_malloc (sizeof (NCL_w_arc));
			if (!newarc) return;
			newarc->c = c0;
			newarc->j0 = cur->j0; newarc->j1 = cur->j1;
			newarc->rad = cur->rad; newarc->ccw = cur->ccw;
			newarc->center[0] = cur->center[0];
			newarc->center[1] = cur->center[1];
			newarc->next = UU_NULL;
			if (prev)
				prev->next = newarc;
			else
				whead = newarc;
			prev = newarc;
		}
		wc1 = newarc;
	}

	nc = pol->num_contours;
	itsk = 1;
	if (nc <= 0) return;

	vtx = (UM_2Dcoord *) UU_LIST_ARRAY (pol->contour);
	vtx += nv0;

	for (c = ccur; c < nc; c++, vtx += nv)
	{
		nv = pol->np[c];
		if (c > ccur && np >= 0) break;
		if (nv < 0) nv = -nv;
		if (nv < MINARC || c == c0) continue;
		rmax = MAX2((pol->box[c].xmax - pol->box[c].xmin),
				(pol->box[c].ymax - pol->box[c].ymin));
		rmax = 4*rmax*rmax;
/*
..... try to fit full circle
*/
		if (ncl_npt_arc (vtx,nv,0,nv,c,rmax,tol,itsk)) continue;
/*
..... try to fit the largest possible arc
*/
		found = UU_FALSE;
		for (np = nv-1; np >= MINARC && !found; np--)
		{
			for (i = 0; i < nv && !found; i++)
				found = ncl_npt_arc (vtx,nv,i,np,c,rmax,tol,itsk);
		}

		if (!found) continue;

/*
..... renumber contour, if necessary, so that it starts with the largest arc
*/
		if (wc0->j0 > 0)
		{
			UU_LIST *ptlst;
			UM_2Dcoord *pp;
			int j,k;

			ncl_reset_aux_ptlist (&ptlst,NPT);
			if (ptlst->data == UU_NULL) return;

			uu_list_push_multiple (ptlst,nv,vtx);
			pp = (UM_2Dcoord *) UU_LIST_ARRAY(ptlst);

			i = wc0->j0;
			for (j = 0; j < nv; j++)
			{
				k = (j + i)%nv;
				vtx[j][0] = pp[k][0]; vtx[j][1] = pp[k][1];
			}
			wc0->j0 = 0;
			j = wc0->j1 - i; if (j < 0) j += nv;
			wc0->j1 = j;
		}
/*
..... try to fit more arcs, if there is room
*/
		i = wc0->j1;
		ncl_find_arcs (vtx,nv,i,nv-1,c,rmax,tol,itsk);
	}

	if (whead) ncl_set_wcirc (whead);

	return;
}
