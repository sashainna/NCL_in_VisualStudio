/*********************************************************************
**    NAME         :  newatcon.c
**       CONTAINS:  Routines for calculating waterline geometry
**
**        ncl_zig_cvproj
**        ncl_zigzag
**        ncl_set_insides
**        ncl_waterline_connect
**        ncl_connect_error
**
**    COPYRIGHT 2001 (c) Numerical Control Computer Sciences Inc.
**                                           All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       newatcon.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:59
*********************************************************************/
#include "nclwaterln.h"
#include "uminmax.h"

static UU_LIST Scur,Sloop;

/*********************************************************************
*********************************************************************/
void ncl_init_connect_lst (num)
int num;
{
	if (num > 0)
	{
		uu_list_init (&Scur,sizeof(int),num,num);
		uu_list_init (&Sloop,sizeof(int),num,num);
	}
	else
	{
		uu_list_init0 (&Scur);
		uu_list_init0 (&Sloop);
	}
}

/*********************************************************************
*********************************************************************/
void ncl_free_connect_lst()
{
	uu_list_free (&Scur);
	uu_list_free (&Sloop);
}

/*********************************************************************
**    E_FUNCTION     : ncl_zig_cvproj (pto,pts,npts,pti)
**       Project a point onto a polyline - in 2D
**    PARAMETERS
**       INPUT  :
**              pto  - point to project
**              pts  - polyline points
**              npts - number of polyline points
**              dis0 - control distance: the point cannot be farther from
**                     the polyline
**              tol  - tolerance
**       OUTPUT :
**              pti  - projection point
**    RETURNS      :
**              index of the polyline segment that contains the projection,
**              -1 if could not find a good projection
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int ncl_zig_cvproj (pto,pts,npts,pti,dis0,tol)
UM_coord *pts,pto,pti;
int npts;
UU_REAL dis0,tol;
{
	int nv,k,l,iret;
	UU_REAL d;
	UM_vector vc;

	nv = abs(npts);

	for (k = 1; k < nv; k++)
	{
		l = (npts < 0)? nv-k: k;
		um_vcmnvc(pts[l],pts[l-1],vc);
		d = um_mag_2d(vc);
		if (d < tol) continue;
		vc[0] /= d; vc[1] /= d; vc[2] = 0;
		iret = um_nptsg1 (pto,pts[l-1],vc,d,pti,&d);
		if (iret == 1)
			return (k-1);
		else if ((iret == 2 && npts < 0) || (iret == 0 && npts > 0) || d > dis0)
			return (-1);
	}

	return (-1);
}

/*********************************************************************
**    E_FUNCTION     : ncl_check_direction (loop,i1,npts,nj,ptsio,plist0,overlap)
**       Fix a zigzag connection between two polylines.
**    PARAMETERS
**       INPUT  :
**       OUTPUT :
**    RETURNS      :
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void ncl_check_direction (loop,i1,npts,nj,ptsio,overlap,tolsq)
UU_LIST *loop,*ptsio,*overlap;
int i1,*npts,*nj;
UU_REAL tolsq;
{
	int i,n,nj0,nj1;
	int *nl;
	UM_coord *pts,*pt0,*pt1;
	UM_vector v0,v1;
	UU_REAL co,d0,d1;
	UU_REAL tol2 = 2500.*tolsq;

	pts = (UM_coord *) UU_LIST_ARRAY (ptsio);
	n = loop->cur_cnt;
	nl = (int *) UU_LIST_ARRAY (loop);
	i = nl[n - 1];
	n = npts[i];
	nj0 = nj[i]; pt0 = pts + nj0;
	nj1 = nj[i1]; pt1 = pts + nj1;
/*
..... check if two 'connected' pieces go in opposite directions
*/
	um_vcmnvc (pt0[n-1],pt0[n-2],v0);
	um_vcmnvc (pt1[1],pt1[0],v1);
	co = UM_DOT (v0,v1);
	if (co >= -0.9*tol2) return;

	d0 = UM_DOT (v0,v0);
	if (d0 < tol2) return;
	d1 = UM_DOT (v1,v1);
	if (d1 < tol2) return;

	if (d0*d1 - co*co >= tolsq*d0) return;
/*
..... if the pieces are also on the same line, add the beginning indices to
..... the list to give a warning
*/
	uu_list_push (overlap,&nj0);
	uu_list_push (overlap,&nj1);

	return;
}

/*********************************************************************
**    E_FUNCTION     : ncl_zigzag (loop,i1,npts,nj,ptsio,tol)
**       Fix a zigzag connection between two polylines.
**    PARAMETERS
**       INPUT  :
**          loop    - indices of connected pieces
**          ptsio   - all points
**          npts    - number of points in each piece
**          nj      - pointers to where each piece starts in ptsio
**          i1      - the index of the next segment
**          tol     - tolerance
**       OUTPUT :
**              ptsio  - fixed connection point
**    RETURNS      :
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_zigzag (loop,i1,npts,nj,ptsio,tol)
UU_LIST *loop,*ptsio;
int i1,*npts,*nj;
UU_REAL tol;
{
	int i0,n0,n1,nlinks,j0,j1,k;
	int *link;
	UM_coord *pts,*pt0,*pt1;
	UM_vector vp,vc,vn;
	UU_REAL co0,co1,dis;
	UM_coord p0,p1;

	pts = (UM_coord *) UU_LIST_ARRAY (ptsio);
	nlinks = loop->cur_cnt;
	link = (int *) UU_LIST_ARRAY (loop);
	i0 = link[nlinks - 1];

	pt0 = pts + nj[i0]; pt1 = pts + nj[i1];
	n0 = npts[i0]; n1 = npts[i1];
/*
..... check if two 'connected' pieces overlap, i.e., if the connection
..... would create a zigzag
*/
	um_vcmnvc (pt0[n0-1],pt0[n0-2],vp);
	um_unitvc (vp,vp);

	dis = um_sqdis(pt1[0],pt0[n0-1]);

	um_vcmnvc (pt1[0],pt0[n0-1],vc);
	um_unitvc (vc,vc);

	co0 = UM_DOT (vp,vc);
	if (co0 + UM_COS30 > 0) return;

	um_vcmnvc (pt1[1],pt1[0],vn);
	um_unitvc (vn,vn);
	co1 = UM_DOT (vc,vn);
	if (co1 + UM_COS30 > 0) return;
/*
..... project the connecting end point of each curve onto the other curve
*/
	j0 = ncl_zig_cvproj (pt1[0],pt0,-n0,p0,dis,tol);
	if (j0 >= 0) j1 = ncl_zig_cvproj (pt0[n0-1],pt1,n1,p1,dis,tol);
	if (j0 < 0 || j1 < 0) return;
/*
..... cut off extra pieces on each curve, thus removing a zigzag
..... connection
*/
	for (k = 0; k <= j0; k++) um_vctovc(p0,pt0[n0-1-k]);
	for (k = 0; k <= j1; k++) um_vctovc(p1,pt1[k]);

	return;
}

/*********************************************************************
**    I_FUNCTION     : void S_polyline_on_base (wbase,wlev,nnp,pts,color,ldisp,
**                                                                      cptslt)
**
**       Evaluate a UV polyline on the base surface and create display segments
**    PARAMETERS
**       INPUT  :
**          wbase     - base surface data
**          wlev      - current level
**          pts       - points
**          nnp       - number of points
**          color
**          ldisp     - "display now" flag
**          cptlst    - initialized list to use
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_polyline_on_base (wbase,wlev,nnp,pts,color,ldisp,cptlst)
NCL_w_base *wbase;
UU_REAL wlev;
int nnp,color,ldisp;
UM_coord *pts;
UU_LIST *cptlst;
{
	int k;
	UM_coord spt;
	UM_coord *cpts;

	UU_LIST_EMPTY (cptlst);
	for (k = 0; k < nnp; k++)
	{
		ncl_wat_evsrf (wbase,pts[k],wlev,spt);
		uu_list_push(cptlst,&spt);
	}
	cpts = (UM_coord *) UU_LIST_ARRAY (cptlst);
	ncl_draw_polyline (nnp,cpts,color,ldisp);
}

/*********************************************************************
**    I_FUNCTION     : void ncl_disp_connect_error (lwatrev,wbase,wlev,curves,
**                                                      pts,npts,nj,in1)
**
**       Create display segments that show the results of a failed attempt to
**       connect the current level intersections
**    PARAMETERS
**       INPUT  :
**          lwatrev   - evaluate on the base surface iff true
**          wbase     - base surface data
**          wlev      - current level
**          pts       - points
**          npts      - number of points in each piece
**          curves    - list of closed curves
**          nj        - pointers to where each piece starts in ptsio
**          in1       - the nearest 'loose' segment
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void ncl_disp_connect_error (lwatrev,wbase,wlev,curves,pts,npts,nj,in1)
UU_LOGICAL lwatrev;
NCL_w_base *wbase;
UU_REAL wlev;
UU_LIST *curves;
int *npts,*nj,in1;
UM_coord *pts;
{
	UU_LIST cptlst;
	UM_coord spt,*cpts;
	int in,i,k,nlinks,icolor,np,n0,nnp;
	int *looplink,*ns;
	NCL_w_geo *cvs;

	ncl_init_view_box();

	nlinks = Sloop.cur_cnt;
	looplink = (int *) UU_LIST_ARRAY (&Sloop);

	for (k = np = 0; k < nlinks; k++)
	{
		in = looplink[k];
		np += (npts[in]-1);
	}

	nnp = MAX2(np,100);
	uu_list_init (&cptlst,sizeof(UM_coord),nnp,nnp);

	np++;
	if (np >= 3)
	{
/*
..... draw the connected part
*/
		for (i = 0; i < nlinks; i++)
		{
			in = looplink[i];
			nnp = npts[in];
			n0 = nj[in];
			for (k = (i > 0); k < nnp; k++)
			{
				if (lwatrev)
					ncl_wat_evsrf (wbase,pts[n0+k],wlev,spt);
				else
					um_vctovc (pts[n0+k],spt);

				uu_list_push(&cptlst,&spt);
			}
		}
		cpts = (UM_coord *) UU_LIST_ARRAY (&cptlst);

		icolor = 12;
		ncl_draw_polyline (np,cpts,icolor,1);
	}
/*
..... draw the nearest 'loose' segment - in red
*/
	if (in1 >= 0)
	{
		icolor = 3;
		nnp = npts[in1]; n0 = nj[in1];
		if (lwatrev)
			S_polyline_on_base (wbase,wlev,nnp,&pts[n0],icolor,1,&cptlst);
		else
			ncl_draw_polyline (nnp,&pts[n0],icolor,1);
	}
/*
..... draw the rest of 'loose' segments
*/
	ns = (int *) UU_LIST_ARRAY (&Scur);
	icolor = 14;
	for (i = 0; i < Scur.cur_cnt; i++)
	{
		in = ns[i];
		if (ns[i] == in1) continue;
		nnp = npts[in]; n0 = nj[in];
		if (lwatrev)
			S_polyline_on_base (wbase,wlev,nnp,&pts[n0],icolor,0,&cptlst);
		else
			ncl_draw_polyline (nnp,&pts[n0],icolor,0);
	}
/*
..... draw the complete contours - in green
*/
	if (curves->cur_cnt > 0)
	{
		icolor = 4;
		cvs = (NCL_w_geo *) UU_LIST_ARRAY (curves);
		for (i = 0; i < curves->cur_cnt; i++)
		{
			if (lwatrev)
				S_polyline_on_base (wbase,wlev,cvs[i].np,cvs[i].pt,icolor,0,&cptlst);
			else
				ncl_draw_polyline (cvs[i].np,cvs[i].pt,icolor,0);
		}
	}

	uu_list_free (&cptlst);
}

/*********************************************************************
**    E_FUNCTION     : ncl_set_insides (curves)
**       For a list of closed curves (represented by polylines) determine
**       which one is inside which. In case of depth > 2 the inside
**       parameter points at a nearest curve. For example, if cv0 is
**       inside cv2 and cv4, and cv4 is inside cv2, then we set:
**       cv0 is inside cv4.
**    PARAMETERS
**       INPUT  :
**                curves   - list of closed curves
**       OUTPUT :
**                curves   - updated list with the inside parameters set
**
**    RETURNS      : none
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_set_insides (curves)
UU_LIST *curves;
{
	int i,j,nloops;
	NCL_w_geo *curve,*loopi,*loopj;
	UU_REAL dx0,dx1,dy0,dy1,toluv,tolsq;
	int **ins,*INS;
	UU_REAL um_get_rltv_tol();
	int status = UU_SUCCESS;
	static UM_vector Uaxis = {1.,0.,0.};
    static UM_vector Vaxis = {0.,1.,0.};

	nloops = curves->cur_cnt;
	if (nloops < 2) return (0);
	curve = (NCL_w_geo *) UU_LIST_ARRAY (curves);

	ins = (int **) uu_malloc (nloops*sizeof(int *));
	INS = (int *) uu_malloc (nloops*nloops*sizeof(int));
	if (!INS || !ins) { status = UU_FAILURE; goto Done; }
	for (i = 0; i < nloops; i++)
		ins[i] = &INS[i*nloops];
/*
..... fill out the ins matrix: if loop_j is inside loop_i ins_ij=1, if
..... loop_i is inside loop_j ins_ij=-1, if neither ins_ij=0.
*/
	for (i = 0; i < nloops-1; i++)
	{
		loopi = curve+i;
		toluv = um_get_rltv_tol(loopi->xrange,loopi->yrange)*7.0;
		tolsq = toluv * toluv;
		for (j = i+1; j < nloops; j++)
		{
			loopj = curve+j;
			dx0 = loopj->xrange[0]-loopi->xrange[0];
			dx1 = loopj->xrange[1]-loopi->xrange[1];
			dy0 = loopj->yrange[0]-loopi->yrange[0];
			dy1 = loopj->yrange[1]-loopi->yrange[1];
			if (dx0 > -toluv && dx1 < toluv && dy0 > -toluv && dy1 < toluv &&
				(um_pt_in_contour(loopi->pt,loopi->np,Uaxis,Vaxis,
					loopj->pt[0],toluv,tolsq) >= 0))
			{
					ins[i][j] = 1; ins[j][i] = -1;
			}
			else if (dx1 > -toluv && dx0 < toluv && dy1 > -toluv && dy0 < toluv &&
				(um_pt_in_contour(loopj->pt,loopj->np,Uaxis,Vaxis,
					loopi->pt[0],toluv,tolsq) >= 0))
			{
					ins[j][i] = 1; ins[i][j] = -1;
			}
			else
				ins[i][j] = ins[j][i] = 0;
		}
	}
/*
..... for each loop the inside is initially =-1; we set it as follows:
..... loop_i.inside = j if loop_i is inside loop_j and there is no loop
..... in between
..... we set loop_i.depth to be the number of loops from outside to loop_i
*/
	for (i = 0; i < nloops; i++)
	{
		loopi = curve+i;
		for (j = 0; j < nloops; j++)
		{
			if (j == i) continue;
			if (ins[i][j] == -1) loopi->depth++;
			if (ins[j][i] != 1) continue;
			if (loopi->inside == -1)
				loopi->inside = j;
			else
			{
				if (ins[loopi->inside][j] == 1) loopi->inside = j;
			}
		}
	}

Done:;
	UU_FREE (INS);
	UU_FREE (ins);

	return (status);
}

/*********************************************************************
**    I_FUNCTION     : S_connect_sqdis (lwatrev,wbase,wlev,pt1,pt2)
**
**       Calculate the squared 3D distance between points
**    PARAMETERS
**       INPUT  :
**          lwatrev   - evaluate on the base surface iff true
**          wbase     - base surface data
**          wlev      - current level
**          pt1,pt2   - points
**       OUTPUT : none
**    RETURNS      : squared distance
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UU_REAL S_connect_sqdis (lwatrev,wbase,wlev,pt1,pt2)
UU_LOGICAL lwatrev;
NCL_w_base *wbase;
UU_REAL wlev;
UM_coord pt1,pt2;
{
	UU_REAL d;

	if (lwatrev)
	{
		ncl_wat_sqdis (wbase,pt1,pt2,wlev,&d);
	}
	else
		d = um_sqdis(pt1,pt2);

	return (d);
}

/*********************************************************************
**    E_FUNCTION     : ncl_waterline_connect (wbase,wlev,nio,nj,ptsio,cgap,
**						   mxtol2,curves,overlap,llast)
**
**  For a collection of curves, connect the pieces into closed loops.
**    PARAMETERS
**       INPUT  :
**          wbase      - base surface data
**          wlev       - current level
**          nio        - list of lengths of pieces
**          ptsio      - list of points representing the curves
**          nj         - pointers to where each piece starts in ptsio
**          mxtol2     - squared maximum gap tolerance
**          llast      - last attempt to connect, output partial results if
**                       still fails
**       OUTPUT :
**          curves     - list of loops
**          cgap       - the gap struct
**    RETURNS      :
**         UU_SUCCESS if no error, else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_waterline_connect (wbase,wlev,nio,nj,ptsio,cgap,mxtol2,curves,overlap,
						   poctype,llast)
NCL_w_base *wbase;
NCL_w_gap *cgap;
UU_LIST *nio,*ptsio,*curves,*overlap;
int *nj,poctype;
UU_REAL wlev,mxtol2;
UU_LOGICAL llast;
{
	int in,i,j,status,*npts,nc,np,*ns,j0;
	NCL_w_geo wgeo;
	UM_coord *pts;
	UU_REAL dis,x,y,told,dis0;
	UM_coord fpt,lpt,fpt1,lpt1;
	int nnp,k,nlinks,*looplink,ip,n0,in0,in1;
	UU_LOGICAL closed,closed0,found,zig;
	UU_LOGICAL lwatrev;
	UU_REAL tol,tolsq;

	status = UU_SUCCESS;
	cgap->lnj = cgap->cnj = 0;

	lwatrev = ncl_is_watrev();

	nc = nio->cur_cnt;
	if (nc < 1) return (status);

	npts = (int *) UU_LIST_ARRAY (nio);
	pts = (UM_coord *) UU_LIST_ARRAY (ptsio);

	wgeo.inside = -1; wgeo.depth = 0;
	wgeo.xrange[0] = 1000000.; wgeo.xrange[1] = -1000000.;
	wgeo.yrange[0] = 1000000.; wgeo.yrange[1] = -1000000.;
	wgeo.np = 0;
	wgeo.pt = UU_NULL;

	Scur.cur_cnt = Sloop.cur_cnt = 0;
	for (j = 0; j < nc; j++)
		uu_list_push(&Scur,&j);

	closed = UU_TRUE;
	in0 = 0; in1 = -1;
	j = j0 = 0;
	ncl_get_wtols (&tol,&tolsq);
	told = 2.25*tol*tol;
	dis0 = 1.e12;
	zig = UU_FALSE;
	ns = (int *) UU_LIST_ARRAY (&Scur);
	while (Scur.cur_cnt > 0 && j < Scur.cur_cnt)
	{
		in = ns[j];
		if (closed)
		{
			Sloop.cur_cnt = 0;
			uu_list_push(&Sloop,&in);
			uu_list_delete (&Scur,0,1); ns = (int *) UU_LIST_ARRAY (&Scur);
			nnp = npts[in];

			in0 = in; in1 = -1;
			um_vctovc (pts[nj[in]],fpt);
			um_vctovc (pts[nj[in]+nnp-1],lpt);
			dis = S_connect_sqdis (lwatrev,wbase,wlev,fpt,lpt);
			closed = (dis < told);
			if (!closed)
			{
				dis0 = dis; um_vctovc (fpt,cgap->cpt);
				closed0 = UU_TRUE;
			}
		}
		if (!closed)
		{
			if (Scur.cur_cnt < 1)
			{
				if (dis0 < mxtol2 && Sloop.cur_cnt == 1) closed = UU_TRUE;
				goto CLSD;
			}
			in = ns[j];
			nnp = npts[in];
			um_vctovc (pts[nj[in]],fpt1);
			um_vctovc (pts[nj[in]+nnp-1],lpt1);
			dis = S_connect_sqdis (lwatrev,wbase,wlev,lpt,fpt1);
			found = (dis < told);
			if (!found)
			{
				if (dis < dis0)
				{
					closed0 = UU_FALSE;
					um_vctovc (fpt1,cgap->cpt);
					dis0 = dis; j0 = j; in1 = in;
				}
				dis = S_connect_sqdis (lwatrev,wbase,wlev,lpt,lpt1);
				found = (dis < told);
				if (found)
				{
					ncl_revers1_list (nnp,nj[in],pts,1);
					x = lpt1[0]; lpt1[0] = fpt1[0]; fpt1[0] = x;
					y = lpt1[1]; lpt1[1] = fpt1[1]; fpt1[1] = y;
				}
				else if (dis < dis0)
				{
					closed0 = UU_FALSE;
					um_vctovc (lpt1,cgap->cpt);
					dis0 = dis; j0 = j; in1 = in;
				}
			}
			if (found)
			{
				if (zig)
					ncl_zigzag (&Sloop,in,npts,nj,ptsio,tol);
				else if (overlap != UU_NULL)
					ncl_check_direction (&Sloop,in,npts,nj,ptsio,overlap,tolsq);
				j0 = 0;
				dis0 = 1.e12;
				told = 2.25*tol*tol;
				zig = UU_FALSE;
				uu_list_push(&Sloop,&in);
				uu_list_delete (&Scur,j,1); ns = (int *) UU_LIST_ARRAY (&Scur);
				dis = S_connect_sqdis (lwatrev,wbase,wlev,fpt,lpt1);
				closed = (dis < told);
				if (!closed)
				{
					if (Scur.cur_cnt < 1 && dis < mxtol2)
					{
						closed = UU_TRUE; goto CLSD;
					}
					dis0 = dis;
					closed0 = UU_TRUE;
					lpt[0] = lpt1[0]; lpt[1] = lpt1[1]; lpt[2] = lpt1[2];
					if (Scur.cur_cnt < 1) um_vctovc (fpt,cgap->cpt);
					in0 = in; in1 = -1;
					j = 0;
				}
			}
			else
			{
				if (j == Scur.cur_cnt - 1 && Sloop.cur_cnt == 1 && dis0 >= mxtol2)
				{
					j = j0 = 0;
					dis0 = 1.e12;
					told = 2.25*tol*tol;
					zig = UU_FALSE;
/*
..... the first curve of a new loop could not be connected - so try to forget
..... it and start again
					uu_list_delete (&Scur,j,1);
*/
					closed = UU_TRUE;
					continue;
				}
				else if (j < Scur.cur_cnt - 1 || dis0 >= mxtol2)
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
			nlinks = Sloop.cur_cnt;
			if (nlinks > 0)
			{
				looplink = (int *) UU_LIST_ARRAY (&Sloop);
				np = 0;
				for (k = 0; k < nlinks; k++)
				{
					in = looplink[k];
					np += (npts[in]-1);
				}
/* add last point of the last segment - to close the loop */
				np++;
				if (np > 3)
				{
					wgeo.np = np;
					wgeo.pt = (UM_coord *) uu_malloc (np*sizeof(UM_coord));
					if (!wgeo.pt) { status = UU_FAILURE; goto Done; }
					wgeo.xrange[0] = 1000000.; wgeo.xrange[1] = -1000000.;
					wgeo.yrange[0] = 1000000.; wgeo.yrange[1] = -1000000.;
					ip = 0;
					for (i = 0; i < nlinks; i++)
					{
						in = looplink[i];
						nnp = (i == nlinks-1)? npts[in]-1: npts[in];
						n0 = nj[in];
						for (k = (i > 0); k < nnp; k++)
						{
							x = pts[n0+k][0]; y = pts[n0+k][1];
							if (x < wgeo.xrange[0]) wgeo.xrange[0] = x;
							if (x > wgeo.xrange[1]) wgeo.xrange[1] = x;
							if (y < wgeo.yrange[0]) wgeo.yrange[0] = y;
							if (y > wgeo.yrange[1]) wgeo.yrange[1] = y;
							um_vctovc (pts[n0+k],wgeo.pt[ip]);
							ip++;
						}
					}
					um_vctovc (fpt,wgeo.pt[ip]);
					uu_list_push (curves,&wgeo);
				}
			}
			zig = UU_FALSE;
			j = 0;
		}
	}

	if (llast && !closed && !ncl_batch_on() && Sloop.cur_cnt > 0)
		ncl_disp_connect_error (lwatrev,wbase,wlev,curves,pts,npts,nj,in1);

	if (!closed || curves->cur_cnt < 1)
	{
		status = UU_FAILURE;
		um_vctovc (lpt,cgap->lpt);
		cgap->lnj = nj[in0]; cgap->cnj = (in1 >= 0)? nj[in1]: -1;
		cgap->dsec = dis0;
		goto Done;
	}

	if (poctype != STK_BOUNDSF && status == UU_SUCCESS)
		status = ncl_set_insides (curves);
Done:
	return (status);
}
