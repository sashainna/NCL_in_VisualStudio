/*********************************************************************
**    NAME         : nepmill.c
**       CONTAINS:  main routines for PMILL
**
**        ncl_pmill_getfl
**        ncl_pmill_getmaxz
**        nclf_pmill_thrupt
**        nclf_pmill_reset_thrupt
**        ncl_pmill_debug_box
**        ncl_pmill_getpt_trianlst
**        ncl_pmill_shift_triangles
**        ncl_pmill_defold
**        ncl_pmill_arc_onoff
**        ncl_pmill_project_slice_norm
**			 ncl_pmill_slice_tess
**        ncl_pmill_spiral_shift
**        ncl_pmill_evolve_polyline
**        ncl_pmill_stepover_toolpath
**        ncl_pmill
**        ncl_pmill_create
**        nclf_pmill_npts
**        nclf_pmill_getpt
**        nclf_pmill_resetpts
**        nclf_pmill_push_pt
**
**    COPYRIGHT 2010 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       nebucket.c , 24.1
**    DATE AND TIME OF LAST MODIFICATION
**       09/11/13 , 13:02:44
*********************************************************************/
#include "nclfc.h"
#include "mdattr.h"
#include "mattr.h"
#include "mgeom.h"
#include "uminmax.h"
#include "mdpick.h"
#include "nclwaterln.h"
#include "nclclip.h"
#include "nccs.h"
/*
.....Use OpenMP libary
*/
#ifdef _OPENMP
#include <omp.h>
#endif

/*
.....Added to use existing SMILL routines
*/
extern int Sclpt_flag;	/*Sclpath flag*/
extern int Sslpt_flag;	/*Sslpath flag*/
extern int NPT;
extern int sfnum;
extern int asfnum;
extern int bsfnum;
extern int csfnum;
extern int Slayer;
extern UU_LIST sfky;
extern UU_LIST pptr;
extern UM_int2 mm;
extern UM_int2 wbatch;
extern UU_KEY_ID Sdpl[2];
extern UU_KEY_ID Sbnd;
extern UU_KEY_ID Sspt;
extern UU_REAL *sfs_xmmx, *sfs_ymmx;
extern NCL_waterline_surf *sff;
extern UU_REAL wtol,wtolsq;
extern UM_real8 tool[6];

extern void error(UM_int2*);

//int Sclpt_flag;	/*Sclpath flag*/
//int Sslpt_flag;	/*Sslpath flag*/
//
//int sfnum = 0;
//UU_LIST sfky;
//UU_LIST pptr;
//UM_int2 mm;
//UM_int2 wbatch = 0;
//UU_KEY_ID Sdpl[2];
//UU_KEY_ID Sbnd;
//UU_KEY_ID Sspt;
//UU_REAL *sfs_xmmx = UU_NULL, *sfs_ymmx = UU_NULL;
//NCL_waterline_surf *sff = UU_NULL;
//UU_REAL wtol,wtolsq;
//UM_real8 tool[6];

static UU_LIST pmill_triangles,pmill_pts;
static UU_LOGICAL pmill_flag = UU_FALSE,pmill_init = UU_FALSE;
static UU_LOGICAL debug_triangles = UU_FALSE;
static UU_REAL pmill_maxz = 0.;
static UU_LOGICAL Suse_thrupt;
static UM_coord Sthrupt;

extern int writeFile1 (char* value);
extern void urest(char*, int, int, char*);

/*********************************************************************
**    E_FUNCTION     : ncl_pmill_getfl(flag)
**       Returns the value of the flag that denotes whether a PMILL
**       command is currently being executed.
**
**    PARAMETERS
**       INPUT  : none
**       OUTPUT :
**          flag - UU_TRUE : PMILL is being executed
**                 UU_FALSE: PMILL is not being executed
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_pmill_getfl(flag)
UU_LOGICAL *flag;
{
	*flag = pmill_flag;
}

/*********************************************************************
**    E_FUNCTION     : ncl_pmill_getmaxz(zlev)
**       Returns the value of the highest z level accepted when 
**       searching for contact points in ncl_projsf_clpoint.
**
**    PARAMETERS
**       INPUT  : none
**       OUTPUT :
**          zlev - Value assigned as the highest z level accepted
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_pmill_getmaxz(zlev)
UU_REAL *zlev;
{
	*zlev = pmill_maxz;
}

/*********************************************************************
**    E_FUNCTION     : nclf_pmill_thrupt(x,y,z)
**       Sets the coordinates of the thru point used to define the
**       tool axis vector.
**
**    PARAMETERS
**       INPUT  :
**          x,y,z - Coordinates of thru point
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclf_pmill_thrupt(x,y,z)
UM_real4 *x,*y,*z;
{
	Sthrupt[0] = *x;
	Sthrupt[1] = *y;
	Sthrupt[2] = *z;
	Suse_thrupt = UU_TRUE;
}

/*********************************************************************
**    E_FUNCTION     : nclf_pmill_reset_thrupt()
**       Resets the parameters of the thru point and sets the flag that
**       denotes whether to use the thru point to UU_FALSE.
**
**    PARAMETERS
**       INPUT  : none
**       OUTPUT :
**          zlev - Value assigned as the highest z level accepted
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclf_pmill_reset_thrupt()
{
	Sthrupt[0] = Sthrupt[1] = Sthrupt[2] = 0.;
	Suse_thrupt = UU_FALSE;
}

/*********************************************************************
**    E_FUNCTION     : ncl_pmill_debug_box(xmmx,ymmx)
**       Ouput debug info for bounding box.
**
**    PARAMETERS
**       INPUT  : 
**          xmmx - X-coordinate bounds
**          ymmx - Y-coordinate bounds
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_pmill_debug_box(xmmx,ymmx)
UU_REAL *xmmx,*ymmx;
{
	char sbuf[80];

	sprintf(sbuf,"LN/%lf,%lf,%lf,%lf,%lf,%lf",xmmx[0],ymmx[0],0.,xmmx[0],
		ymmx[0],pmill_maxz);
	NclxDbgPstr(sbuf);
	sprintf(sbuf,"LN/%lf,%lf,%lf,%lf,%lf,%lf",xmmx[0],ymmx[1],0.,xmmx[0],
		ymmx[1],pmill_maxz);
	NclxDbgPstr(sbuf);
	sprintf(sbuf,"LN/%lf,%lf,%lf,%lf,%lf,%lf",xmmx[1],ymmx[0],0.,xmmx[1],
		ymmx[0],pmill_maxz);
	NclxDbgPstr(sbuf);
	sprintf(sbuf,"LN/%lf,%lf,%lf,%lf,%lf,%lf",xmmx[1],ymmx[1],0.,xmmx[1],
		ymmx[1],pmill_maxz);
	NclxDbgPstr(sbuf);
	sprintf(sbuf,"LN/%lf,%lf,%lf,%lf,%lf,%lf",xmmx[0],ymmx[0],0.,xmmx[0],
		ymmx[1],0.);
	NclxDbgPstr(sbuf);
	sprintf(sbuf,"LN/%lf,%lf,%lf,%lf,%lf,%lf",xmmx[0],ymmx[1],0.,xmmx[1],
		ymmx[1],0.);
	NclxDbgPstr(sbuf);
	sprintf(sbuf,"LN/%lf,%lf,%lf,%lf,%lf,%lf",xmmx[1],ymmx[1],0.,xmmx[1],
		ymmx[0],0.);
	NclxDbgPstr(sbuf);
	sprintf(sbuf,"LN/%lf,%lf,%lf,%lf,%lf,%lf",xmmx[1],ymmx[0],0.,xmmx[0],
		ymmx[0],0.);
	NclxDbgPstr(sbuf);
	sprintf(sbuf,"LN/%lf,%lf,%lf,%lf,%lf,%lf",xmmx[0],ymmx[0],pmill_maxz,
		xmmx[0],ymmx[1],pmill_maxz);
	NclxDbgPstr(sbuf);
	sprintf(sbuf,"LN/%lf,%lf,%lf,%lf,%lf,%lf",xmmx[0],ymmx[1],pmill_maxz,
		xmmx[1],ymmx[1],pmill_maxz);
	NclxDbgPstr(sbuf);
	sprintf(sbuf,"LN/%lf,%lf,%lf,%lf,%lf,%lf",xmmx[1],ymmx[1],pmill_maxz,
		xmmx[1],ymmx[0],pmill_maxz);
	NclxDbgPstr(sbuf);
	sprintf(sbuf,"LN/%lf,%lf,%lf,%lf,%lf,%lf",xmmx[1],ymmx[0],pmill_maxz,
		xmmx[0],ymmx[0],pmill_maxz);
	NclxDbgPstr(sbuf);
}

/*********************************************************************
**    E_FUNCTION     : ncl_pmill_getpt_trianlst(rad,pt,trilst)
**       Find tesselation triangles "under" the spherical cutter
**       with the tool end point pt and a tool axis of (0,0,1).
**       Note that the tessellation is rotated to make the triangles
**       oriented so the current tool axis can be treated as (0,0,1).
**
**    PARAMETERS
**       INPUT  :
**          rad    - Tool radius
**          pt     - Tool end point
**          tol    - Tolerance to use in triangle finding
**       OUTPUT :
**          trilst - List of found triangles
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_pmill_getpt_trianlst(rad,pt,trilst,tol)
UU_REAL rad,tol;
UM_coord pt;
UU_LIST *trilst;
{
	int i,ntri,status;
	UM_trian *tri;
	UU_REAL xmmx[2],ymmx[2],txmmx[2],tymmx[2];
	char sbuf[80];
/*
.....Define the boundaries region of the search area
*/
	xmmx[0] = pt[0] - rad;
	xmmx[1] = pt[0] + rad;
	ymmx[0] = pt[1] - rad;
	ymmx[1] = pt[1] + rad;
	ntri = UU_LIST_LENGTH (&pmill_triangles);
	tri = (UM_trian *) UU_LIST_ARRAY (&pmill_triangles);
	if (debug_triangles) ncl_pmill_debug_box(xmmx,ymmx);
	for (i=0;i<ntri;i++)
	{
/*
.....Ignore any triangle completely above the maximum z level
*/
		if (tri[i].p1[2] > pmill_maxz && tri[i].p2[2] > pmill_maxz &&
			tri[i].p3[2] > pmill_maxz) continue;
		txmmx[0] = tymmx[0] = 1.e12;
		txmmx[1] = tymmx[1] = -1.e12;
/*
.....Get bounding box for triangle
*/
		if (tri[i].p1[0] < txmmx[0]) txmmx[0] = tri[i].p1[0];
		if (tri[i].p1[0] > txmmx[1]) txmmx[1] = tri[i].p1[0];
		if (tri[i].p1[1] < tymmx[0]) tymmx[0] = tri[i].p1[1];
		if (tri[i].p1[1] > tymmx[1]) tymmx[1] = tri[i].p1[1];

		if (tri[i].p2[0] < txmmx[0]) txmmx[0] = tri[i].p2[0];
		if (tri[i].p2[0] > txmmx[1]) txmmx[1] = tri[i].p2[0];
		if (tri[i].p2[1] < tymmx[0]) tymmx[0] = tri[i].p2[1];
		if (tri[i].p2[1] > tymmx[1]) tymmx[1] = tri[i].p2[1];

		if (tri[i].p3[0] < txmmx[0]) txmmx[0] = tri[i].p3[0];
		if (tri[i].p3[0] > txmmx[1]) txmmx[1] = tri[i].p3[0];
		if (tri[i].p3[1] < tymmx[0]) tymmx[0] = tri[i].p3[1];
		if (tri[i].p3[1] > tymmx[1]) tymmx[1] = tri[i].p3[1];
/*
.....Check for box intersection
*/
		status = um_isect_boxes(xmmx,ymmx,txmmx,tymmx,tol);
		if (status == 1)
		{
			uu_list_push(trilst,&tri[i]);
#if 0
	sprintf(sbuf,"PT/%lf,%lf,%lf",tri[i].p1[0],tri[i].p1[1],tri[i].p1[2]);
	NclxDbgPstr(sbuf);
	sprintf(sbuf,"PT/%lf,%lf,%lf",tri[i].p2[0],tri[i].p2[1],tri[i].p2[2]);
	NclxDbgPstr(sbuf);
	sprintf(sbuf,"PT/%lf,%lf,%lf",tri[i].p3[0],tri[i].p3[1],tri[i].p3[2]);
	NclxDbgPstr(sbuf);
#endif
		}
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_pmill_shift_triangles(mx,flag)
**       Translate triangles to new coordinate system.  The triangles
**       are moved so the current SMILL routines can be used for
**       projecting a point onto the tessellation to find the correct
**       cutter height so there are no gouges.  The routines assume
**       the tool axis is (0,0,1) so we rotate the triangles to
**       satiasfy this assumption.
**
**    PARAMETERS
**       INPUT  :
**           mx   - Transformation matrix for shifting to new system
**           flag - Output debug data if flag = 1
**    OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_pmill_shift_triangles(mx,flag)
UM_transf mx;
UU_LOGICAL flag;
{
	int i,ntri;
	UM_trian *tri;
	char sbuf[80];

	ntri = UU_LIST_LENGTH (&pmill_triangles);
	tri = (UM_trian *) UU_LIST_ARRAY (&pmill_triangles);
	#pragma omp parallel for private(i)
	for (i=0;i<ntri;i++)
	{
		um_cctmtf(tri[i].p1,mx,tri[i].p1);
		um_cctmtf(tri[i].p2,mx,tri[i].p2);
		um_cctmtf(tri[i].p3,mx,tri[i].p3);
		if (flag)
		{
			sprintf(sbuf,"PT/%lf,%lf,%lf",tri[i].p1[0],tri[i].p1[1],tri[i].p1[2]);
			NclxDbgPstr(sbuf);
			sprintf(sbuf,"PT/%lf,%lf,%lf",tri[i].p2[0],tri[i].p2[1],tri[i].p2[2]);
			NclxDbgPstr(sbuf);
			sprintf(sbuf,"PT/%lf,%lf,%lf",tri[i].p3[0],tri[i].p3[1],tri[i].p3[2]);
			NclxDbgPstr(sbuf);
		}
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_pmill_defold (cvpoint,tolsq,n1)
**       Get rid of zigzags in the offset curve.
**
**    PARAMETERS
**       INPUT  :
**           cvpoint - list of points to evaluate/modify
**           tolsq   - tolerance sqaured
**           n1      - number of points in cvpoint
**       OUTPUT :
**           cvpoint - list of resulting points
**           n1      - number of points in cvpoint
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_pmill_defold (cvpoint,tolsq,n1)
UU_REAL tolsq;
UU_LIST *cvpoint;
int *n1;
{
	int ipts,tipts,npts,k,ind1,ind2;
	UM_clpt *pp;
	UM_vector *vv;
	UM_vector vvp,vv1,vv2,vv11,vv22;
	UU_REAL dd,co,co1,co2;
	UU_REAL tolsq0 = tolsq/4.;
	UU_LIST cvtang;
	char sbuf[80];

	npts = *n1;
	if (npts <= 0) return;
	pp = (UM_clpt *) UU_LIST_ARRAY(cvpoint);
	uu_list_init(&cvtang,sizeof(UM_vector),npts,npts);
/*
.....Set up forward vectors
*/
	for (ipts=0;ipts<npts;ipts++)
	{
		k = um_mod(ipts+1,npts);
		um_vcmnvc(pp[k].pte,pp[ipts].pte,vvp);
		uu_list_push(&cvtang,&vvp);
	}
/*
.....Check for folds
*/
	for (ipts = 0; ipts < npts-1; ipts++)
	{
		pp = (UM_clpt *) UU_LIST_ARRAY(cvpoint);
		vv = (UM_vector *) UU_LIST_ARRAY(&cvtang);
		um_vcmnvc (pp[ipts+1].pte,pp[ipts].pte,vvp);
		dd = UM_DOT (vvp,vvp);
		if (dd < tolsq0)
		{
			uu_list_delete (cvpoint,ipts+1,1);
			uu_list_delete (&cvtang,ipts+1,1);
			npts--;
			ipts--;
			continue;
		}
		if (dd < tolsq) continue;
		dd = sqrt (dd);
		for (k = 0; k < 3; k++)	vvp[k] = vvp[k]/dd;

		um_unitvc (vv[ipts],vv1);
		um_unitvc (vv[ipts+1],vv2);

		co = UM_DOT (vv2,vv1);
/*
.....Found sharp enough change in direction
*/
		if (co < -0.71067812) /* -45 degrees */
		{
			ind1 = um_mod(ipts-1,npts);
			ind2 = um_mod(ipts+2,npts);
			um_unitvc (vv[ind1],vv11);
			um_unitvc (vv[ind2],vv22);
			co1 = UM_DOT(vv1,vv11);
			co2 = UM_DOT(vv2,vv22);
/*
.....If the previous or next forward vector does not agree with the
.....current forward vector's direction the current forward vector
.....is assumed to be a switchback.
*/
			if (co1 < 0.906307787 || co2 < 0.906307787) /* 25 degrees */
			{
				uu_list_delete (cvpoint,ipts+1,1);
				uu_list_delete (&cvtang,ipts+1,1);
				pp = (UM_clpt *) UU_LIST_ARRAY(cvpoint);
				vv = (UM_vector *) UU_LIST_ARRAY(&cvtang);
				npts--;
				ipts--;
				if (npts <= 0) break;
				if (ipts > 0) k = ipts;
				else k = 0;
				for (tipts=k;tipts<ipts+3;tipts++)
				{
					if (tipts == npts-1)
					{
						um_vctovc(vv[tipts-1],vv[tipts]);
						break;
					}
					um_vcmnvc(pp[tipts+1].pte,pp[tipts].pte,vv[tipts]);
					um_unitvc(vv[tipts],vv[tipts]);
				}
				if (ipts > 0) ipts--;
				if (ipts > 0) ipts--;
			}
		}
	}

	uu_list_free(&cvtang);
	*n1 = npts;
	return;
}
/*********************************************************************
**    E_FUNCTION : ncl_pmill_arc_onoff(rad,pvec,nvec,clpoints,type,flag,dir)
**       Make arc entry and exit moves for the current slice plane.
**       Note that the move is made only in the plane.
**    PARAMETERS
**       INPUT  :
**          rad      - Arc radius desired
**          pvec     - Slice plane normal vector
**          nvec     - Point projection vector in slice plane
**          clpoints - List of tool path points
**          type     - 0:Entry move 1:Exit move
**          flag     - CL point list flag
**          dir      -  CCLW orientation flag
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_pmill_arc_onoff(rad,pvec,nvec,clpoints,type,flag,dir)
UU_REAL rad;
UU_LIST *clpoints;
UM_vector nvec,pvec;
int type,flag,dir;
{
	int i,strt,end,npts,nc,is0,is1;
	UU_REAL deltang,cosa,ang,dang;
	UM_coord cpt,rcpt,pt;
	UM_vector vc1,vc2;
	UM_clpt *cpts,pos;
	UM_transf rot,rtinv;
	UU_LIST apoints;
	char sbuf[80];
/*
.....Initialize routine
*/
	cpts = (UM_clpt *)UU_LIST_ARRAY (clpoints);
	npts = clpoints->cur_cnt;
	if (type == 1) is0 = 1;
	else is0 = npts - 1;
	is1 = is0-1;
/*
.....Calculate entry circle
........Circle center
*/
	um_vcmnvc(cpts[is0].pte,cpts[is1].pte,vc1);
	um_unitvc(vc1,vc1);
	um_cross (vc1,pvec,vc2); um_unitvc(vc2,vc2);
	if (UM_DOT(nvec,vc2) < 0.) um_negvc(vc2,vc2);
#if 0
	sprintf(sbuf,"PV/%lf,%lf,%lf,%lf,%lf,%lf",cpts[0].pte[0],cpts[0].pte[1],
		cpts[0].pte[2],vc2[0],vc2[1],vc2[2]);
	NclxDbgPstr(sbuf);
#endif
	um_translate_point(cpts[0].pte,rad,vc2,cpt);
	um_ptzx_tf(cpts[0].pte,pvec,vc2,rot); um_inverttf(rot,rtinv);
	um_cctmtf(cpt,rtinv,rcpt);
/*
.....Determine how many points to output on circle
*/
	cosa = (rad-.002) / rad;
	if (cosa > 1.) cosa = 1.;
	deltang = fabs(acos(cosa));
	if (deltang <= 0.) nc = 0;
	else nc = UM_HALFPI / deltang;
	if (nc < 4) nc = 4;
	if (nc > 99) nc = 99;
/*
.....Set up starting position data
*/
	dang = UM_HALFPI / nc;
	strt = 0;
	end = nc;
	if (type == 1)
		ang = UM_PI;
	else
	{
		if (dir == 1) ang = UM_HALFPI;
		else ang = UM_PI + UM_HALFPI;
	}
	if (dir == 0) deltang *= -1.;
/*
.....Build points along arc
*/
	uu_list_init(&apoints,sizeof(UM_clpt),nc,nc);
	for (i=strt;i<end;i++)
	{
		pt[0] = rcpt[0] + rad*cos(ang);
		pt[1] = rcpt[1] + rad*sin(ang);
		pt[2] = 0.;
		um_cctmtf(pt,rot,pos.pte);
		um_vcmnvc(Sthrupt,pos.pte,pos.vta); um_unitvc(pos.vta,pos.vta);
		uu_list_push(&apoints,&pos);
#if 0
	sprintf(sbuf,"PT/%lf,%lf,%lf",pos.pte[0],pos.pte[1],pos.pte[2]);
	NclxDbgPstr(sbuf);
#endif
		ang += deltang;
	}
	ncl_sm_clpath_push_list(flag,&apoints);
	uu_list_free(&apoints);
	return;
}

/*********************************************************************
**    E_FUNCTION : ncl_pmill_project_slice_norm(plpt,nvec,toler,ptsio)
**       pmill tool path cl point routine. Project the cl points onto
**       the tessellation to determine the tool path points.
**
**       INPUT  :
**			  points   - Pointer to list of clpoints
**			  nvecs    - Pointer to list of cl point normal vectors
**         npts     - Length of points and nvecs lists
**         plpt     - Slicing plane point
**         nvec     - Slicing plane normal vector
**         crad     - Cutter radius
**         frad     - Cutter fillet radius (Assumed to be 0)
**         thk      - Additional cutter thick value
**         tol      - Tolerance
**         transfl  - Transition type used between passes
**                    0:SPIRAL 1:ARC 2:LINEAR
**         atrad    - Arc transition radius
**         orientfl - 0:CCLW orientation used 1:CLW orientation used
**       OUTPUT  :
**         none
**    RETURNS      : UU_SUCCESS / UU_FAILURE
**    SIDE EFFECTS : Adds resulting tool path points to final tool
**                   path list 
**    WARNINGS     : none
*********************************************************************/
int ncl_pmill_project_slice_norm(points,nvecs,npts,spt,plpt,nvec,crad,
	frad,thk,tol,transfl,atrad,orientfl)
int npts,transfl,orientfl;
UU_LIST *points,*nvecs;
UM_coord plpt,spt;
UM_vector nvec;
UU_REAL crad,frad,thk,tol,atrad;
{
	int i,j,nclpts,status,ntimes,infl;
	UU_REAL rad,dis,dmin,dminsq,dang,tolsq,cur_dis,ang,delt;
	UM_coord *pts,ccpt,tcpt,prev_center,vpt,ppt,tpt,*cpts;
	UM_vector tool_vec,*tvec,fvec,cvec,pvec,test_vec,tpvec;
	UM_vector first_vec,last_vec;
	UU_LOGICAL rotfl,inside,moved,spiralfl,hit = UU_FALSE;
	UM_transf prot,prtinv;
	UU_LIST clpoints,tpts,ccpts,centers;
	UM_clpt pos,*clpts,tclpt;
	struct UM_rotmatrix mx;
	static int cnt = 0;
	char sbuf[80];
/*
.....Set up data for projection
*/
	if (npts <= 0) return UU_FAILURE;
	spiralfl = (transfl == 0);
	pts = (UM_coord *) UU_LIST_ARRAY (points);
	tvec = (UM_vector *) UU_LIST_ARRAY (nvecs);
/*
.....Set up minimum offset distance that is used to check if normal vector
.....used gives good projection point.
*/
	//rad = crad + frad + thk;
	rad = crad + frad;
	moved = UU_FALSE;
	//dmin = 0.875*rad;	//original
	//dmin=0.6*(rad-thk)/thk;
	dmin = 0.4*rad;
	//dmin = 4*thk*rad;
	//dmin = (1-0.45*thk/(rad-thk))*rad;
	dminsq = dmin*dmin;
	tolsq = tol*tol;
/*
.....Project points to get tool center points
*/
	uu_list_init(&clpoints,sizeof(UM_clpt),npts,npts);
	uu_list_init(&ccpts,sizeof(UM_coord),npts,npts);
	uu_list_init(&centers,sizeof(UM_coord),npts,npts);
	rotfl = UU_FALSE; nclpts = 0;
	for (i=0; i<npts; i++)
	{
		moved = UU_FALSE; infl = -1;
		um_vctovc(tvec[i],tool_vec);
		um_cross (tvec[i],nvec,pvec); um_unitvc(pvec,pvec);
		um_ptzx_tf(pts[i],tool_vec,pvec,prot); um_inverttf(prot,prtinv);
		ncl_pmill_shift_triangles(prtinv,UU_FALSE);
		um_cctmtf(pts[i],prtinv,ppt);
		ppt[2] = -rad;
		pmill_maxz = 0.;
		status = ncl_projsf_clpoint(rad,0.,ppt,ccpt,0.,tol,UU_FALSE,0,
			UU_NULL,&inside);
		ncl_pmill_shift_triangles(prot,UU_FALSE);
		um_cctmtf(ppt,prot,pos.pte);
		cur_dis = um_sqdis(pos.pte,pts[i]);
/*
.....Do not use point if it moves the cutter backwards
*/
		if (i > 0)
		{
			um_translate_point(pos.pte,rad,tool_vec,tcpt);
			um_vcmnvc(pts[i],pts[i-1],fvec); um_unitvc(fvec,fvec);
			um_vcmnvc(tcpt,prev_center,cvec); um_unitvc(cvec,cvec);
			if (UM_DOT(fvec,cvec) < 0.) continue;
		}
/*
.....Try to fix offset point if offset point is too far from surface
*/
		if (cur_dis < dminsq && i > 0)
		{
/*
.....Set up validity check data
*/
			um_translate_point(pos.pte,rad,tool_vec,tcpt);
			infl = ncl_pt_in_contour(pts,npts,tool_vec,pvec,tcpt,tol);
			if (infl >= 0) dmin = um_sqdis(tcpt,pts[i]);
			else dmin = 1.e12;
/*
.....Set up rotation data
*/
			um_translate_point(pts[i],rad,tool_vec,vpt);
			ang = 75.; delt = 5.;
			dang = ((UU_REAL)-ang)/UM_RADIAN;
			um_rotatept(vpt,nvec,pts[i],dang,UU_TRUE,&mx);
			um_vcmnvc(vpt,pts[i],test_vec); um_unitvc(test_vec,test_vec);
			dang = delt/UM_RADIAN;
			ntimes = 2*((int)(ang/delt)) + 1;
/*
.....Set up forward sense vector
*/
			um_vcmnvc(pts[i],pts[i-1],fvec); um_unitvc(fvec,fvec);
			for (j=0; j<ntimes; j++)
			{
				if (j != ntimes/2)
				{
/*
.....Try new projection
*/
					um_cross (test_vec,nvec,tpvec); um_unitvc(tpvec,tpvec);
					um_ptzx_tf(pts[i],test_vec,tpvec,prot); um_inverttf(prot,prtinv);
					ncl_pmill_shift_triangles(prtinv,UU_FALSE);
					um_cctmtf(pts[i],prtinv,ppt);
					ppt[2] = -rad;
					pmill_maxz = 0.;
					status = ncl_projsf_clpoint(rad,0.,ppt,ccpt,0.,tol,UU_FALSE,0,
						UU_NULL,&inside);
					ncl_pmill_shift_triangles(prot,UU_FALSE);
					um_cctmtf(ppt,prot,tpt);
					um_translate_point(tpt,rad,test_vec,tcpt);
/*
.....Make sure the move is in the forward direction
*/
					um_vcmnvc(tcpt,prev_center,cvec); um_unitvc(cvec,cvec);
					if (UM_DOT(fvec,cvec) >= 0.)
					{
/*
.....Check if new projection moves cutter center point in or on the contour
*/
						infl = ncl_pt_in_contour(pts,npts,test_vec,tpvec,tcpt,tol);
						if (infl >= 0)
						{
/*
.....Make sure the point used is the closest to the contour point
*/
							dis = um_sqdis(pts[i],tcpt);
							if (dis < dmin)
							{
#if 0
if (cnt == 0)
{
	sprintf(sbuf,"PV/%lf,%lf,%lf,%lf,%lf,%lf",pts[i][0],pts[i][1],pts[i][2],
		test_vec[0],test_vec[1],test_vec[2]);
	NclxDbgPstr(sbuf);
}
#endif
								um_vctovc(tpt,pos.pte);
								um_vctovc(test_vec,pos.vta);
								um_vctovc(test_vec,tool_vec);
								dmin = dis;
								moved = UU_TRUE;
								if (dmin < tol) break;
							}
						}
					}
				}
				um_rotatept(vpt,nvec,pts[i],dang,UU_TRUE,&mx);
				um_vcmnvc(vpt,pts[i],test_vec); um_unitvc(test_vec,test_vec);
			}
			cnt++;
			if (!moved && dmin > 1.e11) continue;
		}
		um_translate_point(pos.pte,rad,tool_vec,pos.pte);
/*
.....Make sure point lies in or on the offset contour to eliminate
.....gouging
*/
		if (!moved)
			infl = ncl_pt_in_contour(pts,npts,tool_vec,pvec,pos.pte,tol);
		if (moved || infl >= 0)
		{
			um_vctovc(pos.pte,prev_center);
/*
.....Use THRU point to set tool axis
*/
			if (Suse_thrupt)
			{
				um_vcmnvc(pos.pte,Sthrupt,pos.vta);
				um_unitvc(pos.vta,pos.vta);
			}
/*
.....Use slice plane normal vector if no THRU point.  This will need
.....to be replaced.  Only added temporarily
*/
			else
				um_negvc(nvec,pos.vta);
			um_translate_point(pos.pte,(crad+frad),pos.vta,pos.pte);
			um_negvc(pos.vta,pos.vta);
			uu_list_push(&clpoints,&pos);
			um_cctmtf(ccpt,prot,tcpt);
			uu_list_push(&ccpts,&tcpt);
			uu_list_push(&centers,&prev_center);
			if (!hit) um_vctovc(tool_vec,first_vec);
			um_vctovc(tool_vec,last_vec);
			nclpts++;
		}
	}
	pmill_maxz = 0.;
	if (nclpts > 0)
	{
		cpts = (UM_coord *) UU_LIST_ARRAY (&centers);
		pts = (UM_coord *) UU_LIST_ARRAY (&ccpts);
/*
.....Extra center point check.  No contact point should be less than the
.....cutter radius away from any center point.  Remove any center points
.....that violate this restriction.
*/
		dmin = (rad-5.*tol)*(rad-5.*tol);
		for (i=0;i<nclpts;i++)
		{
			for (j=0;j<nclpts;j++)
			{
				if (um_sqdis(pts[j],cpts[i]) < dmin)
				{
					uu_list_delete(&centers,i,1);
					uu_list_delete(&clpoints,i,1);
					cpts = (UM_coord *) UU_LIST_ARRAY (&centers);
					i--; nclpts--;
					break;
				}
			}
		}
/*
.....Fold checking is designed to prevent sudden changes in the cutter
.....forward direction.
*/
		ncl_pmill_defold (&clpoints,tol*tol,&nclpts);
		clpts = (UM_clpt *) UU_LIST_ARRAY (&clpoints);
/*
.....Make sure loop is closed
*/
		if (!spiralfl)
		{
			uu_list_push(&clpoints,&clpts[0]);
			nclpts++;
		}
		ncl_weed_clpts(&clpoints,tol);
		ncl_sm_clpath_create(Sslpt_flag);
		if (transfl == 1)
			ncl_pmill_arc_onoff(atrad,nvec,first_vec,&clpoints,0,Sslpt_flag,
				orientfl);
		ncl_sm_clpath_push_list(Sslpt_flag,&clpoints);
		if (transfl == 1)
			ncl_pmill_arc_onoff(atrad,nvec,first_vec,&clpoints,1,Sslpt_flag,
				orientfl);
#if 0
	clpts = (UM_clpt *) UU_LIST_ARRAY (&clpoints);
	for (i=0; i<clpoints.cur_cnt; i++)
	{
		sprintf(sbuf,"GOTO/%lf,%lf,%lf,%lf,%lf,%lf",clpts[i].pte[0],clpts[i].pte[1],
			clpts[i].pte[2],clpts[i].vta[0],clpts[i].vta[1],clpts[i].vta[2]);
		NclxDbgPstr(sbuf);
	}
#endif
	}
	uu_list_free(&clpoints);
	uu_list_free(&ccpts);
	uu_list_free(&centers);
	return UU_SUCCESS;
}
#if 0
/*********************************************************************
**    E_FUNCTION : ncl_pmill_project_plunge(plpt,nvec,toler,ptsio)
**       pmill tool path cl point routine. Project the cl points onto
**       the tessellation to determine the tool path points.
**
**.....This routine is not currently used, but it was left as is in case
**.....plunge style milling is added and any remaining code is wanted
**
**       INPUT  :
**			  points - Pointer to list of clpoints
**			  points - Pointer to list of cl point normal vectors
**         npts   - Number of cl points
**         plpt   - Slicing plane point
**         nvec   - Slicing plane normal vector
**       OUTPUT  :
**         none
**    RETURNS      : UU_SUCCESS / UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_pmill_project_plunge(points,nvecs,npts,spt,plpt,
				nvec,crad,frad,thk,tol,spiralfl,plungefl,ix)
int npts,ix;
UU_LIST *points,*nvecs;
UM_coord plpt,spt;
UM_vector nvec;
UU_REAL crad,frad,thk,tol;
UU_LOGICAL spiralfl,plungefl;
{
	int i,j,k,inside,cnpts,status;
	UU_REAL dis,dmin,len,rad;
	UM_coord *pts,pt,npt,ppt,ccpt,thrupt;
	UM_vector *vecs,fvec,pnvec,tvec,xvec,ovec;
	struct NCL_crvgen_rec ptve,*ptves;
	UU_LIST ptvelst,clpoints,tclpt;
	UM_transf prot,prtinv;
	UM_clpt pos,*tpts;
	char sbuf[80];

#if 0
	ncl_debug_pts(points,0);
#endif
	rad = crad + frad + thk;
	thrupt[0] = -2.; thrupt[1] = 3.7; thrupt[2] = -2.973;
	pts = (UM_coord *) UU_LIST_ARRAY(points);
	vecs = (UM_vector *) UU_LIST_ARRAY(nvecs);
	uu_list_init(&ptvelst,sizeof(struct NCL_crvgen_rec),npts,npts);
	for (i=0;i<npts;i++)
	{
		ptve.x = pts[i][0];
		ptve.y = pts[i][1];
		ptve.z = pts[i][2];
		ptve.inv = 0;
		uu_list_push(&ptvelst,&ptve);
	}
	cnpts = npts;
	ptves = (struct NCL_crvgen_rec *) UU_LIST_ARRAY(&ptvelst);
	ncl_crvfit (&cnpts, ptves);
#if 0
	for (i=0;i<cnpts-1;i++)
	{
		sprintf(sbuf,"LN/%lf,%lf,%lf,%lf,%lf,%lf",ptves[i].x,ptves[i].y,
			ptves[i].z,ptves[i+1].x,ptves[i+1].y,ptves[i+1].z);
		NclxDbgPstr(sbuf);
	}
#endif
/*
.....Find the best fitting projection vector for each point from the
.....curve through the original points.
*/
	uu_list_init(&clpoints,sizeof(UM_clpt),cnpts,cnpts);
	for (i=0;i<cnpts;i++)
	{

		dmin = 1.e12;
		pt[0] = ptves[i].x; pt[1] = ptves[i].y; pt[2] = ptves[i].z;
		for (j=0;j<npts-1;j++)
		{
			um_vcmnvc(pts[j+1],pts[j],fvec); 
			len = UM_MAG(fvec); um_unitvc(fvec,fvec);
			inside = um_nptsg1 (pt,pts[j],fvec,len,npt,&dis);
			if (dis < dmin)
			{
				dmin = dis;
				if (inside == 0)
				{
					k = um_mod(j-1,npts);
					um_vcplvc(vecs[k],vecs[j],pnvec);
				}
				else if (inside == 1)
				{
					um_vcplvc(vecs[j],vecs[j+1],pnvec);
				}
				else
				{
					k = um_mod(j+2,npts);
					um_vcplvc(vecs[k],vecs[j+1],pnvec);
				}
				um_unitvc(pnvec,pnvec);
			}
		}

		tvec[0]=ptves[i].dx; tvec[0]=ptves[i].dy; tvec[0]=ptves[i].dz;
		um_unitvc(tvec,tvec);
#if 0
//if (i%8 == 0)
{
	sprintf(sbuf,"PV/%lf,%lf,%lf,%lf,%lf,%lf",ptves[i].x,ptves[i].y,
		ptves[i].z,pnvec[0],pnvec[1],pnvec[2]);
	NclxDbgPstr(sbuf);
}
#endif
		um_cross(pnvec,tvec,xvec); um_unitvc(xvec,xvec);
		um_translate_point(pt,-2.*tol,pnvec,ppt);
		um_ptzx_tf(ppt,pnvec,xvec,prot); um_inverttf(prot,prtinv);
		ncl_pmill_shift_triangles(prtinv,UU_FALSE);
		um_cctmtf(ppt,prtinv,ppt);
		pmill_maxz = rad;
		status = ncl_projsf_clpoint(rad,0.,ppt,ccpt,0.,tol,UU_FALSE,0,
			UU_NULL,&inside);
		ncl_pmill_shift_triangles(prot,UU_FALSE);
		um_cctmtf(ppt,prot,pos.pte);
		um_translate_point(pos.pte,rad,pnvec,pos.pte);
		um_vcmnvc(pos.pte,thrupt,pos.vta); um_unitvc(pos.vta,pos.vta);
		um_translate_point(pos.pte,(crad+frad),pos.vta,pos.pte);
		um_negvc(pos.vta,pos.vta);
		uu_list_push(&clpoints,&pos);

	}
	cnpts = clpoints.cur_cnt;
	if (ix%2 == 1)
	{
		tpts = (UM_clpt *) UU_LIST_ARRAY (&clpoints);
		k = cnpts/2;
		for (i=0,j=cnpts-1;i<k;i++,j--)
		{
			um_vctovc(tpts[i].pte,pos.pte);
			um_vctovc(tpts[i].vta,pos.vta);
			um_vctovc(tpts[j].pte,tpts[i].pte);
			um_vctovc(tpts[j].vta,tpts[i].vta);
			um_vctovc(pos.pte,tpts[j].pte);
			um_vctovc(pos.vta,tpts[j].vta);
		}
	}
	ncl_weed_clpts(&clpoints,tol);
	ncl_sm_clpath_create(Sslpt_flag);
	ncl_sm_clpath_push_list(Sslpt_flag,&clpoints);
	uu_list_free(&clpoints);
	uu_list_free(&ptvelst);
}
#endif
/*********************************************************************
**    E_FUNCTION : ncl_pmill_slice_tess(plpt,nvec,toler,ptsio)
**       pmill tool path slice routine. Generates a sorted list of points
**       by slicing the entire surface list with the given plane to
**       generate cl points.  The points are sorted so the start point
**       of each slice is the closest point to the start of the previous
**       slice.
**       INPUT  :
**			  plpt     - slicing plane point
**			  nvec     - slicing plane normal vector
**         spt      - Previous starting point.  The point is used when
**                    sorting the current slice points to set the start
**         crad     - Cutter radius
**         frad     - Cutter corner radius (Currently assumed to be 0.)
**         thk      - Additional thick value to apply to cutter
**			  toler    - tolerance to use in calculations
**         orientfl - 0:CCLW orientation 1:CLW orientation
**       OUTPUT  :
**         ptsio    - List of sorted points.
**         nvsio    - Surface normal vectors projected onto the slice
**                    plane
**         nptsio   - number of points in ptsio and vectors in nvsio
**    RETURNS      : UU_SUCCESS / UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_pmill_slice_tess(plpt,nvec,spt,crad,frad,thk,toler,ptsio,nvsio,
	nptsio,orientfl)
int *nptsio,orientfl;
UM_coord plpt,spt;
UM_vector nvec;
UU_REAL crad,frad,thk,toler;
UU_LIST *ptsio,*nvsio;
{
	int i,j,status,chunks,ind,*np,npts,nj,j_ind,idel,pnpts,orient;
	int in1,in2,npt_ind,um_nptsg1();
	UU_REAL eps,d1,d2,d3,d4,dmin,dot;
	UM_coord *ptio,*tptio,*pt_ind,*lstpts,cpt,tpt,npt,snpt,npt2;
	UM_vector *nvecs,*tnvecs,dvec,*vc_ind,fvec,nnvec,npt_nvec;
	UU_LIST ptlst,nvlst,nplst,tpts;
	struct NCL_nclpl_rec tpl;
	UM_transf rot,rtinv;
	UU_LOGICAL rotfl,inside;
	UM_real8 thk8;
	UM_int2 plunge;
	UM_2Dcoord *pts2d,p2d;
	UU_LIST ptlist2d;
	char sbuf[80];

	eps = toler*toler;
	um_vctovc(plpt,tpl.pt);
	um_vctovc(nvec,tpl.nvec);
/*
.....Slice tessellation with plane
*/
	um_cvio_new();
	status = um_cvio_intersection_triangles(&tpl,&pmill_triangles,toler);
	if (status != UU_SUCCESS)
	{
		um_cvio_free();
		return status;
	}
	um_cvio_sort_points (toler);
/*
.....Get slice points
*/
	chunks = um_cvio_getncv();
	uu_list_init(&ptlst,sizeof(UM_coord),100,100);
	uu_list_init(&nvlst,sizeof(UM_vector),100,100);
	uu_list_init(&nplst,sizeof(int),chunks,chunks);
	ind = 0;
	for (i=0;i<chunks;i++)
	{
		um_cvio_get(i,&npts,&ptio,&nvecs);
		uu_list_push_multiple(&ptlst,npts,&ptio[0]);
		uu_list_push_multiple(&nvlst,npts,&nvecs[0]);
		uu_list_push(&nplst,&npts);
	}
	um_cvio_free();
#if 0
	ncl_debug_pts(&ptlst,0);
#endif
/*
.....Sort sets of slice points
.......Currently assuming slice is for one port or pocket so the
.......slice points need to form a closed contour.
.......Always store the first chunk and then connect the reset in chains.
.......Also assumes the port will be closed or the gaps will be small enough
.......for the closest ends to be a reasonable assumption.
*/
	ptio = (UM_coord *) UU_LIST_ARRAY (&ptlst);
	nvecs = (UM_vector *) UU_LIST_ARRAY (&nvlst);
	np = (int *) UU_LIST_ARRAY (&nplst);
	if (chunks > 0)
	{
		if (chunks > 1)
		{
			for (i = 0; i < chunks; i++)
			{
				if (i == 0)
				{
					npts = np[i];
					nj = npts;
					uu_list_push_multiple(ptsio,npts,&ptio[0]);
					uu_list_push_multiple(nvsio,npts,&nvecs[0]);
					np[i] = -np[i];
					ptio += npts;
					nvecs += npts;
				}
				else
				{
					lstpts = (UM_coord *) UU_LIST_ARRAY(ptsio);
					tptio = ptio;
					tnvecs = nvecs;
					dmin = 1.e12;
					j_ind = -1;
/*
.....Find list that connects to the current list of points
.......Finds a match or closest neighbor
*/
					for (j = 1; j < chunks; j++)
					{
						npts = abs(np[j]);
						if (np[j] < 0)
						{
							tptio += npts;
							tnvecs += npts;
							continue;
						}
						d1 = um_sqdis(lstpts[0],tptio[0]);
						d2 = um_sqdis(lstpts[0],tptio[npts-1]);
						d3 = um_sqdis(lstpts[nj-1],tptio[0]);
						d4 = um_sqdis(lstpts[nj-1],tptio[npts-1]);
/*
.....Join lists if a match was found
*/
						if (d1 < 5.*eps || d2 < 5.*eps || d3 < 5.*eps || d4 < 5.*eps)
						{
							if (d1 < 5.*eps || d4 < 5.*eps)
							{
								ncl_revers1_list (npts,0,&tptio[0],1);
								ncl_revers1_list (npts,0,&tnvecs[0],1);
							}
							if (d1 < 5.*eps || d2 < 5.*eps)
							{
								uu_list_insert_multiple (ptsio,0,npts,&tptio[0]);
								uu_list_insert_multiple (nvsio,0,npts,&tnvecs[0]);
							}
							else if (d3 < 5.*eps || d4 < 5.*eps)
							{
								uu_list_push_multiple (ptsio,npts,&tptio[0]);
								uu_list_push_multiple (nvsio,npts,&tnvecs[0]);
							}
							np[j] = -np[j];
							nj += npts;
							j_ind = -1;
							break;
						}
/*
.....Keep track of closest match so they can be joined to form a closed contour
*/
						if (d1 < dmin || d2 < dmin || d3 < dmin || d4 < dmin)
						{
							if (d1 < dmin) {dmin = d1; ind = 1;}
							if (d2 < dmin) {dmin = d2; ind = 2;}
							if (d3 < dmin) {dmin = d3; ind = 3;}
							if (d4 < dmin) {dmin = d4; ind = 4;}
							pt_ind = tptio;
							vc_ind = tnvecs;
							j_ind = j;
						}
						tptio += npts;
						tnvecs += npts;
					}
/*
.....No exact match found
*/
					if (j_ind >= 0)
					{
						npts = np[j_ind];
						if (ind == 1 || ind == 4)
						{
							ncl_revers1_list (npts,0,&pt_ind[0],1);
							ncl_revers1_list (npts,0,&vc_ind[0],1);
						}
						if (ind == 1 || ind == 2)
						{
							if (dmin <= 16.*eps) npts -= 1;
							uu_list_insert_multiple(ptsio,0,npts,&pt_ind[0]);
							uu_list_insert_multiple(nvsio,0,npts,&vc_ind[0]);
						}
						else
						{
							if (dmin > 16.*eps)
							{
								uu_list_push_multiple (ptsio,npts,&pt_ind[0]);
								uu_list_push_multiple (nvsio,npts,&vc_ind[0]);
							}
							else
							{
								npts -= 1;
								uu_list_push_multiple (ptsio,npts,&pt_ind[1]);
								uu_list_push_multiple (nvsio,npts,&vc_ind[1]);
							}
						}
						np[j_ind] = -np[j_ind];
						nj += npts;
					}
				}
			}
		}
/*
.....Only one contour returned.  No sorting necessary
*/
		else
		{
			npts = np[0];
			uu_list_push_multiple(ptsio,npts,&ptio[0]);
			uu_list_push_multiple(nvsio,npts,&nvecs[0]);
		}
#if 0
	ncl_debug_pts(ptsio,0);
#endif
/*
.....Weed points
*/
		lstpts = (UM_coord *) UU_LIST_ARRAY (ptsio);
		npts = UU_LIST_LENGTH (ptsio);
		for (i = 0; i < npts; i++)
		{
			j = um_mod(i+1,npts);
			if (um_sqdis(lstpts[i],lstpts[j]) < 16.*eps)
			{
				idel = j;
				uu_list_delete (ptsio,idel,1);
				uu_list_delete (nvsio,idel,1);
				lstpts = (UM_coord *)UU_LIST_ARRAY(ptsio);
				i--;
				npts--;
			}
		}
		*nptsio = pnpts = npts;
		if (npts <= 0) goto done;
#if 0
	ncl_debug_pts(ptsio,0);
#endif
		nvecs = (UM_vector *) UU_LIST_ARRAY (nvsio);
		um_centroid (lstpts,npts,cpt,toler);
		um_vcmnvc(cpt,lstpts[0],dvec); um_unitvc(dvec,dvec);
		um_unitvc(nvecs[0],nvecs[0]);
		um_vctopln(nvecs[i],nvec,nvecs[i]); um_unitvc(nvecs[0],nvecs[0]);
		if (UM_DOT(dvec,nvecs[0]) < 0.) um_negvc(nvecs[0],nvecs[0]);
/*
.....Project normal vectors onto slice plane
.....Make sure that all normal vectors point to interior or all point to
.....exterior of contour.
*/
		for (i=0; i<npts; i++)
		{
			um_vctopln(nvecs[i],nvec,nvecs[i]);
			um_unitvc(nvecs[i],nvecs[i]);
			if (i > 0)
			{
				dot = UM_DOT(nvecs[i-1],nvecs[i]);
				if (dot < 0.)
					um_negvc (nvecs[i],nvecs[i]);
			}
		}
/*
.....Project points to plane for POCKET offset routines
*/
		uu_list_init(&pmill_pts,sizeof(UM_coord),npts,npts);
		pmill_init = UU_TRUE;
		uu_list_push_list(&pmill_pts,ptsio);
		rotfl = (nvec[2] < 1. - UM_DFUZZ);
		ptio = (UM_coord *) UU_LIST_ARRAY (&pmill_pts);
		if (rotfl)
		{
			fvec[0] = -nvec[1]; fvec[1] = nvec[0]; fvec[2] = 0.;
			um_ptzx_tf(ptio[0],nvec,fvec,rot); um_inverttf(rot,rtinv);
			for (i=0;i<npts;i++) um_cctmtf (ptio[i],rtinv,ptio[i]);
		}
/*
.....Plunge milling not currently supported so just pass 0 to the
.....offset routine.
*/
		thk8 = 2.*thk; plunge = 0;
		opmill(&thk8,&plunge);
		*nptsio = npts = UU_LIST_LENGTH (&pmill_pts);
		ptio = (UM_coord *) UU_LIST_ARRAY (&pmill_pts);
/*
.....Make sure all slices are in the same orientation
*/
		uu_list_init(&ptlist2d,sizeof(UM_2Dcoord),npts,npts);
		for (i=0;i<npts;i++)
		{
			um_vctovc_2d(ptio[i],p2d);
			uu_list_push(&ptlist2d,&p2d);
			if (rotfl) um_cctmtf (ptio[i],rot,ptio[i]);
		}
		pts2d = (UM_2Dcoord *)UU_LIST_ARRAY(&ptlist2d);
		orient = um_polygon_orientation(npts,pts2d);
		uu_list_free(&ptlist2d);
		if ((orient < 0 && orientfl == 1) || (orient > 0 && orientfl == 0))
		{
			tptio = (UM_coord *) UU_LIST_ARRAY (ptsio);
			lstpts = (UM_coord *) UU_LIST_ARRAY (&pmill_pts);
			nvecs = (UM_vector *) UU_LIST_ARRAY (nvsio);
			ncl_revers1_list (npts,0,tptio,1);
			ncl_revers1_list (npts,0,lstpts,1);
			ncl_revers1_list (npts,0,nvecs,1);
		}
		pmill_init = UU_FALSE;
/*
.....Make sure first normal vector points toward center of contour
*/
		tptio = (UM_coord *) UU_LIST_ARRAY (ptsio);
		lstpts = (UM_coord *) UU_LIST_ARRAY (&pmill_pts);
		nvecs = (UM_vector *) UU_LIST_ARRAY (nvsio);
		UU_LIST_EMPTY(&nvlst);
		dmin = 1.e12;
/*
.....Make sure slice points start as close as possible to previous
.....slice's start
*/
		for (i=0; i<npts; i++)
		{
			d1 = um_sqdis(lstpts[i],spt);
			if (d1 < dmin)
			{
				ind = i;
				dmin = d1;
			}
		}
/*
.....If the closest end point is not as close as a point on
.....one of the two neighboring segments of the closest end point,
.....then the point on the segment should be used as the start point.
.....The point on the segment will be added to the contour.
*/
		uu_list_init(&tpts,sizeof(UM_coord),npts,npts);
		npt_ind = um_mod(ind+1,npts);
		um_vcmnvc(lstpts[npt_ind],lstpts[ind],fvec); d4 = UM_MAG(fvec);
		um_unitvc(fvec,fvec);
		in1 = um_nptsg1(spt,lstpts[ind],fvec,d4,npt,&d1);
		npt_ind = um_mod(ind-1,npts);
		um_vcmnvc(lstpts[npt_ind],lstpts[ind],fvec); d4 = UM_MAG(fvec);
		um_unitvc(fvec,fvec);
		in2 = um_nptsg1(spt,lstpts[ind],fvec,d4,npt2,&d2);
		npt_ind = -1;
/*
.....Determine if closer point on segment exists
*/
		if (in2 == 1 && d2 < d1)
		{
			um_vctovc(npt2,npt);
			npt_ind = um_mod(ind-1,npts);
		}
		else if (in1 == 1 && d1 < d2)
		{
			npt_ind = um_mod(ind+1,npts);
/*
.....Increase ind so the new start and end point can be inserted
.....between the original start point and the second point.
*/
			ind = npt_ind;
		}
		else
			um_vctovc(lstpts[ind],spt);
/*
.....Set up near point data to be added later
*/
		if (npt_ind > -1)
		{
			um_vcplvc(nvecs[npt_ind],nvecs[ind],npt_nvec);
			um_unitvc(npt_nvec,npt_nvec);
			um_vctovc(npt,spt);
		}
		if (ind > 0)
		{
			uu_list_init(&tpts,sizeof(UM_coord),npts,npts);
/*
.....Add contour points
*/
			for (i=0; i<npts; i++)
			{
				j = um_mod(i+ind,npts);
				uu_list_push(&tpts,&lstpts[j]);
			}
			UU_LIST_EMPTY(&pmill_pts);
			uu_list_push_list(&pmill_pts,&tpts);
			uu_list_free(&tpts);
			lstpts = (UM_coord *) UU_LIST_ARRAY (&pmill_pts);
		}
/*
.....The start point is now the last point in the list so just placing
.....the new start at both ends of the list will suffice.
*/
		if (npt_ind > -1)
		{
			uu_list_insert(&pmill_pts,0,&npt);
			uu_list_push(&pmill_pts,&npt);
			lstpts = (UM_coord *) UU_LIST_ARRAY (&pmill_pts);
			npts += 2;
		}
/*
.....Find normal vector for offset point.  The vector is found by finding
.....the point that is the best match for being the one that was offset to
.....create the new point
*/
		for (i=0; i<npts; i++)
		{
			dmin = 1.e12;
			if (npt_ind > -1 && (i == 0 || i == npts-1))
			{
				uu_list_push(&nvlst,&npt_nvec);
				continue;
			}
			for (j=0; j<pnpts; j++)
			{
				d1 = fabs(um_dcccc(tptio[j],lstpts[i]) - (crad+frad+thk));
				if (d1 < dmin)
				{
					ind = j;
					dmin = d1;
				}
			}
#if 0
	sprintf(sbuf,"LN/%lf,%lf,%lf,%lf,%lf,%lf",lstpts[i][0],lstpts[i][1],
		lstpts[i][2],tptio[ind][0],tptio[ind][1],tptio[ind][2]);
	NclxDbgPstr(sbuf);
#endif
			uu_list_push(&nvlst,&nvecs[ind]);
		}
#if 0
	ncl_debug_pts(ptsio,0);
#endif
		UU_LIST_EMPTY(ptsio);
		uu_list_push_list(ptsio,&pmill_pts);
/*
.....Make sure the loop is closed
*/
		nvecs = (UM_vector *) UU_LIST_ARRAY (&nvlst);
		if (npt_ind < 0)
		{
			if (um_sqdis(lstpts[0],lstpts[npts-1]) > eps)
			{
				uu_list_push(ptsio,&lstpts[0]);
				uu_list_push(&nvlst,&nvecs[0]);
				npts++;
			}
		}
		else
		{
			um_vcplvc(nvecs[1],nvecs[npts-2],npt_nvec);
#if 0
	sprintf(sbuf,"PV/%lf,%lf,%lf,%lf,%lf,%lf",npt[0],npt[1],npt[2],
		npt_nvec[0],npt_nvec[1],npt_nvec[2]);
	NclxDbgPstr(sbuf);
#endif
			um_unitvc(npt_nvec,npt_nvec);
			um_vctovc(npt_nvec,nvecs[0]);
			um_vctovc(npt_nvec,nvecs[npts-1]);
		}
		*nptsio = npts;
#if 0
//	ncl_debug_pts(ptsio,0);
	lstpts = (UM_coord *) UU_LIST_ARRAY (ptsio);
	sprintf (sbuf,"PT/%lf,%lf,%lf",lstpts[0][0],lstpts[0][1],lstpts[0][2]);
	NclxDbgPstr(sbuf);
#endif
		UU_LIST_EMPTY(nvsio);
		uu_list_push_list(nvsio,&nvlst);
#if 0
	ncl_debug_pts(ptsio,0);

	lstpts = (UM_coord *) UU_LIST_ARRAY (ptsio);
	nvecs = (UM_vector *) UU_LIST_ARRAY (nvsio);
	for (i=0; i<npts; i++)
	{
		sprintf(sbuf,"PV/%lf,%lf,%lf,%lf,%lf,%lf",lstpts[i][0],
			lstpts[i][1],lstpts[i][2],nvecs[i][0],nvecs[i][1],nvecs[i][2]);
		NclxDbgPstr(sbuf);
	}

#endif
	}
done:
	uu_list_free(&ptlst);
	uu_list_free(&nvlst);
	uu_list_free(&nplst);
	uu_list_free(&pmill_pts);
	return UU_SUCCESS;
}

#if 0
/*********************************************************************
** FUNCTION: ncl_pmill_plunge_getpts(slices,cvecs,npaths,nvecs,step,wtol)
**   Build list of plunge slice points
**
**.....This routine is not currently used, but it was left as is in case
**.....plunge style milling is added and any remaining code is wanted
**
**
** PARAMETERS
**    INPUT:
**        slices - 
**        cvecs  - 
**        npaths - 
**        nvecs  - 
**        step   - Maximum distance between tool paths
**        wtol   - Tolerance
**    OUTPUT:
**            pt  -   evaluated point
** RETURNS: UU_SUCCESS
**
** SIDE EFFECTS: none
** WARNINGS    : none
*********************************************************************/
void ncl_pmill_plunge_getpts(slices,cvecs,npaths,nvecs,step,wtol)
UU_LIST *slices,*cvecs;
int *npaths;
UU_REAL step,wtol;
UM_vector *nvecs;
{
	int i,j,np,npts,ind,ind1,ind2,newnpath,npath = *npaths;
	UU_REAL len,maxlen,u,du,tol=wtol*wtol,um_getpolylen();
	UM_coord *pts,*pts1,*pts2,pt,npt,rpt,*tpts,ppt;
	UM_vector *cvec,*cvec2,nvec,plvec,*tvec,dvec1,dvec2,pve;
	UU_LIST newpts,newcvecs,ptlst,veclst,*slist,*clist,*nslist,*nclist;
	char sbuf[80];
/*
.....Find longest slice so we can find du to use for all slices
.....when getting points for plunge milling
*/
	slist = (UU_LIST *) UU_LIST_ARRAY(slices);
	clist = (UU_LIST *) UU_LIST_ARRAY(cvecs);
	maxlen = -1.e12;
	for (i=0;i<npath;i++)
	{
		np = UU_LIST_LENGTH (&slist[i]);
		pts = (UM_coord *) UU_LIST_ARRAY (&slist[i]);
		len = um_getpolylen(np, pts);
#if 0
	sprintf(sbuf,"$$ DU = %lf",du[i]);
	NclxDbgPstr(sbuf);
#endif
		if (len > maxlen)
		{
			ind = i;
			maxlen = len;
		}
	}
/*
.....Calculate du based on longest slice
*/
	
	newnpath = ceil(maxlen/step) + 1;
	du = 1./newnpath;
/*
.....Generate new slices using du to get a point on each slice
.....which will build the plunge slice
*/
	u = 0.;
	uu_list_init (&newpts,sizeof(UU_LIST),newnpath,newnpath);
	uu_list_init (&newcvecs,sizeof(UU_LIST),newnpath,newnpath);
	for (i=0;i<newnpath;i++)
	{
		npts = 0;
		uu_list_init(&ptlst,sizeof(UM_coord),npath,npath);
		uu_list_init(&veclst,sizeof(UM_vector),npath,npath);
/*
.....Get points from each slice
*/
		for (j=0;j<npath;j++)
		{
			pts = (UM_coord *) UU_LIST_ARRAY (&slist[j]);
			cvec = (UM_vector *) UU_LIST_ARRAY (&clist[j]);
			np = UU_LIST_LENGTH (&slist[j]);
			ncl_pmill_evolve_polyline (u,np,pts,pt,&ind);
			ind2 = um_mod(ind+1,np);
			um_vcplvc(cvec[ind],cvec[ind2],nvec); um_unitvc(nvec,nvec);
			uu_list_push(&ptlst,&pt);
			uu_list_push(&veclst,&nvec);
			npts++;
		}
/*
.....Weed out unnecessary points
*/
/*
		pts2 = (UM_coord *) UU_LIST_ARRAY(&ptlst);
		for (j=0;j<npts-2;j++)
		{
			if (um_points_within_tol(pts2[j],pts2[j+1],pts2[j+2],5.*tol))
			{
				uu_list_delete(&ptlst,j+1,1);
				uu_list_delete(&veclst,j+1,1);
				pts2 = (UM_coord *) UU_LIST_ARRAY(&ptlst);
				j--; npts--;
			}
		}
*/
#if 0
	ncl_debug_pts(&ptlst,0);
	if (i == 0)// || i == newnpath)
	{
		sprintf(sbuf,"$$ U = %lf",u);
		NclxDbgPstr(sbuf);
		tpts = (UM_coord *) UU_LIST_ARRAY(&ptlst);
		tvec = (UM_vector *) UU_LIST_ARRAY(&veclst);
		for (j=0;j<npts;j++)
		{
			sprintf(sbuf,"PV/%lf,%lf,%lf,%lf,%lf,%lf",tpts[j][0],tpts[j][1],
				tpts[j][2],tvec[j][0],tvec[j][1],tvec[j][2]);
			NclxDbgPstr(sbuf);
		}
	}
#endif
		uu_list_push(&newpts,&ptlst);
		uu_list_push(&newcvecs,&veclst);
//		uu_list_free(&ptlst);
//		uu_list_free(&veclst);
		u += du;
	}
/*
.....Replace old slices with new paths
*/
	for (i=0;i<npath;i++)
	{
		uu_list_free(&slist[i]);
		uu_list_free(&clist[i]);
	}
	uu_list_free(slices);
	uu_list_free(cvecs);
	uu_list_init(slices,sizeof(UU_LIST),newnpath,newnpath);
	uu_list_init(cvecs,sizeof(UU_LIST),newnpath,newnpath);
	nslist = (UU_LIST *) UU_LIST_ARRAY(&newpts);
	nclist = (UU_LIST *) UU_LIST_ARRAY(&newcvecs);
	for (i=0;i<newnpath;i++)
	{
		uu_list_push(slices,&nslist[i]);
		uu_list_push(cvecs,&nclist[i]);
	}
	slist = (UU_LIST *) UU_LIST_ARRAY(slices);
	clist = (UU_LIST *) UU_LIST_ARRAY(cvecs);
	for (i=0;i<newnpath;i++)
	{
		ind1 = um_mod(i-1,newnpath);
		ind2 = um_mod(i+1,newnpath);
		pts = (UM_coord *) UU_LIST_ARRAY (&slist[i]);
#if 0
/*
.....Generate slicing plane using path ends and middle point.  This
.....appraoch was used to test different ideas for creating plunge
.....tool paths
*/
	um_vcmnvc(pts[0],pts[npath/2],dvec1); um_unitvc(dvec1,dvec1);
	um_vcmnvc(pts[npath-1],pts[npath/2],dvec2); um_unitvc(dvec2,dvec2);
	um_cross(dvec1,dvec2,nvec); um_unitvc(nvec,nvec);
	sprintf (sbuf,"PL/(PV/%lf,%lf,%lf,%lf,%lf,%lf)",pts[npath/2][0],
		pts[npath/2][1],pts[npath/2][2],nvec[0],nvec[1],nvec[2]);
	NclxDbgPstr(sbuf);
#endif
		cvec = (UM_vector *) UU_LIST_ARRAY (&clist[i]);
		pts1 = (UM_coord *) UU_LIST_ARRAY (&slist[ind1]);
		pts2 = (UM_coord *) UU_LIST_ARRAY (&slist[ind2]);
/*
.....Generate normal vectors based on neighboring paths. This approach
.....should help to ensure the change in the normal vectors is less
.....erratic and the normal vectors should point in a desired direction
.......The code above that pushes the normal vector onto the list was kept
.......in case the following code is unwanted.
*/
		for (j=0;j<npath;j++)
		{
			if (j < npath-1)
			{
				um_vcmnvc(pts1[j+1],pts[j],dvec1);
				um_unitvc(dvec1,dvec1);
				um_vcmnvc(pts2[j+1],pts[j],dvec2);
				um_unitvc(dvec2,dvec2);
				um_cross(dvec2,dvec1,nvec);
				um_unitvc(nvec,nvec);
				if ((j == 0 && UM_DOT(nvec,cvec[j]) < 0.) ||
					(j > 0 && UM_DOT(nvec,cvec[j-1]) < 0.))
					um_negvc(nvec,nvec);
			}
			else
			{
				um_vcmnvc(pts1[j-1],pts[j],dvec1);
				um_unitvc(dvec1,dvec1);
				um_vcmnvc(pts2[j-1],pts[j],dvec2);
				um_unitvc(dvec2,dvec2);
				um_cross(dvec1,dvec2,nvec);
				um_unitvc(nvec,nvec);
				if (UM_DOT(nvec,cvec[j-1]) < 0.)
					um_negvc(nvec,nvec);
			}
			um_vctovc(nvec,cvec[j]);
		}
#if 0
	ncl_debug_pts(&slist[i],0);
	if (i == 0)// || i == newnpath)
	{
		sprintf(sbuf,"$$ U = %lf",u);
		NclxDbgPstr(sbuf);
		for (j=0;j<npath;j++)
		{
			sprintf(sbuf,"PV/%lf,%lf,%lf,%lf,%lf,%lf",pts[j][0],pts[j][1],
				pts[j][2],cvec[j][0],cvec[j][1],cvec[j][2]);
			NclxDbgPstr(sbuf);
		}
	}
#endif
	}
	*npaths = newnpath;

	uu_list_free (&newpts);
	uu_list_free (&newcvecs);
}
#endif
/*********************************************************************
** FUNCTION: void ncl_pmill_spiral_shift (points,npts,plpt,nvec)
**    Shift slice points between slicing planes to simulate a spiral
**    for the spiral type tool path motion.
**
** PARAMETERS
**    INPUT:
**        points - List of slice points
**        npts   - Number of points in the list
**        plpt   - Plane point for next slice plane
**        nvec   - Normal vector for next slice plane
**    OUTPUT:
**        points - Modified list of points
** RETURNS: UU_SUCCESS
**
** SIDE EFFECTS: none
** WARNINGS    : none
*********************************************************************/
void ncl_pmill_spiral_shift (points,npts,plpt,nvec)
UU_LIST *points;
int npts;
UM_coord plpt;
UM_vector nvec;
{
	int i,j;
	UM_coord npt,*pts;
	UM_vector dir;
	UU_REAL ddis,delt;
	char sbuf[80];
/*
.....Offset each point by an equalpercentage of the total offset distance.
*/
	ddis = delt = 1./((UU_REAL)npts-1);
	pts = (UM_coord *) UU_LIST_ARRAY(points);
	for (i=1;i<npts;i++)
	{
		um_nptpln(pts[i],plpt,nvec,npt);
		um_vcmnvc(npt,pts[i],dir);
		for (j=0;j<3;j++) pts[i][j] += ddis*dir[j];
		ddis += delt;
	}
#if 0
for (i=0;i<npts-1;i++)
{
	sprintf(sbuf,"LN/%lf,%lf,%lf,%lf,%lf,%lf",pts[i][0],pts[i][1],
			pts[i][2],pts[i+1][0],pts[i+1][1],pts[i+1][2]);
	NclxDbgPstr(sbuf);
}
#endif
}

/*********************************************************************
** FUNCTION: int ncl_pmill_evolve_polyline (t,np,pts,pt,ind)
**  SAME AS um_evolve_polyline.  It is currently unused so I added this
**  routine as a temporary fix.  Added index parameter so it can be used
**  to get the normal vector.
** Evaluates polyline point at given paremeter t
**
** PARAMETERS
**    INPUT:
**           t - curve parameter ( <1, >0)
**           np - # of polyline points
**           pts - polyline points
**    OUTPUT:
**           pt  -   evaluated point
**           ind - Index of segment the point is on
** RETURNS: UU_SUCCESS
**
** SIDE EFFECTS: none
** WARNINGS    : none
*********************************************************************/
int ncl_pmill_evolve_polyline (t,np,pts,pt,ind)
UM_real8 t;
int np,*ind;
UM_coord *pts,pt;
{
   int i,j;
   UM_real8 a,b,d,t1,t0,len;
	UU_REAL um_getpolylen();

   len = um_getpolylen (np,pts);

   for (i=0, t1 = 0.; i < np-1 && t >= t1; i++)
   {
      t0 = t1;
      t1 += um_dcccc(pts[i],pts[i+1])/len;
   }
	*ind = i-1;
   len = t1 - t0;
   if (len == 0.) return  (UU_FAILURE);
   a = (t1-t)/len; b = (t-t0)/len;

   for (j=0;j<3;j++) pt[j] = a*pts[i-1][j] + b*pts[i][j];

   return (UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION : ncl_pmill_stepover_toolpath(sff,sfnum,trad,frad,thk,zmin,
**			  nend,ntype,spt1,spt2,xystep,npaths,vdir,slvec,points,bndpts,bbox,
**         toler,wtol,poly1,poly2,parafl,plcross,sdir)
**       pmill stepver/number toolpath routine.
**       INPUT  :
**			sff        - Array of surface data for all surfaces defining part
**			sfnum      - number of surfaces in sff
**			trad       - tool corner radius
**			frad       - tool flat radius
**			thk        - thick value
**			zmin       - zmin value
**			nend       - CL end condition 0:TO 1:PAST 2:ON 3:CONTCT
**			ntype      - 0: SCRUB 1:COMBIN 2:LACE
**       ntran      - Transition type used between passes
**                    0:SPIRAL 1:ARC 2:LINEAR
**       atrad      - Arc transition radius
**			spt1,spt2  - slice start point and end point
**			xystep     - stepover distance
**			npaths     - number of passes
**			vdir       - slice plane normal
**			slvec      - slice vector
**			points     - 2d boundary contour
**			bndpts     - extended 2d boudnary contour for PAST
**			bbox       - minmax bounding box
**			toler,wtol -  tolerance
**       sdir       - 0:CCLW direction 1: CLW direction
**    RETURNS      : UU_SUCCESS / UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_pmill_stepover_toolpath(sff,sfnum,trad,frad,thk,nend,ntype,ntran,
	atrad,spt,spt1,spt2,xystep,vdir,slvec,toler,wtol,poly1,poly2,parafl,
	plcross,sdir)
NCL_waterline_surf *sff;
int sfnum;
UU_REAL trad,frad,thk,toler,atrad;
int nend,ntype,ntran,sdir;
UM_coord spt,spt1,spt2;
UM_vector vdir,slvec,plcross;
UU_REAL xystep,toler,wtol;
UU_LIST *poly1,*poly2;
UU_LOGICAL parafl;
{
	int i,npts,npts2,npaths,np1,np2,ind;
	UM_coord slpt,slpt1,tspt,slpt2,*pts1,*pts2,ppt1,ppt2,*spts;
	UM_vector tvec,nvec,tvdir,vdir2,*nvecs,rvec;
	NCL_waterline_surf *sfi;
	UU_REAL len1,len2,u,du,tu,tthk,um_getpolylen();
	UM_transf tfmat1,tfmat2;
	UU_LOGICAL slopefl,spiralfl;
	UU_LIST cpoints,cvecs;
/*
.....Not used outide of plunge milling which is not currently supported
*/
	UU_LIST slices,cveclst,*slice,*cvec,sptlst,nveclst;
	char sbuf[80];

	um_vctovc(vdir,tvdir);
	pmill_flag = UU_TRUE;
/*
.....slopefl enables non-parallel planes
*/
	slopefl = UU_TRUE;
	spiralfl = (ntran == 0);
	uu_list_init(&pmill_triangles,sizeof(UM_trian),500,500);
	sfi = sff;
	ncl_sm_clpath_new(Sslpt_flag);
	for (i=0;i<sfnum;i++,sfi++)
		uu_list_push_list(&pmill_triangles,sfi->trianlist);
/*
.....Set up data for slicing planes
*/
	du = xystep;
	if (!parafl)
	{
		np1 = UU_LIST_LENGTH (poly1);
		np2 = UU_LIST_LENGTH (poly2);
		pts1 = (UM_coord *) UU_LIST_ARRAY (poly1);
		pts2 = (UM_coord *) UU_LIST_ARRAY (poly2);
		len1 = um_getpolylen(np1, pts1);
		len2 = um_getpolylen(np2, pts2);
		if (len1 > len2) npaths = ceil(len1/du)+1;
		else npaths = ceil(len2/du)+1;
		du = 1./((UU_REAL)npaths);
	}
	else
		npaths = um_dcccc(spt1,spt2) / du + 1;
	if (spiralfl) npaths+=2;
/*
.....Build slice list
*/
	um_vctovc(spt,tspt);
	um_vctovc(spt1,slpt1);
	tthk = 0.; u = 0.;
/*
.....The slicing was done all in advanced to allow for later optimization
.....and possibly parallelization.
*/
	uu_list_init (&slices,sizeof(UU_LIST),npaths,npaths);
	uu_list_init (&cveclst,sizeof(UU_LIST),npaths,npaths);
	uu_list_init (&sptlst,sizeof(UM_coord),npaths,npaths);
	uu_list_init (&nveclst,sizeof(UM_vector),npaths,npaths);
	for (i = 0; i < npaths; i++)
	{
/*
.....When using spiral style motion the first and last pass will be a complete
.....loop so the spiral starts and ends with full passes around.  Therefore,
.....do not move the slice on the second or second to last passes.
*/
		if (parafl)
		{
			if (i > 0 && (!spiralfl || (spiralfl && i != 1 && i != npaths-2)))
			{
				if (i == 0)
					um_translate_point(slpt1,toler,tvdir,slpt);
				else	
					um_translate_point(slpt,du,tvdir,slpt);
			}
		}
		else 
		{
			if (!spiralfl || (spiralfl && (i == 0 || i == npaths-1)))
			{
				ncl_pmill_evolve_polyline (u,np1,pts1,ppt1,&ind);
				ncl_pmill_evolve_polyline (u,np2,pts2,ppt2,&ind);
				if (!spiralfl) u += du;
				um_middlept(ppt1,ppt2,slpt);
				um_vcmnvc(ppt1,ppt2,tvec); um_unitvc(tvec,tvec);
				um_cross(tvec,plcross,nvec); um_unitvc(nvec,nvec);
				if (UM_DOT(nvec,tvdir) < 0.) um_negvc(nvec,nvec);
				um_vctovc(nvec,tvdir);
			}
		}
/*
.....Generate second slice plane to use when offsetting slice points
.....toward next slice.
*/
		if (spiralfl && i > 0 && i < npaths-1)
		{
			if (!parafl)
			{
				um_middlept(ppt1,ppt2,slpt);
				um_vcmnvc(ppt1,ppt2,tvec); um_unitvc(tvec,tvec);
				um_cross(tvec,plcross,nvec); um_unitvc(nvec,nvec);
				if (UM_DOT(nvec,tvdir) < 0.) um_negvc(nvec,nvec);
				um_vctovc(nvec,tvdir);
				u += du;
				ncl_pmill_evolve_polyline (u,np1,pts1,ppt1,&ind);
				ncl_pmill_evolve_polyline (u,np2,pts2,ppt2,&ind);
				um_middlept(ppt1,ppt2,slpt2);
				um_vcmnvc(ppt1,ppt2,tvec); um_unitvc(tvec,tvec);
				um_cross(tvec,plcross,nvec); um_unitvc(nvec,nvec);
				if (UM_DOT(nvec,tvdir) < 0.) um_negvc(nvec,nvec);
				um_vctovc(nvec,vdir2);
			}
			else
			{
				um_translate_point(slpt,du,tvdir,slpt2);
				um_vctovc(tvdir,vdir2);
			}
		}
/*
.....Store plane points and normal vectors.
*/
#if 0
	sprintf(sbuf,"PL/(PV/%lf,%lf,%lf,%lf,%lf,%lf)",slpt[0],slpt[1],slpt[2],
		tvdir[0],tvdir[1],tvdir[2]);
	NclxDbgPstr(sbuf);
#endif
		uu_list_push(&nveclst,&tvdir);
		uu_list_push(&sptlst,&slpt);
/*
.....Slice tessellation with plane to get cl points
*/
		uu_list_init (&cpoints, sizeof(UM_coord), 100, 100);
		uu_list_init (&cvecs, sizeof(UM_coord), 100, 100);
		ncl_pmill_slice_tess(slpt,tvdir,tspt,trad,frad,tthk,toler,&cpoints,
			&cvecs,&npts,sdir);
		if (spiralfl && i > 0 && i < npaths-1)
			ncl_pmill_spiral_shift(&cpoints,npts,slpt2,vdir2);
/*
.....Store slice points so they can be projected later
*/
		uu_list_push(&slices,&cpoints);
		uu_list_push(&cveclst,&cvecs);
	}
/*
.....Project slices to get tool path
*/
	spts = (UM_coord *) UU_LIST_ARRAY (&sptlst);
	nvecs = (UM_vector *) UU_LIST_ARRAY (&nveclst);
	slice = (UU_LIST *) UU_LIST_ARRAY (&slices);
	cvec = (UU_LIST *) UU_LIST_ARRAY (&cveclst);
	for (i=0;i<npaths;i++)
	{
/*
.....Project cl points to generate motion points
*/
		npts = UU_LIST_LENGTH (&slice[i]);
		ncl_pmill_project_slice_norm(&slice[i],&cvec[i],npts,tspt,spts[i],
			nvecs[i],trad,frad,thk,wtol,ntran,atrad,sdir);
		/*ncl_pmill_project_slice_norm(&slice[i],&cvec[i],npts,tspt,spts[i],
			nvecs[i],trad,frad,thk,wtol,ntran,atrad,sdir);*/
		uu_list_free(&slice[i]);
		uu_list_free(&cvec[i]);
	}
	uu_list_free(&slices);
	uu_list_free(&cveclst);
	uu_list_free(&sptlst);
	uu_list_free(&nveclst);
	uu_list_free(&pmill_triangles);
	pmill_flag = UU_FALSE;
	return UU_SUCCESS;
}

/*********************************************************************
**    E_FUNCTION     : void ncl_pmill(lnum,nsf4,ksf8,nend4,ndrv4,kpl8,
**                    nbnd4,kbnd8,nstep4,kpas8,kstp8,kspt8,ntype4,ier)
**       Fortran interface for the main pmill routine
**    PARAMETERS
**       INPUT  :
**          lnum   - layer number
**          nsf4   - surfaces number
**          ksf8   - surface data
**          ndrv4  - 1:drive vector 2:drive plane
**          kdpl8  - drive plane
**          nend4  - CL end condition 0: TO 1: PAST 2:ON 3:CONTCT
**          nbnd4  - 0:auto contour boundary 1:input boundary curve
**          kbnd8  - boundary curve
**          nstep4 - 0:scallob height 1:PASS number 2: Stepover
**          kpas8  - scallop height or pass number value
**          kstp8  - stepover value
**          kspt8  - START point
**          ntype4 - 0:SCRUB 1:COMBIN 2:LACE
**          ntran4 - Transition type used between passes
**                   0:SPIRAL 1:ARC 2:LINEAR
**          atrad8 - Arc transition radius
**          ndir4  - 0:CCLW direction 1:CLW direction
**       OUTPUT :
**          ier    - error (zero if none)
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_pmill(lnum,nsf4,ksf8,nend4,ndrv4,kpl8,nbnd4,kbnd8,
			   nstep4,kpas8,kstp8,kspt8,ntype4,ntran4,atrad8,ndir4,ier)
UM_int2 *lnum,*ier;
UM_int2 *nsf4,*nend4,*ndrv4,*nbnd4,*nstep4,*ntype4,*ntran4,*ndir4;
UM_real8 *ksf8,*kpl8,*kbnd8,*kpas8,*kstp8,*kspt8,*atrad8;
{
	int status,ifl,val;
/*
.....Initilize surface key list
*/
	uu_list_init(&sfky,sizeof(UU_KEY_ID),100,100);
/*
.....Get the surfaces/drive plane/vector/boundary,START point keys
*/
	ncl_smill_keys(lnum,nsf4,ksf8,ndrv4,kpl8,nbnd4,kbnd8,kspt8,ier);
	if (*ier != 0)
		return;
	
	status = ncl_pmill_create(nend4,ndrv4,nbnd4,nstep4,kpas8,kstp8,
		ntype4,ntran4,atrad8,ndir4);
	*ier = status;
/*
.....Free surface key list
*/	
	uu_list_free (&sfky);
/*
.....Reset ignore inner boundary flag
*/
	ifl = 394; val = 0; setifl(&ifl,&val);
}

/*********************************************************************
**    E_FUNCTION     : ncl_pmill_check_planes(dpl1,dpl2,points,plcross1,sfkey,ind11,ind12,ind21,ind22,ipt11,ipt12,ipt21,ipt22)
**       Intermediate pmill routine. Checks if both parts of contour intersect cutoff planes
**       INPUT  :
**			dpl1,dpl2	port cutoff plane structures
**			points		contour poitns
**			ind11,ind12,ind21,ind22,ipt11,ipt12,ipt21,ipt22	indices of intersection points and coordnates
**			
**    RETURNS      : 1 if not both cutoff planes intersect left and right parts of contour at different indices, 0 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_pmill_check_planes(dpl1,dpl2,points,pts,plcross1,sfkey,ind11,ind12,ind21,ind22,ipt11,ipt12,ipt21,ipt22)

struct NCL_nclpl_rec *dpl1,*dpl2;
UM_vector plcross1;
UU_KEY_ID *sfkey;
UU_LIST *points;
int *ind11,*ind12,*ind21,*ind22;
UM_coord *ipt11,*ipt12,*ipt21,*ipt22;
UM_coord *pts;
{
	UM_int2 irpt;
	UM_plane pln1,pln2;
	UM_plane c_pl1;
	UM_real8 ptx[12];
	UM_coord ipt;
	int nint;
	int i;
	UM_int2 ncvs, np, ier=0;

	vctovc(dpl2->nvec,c_pl1.n);

					
			vctovc(plcross1,&dpl2->nvec);

			sfplio1(sfkey,&dpl2->key,&irpt,ptx,&np,points,plcross1,&ier);
			vctovc(c_pl1.n,&dpl2->nvec);

/*
.....Find the two sides of the contour to keep.  The sides kept will
.....be the ones the planes intersect.  A curve will be defined for
.....each side kept using the points between the planes and the intersection
.....points.  The curves will be used when transitioning the start plane
.....to the end plane to make each slice.
*/
			pts = (UM_coord *) UU_LIST_ARRAY(points);

			
			um_vctovc(dpl1->pt,pln1.p0); um_vctovc(dpl1->nvec,pln1.n);
			um_vctovc(dpl2->pt,pln2.p0); um_vctovc(dpl2->nvec,pln2.n);
			for (i=0;i<points->cur_cnt-1;i++)
			{
				if (*ind11 >= 0 && *ind12 >= 0 && *ind21 >= 0 && *ind22 >= 0) break;
				nint = um_iSegPlane(pts[i+1],pts[i],pln1,ipt);
				if (nint > 0 && *ind11 < 0)
				{
					um_vctovc(ipt,*ipt11);
					*ind11 = i;
				}
				nint = um_iSegPlane(pts[i+1],pts[i],pln1,ipt);
				if (nint > 0 && *ind12 < 0 && um_dcccc(ipt,ipt11) > 0.1*UM_FUZZ/* && i==388*/)
				{
					um_vctovc(ipt,*ipt12);
					*ind12 = i;
				}
				
				nint = um_iSegPlane(pts[i+1],pts[i],pln2,ipt);
				if (nint > 0 && *ind21 < 0)
				{
					um_vctovc(ipt,*ipt21);
					*ind21 = i;
				}
				nint = um_iSegPlane(pts[i+1],pts[i],pln2,ipt);
				if (nint > 0 && *ind22 < 0 && um_dcccc(ipt,ipt21) > 0.1*UM_FUZZ /*&& i==314*/)
				{
					um_vctovc(ipt,*ipt22);
					*ind22 = i;
				}
			}
			if (*ind11 < 0 || *ind12 < 0 || *ind21 < 0 || *ind22 < 0 || *ind11==*ind12 ||*ind11==*ind21 ||*ind11==*ind22 ||*ind12==*ind21 ||*ind12==*ind22 ||*ind21==*ind22 ||
				fabs(*ind11-*ind12) < 0.1*fabs(*ind21-*ind22))
			/*if (*ind11 < 0 || *ind12 < 0 || *ind21 < 0 || *ind22 < 0 || *ind11==*ind12 ||*ind11==*ind21 ||*ind11==*ind22 ||*ind12==*ind21 ||*ind12==*ind22 ||*ind21==*ind22 )*/
			{
				uu_list_free(points);
				//vctovc(c_pl1.n,&dpl2->nvec);
				return 1;
			}
			return 0;
}

/*********************************************************************
**    E_FUNCTION     : ncl_pmill_create(nend,ndrv,nbnd,nstep,npas,nstp,
**                                      ntype,ntran)
**       Intermediate pmill routine.
**       INPUT  :
**			nend		CL end condition 0:TO 1:PAST 2:ON 3:CONTCT
**			ndrv		1: drive vector 2: drive planes
**			nbnd		0: auto contour boundary 2: input boundary curve
**			nstep		0:scallob height 1:PASS number 2: Stepover
**			npas		scallop height or pass number value
**			nstp		stepover value
**			ntype		0: SCRUB 1:COMBIN 2:LACE
**       ntran    Transition type used between passes
**                0:SPIRAL 1:ARC 2:LINEAR
**       atrad    Arc transition radius
**       ndir     0:CCLW direction 1: CLW direction
**    RETURNS      : UU_SUCCESS / UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int ncl_pmill_create(nend,ndrv,nbnd,nstep,npas,nstp,ntype,ntran,
	atrad,ndir)
UM_int2 *nend,*ndrv,*nbnd,*nstep,*ntype,*ntran,*ndir;
UM_real8 *npas,*nstp,*atrad;
{
	UM_int2 isub,idx;
	UM_real8 thk,tol8,tolsq,diam,toolpar,offdis;
	UM_real8 xystep,xystep_max,dstep,fstep,hscal,xystep0,ver;
	UM_vector nvec,nzvec,nxvec,nyvec,vdir,vdir1,slvec;
	UM_transf rot,tfmat,*tf,rtinv,plmx,plinv,srot,srotinv;
	int npts,i,status,npaths,np1,np2;
	UU_REAL len1,len2,toler,fmm,zmin,zmax,trad,frad;
	UU_REAL xmm[2],ymm[2];
	UM_coord spt,spt1,spt2,slpt,slpt1,spt1s,spt2s,*pts1,*pts2;
	UU_REAL bucketSize,erad;
	UM_2box bbox,bbox0;
	int mcsflg, irot, s_irot;

	//UM_int2 *ier=NULL;

	UM_real8 ptx[12];

	
	UU_LIST bndpts;
    UU_LIST *points = UU_NULL;
	UU_LIST *ptsbnd = UU_NULL;
	UU_LIST *points3d = UU_NULL;

	struct NCL_fixed_databag e1,e2;
	struct NCL_nclpl_rec pl,*dpl1,*dpl2;
	struct UM_point_rec dspt;
	struct NCL_curve_rec *dcrv;
	UU_KEY_ID key0,*sfkey;
	UU_LOGICAL plflg,parafl;

	UM_coord *tpts,ipt,ipt11,ipt12,ipt21,ipt22,*pts = NULL;
	int nint,ind,ptind,ind11,ind12,ind21,ind22,tnpts;
	UM_vector plcross,dvec,tvec, plcross1;
	UM_plane pln1,pln2;
	UM_plane c_pl1,c_pl2;
	UU_LIST poly1,poly2;

	UM_int2 irpt;
	UM_int2 ncvs, np, ier=0;

	UM_int4 pflg = 0;
	UM_int4 flag = 0;

	char tbuf[80];
	char sbuf[80];
	FILE* fp;

	idx = 169; getsc(&idx,&ver);
	mcsflg = 0;
	irot = 0;
	npts = 0;
	np=0;

	irpt = 0;
	um_identtf (rot);
	key0 = NULLKEY;


	//if (fp = fopen( "..\\contourPoints.txt", "a" )) // Open file for writing
	//{

	//	fclose(fp);
	//	remove( "..\\contourPoints.txt");
	//}
/*
.....Get unit
*/
	isub = 264; getifl(&isub,&mm);
/*
.....Get the tool data
*/
	getend (tool);
/*
.....Tool unit vector
*/
	nzvec[0] = 0.0; nzvec[1] = 0.0; nzvec[2] = 1.0;
	nxvec[0] = 1.0; nxvec[1] = 0.0; nxvec[2] = 0.0;
	nyvec[0] = 0.0; nyvec[1] = 1.0; nyvec[2] = 0.0;

	pl.key = NULLKEY;
	sfkey = (UU_KEY_ID *) UU_LIST_ARRAY(&sfky);
/*
.....Create slicing plane
*/
	status = ncl_create_wat_botpl(&pl);
	if (status != UU_SUCCESS) goto Err1;

	nvec[0] = tool[3]; nvec[1] = tool[4]; nvec[2] = tool[5];
/*
.....The tool axis does not need to be (0,0,1) in order for the
.....PMILL logic to work, so the surfaces do not need to be rotated.
	if (UM_DOT(nvec,nvec) < UM_FUZZ)
*/
	{
		nvec[0] = tool[3] = 0.; nvec[1] = tool[4] = 0.;
		nvec[2] = tool[5] = 1.;
	}

	ncl_define_rotmx (mm,nvec,rot,&mcsflg);

	plflg = ncl_newzaxis_rot (nvec,plmx,plinv);

	ncl_get_rotmx (&irot,rtinv);

	key0 = pl.key;
	status = ur_update_displayable (key0, UM_NEVERDISPLAYABLE);
	if (status != UU_SUCCESS) goto Err1;

	ncl_init_connect_lst(sfnum);
	sff = (NCL_waterline_surf *) uu_malloc (sfnum * sizeof (*sff));
	if (!sff) goto Err1;
/*
....Initial sfs xy box
*/
	sfs_xmmx = xmm; sfs_ymmx = ymm;
/* 
.....Tolerance
*/
	getsct(&tol8);
	wtol = tol8;
	wtolsq = wtol*wtol;
	toler = (mm == 1)? wtol/25.4: wtol;
	fmm = (mm == 1)? 25.4: 1;
	tolsq = toler * toler;
/*
.....Tool and thick
*/
	thk = 0.0;
	isub = 23; getsc(&isub,&thk);
	isub = 28; getsc(&isub,&diam);

	isub = 2; gettool (&isub ,&toolpar);
	trad = (toolpar + thk);
	frad = 0.5*diam - toolpar;
	if (frad<UM_FUZZ)
		frad = 0.0;
/*
.....Drive plane
*/
	if (*ndrv == 2)
	{
		e1.key = Sdpl[0];
		status = ncl_retrieve_data_fixed (&e1);
		ncl_smill_drive_planes(&e1,irot,rot,nzvec,&dpl1);

		e2.key = Sdpl[1];
		status = ncl_retrieve_data_fixed (&e2);
		ncl_smill_drive_planes(&e2,irot,rot,nzvec,&dpl2);
/*
.....Check if plane are parallel
*/
		parafl = UU_TRUE;
		if(!um_vcparall(dpl1->nvec, dpl2->nvec))
		{
			parafl = UU_FALSE;
			points = (UU_LIST* ) uu_malloc (sizeof(UU_LIST));
			uu_list_init(points,sizeof(UM_coord),400,400);
/*
.....Find stock contour along side view defined by plane perpendicular
.....to the two drive planes
*/
			um_identtf (srot);
			um_cross(dpl1->nvec,dpl2->nvec,plcross); 
			um_unitvc(plcross,plcross);
			vctovc(plcross,plcross1);
			ncl_define_rotmx (mm,plcross,srot,&mcsflg);
			ncl_get_rotmx (&s_irot,srotinv);
			//vctovc(dpl2->nvec,c_pl1.n);


			ind11 = ind12 = ind21 = ind22 = -1;
			/*plcross1[0] = 1.0;
			plcross1[1] = 0.0;
			plcross1[2] = 0.0;*/
			/*um_vcplvc(dpl1->nvec,dpl2->nvec,plcross1);
			um_unitvc(plcross1,plcross1);*/
			tnpts = ncl_pmill_check_planes(dpl1,dpl2,points,pts,plcross1,sfkey,&ind11,&ind12,&ind21,&ind22,&ipt11,&ipt12,&ipt21,&ipt22);

			if (tnpts==1)
			{
				if (points)
					uu_free((char*)points);
				points = (UU_LIST* ) uu_malloc (sizeof(UU_LIST));
				uu_list_init(points,sizeof(UM_coord),400,400);
				ind11 = ind12 = ind21 = ind22 = -1;
				plcross1[0] = 0.0;
				plcross1[1] = 1.0;
				plcross1[2] = 0.0;
				tnpts = ncl_pmill_check_planes(dpl1,dpl2,points,pts,plcross1,sfkey,&ind11,&ind12,&ind21,&ind22);
				if (tnpts==1)
				{
					if (points)
						uu_free((char*)points);
					points = (UU_LIST* ) uu_malloc (sizeof(UU_LIST));
					uu_list_init(points,sizeof(UM_coord),400,400);
					ind11 = ind12 = ind21 = ind22 = -1;
					plcross1[0] = 0.0;
					plcross1[1] = 0.0;
					plcross1[2] = 1.0;
					tnpts = ncl_pmill_check_planes(dpl1,dpl2,points,pts,plcross1,sfkey,&ind11,&ind12,&ind21,&ind22);
					if (tnpts==1)
					{
						if (points)
							uu_free((char*)points);
						points = (UU_LIST* ) uu_malloc (sizeof(UU_LIST));
						uu_list_init(points,sizeof(UM_coord),400,400);
						ind11 = ind12 = ind21 = ind22 = -1;
						plcross1[0] = 1.0;
						plcross1[1] = 1.0;
						plcross1[2] = 0.0;

						um_unitvc(plcross1,plcross1);
						tnpts = ncl_pmill_check_planes(dpl1,dpl2,points,pts,plcross1,sfkey,&ind11,&ind12,&ind21,&ind22,&ipt11,&ipt12,&ipt21,&ipt22);
						if (tnpts==1)
						{
							if (points)
								uu_free((char*)points);
							points = (UU_LIST* ) uu_malloc (sizeof(UU_LIST));
							uu_list_init(points,sizeof(UM_coord),400,400);
							ind11 = ind12 = ind21 = ind22 = -1;
							plcross1[0] = 0.0;
							plcross1[1] = 1.0;
							plcross1[2] = 1.0;
							um_unitvc(plcross1,plcross1);
							tnpts = ncl_pmill_check_planes(dpl1,dpl2,points,pts,plcross1,sfkey,&ind11,&ind12,&ind21,&ind22);
							if (tnpts==1)
							{
								if (points)
									uu_free((char*)points);
								points = (UU_LIST* ) uu_malloc (sizeof(UU_LIST));
								uu_list_init(points,sizeof(UM_coord),400,400);
								ind11 = ind12 = ind21 = ind22 = -1;
								plcross1[0] = 1.0;
								plcross1[1] = 0.0;
								plcross1[2] = 1.0;
								um_unitvc(plcross1,plcross1);
								tnpts = ncl_pmill_check_planes(dpl1,dpl2,points,pts,plcross1,sfkey,&ind11,&ind12,&ind21,&ind22);
								if (tnpts==1)
								{
									ier = 561;
									error(&ier);
									goto Err1;
								}

							}
						}
					}
				}
			}


			/*vctovc(c_pl1.n,&dpl2->nvec);*/


			pts = (UM_coord *) UU_LIST_ARRAY(points);
			um_isegseg (ipt11,ipt21,ipt12,ipt22,&nint,ipt,wtol);
			if (nint > 0)
			{
				i = ind21;
				ind21 = ind22;
				ind22 = i;
				um_vctovc(ipt21,ipt);
				um_vctovc(ipt22,ipt21);
				um_vctovc(ipt,ipt22);
			}

//#if 0
//	sprintf(tbuf,"LN/%lf,%lf,%lf,%lf,%lf,%lf",ipt11[0],ipt11[1],ipt11[2],
//		ipt21[0],ipt21[1],ipt21[2]);
//	NclxDbgPstr(tbuf);
//	sprintf(tbuf,"LN/%lf,%lf,%lf,%lf,%lf,%lf",ipt12[0],ipt12[1],ipt12[2],
//		ipt22[0],ipt22[1],ipt22[2]);
//	NclxDbgPstr(tbuf);
//#endif
/*
.....Build first curve along stock contour
*/
			um_vcmnvc(ipt21,ipt11,dvec); um_unitvc(dvec,dvec);
			i = (ind11 + 1)%points->cur_cnt;
			um_vcmnvc(pts[i],pts[ind11],tvec); um_unitvc(tvec,tvec);
			if (UM_DOT(tvec,dvec) >= 0.)
			{
				if (ind21 > ind11)
					tnpts = ind21 - ind11;
				else
					tnpts = points->cur_cnt + ind21 - ind11 + 1;
				uu_list_init(&poly1,sizeof(UM_coord),tnpts+2,tnpts+2);
				uu_list_push(&poly1,&ipt11);
				ind = ind11 + 1;
				for (i=0;i<tnpts;i++)
				{
					ptind = (i+ind)%points->cur_cnt;
					uu_list_push(&poly1,&(pts[ptind]));
				}
				uu_list_push(&poly1,&ipt21);
			}
			else
			{
				if (ind11 > ind21)
					tnpts = ind11 - ind21;
				else
					tnpts = points->cur_cnt + ind11 - ind21 + 1;
				uu_list_init(&poly1,sizeof(UM_coord),tnpts+2,tnpts+2);
				uu_list_push(&poly1,&ipt11);
				ind = ind11;
				for (i=0;i<tnpts;i++)
				{
					ptind = ind - i;
					if (ptind < 0) ptind += points->cur_cnt;
					uu_list_push(&poly1,&(pts[ptind]));
				}
				uu_list_push(&poly1,&ipt21);
			}
#if 0
	ncl_debug_pts(&poly1,0);
#endif
/*
.....Second curve
*/
			um_vcmnvc(ipt22,ipt12,dvec); um_unitvc(dvec,dvec);
			i = (ind12 + 1)%points->cur_cnt;
			um_vcmnvc(pts[i],pts[ind12],tvec); um_unitvc(tvec,tvec);
			if (UM_DOT(tvec,dvec) >= 0.)
			{
				if (ind22 >ind12)
					tnpts = ind22 - ind12;
				else
					tnpts = points->cur_cnt + ind22 - ind12 + 1;
				uu_list_init(&poly2,sizeof(UM_coord),tnpts+2,tnpts+2);
				uu_list_push(&poly2,&ipt12);
				ind = ind12 + 1;
				for (i=0;i<tnpts;i++)
				{
					ptind = (i+ind)%points->cur_cnt;
					uu_list_push(&poly2,&(pts[ptind]));
				}
				uu_list_push(&poly2,&ipt22);
			}
			else
			{
				if (ind12 > ind22)
					tnpts = ind12 - ind22;
				else
					tnpts = points->cur_cnt + ind12 - ind22 + 1;
				uu_list_init(&poly2,sizeof(UM_coord),tnpts+2,tnpts+2);
				uu_list_push(&poly2,&ipt12);
				ind =ind12;
				for (i=0;i<tnpts;i++)
				{
					ptind = ind - i;
					if (ptind < 0) ptind += points->cur_cnt;
					uu_list_push(&poly2,&(pts[ptind]));
				}
				uu_list_push(&poly2,&ipt22);
			}
#if 0
	ncl_debug_pts(&poly2,0);
#endif
		}
		else goto Err1;
	}
/*
.....START point
*/
	dspt.key = Sspt;
	if (dspt.key == 0)
	{
		spt[0] = tool[0]; spt[1] = tool[1]; spt[2] = tool[2];
	}
	else
	{
		um_get_all_geom(&dspt, sizeof(dspt));
		um_vctovc(dspt.pt,spt);
	}
/*
.....Tesselate surfaces
*/	
	status = ncl_tesselate_sfs (sfnum,sfkey,rot,&zmax,&zmin,sff,wtol,toler,
		sfs_xmmx,sfs_ymmx);	
	if (status != UU_SUCCESS) goto Err3;
	zmin -= toler + trad;
/*
.....Create surfaces tesselation buckets
*/
	bucketSize = diam;
	ncl_create_2box(sfs_xmmx,sfs_ymmx,0.5*diam,&bbox);
	ncl_sfbucket_create(sfnum,sff,bbox,bucketSize);
/*
.....Create 3d contour boundary for CONTCT
*/
	if (*nend == 3)
	{
		ncl_create_contour_boundary3d(points,toler,&points3d);
/*
.....Create 3d boundary bukcet
*/
		ncl_sgbucket_create(points3d,0.5*diam);
	}
/*
.....Initialize clpath
*/
	ncl_sm_clpath_new(Sslpt_flag);
/*
.....Get slice start point (spt1) and end point(spt2)
*/
	if (*ndrv == 2) 
	{
		um_vctovc(dpl1->pt,spt1);
		um_vctovc(dpl2->pt,spt2);
	}
	else if (*ndrv == 1)
	{
		um_boundbox_minmax(points,spt,slvec,1,spt1,spt2);
	}
/*
.....Scallop height,pass number and stepover
*/
	if (*nstep == 0)
	{
		hscal = *npas;
		xystep = trad*trad - (trad-hscal)*(trad-hscal);
/*
.....If hscal is more than twice trad, then xystep will be
.....less than 0.
*/
		if (xystep < 0.)
			xystep = 2*toler;
		else
		{
			xystep = sqrt(xystep) * 2.0;
			if (xystep < 2*toler) xystep = 2*toler;
		}
		xystep0 = xystep;
		npaths = um_dcccc(spt1,spt2) / xystep + 1;
/*
.....Bullnose tool
*/
		if (frad > UM_FUZZ)
			fstep = *nstp;
		else
			fstep = 0.0;
	}
	else if (*nstep == 1)
	{
		npaths = *npas-1;
		if (!parafl)
		{
			np1 = UU_LIST_LENGTH (&poly1);
			np2 = UU_LIST_LENGTH (&poly2);
			pts1 = (UM_coord *) UU_LIST_ARRAY (&poly1);
			pts2 = (UM_coord *) UU_LIST_ARRAY (&poly2);
			len1 = um_getpolylen(np1, pts1);
			len2 = um_getpolylen(np2, pts2);
			if (len1 > len2) xystep = len1/npaths;
			else xystep = len1/npaths;
		}
		else
			xystep = um_dcccc(spt1,spt2)/npaths;
	}
	else if (*nstep == 2 && *nstp > 0.0)
	{
		xystep = *nstp;
		npaths = um_dcccc(spt1,spt2) / xystep + 1;
	}
/*
.....Slice direction vector(slvec) and plane normal direction(vdir)
*/
	if (*ndrv == 2)
	{
		um_vcmnvc(dpl2->pt,dpl1->pt,vdir);
		um_unitvc(vdir,vdir);
		if (um_dot(vdir,dpl1->nvec) < 0.)
			um_vctmsc(dpl1->nvec,-1.,vdir);
		else
			um_vctovc(dpl1->nvec,vdir);
		um_unitvc(vdir,vdir);
		um_vcmnvc(spt2,spt1,vdir1);
		um_cross(dpl1->nvec,nzvec,slvec);
		if ((slvec[0]==0.0) && (slvec[1]==0.0) &&(slvec[2]==0.0))
			um_cross(dpl1->nvec,nxvec,slvec);
		//um_cross(dpl1->nvec,plcross1,slvec);
	}
	else if (*ndrv == 1)
	{
		um_cross(nzvec,slvec,vdir);
		um_vcmnvc(spt2,spt1,vdir1);
		if (um_dot(vdir,vdir1) < 0.)
			um_vctmsc(vdir,-1.,vdir);
	}
/*
.....Project the surfaces with plane normal slvec,
.....used to insert more slice steps for the scallop height
*/
	if (*nstep == 0)
	{
/*
......Get scallop slice bounding box between planes
*/
		if (*ndrv == 2)
		{
			um_boundbox_between_planes_minmax(points,
							dpl1,dpl2,spt,toler,spt1s,spt2s);
			um_vcmnvc(spt2s,spt1s,vdir1);
		}
		else
		{
			um_vctovc(spt1,spt1s);					
			um_vctovc(spt2,spt2s);
		}
/*
.....Calculate the slice clpath for scallop calculation
*/
		ncl_smill_scallop_slice(trad,frad,thk,zmin,vdir,slvec,
						vdir1,spt1s,spt2s,points,toler);
	}
/*
.....Maximum stepover for scallop height
*/
	xystep_max = xystep;
	um_vctovc(spt1,slpt1);
/*
.....Set sorting clpoint vector
*/
	ncl_set_ldir (slvec);
/*
.....Calculate toolpath
*/
	ncl_pmill_stepover_toolpath(sff,sfnum,trad,frad,thk,*nend,*ntype,*ntran,
			*atrad,spt,spt1,spt2,xystep,vdir,slvec,toler,wtol,&poly1,&poly2,
			parafl,plcross,*ndir);
/*
.....Transform the clpath if necessary
*/
	if (irot > 0 && plflg)		
		ncl_smill_transf_clpts(*ntype,irot,rtinv,mcsflg);

	if (status != UU_SUCCESS)
	{
		status = 36; goto Err1;
	}
	goto Done;
Err1:
	status = 561;
	goto failed;

Err2:
	status = 51;
	goto failed;

Err3:
	status = 530;

failed:
	if (ver > 10.002)
	{
		ncl_sm_clpath_remove_all(Sclpt_flag);
		ncl_sm_clpath_remove_all(Sslpt_flag);
	}

Done:
	if (status!=561)
	{
		if (sff)
		{
			int isf;
			int ch;
			NCL_waterline_surf *p1;
			p1 = sff;
			ch=0;
			for (isf = 0; isf < sfnum && p1->key != NULLKEY; isf++, p1++)
			{
				ncl_free_bound (&p1->bound);
				UU_LIST_FREE (p1->trianlist);
			}
			UU_FREE (sff);
		}
	}
	sfs_xmmx = sfs_ymmx = UU_NULL;
	ncl_free_uv();
	ncl_free_nios();
	ncl_free_aux_ptlist();
	ncl_free_tripolst();
	ncl_free_connect_lst();
	if (key0 > NULLKEY) uc_delete (key0);
	if (!parafl)
	{
		if (points->data)
			uu_list_free(points);
		if (status!=561)
		{
			uu_list_free(&poly1);
			uu_list_free(&poly2);
		}
	}

	if (*nbnd == 1)
		if (pptr.data) uu_list_free(&pptr);

	return (status);
}

/*********************************************************************
**    E_FUNCTION : nclf_pmill_npts(npte)
**       Get the number of points in the contour list defined by slicing
**       the tesselation with the slicing plane.
**       INPUT  : none
**       OUTPUT  :
**         npts - Number of points
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclf_pmill_npts(npts)
UM_int2 *npts;
{
	if (!pmill_init) *npts = 0;
	else *npts = UU_LIST_LENGTH (&pmill_pts);
}

/*********************************************************************
**    E_FUNCTION : nclf_pmill_getpt(ind,ptbuf)
**       Get the point in the contour list at the given index.
**       INPUT  :
**			  ind   - Position in the list to retrieve
**       OUTPUT  :
**         ptbuf - Point data retrieved
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclf_pmill_getpt(ind,ptbuf)
UM_int2 *ind;
UM_real8 *ptbuf;
{
	int i,index = *ind - 1;
	UM_coord *pts;

	if (pmill_init)
	{
		pts = (UM_coord *)UU_LIST_ARRAY(&pmill_pts);
		for (i=0; i<3; i++) ptbuf[i] = pts[index][i];
	}
}

/*********************************************************************
**    E_FUNCTION : nclf_pmill_resetpts()
**        Empty the list of points.  The points in the list will be
**        replaced with the result of offsetting the original points
**        in the list.
**       INPUT  : none
**       OUTPUT  : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclf_pmill_resetpts()
{
	if (!pmill_init) return;
	UU_LIST_EMPTY (&pmill_pts);
}

/*********************************************************************
**    E_FUNCTION : nclf_pmill_push_pt(x,y)
**       Pushes an offset slice point onto the point list.
**       INPUT  :
**			  x,y - Coordinates of offset point
**       OUTPUT  : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclf_pmill_push_pt(x,y)
UM_real4 *x,*y;
{
	UM_coord pt;

	if (pmill_init)
	{
		pt[0] = *x; pt[1] = *y; pt[2] = 0.;
		uu_list_push(&pmill_pts,&pt);
	}
}

//
//int both_inter(pts, dv1, dv2, ps, crs, key, in11, in12, in21,in22,ipv1,ipv2,ipv3,ipv4)
//UU_LIST* pts;
//struct NCL_nclpl_rec *dv1, *dv2;
//UM_coord* ps;
//UM_vector crs;
//unsigned int *key;
//int *in11, *in12, *in21, *in22;
//UM_coord *ipv1, *ipv2, *ipv3, *ipv4;
//{
//	int tnpts;
//	tnpts=0;
//
//	if (pts)
//		uu_free((char*)pts);
//	pts = (UU_LIST* ) uu_malloc (sizeof(UU_LIST));
//	uu_list_init(pts,sizeof(UM_coord),400,400);
//	in11 = in12 = in21 = in22 = -1;
//	//crs[0] = 0.0;
//	//crs[1] = 1.0;
//	//crs[2] = 1.0;
//	um_unitvc(crs,crs);
//	tnpts = ncl_pmill_check_planes(dv1,dv2,pts,ps,crs,key,&in11,&in12,&in21,&in22,&ipv1, &ipv2, &ipv3, &ipv4);
//	return tnpts;
//}
