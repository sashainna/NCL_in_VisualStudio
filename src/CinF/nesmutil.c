/*********************************************************************
**    NAME         :  nesmutil.c
**       CONTAINS:  Routines for surface-plane intersection using buckets
**
**		  ncl_nclpl_to_umplane()
**		  ncl_bound_plio()
**		  ncl_bound_segments_between()
**		  ncl_smill_scallop_slice()
**		  ncl_smill_scallop_step()
**		  ncl_bucket_boundplio()
**		  ncl_create_contour_boundary3d()
**		  ncl_point_on_bound3d()
**		  ncl_point_inside_bound2d()
**		  ncl_smill_clpath_between()
**		  ncl_smill_connect_clpath()
**		  ncl_smill_toolpath_combin()
**		  ncl_bound_contour_intf()
**
**    COPYRIGHT 2010 (c) Numerical Control Computer Sciences Inc.
**                          All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       nesmutil.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:50
*********************************************************************/
#include "nclfc.h"
#include "mdattr.h"
#include "mattr.h"
#include "uminmax.h"
#include "modef.h"
#include "mdcoord.h"
#include "mgeom.h"
#include "nclwaterln.h"
#include "nclclip.h"

extern int Sslpt_flag;	/*Sslpath flag*/
extern int Sclpt_flag;	/*Sclpath flag*/
extern UU_LIST Serads;	/*bullnose effective radius list*/

/*********************************************************************
**    E_FUNCTION     : ncl_debug_clpath(nflag)
**       Debug clpath routine.
**    PARAMETERS
**       INPUT  :
**			nflag	- clpath flag(Sclpt_flag/Sslpt_flag)
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_debug_clpath(nflag)
int nflag;
{
	int i,j,npaths,npts;
	UU_LIST *clpts = UU_NULL;
	UM_clpt *pos;
	char tbuf[80];

	npaths = 0;
	ncl_sm_clpath_getnpath(&nflag,&npaths);
	for (i = 0; i < npaths; i++)
	{
		npts = ncl_sm_clpath_getclpts(nflag,i,&clpts);		
/*
		sprintf(tbuf,"npts = %d",npts); 
		NclxDbgPstr(tbuf);
*/
		if (npts > 0)
		{			
			pos = (UM_clpt *) UU_LIST_ARRAY (clpts);
			for (j = 0; j < npts; j++)
				ncl_debug_clpt(pos[j].pte,0);
		}
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_nclpl_to_umplane(pl,pln)
**        Convert NCL_nclpl_rec to UM_Plane .
**    PARAMETERS
**       INPUT  :
**          pl       - NCL_nclpl_rec
**       OUTPUT :
**          pln      - UM_Plane 
**    RETURNS      :
**         none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_nclpl_to_umplane(pl,pln)
struct NCL_nclpl_rec *pl;
UM_plane *pln;
{
	um_vctovc(pl->pt,pln->p0);
	um_vctovc(pl->nvec,pln->n);
}

/*********************************************************************
**    E_FUNCTION     : ncl_bound_plio (bound,pl,tol,inpts,ipts,ier)
**        create intersection of contour boundary with a plane.
**    PARAMETERS
**       INPUT  :
**			bound	- contour boundary
**          pl      - plane
**          tol     - tolerance
**       OUTPUT :
**			inpts   - numbers of intersection points
**			ipts	- intersection points
**          ier     - error number, if fail
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_bound_plio(bound,pl,tol,inpts,ipts,ier)
UU_LIST *bound;
struct NCL_nclpl_rec *pl;
UU_REAL tol;
int *inpts;
UM_coord ipts[];
UM_int2 *ier;
{
	int i,nint,npts;
	UM_coord *pts,pt;
	UM_plane plane;
	
	ncl_nclpl_to_umplane(pl,&plane);

	npts = bound->cur_cnt;
	pts = (UM_coord *) UU_LIST_ARRAY (bound);
#if 0
	ncl_draw_polyline (npts,pts,4,1);
#endif
	nint = 0;
	for (i = 0; i < npts-1; i++)
	{
		if (um_iSegPlane(pts[i],pts[i+1],plane,pt) > 0)
		{
			um_vctovc(pt,ipts[nint]);			
			++nint;
		}
#if 0
		ncl_debug_clpt2(pts[i],pts[i+1],0);
#endif
	}

	if (um_iSegPlane(pts[npts-1],pts[0],plane,pt) > 0)
	{
		um_vctovc(pt,ipts[nint]);			
		++nint;
	}

	*inpts = nint;
}

/*********************************************************************
**    E_FUNCTION     : ncl_bound_segments_between(bound,ps,pe,tol,bpts)
**        Get the boundary portionf rom ps to pe.
**    PARAMETERS
**       INPUT  :
**			bound	- contour boundary
**          ps,pe	- start/end point
**          tol     - tolerance
**       OUTPUT :
**			bpts	- points list from ps to pe
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_bound_segments_between(bound,ps,pe,tol,bpts)
UU_LIST *bound,*bpts;
UM_coord ps,pe;
UU_REAL tol;
{
	int i,nint,npts;
	UM_coord *pts,ps1,pe1;
	UU_LOGICAL lstart,lreverse;

	um_vctovc(ps,ps1);
	ps1[2] = 0.0;
	um_vctovc(pe,pe1);
	pe1[2] = 0.0;

	npts = bound->cur_cnt;
	pts = (UM_coord *) UU_LIST_ARRAY (bound);

	lstart = UU_FALSE;
	lreverse = UU_FALSE;
	for (i = 0; i < npts-1; i++)
	{
		if (!lstart)
		{
			if (um_points_within_tol(pts[i],ps1,pts[i+1],tol) &&
				um_point_in_segment_2d(ps,pts[i],pts[i+1],tol))
			{
				lstart = UU_TRUE;
				uu_list_push(bpts,ps);
				if (um_points_within_tol(pts[i],pe1,pts[i+1],tol) &&
					um_point_in_segment_2d(pe,pts[i],pts[i+1],tol))
				{
					uu_list_push(bpts,pe);
					break;
				}
			}
			else if (um_points_within_tol(pts[i],pe1,pts[i+1],tol) && 
					 um_point_in_segment_2d(pe1,pts[i],pts[i+1],tol))
			{
				lstart = UU_TRUE;
				lreverse = UU_TRUE;
				uu_list_push(bpts,pe);
				if (um_points_within_tol(pts[i],ps1,pts[i+1],tol) &&
					um_point_in_segment_2d(ps,pts[i],pts[i+1],tol))
				{
					uu_list_insert(bpts,0,ps);
					break;
				}
			}
		}
		else if (lstart)
		{		
			if (!lreverse)
			{
				if (um_points_within_tol(pts[i],pe1,pts[i+1],tol) &&			
					um_point_in_segment_2d(pe,pts[i],pts[i+1],tol))
				{
					uu_list_push(bpts,pe);
					break;
				}
				else if (um_point_in_segment_2d(pts[i+1],ps1,pe1,tol))
				{
					uu_list_push(bpts,pts[i+1]);
				}
			}
			else
			{
				if (um_points_within_tol(pts[i],ps1,pts[i+1],tol) &&
					um_point_in_segment_2d(ps,pts[i],pts[i+1],tol))
				{					
					uu_list_insert(bpts,0,ps);
					break;
				}
			}
		}
	}
}

/*********************************************************************
**    E_FUNCTION   : ncl_smill_project_slice(trad,frad,thk,zmin,index,
**										vdir,ipt1,ipt2,toler,clpts)
**       smill project slice routine.
**       INPUT  :
**			trad		- tool corner radius
**			frad		- tool flat radius
**			thk			- thick value
**			zmin		- zmin value
**			index		- sorting index
**			vdir		- slice direction vector
**			ipt1		- slice start point
**			ipt2		- slice end point
**			toler		- tolerance
**       OUTPUT :
**			clpts		- projected cl point list(sorted)
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_smill_project_slice(trad,frad,thk,zmin,index,vdir,ipt1,ipt2,toler,clpts)
UU_REAL trad,frad,thk,zmin,toler;
int index;
UM_coord ipt1,ipt2;
UM_vector vdir;
UU_LIST *clpts;
{
	int iret1,iret2,j,k, nsteps;	
	UU_REAL v;
	UM_coord ccpt1,clpt1,clpt2;
	UU_LOGICAL lcheck;
	char tbuf[80];

	nsteps = um_dcccc(ipt1,ipt2) / (trad + frad) * 2.0;
/*
.....Project points for each slice
*/		
	iret1 = UU_FAILURE;		
	iret2 = UU_FAILURE;		
	for (j = 0; j <= nsteps; j++)		
	{
		v = j * (trad + frad) * 0.5;
		um_translate_point(ipt1,v,vdir,clpt1);	
		if (j == nsteps)			
			um_vctovc(ipt2,clpt1);	
		clpt1[2] = zmin;
				
		if (j < 1 || j > nsteps-1)					
			lcheck = UU_FALSE;				
		else					
			lcheck = UU_TRUE;
	
		iret1 = ncl_projsf_clpoint(trad,frad,clpt1,ccpt1,thk,toler,lcheck,0,
			clpts,UU_NULL);	
		if (iret1 == UU_SUCCESS && iret2 == UU_SUCCESS)
			ncl_projsf_clpoint_middle(trad,frad,thk,zmin,
								5.0*toler,clpt1,clpt2,lcheck,0,clpts);
				
		if (iret1 == UU_SUCCESS)
		{
			iret2 = iret1;
			um_vctovc(clpt1,clpt2);
		}
	}
/*
.....Sort the clpts along the slice vector
*/
	ncl_sort_clpts(clpts,vdir,index,2,toler);

#if 0		
	sprintf(tbuf,"i = %d",index); 	
	NclxDbgPstr(tbuf);
	sprintf(tbuf,"*stop"); 	
	NclxDbgPstr(tbuf);

	ncl_debug_clpts(clpts,0);
#endif
}

/*********************************************************************
**    E_FUNCTION   : ncl_smill_scallop_slice(trad,frad,thk,zmin,vdir,slvec,
**							vdir1,spt1,spt2,points,toler)
**       Intermediate smill routine.
**       INPUT  :
**			trad		- tool radius
**			frad		- tool flat radius
**			thk			- thick value
**			zmin		- zmin value
**			vdir		- scallop slice vector
**			slvec		- slice vector
**			vdir1		- direction vector
**			spt1		- slice start point
**			spt2		- slice end point
**			points		- 2d contour boundary
**			toler		- tolerance
**       OUTPUT :
**			none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_smill_scallop_slice(trad,frad,thk,zmin,vdir,slvec,vdir1,
							 spt1,spt2,points,toler)
UU_REAL trad,frad,thk,zmin,toler;
UM_vector vdir,vdir1,slvec;
UM_coord spt1,spt2;
UU_LIST *points;
{	
	int i,inpts,iret1,iret2,j,k,nsteps,nsegs,nvpaths,npaths,status,status1;
	UM_int2 ierr;
	UM_real8 vstep;
	UU_REAL v,minclz;
	UM_coord ccpt1,clpt1,clpt2,slpt,ipts[20];
	UM_vector nvec,nzvec,slvec1;
	struct NCL_nclpl_rec pl;
	UU_LOGICAL lcheck;
	UU_LIST clpts;
	UM_segment seg,*psegs;
	UM_coord *pt1,*pt2;
	UU_LIST seglst;
	int cmp();
	char tbuf[80];
/*
.....The slice plane
*/
	pl.key = NULLKEY;
	pl.rel_num = NCL_PLN_REL;
	status = ncl_create_wat_botpl(&pl);
	
	lcheck = UU_TRUE;
	iret1 = -1;
	iret2 = -1;
/*
.....Set sorting clpoint vector
*/
	ncl_set_ldir (vdir);

	vstep = 0.05 * trad;
	nvpaths = um_dcccc(spt1,spt2) / vstep;
/*
.....Initialize clpath
*/	
	ncl_sm_clpath_new(Sslpt_flag);
		
	if (um_dot(slvec,vdir1) < 0.)
		um_vctmsc(slvec,-1.,slvec1);
	else
		um_vctovc(slvec,slvec1);
		
	uu_list_init (&seglst,sizeof(UM_segment),nvpaths,nvpaths);

	npaths = 0;
	for (i = 0; i < nvpaths; i++)
	{
		um_translate_point(spt1,i*vstep,slvec1,slpt);
		ncl_set_nclpln(&pl,slpt,slvec);
/*
.....Get the intersection points of plane with boundary
*/
		inpts = 0;
		ncl_bound_plio(points,&pl,toler,&inpts,ipts,&ierr);
		if (inpts < 2) continue;
/*
.....Sort intersection points
*/
		uu_qsort (ipts,inpts,sizeof(UM_coord),cmp);
/*
.....Slice segment
*/
		for (k = 0; k < inpts; k++)
		{
			um_vctovc(ipts[k],seg.p1);
     		um_vctovc(ipts[k+1],seg.p2);
	    	uu_list_push(&seglst,&seg);

			++npaths;
			k += 1;
		}
	}

/*
.....Create cl paths for all slices
*/
	ncl_sm_clpath_create_all(Sslpt_flag,npaths);

	nsegs = seglst.cur_cnt;
	psegs = (UM_segment *)UU_LIST_ARRAY(&seglst);

	#pragma omp parallel for private(k,minclz,clpts)
	for (k = 0; k < nsegs; k++)
	{			
		UU_LIST_EMPTY (&clpts);		
		minclz = zmin;
/*
.....Initilized projected clpoints list
*/
		uu_list_init(&clpts,sizeof(UM_clpt),200,200);
/*
.....Project points for each slice
*/
		ncl_smill_project_slice(trad,frad,thk,minclz,k,vdir,
						psegs[k].p1,psegs[k].p2,toler,&clpts);
			
		#pragma omp critical  
		{
			ncl_sm_clpath_insert_list(Sslpt_flag,k,&clpts);
		}
#if 0	
/*
.....Debug scallop slice clpath
*/
		ncl_debug_clpts(&clpts,0);
#endif
	}

	uu_list_free(&seglst);
}

/*********************************************************************
**    E_FUNCTION     : ncl_iSegPlane_maxz(npts,pts,pl,ipt)
**        Get plane and slice points intersection points with max z
**    PARAMETERS
**       INPUT  :
**			npts	 - numbe rof points
**			pts		 - points
**          pl       - plane
**       OUTPUT :
**          ipt      - the intersection point with max z
**    RETURNS      :
**         none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL ncl_iSegPlane_maxz(npts,pts,pl,ipt)
int npts;
UM_coord *pts,ipt;
UM_plane pl;
{
	int i;
	UU_REAL zmax;
	UM_coord ipt1;
	UU_LOGICAL lints = UU_FALSE;

	for (i = 0; i < npts-1; i++)
	{
		if (um_iSegPlane(pts[i],pts[i+1],pl,ipt1))
		{	
			if (!lints)
			{
				zmax = ipt1[2];
				um_vctovc (ipt1,ipt);
			}
			else if (ipt1[2] > zmax)
			{
				um_vctovc (ipt1,ipt);
				zmax = ipt1[2];
			}
			lints = UU_TRUE;
		}
	}
	return lints;
}

/*********************************************************************
**    E_FUNCTION     : ncl_smill_getlength(clpts,i1,i2,ipt1,ipt2)
**        Get plane and slice points intersection points with max z
**    PARAMETERS
**       INPUT  :
**			pts		  - points
**          i1,i2	  - the start and end index
**          ipt1,ipt2 - the start and end point
**       OUTPUT :
**          dis       - from the ipt1 to ipt2
**    RETURNS      :
**         none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_REAL ncl_smill_getlength(clpts,i1,i2,ipt1,ipt2)
int i1,i2;
UM_clpt *clpts;
UM_coord ipt1,ipt2;
{
	int i;
	UU_REAL dis = 0.0;
	UU_LOGICAL lstart = UU_FALSE;

	for (i = i1; i < i2; ++i)
	{		
		if (!lstart)
		{
			if (um_point_in_segment(ipt1,clpts[i].pte,clpts[i+1].pte,UM_FUZZ))
			{
				lstart = UU_TRUE;
				if (um_point_in_segment(ipt2,clpts[i].pte,clpts[i+1].pte,UM_FUZZ))
					dis = um_dcccc(ipt1,ipt2);
				else
				{
					dis += um_dcccc(ipt1,clpts[i+1].pte);
					um_vctovc(clpts[i+1].pte,ipt2);
				}
				break;
			}
		}
	}

	return dis;
}

/*********************************************************************
**    E_FUNCTION     : ncl_smill_getstep(clpts,i1,i2,ipt1,ipt2,step_max)
**        Get the SMILL stepover distance for scallop height
**    PARAMETERS
**       INPUT  :
**			pts		  - points
**          i1,i2	  - the start and end index
**          ipt1,ipt2 - the start and end point
**			step_max   - the maximum stepver distance
**       OUTPUT :
**          none
**    RETURNS      :
**         The stepover distance
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_REAL ncl_smill_getstep(clpts,i1,i2,ipt1,ipt2,step_max)
int i1,i2;
UM_clpt *clpts;
UM_coord ipt1,ipt2;
UU_REAL step_max;
{
	int i,istart,iend;
	UU_REAL dstep,dis_prev,dis = 0.0;
	UM_vector vec;
	UM_coord pt;
	UU_LOGICAL lstart = UU_FALSE;

	for (i = i1; i < i2; ++i)
	{		
		if (!lstart)
		{
			if (um_point_in_segment(ipt1,clpts[i].pte,clpts[i+1].pte,UM_FUZZ))
			{
				istart = i;
				lstart = UU_TRUE;
				if (um_point_in_segment(ipt2,clpts[i].pte,clpts[i+1].pte,UM_FUZZ))
					dis = um_dcccc(ipt1,ipt2);
				else
					dis += um_dcccc(ipt1,clpts[i+1].pte);
			
				if (dis > step_max)
			    {	
					um_vcmnvc(clpts[i+1].pte,clpts[i].pte,vec);
				    um_unitvc(vec,vec);
				    um_translate_point(ipt1,step_max,vec,pt);
			    	dstep = um_dist_2d(ipt1,pt);
				    break;				
				}
			}
		}
		else if (lstart)
		{
			dis_prev = dis;
			if (um_point_in_segment(ipt2,clpts[i].pte,clpts[i+1].pte,UM_FUZZ))
				dis += um_dcccc(clpts[i].pte,ipt2);	
			else
				dis += um_dcccc(clpts[i].pte,clpts[i+1].pte);
			if (dis > step_max)
			{	
				um_vcmnvc(clpts[i+1].pte,clpts[i].pte,vec);
				um_unitvc(vec,vec);
				um_translate_point(clpts[i].pte,step_max-dis_prev,vec,pt);
				dstep = um_dist_2d(ipt1,pt);
				break;				
			}
		}
	}

	return dstep;
}

/*********************************************************************
**    E_FUNCTION     : ncl_smill_scallop_step(pla,plb,trad,step_max,step)
**        Calculate the max slpath lengthn between two slice plane.
**    PARAMETERS
**       INPUT  :
**			i0			- slice index
**          pla,plb		- planes
**			trad		- tool radius
**          step_max    - maximum stepover
**       OUTPUT :
**          step          - real stepover 
**    RETURNS      :
**         UU_SUCCESS if need to insert slice; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_smill_scallop_step(i0,pla,plb,trad,step_max,step)
int i0;
struct NCL_nclpl_rec *pla,*plb;
UU_REAL trad,step_max,*step;
{
	int i,imax,imax1,j,jmax,jmax1,jmax2,jmax11,jmax12,j1,j11,iflag,nvpaths,npts,status;
	UU_REAL dis,dmax,dstep,dis1,dmax1,dstep1,dmax2,dis2;
	UM_coord *pts,ipt1,ipt2,ipt1max,ipt2max,ipt11max,ipt12max,ipt12,pt;	
	UM_vector vec;
	UM_plane pl1,pl2,pl12;
	UU_LOGICAL lints,lmid;
	UM_clpt *clpts;
	char tbuf[80];

	iflag = 1;
	imax = 0;
	jmax1 = 0;
	jmax2 = 0;
	dmax = 0.0;
	dmax1 = 0.0;
	dmax2 = 0.0;
	status = UU_SUCCESS;
	ncl_nclpl_to_umplane(pla,&pl1);
	ncl_nclpl_to_umplane(plb,&pl2);
					
	um_vcmnvc(pl2.p0,pl1.p0,vec);
	um_unitvc(vec,vec);
	um_translate_point(pl1.p0,0.25*step_max,vec,pl12.p0);
	um_vctovc(pl1.n,pl12.n);
/*
.....Get the numbers of vertical slpath
*/
	ncl_sm_clpath_getnpath(&Sslpt_flag,&nvpaths);

#if 0
	ncl_debug_clpath(Sslpt_flag);
#endif

	for (i = 0; i < nvpaths; i++)
	{
/* 
.....Get the slice points in the ith-pass
*/
		npts = 0;
		ncl_sm_clpath_getpts(Sslpt_flag,i,&npts,&clpts);
#if 0		
		if (i0 == 1 && npts > 0)
		{
			sprintf(tbuf,"i = %d",i); 	
			NclxDbgPstr(tbuf);
			sprintf(tbuf,"*stop"); 	
			NclxDbgPstr(tbuf);
			for (j = 0; j < npts; j++)
				ncl_debug_clpt(clpts[j].pte,0);
		}
#endif
		lints = UU_FALSE;
		for (j = 0; j < npts-1; j++)
		{
			if (um_iSegPlane(clpts[j].pte,clpts[j+1].pte,pl1,ipt1))
			{
				lmid = UU_FALSE;
				for (j1 = j; j1 < npts-1; j1++)
				{
					if (!lmid)		
					{
						lmid = um_iSegPlane(clpts[j1].pte,clpts[j1+1].pte,pl12,ipt12);
						if (lmid)
							j11 = j1;
					}

					if (um_iSegPlane(clpts[j1].pte,clpts[j1+1].pte,pl2,ipt2))
					{
						lints = UU_TRUE;
						break;
					}
				}
			}
			if (lints)
				break;
		}

		if (lints)
		{
			dis2 = um_sqdis(ipt1,ipt2);
			dis1 =  um_sqdis(ipt1,ipt12);
			if (dis2 > dmax2)
			{
				jmax1 = j;
				jmax2 = j1;
				um_vctovc(ipt1,ipt1max);
				um_vctovc(ipt2,ipt2max);
				imax = i;
				dmax2 = dis2;
			}

			if (dis1 > dmax1)
			{
				jmax11 = j;							
				jmax12 = j11;
				um_vctovc(ipt1,ipt11max);
				um_vctovc(ipt12,ipt12max);
				imax1 = i;							
				dmax1 = dis1;
			}
		}
	}
	
	dmax = sqrt(dmax2);
	dmax1 = sqrt(dmax1);

#if 0	
	sprintf(tbuf,"$$imax1=%d,jmax1=%d,jmax2=%d,jmax11=%d,jmax12=%d",
				imax1,jmax1,jmax2,jmax11,jmax12); 	
	NclxDbgPstr(tbuf);
	sprintf(tbuf,"$$dmax=%12.8f,dmax1=%12.8f,dmax1=%12.8f",dmax,dmax1,step_max); 		
	NclxDbgPstr(tbuf);	
#endif

	if (dmax1 > step_max + UM_FUZZ)
	{
		ncl_sm_clpath_getpts(Sslpt_flag,imax1,&npts,&clpts);
		dstep = ncl_smill_getstep(clpts,jmax11,jmax12+1,ipt11max,ipt12max,step_max);				
	}
	else if (dmax > step_max + UM_FUZZ)
	{
		ncl_sm_clpath_getpts(Sslpt_flag,imax,&npts,&clpts);
		dstep = ncl_smill_getstep(clpts,jmax1,jmax2+1,ipt1max,ipt2max,step_max);
	}
	else
	{
		dstep = step_max;
		status = UU_FAILURE;
	}
#if 0		
	sprintf(tbuf,"$$dstep=%12.8f,step_max=%12.8f",dstep,step_max); 	
	NclxDbgPstr(tbuf);	
#endif
	*step = dstep;
	return status;
}

/*********************************************************************
**    E_FUNCTION     : ncl_sfbucket_plio (bbox,pl,tol,ier)
**        create intersection of traingles within bounding box with a plane.
**    PARAMETERS
**       INPUT  :
**          bbox     - bounding box
**          pl       - plane
**          tol     - tolerance
**       OUTPUT :
**          ier     - error number, if fail
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_sfbucket_plio(bbox,pl,tol,ier)
UM_2box bbox;
struct NCL_nclpl_rec *pl;
UU_REAL tol;
UM_int2 *ier;
{
	int isf,i,i1,inpts,j,j1,k,n,nc,npts,ntri,status;
	UU_REAL tolsq,rmax;
	UM_coord *pts,*ptio;
	UU_REAL d,umin,umax,vmin,vmax,u,v;
	UM_int4 isubscr;
	UM_coord ipt[2];
	UM_vector vec,vcross;
	int ncnt,npo,*npi,*np,icolor;
	int istat,trilst_init,itsk;
	UU_LIST trilst,containlst;
	UM_trian *ptri;
	nclsf_prim_type typ;
	UU_REAL zmax,zmin;
	UU_REAL xmmx[2],ymmx[2];
	UM_2box plbox;
	UM_plane plane;

	status = UU_SUCCESS;
	nc = 0;
	tolsq = tol*tol;
	uu_list_init0 (&trilst);;
	trilst_init = 0;

	ncl_nclpl_to_umplane(pl,&plane);
/*
.....Get the bounding box of the plane intersection with bbox
*/
	inpts = um_iBoxPlane(bbox,plane,ipt);
	if (inpts < 2)
		return UU_FAILURE;
	if (inpts == 2)
	{
		plbox.xmin = ipt[0][0] - tol;
		plbox.ymin = ipt[0][1] - tol;
		plbox.xmax = ipt[1][0] + tol;
		plbox.ymax = ipt[1][1] + tol;
	}
/*
...Get the triangle within the bounding box
*/
	if (trilst_init == 0)
	{
		uu_list_init (&trilst,sizeof (UM_trian),200,200);
		trilst_init = 1;
	}
	else
		UU_LIST_EMPTY (&trilst);
	ncl_sfbucket_getbox_trianlst (plbox,&trilst,UU_FALSE);
/*
.....Delete duplicate triangles
*/
	ncnt = trilst.cur_cnt;
	if (ncnt > 0)
	{
		uu_list_init(&containlst, sizeof(UM_trian), ncnt, ncnt);
		uu_list_push_list (&containlst, &trilst);
		UU_LIST_EMPTY (&trilst);

		ptri = (UM_trian *) UU_LIST_ARRAY (&containlst);
		for (k = 0; k < ncnt; k++,ptri++)
		{
			if (trilst.cur_cnt > 0 && ncl_sfbucket_contain_trian(&trilst, ptri))
				continue;
			uu_list_push(&trilst, ptri);
		}

		if (containlst.data)
			uu_list_free(&containlst);
	}
/*
.....Intersection with plane
*/
	ntri = trilst.cur_cnt;
	if (ntri < 1) 
	{
		status = UU_FAILURE;
		goto Done;
	}
	ptri = (UM_trian *)UU_LIST_ARRAY(&trilst);
	for (i = 0; i < ntri; i++)
	{
		ncl_tripln_io(&ptri[i],pl,tol,tolsq);
	}
/*
.....Sort the segments
*/
	ncl_arrange_segs (10*tol);

	ncl_getpts_uv (&npts,&pts);
	nc = 1;
	ncl_get_nios (&nc,&np);

	icolor = 4;
	if (nc == 1)
	{
#if 0
		ncl_draw_polyline(npts,pts,icolor,1);
		for (i=0; i<npts-1;i++)	
			ncl_debug_clpt2(pts[i],pts[i+1],0);
#endif
	}
	else
	{
		j1 = 0;
		for (i = 0; i < nc; i++)
		{
			n = np[i];
			ptio = (UM_coord *) uu_malloc (n*sizeof(UM_coord));
			for (j = 0; j < n; j++) um_vctovc (pts[j1+j],ptio[j]);
#if 0
			ncl_draw_polyline(n,ptio,icolor,1);
			for (i1=0; i1<n-1;i1++)
				ncl_debug_clpt2(pts[i1],pts[i1+1],0);
#endif
			j1 += n;
			if (ptio)
				uu_free(ptio);
		}
	}

Done:
#if 0
	ncl_free_uv();
	ncl_free_nios();
#endif
	if (trilst_init == 1) uu_list_free (&trilst);

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_ptlist_startend_same(ptlist,ptlist1)
**        Check which point list is hgher
**    PARAMETERS
**       INPUT  :
**          ptlist    - point list
**          ptlist1   - point list
**       OUTPUT :
**          none
**    RETURNS      :
**         UU_SUCCESS if ptlist is higher; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UU_LOGICAL ncl_ptlist_startend_same(ptlist,ptlist1)
UU_LIST *ptlist,*ptlist1;
{
	int npts,npts1;
	UM_coord *pts,*pts1;

	pts= (UM_coord *) UU_LIST_ARRAY (ptlist);
	npts = UU_LIST_LENGTH(ptlist);
	pts1= (UM_coord *) UU_LIST_ARRAY (ptlist1);
	npts1 = UU_LIST_LENGTH(ptlist1);

	if (UM_SQDIS_2D(pts[0],pts1[0])< UM_DFUZZ &&
		UM_SQDIS_2D(pts[npts-1],pts1[npts1-1])< UM_DFUZZ)
		return UU_TRUE;

	return UU_FALSE;
}

/*********************************************************************
**    E_FUNCTION     : ncl_ptlist_startend_pt(ptlist,pt)
**        Check which point list is hgher
**    PARAMETERS
**       INPUT  :
**          ptlist    - point list
**          pt		  - point
**       OUTPUT :
**          none
**    RETURNS      :
**         0: not the same
**		   1: start point same
**		   2: end point same
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int ncl_ptlist_startend_pt(ptlist,pt)
UU_LIST *ptlist;
UM_coord pt;
{
	int npts;
	UM_coord *pts,*pts1;

	pts= (UM_coord *) UU_LIST_ARRAY (ptlist);
	npts = UU_LIST_LENGTH(ptlist);
	if (UM_SQDIS_2D(pts[0],pt) < UM_DFUZZ) return 1;
	if (UM_SQDIS_2D(pts[npts-1],pt) < UM_DFUZZ) return 2;

	return 0;
}

/*********************************************************************
**    E_FUNCTION     : ncl_ptlist_higher(ptlist,ptlist1)
**        Check which point list is hgher
**    PARAMETERS
**       INPUT  :
**          ptlist    - point list
**          ptlist1   - point list
**       OUTPUT :
**          none
**    RETURNS      :
**         UU_SUCCESS if ptlist is higher; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL ncl_ptlist_higher(ptlist,ptlist1)
UU_LIST *ptlist,*ptlist1;
{
	int npts,npts1;
	UM_coord *pts,*pts1;

	pts= (UM_coord *) UU_LIST_ARRAY (ptlist);
	npts = UU_LIST_LENGTH(ptlist);		
	pts1= (UM_coord *) UU_LIST_ARRAY (ptlist1);
	npts1 = UU_LIST_LENGTH(ptlist1);
	if (pts[0][2] > pts1[0][2] - UM_DFUZZ &&
		pts[npts-1][2] > pts1[npts1-1][2]- UM_DFUZZ)
		return UU_TRUE;

	return UU_FALSE;
}

/*********************************************************************
**    E_FUNCTION     : ncl_sfplio_get_points(tol,inpts,ipts,ier)
**        Get the extreme end intersection points.
**    PARAMETERS
**       INPUT  :
**          tol     - tolerance used to check gaps
**       OUTPUT :
**			ptlist  - higher intersection points list
**    RETURNS      :
**         none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void ncl_sfplio_get_maxzpoints(spt,ept,tol,npts,pts,ptlist)
int npts;
UM_coord spt,ept, *pts;
UU_REAL tol;
UU_LIST *ptlist;
{
	int i,i1,icolor,npts1,status;
	UM_coord pt1,pt2,spt1,ept1;
	UU_LOGICAL lstart,lreverse;
	UU_REAL tolsq;
	UU_LIST ptlst1;
	int cmp();

	tolsq = tol*tol;
	status = UU_SUCCESS;
	uu_list_init(&ptlst1,sizeof(UM_coord),npts,npts);

#if 0
	ncl_debug_clpt(spt,4);
	ncl_debug_clpt(ept,4);
	for (i = 0; i < npts-1; i++)
		ncl_debug_clpt2(pts[i],pts[i+1],4);
#endif

	i1 = 0;
	lstart = UU_FALSE;
	lreverse = UU_FALSE;
	for (i = 0; i < npts-1; i++)
	{
		um_vctovc(pts[i],pt1);
		pt1[2] = 0.0;
		um_vctovc(pts[i+1],pt2);
		pt2[2] = 0.0;
		if (!lstart)
		{
			if (um_point_in_segment_2d(spt,pt1,pt2,tol))
			{
				lstart = UU_TRUE;
				lreverse = UU_FALSE;
				um_vctovc(spt,spt1);
				um_get_between_point(pts[i],pts[i+1],UU_TRUE,spt1);
				if (!ncl_ptlist_contain_point(&ptlst1,spt1))			
					uu_list_push(&ptlst1,&spt1);
			}
			else if (um_point_in_segment_2d(ept,pt1,pt2,tol))
			{
				lstart = UU_TRUE;
				lreverse = UU_TRUE;
				um_vctovc(ept,ept1);
				um_get_between_point(pts[i],pts[i+1],UU_TRUE,ept1);							
				if (!ncl_ptlist_contain_point(&ptlst1,ept1))			
					uu_list_push(&ptlst1,&ept1);
			}
			else if (um_point_in_segment_2d(pt1,spt,ept,tol) &&
				um_point_in_segment_2d(pt2,spt,ept,tol))
			{
				lstart = UU_TRUE;
				if (!ncl_ptlist_contain_point(&ptlst1,pts[i]))			
					uu_list_push(&ptlst1,&pts[i]);
			}
		}
		if (lstart)
		{
			if (!lreverse)
			{
				if (um_point_in_segment_2d(ept,pt1,pt2,tol))
				{			
					um_vctovc(ept,ept1);
					um_get_between_point(pts[i],pts[i+1],UU_TRUE,ept1);
					if (!ncl_ptlist_contain_point(&ptlst1,ept1))						
						uu_list_push(&ptlst1,&ept1);
					lstart = UU_FALSE;
					i1++;
				}
				else if (UM_SQDIS(spt1,pts[i+1]) > tolsq)
				{
					uu_list_push(&ptlst1,&pts[i+1]);
				}
			}
			else
			{
				if (um_point_in_segment_2d(spt,pt1,pt2,tol))
				{
					um_vctovc(spt,spt1);
					um_get_between_point(pts[i],pts[i+1],UU_TRUE,spt1);
					if (!ncl_ptlist_contain_point(&ptlst1,spt1))						
						uu_list_push(&ptlst1,&spt1);
					lstart = UU_FALSE;
					i1++;
				}
				else if (UM_SQDIS(ept1,pts[i+1]) > tolsq)
				{
					uu_list_push(&ptlst1,&pts[i+1]);
				}
			}

			if (lstart && i == npts-2)
			{
				lstart = UU_FALSE;
				i1++;
			}
		}

		npts1 = UU_LIST_LENGTH(&ptlst1);
		if (!lstart && i1 == 1 && npts1 > 0)
		{
			if (lreverse)
				uu_list_sort(&ptlst1,cmp);
			uu_list_push_list(ptlist,&ptlst1);
			UU_LIST_EMPTY(&ptlst1);
		}
		else if (!lstart && i1 > 1 && npts1 > 0)
		{		
			if (lreverse)
				uu_list_sort(&ptlst1,cmp);
			if (ncl_ptlist_startend_same(ptlist,&ptlst1))
			{
				if (!ncl_ptlist_higher(ptlist,&ptlst1))
				{
					UU_LIST_EMPTY(ptlist);
					uu_list_push_list(ptlist,&ptlst1);
					UU_LIST_EMPTY(&ptlst1);
				}
			}
		}
	}

	if (ptlst1.data) uu_list_free (&ptlst1);	
}

/*********************************************************************
**    E_FUNCTION     : ncl_sfplio_within_segment (spt,ept,tol,ptlist)
**        create intersection segments from spt to ept.
**    PARAMETERS
**       INPUT  :
**          spt     - start point of segment
**          ept     - end point of segment
**          tol     - tolerance
**       OUTPUT :
**          ptlist  - 3d point list
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int ncl_sfplio_within_segment(spt,ept,tol,ptlist)
UM_coord spt,ept;
UU_REAL tol;
UU_LIST *ptlist;
{
	int isf,i,i1,inpts,j,j1,k,n,nc,npts,ntri,status;
	UU_REAL tol1,tolsq,rmax;
	UM_coord *pts,*ptio,pt1,pt2,spt1,ept1;
	UU_REAL d,umin,umax,vmin,vmax,u,v;
	UM_int4 isubscr;
	UM_coord ipt[2],pt;
	UM_vector vec,vcross,nzvec;
	int ncnt,npo,*npi,*np,icolor;
	int istat,trilst_init,itsk;
	UU_LIST trilst,containlst,ptlst,ptlst1;
	UM_trian *ptri;
	nclsf_prim_type typ;
	UU_REAL zmax,zmin;
	UU_REAL xmmx[2],ymmx[2];
	UM_2box plbox;
	UM_plane plane;
	UU_LOGICAL lstart;
	int cmp();

	status = UU_SUCCESS;
	nc = 0;
	tol1 = 0.1 * tol;
	tolsq = tol1*tol1;
	uu_list_init0(&trilst);
	uu_list_init0(&ptlst);
	uu_list_init0(&ptlst1);
	trilst_init = 0;
/*
.....Create vectical plane for each boundary segment
*/
	nzvec[0] = 0.0; nzvec[1] = 0.0; nzvec[2] = 1.0;
	um_translate_point(spt,1.0,nzvec,pt);
	um_plane1(spt,ept,pt,&plane);
/*
.....Set sorting vector for cmp()
*/
	um_vcmnvc(ept,spt,vec);
	ncl_set_ldir(vec);
/*
.....Get the bounding box of the plane intersection with bbox
*/
	plbox.xmin = MIN2(spt[0],ept[0]) - 2.0*tol;
	plbox.ymin = MIN2(spt[1],ept[1]) - 2.0*tol;
	plbox.xmax = MAX2(spt[0],ept[0]) + 2.0*tol;
	plbox.ymax = MAX2(spt[1],ept[1]) + 2.0*tol;
/*
...Get the triangle within the bounding box
*/
	if (trilst_init == 0)
	{
		uu_list_init (&trilst,sizeof (UM_trian),200,200);
		trilst_init = 1;
	}
	else
		UU_LIST_EMPTY (&trilst);
	ncl_sfbucket_getbox_trianlst (plbox,&trilst,UU_FALSE);
/*
.....Delete duplicate triangles
*/
	ncnt = trilst.cur_cnt;
	if (ncnt > 0)
	{
		uu_list_init(&containlst, sizeof(UM_trian), ncnt, ncnt);
		uu_list_push_list (&containlst, &trilst);
		UU_LIST_EMPTY (&trilst);
		ptri = (UM_trian *) UU_LIST_ARRAY (&containlst);
		for (k = 0; k < ncnt; k++,ptri++)
		{
			if (trilst.cur_cnt > 0 && ncl_sfbucket_contain_trian(&trilst, ptri))
				continue;
			uu_list_push(&trilst, ptri);
		}
		if (containlst.data)
			uu_list_free(&containlst);
	}
/*
.....Intersection with plane
*/
	ntri = trilst.cur_cnt;
	if (ntri < 1) 
	{
		status = UU_FAILURE;
		goto Done;
	}
	ptri = (UM_trian *)UU_LIST_ARRAY(&trilst);
	for (i = 0; i < ntri; i++)
	{
#if 0
		ncl_debug_triangle(i,0,0,0,&ptri[i]);
#endif
		ncl_tripln_io0(&ptri[i],plane.p0,plane.n,tol1,tolsq,UU_TRUE);
	}
/*
.....Sort the segments
*/
	ncl_arrange_segs(2.0*tol);

	ncl_getpts_uv (&npts,&pts);
	nc = 1;
	ncl_get_nios (&nc,&np);

#if 0
	ncl_debug_clpt(spt,0);
	ncl_debug_clpt(ept,0);
#endif

	uu_list_init(&ptlst,sizeof(UM_coord),npts,npts);

	icolor = 4;
	if (nc == 1)
	{
		ncl_sfplio_get_maxzpoints(spt,ept,tol,npts,pts,&ptlst);
	}
	else
	{
		j1 = 0;
		for (i = 0; i < nc; i++)
		{
			n = np[i];
			ptio = (UM_coord *) uu_malloc (n*sizeof(UM_coord));
			for (j = 0; j < n; j++) um_vctovc (pts[j1+j],ptio[j]);

			if (i == 0)
				uu_list_init (&ptlst1,sizeof(UM_coord),n,n);
			else
				UU_LIST_EMPTY(&ptlst1);

			ncl_sfplio_get_maxzpoints(spt,ept,tol,n,ptio,&ptlst1);

			j1 += n;
			if (ptio)
				uu_free(ptio);
/*
.....put into ptlist
*/
			if (UU_LIST_LENGTH(&ptlst1) <= 0) continue;

			if (i == 0)
				uu_list_push_list(&ptlst,&ptlst1);
			else
			{
/*
.....Check which points are higher
*/
				if (ncl_ptlist_startend_same(&ptlst,&ptlst1))
				{
					if (!ncl_ptlist_higher(&ptlst,&ptlst1))
					{
						UU_LIST_EMPTY(&ptlst);
						uu_list_push_list(&ptlst,&ptlst1);
					}
				}
				else
				{
					if (ncl_ptlist_startend_pt(&ptlst,spt) == 1 &&
						ncl_ptlist_startend_pt(&ptlst,ept) == 0 &&
						ncl_ptlist_startend_pt(&ptlst1,spt) == 0 &&
						ncl_ptlist_startend_pt(&ptlst1,ept) == 2)
					{
						uu_list_push_list(&ptlst,&ptlst1);
					}
					else if (ncl_ptlist_startend_pt(&ptlst,ept) == 2 &&
						ncl_ptlist_startend_pt(&ptlst,spt) == 0 &&
						ncl_ptlist_startend_pt(&ptlst1,ept) == 0 &&
						ncl_ptlist_startend_pt(&ptlst1,spt) == 1)
					{
						uu_list_push_list(&ptlst1,&ptlst);						
						UU_LIST_EMPTY(&ptlst);
						uu_list_push_list(&ptlst,&ptlst1);
					}
				}
			}
		}
		if (ptlst1.data) uu_list_free (&ptlst1);
	}
/*
.....Put into 3d boundary
*/
	uu_list_push_list(ptlist,&ptlst);
#if 0
	ncl_debug_clpts(&ptlst,4);
#endif

Done:
	ncl_free_uv();
	ncl_free_nios();
	if (trilst_init == 1) uu_list_free (&trilst);
	if (ptlst.data) uu_list_free (&ptlst);

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_bucket_boundplio (bbox,pl,tol,npt,ipt,ier)
**        create intersection of traingles within bounding box with a plane.
**    PARAMETERS
**       INPUT  :
**          bbox     - bounding box
**          pl       - plane
**          tol     - tolerance
**       OUTPUT :
**          ier     - error number, if fail
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_bucket_boundplio(bbox,pl,tol,inpts,ipts,ier)
UM_2box bbox;
struct NCL_nclpl_rec *pl;
UU_REAL tol;
UM_coord ipts[];
UM_int2 *inpts,*ier;
{
	int i,nint,npts,nsegs,status;
	UM_coord *pts,*ptio;
	UU_REAL d,umin,umax,vmin,vmax,u,v;
	UM_vector vec,vcross;
	int npo,*npi,*np,icolor;
	int istat,trilst_init,itsk;
	UU_LIST seglst;
	UM_segment *pseg;
	UM_coord ipt[2],pt;
	UU_REAL zmax,zmin;
	UU_REAL xmmx[2],ymmx[2];
	UM_2box plbox;
	UM_plane plane;

	status = UU_SUCCESS;
	uu_list_init0 (&seglst);
	ncl_nclpl_to_umplane(pl,&plane);
/*
.....Get the bounding box of the plane intersection with bbox
*/
	npts = um_iBoxPlane(bbox,plane,ipt);
	if (npts == 2)
	{
		plbox.xmin = ipt[0][0] - tol;
		plbox.ymin = ipt[0][1] - tol;
		plbox.xmax = ipt[1][0] + tol;
		plbox.ymax = ipt[1][1] + tol;
	}
/*
...Get the triangle within the bounding box
*/
	if (!seglst.data)
		uu_list_init (&seglst,sizeof (UM_segment),20,20);
	else
		UU_LIST_EMPTY (&seglst);

	ncl_sgbucket_getbox_seglst (plbox,&seglst);

	nsegs = seglst.cur_cnt;
	if (nsegs < 1) status = UU_FAILURE;
	pseg = (UM_segment *) UU_LIST_ARRAY (&seglst);

	nint = 0;
	for (i = 0; i < nsegs; i++,pseg++)
	{
#if 0
		ncl_debug_clpt2(pseg->p1,pseg->p2,0);
#endif
		if (um_iSegPlane(pseg->p1,pseg->p2,plane,pt) > 0)
		{
			if(nint == 0 || (nint > 0 && !ncl_points_contain_point(nint,ipts,pt)))
			{
				um_vctovc(pt,ipts[nint]);			
				++nint;
			}
		}
	}

	if (seglst.data)
		uu_list_free (&seglst);
	*inpts = nint;
}

/*********************************************************************
**    E_FUNCTION     : cl_create_contour_boundary3d(bound,bound3d)
**        create 3d contour boundary given 2d contour boudanry.
**    PARAMETERS
**       INPUT  :
**          bound    - 2d contour boundary
**          tol      - tolerance
**       OUTPUT :
**          bound3d  - 3d contour boundary
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_create_contour_boundary3d(bound,tol,bound3d)
UU_LIST *bound;
UU_REAL tol;
UU_LIST **bound3d;
{
	int i,npts,npts1;
	UM_coord *pts,*pts1;

	npts = bound->cur_cnt;
	pts = (UM_coord *) UU_LIST_ARRAY (bound);
	(*bound3d) = (UU_LIST *) uu_malloc (sizeof(UU_LIST));
	uu_list_init (*bound3d, sizeof(UM_coord), npts, npts);

	for (i = 0; i < npts-1; i++)
		ncl_sfplio_within_segment(pts[i],pts[i+1],tol,*bound3d);

#if 0
	pts1= (UM_coord *) UU_LIST_ARRAY (*bound3d);
	npts1 = UU_LIST_LENGTH(*bound3d);
	ncl_draw_polyline (npts1,pts1,7,1);
	ncl_debug_clpts(*bound3d,5);
#endif
}

/*********************************************************************
**    E_FUNCTION     : ncl_point_on_bound3d(pt,trad,tol)
**        Check if point pt on the 3d contour boundary using sgbucket.
**    PARAMETERS
**       INPUT  :
**			pt		- point to check
**			trad	- tool radus
**          tol		- tolerance
**       OUTPUT :
**          none
**    RETURNS      :
**         UU_SUCCESS if on; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_point_on_bound3d(pt,trad,tol)
UM_coord pt;
UU_REAL trad,tol;
{
	int i,npts,nsegs,status;
	UM_coord *pts;
	UM_vector vec;
	UM_2box bbox;
	char tbuf[80];
	UM_segment *pseg,seg_min;
	UU_REAL dissq,dissq_min,tolsq;
	UU_LIST seglst;
	UU_REAL um_sqdis_from_line();
	UU_REAL um_sqdis_from_segment();

	dissq_min = 1000000.0;
	tolsq = tol*tol;
	status = UU_FAILURE;
	uu_list_init0 (&seglst);
/*
.....The bounding box
*/
	bbox.xmin = pt[0] - trad;
	bbox.xmax = pt[0] + trad;
	bbox.ymin = pt[1] - trad;
	bbox.ymax = pt[1] + trad;
/*
...Get the triangle within the bounding box
*/
	if (!seglst.data)
		uu_list_init (&seglst,sizeof (UM_segment),20,20);
	else
		UU_LIST_EMPTY (&seglst);

	ncl_sgbucket_getbox_seglst (bbox,&seglst);

	nsegs = seglst.cur_cnt;
	if (nsegs < 1) status = UU_FAILURE;
	pseg = (UM_segment *) UU_LIST_ARRAY (&seglst);

	for (i = 0; i < nsegs; i++, pseg++)
	{
#if 0
		ncl_debug_clpt2(seg_min.p1,seg_min.p2,0);
#endif
		dissq = um_sqdis_from_segment(pt,pseg->p1,pseg->p2);
		if (dissq < tolsq)
		{
			dissq_min = dissq;
			status = UU_SUCCESS;
			break;
		}
		if (dissq < dissq_min)
		{
			dissq_min = dissq;
			um_vctovc(pseg->p1,seg_min.p1);
			um_vctovc(pseg->p2,seg_min.p2);
		}
	}

#if 0
	sprintf(tbuf,"$$The seg_min dissq =%8.5f",dissq_min); 
	NclxDbgPstr(tbuf);
	ncl_debug_clpt(pt,0);
	ncl_debug_clpt2(seg_min.p1,seg_min.p2,0);
#endif
	
	if (seglst.data)
		uu_list_free (&seglst);

	return status;
}

/*********************************************************************
**    E_FUNCTION     : ncl_point_inside_bound2d(pt,bound,tol)
**        Check if point pt on the 2d contour boundary.
**    PARAMETERS
**       INPUT  :
**			pt		- point to check
**			trad	- tool radus
**          tol		- tolerance
**       OUTPUT :
**          none
**    RETURNS      :
**         UU_SUCCESS if on; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_point_inside_bound2d(pt,bound,box,tol)
UM_coord pt;
UU_LIST *bound;
UM_2box *box;
UU_REAL tol;
{
	int ins,npt;
	UM_2Dcoord *pts;
	pts = (UM_2Dcoord *) UU_LIST_ARRAY (bound);
	npt = UU_LIST_LENGTH(bound);
	ins = um_check_inside(pts,npt,pt,box,tol);
	return ins;
}

/*********************************************************************
**    E_FUNCTION	: ncl_smill_clpath_between
**							(nend,trad,frad,zmin,bound,tol,ps,pe)
**    Added clpath between ps and pe
**    PARAMETERS
**       INPUT  :
**			nend	- end condition 0:TO 1:PAST 2:ON 3:CONTCT
**			trad	- corner radus
**			frad	- flat radus
**			zmin	- min z
**          bound   - boundary list
**          tol     - tolerance
**			ps		- start point
*			pe		- end point
**       OUTPUT :
**          none
**    RETURNS      :
**         none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_smill_clpath_between(nend,trad,frad,thk,zmin,bound,tol,ps,pe)
int nend;
UU_REAL trad,frad,thk,zmin,tol;
UU_LIST *bound;
UM_coord ps,pe;
{
	int i,iret1,iret2,npts;
	UM_coord *pts,clpt1,ccpt1,clpt2,ccpt2;
	UM_vector slvec,clvc;
	UU_LIST pptr,clptlist,clpoints;
	UU_LOGICAL lproj;
	UM_clpt clps,clpe;
/*
.....tool vector
*/
	clvc[0] = 0.0;
	clvc[1] = 0.0;
	clvc[2] = 1.0;

	uu_list_init(&clptlist,sizeof(UM_clpt),100,100);
	uu_list_init(&clpoints,sizeof(UM_clpt),100,100);
/*
.....Get the boundary portion from ps to pe
*/		
	uu_list_init (&pptr,sizeof(UM_coord),10,10);
	if (nend == 0 || nend == 2)
		ncl_bound_segments_between(bound,ps,pe,tol,&pptr);
	else
	{
		uu_list_push(&pptr,ps);
		uu_list_push(&pptr,pe);
	}
/*
.....Check if projection is needed for PAST case
*/
	lproj = UU_TRUE;
	if (nend == 1) /*PAST*/
	{
		lproj = UU_FALSE;
		um_vctovc(ps,clpt1);
		clpt1[2]= zmin;

		iret1 = ncl_projsf_clpoint(trad,frad,clpt1,ccpt1,thk,tol,UU_FALSE,0,
			&clpoints,UU_NULL);
		if (clpt1[2] > ps[2]+tol)
			lproj = UU_TRUE;
		if (!lproj)
		{
			um_vctovc(pe,clpt2);
			clpt2[2]= zmin;
			iret2 = ncl_projsf_clpoint(trad,frad,clpt2,ccpt2,thk,tol,UU_FALSE,0,
				&clpoints,UU_NULL);
			if (clpt2[2] >= pe[2]+tol)
				lproj = UU_TRUE;
		}				
		UU_LIST_EMPTY (&clpoints);

		if (!lproj)
		{
			um_vctovc(ps,clps.pte);
			um_vctovc(clvc,clps.vta);
			um_vctovc(pe,clpe.pte);
			um_vctovc(clvc,clpe.vta);
		    uu_list_push(&clptlist,&clps);
		    uu_list_push(&clptlist,&clpe);
		}
	}

	if (lproj)
	{
/*
.....Project the boundary portion from ps to pe
*/
		iret1 = UU_FAILURE;
		iret2 = UU_FAILURE;
		npts = UU_LIST_LENGTH(&pptr);
		pts = (UM_coord *) UU_LIST_ARRAY (&pptr);
		for (i = 0; i < npts; i++)
		{
			um_vctovc(pts[i],clpt1);
			clpt1[2]= zmin;
			iret1 = ncl_projsf_clpoint(trad,frad,clpt1,ccpt1,thk,tol,UU_FALSE,0,
				&clpoints,UU_NULL);
			if (iret1 == UU_SUCCESS && iret2 == UU_SUCCESS)
			{		
				ncl_projsf_clpoint_middle(trad,frad,thk,zmin,tol,clpt1,clpt2,UU_FALSE,0,&clpoints);
/*
.....Sort the clpts
*/
				um_vcmnvc(clpt1,clpt2,slvec);
				ncl_set_ldir (slvec);
				ncl_sort_clpts(&clpoints,slvec,0,2,tol);
				uu_list_push_list(&clptlist,&clpoints);
				UU_LIST_EMPTY (&clpoints);
			}
			if (iret1 == UU_SUCCESS)
			{
				iret2 = iret1;
				um_vctovc(clpt1,clpt2);
			}
		}
	}
#if 0	
	ncl_debug_clpts(&clptlist,0);
#endif
/*
.....Put the portion clpath to Sslpath
*/ 	
	ncl_sm_clpath_push_list(Sslpt_flag,&clptlist);
/*
.....Free alloctaed memory
*/
	uu_list_free(&pptr);
	uu_list_free(&clptlist);
	uu_list_free(&clpoints);
}

/*********************************************************************
**    E_FUNCTION     : ncl_smill_connect_clpath
**							(ntype,nstep,nend,trad,frad,thk,zmin,bound,tol)
**        Connect and optimize the clpath for SCRUB and COMBIN.
**    PARAMETERS
**       INPUT  :
**			ntype	- 0:SCRUB 1:COMBIN
**			nstep	- 0:scallob height 1:PASS number 2: Stepover
**			nend	- CL end condition 0:TO 1:PAST 2:ON 3:CONTCT
**			trad	- tool corner radius
**			flat	- tool flat radus
**			thk		- thick value
**			zmin	- min z
**          bound   - boundary
**          tol     - tolerance
**       OUTPUT :
**          none
**    RETURNS      :
**         UU_SUCCESS if on; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_smill_connect_clpath(ntype,nstep,nend,trad,frad,thk,zmin,bound,tol)
int ntype,nstep,nend;
UU_REAL trad,frad,thk,zmin,tol;
UU_LIST *bound;
{
	int i,i1,npaths,npts,status;
	UM_coord ps,pe,ps1,pe1,*pts;
	UU_LIST *clpts = UU_NULL;
	UU_LOGICAL lnext;
	UU_REAL	drad;
	drad = trad + frad - thk;
#if 0
	ncl_debug_pts(bound,4);
#endif
/*
.....Create cl path
*/
	if (nstep != 0)
		ncl_sm_clpath_new(Sslpt_flag);
	ncl_sm_clpath_create(Sslpt_flag);
#if 0
/*
.....Debug before toolpath connection
*/
	ncl_debug_clpath(Sclpt_flag);
#endif
/*
.....The first clpath
*/
	i = 0;
	npts = ncl_sm_clpath_getclpts(Sclpt_flag,i,&clpts);
	if (npts > 0)
	{
		ncl_sm_clpath_get_endpts(0,i,ps,pe);
		ncl_sm_clpath_push_list(Sslpt_flag,clpts);
		ncl_sm_clpath_empty(Sclpt_flag, i);
	}

	while (ncl_sm_clpath_not_empty(Sclpt_flag))
	{
		lnext = UU_FALSE;
		if (ncl_sm_clpath_connect_next(ntype,drad,ps,pe,i,&i1))
		{
/*
.....Get the next clpath start/en point
*/
			ncl_sm_clpath_get_endpts(Sclpt_flag,i1,ps1,pe1);
/*
.....Put clapth from pe to ps1 to Sslpath
*/  
			ncl_smill_clpath_between(nend,trad,frad,thk,zmin,
									bound,5.0*tol,pe,ps1);
/*
.....Put the next clpath to Sslpath
*/
			UU_LIST_EMPTY (clpts);
			ncl_sm_clpath_getclpts(Sclpt_flag,i1,&clpts);
			ncl_sm_clpath_push_list(Sslpt_flag,clpts);
			ncl_sm_clpath_empty(Sclpt_flag, i1);
			i = i1;
			um_vctovc(ps1,ps);
			um_vctovc(pe1,pe);
		}
		else
		{
			ncl_sm_clpath_create(Sslpt_flag);
			lnext = ncl_sm_clpath_next(Sclpt_flag,&i);
			if (!lnext)
				break;						
			ncl_sm_clpath_get_endpts(Sclpt_flag,i,ps,pe);

			ncl_sm_clpath_getclpts(Sclpt_flag,i,&clpts);
			ncl_sm_clpath_push_list(Sslpt_flag,clpts);
			ncl_sm_clpath_empty(Sclpt_flag, i);
		}
	}
#if 0
/*
.....Debug after toolpath connection
*/
	ncl_debug_clpath(Sslpt_flag);
/*
	ncl_debug_clpath(Sclpt_flag);
*/
#endif
}

/*********************************************************************
**    E_FUNCTION     : ncl_smill_toolpath_combin(ntype,slvec)
**        Create COMBIN pattern toolpath.
**    PARAMETERS
**       INPUT  :
**			ntype	- 0:SCRUB 1:COMBIN 2:LACES
**			slvec	- sorting vector
**       OUTPUT :
**          none
**    RETURNS      :
**         UU_SUCCESS if on; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_smill_toolpath_combin(ntype,slvec,tol)
int ntype;
UM_vector slvec;
UU_REAL tol;
{
	int i,i1,j,j1,icombin,npaths,nums,*nnums,nps,npts;
	UU_REAL dis2;
	UM_coord ps,pe,ps1,pe1;
	UU_LIST tmpclpts,tmpclnums;
	UM_clpt *pos;
	UU_LIST *sclpts = UU_NULL;
	UU_REAL um_sqdis_from_segment_2d();
/*
.....Initialize tempclpts for COMBIN
*/
	uu_list_init(&tmpclpts,sizeof(UM_clpt),200,200);	
	uu_list_init(&tmpclnums,sizeof(int),10,10);
/*
.....Get the numbers of slice clpath
*/		
	ncl_sm_clpath_getnpath(&Sslpt_flag,&npaths);

	icombin = 0;
	dis2 = 0.0;
	for (i = 0; i < npaths; i++)	
	{
/* 
.....Get the slice points in the ith-pass
*/	
		npts = ncl_sm_clpath_getclpts(Sslpt_flag,i,&sclpts);
		ncl_sm_clpath_get_endpts(Sslpt_flag,i,ps,pe);
/*
......Sort the clpath
*/
		ncl_sort_clpts(sclpts,slvec,icombin,ntype,tol);	

		if (icombin == 1)			
		{				
		    nums = UU_LIST_LENGTH(sclpts);				
		    if (nums > 0)		
		    {	
				uu_list_push_list(&tmpclpts,sclpts);
				uu_list_push(&tmpclnums,&nums);
				UU_LIST_EMPTY(sclpts);
			}
		}	

		if (i > 0)
			dis2 = um_sqdis_from_segment_2d(pe1,ps1,ps,UU_FALSE);
		if (i == 0 ||( i > 0 && dis2 > UM_FUZZ))
			++icombin;		
		um_vctovc (ps,ps1);
		um_vctovc (pe,pe1);
/*
......Combin toolpath by order
*/
		if (icombin == 1 || icombin == 2)		
		{	
			if (UU_LIST_LENGTH(sclpts) > 0)
			{
				ncl_sm_clpath_create(Sclpt_flag);
				ncl_sm_clpath_push_list(Sclpt_flag,sclpts);
				UU_LIST_EMPTY (sclpts);
			}
		}
		else if (icombin == 3)		
		{		
			if (UU_LIST_LENGTH(sclpts) > 0)
			{
				ncl_sm_clpath_create(Sclpt_flag);				
				ncl_sm_clpath_push_list(Sclpt_flag,sclpts);
				UU_LIST_EMPTY (sclpts);
			}
				
			if (UU_LIST_LENGTH(&tmpclpts) > 0)		
			{				
				nums = UU_LIST_LENGTH(&tmpclnums);
				if (nums > 1)					
				{
					nnums = (int*) UU_LIST_ARRAY(&tmpclnums);
					pos = (UM_clpt *) UU_LIST_ARRAY (&tmpclpts);
					ncl_sm_clpath_getnpath(&Sclpt_flag,&nps);
							
					j1 = 0;
					for (i1 = 0; i1 < nums; i1++)
					{
						ncl_sm_clpath_create(Sclpt_flag);
						for (j = 0; j < nnums[i1]; j++)
						{
							ncl_sm_clpath_push_at(Sclpt_flag,nps+i1,&pos[j1]);							
							j1++;						
						}	
					}									
				}						
				else if (nums == 1)				
				{										
					ncl_sm_clpath_create(Sclpt_flag);
					ncl_sm_clpath_push_list(Sclpt_flag,&tmpclpts);
				}
				UU_LIST_EMPTY (&tmpclpts);
				UU_LIST_EMPTY (&tmpclnums);
			}
			icombin = 1;
		}
#if 0	
		ncl_debug_clpts(sclpts,0);
#endif	
	}
	
/*
.....Free memery
*/
	uu_list_free(&tmpclpts);
	uu_list_free(&tmpclnums);
}

/*********************************************************************
**    E_FUNCTION     : ncl_bound_contour_intf (pts,npts,pol0,points,tol,tolsq)
**        Get the common contour bewteen boundary(pts,npts) and pol0.
**    PARAMETERS
**       INPUT  :
**			npts	- number of boundary points
**			pts		- points
**			pol0	- the contour polygon
**          tol     - tolerance
**			tolsq	- tolerance sq
**       OUTPUT :
**          points	The intf contours
**    RETURNS      :
**         UU_SUCCESS if on; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_bound_contour_intf(pts,npts,pol0,points,tol,tolsq)
UM_coord *pts;
int npts;
ncl_polygon *pol0;
UU_LIST **points;
UU_REAL tol,tolsq;
{	
	int status;
	ncl_polygon pockbndr;
	UU_REAL offdis;
	UU_LIST tangs;
	UU_LIST *points1 = UU_NULL;
	int ib = 0;
/*
.....Boundary polygon
*/
	ncl_init_polygon (&pockbndr,npts);
	pockbndr.num_contours = 1;
	pockbndr.np = (int *) uu_malloc(sizeof(int));
	pockbndr.box = (UM_2box *) uu_malloc(sizeof(UM_2box));
	ncl_bound_to_contour(npts,pts,ib,&pockbndr);
/*
.....Get the intersection portion between boundary and curve contour
*/
	status = ncl_polygon_clip (NCL_INTOF,&pockbndr,pol0,&pockbndr,tol,tolsq);
/*
..... offset the contour boundary
*/
	offdis = tol;
	uu_list_init (&tangs,sizeof(UM_vector),0,100);
	if (status == UU_SUCCESS)
		status = ncl_offset_out0(&points1,&pockbndr,&tangs,offdis,tol);

	offdis = -tol;
	if (status == UU_SUCCESS)					
		status = ncl_offset_out0(&points1,NULL,NULL,offdis,tol);
/*
.....Free boundary polygon memery
*/
	ncl_free_polygon (&pockbndr);
	uu_list_free(&tangs);

	*points = UU_NULL;
	*points = points1;

	return status;
}
