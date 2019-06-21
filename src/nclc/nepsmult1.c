/*********************************************************************
**    NAME         :  nepsmult1.c
**       CONTAINS:  Routines to support multiple part surfaces.
**
**              ncl_psmult_init_s
**              ncl_psmult_store_s 
**              ncl_psmult_gougck 
**              ncl_psmult_find_pts
**              ncl_psmult_find_srf
**              ncl_psmult_find_farpt
**              ncl_psmult_search_farpt
**              ncl_psmult_build_plane
**              ncl_cylinder_cross_ps
**              ncl_create_tool
**              ncl_update_tool
**              ncl_update_look
**              ncl_toolstruct_size
**              ncl_sftosf
**              ncl_inter_pln
**
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       nepsmult1.c , 25.1
**     DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:43
*********************************************************************/

#include "usysdef.h"
#include "umath.h"
#include "nclfc.h"
#include "ulist.h"
#include "ncldef.h"
#include "nccs.h"
#include "mgeom.h"
#include "mdeval.h"
#include "ycom.h"
#include "msrfddl.h"
#include "uminmax.h"
#include "nclpsmult.h"
#include "uminmax.h"

extern int NCLX_internal_geom;

typedef struct
{
	int isf;
	int nproj;
	NCL_proj_type type[5];
	UU_REAL dist[5];
	UU_REAL sf[5][10];
} tool_proj_struct;
static tool_proj_struct *tool_proj=0;
static int ntps=0;
static int last_gouged,last_select;
static UU_REAL last_s[10];
/*
... 2 cylinders representing the tool for net PS:
... NCL_tcyl_use = cylinder to determine which PS should be analyzed 
... NCL_tcyl_ps = cylinder to be used to set tool-PS relation 
*/

static UM_cylinder NCL_tcyl_ps;

char *uu_malloc();

/*
.....Debug flag
*/
#define DEBUGON 0
#ifdef DEBUGON
		static char tbuf[80];
		static char *dbgptyp[] = {"NCL_ON_SURFACE", "NCL_ON_CORNER",
			"NCL_ON_CLOSE_EXTENSION", "NCL_ON_SIDE_EXTENSION",
			"NCL_ON_FAR_EXTENSION", "NCL_DISCARD_SURFACE", "NCL_DISCARD_CORNER",
			"NCL_DISCARD"};
		static char *dbgpta[] = {"FIXED", "NORMAL", "ATANGL", "TANTO_DS",
			"FAN", "TANTO_PERPTO", "TANTO_PARELM", "NORMAL_PERPTO", "COMBIN",
			"COMBIN_PARELM", "ATANGL_PERPTO", "ATANGL_CLDIST",
			"ATANGL_CLDIST_PERPTO","THRU_PT","THRU_CV"};
#endif


/*********************************************************************
**    FUNCTION     : void ncl_psmult_init_s (nsf)
**      Allocate memory for tool projections from around cutter onto 
**      multiple part surfaces
**    PARAMETERS
**       INPUT  :
**           nsf - number of nearby surfaces
**       OUTPUT :    none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_psmult_init_s (nsf)
UM_int2 *nsf;
{
	int isiz;
	isiz = sizeof(tool_proj_struct) * *nsf;
	tool_proj = (tool_proj_struct *)uu_malloc(isiz);

	ntps = 0;
}

/*********************************************************************
**    FUNCTION     : void ncl_psmult_store_s (isf,ilook,ist,ien,iext,sf,dist)
**      Store tool-surface projection data, if the projection type is
**      ON_SURFACE, ON_CORNER, or ON_CLOSE_EXTENSION
**
**    PARAMETERS
**       INPUT  :
**           isf   - surface number
**           ilook - current look-point number
**           ist   - starting look-point number
**           ien   - number of look-points
**           iext  - extension type
**           sf    - surface data
**           dist  - surface distance
** 
**       OUTPUT : none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_psmult_store_s (isf,ilook,ist,ien,iext,sf,dist)
UM_int2 *isf,*ilook,*ist,*ien,*iext;
UM_real4 *sf;
UM_real8 *dist;
{
	int i,inc,j;
	NCL_proj_type extype;

	extype = (NCL_proj_type) *iext;
/*
.....Find available or allocated slot
*/
	if (extype == NCL_ON_SURFACE || extype == NCL_ON_CORNER ||
		extype == NCL_ON_CLOSE_EXTENSION)
	{
		for (i = 0; i < ntps; i++)
		{
			if (*isf == tool_proj[i].isf) break;
		}
/*
.....If this is the first time we store for this surface,
.....increment number of used surfaces
*/
		if (i == ntps)
		{
			ntps++;
			tool_proj[i].isf = *isf;
			tool_proj[i].nproj = 0;
		}
		inc = tool_proj[i].nproj;
/*
.....Store tool projection data
*/
		tool_proj[i].type[inc] = extype;
		tool_proj[i].dist[inc] = *dist;
		for (j=0;j<10;j++) tool_proj[i].sf[inc][j] = sf[j];
/*
.....Increment number of this surface projections
*/
		tool_proj[i].nproj++;
	}

	return;
}

/*********************************************************************
**    FUNCTION     : void ncl_psmult_gougck (tool,s,s1,tol,isf,iflg)
**      Replace the selected plane with a better one if some surfaces
**      are gouged.
** 
**    PARAMETERS
**       INPUT  :
**              tool - tool structure containing all info about tool
**   
**              s - array s(1-10,1) containing all info about current
**                      surface projection
**              s1 - previosly selected (before all special logic)
**                      surface projection
**              isf - number of currently selected surface
**              tol - tolerance
**       OUTPUT :
**              s - new current surface projection
**              iflg - flag showing the surface projection is changed
**             
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_psmult_gougck (tool,s,s1,tol,isf,iflg)
UM_int2 *isf,*iflg;
NCL_tool *tool;
UM_real4 *s,*s1;
UM_real8 *tol;
{
	int i,j,k,isrf,ipt,is,iptsel,stat;
	UU_REAL dmin,dmax,d;
	UU_REAL snew[10],slast[10];
/*
.....Initialize routine
*/
	ipt  = iptsel = -1;
	isrf = *isf;
	dmin = 10000;
/*
.....Debug output of surface used
*/
#if DEBUGON == 3
	{
		NclxDbgPstr(" ");
		sprintf(tbuf,"Selected surface = %d",*isf);
		NclxDbgPstr(tbuf);
		sprintf(tbuf,"Tolerance = %g",*tol);
		NclxDbgPstr(tbuf);
		sprintf(tbuf,"End point = %g,%g,%g",tool->end[0],
			tool->end[1],tool->end[2]);
		NclxDbgPstr(tbuf);
		sprintf(tbuf,"Tool axis = %g,%g,%g",tool->axis[0],
			tool->axis[1],tool->axis[2]);
		NclxDbgPstr(tbuf);
	}
#endif
	if (ntps < 2)
	{
		for (i=0;i<10;i++) s[i] = s1[i];
		ipt = 0;
		if (ntps == 0) ipt = -1;
		*iflg = 0;
		goto done;
	}
/*
.....Determine if any gouges
*/
	for (i=0;i<ntps;i++)
	{
/*
.....Debug output of logic type
*/
#if DEBUGON == 3
		{
			NclxDbgPstr(" ");
			for (j=0;j<tool_proj[i].nproj;j++)
			{
				sprintf(tbuf,"Projection type[%d][%d] = %s",tool_proj[i].isf,j,
					dbgptyp[tool_proj[i].type[j]]);
				NclxDbgPstr(tbuf);
				sprintf(tbuf,"Look point = %g,%g,%g",tool_proj[i].sf[j][7],
					tool_proj[i].sf[j][8],tool_proj[i].sf[j][9]);
				NclxDbgPstr(tbuf);
				sprintf(tbuf,"Surface point = %g,%g,%g",tool_proj[i].sf[j][4],
					tool_proj[i].sf[j][5],tool_proj[i].sf[j][6]);
				NclxDbgPstr(tbuf);
				sprintf(tbuf,"Surface plane = %g,%g,%g,%g",tool_proj[i].sf[j][0],
					tool_proj[i].sf[j][1],tool_proj[i].sf[j][2],
					tool_proj[i].sf[j][3]);
				NclxDbgPstr(tbuf);
				sprintf(tbuf,"Distance = %g",tool_proj[i].dist[j]);
				NclxDbgPstr(tbuf);
			}
		}
#endif
/*
.....Determine if surface is gouged
.....Don't check selected surface
*/
		if (tool_proj[i].isf == isrf) ipt = i;
		if (tool_proj[i].isf != isrf || *iflg == 1)
		{
			for (j=0;j<tool_proj[i].nproj;j++)
			{
/*
........Surface is gouged
........Save this s-table
				if (tool_proj[i].dist[j]+*tol < tool->corner_radius &&
					tool_proj[i].dist[j] < dmin &&
					tool_proj[i].type[j] == NCL_ON_SURFACE)
*/
				if (tool_proj[i].type[j] == NCL_ON_SURFACE || 
					tool_proj[i].type[j] == NCL_ON_CLOSE_EXTENSION)
				{
					UU_REAL dot1, EPS = 1.e-3, tcyl_rad;
					dot1 = um_dot (tool->axis, tool_proj[i].sf[j]);
/*
..... check if ncl_tool_ps_rel changed the surface normal direction to
..... make the distance positive, while in fact we do have a gouge
*/
					if (dot1 < -EPS)
					{
						tcyl_rad = NCL_tcyl_ps.radius;
						NCL_tcyl_ps.radius = tool->corner_radius + tool->cut_edge;
						if (um_point_is_in_cylinder(&tool_proj[i].sf[j][4],&NCL_tcyl_ps))
							tool_proj[i].dist[j] = - fabs (tool_proj[i].dist[j]);
						NCL_tcyl_ps.radius = tcyl_rad;
					}
					if (tool_proj[i].dist[j]+*tol < tool->corner_radius &&
						tool_proj[i].dist[j] < dmin)
					{
						dmin = tool_proj[i].dist[j];
						for (k=0;k<10;k++) snew[k] = tool_proj[i].sf[j][k];
						iptsel = i;
					}
				}
			}
		}
	}
/*
.....Surface is gouged
.....If selected surface is also gouged
.....Then let mover move to tool so that
.....the selected surface is no longer gouged
.....
.....Otherwise the created plane will be too short
.....(it does not go the entire width of the cutter)
.....And it would be invalid
*/
	if (dmin != 10000. && *iflg != 1)
	{
		d = tool->corner_radius - *tol;
		stat = ncl_psmult_find_pts(isrf,slast,&d);
		if (stat == UU_SUCCESS)
		{
			for (i=0;i<10;i++) s[i] = s1[i];
#if DEBUGON == 3
			{
				NclxDbgPstr("Both surfaces gouged.");
				sprintf(tbuf,"Old plane = %g,%g,%g,%g",s[0],s[1],s[2],s[3]);
				NclxDbgPstr(tbuf);
			}
#endif
			goto done;
		}
	}
/*
.....If no surface is gouged,
.....but we previously calculated a plane
.....then we need to calculate another plane
.....and not go back to the regular surface normal
*/
	if (dmin == 10000. && *iflg == 1)
	{
/*		if (isrf == last_select) *iflg = 0;
		else*/
		{
		stat = ncl_psmult_find_pts(last_gouged,snew,&dmin);
		if (stat == UU_SUCCESS) stat = ncl_psmult_find_srf(last_select,&ipt);
		if (stat != UU_SUCCESS) for (i=0;i<10;i++) 
			s[i] = (UM_real4) last_s[i];
		else
		{
			dmin = -10000.;
			ncl_psmult_find_srf(last_gouged,&iptsel);
		}
#if DEBUGON == 3
		{
			sprintf(tbuf,"No surface gouged - Last gouged = %d.",last_gouged);
			NclxDbgPstr(tbuf);
		}
#endif
		}
	}
/*
.....Found a surface that was gouged
*/
	if (iptsel >= 0 && ipt >= 0 && dmin != 10000.)
	{
#if DEBUGON == 3
		{
			NclxDbgPstr(" ");
			sprintf(tbuf,"Surface gouge distance = %g",dmin);
			NclxDbgPstr(tbuf);
		}
#endif
/*
........Selected surface is gouged
........Change selected surface with
........previously gouged surface
*/
		if (ipt == iptsel) 
		{
			for (i=0;i<ntps;i++)
			{
				if (tool_proj[i].isf == last_gouged) ipt = i;
			}
/*
........Last gouged and current gouged
........are the same, so try for last
........selected surface
*/
			if (ipt == iptsel)
			{
				if (tool_proj[ipt].isf != last_select)
					stat = ncl_psmult_find_farpt(last_select,snew,slast,&dmax);
				else
					stat = ncl_psmult_search_farpt(tool_proj[ipt].isf,snew,slast);
				if (stat == UU_SUCCESS)
				{
					*iflg = 1;
					ncl_psmult_build_plane(snew,slast,tool,iflg,s,dmin,tol);
				}
				else
/*
........Could not find last gouged surface
........So use last calculated plane
*/
				{
					for (i=0;i<10;i++) s[i] = (UM_real4) last_s[i];
#if DEBUGON == 3
		{
			sprintf(tbuf,"Gouged and selected surface are the same = %d",
				tool_proj[ipt].isf);
			NclxDbgPstr(tbuf);
			sprintf(tbuf,"New plane = %g,%g,%g,%g",s[0],s[1],s[2],s[3]);
			NclxDbgPstr(tbuf);
		}
#endif
				}
				last_gouged = tool_proj[ipt].isf;
				goto done;
			}
		}
/*
........Now find the furthest projection point
........on the selected surface
*/
		dmax = -1.;
		for (i=0;i<tool_proj[ipt].nproj;i++)
		{
			d = um_dcccc(&(tool_proj[ipt].sf[i][4]),&snew[4]);
			if (d > dmax)
			{
				is = i;
				dmax = d;
			}
		}
/*
.....Build a new S-table plane using
.....the points and normals from the two
.....different surface projection points
*/
		*iflg = 1;
		ncl_psmult_build_plane(snew,tool_proj[ipt].sf[is],tool,iflg,s,dmin,tol);
		last_gouged = tool_proj[iptsel].isf;
	}
/*
.....No gouges found
*/
	else
	{
/*
........See if selected surface is gouged
*/
		stat = UU_FAILURE;
		if (ipt != -1 && tool_proj[ipt].isf != last_select)
		{
/*
			dmin = tool->corner_radius - *tol;
*/
			dmin = tool->corner_radius + 5.*(*tol);
			stat = ncl_psmult_find_pts(tool_proj[ipt].isf,snew,&dmin);
		}
/*
........Surface is gouged
........See if previous checked surface is different
........If so build a transition plane between these
........two surfaces
*/
		if (stat == UU_SUCCESS)
		{
			if (*iflg == 1 && tool_proj[ipt].isf != last_select)
				stat = ncl_psmult_find_farpt(last_select,snew,slast,&dmax);
			else
				stat = ncl_psmult_search_farpt(tool_proj[ipt].isf,snew,slast);
			if (stat == UU_SUCCESS)
			{
				*iflg = 1;
				ncl_psmult_build_plane(snew,slast,tool,iflg,s,dmin,tol);
				last_gouged = tool_proj[ipt].isf;
			}
		}
/*
........Surface is not gouged
........Restore original S-table
*/
		if (stat != UU_SUCCESS)
		{
			for (i=0;i<10;i++) s[i] = s1[i];
			*iflg = 0;
#if DEBUGON == 3
			{
				NclxDbgPstr("Use old plane");
				sprintf(tbuf,"Old plane = %g,%g,%g,%g",s[0],s[1],s[2],s[3]);
				NclxDbgPstr(tbuf);
			}
#endif
		}
	}
/*
.....End of routine
*/
done:;
	if (ipt != -1) last_select = tool_proj[ipt].isf;
	if (tool_proj != 0) uu_free(tool_proj);
	tool_proj = 0;
	return;
}

/*********************************************************************
**    FUNCTION     : int ncl_psmult_find_pts(sf,snew,mindis)
**
**    PARAMETERS
**       INPUT  :
**       OUTPUT :
**    RETURNS      : UU_SUCCESS / UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_psmult_find_pts(sf,snew,mindis)
int sf;
UU_REAL *snew;
UU_REAL *mindis;
{
	int j,k,ipt,stat;
	UU_REAL dmin;
/*
.....Find the requested surface
*/
	stat = ncl_psmult_find_srf(sf,&ipt);
/*
.....Find Point closest to surface
.....from last gouged surface
*/
	if (stat == UU_SUCCESS)
	{
		dmin = *mindis;
		stat = UU_FAILURE;
		for (j=0;j<tool_proj[ipt].nproj;j++)
		{
			if (tool_proj[ipt].dist[j] < dmin &&
				(tool_proj[ipt].type[j] == NCL_ON_SURFACE ||
				tool_proj[ipt].type[j] == NCL_ON_CLOSE_EXTENSION))
			{
				dmin = tool_proj[ipt].dist[j];
				stat = UU_SUCCESS;
				for (k=0;k<10;k++) snew[k] = tool_proj[ipt].sf[j][k];
			}
		}
		*mindis = dmin;
	}
	return(stat);
}

/*********************************************************************
**    FUNCTION     : int ncl_psmult_find_srf(sf,ipt)
**
**    PARAMETERS
**       INPUT  :
**       OUTPUT :
**    RETURNS      : UU_SUCCESS / UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_psmult_find_srf(sf,ipt)
int sf,*ipt;
{
	int i,stat;
/*
.....Find selected surface in 'tool_proj' array
*/
	stat = UU_FAILURE;
	for (i=0;i<ntps;i++)
	{
		if (tool_proj[i].isf == sf)
		{
			*ipt = i;
			stat = UU_SUCCESS;
			break;
		}
	}
	return(stat);
}

/*********************************************************************
**    FUNCTION     : int ncl_psmult_find_farpt(isf,snew,sprev,maxdis)
**
**    PARAMETERS
**       INPUT  :
**       OUTPUT :
**    RETURNS      : UU_SUCCESS / UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_psmult_find_farpt(isf,snew,sprev,maxdis)
int isf;
UU_REAL snew[],sprev[],*maxdis;
{
	int i,is,ipt,stat;
	UU_REAL dmax,d;
/*
.....Find the requested surface
*/
	stat = ncl_psmult_find_srf(isf,&ipt);
/*
.....Find the furthest projection point
.....on the selected surface
*/
	if (stat == UU_SUCCESS)
	{
		dmax = -1.;
		stat = UU_FAILURE;
		for (i=0;i<tool_proj[ipt].nproj;i++)
		{
			d = um_dcccc(&(tool_proj[ipt].sf[i][4]),&snew[4]);
			if (d > dmax)
			{
				is = i;
				dmax = d;
				stat = UU_SUCCESS;
			}
		}
		if (stat == UU_SUCCESS)
		{
			for (i=0;i<10;i++) sprev[i] = tool_proj[ipt].sf[is][i];
			*maxdis = dmax;
		}
	}
	return(stat);
}

/*********************************************************************
**    FUNCTION     : int ncl_psmult_search_farpt(isf,snew,sprev)
**
**    PARAMETERS
**       INPUT  :
**       OUTPUT :
**    RETURNS      : UU_SUCCESS / UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_psmult_search_farpt(isf,snew,sprev)
int isf;
UU_REAL snew[],sprev[];
{
	int i,j,status,stat;
	UU_REAL dmax,d,s[10];
/*
.....Loop through all surfaces
.....except the selected surface
*/
	dmax = -1;
	stat = UU_FAILURE;
	for (i=0;i<ntps;i++)
	{
/*
.....Find the far surface point
*/
		if (tool_proj[i].isf != isf)
		{
			status = ncl_psmult_find_farpt(tool_proj[i].isf,snew,s,&d);
			if (status == UU_SUCCESS && d > dmax)
			{
				stat = UU_SUCCESS;
				dmax = d;
				for (j=0;j<10;j++) sprev[j] = s[j];
			}
		}
	}
	return(stat);
}

/*********************************************************************
**    FUNCTION  : int ncl_psmult_build_plane(snew,sprev,tool,iflg,s,dmin,tol)
**
**    PARAMETERS
**       INPUT  :
**       OUTPUT :
**    RETURNS      : UU_SUCCESS / UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_psmult_build_plane(snew,sprev,tool,iflg,s,dmin,tol)
UU_REAL snew[],sprev[];
UU_REAL dmin, *tol;
NCL_tool *tool;
UM_real4 s[];
UM_int2 *iflg;
{
	int i;
	UU_REAL d1,d2;
	UM_vector vec1,vec2,vec,tvec;
	UM_coord pt,vpt,pta,ptb,pt2;
/*
.....Is tool axis vector in same plane as
.....surface normals?
*/
	um_cross(&sprev[0],&snew[0],vec1);
	um_cross(vec1,tool->axis,vec2);
	um_cross(vec2,vec1,tvec);
	um_unitvc(tvec,tvec);
/*
.....No, so create 2 new contact
.....points which are furthest away
.....from each other
*/
	if (um_dot(tvec,tool->axis) < .999)
	{
/*
.....Create forward look point
*/
		um_cross(vec1,tvec,vec2);
		um_unitvc(vec2,vec2);
		um_vctmsc(vec2,tool->ring.radius,vec);
		um_vcplvc(tool->ring.center,vec,pta);
/*
.....Create backward look point
*/
		um_vctmsc(vec,-1.,vec1);
		um_vcplvc(tool->ring.center,vec1,ptb);
/*
.....Create points on gouged surface
*/
		um_nptpln(pta,&snew[4],&snew[0],pt);
		um_nptpln(ptb,&snew[4],&snew[0],pt2);
/*
.....Use the look point closest to the surface
*/
		d1 = (pta[0]*snew[0] + pta[1]*snew[1] + pta[2]*snew[2]) - snew[3];
		d2 = (ptb[0]*snew[0] + ptb[1]*snew[1] + ptb[2]*snew[2]) - snew[3];
		if (d2 < d1)
		{
			um_vctovc(pt2,pt);
			um_nptpln(pta,&sprev[4],&sprev[0],vpt);
		}
		else
			um_nptpln(ptb,&sprev[4],&sprev[0],vpt);
	}
	else
	{
		um_vctovc(&snew[4],pt);
		um_vctovc(&sprev[4],vpt);
	}
/*
.....Build a new S-table plane using
.....the points and normals from the two
.....different surface projection points
*/

	{
		UU_REAL phi,a1,a2,cof,sif,d,cr,rd,del,d0;
		UM_coord pc,newpt,pt1,pt2;
		UM_vector svec1,svec2;
	
		cr = tool->corner_radius;
		rd = 2.*tool->cut_edge;

		um_vctovc(&sprev[0],vec1);
		um_vctovc(&snew[0],vec2);
		um_cross (vec1,vec2,vec);
		um_unitvc (vec,vec);
/*
..... vec defines the plane of two surface normals
*/
		um_cross (vec,vec1,svec1);
		um_cross (vec2,vec,svec2);

		cof = um_dot(vec1,vec2);
		if (cof < UM_DFUZZ || (1-cof) < UM_DFUZZ) goto Snew;
		sif = sqrt (1. - cof*cof);
		phi = acos (cof);

		um_iplnpln (vpt,vec1,pt,vec2,pc,vec);
		um_nptln (tool->end,pc,vec, pc);
/*
..... pc is the "corner" point
*/
		d = um_dcccc (pc, tool->end);
		if (d > 2.*tool->diameter) 
		{
/*
..... if the "corner" point is far form the tool: when there is no gouge, and
..... the tool is not currently on the new plane, and the prev plane is farther
..... than the new one - pick the prev plane to enable going along a curved
..... surface; otherwise pick the new plane. 
*/
			if (dmin == -10000.)
			{
				d1 = um_dot (tool->end, &sprev[0]);
				d1 = d1 - sprev[3];
				d2 = um_dot (tool->end, &snew[0]);
				d2 = d2 - snew[3];

				d = um_dcccc (tool->end, pt);
				if (d > 10.* (*tol) && d1 > d2)
				{
					um_nullvc (&snew[0]);
					um_vctovc (vpt,pt);
				}
			}
			goto Snew;
		}

		um_vcplvc (svec1,svec2,vec);
/*
..... move the pc at the distance cr from both planes
*/
		um_avcplbvc (1.,pc,cr/sif,vec,pc);

		if (rd > 0.001)
		{
			UU_REAL alpha = 0.;
			UM_coord ppt;

			del = phi/50;
			d0 = d1 = d2 = 1000000.;

/*
..... select the best plane so that if the tool moves on it, it touches 
..... both the prev and the new plane, and gouges none
*/
			for (i = 0; i < 51; i++)
			{
				alpha = del*i;
				a2 = sin(alpha)/sif;
				a1 = sin(phi-alpha)/sif;
				um_avcplbvc (1.,pc,rd*a1,svec1,pt1);
				um_avcplbvc (1.,pc,rd*a2,svec2,pt2);
				um_vcmnvc (pt1,pt2,tvec);
				um_unitvc (tvec,tvec);
				um_middlept (pt1,pt2,newpt);
				um_vcmnvc (newpt,tool->end,vec);
				d = fabs (um_dot (vec,tvec));
				if (d >= d0) continue;
				d0 = d;
				d1 = a1; d2 = a2;
				um_vctovc (pt1,ppt);
				if (d0 < UM_FUZZ) break;
			}
			if (d1 < UM_DFUZZ || d2 < UM_DFUZZ)
			{
				if (d2 < UM_DFUZZ) 
				{
					um_vctovc (&sprev[0], &snew[0]);
					um_vctovc (vpt,pt);
					um_vctovc (pt,&snew[4]);
/*
					if (dmin == -10000.) *iflg = 0;
*/
				}
				goto Snew; 
			}
			um_avcplbvc (d1,vec1,d2,vec2,&snew[0]);
			um_avcplbvc (1.,ppt,-cr,&snew[0],pt);
			um_vctovc (pt,&snew[4]);
		}
		else
/*
..... the "ball tool" case
*/
		{
			if (dmin == -10000.) 
			{
				UM_coord pcc;
/*
..... if there is no gouge: if the tool is far from the prev plane, pick the new
..... one; if it is close to prev and far from new, pick the prev plane
*/
				um_avcplbvc (1.,tool->end,cr,tool->axis, pcc);
				d1 = um_dot (pcc, &sprev[0]);
				if (d1 > cr + sprev[3] + 10.*(*tol))
					goto Snew;
				d2 = um_dot (pcc, &snew[0]);
				if (d2 > cr + snew[3] + 10.*(*tol))
				{
					um_nullvc (&snew[0]);
					um_vctovc (vpt,pt);
					goto Snew;
				}
			}
			um_vcmnvc (pc,tool->end,vec);
			d = um_mag (vec);
/*
..... if the tool is far from the "corner" point, find the closest plane so
..... that when moved to it the tool touches both planes, and gouges none
*/
			if (d > UM_FUZZ)
			{
				um_unitvc (vec,vec);
				d = um_dot (vec,vec1);
				if (d <= cof) goto Snew;
				um_vctovc (vec,&snew[0]);
				um_avcplbvc(1.,pc,-cr,&snew[0],pt);
				um_vctovc (pt,&snew[4]);
			}
/*
..... if the tool is too close to the "corner" point, use the prev plane
*/
			else 
			{
				um_nullvc (&snew[0]);
				um_vctovc (vpt,pt);
			}
		}
	}

Snew:;
/*
.....Both normals are the same
.....Simply use one of the surface normals as is
*/
	if (um_mag(&snew[0]) == 0)
	{
		um_vctovc(&sprev[0],&snew[0]);
		*iflg = 0;
	}
/*
.....Make sure plane vector is in correct direction
*/
	if (um_dot(&sprev[0],&snew[0]) < 0.) um_vctmsc(&snew[0],-1.,&snew[0]);
	snew[3] = pt[0]*snew[0] + pt[1]*snew[1] + pt[2]*snew[2];

#if DEBUGON == 3
	{
		sprintf(tbuf,"Select point = %g,%g,%g",sprev[4],sprev[5],sprev[6]);
		NclxDbgPstr(tbuf);
		sprintf(tbuf,"Gouge point = %g,%g,%g",snew[4],snew[5],snew[6]);
		NclxDbgPstr(tbuf);
		sprintf(tbuf,"New gouge point = %g,%g,%g",pt[0],pt[1],pt[2]);
		NclxDbgPstr(tbuf);
		sprintf(tbuf,"New select point = %g,%g,%g",vpt[0],vpt[1],vpt[2]);
		NclxDbgPstr(tbuf);
		sprintf(tbuf,"New plane = %g,%g,%g,%g",snew[0],snew[1],snew[2],
			snew[3]);
		NclxDbgPstr(tbuf);
	}
#endif
/*
.....Save the calculated plane
*/
	for (i=0;i<10;i++) last_s[i] = s[i] = (UM_real4) snew[i];

	return (0);
}

/*********************************************************************
**    FUNCTION     : UU_LOGICAL ncl_cylinder_cross_ps (cyl,srf,dist)
**    Determines if tool cylinder crosses a part surface
**    PARAMETERS
**       INPUT  :
**           cyl - infinite cylinder representing the cutter
**           srf - pointer to one of the multi-PS
**       OUTPUT :
**           dist - distance from cyl to the srf box
**    RETURNS      :
**         UU_TRUE/UU_FALSE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL ncl_cylinder_cross_ps (cyl,srf,dist)
UM_cylinder *cyl;
NCL_psmult_rec *srf;
UU_REAL *dist;
{
   UU_LOGICAL res;
/*
... usual surface
*/
   *dist = 0.;
	if (srf->bskey <= 0)
	{
		UM_3D_box box;
		int i;
		UM_int2 type = NCLI_POINT;

		for (i=0; i<UM_3DBOX_NUM_VERT; i++) 
			from_unibase (srf->box.ver[i],box.ver[i],&type);

		res = um_3dbox_cross_cylinder (&box,cyl,dist);

	}
	else
/*
... CVonSF as PS: it's a ruled surface composed of infinite lines thru
... the curve points in the direction of the base surface normals at 
... the points; check if such a line crosses cylinder
*/
	{
		struct NCL_fixed_databag eptr, bs, *bsptr;
		struct UM_evsrfout evsrf;
		UM_transf tfm, *tf;
		UU_REAL bplm[4];
		UM_coord *uv,*pt;
		UM_line ln;
		int i,nb,nuv;
		UU_REAL dist0, dist1;

		dist1 = 0.;
		dist0 = 1.e9;

		eptr.key = srf->bskey;
/*
... get base surface data
*/
		if (ncl_retrieve_data_fixed (&eptr) != 0) return (UU_FALSE);

		bplm[0] = bplm[2] = 0; 
		bplm[1] = bplm[3] = 1;

		bsptr = &eptr;

		if (ncl_itsa_trimsrf (&eptr))
		{
			ncl_trimsrf_get_fixed (&eptr,&nb,bplm);
			bsptr  = (struct NCL_fixed_databag *) &bs;
			ncl_trimsrf_get_bs (&eptr,&bsptr);
		}

		uc_init_evsrfout (bsptr, &evsrf);
		tf = &tfm;

		if(ncl_trimsrf_get_tf (&eptr, &tf) == UU_SUCCESS)
		{
			uv = (UM_coord *) UU_LIST_ARRAY(srf->bound.uvpts);
			pt = (UM_coord *) UU_LIST_ARRAY(srf->bound.cvpts);
         nuv = UU_LIST_LENGTH(srf->bound.uvpts);

			for(i=0; i< nuv; i++)
			{
				if (ncl_evsrf_tf (UM_NORM, uv[i][0], uv[i][1], bsptr, 
                                        tf, &evsrf) == UU_SUCCESS)
				{
					um_vctovc(pt[i],ln.p0);
					um_unitvc (evsrf.snorm,ln.n);
/*
..... Changed the following call to have the distance if there is
..... no intersection; also to skip calculation of the intersection
..... points p1, p2.
					res = (um_line_cross_cylinder (&ln,cyl,p1,p2) >= 0);
*/
					res = (um_line_cross_cylinder1 (&ln,cyl,&dist1) >= 0);
					if (res) return (res);
					if (dist1 < dist0) dist0 = dist1;
				}
			}

   		if (dist0 > 0.) *dist = dist0;
		}
	}

	return (res);
}

/*********************************************************************
**    FUNCTION : void ncl_create_tool (tool,tend,taxis,fwd,tfwd)
**    PARAMETERS
**       INPUT  :
**                tend     - tool end coordinates
**                taxis    - tool axis vector
**                fwd      - forward vector t(7-9,ia)
**                tfwd     - forward vector perpto taxis
**       OUTPUT :
**              tool  - tool structure without look_pt & look_vec fields;
**    RETURNS      :
**          UU_SUCCESS 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_create_tool (tool,tend,taxis,fwd,tfwd)
NCL_tool *tool;
UM_real4 *tend,*taxis,*fwd,*tfwd;
{
	int i;
	UM_int2 id;

	for (i=0;i<3;i++) tool->end[i]  = tend[i];
	for (i=0;i<3;i++) tool->axis[i] = taxis[i];
	for (i=0;i<3;i++) tool->forward[i]  = tfwd[i];
	for (i=0;i<3;i++) tool->end_forward[i]  = fwd[i];

	id = 1;
	gettool (&id ,&tool->diameter);
	id = 2;
	gettool (&id ,&tool->corner_radius);
	id = 3;
	gettool (&id ,&tool->height);
	id = 6;
	gettool (&id ,&tool->cut_edge);

	tool->ring.radius = tool->cut_edge;
	um_vctovc (tool->axis,tool->ring.n);
	um_translate_point (tool->end,tool->corner_radius,tool->axis,
				tool->ring.center);
}

/*********************************************************************
**    FUNCTION : void ncl_update_tool (tool,pt,vec)
**    PARAMETERS
**       INPUT  :
**                pt   - tool look pt s(8-10;1)
**                vec  - vector from pt to center of the tool inner ring
**       OUTPUT :
**                tool - tool structure fields look_pt & look_vec 
**                       are filled;
**    RETURNS      :
**          none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_update_tool (tool,pt,vec)
NCL_tool *tool;
UM_real4 *pt,*vec;
{
	int i;

	for (i=0;i<3;i++) tool->look_pt[i]  = pt[i];
	for (i=0;i<3;i++) tool->look_vec[i]  = vec[i];

}

/*********************************************************************
**    FUNCTION : void ncl_update_look (tool,pt,vec)
**    PARAMETERS
**       INPUT :
**              tool  - tool structure 
**       OUTPUT :
**                pt   - tool look pt s(8-10;1)
**                vec  - vector from pt to center of the tool inner ring
**    RETURNS      :
**          none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_update_look (tool,pt,vec)
NCL_tool *tool;
UM_real4 *pt;
UM_real4 *vec;
{
	int i;

	for (i=0;i<3;i++) pt[i] = tool->look_pt[i];
	for (i=0;i<3;i++) vec[i] = tool->look_vec[i];
}

/*********************************************************************
**    FUNCTION : int ncl_toolstruct_size ()
**    Returns dimension of NCL_tool structure as real*8 array
**    PARAMETERS
**       INPUT :
**       OUTPUT :
**    RETURNS      :
**          dimension of NCL_tool structure as real*8 array
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_toolstruct_size ()
{
	int size;

	size = sizeof (NCL_tool)/sizeof (UM_real8);

	return (size);
}

/*********************************************************************
**    FUNCTION : int ncl_sftosf (s1,s2)
**		copy surface structure s1 to s2.
**    PARAMETERS
**       INPUT :
**               s1 -  surface structure to copy from
**               s2  - surface structure to copy to
**       OUTPUT :
**					  none
**    RETURNS :
**             none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_sftosf(s1,s2)
NCL_surf_struct *s1,*s2;
{
	int status = UU_SUCCESS;

	um_vctovc(s1->normal,s2->normal);
	s2->distance = s1->distance;
	um_vctovc(s1->pt,s2->pt);
	um_vctovc(s1->uv,s2->uv);
	s2->thick = s1->thick;
	
	return(status);
}

/*********************************************************************
**    FUNCTION     : void ncl_inter_pln (tool,s0,s1,s,tol8)
**      Replace the two planes with a better one for a given tool position.
**      Currently called from fmill.f
** 
**    PARAMETERS
**       INPUT  :
**              tool - tool structure containing all info about tool
**              s - array s(1-10,1) containing all info about current
**                      surface projection
**              s1 - previosly selected (before all special logic)
**                      surface projection
**              tol8 - tolerance
**       OUTPUT :
**              s - new current surface projection
**             
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_inter_pln (tool,s0,s1,s,tol8)
NCL_tool *tool;
UM_real8 *s0,*s1;
UM_real4 *s;
UM_real8 *tol8;
{
	UU_REAL tol,dmin;
	UM_int2 iflg;

	tol = *tol8;
	dmin = -10000.;
	iflg = 1;

	ncl_psmult_build_plane(s1,s0,tool,&iflg,s,dmin,&tol);
}
