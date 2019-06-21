/*********************************************************************
**    NAME         :  nesmill.c
**       CONTAINS:  main routines for smill
**
**		ncl_clpt_cmpXL()
**		ncl_clpt_cmpYL()
**		ncl_clpt_cmpXS()
**		ncl_clpt_cmpYS()
**		ncl_sort_clpts()
**		ncl_smill_keys()
**		ncl_smill()
**		ncl_set_nclpln()
**		ncl_copy_nclpln()
**		ncl_get_point_normal()
**		ncl_smill_contct_point()
**		ncl_smill_contct_points()
**		ncl_smill_slice_endpoints()
**		ncl_smill_create_contour()
**		ncl_smill_auto_boundary()
**		ncl_smill_input_boundary()
**		ncl_smill_drive_planes()
**		ncl_smill_transf_clpts()
**		ncl_smill_get_erad()
**    ncl_smill_check_slices()
**		ncl_smill_stepover_toolpath()
**		ncl_smill_scallop_toolpath()
**		ncl_smill_create()
**		ncl_smfin()
**    COPYRIGHT 2010 (c) Numerical Control Computer Sciences Inc.
**                          All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       nesmill.c , 24.2
**    DATE AND TIME OF LAST MODIFICATION
**       10/22/13 , 11:25:30
*********************************************************************/
#include "nclfc.h"
#include "mdattr.h"
#include "mattr.h"
#include "mgeom.h"
#include "uminmax.h"
#include "mdpick.h"
#include "nclwaterln.h"
#include "nclclip.h"
/*
.....Use OpenMP libary
*/
#ifdef _OPENMP
#include <omp.h>
#endif

UU_LIST Serads; /*bullnose effective radius list*/
extern int Sclpt_flag;	/*Sclpath flag*/
extern int Sslpt_flag;	/*Sslpath flag*/

int NPT = 0;
int sfnum = 0;
int asfnum = 0;
int bsfnum = 0;
int csfnum = 0;
int Slayer = 0;
UU_LIST sfky;
UU_LIST pptr;
UM_int2 mm;
UM_int2 wbatch = 0;
UU_KEY_ID Sdpl[2];
UU_KEY_ID Sbnd;
UU_KEY_ID Sspt;
UU_REAL *sfs_xmmx = UU_NULL, *sfs_ymmx = UU_NULL;
NCL_waterline_surf *sff = UU_NULL;
UU_REAL wtol,wtolsq;
UM_real8 tool[6];

typedef struct _SM_segment
{
	int index;				/*slice index*/
	UM_coord p1;			/*the first point*/
	UM_coord p2;			/*the second point*/
} SM_segment;

/*********************************************************************
**    E_FUNCTION: ncl_clptcmpXL(e1,e2)
**       Comparison routine for the sort algorithm (uu_qsort).  
**    PARAMETERS
**       INPUT  :
**          pt1     - first element to be compared 
**          pt2     - second element
**       OUTPUT :
**    RETURNS      :  -1 if pt1[0] < pt2[0]
**                     0 if pt1[0] = pt2[0]
**                     1 if pt1[0] > pt2[0]
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_clpt_cmpXL(pt1,pt2)
UM_clpt *pt1,*pt2;
{
	if (pt1->pte[0] > pt2->pte[0])
		return(1);
	else if (pt1->pte[0] < pt2->pte[0])
		return(-1);
	return(0);
}

/*********************************************************************
**    E_FUNCTION: ncl_clptcmpYL(e1,e2)
**       Comparison routine for the sort algorithm (uu_qsort).  
**    PARAMETERS
**       INPUT  :
**          pt1     - first element to be compared 
**          pt2     - second element
**       OUTPUT :
**    RETURNS      :  -1 if pt1[1] < pt2[1]
**                     0 if pt1[1] = pt2[1]
**                     1 if pt1[1] > pt2[1]
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_clpt_cmpYL(pt1,pt2)
UM_clpt *pt1,*pt2;
{
	if (pt1->pte[1] > pt2->pte[1])
		return(1);
	else if (pt1->pte[1] < pt2->pte[1])
		return(-1);
	return(0);
}

/*********************************************************************
**    E_FUNCTION: ncl_clptcmpXS(e1,e2)
**       Comparison routine for the sort algorithm (uu_qsort).  
**    PARAMETERS
**       INPUT  :
**          pt1     - first element to be compared 
**          pt2     - second element
**       OUTPUT :
**    RETURNS      :  -1 if pt1[0] > pt2[0]
**                     0 if pt1[0] = pt2[0]
**                     1 if pt1[0] < pt2[0]
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_clpt_cmpXS(pt1,pt2)
UM_clpt *pt1,*pt2;
{
	if (pt1->pte[0] < pt2->pte[0])
		return(1);
	else if (pt1->pte[0] > pt2->pte[0])
		return(-1);
	return(0);
}

/*********************************************************************
**    E_FUNCTION: ncl_clptcmpYS(e1,e2)
**       Comparison routine for the sort algorithm (uu_qsort).  
**    PARAMETERS
**       INPUT  :
**          pt1     - first element to be compared 
**          pt2     - second element
**       OUTPUT :
**    RETURNS      :  -1 if pt1[0] > pt2[0]
**                     0 if pt1[0] = pt2[0]
**                     1 if pt1[0] < pt2[0]
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_clpt_cmpYS(pt1,pt2)
UM_clpt *pt1,*pt2;
{
	if (pt1->pte[1] < pt2->pte[1])
		return(1);
	else if (pt1->pte[1] > pt2->pte[1])
		return(-1);
	return(0);
}

/*********************************************************************
**    E_FUNCTION: ncl_weed_clpts(clptlist,tol)
**       unnecessary points if tolerance is satisfied. 
**    PARAMETERS
**       INPUT  :
**          clptlist    - clptlist
**          tol			- tol
**       OUTPUT :
**          clptlist    - weeped clptlist
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_weed_clpts(clptlist,tol)
UU_LIST *clptlist;
UU_REAL tol;
{
	int i,idel,npts;
	UU_REAL tol1;
	UM_clpt *clpts;
	UM_coord ps;

	tol1 = 0.25 * tol * tol;
	npts = UU_LIST_LENGTH(clptlist);
	clpts = (UM_clpt *) UU_LIST_ARRAY (clptlist);
#if 0
	ncl_debug_clpts(clptlist,0);
#endif
	um_vctovc(clpts[0].pte,ps);
	for (i = 0; i < npts-2; i++)
	{
		if (um_points_within_tol(ps,clpts[i+1].pte,clpts[i+2].pte,tol1))
		{
			idel = i + 1;
			uu_list_delete (clptlist,idel,1);
			clpts = (UM_clpt *)UU_LIST_ARRAY(clptlist);
			i--;
			npts--;
		}
		else
			um_vctovc(clpts[i+1].pte,ps);
	}
}

/*********************************************************************
**    E_FUNCTION: ncl_sort_clpts(clptlist,vec,i,ntype,tol)
**       Sort clptlist along vec.  
**    PARAMETERS
**       INPUT  :
**          clptlist    - clptlist
**          vec			- vector
**			i			- index for Scrub
**			ntype		- 0:SCRUB 2: LACE
**       OUTPUT :
**          clptlist    - sorted clptlist
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_sort_clpts(clptlist,vec,i,ntype,tol)
UU_LIST *clptlist;
UM_vector vec;
int i,ntype;
UU_REAL tol;
{	
/*
.....SCRUB
*/
	if (ntype == 0) 
	{
		if (fabs(vec[0]) > UM_FUZZ)
		{
			if ((i % 2) == 1) 
				uu_list_sort (clptlist,ncl_clpt_cmpXL);
			else
				uu_list_sort (clptlist,ncl_clpt_cmpXS);
		}
		else 
		{
			if ((i % 2) == 1) 
				uu_list_sort (clptlist,ncl_clpt_cmpYL);
			else
				uu_list_sort (clptlist,ncl_clpt_cmpYS);
		}
	}
	else if (ntype == 1) 
	{
/*
.....COMBIN
*/
		if (fabs(vec[0]) > UM_FUZZ)
		{
			if ((i % 2) == 1 || i == 0) 
				uu_list_sort (clptlist,ncl_clpt_cmpXL);
			else
				uu_list_sort (clptlist,ncl_clpt_cmpXS);
		}
		else 
		{
			if ((i % 2) == 1 || i == 0) 
				uu_list_sort (clptlist,ncl_clpt_cmpYL);
			else
				uu_list_sort (clptlist,ncl_clpt_cmpYS);
		}
	}
	else if (ntype == 2)
	{
/*
.....LACE
*/
		if (fabs(vec[0]) > UM_FUZZ)
		{
			if (vec[0] > UM_FUZZ)		
				uu_list_sort (clptlist,ncl_clpt_cmpXL);
			else 
				uu_list_sort (clptlist,ncl_clpt_cmpXS);
		}
		else
		{
			if (vec[1] > UM_FUZZ)
				uu_list_sort (clptlist,ncl_clpt_cmpYL);
			else
				uu_list_sort (clptlist,ncl_clpt_cmpYS);
		}
	}
/*
...weed out unnecessary points if tolerance is satisfied.
*/
	ncl_weed_clpts(clptlist,tol);
}

/*********************************************************************
**    E_FUNCTION     : void ncl_smill_sfkeys(lnum,nsf4,ksf8,kdpl8,ier)
**       Get all the surface keys and drive plane keys for SMILL
**    PARAMETERS
**       INPUT  :
**          lnum            layer number
**			nsf4			surfaces numbers
**			ksf8			surface data
**			ndrv4			1:drive vector 2:drive plane
**			kdpl8			drive vector / drive plane
**			nbnd4			0:auto contour boundary 1:input boundary curve
**			kbnd8			boundary curve
**			kspt8			START point
**       OUTPUT :
**          ier               error (zero if none)
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_smill_keys(lnum,nsf4,ksf8,ndrv4,kdpl8,nbnd4,kbnd8,kspt8,ier)
UM_int2 *lnum,*ier;
UM_int2 *nsf4,*ndrv4,*nbnd4;
UM_real8 *ksf8,*kdpl8,*kbnd8,*kspt8;
{
	int isf,layer,dum,nsf;
	UM_int2 ffinis;
	UM_int2 idx,ifl86;
	UM_real4 dup,ddn;
	UU_KEY_ID nclkey;
	UU_KEY_ID *keys;
	UM_int2 nwds, ietype;
	struct NCL_fixed_databag e;
	UM_sgeo geo;
	int bottype,poctype,frmtype,conpoc;
	UU_REAL mxtol2;
	UU_REAL sfasw;
	NCL_w_setup wset;
	NCL_w_param wpar;

	Slayer = *lnum;
	nsf = *nsf4;
/*
.....Get the layer surface keys
*/
	if (Slayer >= 0)
	{
		vxlfst();
		idx = 35; getifl(&idx,&wbatch);
		while (UU_TRUE)
		{
			if (!ncl_vxlnxt (&nclkey,&ietype)) break;

			if (ietype == NCLI_SURF || ietype == NCLI_SOLID)
			{
				um_get_attrib (&nclkey,&dum,&dum,&dum,&dum,&layer);
				if (layer == Slayer)
				{
					e.key = nclkey;
					if (ncl_retrieve_data_fixed(&e) == UU_SUCCESS)
					{
						geo.key = e.key;
						geo.relnum = e.rel_num;
						nclu_push_sfkey (&sfky,&geo);
					}
				}
			}
			ckintr(&ifl86,&wbatch);
			idx = 86; getifl(&idx,&ifl86);
			if (ifl86 != 0)
			{
				*ier = 36; return;
			}
		}

		if (sfky.cur_cnt < 1)
		{
			*ier = 36; return;
		}
		asfnum = sfnum = sfky.cur_cnt;

		ncl_getnum_listkey (LIST_B,&bsfnum);
		if (bsfnum > 0)
		{
			ncl_get_listkeys (LIST_B,&keys);
			uu_list_push_multiple (&sfky,bsfnum,keys);
			sfnum += bsfnum;
		}

		ncl_getnum_listkey (LIST_C,&csfnum);
		if (csfnum > 0)
		{
			ncl_get_listkeys (LIST_C,&keys);
			uu_list_push_multiple (&sfky,csfnum,keys);
			sfnum += csfnum;
		}
	}
	else
	{
		for (isf = 1; isf <= nsf; isf++)
		{
			sfasw = ksf8[isf-1];
			gtdesc(&sfasw,&nclkey,&nwds,&ietype);
			uu_list_push (&sfky,&nclkey);
		}
		sfnum = sfky.cur_cnt;
	}
/*
.....Drive plane or drive vector key
*/
	for (isf = 1; isf <= *ndrv4; isf++)
	{
		sfasw = kdpl8[isf-1];
		gtdesc(&sfasw,&nclkey,&nwds,&ietype);
		Sdpl[isf-1] = nclkey;
	}
/*
.....boundary curve key
*/
	if (*nbnd4 == 1)
	{
		sfasw = *kbnd8;
		gtdesc(&sfasw,&nclkey,&nwds,&ietype);
		Sbnd = nclkey;
	}
/*
.....START point
*/
	if (*kspt8 != 0)
	{
		sfasw = *kspt8;
		gtdesc(&sfasw,&nclkey,&nwds,&ietype);
		Sspt = nclkey;
	}
	else
	{
		Sspt = 0;
	}
}

/*********************************************************************
**    E_FUNCTION     : void ncl_smill(lnum,ier)
**       Fortran interface for the main smill routine
**    PARAMETERS
**       INPUT  :
**          lnum            layer number
**			nsf4			surfaces number
**			ksf8			surface data
**			ndrv4			1:drive vector 2:drive plane
**			kdpl8           drive plane
**			nend4			CL end condition 0: TO 1: PAST 2:ON 3:CONTCT
**			nbnd4			0:auto contour boundary 1:input boundary curve
**			kbnd8			boundary curve
**			nstep4			0:scallob height 1:PASS number 2: Stepover
**			kpas8			scallop height or pass number value
**			kstp8		    stepover value
**			kspt8			START point
**			ntype4			0: SCRUB 1:COMBIN 2:LACE
**       OUTPUT :
**          ier               error (zero if none)
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_smill(lnum,nsf4,ksf8,nend4,ndrv4,kpl8,nbnd4,kbnd8,
			   nstep4,kpas8,kstp8,kspt8,ntype4,ier)
UM_int2 *lnum,*ier;
UM_int2 *nsf4,*nend4,*ndrv4,*nbnd4,*nstep4,*ntype4;
UM_real8 *ksf8,*kpl8,*kbnd8,*kpas8,*kstp8,*kspt8;
{
	int status,ifl,val;
/*f
.....Initilize surface key list
*/
	uu_list_init(&sfky,sizeof(UU_KEY_ID),100,100);
/*
.....Get the surfaces/drive plane/vector/boundary,START point keys
*/
	ncl_smill_keys(lnum,nsf4,ksf8,ndrv4,kpl8,nbnd4,kbnd8,kspt8,ier);
	if (*ier != 0)
		return;

	status = ncl_smill_create(nend4,ndrv4,nbnd4,nstep4,kpas8,kstp8,ntype4);
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
**    E_FUNCTION     :  ncl_set_nclpln
**       Set  a plane given srub direction
**    PARAMETERS
**       INPUT  :
**          pt     - the point
**          nvec   - the unit vector
**       OUTPUT :
**          pl     - the plane
**    RETURNS      : UU_SUCCESS / UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_set_nclpln (pl,pt,nvec)
struct NCL_nclpl_rec *pl;
UM_coord pt;
UM_vector nvec;
{
	int status = UU_SUCCESS;
	pl->pt[0] = pt[0];
	pl->pt[1] = pt[1]; 
	pl->pt[2] = pt[2];
	pl->nvec[0] = nvec[0]; 
	pl->nvec[1] = nvec[1]; 
	pl->nvec[2] = nvec[2];
	return (status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_copy_cspln (pla,plb)
**       Copy all fields from pla to plb.
*********************************************************************/
void ncl_copy_nclpln (pla,plb)
struct NCL_nclpl_rec *pla,*plb;
{
	plb->pt[0] = pla->pt[0];
	plb->pt[1] = pla->pt[1]; 
	plb->pt[2] = pla->pt[2];
	plb->nvec[0] = pla->nvec[0]; 
	plb->nvec[1] = pla->nvec[1]; 
	plb->nvec[2] = pla->nvec[2];
}

/*********************************************************************
**    E_FUNCTION   : ncl_get_point_normal(pt,trad,tol,nvec)
**       Intermediate smill routine.
**       INPUT  :
**			pt			- point
**			trad		- tool radius
**			tol			- tolerance
**       OUTPUT :
**			clpt		- initial cl point
**			nvec		- unit normal vecot
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_get_point_normal(pt,trad,tol,clpt,nvec)
UM_coord pt,clpt;
UU_REAL trad,tol;
UM_vector nvec;
{
	int i,j,ntri,status;
	UU_REAL zmax,clmaxz;
	UM_trian *ptri,zmaxtri;
	UU_LIST trianlst;
	UM_coord ccpt;
	UM_vector v2,v3,vcross;
	UU_LOGICAL linside;
/*
	char tbuf[80];
*/
	status = UU_FAILURE;
	linside = UU_FALSE;
	uu_list_init0(&trianlst);

	um_vctovc(pt,clpt);
	zmax = -100000;
	clpt[2] = zmax;
	clmaxz = zmax;
/*
	sprintf(tbuf,"$$The point to interest"); 
	NclxDbgPstr(tbuf);
	ncl_debug_clpt(pt,4);
*/
/*
.....Get the triangle list within the trad of clpt
*/
	uu_list_init (&trianlst,sizeof (UM_trian),100,100);
	ncl_sfbucket_getpt_trianlst(pt,trad,&trianlst,UU_FALSE);

	ntri = trianlst.cur_cnt;
	ptri = (UM_trian *)UU_LIST_ARRAY(&trianlst);
	for (j = 0; j < ntri; j++, ++ptri)
	{
		linside = UU_FALSE;
		linside = ncl_facet_contact(0.0,0.0,ptri,clpt,&clmaxz,10.*tol,ccpt);
		if (linside > 0 && clmaxz > zmax)
		{
			zmax = clmaxz;
			um_vctovc(ptri->p1,zmaxtri.p1);
			um_vctovc(ptri->p2,zmaxtri.p2);
			um_vctovc(ptri->p3,zmaxtri.p3);
		}
	}
/*
	ncl_debug_triangle(j,0,0,0,&zmaxtri);
*/
/*
.....triangle normal
*/
	um_vcmnvc(zmaxtri.p2,zmaxtri.p1,v2);
	um_vcmnvc(zmaxtri.p3,zmaxtri.p1,v3);
 	um_cross(v2,v3,vcross);
	um_unitvc(vcross,nvec);
	if (nvec[2] < -0.15)
		um_negvc(nvec,nvec);  
/*
	sprintf(tbuf,"$$The intersedted point"); 
	NclxDbgPstr(tbuf);
	ncl_debug_clpt(clpt,6);
	sprintf(tbuf,"pntvec/%8.5f,%8.5f,%8.5f,%8.5f,%8.5f,%8.5f",
				clpt[0],clpt[1],clpt[2],nvec[0],nvec[1],nvec[2]);
	NclxDbgPstr(tbuf);
*/
	clpt[2] = zmax;
	if (trianlst.data)
		uu_list_free (&trianlst);

	return status;
}

/*********************************************************************
**    E_FUNCTION   : ncl_smill_contct_point(clpt,trad,zmin,slvec,tol,nvec,pt)
**       Get the cl point(pt) along slvec with contact point
** ..... on the 3d boundary within tolerance
**       INPUT  :
**			clpt		- inital cl point
**			trad		- tool radius
**			zmin		- min zvalue for project
**			slvec		- lace slice vector
**			tol			- tolerance
**			nvec		- inital cc point vector
**       OUTPUT :
**			pt		- initial cl point
**			nvec		- unit normal vecot
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_smill_contct_point(clpt1,clpt2,trad,frad,thk,zmin,tol,clpt,ccpt)
UU_REAL trad,frad,thk,zmin,tol;
UM_coord clpt1,clpt2,clpt,ccpt;
{
	int i,iret,iret1,nsteps;
	UU_REAL dis,dis12;
	UM_coord clptm,ccptm,clptm1;
	UM_2Dcoord vec;
	UU_LIST clpoints;
	char tbuf[80];
	UU_LOGICAL lfirst = UU_FALSE;
		
	iret = UU_FAILURE;
	iret1 = UU_FAILURE;

	uu_list_init(&clpoints,sizeof(UM_clpt),100,100);

	um_vcmnvc_2d(clpt2,clpt1,vec);
	um_unitvc_2d(vec,vec);
	dis12 = UM_DIST_2D(clpt1,clpt2);
	dis = 5.0 * tol;
	nsteps = dis12/dis;

	for (i = 1; i < nsteps; i++)
	{
		if (i == 1)	
			um_vcplbvc_2d(clpt1,dis,vec,clptm);
		else
			um_vcplbvc_2d(clptm1,dis,vec,clptm);
		um_vctovc(clptm,clptm1);

		clptm[2] = zmin;
		iret = ncl_projsf_clpoint(trad,frad,clptm,ccptm,
							thk,tol,UU_FALSE,0,&clpoints,UU_NULL);
#if 0
		ncl_debug_clpt(clptm,8);
		ncl_debug_clpt2(clptm,ccptm,8);
#endif	
		if (iret == UU_SUCCESS)
		{
			iret1 = ncl_point_on_bound3d(ccptm,2.0*trad,20*tol);
			if (iret1 == UU_SUCCESS)
			{
				um_vctovc(clptm,clpt);
				um_vctovc(ccptm,ccpt);
				break;
			}
			else
			{
				if (!lfirst)
				{
					um_vctovc(clptm,clpt);
					um_vctovc(ccptm,ccpt);
					dis = 0.1 * tol;
					um_negvc_2d(vec,vec);  
					lfirst = UU_TRUE;
				}
				else if (lfirst)
				{
					um_vctovc(clptm,clpt);
					um_vctovc(ccptm,ccpt);
				}
			}
		}
	}
	
	uu_list_free(&clpoints);
	return iret1;
}

/*********************************************************************
**    E_FUNCTION     : ncl_debug_contct_points2(clptlist)
**       Debug conact point routine.
**    PARAMETERS
**       INPUT  :
**			clpt2	- cl points
**			ccpt2	- cc points
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_debug_contct_point(clpt2,ccpt2)
UM_coord clpt2,ccpt2;
{
	char tbuf[80];
	sprintf(tbuf,"draft/modify,color = 6"); 
	NclxDbgPstr(tbuf);
	sprintf(tbuf,"clspt=PT/%8.5f,%8.5f,%8.5f", clpt2[0],clpt2[1],clpt2[2]);
	NclxDbgPstr(tbuf);
	sprintf(tbuf,"draft/modify,color = 7"); 
	NclxDbgPstr(tbuf);
	sprintf(tbuf,"ccspt=PT/%8.5f,%8.5f,%8.5f",ccpt2[0],ccpt2[1],ccpt2[2]);
	NclxDbgPstr(tbuf);
	sprintf(tbuf,"pntvec/clspt,ccspt");
	NclxDbgPstr(tbuf);
}

/*********************************************************************
**    E_FUNCTION     : ncl_debug_contct_points2(clptlist)
**       Debug conact point routine.
**    PARAMETERS
**       INPUT  :
**			clpt1,clpt2	- cl points
**			ccpt1,ccpt2	- cc points
**			iret		- flag
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_debug_contct_point2(clpt1,clpt2,ccpt1,ccpt2,iret)
UM_coord clpt1,clpt2,ccpt1,ccpt2;
int iret;
{
	char tbuf[80];
	sprintf(tbuf,"draft/modify,color = 6"); 
	NclxDbgPstr(tbuf);
	if (iret == UU_SUCCESS)	
		sprintf(tbuf,"clspt=PT/%8.5f,%8.5f,%8.5f",clpt1[0],clpt1[1],clpt1[2]);
	else	
		sprintf(tbuf,"clspt=PT/%8.5f,%8.5f,%8.5f",clpt2[0],clpt2[1],clpt2[2]);
	NclxDbgPstr(tbuf);
	if (iret == UU_SUCCESS)
	{
		sprintf(tbuf,"draft/modify,color = 7"); 
		NclxDbgPstr(tbuf);
	}
	if (iret == UU_SUCCESS)
		sprintf(tbuf,"ccspt=PT/%8.5f,%8.5f,%8.5f",ccpt1[0],ccpt1[1],ccpt1[2]);
	else
		sprintf(tbuf,"ccspt=PT/%8.5f,%8.5f,%8.5f",ccpt2[0],ccpt2[1],ccpt2[2]);
	NclxDbgPstr(tbuf);
	sprintf(tbuf,"pntvec/clspt,ccspt");
	NclxDbgPstr(tbuf);
}

/*********************************************************************
**    E_FUNCTION   : ncl_smill_contct_points(clpt,trad,zmin,slvec,tol,nvec,pt)
**       Get the cl point(pt) along slvec with contact point
** ..... on the 3d boundary within tolerance
**       INPUT  :
**			clpt		- initial cl point
**			trad		- tool radius
**			frad		- tool flat radius
**			thk			- thick value
**			zmin		- min zvalue for project
**			slvec		- lace slice vector
**			tol			- tolerance
**			nvec		- inital cc point vector
**       OUTPUT :
**			pt		- initial cl point
**			nvec		- unit normal vecot
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_smill_contct_points(clpt,trad,frad,thk,zmin,tol,slvec,ccptvc,pt)
UU_REAL trad,frad,thk,zmin,tol;
UM_vector slvec,ccptvc;
UM_coord clpt,pt;
{
	int iret,iret1,iret2;
	UU_REAL co;
	UM_coord spt,clpt1,clpt2,ccpt1,ccpt2;
	UU_LIST clpoints;
		
	iret = UU_FAILURE;	
	iret1 = UU_FAILURE;	
	iret2 = UU_FAILURE;

	uu_list_init(&clpoints,sizeof(UM_clpt),100,100);
/*
.....Get the cl point along slvec with contact point
..... on the 3d boundary within tolerance
*/
	co = UM_DOT(slvec,ccptvc);
	um_translate_point(clpt,co*trad,slvec,pt);
	um_vctovc(pt,clpt1);
	clpt1[2] = zmin;
	iret = ncl_projsf_clpoint(trad,frad,clpt1,ccpt1,thk,tol,UU_FALSE,0,
		&clpoints,UU_NULL);
	if (iret != UU_SUCCESS)
	{
/*
.....Get the cl point between clpt1 and pt
*/
		iret2 = ncl_smill_contct_point(pt,clpt,trad,frad,thk,zmin,tol,clpt2,ccpt2);
		um_vctovc(clpt2,pt);
	}
	else if (iret == UU_SUCCESS)
	{
		iret1 = ncl_point_on_bound3d(ccpt1,2.0*trad,10*tol);
		if (iret1 != UU_SUCCESS && co != 0.0)
		{
			if (co < 0.0)
				um_translate_point(clpt,-2.0*trad,slvec,spt);
			else
				um_translate_point(clpt,2.0*trad,slvec,spt);
/*
.....Get the cl point between clpt1 and spt
*/
			iret2 = ncl_smill_contct_point(spt,clpt,
								trad,frad,thk,zmin,tol,clpt2,ccpt2);
			if (iret2 == UU_SUCCESS)		
				um_vctovc(clpt2,pt);
			else
				um_vctovc(clpt1,pt);
		}
	}
#if 0
	if (iret == UU_SUCCESS && iret2 != UU_SUCCESS)
		ncl_debug_contct_point2(clpt1,clpt2,ccpt1,ccpt2,iret1);
	else if (iret2 == UU_SUCCESS)
		ncl_debug_contct_point(clpt2,ccpt2);
#endif
	
	uu_list_free(&clpoints);
	if(iret2 == UU_SUCCESS)
		return iret2;

	return iret1;
}

/*********************************************************************
**    E_FUNCTION   : ncl_smill_slice_lengthcheck(inpts,ipts,tol)
**       Intermediate smill routine.
**       INPUT  :
**			inpts		- numbers of intersection points
**			ipts		- intersection points
**			tol			- tolerance
**       OUTPUT :
**			none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL ncl_smill_slice_lengthcheck(inpts,ipts,tol)
int inpts;
UM_coord ipts[];
UU_REAL tol;
{	
	int k;
	int cmp();
	UU_LOGICAL lnosec;
		
	lnosec = UU_TRUE;
	if (inpts == 2 && um_dist_2d(ipts[0],ipts[1]) < 10.*tol)
		return lnosec;
/*
.....Sort intersection points along slvec
*/
	uu_qsort (ipts,inpts,sizeof(UM_coord),cmp);

	for (k = 0; k < inpts; k++)	
	{
		if (um_dist_2d(ipts[k],ipts[k+1]) < 10.*tol)
		{
			k++;
			continue;
		}
		lnosec= UU_FALSE;
		k++;
	}

	return lnosec;
}

/*********************************************************************
**    E_FUNCTION   : ncl_smill_slice_endpoints(trad,frad,thk,zmin,tol,slvec,spt,ept)
**       Intermediate smill routine.
**       INPUT  :
**			trad		- tool radius
**			frad		- tool flat radius
**			thk			- thick value
**			zmin		- zmin value
**			slvec		- slice vector
**			tol			- tolerance
**			spt			- slice start point
**			ept			- slice end point
**       OUTPUT :
**			spt			- slice start point
**			ept			- slice end point
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_smill_slice_endpoints(trad,frad,thk,zmin,tol,slvec,spt,ept)
UU_REAL trad,frad,thk,zmin,tol;
UM_vector slvec;
UM_coord spt,ept;
{
	int iret1,iret2;
	UU_REAL co1,co2;
	UM_vector sptvc,eptvc;
	UM_coord clspt,clept,clspt1,ccspt1,clept1,ccept1;
/*
.....Get the normal at spt and ept
*/
	ncl_get_point_normal(spt,trad,tol,clspt,sptvc);
	ncl_get_point_normal(ept,trad,tol,clept,eptvc);
/*
.....Get the last point along slvec with contact point on the boudnary
*/
	iret1 = ncl_smill_contct_points(clspt,trad,frad,thk,zmin,tol,
									slvec,sptvc,spt);
/*
.....Get the first point along slvec with contact point on the boudnary
*/	
	iret2 = ncl_smill_contct_points(clept,trad,frad,thk,zmin,tol,
									slvec,eptvc,ept);
	UU_LIST_EMPTY (&Serads);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_smill_create_contour (numsf,sff,npt,stock)
**       Create pseudo-stock for a collection of surfaces.
**    PARAMETERS
**       INPUT  :
**          numsf      - number of surfaces
**          sff        - data for each surface
**          npt        - estimated number of points in stock
**			ctol,ctolsq - tolerance
**       OUTPUT :
**          stock      - minimal contour containing all surfaces
**          
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_smill_create_contour(NPT,numsf,sff,rot,ctol,ctolsq)
int NPT,numsf;
NCL_waterline_surf *sff;
UM_transf rot;
UU_REAL ctol,ctolsq;
{
	int status;
	if (NPT < 100) NPT = 100;
	status = ncl_init_wpol (0,2*NPT);
	if (status != UU_SUCCESS)
		return status;
	if (status == UU_SUCCESS) ncl_init_wpol (1,NPT);
	ncl_init_edgelst (2*NPT);

	status = ncl_create_contour_stock (sfnum,sff,rot,ctol,ctolsq);
	return status;
}

/*********************************************************************
**    E_FUNCTION     : ncl_smill_auto_boundary(sfnum,sfkey,rot,zmax,zmin,
**											sff,wtol,toler,bbox,points)
**       Create smill auto boundary.
**    PARAMETERS
**       INPUT  :
**          sfnum      - number of surfaces
**			sfkey	   - surface keys
**			rot		   - transformation matrix
**          sff        - data for each surface
**          wtol,toler - tolerances
**       OUTPUT :
**			zmax,zmin  - surfaces maximum z and minimum z
**			bbox	   - surfaces bounding box
**          points     - boundary  
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_smill_auto_boundary(sfnum,sfkey,rot,zmax,zmin,
								   sff,wtol,toler,bbox,points)
int sfnum;
UU_KEY_ID *sfkey;
NCL_waterline_surf *sff;
UM_transf rot;
UU_REAL *zmax,*zmin;
UU_REAL wtol,toler;
UM_2box *bbox;
UU_LIST **points;
{
	int status;
	UM_int2 ier;
	UM_real8 tolsq,offdis;
	UU_LIST tangs;
	ncl_polygon *pol0;
/*
.....Tesselate the surfaces
*/
	tolsq = toler * toler;
	NPT = 1;
	status = ncl_process_netsf (1,sfnum,sfkey,rot,zmax,zmin,sff,wtol,
		toler,&NPT,0.0,sfs_xmmx,sfs_ymmx,UU_NULL);
/*
.....Create stock contours
*/
	status = ncl_smill_create_contour(NPT,sfnum,sff,rot,toler,tolsq);	
	if (status > 0)
	{	
		ier = -status; status = UU_SUCCESS;	
	}
/*
.....Surafces min-max bounding box
*/
	ncl_create_2box(sfs_xmmx,sfs_ymmx,0.0,bbox);
/*
.....Offset the contour boundary
*/
	offdis = toler;
	uu_list_init (&tangs,sizeof(UM_vector),0,100);
	if (status == UU_SUCCESS)
	{
		ncl_get_wpol (0,&pol0);
		status = ncl_offset_out0(points,pol0,&tangs,offdis,toler);
	}
	
	offdis = -2*toler;
	if (status == UU_SUCCESS)						
		status = ncl_offset_out0(points,NULL,NULL,offdis,toler);

	if (tangs.data)
		uu_list_free(&tangs);

	return status;
}

/*********************************************************************
**    E_FUNCTION     : ncl_smill_input_boundary(sfnum,sfkey,rot,
**									zmax,zmin,sff,wtol,toler,bbox,points)
**       Create smill input boundary.
**    PARAMETERS
**       INPUT  :
**          sfnum      - number of surfaces
**			sfkey	   - surface keys
**			irot	   - rotation flag
**			rot		   - transformation matrix
**          sff        - data for each surface
**          wtol,toler - tolerances
**       OUTPUT :
**			zmax,zmin  - surfaces maximum z and minimum z
**			bbox	   - surfaces bounding box
**          points     - boundary
**          
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int ncl_smill_input_boundary(sfnum,sfkey,irot,rot,
							zmax,zmin,sff,wtol,toler,bbox,points)
int sfnum,irot;
UU_KEY_ID *sfkey;
NCL_waterline_surf *sff;
UM_transf rot;
UU_REAL *zmax,*zmin;
UU_REAL wtol,toler;
UM_2box *bbox;
UU_LIST **points;
{
	int i,npts,npt0,status;
	UM_int2 ier;
	UU_REAL pts_minmax[4],tolsq;
	UM_coord *pts;
	UM_transf tfmat,*tf;
	struct NCL_fixed_databag e1;
	ncl_polygon *pol0;
	
	tolsq = toler * toler;
/*
.....Get the input curves
*/
	e1.key = Sbnd;
	status = ncl_retrieve_data_fixed (&e1);
	if (ncl_itsa_compcrv(&e1))		
		npts = ncl_evolve_composite_curve(&e1,toler,&pptr,UU_NULL,UU_NULL,0);	
	else	
	{	
		tf = &tfmat;
		status = ncl_trimsrf_get_tf (&e1,&tf);
		if (status == UU_SUCCESS)
			npts = ncl_evolve_curve (&e1,tf,toler,&pptr,UU_NULL,UU_NULL,0);	
	}

	pts = (UM_coord *)UU_LIST_ARRAY(*points);
	if (irot > 0)
	{		
		for (i = 0; i < npts; i++)			
			um_cctmtf(pts[i],rot,pts[i]);	
	}
/*
.....Get the curve bounding box
*/	
	um_bound_2box(pts,npts,pts_minmax);
/*
.....Get the surface minmax
*/	
	NPT = 1;
	status = ncl_process_netsf (1,sfnum,sfkey,rot,zmax,zmin,sff,wtol,							
		toler,&NPT,0.0,sfs_xmmx,sfs_ymmx,UU_NULL);
/*
.....Check if pts_minmax is outside sfs_xmmx or sfs_ymmx
*/	
	if (pts_minmax[0]<sfs_xmmx[0]-toler || pts_minmax[1]>sfs_xmmx[1]+toler ||
		pts_minmax[2]<sfs_ymmx[0]-toler || pts_minmax[3]>sfs_ymmx[1]+toler)	
	{	
		status = ncl_smill_create_contour(NPT,sfnum,sff,rot,toler,tolsq);
		if (status > 0)		
		{	
			ier = -status; status = UU_SUCCESS;	
		} 	
			
		ncl_create_2box(sfs_xmmx,sfs_ymmx,0.0,bbox);
		ncl_get_wpol (0,&pol0);
		npt0 = UU_LIST_LENGTH(pol0->contour);
		pol0->num_contours = 1;
		pol0->np = &npt0;
		pol0->box = bbox;
		status = ncl_bound_contour_intf(pts,npts,pol0,points,toler,tolsq);
		if (status > 0)
		{
			ier = -status; status = UU_SUCCESS;
		} 	
	}

	return status;
}

/*********************************************************************
**    E_FUNCTION     : ncl_smill_drive_planes(e1,irot,rot,nzvec,dpl1)
**       Transform the clpts.
**    PARAMETERS
**       INPUT  :
**			e1		   - plane entity
**          irot	   - transformation flag
**          rot        - tranformation matrix
**			nzvec	   - z axis vector
**       OUTPUT :
**         dpl1		   - ncl plane     
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_smill_drive_planes(e1,irot,rot,nzvec,dpl1)
struct NCL_fixed_databag *e1;
int irot;
UM_transf rot;
UM_vector nzvec;
struct NCL_nclpl_rec **dpl1;
{	
	struct NCL_nclln_rec *dln1;
	UM_vector vdir,slvec;
	int status,uv_data[3],prim,type;
	UM_int2 idx;
	UU_REAL prim_data[5];
	UM_coord plpt;
	struct NCL_surface_rec surf;
	struct NCL_trimsf_rec *trim;
	UM_transf tfmat;
	UM_real8 ver;
	
	if (e1->rel_num == NCL_PLN_REL)		
		*dpl1 = (struct NCL_nclpl_rec *) e1;
	else if (e1->rel_num == UM_LINE_REL || e1->rel_num == NCL_LINE_REL)
	{
		dln1 = (struct NCL_nclln_rec *)e1;
		um_vcmnvc(dln1->ept,dln1->spt,slvec);
		um_unitvc(slvec, slvec);
		um_cross(slvec,nzvec,vdir);
		*dpl1 = (struct NCL_nclpl_rec *) uu_malloc(sizeof(struct NCL_nclpl_rec));
		ncl_set_nclpln(*dpl1,dln1->spt,vdir);
	}
	else
	{
		if (e1->rel_num == NCL_TRIMSF_REL)
		{
			trim = (struct NCL_trimsf_rec *)e1;
			surf.key = trim->bs_key;
			ur_retrieve_data(&surf,sizeof(surf));
			idx = 169; getsc(&idx,&ver);
			prim = ncl_get_prim_data(&surf,plpt,slvec,prim_data,uv_data);
/*
.....Trimmed surfaces can have associated transformation matrices and so the
.....primitive data needs to be transformed when one exists.  This change will
.....only apply to NCL version 10.003 and newer - ASF 7/12/13.
*/
			if (ver > 10.002)
			{
				status = uc_retrieve_transf(e1->key, tfmat);
				if (status == UU_SUCCESS && prim >= 0 && prim <= 6)
				{
					um_cctmtf(plpt,tfmat,plpt); um_vctmtf(slvec,tfmat,slvec);
					um_unitvc(slvec,slvec);
				}
			}
		}
		else
			prim = ncl_get_prim_data(e1,plpt,slvec,prim_data,uv_data);
		if (prim != NCLSF_PLANE) 
		{
			ud_wrerr("Invalid drive surface plane");
			return;
		}
		*dpl1 = (struct NCL_nclpl_rec *) uu_malloc(sizeof(struct NCL_nclpl_rec));
		ncl_set_nclpln(*dpl1,plpt,slvec);
	}

	if (irot > 0)	
	{
		um_cctmtf((*dpl1)->pt,rot,(*dpl1)->pt);
		um_cctmtf((*dpl1)->nvec,rot,(*dpl1)->nvec);
		um_unitvc((*dpl1)->nvec, (*dpl1)->nvec);
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_smill_drive_vector(e1,mcsflg,slvec)
**       Transform the clpts.
**    PARAMETERS
**       INPUT  :
**			e1		   - plane entity
**			mcsflg	   - modsys flag	
**       OUTPUT :
**          slvec	   - 2d slcing vector     
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_smill_drive_vector(e1,mcsflg,slvec)
struct NCL_fixed_databag *e1;
UU_LOGICAL mcsflg;
UM_vector slvec;
{	
	struct NCL_vector_rec *dvc1;
	struct NCL_nclpv_rec *dpv1;
	struct NCL_nclln_rec *dln1;

	if (e1->rel_num == NCL_VECTOR_REL)	
	{		
		dvc1 = (struct NCL_vector_rec *)e1;
		if (mcsflg) ncl_wcstomcs(1,dvc1->vec,dvc1->vec);
		um_vctovc(dvc1->vec,slvec);
	}
	else if (e1->rel_num == NCL_POINTVEC_REL)
	{
		dpv1 = (struct NCL_nclpv_rec *)e1;
		if (mcsflg) ncl_wcstomcs(1,dpv1->ve,dpv1->ve);
		um_vctovc(dpv1->ve,slvec);
	}
	else if (e1->rel_num == UM_LINE_REL)
	{
		dln1 = (struct NCL_nclln_rec *) e1;
		if (mcsflg)
		{
			ncl_wcstomcs(1,dln1->spt,dln1->spt);
			ncl_wcstomcs(1,dln1->ept,dln1->ept);
		}
/*
.....This call to ncl_wcstomcs is invalid.  The input should be a
.....vector or coordinate. - Andrew 3/5/13
		if (mcsflg) ncl_wcstomcs(1,dln1,dln1);
*/
		um_vcmnvc(dln1->ept,dln1->spt,slvec);
	}
	
	slvec[2] = 0.0;	
	um_unitvc(slvec, slvec);
}

/*********************************************************************
**    E_FUNCTION     : ncl_smill_transf_clpts(ntype,mcsflg,rot)
**       Transform the clpts.
**    PARAMETERS
**       INPUT  :
**          ntype      - 0: SCRUB 1:COMBIN 2:LACE
**			irot	   - rotation flag
**			mcsflg	   - modsys flag	
**          rot        - tranformation matrix
**       OUTPUT :
**          none     
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_smill_transf_clpts(ntype,irot,rtinv,mcsflg)
int ntype,irot;
UU_LOGICAL mcsflg;
UM_transf rtinv;
{
	int i,iflag,j,npas,npts;
	UU_REAL fmm;
	UM_clpt *clpts;

	fmm = (mm == 1)? 25.4: 1;

	if (ntype == 2)
		iflag = Sclpt_flag;
	else
		iflag = Sslpt_flag;

	ncl_sm_clpath_getnpath(&iflag,&npas);
	for (i = 0; i < npas;  ++i)
	{
		ncl_sm_clpath_getpts(iflag,i,&npts,&clpts);
		for (j = 0; j < npts; ++j)
		{
			if (irot > 0)		
			{
				um_cctmtf (clpts[j].pte, rtinv, clpts[j].pte);
				um_cctmtf (clpts[j].vta, rtinv, clpts[j].vta);
			}

			if (mcsflg) 
			{
				ncl_wcstomcs(0,clpts[j].pte,clpts[j].pte);
				ncl_wcstomcs(1,clpts[j].vta,clpts[j].vta);
			}
			else if (mm == 1)
			{ 
				um_vctmsc(clpts[j].pte,fmm,clpts[j].pte);
			}
		}
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_smill_get_erad(erads)
**		get the minmum erad.
**    PARAMETERS
**       INPUT  :
**          erads      - erad list
**       OUTPUT :
**          none     
**    RETURNS      :
**         minmum erad
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_REAL ncl_smill_get_erad(frad,erads)
UU_REAL frad;
UU_LIST *erads;
{
	int i,npts;
	UU_REAL *erad, erad_min = 1000.0;
	npts = UU_LIST_LENGTH(erads);
	erad = (UU_REAL*) UU_LIST_ARRAY(erads);

	for (i = 0; i < npts; ++i)
	{
		if (erad[i] > frad - UM_FUZZ && erad[i] < frad + UM_FUZZ)
			continue;
		if (erad[i] < erad_min)
			erad_min = erad[i];
	}

	if (erad_min == 1000.0)
		erad_min = frad;
	return erad_min;
}

/*********************************************************************
**    E_FUNCTION : ncl_smill_check_slices(trad,frad,slvec,thk,toler,
**                    inpts,ipts,skip,wtol)
**       Check segments to ensure milling stays inside boundary.
**    INPUT  :
**			trad  - tool corner radius
**			frad  - tool flat radius
**			slvec	- slice vector
**			thk   - thick value
**			toler - tolerance
**       wtol  - tolerance
**       inpts - number of points in point list
**       ipts  - list of points to check
**       skip  - list of logicals that denote whether to skip a seg
**    OUTPUT  :
**       inpts - updated number of points in ipts
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_smill_check_slices(trad,frad,slvec,thk,toler,inpts,ipts,skip,wtol)
UU_REAL trad,frad,thk,toler,wtol;
int *inpts;
UM_coord *ipts;
UM_vector slvec;
UU_LIST *skip;
{
	int i,j,tstat,tinpts;
	UM_coord clpt,ccpt;
	UU_LOGICAL *skips,inside,prevfail,oprevfail;
	UU_LIST tpts;
	char tbuf[80];

	tinpts = *inpts;
	skips = (UU_LOGICAL *)UU_LIST_ARRAY(skip);
	prevfail = UU_FALSE;
	uu_list_init(&tpts,sizeof(UM_clpt),1,5);
/*
.....Remove points that cause milling across gaps and set skip
.....flags for regions where points couldn't be removed, but
.....still need to be skipped/ignored.
*/
	for (i=0;i<tinpts-1;i++)
	{
		skips[i] = UU_FALSE;
		if (um_dist_2d(ipts[i],ipts[i+1]) < 10.*wtol) continue;
		um_middlept(ipts[i],ipts[i+1],clpt);
		tstat = ncl_projsf_clpoint2(trad,frad,slvec,clpt,ccpt,
									thk,0.5*toler,UU_TRUE,0,&tpts,&inside);
		UU_LIST_EMPTY(&tpts);
		if (inside)
		{
			if (prevfail) skips[i-1] = UU_TRUE;
			prevfail = UU_FALSE;
			continue;
		}
		oprevfail = prevfail;
		prevfail = UU_TRUE;
		if (i == 0 || oprevfail)
		{
			for (j=i;j<tinpts-1;j++) um_vctovc(ipts[j+1],ipts[j]);
			i--;
		}
		else if (i < tinpts - 1) continue;
		tinpts--;
	}
	uu_list_free(&tpts);
	*inpts = tinpts;
#if 0
	skips = (UU_LOGICAL *)UU_LIST_ARRAY(skip);
	for (i=0;i<tinpts-1;i++)
	{
		if (um_dcccc(ipts[i],ipts[i+1]) > UM_DFUZZ)
		{
			sprintf(tbuf,"LN/%9.6f,%9.6f,%9.6f,%9.6f,%9.6f,%9.6f",
			ipts[i][0],ipts[i][1],ipts[i][2],
			ipts[i+1][0],ipts[i+1][1],ipts[i+1][2]);
		}
		else
			sprintf(tbuf,"$$ LINE TOO SHORT");
		NclxDbgPstr(tbuf);
	}
#endif
}

/*********************************************************************
**    E_FUNCTION : ncl_smill_stepover_toolpath(trad,frad,thk,zmin,nend,ntype,
**			spt1,spt2,xystep,npaths,vdir,slvec,points,bndpts,bbox,toler,wtol)
**       smill stepver/number toolpath routine.
**       INPUT  :
**			trad		- tool corner radius
**			frad		- tool flat radius
**			thk			- thick value
**			zmin		- zmin value
**			nend		- CL end condition 0:TO 1:PAST 2:ON 3:CONTCT
**			ntype		- 0: SCRUB 1:COMBIN 2:LACE
**			spt1,spt2	- slice start point and end point
**			xystep		- stepover distance
**			npaths		- number of passes
**			vdir		- slice plane normal
**			slvec		- slice vector
**			points		- 2d boundary contour
**			bndpts		- extended 2d boudnary contour for PAST
**			bbox		- minmax bounding box
**			toler,wtol  - tolerance
**    RETURNS      : UU_SUCCESS / UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_smill_stepover_toolpath(trad,frad,thk,zmin,nend,ntype,
				spt1,spt2,xystep,npaths,vdir,slvec,points,bndpts,bbox,toler,wtol)
UU_REAL trad,frad,thk,zmin,toler;
int npaths,nend,ntype;
UM_coord spt1,spt2;
UM_vector vdir,slvec;
UU_LIST *points,*bndpts;
UM_2box *bbox;
UU_REAL xystep,toler,wtol;
{
	int i,j,j1,k,k1,k2,nsteps,nsegs,npts,index,numpts;
	UM_int2 ierr,idx;
	UM_coord clpt,ccpt,clpt1,clpt2,ccpt1,ccpt2,spt,slpt,slpt1,ipts[20],
		ipts1[20],slpt2;
	int iret1,iret2,inpts,inpts1,icombin,iscrub,nums,*nnums,nps;
	UU_REAL v,minclz,xystep_last;
	UM_real8 ver;

	struct NCL_nclpl_rec pl;	
	UU_LIST clpoints,tmpclpts,tmpclnums,clpts;

	SM_segment seg,seg1,*psegs,*psegs1;
	UU_LIST seglst,seglst1,skip;

	UM_clpt *pos;
	int cmp();

	UU_LOGICAL lcheck,lsecond,*skips,fail;
	lcheck = UU_TRUE;
	lsecond = UU_FALSE;

	idx = 169; getsc(&idx,&ver);
	numpts = 0;
	nsegs = 0;
	index = 0;
	pl.key = NULLKEY;
	pl.rel_num = NCL_PLN_REL;
	um_vctovc(spt1,slpt1);

	uu_list_init (&seglst,sizeof(SM_segment),npaths,npaths);
	uu_list_init(&skip,sizeof(UU_LOGICAL),10,10);
/*
.....Added init for Serads for call to ncl_smill_check_slices - ASF 10/22/13.
*/
	uu_list_init(&Serads,sizeof(UU_REAL),200,200);
	if (nend == 1)
		uu_list_init (&seglst1,sizeof(SM_segment),npaths,npaths);

	for (i = 0; i <= npaths; i++)
	{
/*
.....Setup lace slice plane
*/
		if (i == 0)
			um_translate_point(slpt1,toler,vdir,slpt);
		else	
			um_translate_point(slpt1,xystep,vdir,slpt);
/*
.....Check if the last second pass
*/
		if (!lsecond)
		{
			um_translate_point(slpt,xystep,vdir,slpt2);
			if (!um_point_in_segment_2d(slpt2,slpt1,spt2,toler))
			{
				xystep_last = 0.5 * um_dist_2d(slpt1,spt2);
				um_translate_point(slpt1,xystep_last,vdir,slpt);
				lsecond = UU_TRUE;
			}
		}
/*
.....Check if last slice or nor
*/
		if (!um_point_in_segment_2d(slpt,slpt1,spt2,toler))
		{
			um_translate_point(spt2,-toler,vdir,slpt);
			npaths = i;
		}
/*
.....Set up the plane
*/
		ncl_set_nclpln(&pl,slpt,vdir);
		um_vctovc(slpt,slpt1);
/*
.....Get the intersection points of plane with boundary
*/
		inpts = 0;
		ncl_bound_plio(points,&pl,toler,&inpts,ipts,&ierr);
		if (inpts < 2) continue;
		if (ncl_smill_slice_lengthcheck(inpts,ipts,wtol))
			continue;
		fail = UU_FALSE;
		for (k=0;k<inpts-1;k++) uu_list_push(&skip,&fail);
		if (ver > 10.002)
		{
			uu_qsort (ipts,inpts,sizeof(UM_coord),cmp);
			ncl_smill_check_slices(trad,frad,slvec,thk,toler,&inpts,ipts,
				&skip,wtol);
			if (inpts < 2) continue;
		}
		
		if (nend == 1) /*PAST*/
		{
			inpts1 = 0;	
			ncl_bound_plio(bndpts,&pl,toler,&inpts1,ipts1,&ierr);
			uu_qsort (ipts1,inpts1,sizeof(UM_coord),cmp);
		}
/*
.....Sort intersection points along slvec
*/
		uu_qsort (ipts,inpts,sizeof(UM_coord),cmp);
		skips = (UU_LOGICAL *)UU_LIST_ARRAY(&skip);

		for (k = 0; k < inpts; k++)
		{			
			if (um_dist_2d(ipts[k],ipts[k+1]) < 10.*wtol || skips[k])
				continue;

			if (nend == 3) /*CONTCT*/		
				ncl_smill_slice_endpoints(trad,frad,thk,zmin,toler,
										slvec,ipts[k],ipts[k+1]);
/*
.....Slicing segment
*/
			um_vctovc(ipts[k],seg.p1);
     		um_vctovc(ipts[k+1],seg.p2);
			seg.index = index;
	    	uu_list_push(&seglst,&seg);

			if (nend == 1) /*PAST*/
			{
				um_vctovc(ipts1[k],seg1.p1);
     			um_vctovc(ipts1[k+1],seg1.p2);
				seg1.index = index;
	    		uu_list_push(&seglst1,&seg1);
			}

			++nsegs;
			k += 1;
		}
		++index;
	}

/*
.....Create cl paths for all slices
*/
	if (ntype != 1)
		ncl_sm_clpath_create_all(Sclpt_flag,nsegs);
	else
	{
		ncl_sm_clpath_new(Sslpt_flag);
		ncl_sm_clpath_create_all(Sslpt_flag,nsegs);
	}

	nsegs = seglst.cur_cnt;
	psegs = (SM_segment *)UU_LIST_ARRAY(&seglst);

	if (nend == 1)  /*PAST*/
		psegs1 = (SM_segment *)UU_LIST_ARRAY(&seglst1);

	#pragma omp parallel for private(k,minclz,clpts)
	for (k = 0; k < nsegs; k++)
	{			
		UU_LIST_EMPTY (&clpts);		
		minclz = zmin;
/*
..... Initilized projected clpoints list
*/
		uu_list_init(&clpts,sizeof(UM_clpt),500,500);
/*
.....Project points for each slice
*/
		#pragma omp critical  
		{
			ncl_smill_project_clpath(trad,frad,thk,minclz,nend,ntype,k,slvec,
				psegs[k].index,psegs[k].p1,psegs[k].p2,bndpts,bbox,toler,wtol,&clpts);
	
					
			if (nend == 1)  /*PAST*/
			{	
				ncl_smill_past_clpts(trad,frad,thk,zmin,wtol,slvec,
									psegs1[k].p1,psegs1[k].p2,&clpts);
				ncl_sort_clpts(&clpts,slvec,psegs1[k].index,ntype,toler);
			}

			if (ntype != 1)
				ncl_sm_clpath_insert_list(Sclpt_flag,k,&clpts);
			else
				ncl_sm_clpath_insert_list(Sslpt_flag,k,&clpts);
		}
		numpts += clpts.cur_cnt;
	}
	
	if (ntype == 1)
		ncl_smill_toolpath_combin(ntype,slvec,toler);
	
	uu_list_free(&Serads);
	uu_list_free(&seglst);
	if (nend == 1)
		uu_list_free(&seglst1);
	uu_list_free(&skip);
	return(numpts);
}

/*********************************************************************
**    E_FUNCTION : ncl_smill_scallop_toolpath(trad,frad,thk,zmin,nend,ntype,ndrv,
**				spt1,spt2,dpl1,dpl2,xystep,fstep,xystep0,hscal,npaths,
**				vdir,slvec,points,bndpts,bbox,toler,wtol)
**       smill scallop toolpath routine.
**       INPUT  :
**			trad		- tool corner radius
**			frad		- tool flat radius
**			thk			- thick value
**			zmin		- zmin value
**			nend		- CL end condition 0:TO 1:PAST 2:ON 3:CONTCT
**			ntype		- 0: SCRUB 1:COMBIN 2:LACE
**			ndrv		- 1: drive vector 2: drive planes
**			spt1,spt2	- slice start point and end point
**			dpl1,dpl2	- drive planes
**			xystep		- stepover distance
**			fstep		- flat area stepover for Bullnose tool
**			xystep0		- original stepover distance for Bullnose tool
**			hscal		- scallop height value
**			npaths		- number of passes
**			vdir		- slice plane normal
**			slvec		- slice vector
**			points		- 2d boundary contour
**			bndpts		- extended 2d boudnary contour for PAST
**			bbox		- minmax bounding box
**			toler,wtol  - tolerance
**    RETURNS      : UU_SUCCESS / UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_smill_scallop_toolpath(trad,frad,thk,zmin,nend,ntype,ndrv,
				spt1,spt2,dpl1,dpl2,xystep,fstep,xystep0,hscal,npaths,
				vdir,slvec,points,bndpts,bbox,toler,wtol)
UU_REAL trad,frad,thk,zmin,toler;
int npaths,nend,ntype,ndrv;
UM_coord spt1,spt2;
struct NCL_nclpl_rec *dpl1,*dpl2;
UM_vector vdir,slvec;
UU_LIST *points,*bndpts;
UM_2box *bbox;
UU_REAL xystep,fstep,xystep0,hscal,toler,wtol;
{
	int i,j,j1,k,k1,m,nsteps,nfsteps,iret1,iret2,inpts,inpts1,icombin,iscrub,
		nums,*nnums,nps,status1,numpts;
	UM_int2 ierr;
	UM_real8 xystep_max,dstep,xystep_last;
	UM_coord clpt,ccpt,clpt1,clpt2,ccpt1,ccpt2,spt,slpt,slpt1,ipts[20],
		ipts1[20],slpt2;
	UU_REAL v,erad;
	struct NCL_nclpl_rec pl,pl_prev;
	UU_LIST clpoints,tmpclpts,prevclpts,tmpclnums,preverads,tpts,skip;
	UM_clpt *pos;
	int cmp();
	UU_LOGICAL lcheck,lflat,lsecond,prevfail,oprevfail,*skips,inside;
	char tbuf[80];
	UM_int2 idx;
	UM_real8 ver;

	idx = 169; getsc(&idx,&ver);
	lcheck = UU_TRUE;
	lflat = UU_FALSE;
	lsecond = UU_FALSE;
	numpts = 0;
/*
.....Initialize cutter location point
*/
	uu_list_init(&prevclpts,sizeof(UM_clpt),200,200);
	uu_list_init(&Serads,sizeof(UU_REAL),200,200);
	uu_list_init(&preverads,sizeof(UU_REAL),200,200);
	uu_list_init(&skip,sizeof(UU_LOGICAL),10,10);
/*
.....Initilized projected clpoints list
.......Moved to prevent memory problems when clpoints memory
.......is deallocated before being allocated. Andrew 3/25/13
*/
	uu_list_init(&clpoints,sizeof(UM_clpt),200,200);
/*
.....Initialize tempclpts for COMBIN
*/
	if (ntype == 1)
	{
		uu_list_init(&tmpclpts,sizeof(UM_clpt),200,200);
		uu_list_init(&tmpclnums,sizeof(int),10,10);
	}
/*
.....Maximum stepover for scallop height
*/
	xystep_max = xystep;
	um_vctovc(spt1,slpt1);
	
	pl.key = NULLKEY;
	pl.rel_num = NCL_PLN_REL;
	pl_prev.key = NULLKEY;	
	pl_prev.rel_num = NCL_PLN_REL;

	icombin = 0;
	iscrub = 0;
	nfsteps = 1;
	for (i = 0; i <= npaths; i++)
	{
/*
.....Check if the slice point is within drive planes
*/
		if (ndrv == 2 && !um_point_bewteen_planes(slpt1,dpl1,dpl2))
			break;
/*
.....Bullnose tool effective radius and maximum stepover for scallop
*/	
		if (frad > UM_FUZZ && UU_LIST_LENGTH(&preverads) > 0)
		{		
			erad = ncl_smill_get_erad(frad,&preverads);	
			if (erad < frad + UM_FUZZ && erad > frad - UM_FUZZ)
			{
				if (lflat)
				{
					xystep = 2.0 * frad;	
					if (xystep > fstep && fstep > UM_FUZZ)
						xystep = fstep;
				}
				else
					xystep = xystep0;
			}
			else
			{
				xystep = erad*erad - (erad-hscal)*(erad-hscal);
				xystep = sqrt(xystep) * 2.0;
			}
			if (xystep < 2*toler) xystep = 2*toler;
			xystep_max = xystep;
		}
/*
.....Setup lace slice plane
*/
		if (i == 0)
			um_translate_point(slpt1,toler,vdir,slpt);
		else	
			um_translate_point(slpt1,xystep,vdir,slpt);
/*
.....Check if the last second pass
*/
		if (!lsecond)
		{
			um_translate_point(slpt,xystep,vdir,slpt2);
			if (!um_point_in_segment_2d(slpt2,slpt1,spt2,toler))
			{
				xystep_last = 0.5 * um_dist_2d(slpt1,spt2);
				um_translate_point(slpt1,xystep_last,vdir,slpt);
				lsecond = UU_TRUE;
			}
		}
/*
.....Check if last slice or nor
*/
		if (!um_point_in_segment_2d(slpt,slpt1,spt2,toler))
		{
			um_translate_point(spt2,-toler,vdir,slpt);
			npaths = i;
		}
		ncl_set_nclpln(&pl,slpt,vdir);
/*
.....Check if more paths are needed to insert for scallop height
*/
		if (UU_LIST_LENGTH(&prevclpts) > 0)
		{
			status1 = ncl_smill_scallop_step(i,&pl_prev,&pl,
									trad,xystep_max,&dstep);
			if (status1 == UU_SUCCESS)
			{	
				lflat = UU_FALSE;
				if (nfsteps > 1)
				{					
					um_vctovc(slpt1,slpt);				
					ncl_copy_nclpln (&pl_prev,&pl);
					nfsteps = 1; --i;
				}
				else
				{
					xystep = dstep; ++npaths;
					UU_LIST_EMPTY(&prevclpts); --i;					
					UU_LIST_EMPTY(&preverads); 
					if (frad > UM_FUZZ)
						xystep_max = xystep;
					continue;
				}
			}
			else
			{
				lflat = UU_TRUE;
				if (frad > UM_FUZZ && fstep > UM_FUZZ)
				{
					if (fstep > nfsteps*xystep_max)
					{							
						++nfsteps; xystep = xystep_max;
						um_vctovc(slpt,slpt1);
						ncl_copy_nclpln (&pl,&pl_prev);
					}
					else
					{
						xystep = fstep-(nfsteps-1)*xystep_max; 
						nfsteps = 1;	
						UU_LIST_EMPTY(&prevclpts); --i;
						UU_LIST_EMPTY(&preverads); 
					}
					if (i < npaths)			
						continue;
				}
			}
		}
/*
.....Empty previous temporary path
*/
		if (UU_LIST_LENGTH(&prevclpts) > 0)
			UU_LIST_EMPTY(&prevclpts);		
		if (UU_LIST_LENGTH(&preverads) > 0)
			UU_LIST_EMPTY(&preverads);
		um_vctovc(slpt,slpt1);
		xystep = xystep_max;	
/*
.....Copy previous plane for scallop calculation
*/
		ncl_copy_nclpln (&pl,&pl_prev);
/*
.....Get the intersection points of plane with boundary
*/
		inpts = 0;
		ncl_bound_plio(points,&pl,toler,&inpts,ipts,&ierr);
		if (inpts < 2) continue;
		if (ncl_smill_slice_lengthcheck(inpts,ipts,wtol))
			continue;
		prevfail = UU_FALSE;
		for (k=0;k<inpts-1;k++) uu_list_push(&skip,&prevfail);
		if (ver > 10.002)
		{
			uu_qsort (ipts,inpts,sizeof(UM_coord),cmp);
			ncl_smill_check_slices(trad,frad,slvec,thk,toler,&inpts,ipts,
				&skip,wtol);
			if (inpts < 2) continue;
		}
/*
.....Get the intersection points of plane with extended boundary
*/
		if (nend == 1) /*PAST*/
		{
			inpts1 = 0;	
			ncl_bound_plio(bndpts,&pl,toler,&inpts1,ipts1,&ierr);
			uu_qsort (ipts1,inpts1,sizeof(UM_coord),cmp);
		}
/*
.....Sort intersection points along slvec
*/
		uu_qsort (ipts,inpts,sizeof(UM_coord),cmp);
/*
.....For each section of slice
*/
		skips = (UU_LOGICAL *)UU_LIST_ARRAY(&skip);
		for (k = 0; k < inpts; k++)
		{			
			if (um_dist_2d(ipts[k],ipts[k+1]) < 10.*wtol || skips[k])
				continue;

			if (nend == 3)/*CONTCT*/	
				ncl_smill_slice_endpoints(trad,frad,thk,zmin,wtol,
										slvec,ipts[k],ipts[k+1]);

			nsteps = ceil(um_dist_2d(ipts[k],ipts[k+1])/(trad + frad)*2.0);
/*
.....Project for each slice
*/
			iret1 = UU_FAILURE;
			iret2 = UU_FAILURE;

			for (j = 0; j <= nsteps; j++)
			{
				v = j * (trad + frad) * 0.5;
				um_translate_point(ipts[k],v,slvec,clpt1);
				if (j == nsteps)
					um_vctovc(ipts[k+1],clpt1);
				clpt1[2] = zmin;

				if (j <= 2 || j >= nsteps-2)
					lcheck = UU_FALSE;
				else
					lcheck = UU_TRUE;		
				
				iret1 = ncl_projsf_clpoint2(trad,frad,slvec,clpt1,ccpt1,
										   thk,toler,lcheck,0,&clpoints,UU_NULL);
				if (iret1 == UU_SUCCESS && iret2 == UU_SUCCESS)
					ncl_projsf_clpoint2_middle(trad,frad,thk,zmin,toler,
									slvec,clpt1,clpt2,lcheck,0,&clpoints);
				if (iret1 == UU_SUCCESS)
				{
					iret2 = iret1;
					um_vctovc(clpt1,clpt2);
				}
			}
/*
.....Sort the clpts
*/
			if (ntype != 1)		
			{
				ncl_sort_clpts(&clpoints,slvec,iscrub,ntype,toler);
/*
.....Add clpts to PAST sections at both ends
*/
				if (nend == 1) /*PAST*/
				{
					k1 = k;
					if (k1 >= inpts1)
						k1 -= 2;
					ncl_smill_past_clpts(trad,frad,thk,zmin,wtol,slvec,
										ipts1[k1],ipts1[k1+1],&clpoints);
					ncl_sort_clpts(&clpoints,slvec,iscrub,ntype,toler);
				}
			}
			else if (ntype == 1)
			{
				ncl_sort_clpts(&clpoints,slvec,icombin,ntype,toler);
/*
.....Add clpts to PAST sections
*/
				if (nend == 1) /*PAST*/
				{
					ncl_smill_past_clpts(trad,frad,thk,zmin,wtol,slvec,
										ipts1[k],ipts1[k+1],&clpoints);
					ncl_sort_clpts(&clpoints,slvec,icombin,ntype,toler);
				}

				if (icombin == 1)
				{
					nums = UU_LIST_LENGTH(&clpoints);
					if (nums > 0)
					{
						uu_list_push_list(&tmpclpts, &clpoints);	
						uu_list_push(&tmpclnums, &nums);
						UU_LIST_EMPTY (&clpoints);
					}
				}
				if (k == inpts-2)			
					++icombin;								
			}
			numpts += clpoints.cur_cnt;
#if 0
			ncl_debug_clpts(&clpoints,0);
#endif
/*
.....Put the cl points to list
*/
			if (ntype != 1)	
			{
				ncl_sm_clpath_create(Sclpt_flag);
				ncl_sm_clpath_push_list(Sclpt_flag,&clpoints);
			}
			else
			{
				if (icombin == 1 || icombin == 2)
				{
					if (UU_LIST_LENGTH(&clpoints) > 0)
					{
						ncl_sm_clpath_create(Sclpt_flag);
						ncl_sm_clpath_push_list(Sclpt_flag,&clpoints);
						uu_list_push_list(&prevclpts,&clpoints);
						UU_LIST_EMPTY (&clpoints);
					}
				}
				else if (icombin == 3)
				{
					if (UU_LIST_LENGTH(&clpoints) > 0)
					{
						ncl_sm_clpath_create(Sclpt_flag);
						ncl_sm_clpath_push_list(Sclpt_flag,&clpoints);
						uu_list_push_list(&prevclpts,&clpoints);
						UU_LIST_EMPTY (&clpoints);
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
							for (i = 0; i < nums; i++)
							{
								ncl_sm_clpath_create(Sclpt_flag);
								for (j = 0; j < nnums[i]; j++)
								{
									ncl_sm_clpath_push_at(Sclpt_flag,nps+i,&pos[j1]);							
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
			}
/*
.....Set previous clpath
*/
			if (ntype != 1)
			{
				uu_list_push_list(&prevclpts,&clpoints);
				uu_list_push_list(&preverads,&Serads);
				UU_LIST_EMPTY (&clpoints);
				UU_LIST_EMPTY (&Serads);
			}
			iret2 = UU_FAILURE;
			k += 1;
/*
.....end for each section of slice k = 0
*/
		}
/*
.....end for each path i = 0
*/
		++iscrub;
	} 

/*
.....Free memory
*/	
	if (ntype == 1)
	{
		uu_list_free(&tmpclpts);
		uu_list_free(&tmpclnums);
	}
	if (clpoints.data) 	
		uu_list_free(&clpoints);
	uu_list_free(&prevclpts);
	uu_list_free(&skip);
	uu_list_free(&Serads);
	return(numpts);
}

/*********************************************************************
**    E_FUNCTION     : ncl_smill_create(nend,ndrv,nbnd,nstep,npas,nstp,ntype)
**       Intermediate smill routine.
**       INPUT  :
**			nend		CL end condition 0:TO 1:PAST 2:ON 3:CONTCT
**			ndrv		1: drive vector 2: drive planes
**			nbnd		0: auto contour boundary 2: input boundary curve
**			nstep		0:scallob height 1:PASS number 2: Stepover
**			npas		scallop height or pass number value
**			nstp		stepover value
**			ntype		0: SCRUB 1:COMBIN 2:LACE
**    RETURNS      : UU_SUCCESS / UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int ncl_smill_create(nend,ndrv,nbnd,nstep,npas,nstp,ntype)
UM_int2 *nend,*ndrv,*nbnd,*nstep,*ntype;
UM_real8 *npas,*nstp;
{
	UM_int2 isub,idx;
	UM_real8 thk,tol8,tolsq,diam,toolpar,offdis;
	UM_real8 xystep,xystep_max,dstep,fstep,hscal,xystep0,ver;
	UM_vector nvec,nzvec,vdir,vdir1,slvec;
	UM_transf rot,tfmat,*tf,rtinv,plmx,plinv;
	int i,i1,j,j1,k,iret1,iret2,status,status1,npaths,nsteps,nfsteps,nums,npts;
	UU_REAL toler,fmm,zmin,zmax,v;
	UU_REAL trad,frad,bucketSize,erad;
	UU_REAL xmm[2],ymm[2],pts_minmax[4];
	UU_REAL areamin,xl,yb,yb1;
	UM_coord spt,spt1,spt2,slpt,slpt1,spt1s,spt2s;
	UM_2box bbox,bbox0;
	int mcsflg, irot;

	UU_LIST bndpts;
    UU_LIST *points = UU_NULL;
	UU_LIST *ptsbnd = UU_NULL;
	UU_LIST *points3d = UU_NULL;

	struct NCL_fixed_databag e1,e2;
	struct NCL_nclpl_rec pl,*dpl1,*dpl2;
	struct UM_point_rec dspt;
	struct NCL_curve_rec *dcrv;
	UU_KEY_ID key0,*sfkey;
	UU_LOGICAL plflg;

	char tbuf[80];

	idx = 169; getsc(&idx,&ver);
	mcsflg = 0;
	irot = 0;
	npts = 0;
	um_identtf (rot);
	key0 = NULLKEY;
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

	pl.key = NULLKEY;
	sfkey = (UU_KEY_ID *) UU_LIST_ARRAY(&sfky);
/*
.....Create slicing plane
*/
	status = ncl_create_wat_botpl(&pl);
	if (status != UU_SUCCESS) goto Err1;

	nvec[0] = tool[3]; nvec[1] = tool[4]; nvec[2] = tool[5];
	if (UM_DOT(nvec,nvec) < UM_FUZZ)
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
/*
.....Create the contour boundary if not boundary specified
*/
	uu_list_init0 (&bndpts);
	uu_list_init0 (&pptr);
	if (*nbnd == 0)
	{
		status = ncl_smill_auto_boundary(sfnum,sfkey,rot,
						&zmax,&zmin,sff,wtol,toler,&bbox0,&points);
	}
	else if (*nbnd == 1)
	{
/*
....Get the input boundary curve
*/
		uu_list_init (&pptr,sizeof(UM_coord),0,100);
		points = &pptr;
		status = ncl_smill_input_boundary(sfnum,sfkey,irot,rot,
						&zmax,&zmin,sff,wtol,toler,&bbox0,&points);
	}
#if 0
/*
.....Debug boundary contour
*/
	ncl_debug_pts(points,5);
#endif
/*
.....Offset the boundary curve for TO/PAST cases
*/
	if (*nend == 0)
		offdis = - 0.5*diam;
	else if (*nend == 1)
	{
		offdis = 0.5*diam - 3*toler;
		nums = UU_LIST_LENGTH(points);
		uu_list_init (&bndpts,sizeof(UM_coord),nums,nums);		
		uu_list_push_list(&bndpts, points);	
				
		ptsbnd = &bndpts;
		status = ncl_offset_out0(&ptsbnd,NULL,NULL,offdis,toler);
	}
	if (*nend == 0)
		status = ncl_offset_out0(&points,NULL,NULL,offdis,toler);
	if (status != UU_SUCCESS) goto Err2;
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
		if(!um_vcparall(dpl1->nvec, dpl2->nvec))
		{
			status = 250; goto Done;
		}
	}
/*
.....Drive vector
*/
	else if (*ndrv == 1)
	{
		e1.key = Sdpl[0];
		status = ncl_retrieve_data_fixed (&e1);
		ncl_smill_drive_vector(&e1,mcsflg,slvec);
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
	ncl_sm_clpath_new(Sclpt_flag);
/*
.....Get slice start point (spt1) and end point(spt2)
*/
	iret1 = -1;
	iret2 = -1;
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
		if (um_dot(vdir,dpl1->nvec) < 0.)
			um_vctmsc(dpl1->nvec,-1.,vdir);
		else
			um_vctovc(dpl1->nvec,vdir);
		um_unitvc(vdir,vdir);
		um_vcmnvc(spt2,spt1,vdir1);
		um_cross(dpl1->nvec,nzvec,slvec);
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
.....Start to calculate toolpath
*/
	if (*nstep != 0)
	{
/*
.....Calculate the STEP/PASS toolpath
*/
		npts = ncl_smill_stepover_toolpath(trad,frad,thk,zmin,*nend,*ntype,
			spt1,spt2,xystep,npaths,vdir,slvec,points,&bndpts,&bbox0,toler,wtol);
	}
	else
	{		
/*
.....Calculate the HEIGHT,scallop toolpath
*/
		npts = ncl_smill_scallop_toolpath(trad,frad,thk,zmin,*nend,*ntype,*ndrv,
			spt1,spt2,dpl1,dpl2,xystep,fstep,xystep0,hscal,
			npaths,vdir,slvec,points,&bndpts,&bbox0,toler,wtol);
	}
	if (npts == 0) goto Err3;
/*
.....Empty the clpath for scallop calculation
*/
	if (*nstep == 0)
		ncl_sm_clpath_remove_all(Sslpt_flag);
/*
.....clpath connection and optimization for SCRUB/COMBIN
*/
	if (*ntype != 2)
		ncl_smill_connect_clpath(*ntype,*nstep,*nend,
						trad,frad,thk,zmin,points,toler);
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
	status = 177;
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
	if (sff)
	{
		int isf;
		NCL_waterline_surf *p1;
		p1 = sff;
		for (isf = 0; isf < sfnum && p1->key != NULLKEY; isf++, p1++)
		{
			ncl_free_bound (&p1->bound);
			UU_LIST_FREE (p1->trianlist);
		}
		UU_FREE (sff);
	}
	sfs_xmmx = sfs_ymmx = UU_NULL;
	ncl_free_uv();
	ncl_free_nios();
	ncl_free_aux_ptlist();
	ncl_free_tripolst();
	ncl_free_connect_lst();
	if (key0 > NULLKEY) uc_delete (key0);

	if (points->data)
		uu_list_free(points);
	if (*nend == 1)
		if (bndpts.data) uu_list_free(&bndpts);
	if (*nend == 3 && points3d)
	{	
		if (points3d->data)
			uu_list_free(points3d);
	}

	if (*nbnd == 1)
		if (pptr.data) uu_list_free(&pptr);

	ncl_sfbucket_free();	

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_smfin()
**       SMILL finish.
**    PARAMETERS
**       INPUT  :
**          none.
**       OUTPUT :
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_smfin()
{
	ncl_sm_clpath_free(Sclpt_flag);
	ncl_sm_clpath_free(Sslpt_flag);
	return;
}
