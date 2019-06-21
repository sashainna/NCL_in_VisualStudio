/*********************************************************************
**    NAME         :  nesmclpath.c
**       CONTAINS:  ncl_sm_clpath and triangles projection routines for smill
**
**		ncl_sm_clpath_new()
**		ncl_sm_clpath_create()
**		ncl_sm_clpath_remove()
**		ncl_sm_clpath_empty()
**		ncl_sm_clpath_empty_all()
**		ncl_sm_clpath_push_at()
**		ncl_sm_clpath_push_list()
**		ncl_sm_clpath_getpts()
**		ncl_sm_clpath_get_endpts()
**		ncl_sm_clpath_getpt()
**		ncl_sm_clpath_getnpath()
**		ncl_sm_clpath_getnpts()
**		ncl_sm_clpath_not_empty()
**		ncl_sm_clpath_free()
**		ncl_sm_clpath_connect_next()
**		ncl_debug_pts()
**		ncl_debug_clpts()
**		ncl_debug_clpt()
**		ncl_debug_clpt2()
**		ncl_trian_vertex()
**		ncl_trian_edge()
**		ncl_vertex_contact()
**		ncl_vertexs_contact()
**		ncl_facet_contact()
**		ncl_edges_contact()
**		ncl_proj_tri_clpoint()
**		ncl_projsf_clpoint()
**		ncl_projsf_clpoint2()
**		ncl_projsf_clpoint_between()
**		ncl_projsf_clpoint2_between()
**		ncl_smill_project_clpath()
**		ncl_smill_past_clpts()
**    COPYRIGHT 2010 (c) Numerical Control Computer Sciences Inc.
**                          All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       nesmclpath.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:49
*********************************************************************/
#include "mdattr.h"
#include "mattr.h"
#include "mgeom.h"
#include "uminmax.h"
#include "mdpick.h"
#include "nclwaterln.h"
/*
.....Use OpenMP libary
*/
#ifdef _OPENMP
#include <omp.h>
#endif

struct sm_clpath{
	int num;				/*total number of slices*/
	UU_LIST clptlist;		/*cl point list */
};

struct sm_clpath Sclpath;	/*cl path lists*/
struct sm_clpath Sslpath;	/*connected cl path lists*/
int Sclpt_flag = 0;			/*Sclpath flag*/
int Sslpt_flag = 1;			/*Sslpath flag*/
extern UU_LIST Serads;		/*bullnose effective radius list*/
UU_LIST Sedgelst;			/*traingle edgelist*/

static int sign(z)
{
   if (z >= 1.0) 
	   return 1;
   else
	   return -1;
}

/**********************************************************************
**    E_FUNCTION     : ncl_sm_clpaths_new(nflag)
**       Allocate memory and initialize list of cl path used for SMILL
**    PARAMETERS
**       INPUT  :
**          nflag	- 0: cl path lists 1: temp cl path lists
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_SUCCESS 
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
void ncl_sm_clpath_new(nflag)
int nflag;
{
	if (nflag == Sclpt_flag)
	{
		Sclpath.num = 0;
		uu_list_init(&Sclpath.clptlist,sizeof(UU_LIST),20,20);
	}
	else if (nflag == Sslpt_flag)
	{
		Sslpath.num = 0;
		uu_list_init(&Sslpath.clptlist,sizeof(UU_LIST),20,20);
	}
}

/**********************************************************************
**    E_FUNCTION     : ncl_sm_clpath_create(nflag)
**       Allocate memory and initialize list of cl points used for 
**		creation of new cl path SMILL
**    PARAMETERS
**       INPUT  :
**          nglag   -  0: cl path lists 1: temp cl path lists
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_SUCCESS 
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
void ncl_sm_clpath_create(nflag)
int nflag;
{
	UU_LIST clptlist;	
	uu_list_init(&clptlist,sizeof(UM_clpt),100,100);

	if (nflag == Sclpt_flag)
	{
		Sclpath.num++;
		uu_list_push(&Sclpath.clptlist, &clptlist);
	}
	else if (nflag == Sslpt_flag)
	{
		Sslpath.num++;
		uu_list_push(&Sslpath.clptlist, &clptlist);
	}
}

/**********************************************************************
**    E_FUNCTION     : ncl_sm_clpath_create_all(nflag,npaths)
**       Allocate memory and initialize list of clpoints used for 
**		creation of new cl path SMILL
**    PARAMETERS
**       INPUT  :
**          nglag   -  0: cl path lists 1: temp cl path lists
**			npaths	- number of paths
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_SUCCESS 
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
void ncl_sm_clpath_create_all(nflag,npaths)
int nflag,npaths;
{
	int i;
	UU_LIST clptlist;	

	for (i = 0; i < npaths; ++i)
	{
		uu_list_init(&clptlist,sizeof(UM_clpt),100,100);

		if (nflag == Sclpt_flag)
		{
			Sclpath.num++;
			uu_list_push(&Sclpath.clptlist, &clptlist);
		}
		else if (nflag == Sslpt_flag)
		{
			Sslpath.num++;
			uu_list_push(&Sslpath.clptlist, &clptlist);
		}
	}
}

/**********************************************************************
**    E_FUNCTION     : ncl_sm_clpath_remove(nflag)
**       Remove the last path in the active Sclpath/Sslpath list.
**    PARAMETERS
**       INPUT  :
**          nflag   - cl path flag
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_SUCCESS 
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
void ncl_sm_clpath_remove(nflag)
int nflag;
{
	if (nflag == Sclpt_flag)
	{
		Sclpath.num--;
		uu_list_delete(&Sclpath.clptlist,Sclpath.num,1);
	}
	else if (nflag == Sslpt_flag)
	{
		Sslpath.num--;
		uu_list_delete(&Sslpath.clptlist,Sslpath.num,1);
	}
}

/**********************************************************************
**    E_FUNCTION     : ncl_sm_clpath_remove_all(nflag)
**       Remove the last path in the active Sclpath/Sslpath list.
**    PARAMETERS
**       INPUT  :
**          nflag   - cl path flag
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_SUCCESS 
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
void ncl_sm_clpath_remove_all(nflag)
int nflag;
{
	if (nflag == Sclpt_flag)
	{
		while (Sclpath.num > 0)
		{
			Sclpath.num--;
			uu_list_delete(&Sclpath.clptlist,Sclpath.num,1);
		}
	}
	else if (nflag == Sslpt_flag)
	{
		while (Sslpath.num)
		{
			Sslpath.num--;
			uu_list_delete(&Sslpath.clptlist,Sslpath.num,1);
		}
	}
}

/**********************************************************************
**    E_FUNCTION     : ncl_sm_clpath_empty(nflag,npath)
**       Empty the last curve in the active Sclpath list.
**    PARAMETERS
**       INPUT  :
**          nflag   - cl path flag
**          npath	- the npath slice
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_SUCCESS 
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
void ncl_sm_clpath_empty(nflag,npath)
int nflag;
UM_int2 npath;
{
	UU_LIST *clptlist;

	if(nflag == Sclpt_flag)
		clptlist = (UU_LIST *) UU_LIST_ARRAY (&Sclpath.clptlist);	
	else if (nflag == Sslpt_flag)
		clptlist = (UU_LIST *) UU_LIST_ARRAY (&Sslpath.clptlist);
	clptlist[npath].cur_cnt = 0;
}

/**********************************************************************
**    E_FUNCTION     : ncl_sm_clpath_empty_all(nflag)
**       Empty all paths in the active Sclpath/Sslpath list.
**    PARAMETERS
**       INPUT  :
**          nflag   - cl path flag
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_SUCCESS 
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
void ncl_sm_clpath_empty_all(nflag)
int nflag;
{
	int i;
	if (nflag == Sclpt_flag)
	{
		for (i = 0; i < Sclpath.num; i++)
			ncl_sm_clpath_empty(nflag,i);
		Sclpath.num = 0;
	}
	else if (nflag == Sslpt_flag)
	{
		for (i = 0; i < Sslpath.num; i++)
			ncl_sm_clpath_empty(nflag,i);
		Sslpath.num = 0;
	}
}

/**********************************************************************
**    E_FUNCTION     : ncl_sm_clpath_push(ipath,pt)
**       Push clpoint onto the icv-th curve in the Sclpath/Sslpath list
**    PARAMETERS
**       INPUT  : 
**          nflag   - cl path flag
**          ipath	- The slice index in Sclpath list
**          clpt	- Point to store on list.
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_SUCCESS 
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
int ncl_sm_clpath_push_at(nflag,ipath,clpt)
int nflag;
UM_int2 ipath;
UM_clpt *clpt;
{	
	int status;
	UU_LIST *clptlist;
	clptlist = (UU_LIST *) UU_LIST_ARRAY (&Sclpath.clptlist);
	if (clpt)
		status = uu_list_push(&clptlist[ipath], clpt);
	return(status);
}

/**********************************************************************
**    E_FUNCTION     : ncl_sm_clpath_push(nflag,clptlist)
**       Push clpoint list onto the active Sclpath/Sslpath list
**    PARAMETERS
**       INPUT  : 
**          nflag		- cl path flag
**          clptlist	- cl point list to store on list.
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_SUCCESS 
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
int ncl_sm_clpath_push_list(nflag,clptlist)
int nflag;
UU_LIST *clptlist;
{	
	int status;
	int length;
	UU_LIST *ptlist;
	if (nflag == Sclpt_flag)
		ptlist = (UU_LIST *) UU_LIST_ARRAY (&Sclpath.clptlist);
	else if (nflag == Sslpt_flag)
		ptlist = (UU_LIST *) UU_LIST_ARRAY (&Sslpath.clptlist);

	length = UU_LIST_LENGTH(clptlist);
	if (length > 0)
	{
		if (nflag == Sclpt_flag)
			status = uu_list_push_list(&ptlist[Sclpath.num-1], clptlist);
		else if (nflag == Sslpt_flag)	
			status = uu_list_push_list(&ptlist[Sslpath.num-1], clptlist);
	}
	return(status);
}

/**********************************************************************
**    E_FUNCTION     : ncl_sm_clpath_insert_list(nflag,ipath,clptlist)
**       Insert clpoint lsit onto the ipath-th Sclpath/Sslpath list
**    PARAMETERS
**       INPUT  : 
**          nflag		- cl path flag
**			ipath		- clpath index
**          clptlist    - cl point list to insert on the ipath-th list.
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_SUCCESS 
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
int ncl_sm_clpath_insert_list(nflag,ipath,clptlist)
int nflag,ipath;
UU_LIST *clptlist;
{	
	int status;
	int length;
	UU_LIST *ptlist;
	if (nflag == Sclpt_flag)
		ptlist = (UU_LIST *) UU_LIST_ARRAY (&Sclpath.clptlist);
	else if (nflag == Sslpt_flag)
		ptlist = (UU_LIST *) UU_LIST_ARRAY (&Sslpath.clptlist);

	length = UU_LIST_LENGTH(clptlist);
	if (length > 0)
	{
		if (nflag == Sclpt_flag)
			status = uu_list_push_list(&ptlist[ipath], clptlist);
		else if (nflag == Sslpt_flag)	
			status = uu_list_push_list(&ptlist[ipath], clptlist);
	}
	return(status);
}

/**********************************************************************
**    E_FUNCTION     : ncl_sm_get_getpts(nflag,npath,nclpts,clpt)
**       Get the number of points and points in the active Sclpath list
**    PARAMETERS
**       INPUT  :
**			nflag	- cl path flag
**          npath	- the path number
**       OUTPUT :
**          npts    - number of points in the list
**          clpts   - the point in the list
**    RETURNS      :
**       UU_SUCCESS 
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
void ncl_sm_clpath_getpts(nflag,npath,npts,pts)
int nflag,npath, *npts;
UM_clpt **pts;
{	
	UU_LIST *clptlist;
	if (nflag == Sclpt_flag)
		clptlist = (UU_LIST *) UU_LIST_ARRAY (&Sclpath.clptlist);
	else if (nflag == Sslpt_flag)
		clptlist = (UU_LIST *) UU_LIST_ARRAY (&Sslpath.clptlist);

	*npts = UU_LIST_LENGTH(&clptlist[npath]);
	*pts = (UM_clpt *) UU_LIST_ARRAY (&clptlist[npath]);
}

/**********************************************************************
**    E_FUNCTION     : ncl_sm_clpath_get_endpts(nflag,npath,ps,pe)
**       Get the number of points and points in the active Sclpath list
**    PARAMETERS
**       INPUT  :
**			nflag	- cl path flag
**          npath    - the path number
**       OUTPUT :
**          ps   - start point
**          pe   - end  point
**    RETURNS      :
**       UU_TRUE if end points exit, elase return FALSE 
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
UU_LOGICAL ncl_sm_clpath_get_endpts(nflag,npath,ps,pe)
int nflag,npath;
UM_coord ps,pe;
{	
	int npts;
	UM_clpt *pts;
	UU_LIST *clptlist;
	if (nflag == Sclpt_flag)
		clptlist = (UU_LIST *) UU_LIST_ARRAY (&Sclpath.clptlist);
	else if (nflag == Sslpt_flag)
		clptlist = (UU_LIST *) UU_LIST_ARRAY (&Sslpath.clptlist);

	npts = UU_LIST_LENGTH(&clptlist[npath]);
	pts = (UM_clpt *) UU_LIST_ARRAY (&clptlist[npath]);
	if (npts > 0)
	{
		um_vctovc(pts[0].pte,ps);
		um_vctovc(pts[npts-1].pte,pe);
		return UU_TRUE;
	}

	return UU_FALSE;
}

/**********************************************************************
**    E_FUNCTION     : ncl_sm_get_getclpts(npath,clpts)
**       Get the clpts list in the active Sclpath/Sslpath list
**    PARAMETERS
**       INPUT  :
**			nflag	- cl path flag
**          npath   - the path number
**       OUTPUT :
**          clpts   - the clpoint list
**    RETURNS      :
**       UU_SUCCESS 
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
int ncl_sm_clpath_getclpts(nflag,npath,clpts)
int nflag,npath;
UU_LIST **clpts;
{	
	int nc, npts;
	UU_LIST *clptlist = UU_NULL;

	npts = 0;
	nc = 0;
	if (nflag == Sclpt_flag)
		nc = UU_LIST_LENGTH (&Sclpath.clptlist);
	else if(nflag == Sslpt_flag)
		nc = UU_LIST_LENGTH (&Sslpath.clptlist);

	if (nc > 0)
	{
		if (nflag == Sclpt_flag)
			clptlist = (UU_LIST *) UU_LIST_ARRAY (&Sclpath.clptlist);
		else if (nflag == Sslpt_flag)
			clptlist = (UU_LIST *) UU_LIST_ARRAY (&Sslpath.clptlist);

		npts = UU_LIST_LENGTH(&clptlist[npath]);
		(*clpts) = (UU_LIST *) uu_malloc (sizeof(UU_LIST));
		if (npts > 0)
		{
			uu_list_init (*clpts, sizeof(UM_clpt), npts, npts);
			uu_list_push_list(*clpts,  &clptlist[npath]);
		}
	}

	return npts;
}

/**********************************************************************
**    E_FUNCTION     : ncl_sm_get_npath(nflag)
**       Get total number of path in the active Sclpath/Sslpath list
**    PARAMETERS
**			nflag	- cl path flag
**          none
**       OUTPUT :
**          none
**    RETURNS      :
**         the total number of path
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
void ncl_sm_clpath_getnpath(nflag,ix)
UM_int4 *nflag,*ix;
{	
	if (*nflag == Sclpt_flag)
		*ix = Sclpath.num;
	else if (*nflag == Sslpt_flag)
		*ix = Sslpath.num;
}

/*********************************************************************
**    E_FUNCTION     : ncl_sm_get_nptd (nflag,ix,pte)
**       Get SMILL tool position from the list.
**    PARAMETERS
**       INPUT  :
**			nflag	- cl path flag
**             ix   - the ix-th pass
**       OUTPUT :
**             nx    - the numbers of tool end points
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_sm_clpath_getnpts(nflag,ix,nx)
UM_int4 *nflag,*ix,*nx;
{
	int npts,npath;
	UU_LIST *clptlist;
	npath = *ix;

	if (*nflag == Sclpt_flag)
		clptlist = (UU_LIST *) UU_LIST_ARRAY (&Sclpath.clptlist);
	else if (*nflag == Sslpt_flag)
		clptlist = (UU_LIST *) UU_LIST_ARRAY (&Sslpath.clptlist);

	npts = UU_LIST_LENGTH(&clptlist[npath-1]);
	*nx = npts;
}

/*********************************************************************
**    E_FUNCTION     : ncl_sm_get_pt (npath,ix,pte)
**       Get SMILL tool position from the list.
**    PARAMETERS
**       INPUT  :
**			nflag	- cl path flag
**			ix		- the ix-th cl point
**          pte		- tool end, tool axis
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_sm_clpath_getpt(nflag,ipath,ix,pte)
UM_int4 *nflag,*ipath,*ix;
UM_real8 pte[6];
{
	int i,k,npath;
	UM_clpt *pos;
	UU_LIST *clptlist;
	i = *ix-1;
	npath = *ipath;

	if (*nflag == Sclpt_flag)
		clptlist = (UU_LIST *) UU_LIST_ARRAY (&Sclpath.clptlist);
	else if (*nflag == Sslpt_flag)
		clptlist = (UU_LIST *) UU_LIST_ARRAY (&Sslpath.clptlist);

	pos = (UM_clpt *) UU_LIST_ARRAY (&clptlist[npath-1]);

	pos += i;
	for (k = 0; k < 3; k++)
	{
		pte[k] = pos->pte[k];
		pte[k+3] = pos->vta[k];
	}
}

/**********************************************************************
**    E_FUNCTION     : ncl_sm_clpath_not_empty(nflag)
**       Check if all paths are empty or not in the Sclpath/Sslclpath list.
**    PARAMETERS
**       INPUT  :
**          nflag   - cl path flag
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_SUCCESS 
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
UU_LOGICAL ncl_sm_clpath_not_empty(nflag)
int nflag;
{
	int i,npts,npaths;

	if (nflag == Sclpt_flag)
		npaths=  Sclpath.num;
	else if (nflag == Sslpt_flag)
		npaths=  Sslpath.num;

	for (i = 1; i <= npaths; i++)
	{
		ncl_sm_clpath_getnpts(&nflag,&i,&npts);
		if (npts > 0)
			return UU_TRUE;
	}

	return UU_FALSE;
}

/**********************************************************************
**    E_FUNCTION     : ncl_sm_clpath_not_empty(nflag)
**       Check if all paths are empty or not in the Sclpath/Sslclpath list.
**    PARAMETERS
**       INPUT  :
**          nflag   - cl path flag
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_SUCCESS 
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
UU_LOGICAL ncl_sm_clpath_next(nflag,inext)
int nflag,*inext;
{
	int i,npts,npaths;

	if (nflag == Sclpt_flag)
		npaths=  Sclpath.num;
	else if (nflag == Sslpt_flag)
		npaths=  Sslpath.num;

	for (i = 1; i <= npaths; i++)
	{
		ncl_sm_clpath_getnpts(&nflag,&i,&npts);
		if (npts > 0)
		{
			*inext = i-1;
			return UU_TRUE;
		}
	}

	return UU_FALSE;
}

/**********************************************************************
**    E_FUNCTION     : ncl_sm_clpath_free(nflag)
**       Free the active Sclpath curve list
**    PARAMETERS
**       INPUT  :
**			nflag	- cl path flag
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_SUCCESS 
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
void ncl_sm_clpath_free(nflag)
int nflag;
{		
	if (nflag == Sclpt_flag)
	{
		Sclpath.num =0;
		uu_list_free (&Sclpath.clptlist);
	}
	else if (nflag == Sslpt_flag)
	{
		Sslpath.num =0;
		uu_list_free (&Sslpath.clptlist);
	}
}

/**********************************************************************
**    E_FUNCTION     : ncl_sm_clpath_connect_next(ntype,trad,ps,pe,ipath,inext)
**       Get next clpath from Sclpath lists
**    PARAMETERS
**       INPUT  :
**			ntype	- 0:SCRUB 1:COMBIN
**			trad	- tool radius
**			ps,pe	- start/end point
**			ipath	- cl path flag
**       OUTPUT :
**          inext	- next cl path flag
**    RETURNS      :
**       UU_SUCCESS 
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
UU_LOGICAL ncl_sm_clpath_connect_next(ntype,trad,ps,pe,ipath,inext)
int ipath,*inext;
UU_REAL trad;
UM_coord ps,pe;
{
	int i,ipath_min;
	UM_coord ps1,pe1;
	UU_REAL dis,min_dis = 1000000.0;
	UU_REAL dis2,min_dis2 = 1000000.0;
	UU_REAL um_sqdis_from_segment_2d();
	UU_LOGICAL lmin;
	lmin = UU_FALSE;
	ipath_min  = ipath+1;

	for (i = ipath+1; i < Sclpath.num; i++)
	{
		ncl_sm_clpath_get_endpts(Sclpt_flag,i,ps1,pe1);
		dis = UM_SQDIS_2D(pe,ps1);

		dis2 = um_sqdis_from_segment_2d(pe,ps,ps1,UU_FALSE);
		if (dis2 < UM_DFUZZ)
			continue;
		else if (dis2 > min_dis2 + UM_FUZZ)
			break;
		min_dis2 = dis2;

		if (ntype == 0 && dis > 16.0*trad * trad ||
			ntype == 1 && dis > 64.0*trad * trad)
			continue;

		if (dis < min_dis)
		{
			ipath_min = i;
			min_dis = dis;
			lmin= UU_TRUE;
			if (ntype == 1) break;
		}
		else if (dis > min_dis + UM_FUZZ)
			break;
	}
			
	*inext = ipath_min;
	return lmin;
}

/*********************************************************************
**    E_FUNCTION     : ncl_debug_clpts(clptlist)
**       Debug cl point list routine.
**    PARAMETERS
**       INPUT  :
**			clptlist	- cl point list
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_debug_pts(ptlist,color)
UU_LIST *ptlist;
int color;
{
	int i,npts;
	UM_coord *pos;
	char tbuf[80];
	pos = (UM_coord *) UU_LIST_ARRAY (ptlist);
	npts = UU_LIST_LENGTH(ptlist);
	if (color > 0)
		ncl_draw_polyline (npts,pos,color,1);
	for (i = 0; i < npts-1; i++)
	{
		if (um_dcccc(pos[i],pos[i+1]) < .001)
			sprintf(tbuf,"Point/%8.5f,%8.5f,%8.5f",pos[i][0],pos[i][1],pos[i][2]);
		else
			sprintf(tbuf,"Line/%8.5f,%8.5f,%8.5f,%8.5f,%8.5f,%8.5f",
				pos[i][0],pos[i][1],pos[i][2],
				pos[i+1][0],pos[i+1][1],pos[i+1][2]);
		NclxDbgPstr(tbuf);
	}
	NclxDbgPstr("*stop");
}

/*********************************************************************
**    E_FUNCTION     : ncl_debug_clpts(clptlist)
**       Debug cl point list routine.
**    PARAMETERS
**       INPUT  :
**			clptlist	- cl point list
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_debug_clpts(clptlist,color)
UU_LIST *clptlist;
int color;
{
	int i,npts;
	UM_clpt *pos;
	char tbuf[80];
	pos = (UM_clpt *) UU_LIST_ARRAY (clptlist);
	npts = UU_LIST_LENGTH(clptlist);
	if (color > 0)
		ncl_draw_polyline (npts,pos,color,1);
	for (i = 0; i < npts-1; i++)
	{
		sprintf(tbuf,"Line/%8.5f,%8.5f,%8.5f,%8.5f,%8.5f,%8.5f",
			pos[i].pte[0],pos[i].pte[1],pos[i].pte[2],
			pos[i+1].pte[0],pos[i+1].pte[1],pos[i+1].pte[2]);
		NclxDbgPstr(tbuf);
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_debug_clpt(pt,color)
**       Debug cl routine.
**    PARAMETERS
**       INPUT  :
**			clptlist	- cl point list
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_debug_clpt(pt,color)
UM_coord pt;
int color;
{
	char tbuf[80];
	if (color > 0)
	{
		sprintf(tbuf,"draft/modify,color = %d",color); 
		NclxDbgPstr(tbuf);
	}
	sprintf(tbuf,"PT/%8.5f,%8.5f,%8.5f",pt[0],pt[1],pt[2]);
	NclxDbgPstr(tbuf);
}

/*********************************************************************
**    E_FUNCTION     : ncl_debug_clpt(pt,color)
**       Debug cl routine.
**    PARAMETERS
**       INPUT  :
**			clptlist	- cl point list
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_debug_clpt2(pt1,pt2,color)
UM_coord pt1,pt2;
int color;
{
	char tbuf[80];
	if (color > 0)
	{
		sprintf(tbuf,"draft/modify,color = %d",color); 
		NclxDbgPstr(tbuf);	
	}
	sprintf(tbuf,"LN/%8.5f,%8.5f,%8.5f,%8.5f,%8.5f,%8.5f",
				pt1[0],pt1[1],pt1[2],pt2[0],pt2[1],pt2[2]);		
	NclxDbgPstr(tbuf);
}

/*********************************************************************
**    E_FUNCTION     :  ncl_trian_vertex(nvertex,ptri,pt)
**       Get the triangle vertex given the vertex index number
**    PARAMETERS
**       INPUT  :
**			nvertex	-traingle index number
**			ptri	-triangle
**       OUTPUT :
**          pt     - vertex point
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static ncl_trian_vertex(nvertex,ptri,pt)
int nvertex;
UM_trian *ptri;
UM_coord pt;
{
	if (nvertex == 0)
		um_vctovc (ptri->p1,pt);
	else if (nvertex == 1)
		um_vctovc (ptri->p2,pt);
	else if (nvertex == 2)
		um_vctovc (ptri->p3,pt);
}

/*********************************************************************
**    E_FUNCTION     :  ncl_trian_vertex(nvertex,ptri,pt)
**       Get the triangle edge vertexes given the edge index number
**    PARAMETERS
**       INPUT  :
**			nedge	-traingle edge index number
**			ptri	-triangle
**       OUTPUT :
**          pt1     - edge vertex point
**          pt2     - edge vertex point
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_trian_edge(nedge,ptri,pt1,pt2)
int nedge;
UM_trian *ptri;
UM_coord pt1,pt2;
{
	if (nedge == 0)
	{
		um_vctovc (ptri->p1,pt1);
		um_vctovc (ptri->p2,pt2);
	}
	else if (nedge == 1)
	{
		um_vctovc (ptri->p2,pt1);
		um_vctovc (ptri->p3,pt2);
	}
	else if (nedge == 2)
	{
		um_vctovc (ptri->p3,pt1);
		um_vctovc (ptri->p1,pt2);
	}
}

/*********************************************************************
**    E_FUNCTION     :  ncl_vertex_contact(crad,frad,pt,clpt,ccpt)
**       Calculate the clpt max z and ccpt given vertex pt and clpt(x,y)
**    PARAMETERS
**       INPUT  :
**			crad	- tool corner radius
**			frad	- tool flat radius
**          pt     - vertex point
**          clpt   - the clpt (x,y)
**       OUTPUT :
**          clpt   - the clpt (x,y,maxz)
**          ccpt   - the ccpt
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void  ncl_vertex_contact(crad,frad,pt,clpt,clmaxz,ccpt)
UU_REAL crad,frad,*clmaxz;
UM_coord pt,clpt,ccpt;
{	
	UU_REAL dh,dq,dq2,radius,radius2,frad2;
/*
	char tbuf[80];
*/
	radius = crad + frad;
	radius2 = radius * radius;
	dq2 = UM_SQDIS_2D (clpt,pt);
/*
.....Ball Tool
*/
	if (crad > UM_FUZZ && frad < UM_FUZZ)
	{
		if (dq2 <= radius2)
		{
			dh = radius - sqrt(radius2 - dq2); 
			if (pt[2] - dh > *clmaxz)
			{
				*clmaxz = pt[2] - dh;
				um_vctovc(pt, ccpt);
			}
		}
	}
	else if (crad > UM_FUZZ && frad > UM_FUZZ)
	{
/*
.....Bullnose Tool
*/
		frad2 = frad * frad;
		if (dq2 <= frad2)
		{
/*
.....vertex within flat area of tool
*/
			if (pt[2] > *clmaxz)
			{
				*clmaxz = pt[2];
				um_vctovc(pt, ccpt);
			}
		}
		else if (dq2 <= radius2)
		{
/*
.....vertex within corner area of tool
*/
			dq = sqrt(dq2);
			dh = crad - sqrt(crad*crad - (dq-frad)*(dq-frad));  
			if (pt[2] - dh > *clmaxz)
			{
				*clmaxz = pt[2] - dh;
				um_vctovc(pt, ccpt);
			}
		}
	}
}

/*********************************************************************
**    E_FUNCTION     :  ncl_vertexs_contact(crad,frad,ptri,clpt,ccpt)
**       Calculate the clpt max z and ccpt given traingle vertexs
**    PARAMETERS
**       INPUT  :
**			crad	- tool corner radius
**			frad	- tool flat radius
**          ptri   - triangle
**          clpt   - the clpt (x,y)
**       OUTPUT :
**          clpt   - the clpt (x,y,maxz)
**          ccpt   - the ccpt
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_vertexs_contact(crad,frad,ptri,clpt,clmaxz,ccpt)
UU_REAL crad,frad,clmaxz;
UM_trian *ptri;
UM_coord clpt,ccpt;
{ 
	int nvertex;
	UM_coord pt;
	for (nvertex = 0; nvertex < 3; nvertex++)
	{
		ncl_trian_vertex(nvertex,ptri,pt);
		ncl_vertex_contact(crad,frad,pt,clpt,clmaxz,ccpt);
	}
} 

/*********************************************************************
**    E_FUNCTION     :  ncl_facet_contact(crad,frad,ptri,clpt,clmaxz,tol,ccpt)
**       Calculate the clpt max z and ccpt given traingle facet 
**    PARAMETERS
**       INPUT  :
**			crad	- tool corner radius
**			frad	- tool flat radius
**          ptri   - triangle 
**          clpt   - the clpt (x,y)
**       OUTPUT :
**          clpt   - the clpt (x,y,maxz)
**          ccpt   - the ccpt
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL ncl_facet_contact(crad,frad,ptri,clpt,clmaxz,tol,ccpt)
UU_REAL crad,frad,*clmaxz,tol;
UM_trian *ptri;
UM_coord clpt,ccpt;
{ 
     UM_vector norm,v1,v2,vec,vec_horz;
	 UM_coord  ccpos; 
	 UU_REAL a,b,c,d,clz,radius;
	 UU_LOGICAL linside,lcontact;
/*
	 char tbuf[80];
*/
	 linside = UU_FALSE;
	 lcontact = UU_FALSE;
	 radius = crad + frad;
/*
.....Triangle normal
*/
	 um_vcmnvc (ptri->p2, ptri->p1, v1);
	 um_vcmnvc (ptri->p3, ptri->p2, v2);
     um_cross (v1, v2, norm);
	 um_unitvc(norm, norm);
/*
.....Vertical triangle
*/
     if (fabs(norm[2]) < UM_FUZZ) 
         return lcontact;  
	 else if (norm[2] < 0)
		 um_negvc(norm,norm); 
/*
.....Horizontal triangle 
*/
     if (fabs(norm[2] - 1.0) < UM_FUZZ)
	 { 
         ccpos[0] = clpt[0]; 
         ccpos[1] = clpt[1]; 
         ccpos[2] = ptri->p1[2]; 
		 linside = um_point_isin_triangle(ccpos,ptri->p1,ptri->p2,ptri->p3,tol);
         if (linside) 
		 {   
			 *clmaxz = ccpos[2];
			 um_vctovc (ccpos, ccpt);
		     lcontact = UU_TRUE;
         } 		
		 return linside; 
     }   
/*     
.....Traingle plane equation a*x + b*y + c*z + d = 0 
*/
     a = norm[0]; 
     b = norm[1]; 
     c = norm[2]; 
     d = - a * ptri->p1[0] - b * ptri->p1[1] - c * ptri->p1[2];                
/*
.....The vector from tool center to contact point
*/
     um_vctovc (norm, vec);
	 if (frad < UM_DFUZZ)
	 {
		 vec[0] = -radius * vec[0]; 
		 vec[1] = -radius * vec[1]; 
		 vec[2] = -radius * vec[2]; 
	 }
	 else if (frad > UM_DFUZZ)
	 {
/*
.....The horiztal vector
*/
		 um_vctovc (vec, vec_horz);
		 vec_horz[2] = 0.0; 
		 um_unitvc(vec_horz, vec_horz);

		 vec[0] = -crad * vec[0] - frad * vec_horz[0]; 
		 vec[1] = -crad * vec[1] - frad * vec_horz[1]; 
		 vec[2] = -crad * vec[2] - frad * vec_horz[2]; 
	 }
/*
.....The contact point (x,y) coordinates
*/
	 um_vcplvc(clpt,vec,ccpos);
/*
.....Check if contact point is inside the traingle facet
*/
	 linside = um_point_isin_triangle(ccpos,ptri->p1,ptri->p2,ptri->p3,tol);
	 if (linside) 
	 {
/*
.....The contact point z value
*/
	   ccpos[2] = (1.0/c)*(-d - a * ccpos[0] - b * ccpos[1]);
/*
.....The cl point z value
*/
	   clz = ccpos[2] - vec[2] - crad; 

	   *clmaxz = clz;
	   um_vctovc (ccpos, ccpt);
	   lcontact = UU_TRUE;
	 } 

	 return linside;
} 
 
/*********************************************************************
**    E_FUNCTION     :  ncl_edge_contact(crad,ptri,clpt,ccpt)
**       Calculate the clpt max z and ccpt given traingle edges for Ball
**    PARAMETERS
**       INPUT  :
**			crad	- tool corner radius
**          ptri   - triangle 
**          clpt   - the clpt (x,y)
**       OUTPUT :
**          clpt   - the clpt (x,y,maxz)
**          ccpt   - the ccpt
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_edges_contact(crad,ptri,clpt,clmaxz,ccpt)
UU_REAL crad,*clmaxz;
UM_trian *ptri;
UM_coord clpt,ccpt;
{ 
	UM_coord pt1,pt2,scpt,ccpos;
	UM_vector norm,vec,vec1,vec2;
	int nedge;
	UU_REAL ccdis,clz,dispt1,dispt2,du,dz,radius,sdis,t,t1,t2;
	UU_REAL dis2,radius2;
	UU_REAL um_sqdis_from_segment_2d();

	radius = crad;
    radius2 = radius * radius;	
	um_vctovc(clpt,scpt); 

    for (nedge = 0; nedge < 3; nedge++) 
	{
/*
.....Get the triangle edges
*/ 
		ncl_trian_edge(nedge,ptri,pt1,pt2);         
/*
.....Check the edge is not vertical
*/ 
		if (fabs(pt1[0]-pt2[0]) > UM_DFUZZ || fabs(pt1[1]-pt2[1]) > UM_DFUZZ)
		{    
/*
.....The minmum 2d distance from clpt to the edge
*/		
			dis2 = um_sqdis_from_segment_2d (clpt,pt1,pt2,UU_FALSE);             
			if (dis2 <= radius2)
			{
/*
				ncl_debug_clpt2(pt1,pt2,6);
*/
/*
.....The intersection circle radius
*/            
				sdis = sqrt(radius2 - dis2);                   
/*
.....The intersection circle center point scpt
*/
 		 		um_nptln_2d(clpt,pt1,pt2,scpt);
     
			 	um_vcmnvc (pt2,pt1,vec);
                vec[2] =0; 
                um_unitvc(vec,vec);

				um_vcmnvc (pt1,scpt,vec1);
			 	um_vcmnvc (pt2,scpt,vec2);
				dispt2 = um_dot(vec2,vec);
				dispt1 = um_dot(vec1,vec);
/*
.....The vertical plane of the line:   
*/
                dz = pt2[2] - pt1[2];   
                du = dispt2 - dispt1;              
/*
.....The normal to the line
*/
				norm[0] = dz;
				norm[1] = -du;
				norm[2] = 0.0;
                um_unitvc(norm,norm);

				if (norm[1] < 0.0)				
					um_negvc(norm,norm);  
/*
.....The edge is horizontal
*/
				if (fabs(norm[1]) < UM_DFUZZ) 
				{                    
					um_vctovc(scpt,ccpos); 
/*
.....Get the contact point on the edge
*/
					um_get_between_point(pt1,pt2,UU_TRUE,ccpos);
					clz = ccpos[2] + sdis - radius;   
				} 
				else
				{ 
/*
.....The horizon distance of contact point in plane coordinate
*/
					ccdis = - sdis * norm[0];
/*
.....The contact point in XY plane
*/
					um_translate_point(scpt,ccdis,vec,ccpos);
/*
.....Get the contact point on the edge
*/
					um_get_between_point(pt1,pt2,UU_TRUE,ccpos);
					clz = ccpos[2] + sdis * norm[1] - radius; 
				} 
/*
.....Check if the contact point is in the edge
*/
				if (um_point_in_segment(ccpos,pt1,pt2))
				{ 
/*
					ncl_debug_clpt2(pt1,pt2,9);
				    ncl_debug_triangle(0,1,1,0,ptri);
					ncl_debug_clpt(ccpos,10);
*/	
					if (clz > *clmaxz)
					{				
						*clmaxz = clz;
						um_vctovc(ccpos,ccpt);
					}
				} 
			}
		}    
     }
} 

/*********************************************************************
**    E_FUNCTION     :  ncl_proj_tri_clpoint(crad, frad, ptri, clpt, ccpt)
**       Calculate the clpt max z and ccpt given a triangle
**
** "Interferance-free tool-path generation in the NC machining of
**  parametric compound surfaces", CAD,Vol.24,No.12(1992)pp667-676.
**    PARAMETERS
**       INPUT  :
**			crad	- tool corner radius
**			frad	- tool flat radius
**          ptri	- triangle 
**          clpt	- the clpt (x,y)
**          tool	- Tool data
**       OUTPUT :
**          clpt	- the clpt (x,y,maxz)
**          ccpt	- the ccpt
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_proj_tri_clpoint(crad,frad,ptri,clpt,clmaxz,ccpt)
UU_REAL crad,frad,*clmaxz;
UM_trian *ptri;
UM_coord clpt,ccpt;
{
	UU_LOGICAL linside = UU_FALSE;
	linside = ncl_facet_contact(crad,frad,ptri,clpt,clmaxz,UM_DFUZZ,ccpt);
	if (!linside)
	{
		ncl_vertexs_contact(crad,frad,ptri,clpt,clmaxz,ccpt);
		if (frad < UM_FUZZ)
			ncl_edges_contact(crad,ptri,clpt,clmaxz,ccpt);
		else
		    ncl_edges_contact1(crad,frad,ptri,clpt,clmaxz,ccpt);
	}
}

/*********************************************************************
**    FUNCTION : UU_LOGICAL ncl_point_inside_triangles(p, trianlst)
**
**      Finds if a point is inside of triangles
**
**    PARAMETERS
**       INPUT  :
**                  p        - point
**                  trianlst - triangle list
**       OUTPUT :
**                   none
**    RETURNS      : UU_TRUE if p is inside triangle, UU_FALSE otherwise
**    SIDE EFFECTS : none
*********************************************************************/
UU_LOGICAL ncl_point_inside_triangles(pos,trianlst,tol)
UM_2Dcoord pos;
UU_LIST *trianlst;
UU_REAL tol;
{
	int i, ntri;
	UM_trian *ptri;
	UU_REAL um_sqdis_from_segment_2d();
	UU_LOGICAL linside = UU_FALSE;

	ptri = (UM_trian *)UU_LIST_ARRAY(trianlst);
	ntri = trianlst->cur_cnt;
	
	for (i = 0; i < ntri; i++, ++ptri)
	{
		linside = um_point_isin_triangle(pos,ptri->p1,ptri->p2,ptri->p3,tol);
		if (linside)
			break;
	}

	if (!linside)
	{
		ptri = (UM_trian *)UU_LIST_ARRAY(trianlst);
		for (i = 0; i < ntri; i++, ++ptri)
    	{	
			if (um_sqdis_from_segment_2d(pos,ptri->p1,ptri->p2,UU_TRUE) < tol ||
				um_sqdis_from_segment_2d(pos,ptri->p2,ptri->p3,UU_TRUE) < tol ||
				um_sqdis_from_segment_2d(pos,ptri->p3,ptri->p1,UU_TRUE) < tol)
			{
				linside = UU_TRUE;
				break;
			}
		}
	}

	return linside;
}

/*********************************************************************
**    E_FUNCTION   :  ncl_proj_sf_clpoint(crad,frad,clpt,ccpt,lcheck,irec)
**
**       Calculate the clpt max z and ccpt given surfaces number
**    PARAMETERS
**       INPUT  :
**			crad	- tool corner radius
**			frad	- tool flat radius
**          clpt	- the clpt (x,y)
**			lcheck	- flag to check point inside traingles or not
**			irec	- recursion flag
**       OUTPUT :
**          clpt	- the clpt (x,y,maxz)
**          ccpt	- the ccpt
**          inside - UU_TRUE: point lies inside a triangle.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_projsf_clpoint(crad,frad,clpt,ccpt,thk,tol,lcheck,irec,clpoints,inside)
int irec;
UU_REAL crad,frad,thk,tol;
UM_coord clpt,ccpt;
UU_LOGICAL lcheck,*inside;
UU_LIST *clpoints;
{
	int i,j,k,kmax,ntri;
	UU_REAL zmin,erad,*clptz,clmaxz,pmill_maxz;
	UM_coord *ccpts;
	UM_clpt pos;
	UM_trian *ptri;
	UM_vector clvc;
	UU_LIST trianlst;
	UU_LOGICAL linside,pmill_flag;
	int iret = UU_FAILURE;
/*
	char tbuf[80];
*/
	ncl_pmill_getfl(&pmill_flag);
	ncl_pmill_getmaxz(&pmill_maxz);
	uu_list_init0(&trianlst);
	if (irec == 10)
		return iret;
	++irec;
	if (inside) *inside = UU_TRUE;
	zmin = clpt[2];
	clvc[0] = 0.0;
	clvc[1] = 0.0;
	clvc[2] = 1.0;
#if 0
	sprintf(tbuf,"$$The point to project"); 
	NclxDbgPstr(tbuf);
	ncl_debug_clpt(clpt,4);
#endif
/*
.....Get the triangle list within the trad of clpt
*/
	uu_list_init (&trianlst,sizeof (UM_trian),100,100);
	if (!pmill_flag)
	{
		if (frad > UM_FUZZ)
			ncl_sfbucket_getpt_trianlst(clpt,frad+crad,&trianlst,UU_TRUE);
		else
			ncl_sfbucket_getpt_trianlst(clpt,frad+crad,&trianlst,UU_FALSE);
	}
	else
		ncl_pmill_getpt_trianlst(frad+crad,clpt,&trianlst, tol);

	ptri = (UM_trian *)UU_LIST_ARRAY(&trianlst);
	ntri = trianlst.cur_cnt;
/*
.....Check if clpt is inside triangles below
*/
	if (lcheck)
	{
		linside = ncl_point_inside_triangles(clpt,&trianlst,9*tol*tol);
		if (!linside)
		{
			if (trianlst.data) 		
				uu_list_free(&trianlst);
			if (inside) *inside = UU_FALSE;
			return iret;
		}
	}

	clptz = (UU_REAL*) uu_malloc(ntri * sizeof(UU_REAL));	
	ccpts = (UM_coord*) uu_malloc(ntri * sizeof(UM_coord));

    #pragma omp parallel for private(i)
	for (i = 0; i < ntri; i++)		
		clptz[i] = clpt[2];

	#pragma omp parallel for private(j)
	for (j = 0; j < ntri; j++)
	{
		ncl_proj_tri_clpoint(crad,frad,&ptri[j],clpt,&clptz[j],ccpts[j]);
	}
	
	clmaxz = zmin;
	kmax = -1;
	for (k = 0; k < ntri; k++)
	{
		if (clptz[k] > clmaxz)
		{
			clmaxz = clptz[k];
			kmax = k;
		}
	}

	if (kmax >=0 && kmax < ntri)   
		um_vctovc(ccpts[kmax],ccpt);
	
	if (trianlst.data) 		
		uu_list_free(&trianlst);	
	uu_free(clptz);
	uu_free(ccpts);

/*
.....Store the clpt
*/	
	clpt[2] = clmaxz;
	if (clpt[2] > zmin)
	{
		if (fabs(thk) < UM_FUZZ)
			um_vctovc(clpt, pos.pte);
		else
			um_translate_point(clpt,thk,clvc,pos.pte);
		um_vctovc(clvc, pos.vta);
		if (!pmill_flag)
			uu_list_push(clpoints,&pos);
		else
			um_vctovc(pos.pte, clpt);

		iret = UU_SUCCESS;
#if 0
		sprintf(tbuf,"$$The projected cl point"); 
		NclxDbgPstr(tbuf);
		ncl_debug_clpt(clpt,9);
		sprintf(tbuf,"$$The contact point"); 
		NclxDbgPstr(tbuf);	
		ncl_debug_clpt(ccpt,8);
		if (um_dcccc(clpt,ccpt) > 2.0*tol)
		{
			sprintf(tbuf,"$$The contact point to tool center"); 
			NclxDbgPstr(tbuf);	
			ncl_debug_clpt2(clpt,ccpt,5);
		}
#endif
	}
	return iret;
}

/*********************************************************************
**    E_FUNCTION   :  ncl_proj_sf_clpoint2(crad,frad,clpt,ccpt,lcheck,irec)
**
**       Calculate the clpt max z and ccpt given surfaces number
**    PARAMETERS
**       INPUT  :
**			crad	- tool corner radius
**			frad	- tool flat radius
**			slvec	- slice vector
**          clpt	- the clpt (x,y)
**			lcheck	- flag to check point inside traingles or not
**			irec	- recursion flag
**       OUTPUT :
**          clpt	- the clpt (x,y,maxz)
**          ccpt	- the ccpt
**          inside - UU_TRUE: point lies inside a triangle.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_projsf_clpoint2(crad,frad,slvec,clpt,ccpt,thk,tol,lcheck,irec,clpoints,
	inside)
int irec;
UU_REAL crad,frad,thk,tol;
UM_vector slvec;
UM_coord clpt,ccpt;
UU_LOGICAL lcheck,*inside;
UU_LIST *clpoints;
{
	UM_vector vec;
	UU_REAL erad,dcs;
	int iret = UU_FAILURE;
/*
	char tbuf[80];
*/
	iret = ncl_projsf_clpoint(crad,frad,clpt,ccpt,thk,tol,lcheck,irec,
		clpoints,inside);
	if (iret == UU_SUCCESS)
	{
/*
		sprintf(tbuf,"$$The projected cl point"); 
		NclxDbgPstr(tbuf);
		ncl_debug_clpt(clpt,9);
		sprintf(tbuf,"$$The contact point"); 
		NclxDbgPstr(tbuf);	
		ncl_debug_clpt(ccpt,8);
		if (um_dcccc(clpt,ccpt) > 2.0*tol)
		{
			sprintf(tbuf,"$$The contact point to tool center"); 
			NclxDbgPstr(tbuf);	
			ncl_debug_clpt2(clpt,ccpt,5);
		}
*/
		if (frad > UM_FUZZ)
		{
			erad = um_dist_2d(clpt,ccpt);
			if (erad < frad || fabs(clpt[2]-ccpt[2]) < tol)
				erad = frad;
			else
			{			
				um_vcmnvc(clpt,ccpt,vec);
				vec[2] = 0;
			    um_unitvc(vec, vec);
				dcs = um_dot(vec,slvec);
				if (dcs < 0)
					dcs = -dcs;
				erad = dcs * erad;
				if (erad < crad)
					erad = crad;
			}
			uu_list_push(&Serads,&erad);
		}
	}

	return iret;
}

/*********************************************************************
**    E_FUNCTION   :  ncl_projsf_clpoint_middle(crad,frad,zmin,
**										tol,slvec,clpt1,clpt2,irec)
**
**       Calculate the clpt max z and ccpt given surfaces number
**    PARAMETERS
**       INPUT  :
**			crad	- tool corner radius
**			frad	- tool flat radius
**			zmin	- minimum z
**			tol		- tolerance to check
**			slvec	- slice vector
**          clpt1	- the start clpt (x,y,z)
**          clpt2	- the end clpt (x,y,z)
**			lcheck	- flag to check point inside traingles or not
**       OUTPUT :
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_projsf_clpoint_middle(crad,frad,thk,zmin,tol,
							  clpt1,clpt2,lcheck,irec,clpoints)
int irec;
UU_REAL crad,frad,thk,zmin,tol;
UM_coord clpt1,clpt2;
UU_LOGICAL lcheck;
UU_LIST *clpoints;
{
	UM_coord clpt,ccpt,nvec;
	UM_vector clvc;
	int iret = UU_FAILURE;
	UM_int2 idx;
	UM_real8 ver;
	
	idx = 169; getsc(&idx,&ver);
	clvc[0] = 0.0;
	clvc[1] = 0.0;
	clvc[2] = 1.0;
/*
.....The middle point between
*/
	um_middlept_2d(clpt1,clpt2,clpt);
	clpt[2] = zmin;
	if (UM_SQDIS_2D(clpt1,clpt2) < 4.0*tol*tol)
		return UU_SUCCESS;
/*
.....Project the middle point and check if within the tolerance
*/
	iret = ncl_projsf_clpoint(crad,frad,clpt,ccpt,
						thk,tol,lcheck,irec,clpoints,UU_NULL);
/*
.......Added additional midpoint calculation so sharp changes would
.......be better represented without tightening tolerance - ASF 7/16/13
*/
	if (iret == UU_SUCCESS && ((ver > 10.002 && irec > 0) || ver <= 10.002) &&
		um_points_within_tol(clpt1,clpt,clpt2,2.0*tol))
		return UU_SUCCESS;
/*
.....Recursion until within the tolerance
*/
	if (iret == UU_SUCCESS)
	{
		iret = ncl_projsf_clpoint_middle(crad,frad,thk,zmin,tol,
								clpt1,clpt,lcheck,irec+1,clpoints);
		iret = ncl_projsf_clpoint_middle(crad,frad,thk,zmin,tol,
								clpt,clpt2,lcheck,irec+1,clpoints);
	}
	return iret;
}

/*********************************************************************
**    E_FUNCTION   :  ncl_projsf_clpoint2_middle(crad,frad,zmin,
**										tol,slvec,clpt1,clpt2,irec)
**
**       Calculate the clpt max z and ccpt given surfaces number
**    PARAMETERS
**       INPUT  :
**			crad	- tool corner radius
**			frad	- tool flat radius
**			zmin	- minimum z
**			tol		- tolerance to check
**			slvec	- slice vector
**          clpt1	- the start clpt (x,y,z)
**          clpt2	- the end clpt (x,y,z)
**			lcheck	- flag to check point inside traingles or not
**       OUTPUT :
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_projsf_clpoint2_middle(crad,frad,thk,zmin,tol,slvec,
							  clpt1,clpt2,lcheck,irec,clpoints)
int irec;
UU_REAL crad,frad,thk,zmin,tol;
UM_vector slvec;
UM_coord clpt1,clpt2;
UU_LOGICAL lcheck;
UU_LIST *clpoints;
{
	UM_coord clpt,ccpt,nvec;
	UM_vector clvc;
	int iret = UU_FAILURE;
	UM_int2 idx;
	UM_real8 ver;
	
	idx = 169; getsc(&idx,&ver);
	clvc[0] = 0.0;
	clvc[1] = 0.0;
	clvc[2] = 1.0;
/*
.....The middle point between
*/
	um_middlept_2d(clpt1,clpt2,clpt);
	clpt[2] = zmin;
	if (UM_SQDIS_2D(clpt1,clpt2) < 4.00*tol*tol)
		return UU_SUCCESS;
/*
.....Project the middle point and check if within the tolerance
*/
	iret = ncl_projsf_clpoint2(crad,frad,slvec,clpt,ccpt,
						thk,tol,lcheck,irec,clpoints,UU_NULL);
/*
.......Added additional midpoint calculation so sharp changes would
.......be better represented without tightening tolerance - ASF 7/16/13
*/
	if (iret == UU_SUCCESS && ((ver > 10.002 && irec > 0) || ver <= 10.002) &&
		um_points_within_tol(clpt1,clpt,clpt2,2.0*tol))
		return UU_SUCCESS;
/*
.....Recursion until within the tolerance
*/
	if (iret == UU_SUCCESS)
	{
		iret = ncl_projsf_clpoint2_middle(crad,frad,thk,zmin,tol,
							slvec,clpt1,clpt,lcheck,irec+1,clpoints);
		iret = ncl_projsf_clpoint2_middle(crad,frad,thk,zmin,tol,
							slvec,clpt,clpt2,lcheck,irec+1,clpoints);
	}
	return iret;
}

/*********************************************************************
**    E_FUNCTION   : ncl_smill_project_slice(trad,frad,thk,zmin,i,
**										vdir,ipt1,ipt2,toler,clpts)
**       smill project slice routine.
**       INPUT  :
**			trad		- tool radius
**			frad		- tool flat radius
**			thk			- part surfaces thin value
**			zmin		- zmin value
**			vdir		- direction vector
**			vdir1		- direction vector
**			ipt1		- slice start point
**			ipt2		- slice end point
**			toler		- tolerance
**       OUTPUT :
**			none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_smill_project_clpath(trad,frad,thk,zmin,nend,ntype,i,slvec,
						index,ipt1,ipt2,bndpts,bbox0,toler,wtol,clpts)
UU_REAL trad,frad,thk,zmin,toler,wtol;
int nend,ntype,i,index;
UM_coord ipt1,ipt2;
UM_vector slvec;
UU_LIST *bndpts;
UM_2box *bbox0;
UU_LIST *clpts;
{
	int iret1,iret2,j,k, nsteps;	
	UU_REAL v;
	UM_coord ccpt1,clpt1,clpt2;
	UU_LOGICAL lcheck;
	char tbuf[80];
			
	nsteps = ceil(um_dist_2d(ipt1,ipt2)/(trad + frad)*2.0);
/*
.....Project points for each slice
*/		
	iret1 = UU_FAILURE;		
	iret2 = UU_FAILURE;		
	for (j = 0; j <= nsteps; j++)		
	{
		v = j * (trad + frad) * 0.5;
		um_translate_point(ipt1,v,slvec,clpt1);	
		if (j == nsteps)			
			um_vctovc(ipt2,clpt1);	
		clpt1[2] = zmin;
						
		if (j <= 2 || j >= nsteps-2)			
			lcheck = UU_FALSE;		
		else		
			lcheck = UU_TRUE;
		
		iret1 = ncl_projsf_clpoint(trad,frad,clpt1,ccpt1,
								thk,toler,lcheck,0,clpts,UU_NULL);	
		if (iret1 == UU_SUCCESS && iret2 == UU_SUCCESS)
			ncl_projsf_clpoint_middle(trad,frad,thk,zmin,toler,
								clpt1,clpt2,lcheck,0,clpts);

		if (iret1 == UU_SUCCESS)
		{
			iret2 = iret1;
			um_vctovc(clpt1,clpt2);
		}
	}

/*
.....Sort the cl points
*/
	ncl_sort_clpts(clpts,slvec,index,ntype,toler);

#if 0	
	sprintf(tbuf,"i = %d",i); 	
	NclxDbgPstr(tbuf);
	sprintf(tbuf,"*stop"); 	
	NclxDbgPstr(tbuf);
	ncl_debug_clpts(clpts,0);
#endif
}

/*********************************************************************
**    E_FUNCTION   :  ncl_smill_past_clpts(crad,frad,thk,zmin,tol,
*										slvec,clpt1,clpt2,clptlist)
**
**       Calculate the toclpath for the past sections at both ends
**    PARAMETERS
**       INPUT  :
**			crad	- tool corner radius
**			frad	- tool flat radius
**			zmin	- minimum z
**			tol		- tolerance to check
**			slvec	- slice vector
**          clpt1	- the start clpt (x,y,z)
**          clpt2	- the end clpt (x,y,z)
**       OUTPUT :
**			clptlist -clpath list
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_smill_past_clpts(crad,frad,thk,zmin,tol,slvec,clpt1,clpt2,clptlist)
UU_REAL crad,frad,thk,zmin,tol;
UM_vector slvec;
UM_coord clpt1,clpt2;
UU_LIST *clptlist;
{
	int i,npts,npts1,npts2,nums,iret1,iret2;
	UM_coord clps1,ccps1,clpe1,ccpe1;
	UM_clpt *pos,clps,clpe;
	UU_LOGICAL lstart,lend;
	UU_LIST clpoints;

	lstart = UU_FALSE;
	lend = UU_FALSE;

	pos = (UM_clpt *) UU_LIST_ARRAY (clptlist);
	npts = UU_LIST_LENGTH(clptlist);
	if (npts < 2)
		return;

	uu_list_init(&clpoints,sizeof(UM_clpt),100,100);

/*
.....Get end points on the offseted boudnary
*/	
	um_vctovc(pos[0].vta,clps.vta);
	um_vctovc(pos[0].vta,clpe.vta);
	if (um_dist_2d(clpt1,pos[0].pte) < um_dist_2d(clpt2,pos[0].pte) && 
		um_dist_2d(clpt1,pos[0].pte) < 1.5*(crad+frad))
	{	
		lstart = UU_TRUE;		
		um_vctovc(clpt1,clps.pte);

		if (um_dist_2d(clpt2,pos[npts-1].pte) < 1.5*(crad+frad))
		{
			lend = UU_TRUE;
			um_vctovc(clpt2,clpe.pte);
		}
	}
	else if (um_dist_2d(clpt2,pos[0].pte) < um_dist_2d(clpt1,pos[0].pte) &&
			um_dist_2d(clpt2,pos[0].pte) < 1.5*(crad+frad))
	{	
		lstart = UU_TRUE;
		um_vctovc(clpt2,clps.pte);

		if (um_dist_2d(clpt1,pos[npts-1].pte) < 1.5*(crad+frad))
		{
			lend = UU_TRUE;
			um_vctovc(clpt1,clpe.pte);	
		}
	}
	else if (um_dist_2d(clpt1,pos[npts-1].pte) < um_dist_2d(clpt2,pos[npts-1].pte) && 
			um_dist_2d(clpt1,pos[npts-1].pte) < 1.5*(crad+frad))
	{
		lend = UU_TRUE;		
		um_vctovc(clpt1,clpe.pte);

		if (um_dist_2d(clpt2,pos[0].pte) < 1.5*(crad+frad))
		{
			lstart = UU_TRUE;
			um_vctovc(clpt2,clpe.pte);
		}
	}	
	else if (um_dist_2d(clpt2,pos[npts-1].pte) < um_dist_2d(clpt1,pos[npts-1].pte) && 
			um_dist_2d(clpt2,pos[npts-1].pte) < 1.5*(crad+frad))
	{
		lend = UU_TRUE;		
		um_vctovc(clpt2,clpe.pte);

		if (um_dist_2d(clpt1,pos[0].pte) < 1.5*(crad+frad))
		{
			lstart = UU_TRUE;
			um_vctovc(clpt1,clps.pte);
		}
	}
	
	if (lstart)
		um_get_between_point(pos[1].pte,pos[0].pte,UU_FALSE,clps.pte);	
	if (lend)
		um_get_between_point(pos[npts-2].pte,pos[npts-1].pte,UU_FALSE,clpe.pte);
/*
......Check if need to project the extension sections
*/
	if (lstart)
	{
		um_vctovc(clps.pte,clps1);	
		clps1[2]= zmin;	
		iret1 = ncl_projsf_clpoint(crad,frad,clps1,ccps1,thk,tol,
								   UU_FALSE,0,&clpoints,UU_NULL);	
		if (clps1[2] > clps.pte[2]+tol)	
		{		
			ncl_projsf_clpoint_middle(crad,frad,thk,zmin,tol,pos[0].pte,clps1,
									  UU_FALSE,0,&clpoints);					
			nums = UU_LIST_LENGTH(&clpoints);
			if (nums > 0)	
				uu_list_push_list(clptlist, &clpoints);	
		}	
		else	
		{
			uu_list_push(clptlist,&clps);	
		}
	}

	if (lend)
	{
		um_vctovc(clpe.pte,clpe1);
		clpe1[2]= zmin;
		iret2 = ncl_projsf_clpoint(crad,frad,clpe1,ccpe1,thk,tol,UU_FALSE,0,
			&clpoints,UU_NULL);
		if (clpe1[2] > clpe.pte[2]+tol)
		{
			ncl_projsf_clpoint_middle(crad,frad,thk,zmin,tol,pos[npts-1].pte,clpe1,UU_FALSE,0,&clpoints);
			nums = UU_LIST_LENGTH(&clpoints);
			if (nums > 0)	
				uu_list_push_list(clptlist, &clpoints);	
		}
		else
		{
			uu_list_push(clptlist,&clpe);
		}
	}
}
