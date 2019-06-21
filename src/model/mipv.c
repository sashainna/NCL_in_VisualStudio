/*********************************************************************
**    NAME  :  mipv.c
**       CONTAINS: surface tessellation routines for NCLIPV only
**
**    um_init_tess
**    um_clean_tess
**    um_free_tess
**		um_linear_units_str 
**
**    COPYRIGHT 2010 (c) Numerical Control Computer Sciences Inc.
**                       All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       mipv.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:14
*********************************************************************/

#include "nccs.h"
#include "ulist.h"
#include "uminmax.h"
#include "mgeom.h"
#include "mdeval.h"
#include "nclfc.h"
#include "mdcoord.h"
#include "ngeom.h"
#include "mdunits.h"

typedef struct
{
	UU_LOGICAL inner;
	int n1;
	int n2;
	int n3;
} S_tript;

static UU_REAL UM_TESSELLATION_TOLERANCE = 0.;
static UU_LIST chain,v_list,bps,Strilst;
static struct UM_evsrfout Sevsrf;

void um_clean_tess1();

static int um_pan_cross_bndr1();
static void um_weed_chain();
static int S_fit_chain();
static int um_find_edge();
/*********************************************************************
**    FUNCTION : int um_clean_tess (tess)
**      Sets counters to zero, does not deallocate memory.
**    PARAMETERS
**       INPUT :
**          tess - tessellation
**       OUTPUT :
**          tess - counters reset to zero
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void um_clean_tess (tess)
UM_tessellation *tess;
{
	tess->np = tess->ntri = 0;

	UU_LIST_EMPTY (&tess->vertices);
	UU_LIST_EMPTY (&tess->normals);
	UU_LIST_EMPTY (&tess->tri);
}

/*********************************************************************
**    FUNCTION : void um_init_tess(tess)
**       Initializes a tessellation structure without allocating any
**       memory for the lists.
**    PARAMETERS
**       INPUT :
**          tess - tessellation
**       OUTPUT :
**          initializes lists in the tessellation
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void um_init_tess(tess)
UM_tessellation *tess;
{
	tess->toler = 0.;
	tess->np = tess->ntri = 0;
	uu_list_init0 (&tess->vertices);
	uu_list_init0 (&tess->normals);
	uu_list_init0 (&tess->tri);
}
/*********************************************************************
**    FUNCTION : void um_free_tess (tess)
**
**    PARAMETERS
**       INPUT :
**          tess - tessellation
**       OUTPUT :
**          deallocates lists in the tessellation
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void um_free_tess (tess)
UM_tessellation *tess;
{
	if (tess->np > 0)
	{
		uu_list_free (&tess->vertices);
		uu_list_free (&tess->normals);
	}
	if (tess->ntri > 0)
		uu_list_free (&tess->tri);
	tess->toler = 0.;
	tess->np = tess->ntri = 0;
}

/*********************************************************************
**    FUNCTION : void um_merge_tessel(tess,tmptess)
**         Merges a tessellation list into another tessellation list.
**
**    PARAMETERS
**       INPUT :
**          tess    - Base tessellation list to merge into.
**          tmptess - Tessellation list to merge into 'tess'.
**       OUTPUT :
**          tess    - Updated tessellation list.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void um_merge_tessel(tess,tmptess)
UM_tessellation *tess,*tmptess;
{
	int i;
	UM_tript *tri;
/*
.....Merge tessellation lists
*/
	uu_list_push_list(&tess->vertices,&tmptess->vertices);
	uu_list_push_list(&tess->normals,&tmptess->normals);
	uu_list_push_list(&tess->tri,&tmptess->tri);
/*
.....Update triangle indexes
*/
	if (tess->np != 0)
	{
		tri = (UM_tript *)UU_LIST_ARRAY(&tess->tri);
		for (i=tess->ntri;i<tess->ntri+tmptess->ntri;i++)
		{
			tri[i].n1 += tess->np; tri[i].n2 += tess->np; tri[i].n3 += tess->np;
		}
	}
/*
.....Update counts
*/
	tess->np += tmptess->np;
	tess->ntri += tmptess->ntri;
}

/*********************************************************************
**    FUNCTION : void um_debug_tess(tess)
**         Prints out the triangle list as line segments to 'lp.lis'.
**
**    PARAMETERS
**       INPUT :
**          tess    - Tessellation list to output.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void um_debug_tess(tess)
UM_tessellation *tess;
{
	int i,i1,i2,i3;
	char sbuf[80];
	UM_coord *pts;
	UM_tript *tri;
/*
.....Print out triangles
*/
	tri = (UM_tript *)UU_LIST_ARRAY(&tess->tri);
	pts = (UM_coord *)UU_LIST_ARRAY(&tess->vertices);
	for (i=0;i<tess->ntri;i++)
	{
		sprintf(sbuf,"tri = %d",i);
		NclxDbgPstr(sbuf);
		i1 = tri[i].n1; i2 = tri[i].n2; i3 = tri[i].n3;
		sprintf(sbuf,"LINE/%8.3f,%8.3f,%8.3f, %8.3f,%8.3f,%8.3f",
			pts[i1][0],pts[i1][1],pts[i1][2], pts[i2][0],pts[i2][1],pts[i2][2]);
		NclxDbgPstr(sbuf);
		sprintf(sbuf,"LINE/%8.3f,%8.3f,%8.3f, %8.3f,%8.3f,%8.3f",
			pts[i2][0],pts[i2][1],pts[i2][2], pts[i3][0],pts[i3][1],pts[i3][2]);
		NclxDbgPstr(sbuf);
		sprintf(sbuf,"LINE/%8.3f,%8.3f,%8.3f, %8.3f,%8.3f,%8.3f",
			pts[i3][0],pts[i3][1],pts[i3][2], pts[i1][0],pts[i1][1],pts[i1][2]);
		NclxDbgPstr(sbuf);
	}
}

/*********************************************************************
**    E_FUNCTION     : um_linear_units_str(unit, str)
**       Return the name corresponding to the given linear unit
**			scalar.
**    PARAMETERS   
**       INPUT  : 
**          unit						linear unit scalar
**       OUTPUT :  
**          str						name for unit
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_linear_units_str(unit, str)
	int unit;
	char *str;

	{
	if ((unit < 0) || (unit > 7))
		strcpy(str, "err");
	else
		strcpy(str, UM_linear_units_name[unit]);
	return(UU_SUCCESS);
	}

