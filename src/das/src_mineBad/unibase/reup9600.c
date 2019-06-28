/****************************************************************
**     NAME       : reup9600.c
**     CONTAINS   :
**			ur_update_9600 (in_ptr,out_ptr)
**			ur_up_960_line_fxd (in_ptr,out_ptr)
**			ur_up_960_circle_fxd (in_ptr,out_ptr)
**			ur_up_960_conic_fxd (in_ptr,out_ptr)
**			ur_up_960_compcrv_fxd (in_ptr,out_ptr)
**			ur_up_960_bsplcrv_fxd (in_ptr,out_ptr)
**			ur_up_960_rbsplcrv_fxd (in_ptr,out_ptr)
**			ur_up_960_uvcvonsf_fxd (in_ptr,out_ptr)
**			ur_up_960_poly_fxd (in_ptr,out_ptr)
**			ur_up_960_polyline_fxd (in_ptr,out_ptr)
**			ur_up_960_rbsplsrf_fxd (in_ptr,out_ptr)
**			ur_up_960_nclpt_fxd (in_ptr,out_ptr)
**			ur_up_960_nclln_fxd (in_ptr,out_ptr)
**			ur_up_960_nclci_fxd (in_ptr,out_ptr)
**			ur_up_960_vector_fxd (in_ptr,out_ptr)
**			ur_up_960_nclpl_fxd (in_ptr,out_ptr)
**			ur_up_960_matrix_fxd (in_ptr,out_ptr)
**			ur_up_960_curve_fxd (in_ptr,out_ptr)
**			ur_up_960_surface_fxd (in_ptr,out_ptr)
**			ur_up_960_revsurf_fxd (in_ptr,out_ptr)
**			ur_up_960_meshsf_fxd (in_ptr,out_ptr)
**			ur_up_960_quiltsf_fxd (in_ptr,out_ptr)
**			ur_up_960_patern_fxd (in_ptr,out_ptr)
**			ur_up_960_netsf_fxd (in_ptr,out_ptr)
**			ur_up_960_shape_fxd (in_ptr,out_ptr)
**			ur_up_960_evalcv_fxd (in_ptr,out_ptr)
**			ur_up_960_evalsf_fxd (in_ptr,out_ptr)
**			ur_up_960_nclpv_fxd (in_ptr,out_ptr)
**			ur_up_960_trimsf_fxd (in_ptr,out_ptr)
**			ur_update_9600_attr (key,rel_num)
**			ur_up_960_scalar_fxd(in_ptr,out_ptr)
**    COPYRIGHT 2006 (c) NCCS Inc.  All Rights Reserved.
**
**-------------------------  IMPORTANT NOTE  ---------------------
**
**  Be sure to change all standard Unibase structure names to ...
**
**              'xxxver'
**
**  ... in THIS FILE when updating to the next version.  The newly
**  created file should still have the correct structure names
**  (NCL_surface_rec) just in case the structure changes during the
**  release cycle.
**
**  Where:  xxx = symbolic abbreviation for entity, for example,
**                'NCL_surface_rec' becomes 'surf'.
**
**          ver = Version number that THIS FILE was created to update
**                the Unibase TO.  For example '91'.
**
**  This would create the structure name 'surf91'.  These new structure names
**  will be added to the NEW include file created for the next version's update
**  routine, for example 'rver9200.h'.  This new include file will of course
**  have to be added to THIS FILE also.
**
**  The reason for this is that all of the routines used to use the actual
**  structure name (ex. NCL_surface_rec) to update to, even though the
**  structure was changed from release to release.  Needless to say this
**  caused the structure to be updated incorrectly (especially since it would
**  not be updated multiple releases due to the old logic in
**  'ur_update_entity').
**
**-------------------------  IMPORTANT NOTE  ---------------------
**
**     MODULE NAME AND RELEASE LEVEL
**       reup9600.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:40
**
*****************************************************************/

#include "mlabddl.h"
#include "class.h"
#include "udebug.h"
#include "uhep.h"
#include "ribase.h"
#include "ritrnerr.h"
#include "rerrdef.h"
#include "xenv1.h"
#include "nclfc.h"
#include "mfort.h"
#include "mdrel.h"
#include "msrf.h"
#include "mcrv.h"
#include "mxxx.h"
#include "nccs.h"
#include "ncl.h"
#include "dtypes.h"
#include "lcom.h"
#include "nclver.h"
#include "view.h"
#include "rver9600.h"
#include "nclmodals.h"
/***************************************************************************
**    E_FUNCTION     :  ur_update_9600 (in_ptr,out_ptr)
**    Update fixed data of entity (Unibase ver < 9.600). 
**    PARAMETERS
**       INPUT  :
**          in_ptr    - Pointer to input structure.
**       OUTPUT :
**          out_ptr   - Pointer to output structure.
****************************************************************************/
int ur_update_9600 (in_ptr,out_ptr)
struct UR_data *in_ptr, *out_ptr;
{
	int status;
	status = 0; 
	switch (in_ptr->rel_num)
	{
		case UM_POINT_REL:
			status = ur_up_960_point_fxd (in_ptr,out_ptr);
			break;
		case UM_LINE_REL:
			status = ur_up_960_line_fxd (in_ptr,out_ptr);
			break;
		case UM_CIRCLE_REL:
			status = ur_up_960_circle_fxd (in_ptr,out_ptr);
			break;
		case UM_CONIC_REL:
			status = ur_up_960_conic_fxd (in_ptr,out_ptr);
			break;
		case UM_COMPCRV_REL:
			status = ur_up_960_compcrv_fxd (in_ptr,out_ptr);
			break;
		case 6: /*UM_BSPLCRV_REL*/
			status = ur_up_960_bsplcrv_fxd (in_ptr,out_ptr);
			break;
		case UM_RBSPLCRV_REL:
			status = ur_up_960_rbsplcrv_fxd (in_ptr,out_ptr);
			break;
		case UM_UVCVONSF_REL:
			status = ur_up_960_uvcvonsf_fxd (in_ptr,out_ptr);
			break;
		case UM_POLY_REL:
			status = ur_up_960_poly_fxd (in_ptr,out_ptr);
			break;
		case UM_POLYLINE_REL:
			status = ur_up_960_polyline_fxd (in_ptr,out_ptr);
			break;
		case UM_RBSPLSRF_REL:
			status = ur_up_960_rbsplsrf_fxd (in_ptr,out_ptr);
			break;
		case NCL_POINT_REL:
			status = ur_up_960_nclpt_fxd (in_ptr,out_ptr);
			break;
		case NCL_LINE_REL:
			status = ur_up_960_nclln_fxd (in_ptr,out_ptr);
			break;
		case NCL_CIRCLE_REL:
			status = ur_up_960_nclci_fxd (in_ptr,out_ptr);
			break;
		case NCL_VECTOR_REL:
			status = ur_up_960_vector_fxd (in_ptr,out_ptr);
			break;
		case NCL_PLN_REL:
			status = ur_up_960_nclpl_fxd (in_ptr,out_ptr);
			break;
		case NCL_MATRIX_REL:
			status = ur_up_960_matrix_fxd (in_ptr,out_ptr);
			break;
		case NCL_CURVE_REL:
			status = ur_up_960_curve_fxd (in_ptr,out_ptr);
			break;
		case NCL_SURF_REL:
			status = ur_up_960_surface_fxd (in_ptr,out_ptr);
			break;
		case NCL_REVSURF_REL:
			status = ur_up_960_revsurf_fxd (in_ptr,out_ptr);
			break;
		case NCL_MESHSURF_REL:
			status = ur_up_960_meshsf_fxd (in_ptr,out_ptr);
			break;
		case NCL_QUILTSURF_REL:
			status = ur_up_960_quiltsf_fxd (in_ptr,out_ptr);
			break;
		case NCL_PATERN_REL:
			status = ur_up_960_patern_fxd (in_ptr,out_ptr);
			break;
		case NCL_NETSF_REL:
			status = ur_up_960_netsf_fxd (in_ptr,out_ptr);
			break;
		case NCL_SHAPE_REL:
			status = ur_up_960_shape_fxd (in_ptr,out_ptr);
			break;
		case NCL_EVALCV_REL:
			status = ur_up_960_evalcv_fxd (in_ptr,out_ptr);
			break;
		case NCL_EVALSF_REL:
			status = ur_up_960_evalsf_fxd (in_ptr,out_ptr);
			break;
		case NCL_POINTVEC_REL:
			status = ur_up_960_nclpv_fxd (in_ptr,out_ptr);
			break;
		case NCL_TRIMSF_REL:
			status = ur_up_960_trimsf_fxd (in_ptr,out_ptr);
			break;
		case NCL_SCALAR_REL:
			status = ur_up_960_scalar_fxd (in_ptr,out_ptr);
			break;
		default:
			status = 0;
			break;
	}
	return(status);
}
/***************************************************************************
**    E_FUNCTION     :  ur_up_960_point_fxd (e1,e2)
**    Update fixed data  point entity.
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old)  point structure.
**       OUTPUT :
**          e2   - pointer to updated output  point structure.
**
****************************************************************************/
int ur_up_960_point_fxd (e1,e2)
struct UM_point_rec95 *e1;
struct UM_point_rec96 *e2;
{
	int i;
/*
...Copy unchanged fields of surface
*/
	ur_up_960_ident (e1,e2);
	e2->markertype = e1->markertype;
	e2->snap_node = e1->snap_node;
	for (i = 0; i < 3; i++) e2->pt[i] = e1->pt[i];
	e2->no_displst = e1->no_displst;
	
	return(1);
}

/***************************************************************************
**    E_FUNCTION     :  ur_up_960_line_fxd (e1,e2)
**    Update fixed data line entity.
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old)  line structure.
**       OUTPUT :
**          e2   - pointer to updated output line structure.
**
****************************************************************************/
int ur_up_960_line_fxd (e1,e2)
struct UM_line_rec95 *e1;
struct UM_line_rec96 *e2;
{
	int i;
/*
...Copy unchanged fields of surface
*/
	ur_up_960_ident (e1,e2);
	for (i = 0; i < 3; i++) 
	{
		e2->spt[i] = e1->spt[i];
		e2->ept[i] = e1->ept[i];
	}
	e2->no_displst = e1->no_displst;
	
	return(1);
}

/***************************************************************************
**    E_FUNCTION     :  ur_up_960_circle_fxd (e1,e2)
**    Update fixed data  circle entity.
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old)  circle structure.
**       OUTPUT :
**          e2   - pointer to updated output circle structure.
**
****************************************************************************/
int ur_up_960_circle_fxd (e1,e2)
struct UM_circle_rec95 *e1;
struct UM_circle_rec96 *e2;
{
	int i;
/*
...Copy unchanged fields of surface
*/
	ur_up_960_ident (e1,e2);
	e2->radius = e1->radius;
	e2->dang = e1->dang;
	for (i = 0; i < 3; i++) 
	{
		e2->center[i] = e1->center[i];
		e2->svec[i] = e1->svec[i];
		e2->nvec[i] = e1->nvec[i];
	}
	e2->no_displst = e1->no_displst;
	
	return(1);
}

/***************************************************************************
**    E_FUNCTION     :  ur_up_960_conic_fxd (e1,e2)
**    Update fixed data  conic entity.
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old)  conic structure.
**       OUTPUT :
**          e2   - pointer to updated output  conic structure.
**
****************************************************************************/
int ur_up_960_conic_fxd (e1,e2)
struct UM_conic_rec95 *e1;
struct UM_conic_rec96 *e2;
{
	int i,j;
/*
...Copy unchanged fields of surface
*/
	ur_up_960_ident (e1,e2);
	e2->type = e1->type;
	for (j = 0; j < 2; j++)
		e2->invariants[j] = e1->invariants[j];
	for (i = 0; i < 4; i++) 
		for (j = 0; j < 3; j++)
			e2->tfmat[i][j] = e1->tfmat[i][j];
	e2->t0 = e1->t0;
	e2->t1 = e1->t1;
	e2->no_displst = e1->no_displst;
	
	return(1);
}

/***************************************************************************
**    E_FUNCTION     :  ur_up_960_compcrv_fxd (e1,e2)
**    Update fixed data  compcrv entity.
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old)  compcrv structure.
**       OUTPUT :
**          e2   - pointer to updated output  compcrv structure.
**
****************************************************************************/
int ur_up_960_compcrv_fxd (e1,e2)
struct UM_compcrv_rec95 *e1;
struct UM_compcrv_rec96 *e2;
{
/*
...Copy unchanged fields of surface
*/
	ur_up_960_ident (e1,e2);
	e2->closdinu = e1->closdinu;
	e2->arclen = e1->arclen;
	e2->planar = e1->planar;
	e2->open = e1->open;
	e2->continuity = e1->continuity;
	e2->fcolor = e1->fcolor;
	e2->no_cid = e1->no_cid;
	e2->no_displst = e1->no_displst;
	
	return(1);
}

/***************************************************************************
**    E_FUNCTION     :  ur_up_960_bsplcrv_fxd (e1,e2)
**    Update fixed data  bsplcrv entity.
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old)  bsplcrv structure.
**       OUTPUT :
**          e2   - pointer to updated output bsplcrv structure.
**
****************************************************************************/
int ur_up_960_bsplcrv_fxd (e1,e2)
struct UM_bsplcrv_rec95 *e1;
struct UM_bsplcrv_rec96 *e2;
{
/*
...Copy unchanged fields of surface
*/
	ur_up_960_ident (e1,e2);
	e2->inverted = e1->inverted;
	e2->planar = e1->planar;
	e2->open = e1->open;
	e2->k = e1->k;
	e2->n = e1->n;
	e2->t0 = e1->t0;
	e2->t1 = e1->t1;
	e2->no_pt = e1->no_pt;
	e2->no_displst = e1->no_displst;
	
	return(1);
}

/***************************************************************************
**    E_FUNCTION     :  ur_up_960_rbsplcrv_fxd (e1,e2)
**    Update fixed data  rbsplcrv entity.
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old)  rbsplcrv structure.
**       OUTPUT :
**          e2   - pointer to updated output  rbsplcrv structure.
**
****************************************************************************/
int ur_up_960_rbsplcrv_fxd (e1,e2)
struct UM_rbsplcrv_rec95 *e1;
struct UM_rbsplcrv_rec96 *e2;
{
	
/*
...Copy unchanged fields of surface
*/
	ur_up_960_ident (e1,e2);
	e2->planar = e1->planar;
	e2->open = e1->open;
	e2->closdinu = e1->closdinu;
	e2->k = e1->k;
	e2->n = e1->n;
	e2->t0 = e1->t0;
	e2->t1 = e1->t1;
	e2->no_t = e1->no_t;
	e2->no_pt = e1->no_pt;
	e2->no_wt = e1->no_wt;
	e2->no_displst = e1->no_displst;
	
	return(1);
}

/***************************************************************************
**    E_FUNCTION     :  ur_up_960_uvcvonsf_fxd (e1,e2)
**    Update fixed data  uvcvonsf entity.
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old)  uvcvonsf structure.
**       OUTPUT :
**          e2   - pointer to updated output  uvcvonsf structure.
**
****************************************************************************/
int ur_up_960_uvcvonsf_fxd (e1,e2)
struct UM_uvcvonsf_rec95 *e1;
struct UM_uvcvonsf_rec96 *e2;
{
	
/*
...Copy unchanged fields of surface
*/
	ur_up_960_ident (e1,e2);
	e2->bskey = e1->bskey;
	e2->dummy = e1->dummy;
	e2->planar = e1->planar;
	e2->open = e1->open;
	e2->closdinu = e1->closdinu;
	e2->k = e1->k;
	e2->n = e1->n;
	e2->t0 = e1->t0;
	e2->t1 = e1->t1;
	e2->no_t = e1->no_t;
	e2->no_pt = e1->no_pt;
	e2->no_wt = e1->no_wt;
	e2->no_displst = e1->no_displst;
	
	return(1);
}

/***************************************************************************
**    E_FUNCTION     :  ur_up_960_poly_fxd (e1,e2)
**    Update fixed data  poly entity.
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old)  poly structure.
**       OUTPUT :
**          e2   - pointer to updated output  poly structure.
**
****************************************************************************/
int ur_up_960_poly_fxd (e1,e2)
struct UM_poly_rec95 *e1;
struct UM_poly_rec96 *e2;
{
	int i,j;
/*
...Copy unchanged fields of surface
*/
	ur_up_960_ident (e1,e2);
	e2->fcolor = e1->fcolor;
	e2->numvtx = e1->numvtx;
	for (i = 0; i < 200; i++) 
		for (j = 0; j < 3; j++) 
			e2->vertex[i][j] = e1->vertex[i][j];
	e2->no_displst = e1->no_displst;
	
	return(1);
}

/***************************************************************************
**    E_FUNCTION     :  ur_up_960_polyline_fxd (e1,e2)
**    Update fixed data  polyline entity.
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old)  polyline structure.
**       OUTPUT :
**          e2   - pointer to updated output  polyline structure.
**
****************************************************************************/
int ur_up_960_polyline_fxd (e1,e2)
struct UM_polyline_rec95 *e1;
struct UM_polyline_rec96 *e2;
{

/*
...Copy unchanged fields of surface
*/
	ur_up_960_ident (e1,e2);
	e2->dummy = e1->dummy;
	e2->no_pt = e1->no_pt;
	e2->no_displst = e1->no_displst;
	
	return(1);
}

/***************************************************************************
**    E_FUNCTION     :  ur_up_960_rbsplsrf_fxd (e1,e2)
**    Update fixed data  rbsplsrf entity.
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old)  rbsplsrf structure.
**       OUTPUT :
**          e2   - pointer to updated output  rbsplsrf structure.
**
****************************************************************************/
int ur_up_960_rbsplsrf_fxd (e1,e2)
struct UM_rbsplsrf_rec95 *e1;
struct UM_rbsplsrf_rec96 *e2;
{	int i;
/*
...Copy unchanged fields of surface
*/
	ur_up_960_ident (e1,e2);
	e2->material = e1->material;
	e2->numupaths = e1->numupaths;
	e2->numvpaths = e1->numvpaths;
	e2->ptsperucrv = e1->ptsperucrv;
	e2->ptspervcrv = e1->ptspervcrv;
	e2->rldnu = e1->rldnu;
	e2->rldnv = e1->rldnv;
	e2->rev_normal = e1->rev_normal;
	e2->closdinu = e1->closdinu;
	e2->closdinv = e1->closdinv;
	e2->offset = e1->offset;
	e2->offdist = e1->offdist;
	e2->ku = e1->ku;
	e2->kv = e1->kv;
	e2->nu = e1->nu;
	e2->nv = e1->nv;
	e2->dum1 = e1->dum1;
	e2->primitive = e1->primitive;
	for (i = 0; i < 16; i++) e2->prim_param[i] = e1->prim_param[i];
	e2->shaded  = e1->shaded;
	e2->lucency = e1->lucency;
	e2->no_tu = e1->no_tu;
	e2->no_tv = e1->no_tv;
	e2->no_pt = e1->no_pt;
	e2->no_wt = e1->no_wt;
	e2->no_sskey = e1->no_sskey;
	e2->no_displst = e1->no_displst;
	e2->no_tesslst = e1->no_tesslst;
	e2->no_boxlst = e1->no_boxlst;
	e2->no_xyzbylst = e1->no_xyzbylst;

	return(1);
}

/***************************************************************************
**    E_FUNCTION     :  ur_up_960_nclpt_fxd (e1,e2)
**    Update fixed data NCL point entity.
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) NCL point structure.
**       OUTPUT :
**          e2   - pointer to updated output NCL point structure.
**
****************************************************************************/
int ur_up_960_nclpt_fxd (e1,e2)
struct NCL_nclpt_rec95 *e1;
struct NCL_nclpt_rec96 *e2;
{
	int i;
/*
...Copy unchanged fields of surface
*/
	ur_up_960_ident (e1,e2);
	e2->markertype = e1->markertype;
	e2->snap_node = e1->snap_node;
	for (i = 0; i < 3; i++) e2->pt[i] = e1->pt[i];
	e2->no_displst = e1->no_displst;
	
	return(1);
}
/***************************************************************************
**    E_FUNCTION     :  ur_up_960_nclln_fxd (e1,e2)
**    Update fixed data NCL line entity.
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) NCL line structure.
**       OUTPUT :
**          e2   - pointer to updated output NCL line structure.
**
****************************************************************************/
int ur_up_960_nclln_fxd (e1,e2)
struct NCL_nclln_rec95 *e1;
struct NCL_nclln_rec96 *e2;
{
	int i;
/*
...Copy unchanged fields of surface
*/
	ur_up_960_ident (e1,e2);
	for (i = 0; i < 3; i++) 
	{
		e2->spt[i] = e1->spt[i];
		e2->ept[i] = e1->ept[i];
	}
	e2->no_displst = e1->no_displst;
	
	return(1);
}
/***************************************************************************
**    E_FUNCTION     :  ur_up_960_nclci_fxd (e1,e2)
**    Update fixed data  NCL circle entity.
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old)  NCL circle structure.
**       OUTPUT :
**          e2   - pointer to updated output NCL circle structure.
**
****************************************************************************/
int ur_up_960_nclci_fxd (e1,e2)
struct NCL_nclci_rec95 *e1;
struct NCL_nclci_rec96 *e2;
{
	int i;
/*
...Copy unchanged fields of surface
*/
	ur_up_960_ident (e1,e2);
	e2->radius = e1->radius;
	e2->dang = e1->dang;
	for (i = 0; i < 3; i++) 
	{
		e2->center[i] = e1->center[i];
		e2->svec[i] = e1->svec[i];
		e2->nvec[i] = e1->nvec[i];
	}
	e2->no_displst = e1->no_displst;
	
	return(1);
}

/***************************************************************************
**    E_FUNCTION     :  ur_up_960_vector_fxd (e1,e2)
**    Update fixed data  NCL vectore entity.
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old)  NCL vector structure.
**       OUTPUT :
**          e2   - pointer to updated output NCL vector structure.
**
****************************************************************************/
int ur_up_960_vector_fxd (e1,e2)
struct NCL_vector_rec95 *e1;
struct NCL_vector_rec96 *e2;
{
	int i;
/*
...Copy unchanged fields of surface
*/
	ur_up_960_ident (e1,e2);
	for (i = 0; i < 3; i++) 
		e2->vec[i] = e1->vec[i];
	e2->no_displst = e1->no_displst;
	
	return(1);
}

/***************************************************************************
**    E_FUNCTION     :  ur_up_960_nclpl_fxd (e1,e2)
**    Update fixed data  NCL plane entity.
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old)  NCL plane structure.
**       OUTPUT :
**          e2   - pointer to updated output NCL plane structure.
**
****************************************************************************/
int ur_up_960_nclpl_fxd (e1,e2)
struct NCL_nclpl_rec95 *e1;
struct NCL_nclpl_rec96 *e2;
{
	int i;
/*
...Copy unchanged fields of surface
*/
	ur_up_960_ident (e1,e2);
	e2->radius = e1->radius;
	for (i = 0; i < 3; i++) 
	{
		e2->pt[i] = e1->pt[i];
		e2->nvec[i] = e1->nvec[i];
	}
	e2->no_displst = e1->no_displst;
	
	return(1);
}

/***************************************************************************
**    E_FUNCTION     :  ur_up_960_curve_fxd (e1,e2)
**    Update fixed data  NCL curve entity.
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old)  NCL curve structure.
**       OUTPUT :
**          e2   - pointer to updated output NCL curve structure.
**
****************************************************************************/
int ur_up_960_curve_fxd (e1,e2)
struct NCL_curve_rec95 *e1;
struct NCL_curve_rec96 *e2;
{
/*
...Copy unchanged fields of surface
*/
	ur_up_960_ident (e1,e2);
	e2->closdinu = e1->closdinu;
	e2->t0 = e1->t0;
	e2->t1 = e1->t1;
	e2->t_end = e1->t_end;
	e2->no_param= e1->no_param;
	e2->no_segment = e1->no_segment;
	e2->no_displst = e1->no_displst;
	
	return(1);
}

/***************************************************************************
**    E_FUNCTION     :  ur_up_960_matrix_fxd (e1,e2)
**    Update fixed data  NCL matrix entity.
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old)  NCL matrix structure.
**       OUTPUT :
**          e2   - pointer to updated output NCL matrix structure.
**
****************************************************************************/
int ur_up_960_matrix_fxd (e1,e2)
struct NCL_matrix_rec95 *e1;
struct NCL_matrix_rec96 *e2;
{
	int i,j;
/*
...Copy unchanged fields of surface
*/
	ur_up_960_ident (e1,e2);
	e2->dalen = e1->dalen;
	for (i = 0; i < 3; i++) 
	{
		e2->dbox[i] = e1->dbox[i];
		for (j = 0; j < 4; j++) 
			e2->mat[i][j] = e1->mat[i][j];
	}
	e2->no_displst = e1->no_displst;
	
	return(1);
}

/***************************************************************************
**    E_FUNCTION     :  ur_up_960_surface_fxd (e1,e2)
**    Update fixed data NCL surface entity.
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) NCL surface structure.
**       OUTPUT :
**          e2   - pointer to updated output NCL surface structure.
**
****************************************************************************/
int ur_up_960_surface_fxd (e1,e2)
struct NCL_surface_rec95 *e1;
struct NCL_surface_rec96 *e2;
{	int i;
/*
...Copy unchanged fields of surface
*/
	ur_up_960_ident (e1,e2);
	e2->material = e1->material;
	e2->numupaths = e1->numupaths;
	e2->numvpaths = e1->numvpaths;
	e2->ptsperucrv = e1->ptsperucrv;
	e2->ptspervcrv = e1->ptspervcrv;
	e2->rldnu = e1->rldnu;
	e2->rldnv = e1->rldnv;
	e2->rev_normal = e1->rev_normal;
	e2->closdinu = e1->closdinu;
	e2->closdinv = e1->closdinv;
	e2->offset = e1->offset;
	e2->offdist = e1->offdist;
	e2->surf_type = e1->surf_type;
	e2->dum1 = e1->dum1;	
	e2->primitive = e1->primitive;
	for (i = 0; i < 16; i++) e2->prim_param[i] = e1->prim_param[i];
	e2->shaded  = e1->shaded;
	e2->lucency = e1->lucency;
	e2->no_panelkey = e1->no_panelkey;
	e2->no_sskey = e1->no_sskey;
	e2->no_displst = e1->no_displst;
	e2->no_tesslst = e1->no_tesslst;
	e2->no_boxlst = e1->no_boxlst;
	e2->no_xyzbylst = e1->no_xyzbylst;

	return(1);
}

/***************************************************************************
**    E_FUNCTION     :  ur_up_960_revsurf_fxd (e1,e2)
**    Update fixed data NCL revsurf entity.
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) NCL revsurf structure.
**       OUTPUT :
**          e2   - pointer to updated output NCL revsurf structure.
**
****************************************************************************/
int ur_up_960_revsurf_fxd (e1,e2)
struct NCL_revsurf_rec95 *e1;
struct NCL_revsurf_rec96 *e2;
{	int i;
/*
...Copy unchanged fields of surface
*/
	ur_up_960_ident (e1,e2);
	e2->material = e1->material;
	e2->numupaths = e1->numupaths;
	e2->numvpaths = e1->numvpaths;
	e2->ptsperucrv = e1->ptsperucrv;
	e2->ptspervcrv = e1->ptspervcrv;
	e2->closdinu = e1->closdinu;
	e2->closdinv = e1->closdinv;
	e2->offdist = e1->offdist;
	e2->surf_type = e1->surf_type;
	e2->primitive = e1->primitive;
	for (i = 0; i < 16; i++) e2->prim_param[i] = e1->prim_param[i];
	e2->shaded  = e1->shaded;
	e2->lucency = e1->lucency;
	e2->rev_normal = e1->rev_normal;
	e2->cvkey = e1->cvkey;
	for (i = 0; i < 3; i++)
	{
		e2->pta[i] = e1->pta[i]; e2->vca[i] = e1->vca[i];
	}
	e2->sa = e1->sa;
	e2->ta = e1->ta;
	e2->no_sskey = e1->no_sskey;
	e2->no_displst = e1->no_displst;
	e2->no_tesslst = e1->no_tesslst;
	e2->no_boxlst = e1->no_boxlst;
	e2->no_xyzbylst = e1->no_xyzbylst;

	return(1);
}

/***************************************************************************
**    E_FUNCTION     :  ur_up_960_meshsf_fxd (e1,e2)
**    Update fixed data NCL meshsf entity.
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) NCL meshsf structure.
**       OUTPUT :
**          e2   - pointer to updated output NCL meshsf structure.
**
****************************************************************************/
int ur_up_960_meshsf_fxd (e1,e2)
struct NCL_meshsf_rec95 *e1;
struct NCL_meshsf_rec96 *e2;
{	
/*
...Copy unchanged fields of surface
*/
	ur_up_960_ident (e1,e2);
	e2->material = e1->material;
	e2->numupaths = e1->numupaths;
	e2->numvpaths = e1->numvpaths;
	e2->ptsperucrv = e1->ptsperucrv;
	e2->ptspervcrv = e1->ptspervcrv;
	e2->rldnu = e1->rldnu;
	e2->rldnv = e1->rldnv;
	e2->rev_normal = e1->rev_normal;
	e2->closdinu = e1->closdinu;
	e2->closdinv = e1->closdinv;
	e2->offset = e1->offset;
	e2->offdist = e1->offdist;
	e2->surf_type = e1->surf_type;
	e2->m = e1->m;
	e2->n = e1->n;
	e2->shaded  = e1->shaded;
	e2->lucency = e1->lucency;
	e2->no_mpatch = e1->no_mpatch;
	e2->no_displst = e1->no_displst;
	e2->no_tesslst = e1->no_tesslst;
	e2->no_boxlst = e1->no_boxlst;
	e2->no_xyzbylst = e1->no_xyzbylst;

	return(1);
}

/***************************************************************************
**    E_FUNCTION     :  ur_up_960_quiltsf_fxd (e1,e2)
**    Update fixed data NCL quiltsf entity.
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) NCL quiltsf structure.
**       OUTPUT :
**          e2   - pointer to updated output NCL quiltsf structure.
**
****************************************************************************/
int ur_up_960_quiltsf_fxd (e1,e2)
struct NCL_quiltsf_rec95 *e1;
struct NCL_quiltsf_rec96 *e2;
{	int i,j;
/*
...Copy unchanged fields of surface
*/
	ur_up_960_ident (e1,e2);
	e2->material = e1->material;
	e2->surf_type = e1->surf_type;
	e2->numpatches = e1->numpatches;
	e2->rldnu = e1->rldnu;
	e2->rldnv = e1->rldnv;
	e2->offset = e1->offset;
	e2->offdist = e1->offdist;
	for (i = 0; i < 12; i++)
		for (j = 0; j < 3; j++)
			e2->midpt[i][j] = e1->midpt[i][j];
	e2->shaded  = e1->shaded;
	e2->lucency = e1->lucency;
	e2->dummy = e1->dummy;
	e2->no_qpatch = e1->no_qpatch;
	e2->no_displst = e1->no_displst;
	e2->no_tesslst = e1->no_tesslst;
	e2->no_boxlst = e1->no_boxlst;
	e2->no_xyzbylst = e1->no_xyzbylst;

	return(1);
}

/***************************************************************************
**    E_FUNCTION     :  ur_up_960_patern_fxd (e1,e2)
**    Update fixed data NCL patern entity.
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) NCL patern structure.
**       OUTPUT :
**          e2   - pointer to updated output NCL patern structure.
**
****************************************************************************/
int ur_up_960_patern_fxd (e1,e2)
struct NCL_patern_rec95 *e1;
struct NCL_patern_rec96 *e2;
{	
/*
...Copy unchanged fields of surface
*/
	ur_up_960_ident (e1,e2);
	e2->markertype = e1->markertype;
	e2->pntype = e1->pntype;
	e2->dummy = e1->dummy;
	e2->no_patpnt = e1->no_patpnt;
	e2->no_displst = e1->no_displst;
	
	return(1);
}

/***************************************************************************
**    E_FUNCTION     :  ur_up_960_netsf_fxd (e1,e2)
**    Update fixed data NCL netsf entity.
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) NCL netsf structure.
**       OUTPUT :
**          e2   - pointer to updated output NCL netsf structure.
**
****************************************************************************/
int ur_up_960_netsf_fxd (e1,e2)
struct NCL_netsf_rec95 *e1;
struct NCL_netsf_rec96 *e2;
{	int i,j;
/*
...Copy unchanged fields of surface
*/
	ur_up_960_ident (e1,e2);
	e2->material = e1->material;
	e2->surf_type = e1->surf_type;
	e2->rldnu = e1->rldnu;
	e2->rldnv = e1->rldnv;
	for (i = 0; i < 40; i++)
		for (j = 0; j < 4; j++)
			e2->bndsfs[i][j] = e1->bndsfs[i][j];
	e2->shaded  = e1->shaded;
	e2->lucency = e1->lucency;
	e2->no_netkey = e1->no_netkey;
	e2->no_sskey = e1->no_sskey;
	e2->no_displst = e1->no_displst;
	e2->no_tesslst = e1->no_tesslst;

	return(1);
}

/***************************************************************************
**    E_FUNCTION     :  ur_up_960_shape_fxd (e1,e2)
**    Update fixed data NCL shape entity.
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) NCL shape structure.
**       OUTPUT :
**          e2   - pointer to updated output NCL shape structure.
**
****************************************************************************/
int ur_up_960_shape_fxd (e1,e2)
struct NCL_shape_rec95 *e1;
struct NCL_shape_rec96 *e2;
{	
/*
...Copy unchanged fields of surface
*/
	ur_up_960_ident (e1,e2);
	e2->material = e1->material;
	e2->f2d3d = e1->f2d3d;
	e2->numupaths = e1->numupaths;
	e2->numvpaths = e1->numvpaths;
	e2->ptsperucrv = e1->ptsperucrv;
	e2->ptspervcrv = e1->ptspervcrv;
	e2->shaded  = e1->shaded;
	e2->lucency = e1->lucency;
	e2->dummy = e1->dummy;
	e2->no_shapwd = e1->no_shapwd;
	e2->no_displst = e1->no_displst;
	e2->no_tesslst = e1->no_tesslst;

	return(1);
}

/***************************************************************************
**    E_FUNCTION     :  ur_up_960_evalcv_fxd (e1,e2)
**    Update fixed data NCL evalcv entity.
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) NCL evalcv structure.
**       OUTPUT :
**          e2   - pointer to updated output NCL evalcv structure.
**
****************************************************************************/
int ur_up_960_evalcv_fxd (e1,e2)
struct NCL_evalcv_rec95 *e1;
struct NCL_evalcv_rec96 *e2;
{	
/*
...Copy unchanged fields of surface
*/
	ur_up_960_ident (e1,e2);
	e2->curve_type = e1->curve_type;
	e2->no_evwd = e1->no_evwd;
	e2->no_displst = e1->no_displst;

	return(1);
}

/***************************************************************************
**    E_FUNCTION     :  ur_up_960_evalsf_fxd (e1,e2)
**    Update fixed data NCL evalsf entity.
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) NCL evalsf structure.
**       OUTPUT :
**          e2   - pointer to updated output NCL evalsf structure.
**
****************************************************************************/
int ur_up_960_evalsf_fxd (e1,e2)
struct NCL_evalsf_rec95 *e1;
struct NCL_evalsf_rec96 *e2;
{	
/*
...Copy unchanged fields of surface
*/
	ur_up_960_ident (e1,e2);
	e2->material = e1->material;
	e2->numupaths = e1->numupaths;
	e2->numvpaths = e1->numvpaths;
	e2->ptsperucrv = e1->ptsperucrv;
	e2->ptspervcrv = e1->ptspervcrv;
	e2->rldnu = e1->rldnu;
	e2->rldnv = e1->rldnv;
	e2->closdinu = e1->closdinu;
	e2->closdinv = e1->closdinv;
	e2->surf_type = e1->surf_type;
	e2->offset = e1->offset;
	e2->shaded  = e1->shaded;
	e2->lucency = e1->lucency;
	e2->offdist = e1->offdist;
	e2->no_evwd = e1->no_evwd;
	e2->no_displst = e1->no_displst;
	e2->no_tesslst = e1->no_tesslst;
	
	return(1);
}

/***************************************************************************
**    E_FUNCTION     :  ur_up_960_nclpv_fxd (e1,e2)
**    Update fixed data NCL nclpv entity.
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) NCL nclpv structure.
**       OUTPUT :
**          e2   - pointer to updated output NCL nclpv structure.
**
****************************************************************************/
int ur_up_960_nclpv_fxd (e1,e2)
struct NCL_nclpv_rec95 *e1;
struct NCL_nclpv_rec96 *e2;
{	int i;
/*
...Copy unchanged fields of surface
*/
	ur_up_960_ident (e1,e2);
	for(i=0;i<3;i++)
	{
		e2->pt[i] = e1->pt[i];
		e2->ve[i] = e1->ve[i];
	}
	e2->no_displst = e1->no_displst;
		
	return(1);
}

/***************************************************************************
**    E_FUNCTION     :  ur_up_960_trimsf_fxd (e1,e2)
**    Update fixed data NCL trimsfentity.
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) NCL trimsf structure.
**       OUTPUT :
**          e2   - pointer to updated output NCL trimsf structure.
**
****************************************************************************/
int ur_up_960_trimsf_fxd (e1,e2)
struct NCL_trimsf_rec95 *e1;
struct NCL_trimsf_rec96 *e2;
{	
/*
...Copy unchanged fields of surface
*/
	ur_up_960_ident (e1,e2);
	e2->material = e1->material;
	e2->numupaths = e1->numupaths;
	e2->numvpaths = e1->numvpaths;
	e2->ptsperucrv = e1->ptsperucrv;
	e2->ptspervcrv = e1->ptspervcrv;
	e2->rev_normal = e1->rev_normal;
	e2->closdinu = e1->closdinu;
	e2->closdinv = e1->closdinv;
	e2->offdist = e1->offdist;
	e2->uv_key = e1->uv_key;
	e2->cv_key = e1->cv_key;
	e2->bs_key = e1->bs_key;
	e2->ub_min = e1->ub_min;
	e2->ub_max = e1->ub_max;
	e2->vb_min = e1->vb_min;
	e2->vb_max = e1->vb_max;
	e2->u_min = e1->u_min;
	e2->u_max = e1->u_max;
	e2->v_min = e1->v_min;
	e2->v_max = e1->v_max;
	e2->drive_type = e1->drive_type;
	e2->shaded  = e1->shaded;
	e2->lucency = e1->lucency;
	e2->no_ibndykey = e1->no_ibndykey;
	e2->no_displst = e1->no_displst;
	e2->no_tesslst = e1->no_tesslst;
	e2->no_boxlst = e1->no_boxlst;
	e2->no_xyzbylst = e1->no_xyzbylst;
	e2->no_uvbylst = e1->no_uvbylst;
	e2->no_uvboxlst = e1->no_uvboxlst;

	return(1);
}


/**************************************************************************
**    E_FUNCTION     :  ur_up_960_ident (e1,e2)
**    Update identity bundle for general entity (Unibase ver < 9.600).
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) identity structure.
**       OUTPUT :
**          e2   - pointer to updated output identity structure.
**
****************************************************************************/
int
 ur_up_960_ident (e1,e2)
 struct id95 *e1;
 struct id96 *e2;
  {
/*
.....copy unchanged fields
*/   
	e2->key = e1->key;
	e2->rel_num = e1->rel_num;
	strcpy (e2->label,e1->label);
	e2->subscr = e1->subscr;
	um_vctovc (e1->labloc,e2->labloc);

/*
.....Initialize new fields
*/
	e2->ldrloc[0] = e2->ldrloc[1] = e2->ldrloc[2] = 0.; 

   return(1); 
  }

/***************************************************************************
**    E_FUNCTION     :  ur_update_9600_attr (key)
**    Update attribute bundle (Unibase ver < 9.600).
**    PARAMETERS
**       INPUT  :
**          key    - Entity key.
**       OUTPUT :
**          none
**
****************************************************************************/
int ur_update_9600_attr (key,rel_num)
UU_KEY_ID key;
int rel_num;
{
	int status,temp;
	struct NCL_nclattr_rec attr;

	switch (rel_num)
	{
		case UM_POINT_REL:
		case UM_LINE_REL:
		case UM_CIRCLE_REL:
		case UM_CONIC_REL:
		case UM_COMPCRV_REL:
		case 6: /*UM_BSPLCRV_REL*/
		case UM_RBSPLCRV_REL:
		case UM_UVCVONSF_REL:
		case UM_POLY_REL:
		case UM_POLYLINE_REL:
		case UM_RBSPLSRF_REL:
		case NCL_POINT_REL:
		case NCL_LINE_REL:
		case NCL_CIRCLE_REL:
		case NCL_VECTOR_REL:
		case NCL_PLN_REL:
		case NCL_MATRIX_REL:
		case NCL_CURVE_REL:
		case NCL_SURF_REL:
		case NCL_REVSURF_REL:
		case NCL_MESHSURF_REL:
		case NCL_QUILTSURF_REL:
		case NCL_PATERN_REL:
		case NCL_NETSF_REL:
		case NCL_SHAPE_REL:
		case NCL_EVALCV_REL:
		case NCL_EVALSF_REL:
		case NCL_POINTVEC_REL:
		case NCL_TRIMSF_REL:
			attr.key = key;
			temp =0;
			status = ur_retrieve_attr(&attr);
			if(attr.label_on == -1)
				attr.label_on = temp | NCL_ALTER_BIT;
			if(attr.label_on == 2)
			{
				temp = temp | NCL_ALTER_BIT;
				attr.label_on = temp | NCL_LABEL_BIT;
			}
			ur_update_attr(&attr);
			break;
		default:
			status = 0;
			break;
	}
 
	uu_dexit;
	return(status);
}
/***************************************************************************
**    E_FUNCTION     :  ur_up_960_scalar_fxd (e1,e2)
**    Update fixed data NCL surface entity.
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) Layer structure.
**       OUTPUT :
**          e2   - pointer to updated output Layer structure.
**
****************************************************************************/
int ur_up_960_scalar_fxd (e1,e2)
struct NCL_scalar_rec96 *e2;
struct NCL_scalar_rec95 *e1;
{
/*
...copy data
*/
	e2->key = e1->key;
	e2->rel_num = e1->rel_num;
	strcpy (e2->label,e1->label);
	e2->subscr = e1->subscr;
	e2->scalar_value = e1->scalar_value;
/*
.....initial descript to label name
*/
	e2->descript[0] = '\0';
	strcpy(e2->classnm, "Default"); 
	return(1);
}
