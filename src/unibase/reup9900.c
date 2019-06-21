/****************************************************************
**     NAME       : reup9900.c
**     CONTAINS   :
**        ur_update_9900()
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
**                the Unibase TO.  For example '94'.
**
**  This would create the structure name '93'.  These new structure names
**  will be added to the NEW include file created for the next version's update
**  routine, for example 'rver9400.h'.  This new include file will of course
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
**       reup9900.c , 25.1
**     DATE AND TIME OF LAST MODIFICATION
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
#include "mdrel.h"
#include "atext.h"
#include "bsym.h"

#include "msrf.h"
#include "nccs.h"
#include "ncl.h"
#include "mdcoord.h"

#include "dtypes.h"
#include "nclver.h"
#define V99MAIN
#include "rver9900.h"
#undef V99MAIN
#include "rver10000.h"

/***************************************************************************
**    E_FUNCTION     :  ur_update_9900 (in_ptr,out_ptr)
**    Update fixed data of entity (Unibase ver < 9.900). 
**    PARAMETERS
**       INPUT  :
**          in_ptr    - Pointer to input structure.
**       OUTPUT :
**          out_ptr   - Pointer to output structure.
****************************************************************************/
int ur_update_9900 (in_ptr,out_ptr)
struct UR_data *in_ptr, *out_ptr;
{
	int status;
	status = 0; 
	switch (in_ptr->rel_num)
	{
		case UM_RBSPLSRF_REL:
			status = ur_up_990_rbsplsrf_fxd (in_ptr,out_ptr);
			break;
		case NCL_SURF_REL:
			status = ur_up_990_surface_fxd (in_ptr,out_ptr);
			break;
		case NCL_MESHSURF_REL:
			status = ur_up_990_meshsf_fxd (in_ptr,out_ptr);
			break;
		case NCL_REVSURF_REL:
			status = ur_up_990_revsurf_fxd (in_ptr,out_ptr);
			break;
		case NCL_NETSF_REL:
			status = ur_up_990_netsf_fxd (in_ptr,out_ptr);
			break;
		case NCL_TRIMSF_REL:
			status = ur_up_990_trimsf_fxd (in_ptr,out_ptr);
			break;
		case NCL_SHAPE_REL:
			status = ur_up_990_shape_fxd (in_ptr,out_ptr);
			break;
		default:
			status = 0;
			break;
	}
	return(status);
}

/**************************************************************************
**    E_FUNCTION     :  ur_up_990_header (e1,e2)
**    copy surface header.
****************************************************************************/
void ur_up_990_header (e1,e2)
struct NCL_sfhead98 *e1;
struct NCL_sfhead98 *e2;
{
	e2->key = e1->key;
	e2->rel_num = e1->rel_num;
	strcpy (e2->label,e1->label);
	e2->subscr = e1->subscr;
	um_vctovc (e1->labloc,e2->labloc);
	um_vctovc (e1->ldrloc,e2->ldrloc);
}

/***************************************************************************
**    E_FUNCTION     :  ur_up_990_rbsplsrf_fxd (e1,e2)
**    Update fixed data rbsplsrf entity.
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old)  rbsplsrf structure.
**       OUTPUT :
**          e2   - pointer to updated output  rbsplsrf structure.
**
****************************************************************************/
int ur_up_990_rbsplsrf_fxd (e1,e2)
struct UM_rbsplsrf_rec98 *e1;
struct UM_rbsplsrf_rec99 *e2;
{
	int i;
/*
...Copy unchanged fields of surface
*/
	ur_up_990_header (e1,e2);
	e2->rldnu = e1->rldnu;
	e2->swapuv = 0;
	e2->rev_normal = e1->rev_normal;
	e2->closdinu = e1->closdinu;
	e2->closdinv = e1->closdinv;
	e2->offdist = e1->offdist;
	e2->ku = e1->ku;
	e2->kv = e1->kv;
	e2->nu = e1->nu;
	e2->nv = e1->nv;
	e2->primitive = e1->primitive;
	for (i = 0; i < 16; i++) e2->prim_param[i] = e1->prim_param[i];
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
**    E_FUNCTION     :  ur_up_990_surface_fxd (e1,e2)
**    Update fixed data NCL surface entity.
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) NCL surface structure.
**       OUTPUT :
**          e2   - pointer to updated output NCL surface structure.
**
****************************************************************************/
int ur_up_990_surface_fxd (e1,e2)
struct NCL_surface_rec98 *e1;
struct NCL_surface_rec99 *e2;
{	
	int i;
/*
...Copy unchanged fields of surface
*/
	ur_up_990_header (e1,e2);
	e2->rldnu = e1->rldnu;
	e2->swapuv = 0;
	e2->rev_normal = e1->rev_normal;
	e2->closdinu = e1->closdinu;
	e2->closdinv = e1->closdinv;
	e2->offset = e1->offset;
	e2->offdist = e1->offdist;
	e2->surf_type = e1->surf_type;
	e2->primitive = e1->primitive;
	for (i = 0; i < 16; i++) e2->prim_param[i] = e1->prim_param[i];
	e2->no_panelkey = e1->no_panelkey;
	e2->no_sskey = e1->no_sskey;
	e2->no_displst = e1->no_displst;
	e2->no_tesslst = e1->no_tesslst;
	e2->no_boxlst = e1->no_boxlst;
	e2->no_xyzbylst = e1->no_xyzbylst;

	return(1);
}

/***************************************************************************
**    E_FUNCTION     :  ur_up_990_revsurf_fxd (e1,e2)
**    Update fixed data NCL revsurf entity.
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) NCL revsurf structure.
**       OUTPUT :
**          e2   - pointer to updated output NCL revsurf structure.
**
****************************************************************************/
int ur_up_990_revsurf_fxd (e1,e2)
struct NCL_revsurf_rec98 *e1;
struct NCL_revsurf_rec99 *e2;
{	
	int i;
/*
...Copy unchanged fields of surface
*/
	ur_up_990_header (e1,e2);
	e2->rldnu = -1;
	e2->swapuv = 0;
	e2->rev_normal = e1->rev_normal;
	e2->closdinu = e1->closdinu;
	e2->closdinv = e1->closdinv;
	e2->offdist = e1->offdist;
	e2->primitive = e1->primitive;
	for (i = 0; i < 16; i++) e2->prim_param[i] = e1->prim_param[i];
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
**    E_FUNCTION     :  ur_up_990_meshsf_fxd (e1,e2)
**    Update fixed data NCL meshsf entity.
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) NCL meshsf structure.
**       OUTPUT :
**          e2   - pointer to updated output NCL meshsf structure.
**
****************************************************************************/
int ur_up_990_meshsf_fxd (e1,e2)
struct NCL_meshsf_rec98 *e1;
struct NCL_meshsf_rec99 *e2;
{	
/*
...Copy unchanged fields of surface
*/
	ur_up_990_header (e1,e2);
	e2->rldnu = e1->rldnu;
	e2->swapuv = 0;
	e2->rev_normal = e1->rev_normal;
	e2->closdinu = e1->closdinu;
	e2->closdinv = e1->closdinv;
	e2->offset = e1->offset;
	e2->offdist = e1->offdist;
	e2->surf_type = e1->surf_type;
	e2->m = e1->m;
	e2->n = e1->n;
	e2->no_mpatch = e1->no_mpatch;
	e2->no_displst = e1->no_displst;
	e2->no_tesslst = e1->no_tesslst;
	e2->no_boxlst = e1->no_boxlst;
	e2->no_xyzbylst = e1->no_xyzbylst;

	return(1);
}

/***************************************************************************
**    E_FUNCTION     :  ur_up_990_netsf_fxd (e1,e2)
**    Update fixed data NCL netsf entity.
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) NCL netsf structure.
**       OUTPUT :
**          e2   - pointer to updated output NCL netsf structure.
**
****************************************************************************/
int ur_up_990_netsf_fxd (e1,e2)
struct NCL_netsf_rec98 *e1;
struct NCL_netsf_rec99 *e2;
{	
	int i,j;
/*
...Copy unchanged fields of surface
*/
	ur_up_990_header (e1,e2);
	e2->surf_type = e1->surf_type;
	for (i = 0; i < 40; i++)
		for (j = 0; j < 4; j++)
			e2->bndsfs[i][j] = e1->bndsfs[i][j];
	e2->no_netkey = e1->no_netkey;
	e2->no_sskey = e1->no_sskey;
	e2->no_displst = e1->no_displst;
	e2->no_tesslst = e1->no_tesslst;

	return(1);
}

/***************************************************************************
**    E_FUNCTION     :  ur_up_990_trimsf_fxd (e1,e2)
**    Update fixed data NCL trimsfentity.
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) NCL trimsf structure.
**       OUTPUT :
**          e2   - pointer to updated output NCL trimsf structure.
**
****************************************************************************/
int ur_up_990_trimsf_fxd (e1,e2)
struct NCL_trimsf_rec98 *e1;
struct NCL_trimsf_rec99 *e2;
{	
/*
...Copy unchanged fields of surface
*/
	ur_up_990_header (e1,e2);
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
	e2->no_ibndykey = e1->no_ibndykey;
	e2->no_displst = e1->no_displst;
	e2->no_tesslst = e1->no_tesslst;
	e2->no_boxlst = e1->no_boxlst;
	e2->no_xyzbylst = e1->no_xyzbylst;
	e2->no_uvbylst = e1->no_uvbylst;
	e2->no_uvboxlst = e1->no_uvboxlst;

	return(1);
}

/***************************************************************************
**    E_FUNCTION     :  ur_up_990_shape_fxd (e1,e2)
**    Update fixed data NCL shape entity.
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) NCL shape structure.
**       OUTPUT :
**          e2   - pointer to updated output NCL shape structure.
**
****************************************************************************/
int ur_up_990_shape_fxd (e1,e2)
struct NCL_shape_rec98 *e1;
struct NCL_shape_rec99 *e2;
{	
/*
...Copy unchanged fields of surface
*/
	ur_up_990_header (e1,e2);
	e2->f2d3d = e1->f2d3d;
	e2->dummy = e1->dummy;
	e2->no_shapwd = e1->no_shapwd;
	e2->no_displst = e1->no_displst;
	e2->no_tesslst = e1->no_tesslst;

	return(1);
}
