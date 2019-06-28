/****************************************************************
**     NAME       : reup9100.c
**     CONTAINS   :
**        ur_update_9100 (in_ptr,out_ptr)
**        ur_up_910_rbsf_fxd (e1,e2)
**        ur_up_910_sf_fxd (e1,e2)
**        ur_up_910_nsf_fxd (e1,e2)
**        ur_up_910_tsf_fxd (e1,e2)
**        ur_up_910_msf_fxd (e1,e2)
**        ur_up_910_qsf_fxd (e1,e2)
**        ur_up_910_esf_fxd (e1,e2)
**        ur_up_910_shape_fxd (e1,e2)
**        ur_up_910_vport_fxd (e1,e2)
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
**        reup9100.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**        04/29/15 , 15:11:39
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
#include "dtypes.h"
#include "lcom.h"
#include "nclver.h"
#include "view.h"
#include "rver9100.h"
#include "rver9200.h"

/***************************************************************************
**    E_FUNCTION     :  ur_update_9100 (in_ptr,out_ptr)
**    Update fixed data of entity (Unibase ver < 9.100). 
**    PARAMETERS
**       INPUT  :
**          in_ptr    - Pointer to input structure.
**       OUTPUT :
**          out_ptr   - Pointer to output structure.
****************************************************************************/
int ur_update_9100 (in_ptr,out_ptr)
struct UR_data *in_ptr, *out_ptr;
{
	int status;
	status = 0; 
	switch (in_ptr->rel_num)
	{
/*
.....Surfaces
*/
		case NCL_SURF_REL:
			status = ur_up_910_sf_fxd (in_ptr,out_ptr);
			break;
		case NCL_NETSF_REL:
			status = ur_up_910_ntsf_fxd (in_ptr,out_ptr);
			break;
		case UM_RBSPLSRF_REL:
			status = ur_up_910_rbsf_fxd (in_ptr,out_ptr);
			break;
		case NCL_TRIMSF_REL:
			status = ur_up_910_tsf_fxd (in_ptr,out_ptr);
			break;
		case NCL_MESHSURF_REL:
			status = ur_up_910_msf_fxd (in_ptr,out_ptr);
			break;
		case NCL_QUILTSURF_REL:
			status = ur_up_910_qsf_fxd (in_ptr,out_ptr);
			break;
		case NCL_EVALSF_REL:
			status = ur_up_910_esf_fxd (in_ptr,out_ptr);
			break;
/*
.....Shape
*/
		case NCL_SHAPE_REL:
			status = ur_up_910_tsf_fxd (in_ptr,out_ptr);
			break;
/*
.....Viewport
*/
		case UV_VPORT_REL:
			status = ur_up_910_vport_fxd (in_ptr,out_ptr);
			break;
		default:
			status = 0;
			break;
	}
	return(status);
}

/***************************************************************************
**    E_FUNCTION     :  ur_up_910_sf_fxd (e1,e2)
**    Update fixed data NCL surface entity.
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) NCL surface structure.
**       OUTPUT :
**          e2   - pointer to updated output NCL surface structure.
**
****************************************************************************/
int ur_up_910_sf_fxd (e1,e2)
struct surf91 *e2;
struct surf90 *e1;
{
/*
...Copy unchanged fields of surface
*/
	ur_up_910_ident (e1,e2);
	e2->surf_type = e1->surf_type;
	e2->rldnu = e1->rldnu;
	e2->rldnv = e1->rldnv;
	e2->closdinu = e1->closdinu;
	e2->closdinv = e1->closdinv;
	e2->offset = e1->offset;
	e2->offdist = e1->offdist;
	e2->no_panelkey = e1->no_panelkey;
	e2->numupaths = e1->numupaths;
	e2->numvpaths = e1->numvpaths;
	e2->ptsperucrv = e1->ptsperucrv;
	e2->ptspervcrv = e1->ptspervcrv;
	e2->rev_normal = e1->rev_normal;
	e2->material = e1->material;
	e2->no_displst = e1->no_displst;
	e2->no_tesslst = e1->no_tesslst;
	e2->no_sskey = e1->no_sskey;
/*
.....Initialize new fields
*/
	e2->no_boxlst = 0;
	e2->no_xyzbylst = 0;
	e2->lucency = 100;
	e2->shaded  = UU_FALSE;

	return(1);
}

/***************************************************************************
**    E_FUNCTION     :  ur_up_910_ntsf_fxd (e1,e2)
**    Update fixed data mesh surface entity (Unibase ver < 8.105). 
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) net surface structure.
**       OUTPUT :
**          e2   - pointer to updated output net surface structure.
**
****************************************************************************/
int ur_up_910_ntsf_fxd (e1,e2)
struct nsf91 *e2;
struct nsf90 *e1;
{
	int i,j;
/*
...Copy unchanged fields of surface
*/
	ur_up_910_ident (e1,e2);
	e2->surf_type = e1->surf_type;
	e2->rldnu = e1->rldnu;
	e2->rldnv = e1->rldnv;
	for (i=0; i<40; i++)
	{
		for (j=0; j<4; j++) e2->bndsfs[i][j] = e1->bndsfs[i][j];
	}
	e2->no_netkey = e1->no_netkey;
	e2->no_sskey = e1->no_sskey;
	e2->no_displst = e1->no_displst;
	e2->no_tesslst = e1->no_tesslst;
/*
.....Initialize new fields
*/
	e2->lucency = 100;
	e2->shaded  = UU_FALSE;

	return(1);
}
/***************************************************************************
**    E_FUNCTION     :  ur_up_910_rbsf_fxd (e1,e2)
**    Update fixed data rbspline surf entity (Unibase ver < 8.500).
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) rbspline surf structure.
**       OUTPUT :
**          e2   - pointer to updated output rbspline surf structure.
**
****************************************************************************/
int
ur_up_910_rbsf_fxd (e1,e2)
struct rbsf90 *e1;
struct rbsf91 *e2;
{
	ur_up_910_ident (e1,e2);
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
	e2->no_tu = e1->no_tu;
	e2->no_tv = e1->no_tv;
	e2->no_pt = e1->no_pt;
	e2->no_wt = e1->no_wt;
	e2->no_displst = e1->no_displst;
	e2->no_tesslst = e1->no_tesslst;
	e2->no_sskey = e1->no_sskey;
/*
.....Initialize new fields
*/
	e2->no_boxlst = 0;
	e2->no_xyzbylst = 0;
	e2->lucency = 100;
	e2->shaded  = UU_FALSE;
	return(1);
}

/***************************************************************************
**    E_FUNCTION     :  ur_up_910_tsf_fxd (e1,e2)
**    Update fixed data trimmed surf entity.
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) trimmed surf structure.
**       OUTPUT :
**          e2   - pointer to updated output trimmed surf structure.
**
****************************************************************************/
int
ur_up_910_tsf_fxd (e1,e2)
struct tsf90 *e1;
struct tsf91 *e2;
{
	ur_up_910_ident (e1,e2);
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
	e2->no_ibndykey = e1->no_ibndykey;
	e2->no_displst = e1->no_displst;
	e2->no_tesslst = e1->no_tesslst;
/*
.....Initialize new fields
*/
	e2->no_boxlst = 0;
	e2->no_xyzbylst = 0;
	e2->no_uvbylst = 0;
	e2->no_uvboxlst = 0;
	e2->lucency = 100;
	e2->shaded  = UU_FALSE;
	return(1);
}

/***************************************************************************
**    E_FUNCTION     :  ur_up_910_msf_fxd (e1,e2)
**    Update fixed data Mesh surf entity.
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) Mesh surf structure.
**       OUTPUT :
**          e2   - pointer to updated output Mesh surf structure.
**
****************************************************************************/
int
ur_up_910_msf_fxd (e1,e2)
struct msf90 *e1;
struct msf91 *e2;
{
	ur_up_910_ident (e1,e2);
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
	e2->no_mpatch = e1->no_mpatch;
	e2->no_displst = e1->no_displst;
	e2->no_tesslst = e1->no_tesslst;
/*
.....Initialize new fields
*/
	e2->no_boxlst = 0;
	e2->no_xyzbylst = 0;
	e2->lucency = 100;
	e2->shaded  = UU_FALSE;
	return(1);
}

/***************************************************************************
**    E_FUNCTION     :  ur_up_910_qsf_fxd (e1,e2)
**    Update fixed data quilt surf entity.
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) quilt surf structure.
**       OUTPUT :
**          e2   - pointer to updated output quilt surf structure.
**
****************************************************************************/
int
ur_up_910_qsf_fxd (e1,e2)
struct qsf90 *e1;
struct qsf91 *e2;
{
	int i;
	ur_up_910_ident (e1,e2);
	e2->surf_type = e1->surf_type;
	e2->numpatches = e1->numpatches;
	e2->rldnu = e1->rldnu;
	e2->rldnv = e1->rldnv;
	e2->offset = e1->offset;
	e2->offdist = e1->offdist;
	for (i=0;i<12;i++)
	{
		e2->midpt[i][0] = e1->midpt[i][0];
		e2->midpt[i][1] = e1->midpt[i][1];
		e2->midpt[i][2] = e1->midpt[i][2];
	}
	e2->no_qpatch = e1->no_qpatch;
	e2->no_displst = e1->no_displst;
	e2->no_tesslst = e1->no_tesslst;
/*
.....Initialize new fields
*/
	e2->no_boxlst = 0;
	e2->no_xyzbylst = 0;
	e2->lucency = 100;
	e2->shaded  = UU_FALSE;
	return(1);
}

/***************************************************************************
**    E_FUNCTION     :  ur_up_910_esf_fxd (e1,e2)
**    Update fixed data evaluated surf entity.
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) evaluated surf structure.
**       OUTPUT :
**          e2   - pointer to updated output evaluated surf structure.
**
****************************************************************************/
int
ur_up_910_esf_fxd (e1,e2)
struct evsf90 *e1;
struct evsf91 *e2;
{
	ur_up_910_ident (e1,e2);
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
	e2->offdist = e1->offdist;
	e2->no_evwd = e1->no_evwd;
	e2->no_displst = e1->no_displst;
	e2->no_tesslst = e1->no_tesslst;
/*
.....Initialize new fields
*/
	e2->lucency = 100;
	e2->shaded  = UU_FALSE;
	return(1);
}

/***************************************************************************
**    E_FUNCTION     :  ur_up_910_shape_fxd (e1,e2)
**    Update fixed data shape entity.
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) trimmed surf structure.
**       OUTPUT :
**          e2   - pointer to updated output trimmed surf structure.
**
****************************************************************************/
int
ur_up_910_shape_fxd (e1,e2)
struct sh90 *e1;
struct sh91 *e2;
{
	ur_up_910_ident (e1,e2);
	e2->no_shapwd = e1->no_shapwd;
	e2->no_displst = e1->no_displst;
/*
.....Initialize new fields
*/
	e2->f2d3d = 1;
	e2->numupaths  = 5;
	e2->numvpaths  = 5;
	e2->ptsperucrv  = 0;
	e2->ptspervcrv  = 0;
	e2->lucency = 100;
	e2->shaded  = UU_FALSE;
	e2->no_tesslst = 0;
	return(1);
}

/***************************************************************************
**    E_FUNCTION     :  ur_up_910_vport_fxd (e1,e2)
**    Update fixed data viewport entity.
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) trimmed surf structure.
**       OUTPUT :
**          e2   - pointer to updated output trimmed surf structure.
**
****************************************************************************/
int
ur_up_910_vport_fxd (e1,e2)
struct vp90 *e1;
struct vp91 *e2;
{
	int i;
	e2->key = e1->key;
	e2->rel_num = e1->rel_num;
	strcpy(e2->name,e1->name);
	for (i=0;i<3;i++)
	{
		e2->llf[i] = e1->llf[i];
		e2->urb[i] = e1->urb[i];
	}
	e2->xform = e1->xform;
	e2->cur_view = e1->cur_view;
	e2->disp_prio = e1->disp_prio;
	e2->input_prio = e1->input_prio;
	e2->disp_all = e1->disp_all;
	e2->bord_seg = e1->bord_seg;
	e2->aperture_on = e1->aperture_on;
	e2->v_axis_on = e1->v_axis_on;
	e2->name_on = e1->name_on;
	e2->bord_on = e1->bord_on;
	e2->nverts = e1->nverts;
	e2->grid_seg = e1->grid_seg;
	e2->grid_on = e1->grid_on;
	e2->motion = e1->motion;
	for (i=0;i<60;i++) e2->vertices[i] = e1->vertices[i];
/*
.....Initialize new fields
*/
	e2->disp_mode = 2;
	e2->wireframe = UU_TRUE;
	return(1);
}

/**************************************************************************
**    E_FUNCTION     :  ur_up_910_ident (e1,e2)
**    Update identity bundle for general entity (Unibase ver < 8.500).
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) identity structure.
**       OUTPUT :
**          e2   - pointer to updated output identity structure.
**
****************************************************************************/
int
ur_up_910_ident (e1,e2)
struct NCL_id_rec *e1, *e2;
{
/*
...copy data
*/
	e2->key = e1->key;
	e2->rel_num = e1->rel_num;
	strcpy (e2->label,e1->label);
	e2->subscr = e1->subscr;
	um_vctovc (e1->labloc,e2->labloc);

	return(1);
}
