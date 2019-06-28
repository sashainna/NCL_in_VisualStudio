/****************************************************************
**     NAME       : reup9200.c
**     CONTAINS   :
**        ur_update_9200 (in_ptr,out_ptr)
**        ur_up_920_rbsf_fxd (e1,e2)
**        ur_up_920_sf_fxd (e1,e2)
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
**        reup9200.c , 25.1
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
#include "ncl.h"
#include "dtypes.h"
#include "lcom.h"
#include "nclver.h"
#include "view.h"
#include "rver9200.h"

/***************************************************************************
**    E_FUNCTION     :  ur_update_9200 (in_ptr,out_ptr)
**    Update fixed data of entity (Unibase ver < 9.200). 
**    PARAMETERS
**       INPUT  :
**          in_ptr    - Pointer to input structure.
**       OUTPUT :
**          out_ptr   - Pointer to output structure.
****************************************************************************/
int ur_update_9200 (in_ptr,out_ptr)
struct UR_data *in_ptr, *out_ptr;
{
	int status;
	status = 0; 
	switch (in_ptr->rel_num)
	{
		case NCL_SURF_REL:
			status = ur_up_920_sf_fxd (in_ptr,out_ptr);
			break;
		case UM_RBSPLSRF_REL:
			status = ur_up_920_rbsf_fxd (in_ptr,out_ptr);
			break;
		default:
			status = 0;
			break;
	}
	return(status);
}

/***************************************************************************
**    E_FUNCTION     :  ur_up_920_sf_fxd (e1,e2)
**    Update fixed data NCL surface entity.
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) NCL surface structure.
**       OUTPUT :
**          e2   - pointer to updated output NCL surface structure.
**
****************************************************************************/
int ur_up_920_sf_fxd (e1,e2)
struct surf92 *e2;
struct surf91 *e1;
{
/*
...Copy unchanged fields of surface
*/
	ur_up_920_ident (e1,e2);
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
	e2->no_boxlst = e1->no_boxlst;
	e2->no_xyzbylst = e1->no_xyzbylst;
	e2->lucency = e1->lucency;
	e2->shaded  = e1->shaded;
/*
.....Initialize new fields
*/
        e2->primitive = NCLSF_FREEFORM;
	return(1);
}

/***************************************************************************
**    E_FUNCTION     :  ur_up_920_rbsf_fxd (e1,e2)
**    Update fixed data rbspline surf entity (Unibase ver < 9.200).
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) rbspline surf structure.
**       OUTPUT :
**          e2   - pointer to updated output rbspline surf structure.
**
****************************************************************************/
int
ur_up_920_rbsf_fxd (e1,e2)
struct rbsf91 *e1;
struct rbsf92 *e2;
{
	ur_up_920_ident (e1,e2);
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
	e2->no_boxlst = e1->no_boxlst;
	e2->no_xyzbylst = e1->no_xyzbylst;
	e2->lucency = e1->lucency;
	e2->shaded  = e1->shaded;
/*
.....Initialize new fields
*/
        e2->primitive = NCLSF_FREEFORM;
	return(1);
}

/**************************************************************************
**    E_FUNCTION     :  ur_up_920_ident (e1,e2)
**    Update identity bundle for general entity (Unibase ver < 9.200).
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) identity structure.
**       OUTPUT :
**          e2   - pointer to updated output identity structure.
**
****************************************************************************/
int
ur_up_920_ident (e1,e2)
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

