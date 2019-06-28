/****************************************************************
**     NAME       : reup9300.c
**     CONTAINS   :
**        ur_update_9300 (in_ptr,out_ptr)
**        ur_up_930_revsf_fxd (e1,e2)
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
**       reup9300.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:39
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
#include "rver9300.h"

/***************************************************************************
**    E_FUNCTION     :  ur_update_9300 (in_ptr,out_ptr)
**    Update fixed data of entity (Unibase ver < 9.300). 
**    PARAMETERS
**       INPUT  :
**          in_ptr    - Pointer to input structure.
**       OUTPUT :
**          out_ptr   - Pointer to output structure.
****************************************************************************/
int ur_update_9300 (in_ptr,out_ptr)
struct UR_data *in_ptr, *out_ptr;
{
	int status;
	status = 0; 
	switch (in_ptr->rel_num)
	{
		case NCL_REVSURF_REL:
			status = ur_up_930_revsf_fxd (in_ptr,out_ptr);
			break;
		default:
			status = 0;
			break;
	}
	return(status);
}

/***************************************************************************
**    E_FUNCTION     :  ur_up_930_revsf_fxd (e1,e2)
**    Update fixed data NCL surface entity.
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) NCL surface structure.
**       OUTPUT :
**          e2   - pointer to updated output NCL surface structure.
**
****************************************************************************/
int ur_up_930_revsf_fxd (e1,e2)
struct revsurf93 *e2;
struct revsurf92 *e1;
{
	int i;
/*
...Copy unchanged fields of surface
*/
	ur_up_930_ident (e1,e2);
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
/*
.....Initialize new fields
*/
	e2->no_xyzbylst = 0;
	return(1);
}

/**************************************************************************
**    E_FUNCTION     :  ur_up_930_ident (e1,e2)
**    Update identity bundle for general entity (Unibase ver < 9.300).
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) identity structure.
**       OUTPUT :
**          e2   - pointer to updated output identity structure.
**
****************************************************************************/
int
ur_up_930_ident (e1,e2)
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
