/****************************************************************
**     NAME       : reup9400.c
**     CONTAINS   :
**        ur_update_9400()
**        ur_up_940_layer_fxd()
**        ur_up_940_light()
**        ur_up_940_ntsf_fxd()
**        ur_up_940_qsf_fxd()
**        ur_up_940_esf_fxd()
**        ur_up_940_shape_fxd()
**        ur_up_940_datast_fxd()
**        ur_up_940_ident()
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
**       reup9400.c , 25.1
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
#define V94MAIN
#include "rver9400.h"
#undef V94MAIN
#include "mrenddl.h"

/***************************************************************************
**    E_FUNCTION     :  ur_update_9400 (in_ptr,out_ptr)
**    Update fixed data of entity (Unibase ver < 9.400). 
**    PARAMETERS
**       INPUT  :
**          in_ptr    - Pointer to input structure.
**       OUTPUT :
**          out_ptr   - Pointer to output structure.
****************************************************************************/
int ur_update_9400 (in_ptr,out_ptr)
struct UR_data *in_ptr, *out_ptr;
{
	int status;
	status = 0; 
	switch (in_ptr->rel_num)
	{
		case UM_LAYER_REL:
			status = ur_up_940_layer_fxd (in_ptr,out_ptr);
			break;
		case UM_LIGHT_REL:
			status = ur_up_940_light (in_ptr,out_ptr);
			break;
		case NCL_NETSF_REL:
			status = ur_up_940_ntsf_fxd (in_ptr,out_ptr);
			break;
		case NCL_QUILTSURF_REL:
			status = ur_up_940_qsf_fxd (in_ptr,out_ptr);
			break;
		case NCL_EVALSF_REL:
			status = ur_up_940_esf_fxd (in_ptr,out_ptr);
			break;
		case NCL_SHAPE_REL:
			status = ur_up_940_shape_fxd (in_ptr,out_ptr);
			break;
		case NCL_DATAST_REL:
			status = ur_up_940_datast_fxd (in_ptr,out_ptr);
			break;
		default:
			status = 0;
			break;
	}
	return(status);
}

/***************************************************************************
**    E_FUNCTION     :  ur_up_940_layer_fxd (e1,e2)
**    Update fixed data NCL surface entity.
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) Layer structure.
**       OUTPUT :
**          e2   - pointer to updated output Layer structure.
**
****************************************************************************/
int ur_up_940_layer_fxd (e1,e2)
struct layer94 *e2;
struct layer93 *e1;
{
int i;
/*
...Copy unchanged fields of layer
*/
	ur_up_940_ident (e1,e2);
	e2->num = e1->num;
	strcpy(e2->name,e1->name);
/*
.....Initialize new fields
*/
	e2->displayable = UU_TRUE;
	e2->selectable = UU_TRUE;
	e2->no_layers = 0;
	return(1);
}

/***************************************************************************
**    E_FUNCTION     :  ur_up_940_light (e1,e2)
**    Update light entity.
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) light structure.
**       OUTPUT :
**          e2   - pointer to updated output light structure.
**
****************************************************************************/
int ur_up_940_light (e1,e2)
struct UM_light_rec94 *e2;
struct UM_light_rec93 *e1;
{
	int i;
	e2->key = e1->key;
	e2->rel_num = e1->rel_num;
	e2->index = e1->index;
	e2->type = e1->type;
	e2->intens = e1->intens;
	e2->position[0] = e1->position[0];
	e2->position[1] = e1->position[1];
	e2->position[2] = e1->position[2];
	e2->direction[0] = e1->direction[0];
	e2->direction[1] = e1->direction[1];
	e2->direction[2] = e1->direction[2];
	e2->attenuation[0] = e1->attenuation[0];
	e2->attenuation[1] = e1->attenuation[1];
	e2->cone_angle = e1->cone_angle;
	e2->scale = e1->scale;
/*
.....Initialize new fields
*/
	e2->attenuation[2] = 1.0;
/*
.....Initialize new fields
*/
	e2->space = 0;
	e2->exp = 0.0;
	e2->ambient[0] = 0.2;
	e2->ambient[1] = 0.2;
	e2->ambient[2] = 0.2;
	e2->ambient[3] = 1.0;
	return(1);
}

/***************************************************************************
**    E_FUNCTION     :  ur_up_940_qsf_fxd (e1,e2)
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
ur_up_940_qsf_fxd (e1,e2)
struct NCL_quiltsf_rec93 *e1;
struct NCL_quiltsf_rec94 *e2;
{
	int i;
	ur_up_940_ident (e1,e2);
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
	e2->no_boxlst = 0;
	e2->no_xyzbylst = 0;
	e2->lucency = 100;
	e2->shaded  = UU_FALSE;
/*
.....Initialize new fields
*/
	e2->material = 0;
	return(1);
}

/***************************************************************************
**    E_FUNCTION     :  ur_up_940_esf_fxd (e1,e2)
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
ur_up_940_esf_fxd (e1,e2)
struct NCL_evalsf_rec93 *e1;
struct NCL_evalsf_rec94 *e2;
{
	ur_up_940_ident (e1,e2);
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
	e2->lucency = 100;
	e2->shaded  = UU_FALSE;
/*
.....Initialize new fields
*/
	e2->material = 0;
	return(1);
}

/***************************************************************************
**    E_FUNCTION     :  ur_up_940_shape_fxd (e1,e2)
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
ur_up_940_shape_fxd (e1,e2)
struct NCL_shape_rec93 *e1;
struct NCL_shape_rec94 *e2;
{
	ur_up_940_ident (e1,e2);
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
/*
.....Initialize new fields
*/
	e2->material = 0;
	return(1);
}
/***************************************************************************
**    E_FUNCTION     :  ur_up_940_ntsf_fxd (e1,e2)
**    Update fixed data mesh surface entity (Unibase ver < 8.105). 
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) net surface structure.
**       OUTPUT :
**          e2   - pointer to updated output net surface structure.
**
****************************************************************************/
int ur_up_940_ntsf_fxd (e1,e2)
struct NCL_netsf_rec94 *e2;
struct NCL_netsf_rec93 *e1;
{
	int i,j;
/*
...Copy unchanged fields of surface
*/
	ur_up_940_ident (e1,e2);
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
	e2->lucency = 100;
	e2->shaded  = UU_FALSE;
/*
.....Initialize new fields
*/
	e2->material = 0;

	return(1);
}
/***************************************************************************
**    E_FUNCTION     :  ur_up_940_datast_fxd (e1,e2)
**    Update data statement label to be @UN so it will not be loaded from a
**    9.3 unibase.
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) data statement structure.
**       OUTPUT :
**          e2   - pointer to updated output data statement structure.
**
****************************************************************************/
int
ur_up_940_datast_fxd (e1,e2)
struct NCL_datast_rec *e1, *e2;
{
	e2->key = e1->key;
	e2->rel_num = e1->rel_num;
	strcpy (e2->label,"@UN");
	e2->subscr    = 0;
	e2->nargs     = e1->nargs;
	e2->no_datael = e1->no_datael;
	e2->datael    = e1->datael;
	return(1);
}
/**************************************************************************
**    E_FUNCTION     :  ur_up_940_ident (e1,e2)
**    Update identity bundle for general entity (Unibase ver < 9.4).
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) identity structure.
**       OUTPUT :
**          e2   - pointer to updated output identity structure.
**
****************************************************************************/
int
ur_up_940_ident (e1,e2)
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
