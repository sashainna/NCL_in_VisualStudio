/****************************************************************
**     NAME       : reupd8500.c
**     CONTAINS   :
**        ur_update_8500 (in_ptr,out_ptr)
**        ur_up_500_rbsf_fxd (e1,e2)
**        ur_up_500_sf_fxd (e1,e2)
**        ur_up_500_nsf_fxd (e1,e2)
			 ur_up_500_draw_fxd (in_ptr,out_ptr);
**
**     NOTE: Not used for now (not created).
**        ur_update_500_varl (eptr,list,vli_ptr,vlo_ptr)
**        ur_get_asize_500 (rel_num,lst_num,atom_size)
**
**     MODULE NAME AND RELEASE LEVEL
**        reup8500.c , 25.1
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
#include "nccs.h"
#include "mxxx.h"
#include "dtypes.h"
#include "lcom.h"
#include "nclver.h"
#include "rver8500.h"
#include "rver9100.h"

/***************************************************************************
**    E_FUNCTION     :  ur_update_8500 (in_ptr,out_ptr)
**    Update fixed data of entity (Unibase ver < 8.200). 
**    PARAMETERS
**       INPUT  :
**          in_ptr    - Pointer to input structure.
**       OUTPUT :
**          out_ptr   - Pointer to output structure.
****************************************************************************/
int ur_update_8500 (in_ptr,out_ptr)
struct UR_data *in_ptr, *out_ptr;
{
	int status;
	status = 0; 
	switch (in_ptr->rel_num)
	{
/*
...Surfaces
*/
		case NCL_SURF_REL:
			status = ur_up_500_sf_fxd (in_ptr,out_ptr);
			break;
		case NCL_NETSF_REL:
			status = ur_up_500_ntsf_fxd (in_ptr,out_ptr);
			break;
		case UM_RBSPLSRF_REL:
			status = ur_up_500_rbsf_fxd (in_ptr,out_ptr);
			break;
		case UM_DRAWING_REL:
			status = ur_up_500_draw_fxd (in_ptr,out_ptr);
			break;
		default:
			status = 0;
			break;
	}
	return(status);
}

/***************************************************************************
**    E_FUNCTION     :  ur_up_500_sf_fxd (e1,e2)
**    Update fixed data NCL surface entity (Unibase ver < 8.5). 
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) NCL surface structure.
**       OUTPUT :
**          e2   - pointer to updated output NCL surface structure.
**
****************************************************************************/
int ur_up_500_sf_fxd (e1,e2)
struct surf90 *e2;
struct surf84 *e1;
{
/*
...Copy unchanged fields of surface
*/
	ur_up_500_ident (e1,e2);
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
/*
...Still not used but copy anyway
*/
	e2->no_displst = e1->no_displst;
	e2->no_tesslst = e1->no_tesslst;
/*
...New fields 
*/
	e2->no_sskey = 0;
	e2->sskey = UU_NULL;

	return(1);
}

/***************************************************************************
**    E_FUNCTION     :  ur_up_500_ntsf_fxd (e1,e2)
**    Update fixed data mesh surface entity (Unibase ver < 8.105). 
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) net surface structure.
**       OUTPUT :
**          e2   - pointer to updated output net surface structure.
**
****************************************************************************/
int ur_up_500_ntsf_fxd (e1,e2)
struct nsf90 *e2;
struct nsf84 *e1;
{
	int i,j;
/*
...Copy unchanged fields of surface
*/
	ur_up_500_ident (e1,e2);
	e2->surf_type = e1->surf_type;
	e2->rldnu = e1->rldnu;
	e2->rldnv = e1->rldnv;
	e2->no_netkey = e1->no_netkey;
	for (i=0; i<40; i++)
	{
		for (j=0; j<4; j++) e2->bndsfs[i][j] = e1->bndsfs[i][j];
	}
/*
...Still not used but copy anyway
*/
	e2->no_displst = e1->no_displst;
	e2->no_tesslst = e1->no_tesslst;
/*
...New fields 
*/
	e2->no_sskey = 0;
	e2->sskey = UU_NULL;

	return(1);
}
/***************************************************************************
**    E_FUNCTION     :  ur_up_500_rbsf_fxd (e1,e2)
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
ur_up_500_rbsf_fxd (e1,e2)
struct rbsf84 *e1;
struct rbsf90 *e2;
{
	ur_up_500_ident (e1,e2);
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
/*
...New fields
*/
	e2->no_sskey = 0;
	e2->sskey = UU_NULL;
	
	return(1);
}

/***************************************************************************
**    E_FUNCTION     :  ur_up_500_draw_fxd (e1,e2)
**    Update fixed data Drawing entity (Unibase ver < 8.5). 
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) NCL surface structure.
**       OUTPUT :
**          e2   - pointer to updated output NCL surface structure.
**
****************************************************************************/
int ur_up_500_draw_fxd (e1,e2)
struct draw90 *e2;
struct draw90 *e1;
{
	int sz[3],plt,inc;
/*
.....Initialize routine
*/
	sz[0] = 1;
	sz[1] = 0;
	sz[2] = 3;
/*
...Copy unchanged fields of surface
*/
	uu_move_byte(e1,e2,sizeof(struct draw90));
/*
.....Update plotter type/size field
*/
	plt = e1->drwsize/12;
	inc = e1->drwsize - plt;
	e2->drwsize = sz[plt] * 16 + inc;
	return(1);
}

/**************************************************************************
**    E_FUNCTION     :  ur_up_500_ident (e1,e2)
**    Update identity bundle for general entity (Unibase ver < 8.500).
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) identity structure.
**       OUTPUT :
**          e2   - pointer to updated output identity structure.
**
****************************************************************************/
int
ur_up_500_ident (e1,e2)
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
