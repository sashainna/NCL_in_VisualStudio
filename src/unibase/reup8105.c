/****************************************************************
**     NAME       : reupd8105.c
**     CONTAINS   :
**        ur_update_8105 (in_ptr,out_ptr)
**        ur_up_105_sf_fxd (e1,e2)
**        ur_up_105_msf_fxd (e1,e2)
**
**     NOTE: Not used for now (not created).
**        ur_update_8105_varl (eptr,list,vli_ptr,vlo_ptr)
**        ur_get_asize_8105 (rel_num,lst_num,atom_size)
**
**     MODULE NAME AND RELEASE LEVEL
**       reup8105.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:38
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
#include "dtypes.h"
#include "lcom.h"
#include "nclver.h"
#include "rver8105.h"
#include "rver8200.h"

/***************************************************************************
**    E_FUNCTION     :  ur_update_8105 (in_ptr,out_ptr)
**    Update fixed data of entity (Unibase ver < 8.200). 
**    PARAMETERS
**       INPUT  :
**          in_ptr    - Pointer to input structure.
**       OUTPUT :
**          out_ptr   - Pointer to output structure.
****************************************************************************/
int ur_update_8105 (in_ptr,out_ptr)
 struct UR_data *in_ptr, *out_ptr;
 {
  int status;
  status = 0; 
  switch (in_ptr->rel_num)
    {
     case UM_RBSPLCRV_REL:
       status = ur_up_105_rbcv_fxd (in_ptr,out_ptr);
       break;
/*
...Surfaces
*/
     case NCL_SURF_REL:
       status = ur_up_105_sf_fxd (in_ptr,out_ptr);
       break;
     case NCL_MESHSURF_REL:
       status = ur_up_105_msf_fxd (in_ptr,out_ptr);
       break;
     default:
       status = 0;
       break;
    }
  return(status);
 }

/***************************************************************************
**    E_FUNCTION     :  ur_up_105_rbcv_fxd (e1,e2)
**    Update fixed data RBsplane curve entity (Unibase ver < 8.103).
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) RBsplane curve structure.
**       OUTPUT :
**          e2   - pointer to updated output RBsplane curve structure.
**
****************************************************************************/
int ur_up_105_rbcv_fxd (e1,e2)
 struct rbcv8105 *e2;
 struct rbcv8103 *e1;
 {
/*
...Copy unchanged fields of curve
*/
  ur_up_400_ident (e1,e2);
  e2->planar = e1->planar;
  e2->open = e1->open;
  e2->k = e1->k;
  e2->n = e1->n;
  e2->t0 = e1->t0;
  e2->t1 = e1->t1;
  e2->no_t = e1->no_t;
  e2->no_pt = e1->no_pt;
  e2->no_wt = e1->no_wt;
/*
...New/Updated fields
*/
  e2->closdinu = 0;

  return(1);
 }

/***************************************************************************
**    E_FUNCTION     :  ur_up_105_sf_fxd (e1,e2)
**    Update fixed data NCL surface entity (Unibase ver < 8.105). 
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) NCL surface structure.
**       OUTPUT :
**          e2   - pointer to updated output NCL surface structure.
**
****************************************************************************/
int ur_up_105_sf_fxd (e1,e2)
 struct surf8105 *e2;
 struct surf8103 *e1;
 {
/*
...Copy unchanged fields of surface
*/
  ur_up_400_ident (e1,e2);
  e2->surf_type = e1->surf_type;
  e2->numupaths = e1->numupaths;
  e2->numvpaths = e1->numvpaths;
  e2->rldnu = e1->rldnu;
  e2->rldnv = e1->rldnv;
  e2->closdinu = e1->closdinu;
  e2->closdinv = e1->closdinv;
  e2->offset = e1->offset;
  e2->offdist = e1->offdist;
  e2->no_panelkey = e1->no_panelkey;
/*
...New fields 
*/
  e2->ptsperucrv = e1->numvpaths;
  e2->ptspervcrv = e1->numupaths;
  e2->rev_normal = UU_FALSE;
  e2->material = 0;

  return(1);
 }

/***************************************************************************
**    E_FUNCTION     :  ur_up_105_msf_fxd (e1,e2)
**    Update fixed data mesh surface entity (Unibase ver < 8.105). 
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) mesh surface structure.
**       OUTPUT :
**          e2   - pointer to updated output mesh surface structure.
**
****************************************************************************/
int ur_up_105_msf_fxd (e1,e2)
 struct msf8105 *e2;
 struct msf8103 *e1;
 {
/*
...Copy unchanged fields of surface
*/
  ur_up_400_ident (e1,e2);
  e2->surf_type = e1->surf_type;
  e2->m = e1->m;
  e2->n = e1->n;
  e2->numupaths = e1->numupaths;
  e2->numvpaths = e1->numvpaths;
  e2->rldnu = e1->rldnu;
  e2->rldnv = e1->rldnv;
  e2->closdinu = e1->closdinu;
  e2->closdinv = e1->closdinv;
  e2->offset = e1->offset;
  e2->offdist = e1->offdist;
  e2->no_mpatch = e1->no_mpatch;
/*
...New fields 
*/
  e2->ptsperucrv = e1->numvpaths;
  e2->ptspervcrv = e1->numupaths;
  e2->rev_normal = UU_FALSE;
  e2->material = 0;

  return(1);
 }

