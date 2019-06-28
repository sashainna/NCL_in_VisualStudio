/*****************************************************************
**     NAME       : reupd8103.c
**     CONTAINS   :
**        ur_update_8103 (in_ptr,out_ptr)
**        ur_up_103_agcv_fxd (e1,e2)
**        ur_up_103_agsf_fxd (e1,e2)
**        ur_up_103_ccv_fxd (e1,e2)
**        ur_up_103_cv_fxd (e1,e2)
**        ur_up_103_evsf_fxd (e1,e2)
**        ur_up_103_msf_fxd (e1,e2)
**        ur_up_103_sf_fxd (e1,e2)
**
**     NOTE: Not used for now (not created).
**        ur_update_8103_varl (eptr,list,vli_ptr,vlo_ptr)
**        ur_get_asize_8103 (rel_num,lst_num,atom_size)
**
**    MODULE NAME AND RELEASE LEVEL
**       reup8103.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:38
**
******************************************************************/

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
#include "mcrv.h"
#include "msrf.h"
#include "nccs.h"
#include "dtypes.h"
#include "lcom.h"
#include "nclver.h"
#include "rver8103.h"
#include "rver8105.h"

/***************************************************************************
**    E_FUNCTION     :  ur_update_8103 (in_ptr,out_ptr)
**    Update fixed data of entity (Unibase ver < 8.200). 
**    PARAMETERS
**       INPUT  :
**          in_ptr    - Pointer to input structure.
**       OUTPUT :
**          out_ptr   - Pointer to output structure.
****************************************************************************/
int ur_update_8103 (in_ptr,out_ptr)
 struct UR_data *in_ptr, *out_ptr;
 {
  int status;
  status = 0; 
  switch (in_ptr->rel_num)
    {
/*
...Curves
*/
     case NCL_CURVE_REL:
       status = ur_up_103_cv_fxd (in_ptr,out_ptr);
       break;
     case UM_COMPCRV_REL:
       status = ur_up_103_ccv_fxd (in_ptr,out_ptr);
       break;
     case UM_AGCRV_REL:
       status = ur_up_103_agcv_fxd (in_ptr,out_ptr);
       break;
/*
...Surfaces
*/
     case NCL_SURF_REL:
       status = ur_up_103_sf_fxd (in_ptr,out_ptr);
       break;
     case NCL_MESHSURF_REL:
       status = ur_up_103_msf_fxd (in_ptr,out_ptr);
       break;
     case NCL_EVALSF_REL:
       status = ur_up_103_evsf_fxd (in_ptr,out_ptr);
       break;
     case UM_AGSRF_REL:
       status = ur_up_103_agsf_fxd (in_ptr,out_ptr);
       break;
     default:
       status = 0;
       break;
    }
  return(status);
 }

/***************************************************************************
**    E_FUNCTION     :  ur_up_103_cv_fxd (e1,e2)
**    Update fixed data NCL curve entity (Unibase ver < 8.103). 
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) NCL curve structure.
**       OUTPUT :
**          e2   - pointer to updated output NCL curve structure.
**
****************************************************************************/
int ur_up_103_cv_fxd (e1,e2)
 struct curve8103 *e2;
 struct curve80 *e1;
 {
/*
...Copy unchanged fields of curve
*/
  ur_up_400_ident (e1,e2);
  e2->no_param = e1->no_param;
  e2->no_segment = e1->no_segment;
/*
...New/Updated fields 
*/
  e2->closdinu = 0;
  return(1);
 }

/***************************************************************************
**    E_FUNCTION     :  ur_up_103_ccv_fxd (e1,e2)
**    Update fixed data composite curve entity (Unibase ver < 8.103). 
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) composite curve structure.
**       OUTPUT :
**          e2   - pointer to updated output composite curve structure.
**
****************************************************************************/
int ur_up_103_ccv_fxd (e1,e2)
 struct ccv8103 *e2;
 struct ccv80 *e1;
 {
/*
...Copy unchanged fields of curve
*/
  ur_up_400_ident (e1,e2);
  e2->arclen = e1->arclen;
  e2->planar = e1->planar;
  e2->open = e1->open;
  e2->continuity = e1->continuity;
  e2->fcolor = e1->fcolor;
  e2->no_cid = e1->no_cid;
/*
...New/Updated fields 
*/
  e2->closdinu = 0;

  return(1);
 }

/***************************************************************************
**    E_FUNCTION     :  ur_up_103_agcv_fxd (e1,e2)
**    Update fixed data AG curve entity (Unibase ver < 8.103). 
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) AG curve structure.
**       OUTPUT :
**          e2   - pointer to updated output AG curve structure.
**
****************************************************************************/
int ur_up_103_agcv_fxd (e1,e2)
 struct agcv8103 *e2;
 struct agcv80 *e1;
 {
/*
...Copy unchanged fields of curve
*/
  e2->key = e1->key;
  e2->rel_num = e1->rel_num;
  strcpy (e2->label,e1->label);
  e2->subscr = e1->subscr; 
  e2->crvaddr = e1->crvaddr;
/*
...New/Updated fields 
*/
  e2->closdinu = 0;

  return(1);
 }

/***************************************************************************
**    E_FUNCTION     :  ur_up_103_sf_fxd (e1,e2)
**    Update fixed data NCL surface entity (Unibase ver < 8.103). 
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) NCL surface structure.
**       OUTPUT :
**          e2   - pointer to updated output NCL surface structure.
**
****************************************************************************/
int ur_up_103_sf_fxd (e1,e2)
 struct surf8103 *e2;
 struct surf80 *e1;
 {
/*
...Copy unchanged fields of curve
*/
  ur_up_400_ident (e1,e2);
  e2->surf_type = e1->surf_type;
  e2->numupaths = e1->numupaths;
  e2->numvpaths = e1->numvpaths;
  e2->rldnu = e1->rldnu;
  e2->rldnv = e1->rldnv;
  e2->offset = e1->offset;
  e2->offdist = e1->offdist;
  e2->no_panelkey = e1->no_panelkey;
/*
...New fields 
...and display parameters instant update to 8400 version 
*/
  e2->ptsperucrv = e1->numvpaths;
  e2->ptspervcrv = e1->numupaths;
  e2->closdinu = 0;
  e2->closdinv = 0;

  return(1);
 }

/***************************************************************************
**    E_FUNCTION     :  ur_up_103_msf_fxd (e1,e2)
**    Update fixed data mesh surface entity (Unibase ver < 8.103). 
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) mesh surface structure.
**       OUTPUT :
**          e2   - pointer to updated output mesh surface structure.
**
****************************************************************************/
int ur_up_103_msf_fxd (e1,e2)
 struct msf8103 *e2;
 struct msf80 *e1;
 {
/*
...Copy unchanged fields of curve
*/
  ur_up_400_ident (e1,e2);
  e2->surf_type = e1->surf_type;
  e2->m = e1->m;
  e2->n = e1->n;
  e2->numupaths = e1->numupaths;
  e2->numvpaths = e1->numvpaths;
  e2->rldnu = e1->rldnu;
  e2->rldnv = e1->rldnv;
  e2->offset = e1->offset;
  e2->offdist = e1->offdist;
  e2->no_mpatch = e1->no_mpatch;
/*
...New fields,
...and display parameters instant update to 8251 version 
*/
  e2->ptsperucrv = e1->numvpaths;
  e2->ptspervcrv = e1->numupaths;
  e2->closdinu = 0;
  e2->closdinv = 0;

  return(1);
 }

/***************************************************************************
**    E_FUNCTION     :  ur_up_103_evsf_fxd (e1,e2)
**    Update fixed data evaluated surface entity (Unibase ver < 8.103). 
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) evaluated surface structure.
**       OUTPUT :
**          e2   - pointer to updated output evaluated surface structure.
**
****************************************************************************/
int ur_up_103_evsf_fxd (e1,e2)
 struct evsf8103 *e2;
 struct evsf80 *e1;
 {
/*
...Copy unchanged fields of curve
*/
  ur_up_400_ident (e1,e2);
  e2->surf_type = e1->surf_type;
  e2->numupaths = e1->numupaths;
  e2->numvpaths = e1->numvpaths;
  e2->rldnu = e1->rldnu;
  e2->rldnv = e1->rldnv;
  e2->offset = e1->offset;
  e2->offdist = e1->offdist;
  e2->no_evwd = e1->no_evwd;
/*
...New fields 
...and display parameters instant update to 8251 version 
*/
  e2->ptsperucrv = e1->numvpaths;
  e2->ptspervcrv = e1->numupaths;
  e2->closdinu = 0;
  e2->closdinv = 0;

  return(1);
 }

/***************************************************************************
**    E_FUNCTION     :  ur_up_103_agsf_fxd (e1,e2)
**    Update fixed data AG surface entity (Unibase ver < 8.103). 
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) AG surface structure.
**       OUTPUT :
**          e2   - pointer to updated output AG surface structure.
**
****************************************************************************/
int ur_up_103_agsf_fxd (e1,e2)
 struct agsf8103 *e2;
 struct agsf80 *e1;
 {
/*
...Copy unchanged fields of curve
*/
  e2->key = e1->key;
  e2->rel_num = e1->rel_num;
  strcpy (e2->label,e1->label);
  e2->subscr = e1->subscr;
  e2->material = e1->material;
  e2->numupaths = e1->numupaths;
  e2->numvpaths = e1->numvpaths;
  e2->ptsperucrv = e1->ptsperucrv;
  e2->ptspervcrv = e1->ptspervcrv;
  e2->rldnu = e1->rldnu;
  e2->rldnv = e1->rldnv;
  e2->rev_normal = e1->rev_normal;
  e2->srfaddr = e1->srfaddr;
/*
...New fields 
*/
  e2->closdinu = 0;
  e2->closdinv = 0;

  return(1);
 }

