/*************************************************************
**     NAME       : reup8400.c
**     CONTAINS   :
**        ur_update_8400 (in_ptr,out_ptr)
**        ur_update_8400_attr (key,rel_num)
**        ur_up_400_mx (e1,e2)
**        ur_up_400_mx_attr (key)
**        ur_up_400_pt (in_ptr,out_ptr)
**        ur_up_400_ln (in_ptr,out_ptr)
**        ur_up_400_ci (in_ptr,out_ptr)
**        ur_up_400_vc (in_ptr,out_ptr)
**        ur_up_400_pl (in_ptr,out_ptr)
**        ur_up_400_cv_fxd (in_ptr,out_ptr)
**        ur_up_400_mx (in_ptr,out_ptr)
**        ur_up_400_evcv_fxd (in_ptr,out_ptr)
**        ur_up_400_evsf_fxd (in_ptr,out_ptr)
**        ur_up_400_trsf_fxd (in_ptr,out_ptr)
**        ur_up_400_mssf_fxd (in_ptr,out_ptr)
**        ur_up_400_qlsf_fxd (in_ptr,out_ptr)
**        ur_up_400_ntsf_fxd (in_ptr,out_ptr)
**        ur_up_400_pv (in_ptr,out_ptr)
**        ur_up_400_sc (in_ptr,out_ptr)
**        ur_up_400_sp_fxd (in_ptr,out_ptr)
**        ur_up_400_pn_fxd (in_ptr,out_ptr)
**        ur_up_400_cn (in_ptr,out_ptr)
**        ur_up_400_ccv_fxd (in_ptr,out_ptr)
**        ur_up_400_bcv_fxd (in_ptr,out_ptr)
**        ur_up_400_rbcv_fxd (in_ptr,out_ptr)
**        ur_up_400_rbsf_fxd (in_ptr,out_ptr)
**        ur_up_400_pol (in_ptr,out_ptr)
**        ur_up_400_poll_fxd (in_ptr,out_ptr)
**
**     NOTE: Not used for now (not created).
**        ur_update_8400_varl (eptr,list,vli_ptr,vlo_ptr)
**        ur_get_asize_8400 (rel_num,lst_num,atom_size)
**
**    MODULE NAME AND RELEASE LEVEL
**       reup8400.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:11:39
**
****************************************************************/

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
#include "mdattr.h"
#include "nccs.h"
#include "msrf.h"
#include "mcrv.h"
#include "dtypes.h"
#include "lcom.h"
#include "nclver.h"
#include "rver8400.h"
#include "rver8500.h"

/**********************************************************************
**    E_FUNCTION     :  ur_update_8400 (in_ptr,in_ptr)
**    Update fixed data of entity (Unibase ver < 8.400).
**    PARAMETERS
**       INPUT  :
**          in_ptr    - Pointer to input structure.
**       OUTPUT :
**          out_ptr   - Pointer to output structure.
**
***********************************************************************/
int
 ur_update_8400 (in_ptr,out_ptr)
 struct UR_data *in_ptr, *out_ptr;
 {
  int status;
 
  switch (in_ptr->rel_num)
    {
     case UM_POINT_REL:
     case NCL_POINT_REL:
       status = ur_up_400_pt (in_ptr,out_ptr);         
       break;
     case UM_LINE_REL:
     case NCL_LINE_REL:
       status = ur_up_400_ln (in_ptr,out_ptr);         
       break;
     case UM_CIRCLE_REL:
     case NCL_CIRCLE_REL:
       status = ur_up_400_ci (in_ptr,out_ptr);         
       break;
     case NCL_VECTOR_REL:
       status = ur_up_400_vc (in_ptr,out_ptr);         
       break;
     case NCL_PLN_REL:
       status = ur_up_400_pl (in_ptr,out_ptr);         
       break;
     case NCL_CURVE_REL:
       status = ur_up_400_cv_fxd (in_ptr,out_ptr);     
       break;
     case NCL_MATRIX_REL:
       status = ur_up_400_mx (in_ptr,out_ptr);         
       break;
     case NCL_EVALCV_REL:
       status = ur_up_400_evcv_fxd (in_ptr,out_ptr);   
       break;
     case NCL_SURF_REL:
       status = ur_up_400_surf_fxd (in_ptr,out_ptr);   
       break;
     case NCL_EVALSF_REL:
       status = ur_up_400_evsf_fxd (in_ptr,out_ptr);   
       break;
     case NCL_TRIMSF_REL:
       status = ur_up_400_trsf_fxd (in_ptr,out_ptr);
       break;
     case NCL_MESHSURF_REL:
       status = ur_up_400_mssf_fxd (in_ptr,out_ptr);   
       break;
     case NCL_QUILTSURF_REL:
       status = ur_up_400_qlsf_fxd (in_ptr,out_ptr);   
       break;
     case NCL_NETSF_REL:
       status = ur_up_400_ntsf_fxd (in_ptr,out_ptr);   
       break;
     case NCL_POINTVEC_REL:
       status = ur_up_400_pv (in_ptr,out_ptr);         
       break;
     case NCL_SCALAR_REL:
       status = ur_up_400_sc (in_ptr,out_ptr);         
       break;
     case NCL_SHAPE_REL:
       status = ur_up_400_sp_fxd (in_ptr,out_ptr);     
       break;
     case NCL_PATERN_REL:
       status = ur_up_400_pn_fxd (in_ptr,out_ptr);     
       break;
     case UM_CONIC_REL:
       status = ur_up_400_cn (in_ptr,out_ptr);         
       break;
     case UM_COMPCRV_REL:
       status = ur_up_400_ccv_fxd (in_ptr,out_ptr);    
       break;
     case 6:  /*  UM_BSPLCRV_REL */
       status = ur_up_400_bcv_fxd (in_ptr,out_ptr);    
       break;
     case UM_RBSPLCRV_REL:
       status = ur_up_400_rbcv_fxd (in_ptr,out_ptr);   
       break;
     case UM_RBSPLSRF_REL:
       status = ur_up_400_rbsf_fxd (in_ptr,out_ptr);   
       break;
     case UM_POLY_REL:
       status = ur_up_400_pol (in_ptr,out_ptr);        
       break;
     case UM_POLYLINE_REL:
       status = ur_up_400_poll_fxd (in_ptr,out_ptr);   
       break;

     default:
       status = 0;
       break;
    }
  uu_dexit;
  return(status);
 }

/***************************************************************************
**    E_FUNCTION     :  ur_up_400_cv_fxd (e1,e2)
**    Update fixed data NCL cv entity (Unibase ver < 8.400).
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) curve structure.
**       OUTPUT :
**          e2   - pointer to updated output curve structure.
**
****************************************************************************/
int ur_up_400_cv_fxd (e1,e2)
 struct curve82 *e1;
 struct curve84 *e2;
 {
  ur_up_400_ident (e1,e2);
  e2->no_param = e1->no_param;
  e2->no_segment = e1->no_segment;
  e2->closdinu = e1->closdinu;
/*
...New fields
*/
  e2->no_displst = 0;
  e2->displst = UU_NULL;
  e2->t0 = 0;
  e2->t1 = 1.0;
  e2->t_end = -1.0;  

  return(1);
 }

/***************************************************************************
**    E_FUNCTION     :  ur_up_400_bcv_fxd (e1,e2)
**    Update fixed data bspline curve entity (Unibase ver < 8.400).
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) bspline curve structure.
**       OUTPUT :
**          e2   - pointer to updated output bspline curve structure.
**
****************************************************************************/
int
 ur_up_400_bcv_fxd (e1,e2)
 struct bcv82 *e1;
 struct bcv84 *e2;
 {
  ur_up_400_ident (e1,e2);
  e2->inverted = e1->inverted;
  e2->planar = e1->planar;
  e2->open = e1->open;
  e2->k = e1->k;
  e2->n = e1->n;
  e2->t0 = e1->t0;
  e2->t1 = e1->t1;
  e2->no_pt = e1->no_pt;
/*
...New fields
*/
  e2->no_displst = 0;
  e2->displst = UU_NULL;

  return(1);
 }

/***************************************************************************
**    E_FUNCTION     :  ur_up_400_rbcv_fxd (e1,e2)
**    Update fixed data rbspline curve entity (Unibase ver < 8.400).
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) rbspline curve structure.
**       OUTPUT :
**          e2   - pointer to updated output rbspline curve structure.
**
****************************************************************************/
int
 ur_up_400_rbcv_fxd (e1,e2)
 struct rbcv82 *e1;
 struct rbcv84 *e2;
 {
  ur_up_400_ident (e1,e2);
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
/*
...New fields
*/
  e2->no_displst = 0;
  e2->displst = UU_NULL;

  return(1);
 }

/***************************************************************************
**    E_FUNCTION     :  ur_up_400_ccv_fxd (e1,e2)
**    Update fixed data composite curve entity (Unibase ver < 8.400).
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) composite curve structure.
**       OUTPUT :
**          e2   - pointer to updated output composite curve structure.
**
****************************************************************************/
int
 ur_up_400_ccv_fxd (e1,e2)
 struct ccv82 *e1;
 struct ccv84 *e2;
 {
  ur_up_400_ident (e1,e2);
  e2->arclen = e1->arclen;
  e2->planar = e1->planar;
  e2->open = e1->open;
  e2->continuity = e1->continuity;
  e2->fcolor = e1->fcolor;
  e2->no_cid = e1->no_cid;
  e2->closdinu = e1->closdinu;
/*
...New fields
*/
  e2->no_displst = 0;
  e2->displst = UU_NULL;

  return(1);
 }

/***************************************************************************
**    E_FUNCTION     :  ur_up_400_mx (e1,e2)
**    Update fixed data matrix entity (Unibase ver < 8.400).
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) matrix structure.
**       OUTPUT :
**          e2   - pointer to updated output matrix structure.
**
****************************************************************************/
int ur_up_400_mx (e1,e2)
 struct mx82 *e1;
 struct mx84 *e2;
 {
  int i,j;
  ur_up_400_ident (e1,e2);
  e2->no_displst = 0;
  e2->displst = UU_NULL;
  e2->dalen = 1.0;
  for (i=0; i<3; i++)
   { 
    e2->dbox[i] = .75;
    for (j=0; j<4; j++) e2->mat[i][j] = e1->mat[i][j]; 
   }
  return(1);
 }

/***************************************************************************
**    E_FUNCTION     :  ur_up_400_pt (e1,e2)
**    Update fixed data point entity (Unibase ver < 8.400).
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) point structure.
**       OUTPUT :
**          e2   - pointer to updated output point structure.
**
****************************************************************************/
int
 ur_up_400_pt (e1,e2)
 struct pt82 *e1;
 struct pt84 *e2;
 {
  ur_up_400_ident (e1,e2);
  e2->markertype = e1->markertype;
  e2->snap_node = e1->snap_node;
  e2->no_displst = 0;
  e2->displst = UU_NULL;
  um_vctovc (e1->pt,e2->pt); 
  return(1);
 }

/***************************************************************************
**    E_FUNCTION     :  ur_up_400_ln (e1,e2)
**    Update fixed data line entity (Unibase ver < 8.400).
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) line structure.
**       OUTPUT :
**          e2   - pointer to updated output line structure.
**
****************************************************************************/
int
 ur_up_400_ln (e1,e2)
 struct ln82 *e1;
 struct ln84 *e2;
 {
  ur_up_400_ident (e1,e2);
  e2->no_displst = 0;
  e2->displst = UU_NULL;
  um_vctovc (e1->spt,e2->spt); 
  um_vctovc (e1->ept,e2->ept); 
  return(1);
 }

/***************************************************************************
**    E_FUNCTION     :  ur_up_400_ci (e1,e2)
**    Update fixed data circle entity (Unibase ver < 8.400).
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) circle structure.
**       OUTPUT :
**          e2   - pointer to updated output circle structure.
**
****************************************************************************/
int
 ur_up_400_ci (e1,e2)
 struct ci82 *e1;
 struct ci84 *e2;
 {
  ur_up_400_ident (e1,e2);
  e2->no_displst = 0;
  e2->displst = UU_NULL;
  e2->radius = e1->radius;
  e2->dang = e1->dang;
  um_vctovc (e1->center,e2->center); 
  um_vctovc (e1->svec,e2->svec); 
  um_vctovc (e1->nvec,e2->nvec); 
  return(1);
 }

/***************************************************************************
**    E_FUNCTION     :  ur_up_400_vc (e1,e2)
**    Update fixed data vector entity (Unibase ver < 8.400).
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) vector structure.
**       OUTPUT :
**          e2   - pointer to updated output vector structure.
**
****************************************************************************/
int
 ur_up_400_vc (e1,e2)
 struct ve82 *e1;
 struct ve84 *e2;
 {
  ur_up_400_ident (e1,e2);
  e2->no_displst = 0;
  e2->displst = UU_NULL;
  um_vctovc (e1->vec,e2->vec); 
  return(1);
 }

/***************************************************************************
**    E_FUNCTION     :  ur_up_400_pl (e1,e2)
**    Update fixed data plane entity (Unibase ver < 8.400).
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) plane structure.
**       OUTPUT :
**          e2   - pointer to updated output plane structure.
**
****************************************************************************/
int
 ur_up_400_pl (e1,e2)
 struct pln82 *e1;
 struct pln84 *e2;
 {
  ur_up_400_ident (e1,e2);
  e2->no_displst = 0;
  e2->displst = UU_NULL;
  e2->radius = e1->radius;
  um_vctovc (e1->pt,e2->pt); 
  um_vctovc (e1->nvec,e2->nvec); 
  return(1);
 }

/***************************************************************************
**    E_FUNCTION     :  ur_up_400_sc (e1,e2)
**    Update fixed data scalar entity (Unibase ver < 8.400).
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) scalar structure.
**       OUTPUT :
**          e2   - pointer to updated output scalar structure.
**
****************************************************************************/
int
 ur_up_400_sc (e1,e2)
 struct sc82 *e1;
 struct sc84 *e2;
 {
  e2->key = e1->key;
  e2->rel_num = e1->rel_num;
  strcpy (e2->label,e1->label);
  e2->subscr = e1->subscr;
  e2->scalar_value = e1->scalar_value;
  return(1);
 }

/***************************************************************************
**    E_FUNCTION     :  ur_up_400_pv (e1,e2)
**    Update fixed data point-vector entity (Unibase ver < 8.400).
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) point-vector structure.
**       OUTPUT :
**          e2   - pointer to updated output point-vector structure.
**
****************************************************************************/
int
 ur_up_400_pv (e1,e2)
 struct pv82 *e1;
 struct pv84 *e2;
 {
  ur_up_400_ident (e1,e2);
  e2->no_displst = 0;
  e2->displst = UU_NULL;
  um_vctovc (e1->ve,e2->ve); 
  um_vctovc (e1->pt,e2->pt); 
  return(1);
 }

/***************************************************************************
**    E_FUNCTION     :  ur_up_400_pol (e1,e2)
**    Update fixed data poly entity (Unibase ver < 8.400).
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) poly structure.
**       OUTPUT :
**          e2   - pointer to updated output poly structure.
**
****************************************************************************/
int
 ur_up_400_pol (e1,e2)
 struct polgon82 *e1;
 struct polgon84 *e2;
 {
  int i,j;
  ur_up_400_ident (e1,e2);
  e2->fcolor = e1->fcolor;
  e2->numvtx = e1->numvtx;
  for (i=0; i<200; i++)
   {
    for (j=0; j<3; j++) e2->vertex[i][j] = e1->vertex[i][j];
   }
  e2->no_displst = 0;
  e2->displst = UU_NULL;
  return(1);
 }

/***************************************************************************
**    E_FUNCTION     :  ur_up_400_poll_fxd (e1,e2)
**    Update fixed data polyline entity (Unibase ver < 8.400).
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) polyline structure.
**       OUTPUT :
**          e2   - pointer to updated output polyline structure.
**
****************************************************************************/
int
 ur_up_400_poll_fxd (e1,e2)
 struct polln82 *e1;
 struct polln84 *e2;
 {
  ur_up_400_ident (e1,e2);
  e2->no_pt = e1->no_pt;

  e2->no_displst = 0;
  e2->displst = UU_NULL;
  return(1);
 }

/***************************************************************************
**    E_FUNCTION     :  ur_up_400_cn (e1,e2)
**    Update fixed data conic entity (Unibase ver < 8.400).
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) conic structure.
**       OUTPUT :
**          e2   - pointer to updated output conic structure.
**
****************************************************************************/
int
 ur_up_400_cn (e1,e2)
 struct cn82 *e1;
 struct cn84 *e2;
 {
  ur_up_400_ident (e1,e2);
  e2->no_displst = 0;
  e2->displst = UU_NULL;
  e2->type = e1->type;
  um_cparray (12,e1->tfmat,e2->tfmat);
  e2->invariants[0] = e1->invariants[0];
  e2->invariants[1] = e1->invariants[1];
  e2->t0 = e1->t0;
  e2->t1 = e1->t1;
  return(1);
 }

/***************************************************************************
**    E_FUNCTION     :  ur_up_400_sp_fxd (e1,e2)
**    Update fixed data shape entity (Unibase ver < 8.400).
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) shape structure.
**       OUTPUT :
**          e2   - pointer to updated output shape structure.
**
****************************************************************************/
int
 ur_up_400_sp_fxd (e1,e2)
 struct sh82 *e1;
 struct sh84 *e2;
 {
  ur_up_400_ident (e1,e2);
  e2->no_shapwd = e1->no_shapwd;
  e2->no_displst = 0;
  e2->displst = UU_NULL;
  return(1);
 }

/***************************************************************************
**    E_FUNCTION     :  ur_up_400_pn_fxd (e1,e2)
**    Update fixed data patern entity (Unibase ver < 8.400).
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) patern structure.
**       OUTPUT :
**          e2   - pointer to updated output patern structure.
**
****************************************************************************/
int
 ur_up_400_pn_fxd (e1,e2)
 struct pn82 *e1;
 struct pn84 *e2;
 {
  ur_up_400_ident (e1,e2);
  e2->pntype = e1->pntype;
  e2->markertype = e1->markertype;
  e2->no_patpnt = e1->no_patpnt;
  e2->no_displst = 0;
  e2->displst = UU_NULL;
  return(1);
 }

/***************************************************************************
**    E_FUNCTION     :  ur_up_400_evcv_fxd (e1,e2)
**    Update fixed data evaluated crv entity (Unibase ver < 8.400).
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) evaluated crv structure.
**       OUTPUT :
**          e2   - pointer to updated output evaluated crv structure.
**
****************************************************************************/
int
 ur_up_400_evcv_fxd (e1,e2)
 struct evcv82 *e1;
 struct evcv84 *e2;
 {
  ur_up_400_ident (e1,e2);
  e2->curve_type = e1->curve_type;
  e2->no_evwd = e1->no_evwd;  
  e2->no_displst = 0;
  e2->displst = UU_NULL;

  return(1);
 }

/***************************************************************************
**    E_FUNCTION     :  ur_up_400_rbsf_fxd (e1,e2)
**    Update fixed data rbspline surf entity (Unibase ver < 8.400).
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) rbspline surf structure.
**       OUTPUT :
**          e2   - pointer to updated output rbspline surf structure.
**
****************************************************************************/
int
 ur_up_400_rbsf_fxd (e1,e2)
 struct rbsf82 *e1;
 struct rbsf84 *e2;
 {
  ur_up_400_ident (e1,e2);
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

  e2->no_displst = 0;
  e2->displst = UU_NULL;
  e2->no_tesslst = 0;
  e2->tesslst = UU_NULL;

  return(1);
 }

/***************************************************************************
**    E_FUNCTION     :  ur_up_400_surf_fxd (e1,e2)
**    Update fixed data NCL surf entity (Unibase ver < 8.400).
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) NCL surf structure.
**       OUTPUT :
**          e2   - pointer to updated output NCL surf structure.
**
****************************************************************************/
int
 ur_up_400_surf_fxd (e1,e2)
 struct surf82 *e1;
 struct surf84 *e2;
 {
  ur_up_400_ident (e1,e2);
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
  e2->no_panelkey = e1->no_panelkey;  

  e2->no_displst = 0;
  e2->displst = UU_NULL;
  e2->no_tesslst = 0;
  e2->tesslst = UU_NULL;

  return(1);
 }

/***************************************************************************
**    E_FUNCTION     :  ur_up_400_evsf_fxd (e1,e2)
**    Update fixed data evaluated surf entity (Unibase ver < 8.400).
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) evaluated surf structure.
**       OUTPUT :
**          e2   - pointer to updated output evaluated surf structure.
**
****************************************************************************/
int
 ur_up_400_evsf_fxd (e1,e2)
 struct evsf82 *e1;
 struct evsf84 *e2;
 {
  ur_up_400_ident (e1,e2);
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

  e2->no_displst = 0;
  e2->displst = UU_NULL;
  e2->no_tesslst = 0;
  e2->tesslst = UU_NULL;

  return(1);
 }

/***************************************************************************
**    E_FUNCTION     :  ur_up_400_trsf_fxd (e1,e2)
**    Update fixed data tirmmed surf entity (Unibase ver < 8.400).
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) tirmmed surf structure.
**       OUTPUT :
**          e2   - pointer to updated output tirmmed surf structure.
**
****************************************************************************/
int
 ur_up_400_trsf_fxd (e1,e2)
 struct tsf82 *e1;
 struct tsf84 *e2;
 {
  ur_up_400_ident (e1,e2);
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
  e2->no_ibndykey = e1->no_ibndykey;  

  e2->no_displst = 0;
  e2->displst = UU_NULL;
  e2->no_tesslst = 0;
  e2->tesslst = UU_NULL;

  return(1);
 }

/***************************************************************************
**    E_FUNCTION     :  ur_up_400_mssf_fxd (e1,e2)
**    Update fixed data mesh surf entity (Unibase ver < 8.400).
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) mesh surf structure.
**       OUTPUT :
**          e2   - pointer to updated output mesh surf structure.
**
****************************************************************************/
int
 ur_up_400_mssf_fxd (e1,e2)
 struct msf82 *e1;
 struct msf84 *e2;
 {
  ur_up_400_ident (e1,e2);
  e2->surf_type = e1->surf_type;
  e2->material = e1->material;
  e2->m = e1->m;
  e2->n = e1->n;
  e2->numupaths = e1->numupaths;
  e2->numvpaths = e1->numvpaths;
  e2->rldnu = e1->rldnu;
  e2->rldnv = e1->rldnv;
  e2->offset = e1->offset;
  e2->offdist = e1->offdist;
  e2->ptsperucrv = e1->ptsperucrv;
  e2->ptspervcrv = e1->ptspervcrv;
  e2->closdinu = e1->closdinu;
  e2->closdinv = e1->closdinv;
  e2->rev_normal = e1->rev_normal;
  e2->no_mpatch = e1->no_mpatch;  

  e2->no_displst = 0;
  e2->displst = UU_NULL;
  e2->no_tesslst = 0;
  e2->tesslst = UU_NULL;

  return(1);
 }

/***************************************************************************
**    E_FUNCTION     :  ur_up_400_qlsf_fxd (e1,e2)
**    Update fixed data quilt surf entity (Unibase ver < 8.400).
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) quilt surf structure.
**       OUTPUT :
**          e2   - pointer to updated output quilt surf structure.
**
****************************************************************************/
int
 ur_up_400_qlsf_fxd (e1,e2)
 struct qsf82 *e1;
 struct qsf84 *e2;
 {
  int i,j;

  ur_up_400_ident (e1,e2);
  e2->surf_type = e1->surf_type;
  e2->numpatches = e1->numpatches;
  e2->rldnu = e1->rldnu;
  e2->rldnv = e1->rldnv;
  e2->offset = e1->offset;
  e2->offdist = e1->offdist;
  for (i=0; i<12; i++)
   {
    for (j=0; j<3; j++) e2->midpt[i][j] = e1->midpt[i][j];
   }

  e2->no_qpatch = e1->no_qpatch;  
  e2->no_displst = 0;
  e2->displst = UU_NULL;
  e2->no_tesslst = 0;
  e2->tesslst = UU_NULL;

  return(1);
 }

/***************************************************************************
**    E_FUNCTION     :  ur_up_400_ntsf_fxd (e1,e2)
**    Update fixed data net surf entity (Unibase ver < 8.400).
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) net surf structure.
**       OUTPUT :
**          e2   - pointer to updated output net surf structure.
**
****************************************************************************/
int
 ur_up_400_ntsf_fxd (e1,e2)
 struct nsf82 *e1;
 struct nsf84 *e2;
 {
  int i,j;

  ur_up_400_ident (e1,e2);
  e2->surf_type = e1->surf_type;
  e2->rldnu = e1->rldnu;
  e2->rldnv = e1->rldnv;
  for (i=0; i<40; i++)
   {
    for (j=0; j<4; j++) e2->bndsfs[i][j] = e1->bndsfs[i][j];
   }

  e2->no_netkey = e1->no_netkey;  
  e2->no_displst = 0;
  e2->displst = UU_NULL;
  e2->no_tesslst = 0;
  e2->tesslst = UU_NULL;

  return(1);
 }

/***************************************************************************
**    E_FUNCTION     :  ur_update_8400_attr (key)
**    Update attribute bundle (Unibase ver < 8.400).
**    PARAMETERS
**       INPUT  :
**          key    - Entity key.
**       OUTPUT :
**          none
**
****************************************************************************/
int ur_update_8400_attr (key,rel_num)
 UU_KEY_ID key;
 int rel_num;
 {
  int status;
 
  switch (rel_num)
    {
     case NCL_MATRIX_REL:
       status = ur_up_400_mx_attr (key);
       break;
     default:
       status = 0;
       break;
    }
  uu_dexit;
  return(status);
 }

/**************************************************************************
**    E_FUNCTION     :  ur_up_400_mx_attr (key)
**    Update attribute bundle for matrix (Unibase ver < 8.400).
**    PARAMETERS
**       INPUT  :
**          key    - Entity key.
**       OUTPUT :
**          none
**
****************************************************************************/
int 
 ur_up_400_mx_attr (key)
  UU_KEY_ID key;
  {
   int status;
/*
...Make MX displayable, set color &
...line type
*/   
   status = ur_update_displayable (key,UM_DISPLAYABLE);
   if (status == UU_SUCCESS)
      status = ur_update_color(key,UM_DARKBLUE);
   if (status == UU_SUCCESS)
      status = ur_update_line_style(key,UM_DOT_LINE);

   uu_dexit;
   return(status); 
  }

/**************************************************************************
**    E_FUNCTION     :  ur_up_400_ident (e1,e2)
**    Update identity bundle for general entity (Unibase ver < 8.400).
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) identity structure.
**       OUTPUT :
**          e2   - pointer to updated output identity structure.
**
****************************************************************************/
int 
 ur_up_400_ident (e1,e2)
 struct id82 *e1;
 struct id84 *e2;
  {
/*
...copy data
*/   
   e2->key = e1->key;
   e2->rel_num = e1->rel_num;
   strcpy (e2->label,e1->label);
   e2->subscr = e1->subscr;

   e2->labloc[0] = e2->labloc[1] = e2->labloc[2] = 0.; 

   uu_dexit;
   return(0); 
  }
