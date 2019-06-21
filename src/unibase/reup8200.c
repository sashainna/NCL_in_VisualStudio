/****************************************************
**     NAME       : reupd8200.c
**     CONTAINS   :
**        ur_update_8200 (in_ptr,out_ptr)
**        ur_update_8200_varl (eptr,list,vli_ptr,vlo_ptr)
**        ur_up_8200_atomsize (rel_num,lst_num,atom_size)
**        ur_up_200_pn_fxd (e1,e2)
**        ur_up_200_pn_varlist (e2,lst_num,vl1,vl2)
**
**     MODULE NAME AND RELEASE LEVEL
**       reup8200.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:39
**
*****************************************************/

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
#include "nccs.h"
#include "view.h"
#include "dtypes.h"
#include "lcom.h"
#include "nclver.h"
#include "rver8200.h"
#include "rver8400.h"

/***************************************************************************
**    E_FUNCTION     :  ur_update_8200 (in_ptr,out_ptr)
**    Update fixed data of entity (Unibase ver < 8.200). 
**    PARAMETERS
**       INPUT  :
**          in_ptr    - Pointer to input structure.
**       OUTPUT :
**          out_ptr   - Pointer to output structure.
****************************************************************************/
int ur_update_8200 (in_ptr,out_ptr)
 struct UR_data *in_ptr, *out_ptr;
 {
  int status;
  status = 0; 
  switch (in_ptr->rel_num)
    {
     case UV_VPORT_REL:
       status = ur_up_200_vport_fxd (in_ptr,out_ptr);
       break;
     case NCL_PATERN_REL:
       status = ur_up_200_pn_fxd (in_ptr,out_ptr);
       break;
     default:
       status = 0;
       break;
    }
  uu_dexit;
  return(status);
 }

/***************************************************************************
**    E_FUNCTION     :  ur_update_8200_varl (eptr,list,vli_ptr,vlo_ptr)
**    Update variable list data of entity (Unibase ver < 8.200). 
**    PARAMETERS
**       INPUT  :
**          eptr     - Pointer to input structure (updated already).
**          list     - Var-list number to update.
**          vli_ptr  - Pointer to input variable list data (#list).
**       OUTPUT :
**          vli_ptr  - Pointer to updated list data.
****************************************************************************/
int ur_update_8200_varl (eptr,list,vli_ptr,vlo_ptr)
 struct UR_data *eptr;
 int list;
 char *vli_ptr, **vlo_ptr;
 {
  int status;
  switch (eptr->rel_num)
    {
     case NCL_PATERN_REL:
       status = ur_up_200_pn_varlist (eptr,list,vli_ptr,vlo_ptr);
       break;
     default:
       status = 0;
       break;
    }
  uu_dexit;
  return(status);
 }

/***************************************************************************
**    E_FUNCTION     :  ur_up_200_vport_fxd (e1,e2)
**    Update fixed data of vport entity (Unibase ver < 8.200). 
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) vport structure.
**       OUTPUT :
**          e2   - pointer to updated output vport structure.
**
****************************************************************************/
int ur_up_200_vport_fxd (e1,e2)
 struct vp8105 *e1;
 struct vp82 *e2;
 {
  int i;
/*
...Copy unchanged fields of UV_vport
*/
  e2->key = e1->key;
  e2->rel_num = e1->rel_num;
  strcpy (e2->name,e1->name);
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
  for (i=0; i<3; i++) 
   {
    e2->llf[i] = e1->llf[i];
    e2->urb[i] = e1->urb[i];
   }
  for (i=0; i<60; i++) e2->vertices[i] = e1->vertices[i]; 
/*
...Updated fields 
*/
  e2->motion = UU_TRUE;

  uu_dexit;
  return(1);
 }
/***************************************************************************
**    E_FUNCTION     :  ur_up_200_pn_fxd (e1,e2)
**    Update fixed data patern entity (Unibase ver < 8.200). 
**
**    NOTE: Old structure for entity (here 'old_pn') must be defined in 
**    the 'ver9999.h' file.  To obtain old structure for specific relation
**    number get old version of 'isysddl.ddl' file (see note in 
**    'ur_up_8200_atomsize' function) where entity is defined. Find the 
**    name of include file where ddltool puts generated structure and
**    get this structure from named file. Rename structure when putting
**    in 'ver9999.h' file so it will not collide with the current structure
**    defining entity. 
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) patern structure.
**       OUTPUT :
**          e2   - pointer to updated output patern structure.
****************************************************************************/
int ur_up_200_pn_fxd (e1,e2)
 struct pn8105 *e1;
 struct pn82 *e2;
 {
  int i,j;
/*
...Copy unchanged fields of patern
*/
  ur_up_400_ident (e1,e2);
  e2->markertype = e1->markertype;
/*
...Updated fields 
*/
  e2->pntype = 1;
  e2->no_patpnt = e1->no_patpnt * 3;

  uu_dexit;
  return(1);
 }

/***************************************************************************
**    E_FUNCTION     :  ur_up_200_pn_varlist (e2,lst_num,vl1,vlo)
**    Update variable list in patern entity (Unibase ver < 8.200). 
**    PARAMETERS
**       INPUT  :
**          e2      - pointer to patern structure.
**          lst_num - var-list number (not used for patern). 
**          vl1     - pointer to input var-list.
**       OUTPUT :
**          vlo     - pointer to pointer of updated output var-list.
****************************************************************************/
int ur_up_200_pn_varlist (e2,lst_num,vl1,vlo)
 struct pn82 *e2;
 int lst_num;
 struct NCL_patpnt_rec *vl1;
 char **vlo;
 {
  int i,j,np,nr;
  UU_REAL *vl2;

  nr    = e2->no_patpnt; 
  np    = nr / 3;
  vl2   = UU_NULL;
  vl2   = (UU_REAL *) uu_toolmalloc (nr * sizeof(UU_REAL));
  *vlo  = (char *) vl2;
  i     = 0;
  for (j=0; j<np; j++)
    {
     vl2[i++] = vl1->pt[0];
     vl2[i++] = vl1->pt[1];
     vl2[i++] = vl1->pt[2];
     vl1++;
    }
  uu_dexit;
  return(1);
 }

/***************************************************************************
**    E_FUNCTION     :  ur_up_8200_atomsize (rel_num,lst_num,atom_size)
**    Gets size of var-list number lst_num for entity defined by rel_num
**    for changed only relations in the 8.200 version. 
**
**    NOTE: To add new relation/var-list modify includ file ver8200.h
**    adding array with sizes (in bytes) for every var-list associated with
**    relation.  To obtain old size of list retrieve required version
**    of 'isysddl.ddl' file from sccs database and run ddltool against it
**    to create prehistoric 'riatrdct.h' and 'rireldct.h' files. 
**    In the 'rireldct.h' file find required variable list (by name) and
**    the 3-rd parameter in the relblk record will be the record number in the 
**    'riatrdct.h' file where description of the variable list begins.
**    Open 'riatrdct.h' file and find this record in 'attr_def' structure. 
**    Specific atom size can be obtained by adding parameter 4 and 5 
**    specified in the last record describing this list (the last record of 
**    specification is followed by record with "key" label which starts next
**    list or relation description).
**    in the relblk record specific for required relation contains record
**    number (count from 0) of the attr_def structure stored in 'riatrdct.h'.  
**
**    PARAMETERS
**       INPUT  :
**          rel_num   - Relation number.
**          lst_num   - Variable list index in relation.
**       OUTPUT :
**          atom_size - List size (in bytes).
****************************************************************************/
int ur_up_8200_atomsize (rel_num,lst_num,atom_size)
 UR_REL_NUM rel_num;
 int lst_num;
 int *atom_size;
 {
  int status;
  status = 0;
/*
...Update from < 8.200 version to current contains
...lists for following relations: patern 
...Go to required relation number
*/
  switch (rel_num)
   {
    case NCL_PATERN_REL:
       *atom_size = REL_92_LSIZE[lst_num-1]; 
       break;
    default:
       status = 1;
       break;
   } 

  uu_dexit;
  return(status);
 }
