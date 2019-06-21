/*********************************************************************
**    NAME         :  m3evect.c
**       CONTAINS:
**   int um_tf2_tranfvec (eptr,tranfmat,store)
**   int um_tf2_tranfpvec(eptr,tranfmat,store)
**
**    COPYRIGHT 1994 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**        m3evect.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**        04/29/15 , 15:07:55
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "go.h"
#include "mdrel.h"
#include "mdcoord.h"
#include "mattr.h"
#include "mdattr.h"
#include "mcrv.h"
#include "mdeval.h"
#include "modef.h"
#include "mplot.h"
#include "mdebug.h"
#include "nccs.h"

/*********************************************************************
**    E_FUNCTION     : int um_tf2_tranfvec(eptr,tranfmat,store)
**   Transform a vector by applying the specified 4X3 transformation
**   and update UNIBASE if store == UU_TRUE.
**    PARAMETERS
**       INPUT  :
**    eptr          pointer to the entity to be transformed
**          tranfmat      the 4x3 transformation matrix
**    store     TRUE iff UNIBASE is to be updated here.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_tf2_tranfvec(eptr,tranfmat,store)
 struct NCL_vector_rec *eptr;
 UM_transf    tranfmat;
 UU_LOGICAL store;

 {

 uu_denter(UU_MTRC,(us,"um_tf2_tranfvec(key:%d,tfmat:%x,store:%d)",
     eptr->key, tranfmat,store));

   um_vctmtf(eptr->vec, tranfmat, eptr->vec);

 if (store)
  um_update_geom(eptr, UM_DEFAULT_TF);
 uu_dexit;
 return (UU_SUCCESS);
 }

/*********************************************************************
**    E_FUNCTION     : int um_tf2_tranfpvec(eptr,tranfmat,store)
**   Transform a pointvector by applying the specified 4X3 transformation
**   and update UNIBASE if store == UU_TRUE.
**    PARAMETERS
**       INPUT  :
**    eptr          pointer to the entity to be transformed
**          tranfmat      the 4x3 transformation matrix
**    store     TRUE iff UNIBASE is to be updated here.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_tf2_tranfpvec(eptr,tranfmat,store)
 struct NCL_nclpv_rec *eptr;
 UM_transf    tranfmat;
 UU_LOGICAL store;

 {

 uu_denter(UU_MTRC,(us,"um_tf2_tranfvec(key:%d,tfmat:%x,store:%d)",
     eptr->key, tranfmat,store));

   um_cctmtf(eptr->pt, tranfmat, eptr->pt);
   um_vctmtf(eptr->ve, tranfmat, eptr->ve);

 if (store)
  um_update_geom(eptr, UM_DEFAULT_TF);
 uu_dexit;
 return (UU_SUCCESS);
 }

