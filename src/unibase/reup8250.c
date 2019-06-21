/************************************************************************
**     NAME       : reupd8250.c
**     CONTAINS   :
**        ur_up_ate_8250 (in_ptr,out_ptr)
**        ur_up_250_sf_fxd (e1,e2)
**        ur_up_250_evsf_fxd (e1,e2)
**        ur_up_250_msf_fxd (e1,e2)
**
**     MODULE NAME AND RELEASE LEVEL
**       reup8250.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:39
**
*************************************************************************/

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
#include "nccs.h"
#include "dtypes.h"
#include "lcom.h"
#include "nclver.h"
#include "mcrv.h"
#include "msrf.h"
#include "rver9600.h"

/***************************************************************************
**    E_FUNCTION     :  ur_update_8250 (in_ptr,out_ptr)
**    Update fixed data of entity (Unibase ver < 8.250).
**    PARAMETERS
**       INPUT  :
**          in_ptr    - Pointer to input structure.
**       OUTPUT :
**          out_ptr   - Pointer to output structure.
****************************************************************************/
int ur_update_8250 (in_ptr,out_ptr)
 struct UR_data *in_ptr, *out_ptr;
 {
  int rel, status;
  rel = in_ptr->rel_num;
 
  switch (rel)
    {
     case NCL_SURF_REL:
       status = ur_up_250_sf_fxd (in_ptr,out_ptr);
       break;
     case NCL_EVALSF_REL:
       status = ur_up_250_evsf_fxd (in_ptr,out_ptr);
       break;
     case NCL_MESHSURF_REL:
       status = ur_up_250_msf_fxd (in_ptr,out_ptr);
       break;
     default:
       status = 0;
       break;
    }
  uu_dexit;
  return(status);
 }

/***************************************************************************
**    E_FUNCTION     :  ur_up_250_sf_fxd (e1,e2)
**    Update display parameters of NCL surf (Unibase ver < 8.250).
**    PARAMETERS
**       INPUT  :
**          e1    - Pointer to input structure.
**       OUTPUT :
**          e2    - Pointer to output structure (not used
**                  so routine returns 0 since change is in e1).
****************************************************************************/
int ur_up_250_sf_fxd (e1,e2)
 struct NCL_surface_rec95 *e1, *e2;
 {
  e1->ptsperucrv = e1->numvpaths;
  e1->ptspervcrv = e1->numupaths;

  uu_dexit;
  return(0);
 }

/***************************************************************************
**    E_FUNCTION     :  ur_up_250_evsf_fxd (e1,e2)
**    Update display parameters of evaluated surf (Unibase ver < 8.250).
**    PARAMETERS
**       INPUT  :
**          e1    - Pointer to input structure.
**       OUTPUT :
**          e2    - Pointer to output structure (not used
**                  so routine returns 0 since change is in e1).
****************************************************************************/
int ur_up_250_evsf_fxd (e1,e2)
 struct NCL_evalsf_rec *e1, *e2;
 {
  e1->ptsperucrv = e1->numvpaths;
  e1->ptspervcrv = e1->numupaths;

  uu_dexit;
  return(0);
 }

/***************************************************************************
**    E_FUNCTION     :  ur_up_250_evsf_fxd (e1,e2)
**    Update display parameters of mesh surf (Unibase ver < 8.250).
**    PARAMETERS
**       INPUT  :
**          e1    - Pointer to input structure.
**       OUTPUT :
**          e2    - Pointer to output structure (not used
**                  so routine returns 0 since change is in e1).
****************************************************************************/
int ur_up_250_msf_fxd (e1,e2)
 struct NCL_meshsf_rec95 *e1, *e2;
 {
  e1->ptsperucrv = e1->numvpaths;
  e1->ptspervcrv = e1->numupaths;

  uu_dexit;
  return(0);
 }


