/*********************************************************************
**    NAME         :  nedrvcrv.c
**       CONTAINS: 
**       int cptint (nclkey, tol, irev, npts)
**       int cptnxt (pt, ve)
**       int cptend ()
**    COPYRIGHT 1995 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       nedrvcrv.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:32
*********************************************************************/

#include "usysdef.h"
#include "umath.h"
#include "mdcoord.h"
#include "mfort.h"
#include "ncl.h"
#include "nclfc.h"
#include "nccs.h"
#include "ulist.h"

static UU_LIST NCL_crvpts;
static UU_LIST NCL_crvve;
static UU_REAL *NCL_ppt;
static UU_REAL *NCL_pve;
static int NCL_npts;
static int NCL_ptix;
static int NCL_dir;

/*********************************************************************
**    E_FUNCTION     : int cptint (nclkey, tol, irev, npts)
**       Fortran callable routine to initialize getting points on a curve.
**    PARAMETERS   
**       INPUT  : 
**          nclkey       - UNIBASE key of entity to drive
**          tol          - curve tolerance.
**          irev         - Get points in reverse order.
**       OUTPUT :  
**          npts         - Number of output points or 0 iff error.
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int cptint (nclkey, tol, irev, npts)
UM_int4 *nclkey;
UM_real8 *tol;
UM_int2 *irev;
UM_int2 *npts;
{
   int status;
   UU_REAL ctol;
   struct NCL_fixed_databag crv;
   UM_transf tfmat;

   *npts = 0;
   crv.key = *nclkey;
   status = ncl_retrieve_data_fixed (&crv);
   if (status == UU_SUCCESS)
     status = um_get_transformation (crv.key, tfmat);
   if (status == UU_SUCCESS)
     {
     ctol = *tol;
     uu_list_init (&NCL_crvpts, sizeof(UM_coord), 128, 128);
     uu_list_init (&NCL_crvve, sizeof(UM_coord), 128, 128);
     NCL_npts = 
       ncl_evolve_curve (&crv,tfmat,ctol,&NCL_crvpts,&NCL_crvve,UU_NULL,0);
     if (NCL_npts < 1) status = UU_FAILURE;
     *npts = NCL_npts;
     if (*irev)
       {
       NCL_ptix = NCL_npts-1;
       NCL_dir = -1;
       }
     else
       {
       NCL_ptix = 0;
       NCL_dir = 1;
       }
     NCL_ppt = (UU_REAL *) UU_LIST_ARRAY (&NCL_crvpts);
     NCL_pve = (UU_REAL *) UU_LIST_ARRAY (&NCL_crvve);
     }

   return (status);
}
/*********************************************************************
**    E_FUNCTION     : int cptnxt (pt, ve)
**       Fortran callable routine to get next point on a curve.
**    PARAMETERS   
**       INPUT  : 
**          none
**          tol          - curve tolerance.
**       OUTPUT :  
**          pt           - Next point.
**          ve           - Next vector.
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int cptnxt (pt, ve)
UM_real8 *pt, *ve;
{
   int status, i, ix;

   if (NCL_ptix < 0 || NCL_ptix >= NCL_npts)
     status = UU_FAILURE;
   else
     {
     status = UU_SUCCESS;
     ix = NCL_ptix * 3;
     for (i=0;i<3;i++)
       {
       pt[i] = NCL_ppt[ix+i];
       ve[i] = NCL_pve[ix+i];
       }
     }

   NCL_ptix += NCL_dir;

   return (status);
}
/*********************************************************************
**    E_FUNCTION     : int cptend ()
**       Fortran callable routine to free points on a curve lists..
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int cptend ()
{
   int status = UU_SUCCESS;

   uu_list_free (&NCL_crvpts);
   uu_list_free (&NCL_crvve);

   return (status);
}
