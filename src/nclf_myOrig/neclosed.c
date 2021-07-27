/*********************************************************************
**    NAME         :  neclosed.c
**       CONTAINS: routines to handle storing & retrieving of curve & surface
**                 closed.
**       int ptclsd (nclkey, iflg)
**       int gtclsd1 (nclkey, iflg)
**		 int ncl_get_closdinuv (sfkey,closdinu,closdinv)
**    COPYRIGHT 1991 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       neclosed.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:26
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "class.h"
#include "mfort.h"
#include "mdrel.h"
#include "mdcoord.h"
#include "mattr.h"
#include "mdattr.h"
#include "mdebug.h"
#include "mcrv.h"
#include "msrf.h"
#include "nccs.h"
#include "ncl.h"
#include "nclfc.h"
#include "mdeval.h"

/*********************************************************************
**    E_FUNCTION     : int ptclsd (nclkey, iflg, iclosd)
**       Update the closed flag for a curve or surface.
**    PARAMETERS
**       INPUT  :
**          nclkey               UNIBASE key of entity
**          iflg                 0 = update closed in u flag.
**                               1 = update closed in v flag.
**          iclosd               0 = open
**                               1 = closed
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ptclsd (nclkey, iflg, iclosd)
   UM_int4  *nclkey;
   UM_int2  *iflg;
   UM_int2  *iclosd;

   {
   int status;
   UU_KEY_ID key;
   int rel_num;
   int lflg;
   int lclosd;

   uu_denter(UU_MTRC,(us,"ptclsd (key=%d, iflg=%d, iclosd=%d)",
                *nclkey, *iflg, *iclosd));

   key = *nclkey;
   lflg = *iflg;
   lclosd = *iclosd;
   status = UU_FAILURE;
   if (ur_retrieve_data_relnum(key, &rel_num) == 0)
     {
     switch (rel_num)
        {
        case NCL_CURVE_REL:
           status = ncl_update_ncv_closed (key, lclosd);
           break;
        case UM_COMPCRV_REL:
           status = ncl_update_cmp_closed (key, lclosd);
           break;
        case UM_AGCRV_REL:
           status = ncl_update_agcv_closed (key, lclosd);
           break;
        case UM_RBSPLCRV_REL:
           status = ncl_update_rbcv_closed (key, lclosd);
           break;
        case NCL_SURF_REL:
           status = ncl_update_nsf_closed (key, lflg, lclosd);
           break;
        case NCL_REVSURF_REL:
           status = ncl_update_revsf_closed (key, lflg, lclosd);
           break;
        case NCL_MESHSURF_REL:
           status = ncl_update_msf_closed (key, lflg, lclosd);
           break;
        case UM_AGSRF_REL:
           status = ncl_update_agsf_closed (key, lflg, lclosd);
           break;
        case UM_RBSPLSRF_REL:
           status = ncl_update_rbsf_closed (key, lflg, lclosd);
           break;
        default:
           break;
        }
     }

   uu_dexit;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : int gtclsd1 (nclkey, iflg, iclosd)
**       Retrieve the closed flag for a curve or surface.
**    PARAMETERS
**       INPUT  :
**          nclkey               UNIBASE key of entity
**          iflg                 0 = get closed in u flag
**                               1 = get closed in v flag
**       OUTPUT :
**          iclosed              closed flag
**    RETURNS      :
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
gtclsd1 (nclkey, iflg, iclosd)
   UM_int4  *nclkey;
   UM_int2  *iflg;
   UM_int2  *iclosd;

   {
   int status;
   UU_KEY_ID key;
   int rel_num;
   int lflg;
   int lclosd;

   uu_denter(UU_MTRC,(us,"gtclsd1 (key=%d)", *nclkey));

   key = *nclkey;
   lflg = *iflg;
   lclosd = 0;
   status = UU_FAILURE;
   if (ur_retrieve_data_relnum(key, &rel_num) == 0)
     {
     switch (rel_num)
        {
        case NCL_CURVE_REL:
           status = ncl_get_ncv_closed (key, &lclosd);
           break;
        case UM_CONIC_REL:
           status = ncl_get_conic_closed (key, &lclosd);
           break;
        case UM_COMPCRV_REL:
           status = ncl_get_cmp_closed (key, &lclosd);
           break;
        case UM_AGCRV_REL:
           status = ncl_get_agcv_closed (key, &lclosd);
           break;
        case UM_RBSPLCRV_REL:
           status = ncl_get_rbcv_closed (key, &lclosd);
           break;
        case UM_POLYLINE_REL:
           status = ncl_get_polyln_closed (key, &lclosd);
           break;
        case NCL_SURF_REL:
           status = ncl_get_nsf_closed (key, lflg, &lclosd);
           break;
        case NCL_REVSURF_REL:
           status = ncl_get_revsf_closed (key, lflg, &lclosd);
           break;
        case NCL_MESHSURF_REL:
           status = ncl_get_msf_closed (key, lflg, &lclosd);
           break;
        case UM_AGSRF_REL:
           status = ncl_get_agsf_closed (key, lflg, &lclosd);
           break;
        case UM_RBSPLSRF_REL:
           status = ncl_get_rbsf_closed (key, lflg, &lclosd);
           break;
        default:
           break;
        }
     }

   *iclosd = lclosd;

   uu_dexit;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : ncl_update_ncv_closed (key, iclosd)
**       Update the closed flag for a NCL curve.
**    PARAMETERS
**       INPUT  :
**          nclkey               UNIBASE key of entity
**          iclosd               Closed flag.
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_update_ncv_closed (key, iclosd)
   UU_KEY_ID key;
   int iclosd;

   {
   int status;
   struct NCL_curve_rec cv;

   uu_denter(UU_MTRC,(us,"ncl_update_ncv_closed (key=%d, iclosd=%d)", key, iclosd));

   status = UU_FAILURE;
   cv.key = key;
   if (ur_retrieve_data_fixed(&cv) ==0)
       {
       cv.closdinu = iclosd;
       if (ur_update_data_fixed(&cv) == 0) status = UU_SUCCESS;
       }

   uu_dexit;
   return (status);
   }
/*********************************************************************
**    E_FUNCTION     : ncl_update_cmp_closed (key, iclosd)
**       Update the closed flag for a composite curve.
**    PARAMETERS
**       INPUT  :
**          nclkey               UNIBASE key of entity
**          iclosd               Closed flag.
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_update_cmp_closed (key, iclosd)
   UU_KEY_ID key;
   int iclosd;

   {
   int status;
   struct UM_compcrv_rec cv;

   uu_denter(UU_MTRC,(us,"ncl_update_cmp_closed (key=%d, iclosd=%d)", key, iclosd));

   status = UU_FAILURE;
   cv.key = key;
   if (ur_retrieve_data_fixed(&cv) ==0)
       {
       cv.closdinu = iclosd;
       if (ur_update_data_fixed(&cv) == 0) status = UU_SUCCESS;
       }

   uu_dexit;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : ncl_update_agcv_closed (key, iclosd)
**       Update the closed flag for a AG curve.
**    PARAMETERS
**       INPUT  :
**          nclkey               UNIBASE key of entity
**          iclosd               Closed flag.
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_update_agcv_closed (key, iclosd)
   UU_KEY_ID key;
   int iclosd;

   {
   int status;
   struct UM_agcrv_rec cv;

   uu_denter(UU_MTRC,(us,"ncl_update_agcv_closed (key=%d, iclosd=%d)", key, iclosd));

   status = UU_FAILURE;
   cv.key = key;
   if (ur_retrieve_data_fixed(&cv) ==0)
       {
       cv.closdinu = iclosd;
       if (ur_update_data_fixed(&cv) == 0) status = UU_SUCCESS;
       }

   uu_dexit;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : ncl_update_rbcv_closed (key, iclosd)
**       Update the closed flag for a rb curve.
**    PARAMETERS
**       INPUT  :
**          nclkey               UNIBASE key of entity
**          iclosd               Closed flag.
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_update_rbcv_closed (key, iclosd)
   UU_KEY_ID key;
   int iclosd;

   {
   int status;
   struct UM_rbsplcrv_rec cv;

   uu_denter(UU_MTRC,(us,"ncl_update_rbcv_closed (key=%d, iclosd=%d)", key, iclosd));

   status = UU_FAILURE;
   cv.key = key;
   if (ur_retrieve_data_fixed(&cv) ==0)
       {
       cv.closdinu = iclosd;
       if (ur_update_data_fixed(&cv) == 0) status = UU_SUCCESS;
       }

   uu_dexit;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : ncl_update_nsf_closed (key, lflg, iclosd);
**       Update the closed flag for a NCL surface.
**    PARAMETERS
**       INPUT  :
**          nclkey               UNIBASE key of entity
**          iflg                 which flag
**          iclosd               flag value
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_update_nsf_closed (key, iflg, iclosd)
   UU_KEY_ID key;
   int iflg;
   int iclosd;

   {
   int status;
   struct NCL_surface_rec surf;

   uu_denter(UU_MTRC,(us,"ncl_update_nsf_closed (key=%d, iflg=%d)", key, iflg));

   status = UU_FAILURE;
   surf.key = key;
   if (ur_retrieve_data_fixed(&surf) ==0)
       {
       if (iflg == 0) surf.closdinu = iclosd;
       else if (iflg == 1) surf.closdinv = iclosd;
       if (ur_update_data_fixed(&surf) == 0) status = UU_SUCCESS;
       }

   uu_dexit;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : ncl_update_revsf_closed (key, lflg, iclosd);
**       Update the closed flag for a NCL surface of revolution.
**    PARAMETERS
**       INPUT  :
**          nclkey               UNIBASE key of entity
**          iflg                 which flag
**          iclosd               flag value
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_update_revsf_closed (key, iflg, iclosd)
   UU_KEY_ID key;
   int iflg;
   int iclosd;

   {
   int status;
   struct NCL_revsurf_rec surf;

   uu_denter(UU_MTRC,(us,"ncl_update_nsf_closed (key=%d, iflg=%d)", key, iflg));

   status = UU_FAILURE;
   surf.key = key;
   if (ur_retrieve_data_fixed(&surf) ==0)
       {
       if (iflg == 0) surf.closdinu = iclosd;
       else if (iflg == 1) surf.closdinv = iclosd;
       if (ur_update_data_fixed(&surf) == 0) status = UU_SUCCESS;
       }

   uu_dexit;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : ncl_update_msf_closed (key, lflg, iclosd);
**       Update the closed flag for a NCL surface.
**    PARAMETERS
**       INPUT  :
**          nclkey               UNIBASE key of entity
**          iflg                 which flag
**          iclosd               flag value
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_update_msf_closed (key, iflg, iclosd)
   UU_KEY_ID key;
   int iflg;
   int iclosd;

   {
   int status;
   struct NCL_meshsf_rec surf;

   uu_denter(UU_MTRC,(us,"ncl_update_msf_closed (key=%d, iflg=%d)", key, iflg));

   status = UU_FAILURE;
   surf.key = key;
   if (ur_retrieve_data_fixed(&surf) ==0)
       {
       if (iflg == 0) surf.closdinu = iclosd;
       else if (iflg == 1) surf.closdinv = iclosd;
       if (ur_update_data_fixed(&surf) == 0) status = UU_SUCCESS;
       }

   uu_dexit;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : ncl_update_agsf_closed (key, lflg, iclosd);
**       Update the closed flag for an AG surface.
**    PARAMETERS
**       INPUT  :
**          nclkey               UNIBASE key of entity
**          iflg                 which flag
**          iclosd               flag value
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_update_agsf_closed (key, iflg, iclosd)
   UU_KEY_ID key;
   int iflg;
   int iclosd;

   {
   int status;
   struct UM_agsrf_rec surf;

   uu_denter(UU_MTRC,(us,"ncl_update_agsf_closed (key=%d, iflg=%d)", key, iflg));

   status = UU_FAILURE;
   surf.key = key;
   if (ur_retrieve_data_fixed(&surf) ==0)
       {
       if (iflg == 0) surf.closdinu = iclosd;
       else if (iflg == 1) surf.closdinv = iclosd;
       if (ur_update_data_fixed(&surf) == 0) status = UU_SUCCESS;
       }

   uu_dexit;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : ncl_update_rbsf_closed (key, lflg, iclosd);
**       Update the closed flag for an rb surface.
**    PARAMETERS
**       INPUT  :
**          nclkey               UNIBASE key of entity
**          iflg                 which flag
**          iclosd               flag value
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_update_rbsf_closed (key, iflg, iclosd)
   UU_KEY_ID key;
   int iflg;
   int iclosd;

   {
   int status;
   struct UM_rbsplsrf_rec surf;

   uu_denter(UU_MTRC,(us,"ncl_update_rbsf_closed (key=%d, iflg=%d)", key, iflg));

   status = UU_FAILURE;
   surf.key = key;
   if (ur_retrieve_data_fixed(&surf) ==0)
       {
       if (iflg == 0) surf.closdinu = iclosd;
       else if (iflg == 1) surf.closdinv = iclosd;
       if (ur_update_data_fixed(&surf) == 0) status = UU_SUCCESS;
       }

   uu_dexit;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : ncl_get_ncv_closed (key, iclosd);
**       Get the closed flag for a NCL curve.
**    PARAMETERS
**       INPUT  :
**          nclkey               UNIBASE key of entity
**       OUTPUT :
**          iclosd               Retrieved closed flag
**    RETURNS      :
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_get_ncv_closed (key, iclosd)
   UU_KEY_ID key;
   int *iclosd;

   {
   int status;
   struct NCL_curve_rec cv;

   uu_denter(UU_MTRC,(us,"ncl_get_ncv_closed (key=%d)", key));

   cv.key = key;
   status = UU_FAILURE;
   *iclosd = 0;
   if (ur_retrieve_data_fixed(&cv) == 0)
       {
       status = UU_SUCCESS;
       *iclosd = cv.closdinu;
       }

   uu_dexit;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : ncl_get_conic_closed (key, iclosd);
**       Return closed flag of 1 if this conic is an ellipse &
**       difference between t1 & t0 fields is close to 4. 
**    PARAMETERS
**       INPUT  :
**          nclkey               UNIBASE key of conic
**       OUTPUT :
**          iclosd               Returned closed flag
**    RETURNS      :
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_get_conic_closed (key, iclosd)
   UU_KEY_ID key;
   int *iclosd;

   {
   int status;
   struct UM_conic_rec cv;
   UU_REAL t;

   uu_denter(UU_MTRC,(us,"ncl_get_conic_closed (key=%d)", key));

   status = UU_FAILURE;
   *iclosd = 0;
   cv.key = key;
   if (ur_retrieve_data_fixed(&cv) == 0)
     {
     status = UU_SUCCESS;
     if (cv.type == UM_ELLIPSE)
       {
       t = cv.t1 - cv.t0;
       if (t < 0.0) t += 4.0;
       if (t > 3.995) *iclosd = 1;
       }
     }

   uu_dexit;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : ncl_get_cmp_closed (key, iclosd);
**       Get the closed flag for a composite curve.
**    PARAMETERS
**       INPUT  :
**          nclkey               UNIBASE key of entity
**       OUTPUT :
**          iclosd               Retrieved closed flag
**    RETURNS      :
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_get_cmp_closed (key, iclosd)
   UU_KEY_ID key;
   int *iclosd;

   {
   int status;
   struct UM_compcrv_rec cv;

   uu_denter(UU_MTRC,(us,"ncl_get_cmp_closed (key=%d)", key));

   cv.key = key;
   status = UU_FAILURE;
   *iclosd = 0;
   if (ur_retrieve_data_fixed(&cv) == 0)
       {
       status = UU_SUCCESS;
       *iclosd = cv.closdinu;
       }

   uu_dexit;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : ncl_get_agcv_closed (key, iclosd);
**       Get the closed flag for a AG curve.
**    PARAMETERS
**       INPUT  :
**          nclkey               UNIBASE key of entity
**       OUTPUT :
**          iclosd               Retrieved closed flag
**    RETURNS      :
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_get_agcv_closed (key, iclosd)
   UU_KEY_ID key;
   int *iclosd;

   {
   int status;
   struct UM_agcrv_rec cv;

   uu_denter(UU_MTRC,(us,"ncl_get_agcv_closed (key=%d)", key));

   cv.key = key;
   status = UU_FAILURE;
   *iclosd = 0;
   if (ur_retrieve_data_fixed(&cv) == 0)
       {
       status = UU_SUCCESS;
       *iclosd = cv.closdinu;
       }

   uu_dexit;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : ncl_get_rbcv_closed (key, iclosd);
**       Get the closed flag for a rb curve.
**    PARAMETERS
**       INPUT  :
**          nclkey               UNIBASE key of entity
**       OUTPUT :
**          iclosd               Retrieved closed flag
**    RETURNS      :
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_get_rbcv_closed (key, iclosd)
   UU_KEY_ID key;
   int *iclosd;

   {
   int status;
   struct UM_rbsplcrv_rec cv;

   uu_denter(UU_MTRC,(us,"ncl_get_rbcv_closed (key=%d)", key));

   cv.key = key;
   status = UU_FAILURE;
   *iclosd = 0;
   if (ur_retrieve_data_fixed(&cv) == 0)
       {
       status = UU_SUCCESS;
       *iclosd = cv.closdinu;
       }

   uu_dexit;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : ncl_get_rbcv_closed (key, iclosd);
**       Get the closed flag for a rb curve.
**    PARAMETERS
**       INPUT  :
**          nclkey               UNIBASE key of entity
**       OUTPUT :
**          iclosd               Retrieved closed flag
**    RETURNS      :
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_get_polyln_closed (key, iclosd)
UU_KEY_ID key;
int *iclosd;
{
   int status;
   int no_pt;
   struct UM_polyline_rec cv;
   UM_coord *pt;
   UM_real8 tol;

   uu_denter(UU_MTRC,(us,"ncl_get_rbcv_closed (key=%d)", key));

   cv.key = key;
   status = UU_FAILURE;
   *iclosd = 0;
   if (ncl_retrieve_data(&cv, sizeof(struct UM_polyline_rec)) == UU_SUCCESS)
   {
	   gettol(&tol);
       status = UU_SUCCESS;
	   pt = (UM_coord *) cv.pt;
	   no_pt = cv.no_pt;
       *iclosd = (UM_SQDIS(pt[0],pt[no_pt-1]) < tol*tol);
   }

   uu_dexit;
   return (status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_get_nsf_closed (key, lflg, iclosd);
**       Get the closed flag for a NCL surface.
**    PARAMETERS
**       INPUT  :
**          nclkey               UNIBASE key of entity
**          iflg                 which flag value
**       OUTPUT :
**          iclosd               flag value
**    RETURNS      :
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_get_nsf_closed (key, iflg, iclosd)
   UU_KEY_ID key;
   int iflg;
   int *iclosd;

   {
   int status;
   struct NCL_surface_rec surf;

   uu_denter(UU_MTRC,(us,"ncl_get_nsf_closed (key=%d)", key));

   status = UU_FAILURE;
   *iclosd = 0;
   surf.key = key;
   if (ur_retrieve_data_fixed(&surf) ==0)
       {
       if (iflg == 0) *iclosd = surf.closdinu;
       else if (iflg == 1) *iclosd = surf.closdinv;
       status = UU_SUCCESS;
       }

   uu_dexit;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : ncl_get_revsf_closed (key, lflg, iclosd);
**       Get the closed flag for a NCL surface of revolution.
**    PARAMETERS
**       INPUT  :
**          nclkey               UNIBASE key of entity
**          iflg                 which flag value
**       OUTPUT :
**          iclosd               flag value
**    RETURNS      :
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_get_revsf_closed (key, iflg, iclosd)
   UU_KEY_ID key;
   int iflg;
   int *iclosd;

   {
   int status;
   struct NCL_revsurf_rec surf;

   uu_denter(UU_MTRC,(us,"ncl_get_nsf_closed (key=%d)", key));

   status = UU_FAILURE;
   *iclosd = 0;
   surf.key = key;
   if (ur_retrieve_data_fixed(&surf) ==0)
       {
       if (iflg == 0) *iclosd = surf.closdinu;
       else if (iflg == 1) *iclosd = surf.closdinv;
       status = UU_SUCCESS;
       }

   uu_dexit;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : ncl_get_msf_closed (key, lflg, iclosd);
**       Get the closed flag for a NCL surface.
**    PARAMETERS
**       INPUT  :
**          nclkey               UNIBASE key of entity
**          iflg                 which flag value
**       OUTPUT :
**          iclosd               flag value
**    RETURNS      :
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_get_msf_closed (key, iflg, iclosd)
   UU_KEY_ID key;
   int iflg;
   int *iclosd;

   {
   int status;
   struct NCL_meshsf_rec surf;

   uu_denter(UU_MTRC,(us,"ncl_get_msf_closed (key=%d)", key));

   status = UU_FAILURE;
   *iclosd = 0;
   surf.key = key;
   if (ur_retrieve_data_fixed(&surf) ==0)
       {
       if (iflg == 0) *iclosd = surf.closdinu;
       else if (iflg == 1) *iclosd = surf.closdinv;
       status = UU_SUCCESS;
       }

   uu_dexit;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : ncl_get_agsf_closed (key, lflg, iclosd);
**       Get the closed flag for an AG surface.
**    PARAMETERS
**       INPUT  :
**          nclkey               UNIBASE key of entity
**          iflg                 which flag value
**       OUTPUT :
**          iclosd               flag value
**    RETURNS      :
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_get_agsf_closed (key, iflg, iclosd)
   UU_KEY_ID key;
   int iflg;
   int *iclosd;

   {
   int status;
   struct UM_agsrf_rec surf;

   uu_denter(UU_MTRC,(us,"ncl_get_agsf_closed (key=%d)", key));

   status = UU_FAILURE;
   *iclosd = 0;
   surf.key = key;
   if (ur_retrieve_data_fixed(&surf) ==0)
       {
       if (iflg == 0) *iclosd = surf.closdinu;
       else if (iflg == 1) *iclosd = surf.closdinv;
       status = UU_SUCCESS;
       }

   uu_dexit;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : ncl_get_rbsf_closed (key, lflg, iclosd);
**       Get the closed flag for an rb surface.
**    PARAMETERS
**       INPUT  :
**          nclkey               UNIBASE key of entity
**          iflg                 which flag value
**       OUTPUT :
**          iclosd               flag value
**    RETURNS      :
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_get_rbsf_closed (key, iflg, iclosd)
   UU_KEY_ID key;
   int iflg;
   int *iclosd;

   {
   int status;
   struct UM_rbsplsrf_rec surf;

   uu_denter(UU_MTRC,(us,"ncl_get_rbsf_closed (key=%d)", key));

   status = UU_FAILURE;
   *iclosd = 0;
   surf.key = key;
   if (ur_retrieve_data_fixed(&surf) ==0)
       {
       if (iflg == 0) *iclosd = surf.closdinu;
       else if (iflg == 1) *iclosd = surf.closdinv;
       status = UU_SUCCESS;
       }

   uu_dexit;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : int ncl_get_closdinuv (sfkey,closdinu,closdinv)
**			determines if surface is closed in u/v
**    PARAMETERS
**       INPUT  :
**          sfkey      - surface key
**       OUTPUT :
**          closdinu,closdinv
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_get_closdinuv (sfkey,uclosed,vclosed)
UU_KEY_ID sfkey;
int *uclosed,*vclosed;
{
   struct NCL_fixed_databag sf;
   int status;

	*uclosed = *vclosed = 0;
   sf.key = sfkey;
   status = ncl_retrieve_data_fixed (&sf);

   if (status == 0)
   {
      if (sf.rel_num == NCL_TRIMSF_REL)
      {
			struct NCL_trimsf_rec *e;
			e = (struct NCL_trimsf_rec *) &sf;
			status = ncl_get_closdinuv (e->bs_key,uclosed,vclosed);
      }
      else if (sf.rel_num == UM_RBSPLSRF_REL)
      {
			struct UM_rbsplsrf_rec *e;
			e = (struct UM_rbsplsrf_rec *) &sf;
			*uclosed = e->closdinu;
			*vclosed = e->closdinv;
		}
      else if (sf.rel_num == NCL_REVSURF_REL)
      {
			struct NCL_revsurf_rec *e;
			e = (struct NCL_revsurf_rec *) &sf;
			*uclosed = e->closdinu;
			*vclosed = e->closdinv;
		}
		else
			status = UU_FAILURE;
   }
	return (status);
}
