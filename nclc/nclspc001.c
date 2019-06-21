/*********************************************************************
**    NAME         :  nclspc001.c
**       CONTAINS: C-routines used in NCL and IGES. 
**                 Here are data retrieval routines for mapping UNIBASE entities
**                 to the data representation for NCL; these routines
**                 are FORTRAN callable and hence follow the standard
**                 FORTRAN/C interface conventions:
**
**       gtspa1 (nclkey, ispan, buf, pankey)
**       gtppat (nclkey, ipatch, buf)
**       ncl_get_srfpanel (panel, srfpanel)
**       ncl_get_srfpatch (paneltype, patch, srfpatch)
**       ncl_get_srfheader (surf, buf)
**       ncl_presrf (ncrvs, crvp, slist)
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       nclspc001.c , 25.1
**     DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:22
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "class.h"
#include "mfort.h"
#include "mdrel.h"
#include "mdcoord.h"
#include "msrf.h"

#include "nccs.h"
#include "ncl.h"
#include "nclfc.h"
#include "ulist.h"

int sbsolv_insert = 0;

/*********************************************************************
**    E_FUNCTION     : int gtspa1(nclkey, ispan, buf, pankey)
**       Retrieve the panel header data for the specified span (ISPAN)
**       of the surface (NCLKEY).
**    PARAMETERS
**       INPUT  :
**          nclkey            UNIBASE key of the surface entity
**          ispan             span to get panel header
**       OUTPUT :
**          buf               buffer to place the data
**          pankey            UNIBASE key of the panel
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
gtspa1(nclkey, ispan, buf, pankey)
   UU_KEY_ID *nclkey;
   UM_int2  *ispan;
   UM_real8 buf[];
   UU_KEY_ID *pankey;

   {
   int status;
   int span;
   int rel_num;
   UU_KEY_ID key;
   UU_KEY_ID panelkey;
   struct NCL_panel_rec panel;

   uu_denter(UU_MTRC,(us,"gtspa1(nclkey=%x, ispan=%d, buf=%x)",
      *nclkey, *ispan, buf));
   key = *nclkey;
   span = *ispan;
   if (ur_retrieve_data_relnum(key, &rel_num) != 0)
      status = UU_FAILURE;
   else if (rel_num != NCL_SURF_REL && rel_num != NCL_NETSF_REL)
      status = UU_FAILURE;
   else
      {
      if (ur_retrieve_data_varlist(key, 1, &panelkey, span, 1) != 0)
         status = UU_FAILURE;
      else
         {
         panel.key = panelkey;
         if (ur_retrieve_data_fixed(&panel) != 0)
            status = UU_FAILURE;
         else
            {
            ncl_get_srfpanel(&panel, buf);
            *pankey = panel.key;
            status = UU_SUCCESS;
            }
         }
      }
   if (status != UU_SUCCESS) uu_dprint(-1,(us,"gtspa1 returned FAILURE"));
   uu_dexit;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : int gtppat(nclkey, ipatch, buf)
**       Retrieve the data defining a patch (IPATCH) within a
**       span (ISPAN) of an NCL surface entity.
**    PARAMETERS
**       INPUT  :
**          nclkey            UNIBASE key of the entity
**          ipatch            patch within span
**       OUTPUT :
**          buf               buffer to place the data
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
gtppat(nclkey, ipatch, buf)
   UU_KEY_ID *nclkey;
   UM_int2  *ipatch;
   UM_real8 buf[];

   {
   int status;
   int patch;
   struct NCL_panel_rec panel;
   struct NCL_patch_rec patchdata;

   uu_denter(UU_MTRC,(us,"gtppat(nclkey=%x, ipatch=%d, buf=%x)",
      *nclkey, *ipatch, buf));
   panel.key = *nclkey;
   patch = *ipatch;
   if(ur_retrieve_data_fixed(&panel) != 0)
      status = UU_FAILURE;
   else if (panel.rel_num != NCL_PANEL_REL)
      status = UU_FAILURE;
   else
      {
      if (ur_retrieve_data_varlist(panel.key, 1, &patchdata, patch, 1)
           != 0) status = UU_FAILURE;
      else
         {
         ncl_get_srfpatch(panel.type, &patchdata, buf);
         status = UU_SUCCESS;
         }
      }
   if (status != UU_SUCCESS) uu_dprint(-1,(us,"gtppat returned FAILURE"));
   uu_dexit;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : ncl_get_srfpanel(panel, srfpanel)
**       Move the data for a surface panel into an NCLI
**       surface panel structure.
**    PARAMETERS   
**       INPUT  : 
**          panel             panel record
**       OUTPUT :  
**          srfpanel          NCLI representation of a panel
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_get_srfpanel(panel, srfpanel)
   struct NCL_panel_rec *panel;
   struct NCLI_srfpanel_rec *srfpanel;

   {
   int i;

   uu_denter(UU_MTRC,(us,"ncl_get_srfpanel(panel=%x, srfpanel=%x)",
      panel, srfpanel));
   srfpanel->paneltype = panel->type;
   srfpanel->numpatch = panel->no_patch - 1;
   for (i=0; i<panel->no_patch; i++)
      srfpanel->param[i] = panel->param[i];
   uu_dexit;

	return (0);
   }

/*********************************************************************
**    E_FUNCTION     : ncl_get_srfpatch(paneltype, patch, srfpatch)
**       Move a surface patch into an NCLI internal structure.
**    PARAMETERS   
**       INPUT  : 
**          paneltype         0 => 8 point patch
**                            1 => 4 point patch
**          patch             surface patch
**       OUTPUT :  
**          sfrpatch          NCLI representation of a surface
**                            patch
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_get_srfpatch(paneltype, patch, srfpatch)
   int paneltype;
   struct NCL_patch_rec *patch;
   struct NCLI_srfpatch_rec *srfpatch;

   {
   int i,j,k;

   uu_denter(UU_MTRC,(us,"ncl_get_srfpatch(panel_type=%d,patch=%x,srfpatch=%x)",                      paneltype, patch, srfpatch));
   ncl_uureal_to_real8(3, patch->pt, srfpatch->point);
   if (paneltype == 0) k = 7; else k = 3;
   for (j=0; j<k; j++) 
      for (i=0; i<3; i++) srfpatch->delta[j][i] = patch->delta[j][i];
   srfpatch->delta[k][0] = patch->rho;
   uu_dexit;

	return (0);
   }

/*********************************************************************
**    E_FUNCTION     : ncl_get_srfheader(surf, buf)
**       Move the data for a surface header into an NCLI
**       surface header structure.
**    PARAMETERS   
**       INPUT  : 
**          surf              surface record
**       OUTPUT :  
**          buf               NCLI representation of a surface header
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_get_srfheader(surf, buf)
   struct NCL_surface_rec *surf;
   struct NCLI_srfhead_rec *buf;

   {
	struct UM_surfattr_rec attr;

   uu_denter(UU_MTRC,(us,"ncl_get_srfheader(surf=%x, buf=%x)",
      surf, buf));
	attr.key = surf->key;
	ur_retrieve_attr(&attr);
   buf->surftype = surf->surf_type;
   buf->numpanels = surf->no_panelkey;
   buf->offset = surf->offset;
   buf->offdist = surf->offdist;
   /*  Added U and V values to the surface header in unibase. */
   buf->numupaths = attr.numupaths;
   buf->numvpaths = attr.numvpaths;
   buf->ptsperucrv = attr.ptsperucrv;
   buf->ptspervcrv = attr.ptspervcrv;
   uu_dexit;

	return (0);
   }
/*********************************************************************
**    E_FUNCTION     : ncl_presrf (ncrvs, crvp, keyslp)
**       This routine is temporary source to be used in in both
**       NCL and IGES when creating Bsplane srfs.
**    PARAMETERS   
**       INPUT  : 
**          ncrvs      - Number of curves.
**          crvp       - Array of pointers to curves.
**          keyslp     - Array of keys of slope control geometry.
**       OUTPUT :  
**          sfp        - Pointer to created surface.
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_presrf (ncrvs, crvp, tfa, slist)
   int ncrvs;
   struct NCL_fixed_databag *crvp;
   UM_transf *tfa;
   UU_LIST *slist;
   {
   int i, status;
   int ns;
   UU_REAL sa,sb,sb1;
   struct NCL_fixed_databag *cvp;
   UM_transf *tfp;

   UU_REAL del,ds,dsmax,dsmin;

   uu_denter(UU_MTRC,(us,"ncl_presrf ()"));

   status = UU_SUCCESS;
   uu_list_init (slist, sizeof(UU_REAL), 20, 20);
   sa = sb = 0.0;
   uu_list_push (slist, &sb);
   ns = 1;
   if (tfa == 0) status = UU_FAILURE;
   cvp = crvp;
   tfp = tfa;
   for (i=0; i<ncrvs && status == UU_SUCCESS; i++)
   {
     status = uc_retrieve_transf (cvp->key, tfp);
     cvp++;
     tfp++;
   }
		if (sbsolv_insert > 0) del = 1./sbsolv_insert;
		dsmax = del/50;
		dsmin = del/1000;

   while (sb < .99999 && status == UU_SUCCESS)
   {
     cvp = crvp;
     tfp = tfa;
     status = ncl_sbsolv (1, sa, &sb, cvp, tfp);
 		
		if (sbsolv_insert > 0)
		{
			ds = sb - sa;

			if (ds > dsmin)
			{
				if (ds > dsmax)
					ds = dsmax;
				else if (ds < dsmin)
					ds = dsmin;
					
				sb = sa + ds;
			}
		}

     sb1 = sb;
     for (i=1; i<ncrvs && status == UU_SUCCESS; i++)
     {
       cvp++;
       tfp++;
       status = ncl_sbsolv (2, sa, &sb, cvp, tfp);
       if (sb1>sb) sb1 = sb;
     }
/*
.....Depending on the curves creating the surfaces, ncl_sbsolv may just
.....continually return sb =1.  This can result in the interior of a surface
.....being out of tolerance. We want slist to contain at least the values 0,.5,1.0
.....These values will be used as the u-vaules in evaluating the curves and creating
.....the slope vectors of the surface.  JLS 6/22/99
*/
	  if(ns<4 && sb1 > sa +.5) sb1 = sa + .5;

     uu_list_push (slist, &sb1);
     sa = sb = sb1;
     ns++;
   }
   uu_dexitstatus("ncl_presrf ()", status);
   return(status);
  }
