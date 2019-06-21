/*********************************************************************
**    NAME         :  nesurf.c
**       CONTAINS: routines to handle general NCL surfaces.
**
**       int ncl_get_srfspan
**       ncl_put_srfpanel
**       ncl_put_srfpatch
**       ncl_p_srfhead
**       ncl_p_srfpanel
**       ncl_p_srfpatch
**       ncl_p81_surf
**       ncl_p82_panel
**       int ncl_srfpanelsize
**       int ncl_srfpatchsize
**       ncl_p_surf
**       int ncl_class_copy_nsf
**       int ncl_copy_nsf
**       int ncl_transform_nsf
**       int ncl_transf_npatch
**       void sfoutj
**
**    COPYRIGHT 1984 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       nesf.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:47
*********************************************************************/

#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "mfort.h"
#include "mdrel.h"
#include "mcrv.h"
#include "mdattr.h"
#include "mdebug.h"
#include "mdeval.h"
#include "mdgenent.h"
#include "modef.h"
#include "msrf.h"
#include "msol.h"
#include "uminmax.h"

#include "nccs.h"
#include "ncl.h"
#include "nclfc.h"
#include "nclxmdl.h"
#include "nconst.h"

extern int NCLX_internal_geom;
extern int NCL_multi_ent;
extern int NCL_ubcopy;

/*********************************************************************
**    E_FUNCTION     : int ncl_get_srfspan(panel, buf)
**       Retrieve a "span" of a general NCL surface (i.e. move
**       all of the patches for a specific panel of a surface).
**    PARAMETERS   
**       INPUT  : 
**          panel             surface panel
**       OUTPUT :  
**          buf               NCLI representation of a surface
**                            span
**    RETURNS      : 
**       UU_SUCCESS iff no error
**       UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_get_srfspan(panel, buf)
   struct NCL_panel_rec *panel;
   UM_real8 buf[];

   {
   int status;
   struct NCLI_srfpanel_rec *srfpanel;
   int panelsize;
   int patchsize;
   int i;

   uu_denter(UU_MTRC,(us,"ncl_get_srfpanel(panel=%x, buf=%x)",
      panel->key, buf));

   /* move the surface panel header */
   srfpanel = (struct NCLI_srfpanel_rec *) buf;
   ncl_get_srfpanel(panel, srfpanel);

   /* move each of the surface patches 

   panelsize = ncl_srfpanelsize(srfpanel);
   patchsize = ncl_srfpatchsize(srfpanel->paneltype);
   srfpanel = (struct NCLI_srfpanel_rec *) (buf + panelsize);
   for (i=0; i<panel->no_patch; i++)
      {
      ncl_get_srfpatch(panel->type, &panel->patch[i], srfpanel);
      srfpanel = (struct NCLI_srfpanel_rec *) (srfpanel + patchsize);
      }

   */
   status = UU_SUCCESS;
   uu_dexit;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : ncl_put_srfpanel(srfpanel, panel)
**       Move the data from an NCLI surface panel into a 
**       surface panel structure.
**    PARAMETERS   
**       INPUT  : 
**          srfpanel          NCLI representation of a panel
**       OUTPUT :  
**          panel             panel record
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_put_srfpanel(srfpanel, panel)
   struct NCLI_srfpanel_rec *srfpanel;
   struct NCL_panel_rec *panel;

   {
   int i;

   uu_denter(UU_MTRC,(us,"ncl_put_srfpanel(srfpanel=%x, panel=%x)",
      srfpanel, panel));
   panel->type = srfpanel->paneltype;
   panel->no_param = srfpanel->numpatch;
   panel->no_patch = srfpanel->numpatch + 1;
   for (i=0; i<srfpanel->numpatch; i++)
      panel->param[i] = srfpanel->param[i];
/* Put 1. in last param to avoid illegal instruction fault on Vax */
   panel->param[i] = 1.0;
   /*MILLS: initialize remaining fields to 0. */
   for (i++; i<50; i++)
      panel->param[i] = 0.0;

   uu_dexit;
   }

/*********************************************************************
**    E_FUNCTION     : ncl_put_srfpatch(paneltype, srfpatch, patch)
**       Move an NCLI surface patch into a surface patch.
**    PARAMETERS   
**       INPUT  : 
**          paneltype         0 => 28 point patch
**                            1 => 14 point patch
**          sfrpatch          NCLI representation of a surface
**                            patch
**       OUTPUT :  
**          patch             surface patch
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_put_srfpatch(paneltype, srfpatch, patch)
   int paneltype;
   struct NCLI_srfpatch_rec *srfpatch;
   struct NCL_patch_rec *patch;

   {
   int i,j,k;

   uu_denter(UU_MTRC,(us,"ncl_put_srfpatch(paneltype = %d,srfpatch=%x,patch=%x)", paneltype, srfpatch, patch));
   ncl_uureal_to_real8(3, srfpatch->point, patch->pt);
   if (paneltype == 0) k = 7; else k = 3;
   for (j=0; j<k; j++) 
      for (i=0; i<3; i++) patch->delta[j][i] = srfpatch->delta[j][i];
   /*MILLS: to initialize fields to zero. */
   for (;j < 7; j++)
      for (i=0; i<3; i++) patch->delta[j][i] = 0.0;
   patch->rho = srfpatch->delta[k][0];
   uu_dexit;
   }

/*********************************************************************
**    E_FUNCTION     : ncl_p_srfhead(srfhead)
**       Print an NCLI surface header record.
**    PARAMETERS   
**       INPUT  : 
**          srfhead           NCLI surface header record
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_p_srfhead(srfhead)
   struct NCLI_srfhead_rec *srfhead;

   {
   uu_denter(UU_MTRC,(us,"ncl_p_srfhead(srfhead=%x)",srfhead));

   um_pscroll("NCLI_srfhead_rec");
   sprintf(UM_sbuf,"  surftype=%d", srfhead->surftype);
   um_pscroll(UM_sbuf);

   uu_dexit;
   }

/*********************************************************************
**    E_FUNCTION     : ncl_p_srfpanel(srfpanel)
**       Print an NCLI surface panel record.
**    PARAMETERS   
**       INPUT  : 
**          srfpanel          NCLI surface panel record
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_p_srfpanel(srfpanel)
   struct NCLI_srfpanel_rec *srfpanel;

   {
   int i;

   uu_denter(UU_MTRC,(us,"ncl_p_srfpanel(srfpanel=%x)",srfpanel));

   um_pscroll("NCLI_srfpanel_rec");
   sprintf(UM_sbuf,"  panel type = %d", srfpanel->paneltype);
   um_pscroll(UM_sbuf);
   sprintf(UM_sbuf,"  number patches = %d", srfpanel->numpatch);
   um_pscroll(UM_sbuf);
   for (i=0; i<srfpanel->numpatch; i++)
      {
      sprintf(UM_sbuf,"  param[%d] = %g", i, srfpanel->param[i]);
      um_pscroll(UM_sbuf);
      }

   uu_dexit;
   }

/*********************************************************************
**    E_FUNCTION     : ncl_p_srfpatch(num, srfpatch)
**       Print an NCLI surface patch record.
**    PARAMETERS   
**       INPUT  : 
**          srfpatch          NCLI surface patch record
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_p_srfpatch(num, srfpatch)
   int num;
   struct NCLI_srfpatch_rec *srfpatch;

   {
   int i;

   uu_denter(UU_MTRC,(us,"ncl_p_srfpatch(srfpatch=%x)",srfpatch));

   um_pscroll("NCLI_srfpatch_rec");
   sprintf(UM_sbuf, "  point = (%g,%g,%g)", srfpatch->point[0],
      srfpatch->point[1], srfpatch->point[2]);
   um_pscroll(UM_sbuf);
   for (i=0; i<num; i++)
      {
      sprintf(UM_sbuf, "  delta[%d] = (%g,%g,%g)", i, srfpatch->delta[i][0],
         srfpatch->delta[i][1], srfpatch->delta[i][2]);
      um_pscroll(UM_sbuf);
      }
   sprintf(UM_sbuf,"   rho = %g", srfpatch->delta[num][0]);
   um_pscroll(UM_sbuf);

   uu_dexit;
   }

/*********************************************************************
**    E_FUNCTION     : ncl_p81_surf(surf)
**       Print an NCL general surface record.
**    PARAMETERS   
**       INPUT  : 
**          surf                 an NCL surface record (UNIBASE format)
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_p81_surf(surf)
   struct NCL_surface_rec *surf;

   {
   uu_denter(UU_MTRC,(us,"ncl_p81_surf(key=%x)", surf->key));

   sprintf(UM_sbuf,"NCL SURFACE: %x", surf->key);
   um_pscroll(UM_sbuf);

   sprintf(UM_sbuf, "label %s",surf->label);
   um_pscroll(UM_sbuf);

   um_p_ary(UM_PINT,"surf_type", 1, &surf->surf_type);
   um_p_ary(UM_PHEX,"panelkey", surf->no_panelkey, surf->panelkey);

   uu_dexit;
   }

/*********************************************************************
**    E_FUNCTION     : ncl_p82_panel(panel)
**       Print a UNIBASE panel record.
**    PARAMETERS   
**       INPUT  : 
**          panel                UNIBASE panel record
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_p82_panel(panel)
   struct NCL_panel_rec *panel;

   {
   int i,j;

   uu_denter(UU_MTRC,(us,"ncl_p82_panel(key=%x)",panel->key));

   sprintf(UM_sbuf,"NCL panel: %x", panel->key);
   um_pscroll(UM_sbuf);
   um_p_ary(UM_PINT, "type", 1, &panel->type);
   um_pscroll("");
   for (i=0; i<panel->no_patch; i++)
      {
      sprintf(UM_sbuf,"patch %d", i);
      um_pscroll(UM_sbuf);
      sprintf(UM_sbuf,"param %g", panel->param[i]);
      um_pscroll(UM_sbuf);
      sprintf(UM_sbuf,"point=(%g,%g,%g)", panel->patch[i].pt[0],
         panel->patch[i].pt[1], panel->patch[i].pt[2]);
      um_pscroll(UM_sbuf);
      for (j=0; j<28; j++)
         {
         sprintf(UM_sbuf,"delta=(%g,%g,%g)", panel->patch[i].delta[j][0],
            panel->patch[i].delta[j][1], panel->patch[i].delta[j][2]);
         um_pscroll(UM_sbuf);
         }
      um_pscroll("");
      }
   uu_dexit;
   }

/*********************************************************************
**    E_FUNCTION     : int ncl_srfpanelsize(srfpanel)
**       Calculate the size (in UM_real8) of a panel record
**    PARAMETERS   
**       INPUT  : 
**          srfpanel             NCLI surface panel record
**       OUTPUT :  
**          none
**    RETURNS      : 
**       actual size of surface panel in UM_real8
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_srfpanelsize(srfpanel)
   struct NCLI_srfpanel_rec *srfpanel;

   {
   int srfpanelsize;
   int numpatch;

   uu_denter(UU_MTRC,(us,"ncl_srfpanelsize(srfpanel=%x)", srfpanel));
   numpatch = srfpanel->numpatch;
   srfpanelsize = 8 * sizeof(UM_int2) + (numpatch * sizeof(UM_real4));
   /*um_p_ary(UM_PINT, "byte size", 1, &srfpanelsize);*/
   srfpanelsize = srfpanelsize / sizeof(UM_real8);
   /*um_p_ary(UM_PINT, "real8 size", 1, &srfpanelsize);*/
   if (numpatch & 1) srfpanelsize++;
   /*um_p_ary(UM_PINT, "upreal8 size", 1, &srfpanelsize);*/
   uu_dexit;
   return (srfpanelsize);
   }

/*********************************************************************
**    E_FUNCTION     : int ncl_srfpatchsize(paneltype)
**       Calculate the size (in UM_real8) of a patch record
**    PARAMETERS   
**       INPUT  : 
**          paneltype            0 => 28 point patch
**                               1 => 14 point patch
**       OUTPUT :  
**          none
**    RETURNS      : 
**       actual size of surface patch in UM_real8
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_srfpatchsize(paneltype)
   int paneltype;

   {
   int srfpatchsize;
   int k;

   uu_denter(UU_MTRC,(us,"ncl_srfpatchsize(paneltype=%d)", paneltype));
   if (paneltype == 0) k = 14; else k = 8;
   srfpatchsize = k * sizeof(UM_real8) ;
   /*um_p_ary(UM_PINT, "byte size", 1, &srfpatchsize);*/
   srfpatchsize = srfpatchsize / sizeof(UM_real8);
   /*um_p_ary(UM_PINT, "real8 size", 1, &srfpatchsize);*/
   if (srfpatchsize & 1) srfpatchsize++;
   /*um_p_ary(UM_PINT, "upreal8 size", 1, &srfpatchsize);*/
   uu_dexit;
   return (srfpatchsize);
   }

/*********************************************************************
**    E_FUNCTION     : ncl_p_surf(srfkey)
**       Print all of the data for an NCL surface .
**    PARAMETERS   
**       INPUT  : 
**          srfkey            NCL surface key
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_p_surf(srfkey)

   UU_KEY_ID srfkey;
   {
   int status;
   int rel_num;
   struct NCL_surface_rec surf;
   UU_KEY_ID panelkey[100];
   struct NCL_panel_rec panel;
   struct NCLI_srfpanel_rec *srfpanel;
   struct NCLI_srfpatch_rec *srfpatch;
   int panelsize;
   int patchsize;
   int i;

   uu_denter(UU_MTRC,(us,"ncl_p_surf(srfkey=%x)",srfkey));
   status = UU_SUCCESS;
   if (ur_retrieve_data_relnum(srfkey, &rel_num) != 0)
      status = UU_FAILURE;
   else if (rel_num != NCL_SURF_REL)
      status = UU_FAILURE;
   else
      {
      surf.key = srfkey;
      if (ur_retrieve_data_fixed(&surf) != 0)
         status = UU_FAILURE;
      else
         {
         ur_retrieve_data_varlist(surf.key, 1, panelkey, 1, surf.no_panelkey);
         um_pscroll("NCL_srfhead_rec");
         sprintf(UM_sbuf,"  surftype=%d", surf.surf_type);
         um_pscroll(UM_sbuf);
         for(i=0;i<surf.no_panelkey;i++)
            {
            sprintf(UM_sbuf,"  indx = %d, key = %x",i,panelkey[i]);
            um_pscroll(UM_sbuf);
            }
         }
      }

   uu_dexit;
   }
/*********************************************************************
**    E_FUNCTION     : int ncl_class_copy_nsf (e1, e2, bagsize)
**       Copy an NCL surface with updated label & store in ranfile.
**    PARAMETERS   
**       INPUT  : 
**          e1         - pointer to surface to copy
**          bagsize    - size of surface record.
**       OUTPUT :  
**          e2         - pointer to copied surface.
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_class_copy_nsf (e1, e2, bagsize)
   struct NCL_surface_rec *e1, *e2;
   int bagsize;

   {
   int status;
   UM_int2 nclstatus;

   uu_denter(UU_MTRC,(us,"ncl_class_copy_nsf(key=%x)", e1->key));

   status = ncl_label_wf(NCL_SURF_REL, e2->label, &e2->subscr, 0,
                         &nclstatus);
   if (status == UU_SUCCESS) status = ncl_copy_nsf(e1, e2);
   if (status == UU_SUCCESS) status = ncl_store_wf1(e2->key);

   uu_dexit;
   return (status);
   }
/*********************************************************************
**    E_FUNCTION     : int ncl_copy_nsf (e1, e2)
**       Copy an NCL surface.
**    PARAMETERS   
**       INPUT  : 
**          e1         - pointer to surface to copy
**       OUTPUT :  
**          e2         - pointer to copied surface.
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_copy_nsf (e1, e2)
   struct NCL_surface_rec *e1, *e2;

   {
   int ipan, ipat, npats, isize, status;
   struct NCL_panel_rec panel;
   struct NCL_patch_rec patch;
   UU_KEY_ID key;

   uu_denter(UU_MTRC,(us,"ncl_copy_nsf(key=%x)", e1->key));

   e2->key = e1->key;
   status = ncl_retrieve_data_fixed (e2);;
/*
...vp 10/9/97 currently surface is copied without curves
*/
   if (status == UU_SUCCESS)
   {
		ncl_reset_assoc (e2);
		e2->key = 0;
		e2->no_panelkey = 0;
		status = ncl_create_entity (e2, 9);
   }

   for (ipan=1; ipan<=e1->no_panelkey && status == UU_SUCCESS; ipan++)
     {
     if (ur_retrieve_data_varlist (e1->key, 1, &panel.key, ipan, 1) != 0)
        status = UU_FAILURE;
     else if (ur_retrieve_data_fixed(&panel) != 0)
        status = UU_FAILURE;
     else 
       {
       key = panel.key;
       npats = panel.no_patch;
       panel.key = 0;
       panel.no_patch = 0;
       if (ur_create_data(&panel) != 0) status = UU_FAILURE;
       for (ipat=1; ipat<=npats && status == UU_SUCCESS; ipat++)
         {
         if (ur_retrieve_data_varlist (key, 1, &patch, ipat, 1) != 0)
           status = UU_FAILURE;
         else if (ur_update_data_varlist (panel.key, 1, &patch, ipat, 1) != 0)
           status = UU_FAILURE;
         }
       if (status == UU_SUCCESS)
         if (ur_update_data_varlist (e2->key, 1, &panel.key, ipan, 1) != 0)
           status = UU_FAILURE;
       }
     }
     if (status == UU_SUCCESS) status = ncl_retrieve_data_fixed (e2);

   uu_dexit;
   return (status);
   }
/*********************************************************************
**    E_FUNCTION     : int ncl_transform_nsf (e1, tfmat, store)
**       Transform an NCL surface.
**    PARAMETERS   
**       INPUT  : 
**          e1         - pointer to surface to transform
**          tfmat      - transformation matrix.
**          store      - TRUE if transformed entity is to be stored.
**       OUTPUT :  
**          e1         - pointer to transformed surface.
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : Probably does nothing if store is false.
*********************************************************************/
int
ncl_transform_nsf (e1, tfmat, store)
   struct NCL_surface_rec *e1;
   UM_transf tfmat;
   UU_LOGICAL store;

   {
   int ipan, ipat, npats, type, status;
   struct NCL_panel_rec panel;
   struct NCL_patch_rec patch;
   UU_KEY_ID key;
   UU_REAL det,dist;
	void ncl_conv_sfseg_reset();
	
   uu_denter(UU_MTRC,(us,"ncl_transform_nsf(key=%x)", e1->key));

   status = UU_SUCCESS;

/*
...fix surface offset: the previous version did it incorrectly, we put a change 
... with the version flag. Eduard 062100.
*/
	dist = e1->offdist;
	if (dist != 0.0)
	{
		UM_real8 ver;
		UM_int2 idx;

		um_determinant (tfmat,&det);
		idx = 169;
		getsc (&idx, &ver);
		if (ver < 9.149)
			e1->offdist = (det > 0.0)? dist: -dist;
		else
		{
			int neg = 0;
			if (det < 0.0)
			{
				neg = 1;
				det = -det;
			}
			if (det != 1.0) det = pow (det,1./3.);
			if (neg) det = -det;
			e1->offdist = dist * det;
		}
		if (store) ur_update_data_fixed (e1);
	}

	if (e1->primitive >= NCLSF_PLANE)
	{
		ncl_transform_primdat (&e1->primitive,e1->prim_param,tfmat);
		if (store) ur_update_data_fixed (e1);
	}

   for (ipan=1; ipan<=e1->no_panelkey && status == UU_SUCCESS; ipan++)
     {
     if (ur_retrieve_data_varlist (e1->key, 1, &panel.key, ipan, 1) != 0)
        status = UU_FAILURE;
     else if (ur_retrieve_data_fixed(&panel) != 0)
        status = UU_FAILURE;
     else 
       {
       key = panel.key;
       npats = panel.no_patch;
       type = panel.type;
       for (ipat=1; ipat<=npats && status == UU_SUCCESS; ipat++)
         {
         if (ur_retrieve_data_varlist (key, 1, &patch, ipat, 1) != 0)
           status = UU_FAILURE;
         else status = ncl_transf_npatch (&patch, tfmat, type);
         if (status == UU_SUCCESS && store)
           if (ur_update_data_varlist (key, 1, &patch, ipat, 1) != 0)
             status = UU_FAILURE;
         }
       }
     }
/*
.....changed because we may have tesslst that need delete too
.....Yurong 1/28/99
*/
/*   if (status == UU_SUCCESS) ncl_displst_delete(&e1->key);   */
	if (status == UU_SUCCESS)
	{
		if (e1->no_tesslst!=0)
			ncl_lst_delete(TESSELLATION_LIST,&e1->key);
		if (e1->no_displst!=0)
			ncl_lst_delete(DISPLAY_LIST,&e1->key);
		if (e1->no_xyzbylst!=0)
			ncl_lst_delete(WHOLE_BOUNDARY_LIST, &e1->key);
	}
	ncl_conv_sfseg_reset();
   uu_dexit;
   return (status);
   }
/*********************************************************************
**    E_FUNCTION     : int ncl_transf_npatch (patch, tfmat, type)
**       Transform an NCL surface patch.
**    PARAMETERS   
**       INPUT  : 
**          patch      - pointer to ncl surface patch
**          tfmat      - transformation matrix.
**          type       - type of patch, 0 = full, 1 = ruled
**       OUTPUT :  
**          patch      - pointer to transformed surface patch.
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_transf_npatch (patch, tfmat, type)
   struct NCL_patch_rec *patch;
   UM_transf tfmat;
   int type;

   {
   int i, j, nvecs, status;
   UM_vector vec;

   uu_denter(UU_MTRC,(us,"ncl_transf_npatch()"));

   status = UU_SUCCESS;

   nvecs = 7;
   if (type == 1) nvecs = 3;
   um_cctmtf(patch->pt,tfmat,patch->pt);
   for (i=0; i<nvecs; i++) 
     {
     for (j=0; j<3; j++) vec[j] = patch->delta[i][j];
     um_vctmtf(vec,tfmat,vec);
     for (j=0; j<3; j++) patch->delta[i][j] = vec[j];
     }

   uu_dexit;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : void sfoutj (sfkey,j,llab,subscr,ier)
**       Fortran callable function to extract a component from
**       a composite surface or solid.
**    PARAMETERS
**       INPUT  :
**          sfkey   - key of composite surface.
**          j       - component number
**          llab    - Base label to name extracted surfaces.
**          subscr  - Base subscript for label.
**       OUTPUT :
**          ier   - 0 iff no errors
**    RETURNS      :
**         UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void sfoutj (sfkey,j,llab,subscr,ier)
UM_int4 *sfkey,*subscr;
UM_int2 *j,*ier;
UM_f77_str_ptr llab;
{
	int i,len,inc,status,rel_num,nkeys,isubscr;
	UM_int2 ilab,isub,caon,isubc;
	UM_transf tran;
	UM_int2 i2,lblst;
	UM_int4 i4;
	char slabel[NCL_MAX_LABEL+1],buf[NCL_MAX_LABEL_AND_SUBSCRIPT+1];
	char *lb0;
	UM_f77_str_ptr str77;
	struct UM_srfdatabag s2,tsf;
	struct UM_surfattr_rec attr;
	int size = sizeof(tsf);
	UU_KEY_ID nclkey,*keys,keyi;
	UM_int2 ifnd;
	struct UM_solid_rec solent;
	struct NCL_netsf_rec srfent;
	struct UM_solid_rec *trment;
/*
.....Initialize routine
*/
	*ier = 541;
	isubscr = *subscr;
	isub = 296; getifl(&isub,&ilab);
	isubc = 41; getifl(&isubc,&caon);
	lb0 = UM_cstr_of_f77_str(llab);
	UM_init_f77_str (str77, buf, 80);
	keys = UU_NULL;
/*
.....Get the composite surface/solid
*/
	nclkey = *sfkey;
	status = ur_retrieve_data_relnum(nclkey,&rel_num);
	if (rel_num == NCL_NETSF_REL)
	{
		srfent.key = nclkey;
		status = ncl_retrieve_data_fixed(&srfent,sizeof(srfent));
		if (status != UU_SUCCESS) goto done;
		strncpy(slabel,srfent.label,NCL_MAX_LABEL);
		slabel[NCL_MAX_LABEL] = '\0';
		nkeys = srfent.no_netkey;
		keys = (UU_KEY_ID *)uu_malloc(sizeof(UU_KEY_ID)*nkeys);
		if (keys == UU_NULL) goto done;
		if (ur_retrieve_data_varlist(nclkey,1,keys,1,nkeys) != UU_SUCCESS)
           goto done;
	}
	else if (rel_num == UM_SOLID_REL)
	{
		solent.key = nclkey;
		status = ur_retrieve_data_fixed(&solent,sizeof(solent));
		if (status != UU_SUCCESS) goto done;
		strncpy(slabel,solent.label,NCL_MAX_LABEL);
		slabel[NCL_MAX_LABEL] = '\0';
		nkeys = solent.no_netkey;
		keys = (UU_KEY_ID *)uu_malloc(sizeof(UU_KEY_ID)*nkeys);
		if (keys == UU_NULL) goto done;
		if (ur_retrieve_data_varlist(nclkey,4,keys,1,nkeys) != UU_SUCCESS)
           goto done;
	}

	if (status == UU_SUCCESS)
		status = uc_retrieve_transf (nclkey,tran);
	if (status != UU_SUCCESS) goto done;

	inc = *j - 1;
	if (inc >= nkeys) goto done;
	tsf.key = keys[inc];
	status = ncl_retrieve_data_fixed(&tsf);
	if (status != UU_SUCCESS) goto done;
	trment = (struct UM_solid_rec *)&tsf;

	NCL_multi_ent = 1;
	i=0;

	if (ilab > 0)
	{
		buf[0]='\0';
		sprintf(buf,"%s%d",lb0,ilab);
		len = strlen(buf);
/*
.....If the next label is equal to the composite label,then skip this
.....label and update saveid.This prevents the components from being labeled
.....the same as the composite label, thus avoiding replacment of the
.....composite entity
*/
		while(slabel[i]==buf[i] && slabel[i]!=' ' )
			i++;
		if(i && slabel[i]==' ' && len==i)
		{
			ncl_sfs_update_saveid (lb0,&ilab,&isubscr);
			setifl(&isub,&ilab);
		}
/*
.....If the canon flag is off and the next label already exists,then skip it
.....and update saveid, till you get an unsued label.This prevents the
.....components from being labeled the same as the composite curve label.
*/
		sprintf(buf,"%s%d",lb0,ilab);
		i4 = isubscr;
		while (caon == 0)
		{
			sprintf(buf,"%s%d",lb0,ilab);
			i4 = isubscr;
			keyi = NULLKEY;
			chklab (str77, &keyi, &i4, &ifnd, &caon);
			if (ifnd == 1)
			{
				ncl_sfs_update_saveid (lb0,&ilab,&isubscr);
				setifl(&isub,&ilab);
			}
			else
				break;
		}
	}
	strcpy(s2.label,buf);
	s2.subscr = isubscr;
	NCL_ubcopy = 2;
	status = uc_copy (&tsf,&s2,size);
	NCL_ubcopy = 0;
	if (status != UU_SUCCESS) goto done;
/*	status = ncl_store_wf2(s2.key,s2.rel_num,s2.label,s2.subscr);*/
	status = ncl_retrieve_data_fixed(&s2);
	trment = (struct UM_solid_rec *)&s2;

	NCL_multi_ent = 0;
	if (status == UU_SUCCESS)
	{
		isub = 296; getifl(&isub,&ilab);
		ncl_sfs_update_saveid (lb0,&ilab,&isubscr);
/*
..... check if label should be on
*/
		if (ncl_get_type(s2.rel_num, &i2) == UU_SUCCESS)
		{
			lblchk(&i2,&lblst);
			if (lblst > 0)
			{
				uc_retrieve_attr(s2.key, &attr);
				attr.label_on = lblst;
				ur_update_attr(&attr);
			}
		}
/*		uc_transform(&s2,tran,UU_TRUE);*/
		ncl_def_color (s2.key);
		ur_update_displayable(s2.key, UM_DISPLAYABLE);
		uc_display (&s2);
	}

	if (status == UU_SUCCESS) *ier = 0;
done:;
	if (keys != UU_NULL) uu_free(keys);
	return;
}
