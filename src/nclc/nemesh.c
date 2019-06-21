/*********************************************************************
**    NAME         :  nemesh.c
**       CONTAINS: routines to handle NCL mesh surfaces.
**       ncl_get_meshheader(surf, buf)
**       ncl_get_meshpatch(patch, meshpatch)
**       ncl_put_meshpatch(paneltype, meshpatch, patch)
**       ncl_p_meshhead(meshhead)
**       ncl_p_meshpatch(meshpatch)
**       ncl_p83_meshsurf(surf)
**       int ncl_class_copy_msh (e1, e2, bagsize)
**       int ncl_copy_msh (e1, e2)
**       int ncl_transform_msh (e1, tfmat, store)
**       int ncl_transf_mpatch (patch, tfmat)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       nemesh.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:37
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "mfort.h"
#include "mdrel.h"
#include "mcrv.h"
#include "msrf.h"
#include "mdebug.h"

#include "nccs.h"
#include "ncl.h"
#include "nclfc.h"

/*********************************************************************
**    E_FUNCTION     : ncl_get_meshheader(surf, buf)
**       Move the data for a mesh surface header into an NCLI
**       mesh surface header structure.
**    PARAMETERS   
**       INPUT  : 
**          surf              mesh surface record
**       OUTPUT :  
**          buf               NCLI representation of a mesh surface header
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_get_meshheader(surf, buf)
   struct NCL_meshsf_rec *surf;
   struct NCLI_meshhead_rec *buf;

   {
	int status;
	struct UM_surfattr_rec surfattr;

   uu_denter(UU_MTRC,(us,"ncl_get_meshheader(surf=%x, buf=%x)",
      surf, buf));
   buf->surftype = surf->surf_type;
   buf->numpatches = surf->no_mpatch;
   buf->m = surf->m;
   buf->n = surf->n;
   buf->offset = surf->offset;
   buf->offdist = surf->offdist;
	surfattr.key = surf->key;
	status = ur_retrieve_attr(&surfattr);
	if (status == UU_SUCCESS)
	{
   	buf->numupaths = surfattr.numupaths;
   	buf->numvpaths = surfattr.numvpaths;
   	buf->ptsperucrv = surfattr.ptsperucrv;
   	buf->ptspervcrv = surfattr.ptspervcrv;
	}
   uu_dexit;
   }

/*********************************************************************
**    E_FUNCTION     : ncl_get_meshpatch(patch, meshpatch)
**       Move a mesh surface patch into an NCLI internal structure.
**    PARAMETERS   
**       INPUT  : 
**          patch             surface patch
**       OUTPUT :  
**          sfrpatch          NCLI representation of a surface
**                            patch
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_get_meshpatch(patch, meshpatch)
   struct NCL_mpatch_rec *patch;
   struct NCLI_meshpatch_rec *meshpatch;

   {
   int i,j,k;

   uu_denter(UU_MTRC,(us,"ncl_get_meshpatch(patch=%x,meshpatch=%x)",
      patch, meshpatch));

   ncl_uureal_to_real8(3, patch->pt, meshpatch->point);
   for (j=0; j<15; j++) 
      {
      for (i=0; i<3; i++) meshpatch->delta[j][i] = patch->delta[j][i];
      }
   uu_dexit;
   }

/*********************************************************************
**    E_FUNCTION     : ncl_put_meshpatch(paneltype, meshpatch, patch)
**       Move an NCLI mesh surface patch into a mesh surface patch.
**    PARAMETERS   
**       INPUT  : 
**          sfrpatch          NCLI representation of a mesh surface
**                            patch
**       OUTPUT :  
**          patch             mesh surface patch
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_put_meshpatch(meshpatch, patch)
   struct NCLI_meshpatch_rec *meshpatch;
   struct NCL_mpatch_rec *patch;

   {
   int i,j,k;

   uu_denter(UU_MTRC,(us,"ncl_put_meshpatch(meshpatch=%x,patch=%x)",
      meshpatch, patch));
/*   ncl_p_meshpatch(meshpatch); */
   ncl_real8_to_uureal(3, meshpatch->point, patch->pt);
   for (j=0; j<15; j++) 
      {
      for (i=0; i<3; i++) patch->delta[j][i] = meshpatch->delta[j][i];
      }
   uu_dexit;
   }

/*********************************************************************
**    E_FUNCTION     : ncl_p_meshhead(meshhead)
**       Print an NCLI mesh surface header record.
**    PARAMETERS   
**       INPUT  : 
**          meshhead          NCLI mesh surface header record
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_p_meshhead(meshhead)
   struct NCLI_meshhead_rec *meshhead;

   {
   uu_denter(UU_MTRC,(us,"ncl_p_meshhead(meshhead=%x)",meshhead));

   um_pscroll("NCLI_meshhead_rec");
   sprintf(UM_sbuf,"  surftype=%d", meshhead->surftype);
   um_pscroll(UM_sbuf);
   sprintf(UM_sbuf,"  m=%d, n=%d", meshhead->m, meshhead->n);
   um_pscroll(UM_sbuf);

   uu_dexit;
   }

/*********************************************************************
**    E_FUNCTION     : ncl_p_meshpatch(meshpatch)
**       Print an NCLI mesh surface patch record.
**    PARAMETERS   
**       INPUT  : 
**          meshpatch            NCLI mesh surface patch record
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_p_meshpatch(meshpatch)
   struct NCLI_meshpatch_rec *meshpatch;

   {
   int i;

   uu_denter(UU_MTRC,(us,"ncl_p_meshpatch(meshpatch=%x)",meshpatch));

   um_pscroll("NCLI_meshpatch_rec");
   sprintf(UM_sbuf, "  point = (%g,%g,%g)", meshpatch->point[0],
      meshpatch->point[1], meshpatch->point[2]);
   um_pscroll(UM_sbuf);
   for (i=0; i<15; i++)
      {
      sprintf(UM_sbuf, "  delta[%d] = (%g,%g,%g)", i, meshpatch->delta[i][0],
         meshpatch->delta[i][1], meshpatch->delta[i][2]);
      um_pscroll(UM_sbuf);
      }

   uu_dexit;
   }

/*********************************************************************
**    E_FUNCTION     : ncl_p83_meshsurf(surf)
**       Print an NCL mesh surface record.
**    PARAMETERS   
**       INPUT  : 
**          surf                 an NCL mesh record (UNIBASE format)
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_p83_meshsurf(surf)
   struct NCL_meshsf_rec *surf;

   {
   int i,j;

   uu_denter(UU_MTRC,(us,"ncl_p83_meshsurf(key=%x)", surf->key));

   sprintf(UM_sbuf,"NCL MESHSURF: %x", surf->key);
   um_pscroll(UM_sbuf);

   sprintf(UM_sbuf, "label %s",surf->label);
   um_pscroll(UM_sbuf);

   um_p_ary(UM_PINT,"surf_type", 1, &surf->surf_type);
   um_p_ary(UM_PINT,"m", 1, &surf->m);
   um_p_ary(UM_PINT,"n", 1, &surf->n);

   for (i=0; i<surf->no_mpatch; i++)
      {
      sprintf(UM_sbuf, "  PATCH %d", i);
      um_pscroll(UM_sbuf);
      sprintf(UM_sbuf, "    point=(%g,%g,%g)", surf->mpatch[i].pt[0],
         surf->mpatch[i].pt[1], surf->mpatch[i].pt[2]);
      um_pscroll(UM_sbuf);
      for (j=0; j<15; j++)
         {
         sprintf(UM_sbuf, "    delta(%d)=(%g,%g,%g)", j,
            surf->mpatch[i].delta[j][0], surf->mpatch[i].delta[j][1],
            surf->mpatch[i].delta[j][2]);
         um_pscroll(UM_sbuf);
         }
      }

   uu_dexit;
   }
/*********************************************************************
**    E_FUNCTION     : int ncl_class_copy_msh (e1, e2, bagsize)
**       Copy a mesh surface with updated label & store in ranfile.
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
ncl_class_copy_msh (e1, e2, bagsize)
   struct NCL_surface_rec *e1, *e2;
   int bagsize;

   {
   int status;
   UM_int2 nclstatus;

   uu_denter(UU_MTRC,(us,"ncl_class_copy_msh(key=%x)", e1->key));

   status = ncl_label_wf(NCL_MESHSURF_REL, e2->label, &e2->subscr, 0,
                         &nclstatus);
   if (status == UU_SUCCESS) status = ncl_copy_msh(e1, e2);
   if (status == UU_SUCCESS) status = ncl_store_wf1(e2->key);

   uu_dexit;
   return (status);
   }
/*********************************************************************
**    E_FUNCTION     : int ncl_copy_msh (e1, e2)
**       Copy a Mesh surface.
**    PARAMETERS   
**       INPUT  : 
**          e1         - pointer to surface to copy
**       OUTPUT :  
**          e2         - pointer to copied surface.
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : Uses default transformation
*********************************************************************/
int
ncl_copy_msh (e1, e2)
   struct NCL_meshsf_rec *e1, *e2;

   {
   int ipan, ipat, npats, isize, status;
   struct NCL_mpatch_rec patch;
   UU_KEY_ID key;

   uu_denter(UU_MTRC,(us,"ncl_copy_msh(key=%x)", e1->key));

   e2->key = e1->key;
   status = ncl_retrieve_data_fixed (e2);
   if (status == UU_SUCCESS)
     {
     e2->key = 0;
     e2->no_mpatch = 0;
     status = ncl_create_entity (e2, 9);
     }

   if (status == UU_SUCCESS)
     if (ur_update_data_varlist (e2->key, 1, e1->mpatch, 1, e1->no_mpatch) != 0)
       status = UU_FAILURE;
     else status = ncl_retrieve_data_fixed(e2);

   uu_dexit;
   return (status);
   }
/*********************************************************************
**    E_FUNCTION     : int ncl_transform_msh (e1, tfmat, store)
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
**    WARNINGS     : none
*********************************************************************/
int
ncl_transform_msh (e1, tfmat, store)
   struct NCL_meshsf_rec *e1;
   UM_transf tfmat;
   UU_LOGICAL store;

   {
   int ipat ,status;
   struct NCL_mpatch_rec patch;
   UU_REAL dist, det;

   uu_denter(UU_MTRC,(us,"ncl_transform_msh(key=%x)", e1->key));

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

   for (ipat=1; ipat<=e1->no_mpatch && status == UU_SUCCESS; ipat++)
     {
     if (ur_retrieve_data_varlist (e1->key, 1, &patch, ipat, 1) != 0)
        status = UU_FAILURE;
     else status = ncl_transf_mpatch (&patch, tfmat);
     if (status == UU_SUCCESS && store)
       if (ur_update_data_varlist (e1->key, 1, &patch, ipat, 1) != 0)
         status = UU_FAILURE;
     }

   uu_dexit;
   return (status);
   }
/*********************************************************************
**    E_FUNCTION     : int ncl_transf_mpatch (patch, tfmat)
**       Transform a mesh surface patch.
**    PARAMETERS   
**       INPUT  : 
**          patch      - pointer to mesh surface patch
**          tfmat      - transformation matrix.
**       OUTPUT :  
**          patch      - pointer to transformed surface patch.
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_transf_mpatch (patch, tfmat)
   struct NCL_mpatch_rec *patch;
   UM_transf tfmat;

   {
   int i, j, status;

   uu_denter(UU_MTRC,(us,"ncl_transf_mpatch()"));

   status = UU_SUCCESS;

   um_cctmtf(patch->pt,tfmat,patch->pt);
   for (i=0; i<15; i++) 
     {
     um_vctmtf(patch->delta[i],tfmat,patch->delta[i]);
     }

   uu_dexit;
   return (status);
   }
