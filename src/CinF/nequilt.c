

/*********************************************************************
**    NAME         :  nequilt.c
**       CONTAINS: routines to handle NCL quilt surfaces.
**       ncl_get_quiltheader(surf, buf)
**       ncl_get_quiltpatch(patch, quiltpatch)
**       ncl_put_quiltpatch(paneltype, quiltpatch, patch)
**       ncl_p_quilthead(quilthead)
**       ncl_p_quiltpatch(quiltpatch)
**       ncl_p86_quiltsurf(surf)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       nequilt.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:44
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "mfort.h"
#include "mdrel.h"
#include "mcrv.h"
#include "mdebug.h"

#include "nccs.h"
#include "ncl.h"

/*********************************************************************
**    E_FUNCTION     : ncl_get_quiltheader(surf, buf)
**       Move the data for a quilt surface header into an NCLI
**       quilt surface header structure.
**    PARAMETERS   
**       INPUT  : 
**          surf              quilt surface record
**       OUTPUT :  
**          quilt             NCLI representation of a quilt surface header
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_get_quiltheader(surf, quilt)
   struct NCL_quiltsf_rec *surf;
   struct NCLI_quilthead_rec *quilt;

   {
   int i,j;

   uu_denter(UU_MTRC,(us,"ncl_get_quiltheader(surf=%x, buf=%x)",
      surf, quilt));

   quilt->surftype = surf->surf_type;
   quilt->numpatches = surf->numpatches;
   quilt->offset = surf->offset;
   quilt->offdist = surf->offdist;
   for (j=0; j<12; j++)
      for (i=0; i<3; i++) quilt->midpt[j][i] = surf->midpt[j][i];

   uu_dexit;
   }

/*********************************************************************
**    E_FUNCTION     : ncl_get_quiltpatch(patch, quiltpatch)
**       Move a quilt surface patch into an NCLI internal structure.
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
ncl_get_quiltpatch(patch, quiltpatch)
   struct NCL_qpatch_rec *patch;
   struct NCLI_quiltpatch_rec *quiltpatch;

   {
   int i;

   uu_denter(UU_MTRC,(us,"ncl_get_quiltpatch(patch=%x,quiltpatch=%x)",
      patch, quiltpatch));

   for (i=0; i<4; i++) quiltpatch->bpatchnum[i] = patch->bpatchnum[i];
   for (i=0; i<3; i++) quiltpatch->origin[i] = patch->origin[i];
   for (i=0; i<25; i++) quiltpatch->xvalues[i] = patch->xvalues[i];
   for (i=0; i<25; i++) quiltpatch->yvalues[i] = patch->yvalues[i];
   for (i=0; i<25; i++) quiltpatch->zvalues[i] = patch->zvalues[i];

   uu_dexit;
   }

/*********************************************************************
**    E_FUNCTION     : ncl_put_quiltpatch(quiltpatch, patch)
**       Move an NCLI quilt surface patch into a quilt surface patch.
**    PARAMETERS   
**       INPUT  : 
**          sfrpatch          NCLI representation of a quilt surface
**                            patch
**       OUTPUT :  
**          patch             quilt surface patch
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_put_quiltpatch(quiltpatch, patch)
   struct NCLI_quiltpatch_rec *quiltpatch;
   struct NCL_qpatch_rec *patch;

   {
   int i;

   uu_denter(UU_MTRC,(us,"ncl_put_quiltpatch(quiltpatch=%x,patch=%x)",
      quiltpatch, patch));

   for (i=0; i<4; i++) patch->bpatchnum[i] = quiltpatch->bpatchnum[i];
   for (i=0; i<3; i++) patch->origin[i] = quiltpatch->origin[i];
   for (i=0; i<25; i++) patch->xvalues[i] = quiltpatch->xvalues[i];
   for (i=0; i<25; i++) patch->yvalues[i] = quiltpatch->yvalues[i];
   for (i=0; i<25; i++) patch->zvalues[i] = quiltpatch->zvalues[i];

   uu_dexit;
   }

/*********************************************************************
**    E_FUNCTION     : ncl_p_quilthead(quilthead)
**       Print an NCLI quilt surface header record.
**    PARAMETERS   
**       INPUT  : 
**          quilthead            NCLI quilt surface header record
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_p_quilthead(quilthead)
   struct NCLI_quilthead_rec *quilthead;

   {
   uu_denter(UU_MTRC,(us,"ncl_p_quilthead(quilthead=%x)",quilthead));

   um_pscroll("NCLI_quilthead_rec");
   sprintf(UM_sbuf,"  surftype=%d", quilthead->surftype);
   um_pscroll(UM_sbuf);
   sprintf(UM_sbuf,"  numpatches=%d", quilthead->numpatches);
   um_pscroll(UM_sbuf);

   uu_dexit;
   }

/*********************************************************************
**    E_FUNCTION     : ncl_p_quiltpatch(quiltpatch)
**       Print an NCLI quilt surface patch record.
**    PARAMETERS   
**       INPUT  : 
**          quiltpatch           NCLI quilt surface patch record
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_p_quiltpatch(quiltpatch)
   struct NCLI_quiltpatch_rec *quiltpatch;

   {
   int i;

   uu_denter(UU_MTRC,(us,"ncl_p_quiltpatch(quiltpatch=%x)",quiltpatch));

   um_pscroll("NCLI_quiltpatch_rec");

   sprintf(UM_sbuf, "  bpatchnum = (%d,%d,%d,%d)",
      quiltpatch->bpatchnum[0], quiltpatch->bpatchnum[1],
      quiltpatch->bpatchnum[2], quiltpatch->bpatchnum[3]);
   um_pscroll(UM_sbuf);

   sprintf(UM_sbuf, "  origin = (%g,%g,%g)", quiltpatch->origin[0],
      quiltpatch->origin[1], quiltpatch->origin[2]);
   um_pscroll(UM_sbuf);

   for (i=0; i<25; i++)
      {
      sprintf(UM_sbuf, "  xyz values[%d] = (%g,%g,%g)", i, 
         quiltpatch->xvalues[i], quiltpatch->yvalues[i], quiltpatch->zvalues[i]);
      um_pscroll(UM_sbuf);
      }

   uu_dexit;
   }

/*********************************************************************
**    E_FUNCTION     : ncl_p86_quiltsurf(surf)
**       Print an NCL quilt surface record.
**    PARAMETERS   
**       INPUT  : 
**          surf                 an NCL quilt record (UNIBASE format)
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_p86_quiltsurf(surf)
   struct NCL_quiltsf_rec *surf;

   {
   int i,j;

   uu_denter(UU_MTRC,(us,"ncl_p86_quiltsurf(key=%x)", surf->key));

   sprintf(UM_sbuf,"NCL QUILTSURF: %x", surf->key);
   um_pscroll(UM_sbuf);

   sprintf(UM_sbuf, "label %s",surf->label);
   um_pscroll(UM_sbuf);

   um_p_ary(UM_PINT,"surf_type", 1, &surf->surf_type);
   um_p_ary(UM_PINT,"numpatches", 1, &surf->numpatches);

   for (i=0; i<12; i++)
      {
      sprintf(UM_sbuf, "    midpt[%d]=(%g,%g,%g)", i, surf->midpt[i][0],
         surf->midpt[i][1], surf->midpt[i][2]);
      um_pscroll(UM_sbuf);
      }

   for (i=0; i<surf->no_qpatch; i++)
      {
      sprintf(UM_sbuf, "  PATCH %d", i);
      um_pscroll(UM_sbuf);
      sprintf(UM_sbuf, "    bpatchnum=(%d,%d,%d,%d)",
         surf->qpatch[i].bpatchnum[0], surf->qpatch[i].bpatchnum[1],
         surf->qpatch[i].bpatchnum[2], surf->qpatch[i].bpatchnum[3]);
      um_pscroll(UM_sbuf);
      sprintf(UM_sbuf, "    origin=(%g,%g,%g)", surf->qpatch[i].origin[0],
         surf->qpatch[i].origin[1], surf->qpatch[i].origin[2]);
      um_pscroll(UM_sbuf);
      for (j=0; j<25; j++)
         {
         sprintf(UM_sbuf, "    xyz values(%d)=(%g,%g,%g)", j,
            surf->qpatch[i].xvalues[j], surf->qpatch[i].yvalues[j],
            surf->qpatch[i].zvalues[j]);
         um_pscroll(UM_sbuf);
         }
      }

   uu_dexit;
   }

