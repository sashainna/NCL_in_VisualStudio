/*********************************************************************
**    NAME         :  nput.c
**       CONTAINS: data storage routines for mapping NCL entities
**                   to the data representation for UNIBASE; these routines
**                   are FORTRAN callable and hence follow the standard
**                   FORTRAN/C interface conventions
**       ncl_create_entity (entity)
**       ncl_create_data()
**       int ptgeo(type, buf, nclkey)
**       int ptshd(buf, nclkey)
**       int ptspan(nclkey, ispan, buf)
**       int ptmhed(buf, nclkey)
**       int ptmpat(nclkey, ipatch, buf)
**       int ptqhed(buf, nclkey)
**       int ptqpat(nclkey, ipatch, buf)
**       int ptpnhd(buf, nclkey, ifirst)
**       int ptpnpt(buf, nclkey, ipt)
**       int ptntsf(buf, nclkey)
**       int ptshap(buf, nclkey, nw)
**       int upsfuv(nclkey, kuvs)
**       int upsurf(nclkey, kuvs)
**       void upsrst()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       nput.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:09:02
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

#include "ncl.h"
#include "nccs.h"
#include "nclfc.h"
#include "nclvx.h"
#include "mgeom.h"
#include "msrf.h"

#if UU_COMP == UU_IRIS || UU_COMP == UU_IRIS4D
#ifdef UU_RS6000
#include <sys/time.h>
#endif
#include <time.h>
#else
#if (UU_COMP != UU_WIN2K)
#include <sys/time.h>
#else
#include <time.h>
#endif
#endif

static UM_tess_settype tesstyp;
static UU_REAL tesstol;
static int kupts,kvpts;

/*********************************************************************
**    E_FUNCTION :  ncl_create_entity (entity,type)
**       Creates the entity given in the generic entity format in unibase.
**    PARAMETERS
**       INPUT  :
**          entity - generic entity to save
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ncl_create_entity(eptr,type)
   struct  UC_entitydatabag *eptr;
   int     type; /* geo type */

   {
   struct UM_transf_rec tran;
   struct NCL_nclattr_rec attr;
	struct UM_surfattr_rec sfattr;
   struct NCL_curve_rec *leptr;
   struct NCL_scalar_rec *septr;
   int status;
   UM_f77_str ncllabel;
   UM_int4 subscr;
	time_t todaytime;

   uu_denter(UU_MTRC,(us,"ncl_create_entity(rel_num=%d,)",eptr->rel_num));

   /* Create NCL point, line and circle as CADD geo */
   if (eptr->rel_num == NCL_POINT_REL)
       eptr->rel_num = UM_POINT_REL;
   else if (eptr->rel_num == NCL_LINE_REL)
       eptr->rel_num = UM_LINE_REL;
   else if (eptr->rel_num == NCL_CIRCLE_REL)
       eptr->rel_num = UM_CIRCLE_REL;

   /* if geo already exist and it is a point,line or circle, update it */
	if ((eptr->key != 0) && ((eptr->rel_num == UM_CIRCLE_REL) ||
		(eptr->rel_num == UM_LINE_REL) || (eptr->rel_num == UM_POINT_REL)
		|| (eptr->rel_num == NCL_SCALAR_REL) || (eptr->rel_num==NCL_TEXTVAR_REL) ))
	{
		if (eptr->rel_num == NCL_SCALAR_REL)
		{
			septr = (struct NCL_scalar_rec *) eptr;
/*
.....initial modified to current time
*/
#ifndef UU_RS6000
			time(&todaytime);
#else
			ftime(&todaytime);
#endif
			septr->modified = (double)todaytime;
		}
		status = ur_update_data(eptr);
		if (eptr->rel_num == NCL_SCALAR_REL)
		{
			ncl_update_scalar_frm();
		}
		goto done;
	}
   /* get the current label and store in this entity */
   leptr = (struct NCL_curve_rec *) eptr;
   UM_init_f77_str(ncllabel, leptr->label, NCL_MAX_LABEL);
   curnam(UM_addr_of_f77_str(ncllabel), &subscr);
   leptr->label[NCL_MAX_LABEL-1] = '\0';
   if (eptr->rel_num == NCL_SCALAR_REL || eptr->rel_num == NCL_DATAST_REL ||
       eptr->rel_num == NCL_TEXTVAR_REL)
	{
		septr = (struct NCL_scalar_rec *) eptr;
		septr->subscr = subscr;
		if (eptr->rel_num == NCL_SCALAR_REL)
		{
			septr->descript[0] = '\0'; 
			strcpy(septr->classnm, "Default"); 
/*
.....initial modified to current time
*/
#ifndef UU_RS6000
			time(&todaytime);
#else
			ftime(&todaytime);
#endif
			septr->modified = (double)todaytime;
		}
     }
   else
     {
     leptr->subscr = subscr;
     um_init_lablocal (leptr);
     }

   /* create UNIBASE entity */
   status = ncl_create_data(eptr);
   if (status != UU_SUCCESS) goto done;

   /* create UNIBASE attribute bundle and associate with entity */
   /* pass ncl geometry type to the routine. kathy */

	if ( eptr->rel_num == UM_RBSPLSRF_REL ||
		eptr->rel_num == NCL_SURF_REL ||
		eptr->rel_num == NCL_REVSURF_REL ||
		eptr->rel_num == NCL_MESHSURF_REL ||
		eptr->rel_num == NCL_NETSF_REL ||
		eptr->rel_num == NCL_SHAPE_REL ||
		eptr->rel_num == NCL_TRIMSF_REL )
	{
		ncl_init_surf_attr(eptr->key,&sfattr,NCLI_SURF);
	}
	else
	{
		ncl_init_attr_rec(eptr->key, &attr, type);
	}

   /* create UNIBASE identity transformation and associate with entity */
   tran.key = eptr->key;
   tran.rel_num = UM_TRANSFORM_REL;
   um_tftotf(UM_idmat,tran.tfmat);
   status = ur_update_transf(&tran);
   if (status != UU_SUCCESS) goto done;

   /* make sure the entity can be viewed in all views */
   status = ur_update_view_key(eptr->key, 0);
   if (status != UU_SUCCESS) goto done;

   /* set displayablity of entity */
   switch (eptr->rel_num)
      {
      case NCL_SCALAR_REL:
      case NCL_TEXTVAR_REL:
         status = ur_update_displayable(eptr->key, UM_NEVERDISPLAYABLE);
         break;
      case NCL_CURVE_REL:
         status = ncl_fix_curve (eptr);
         break;
      default:
         break;
      }
/*
.....updated the scalar form if it opened
*/	
	if (eptr->rel_num == NCL_SCALAR_REL)
		ncl_update_scalar_frm();
done:;
   uu_dexit;
   return(status);
   }

/*********************************************************************
**    E_FUNCTION :  ncl_create_data (eptr),type)
**       Creates the entity in the unibase and updates the source
**       control with this entity.
**    PARAMETERS
**       INPUT  :
**          eptr   - generic entity to save
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_create_data(eptr)
struct  UC_entitydatabag *eptr;
{
	int status;
/*
.....Create Unibase entity
*/
   status = ur_create_data(eptr);
/*
.....Update Source Control
*/
	if (status == UU_SUCCESS) ncl_srcctl_put(eptr->key);
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : int ptgeo(type, buf, nclkey)
**       Store the given NCL representation of geometry (TYPE, BUF)
**       in UNIBASE and return the UNIBASE key (NCLKEY).
**    PARAMETERS
**       INPUT  :
**          type              type of NCL geometry
**          buf               buffer holding the NCL data
**       OUTPUT :
**          nclkey            UNIBASE key of the entity
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ptgeo(type, buf, nclkey)
   UM_int2  *type;
   UM_real8 buf[];
   UU_KEY_ID *nclkey;

   {
   int status;
   int ityp;
   struct UC_entitydatabag e;

   uu_denter(UU_MTRC,(us,"ptgeo(type=%d, buf=%x)", *type, buf));
   status = UU_FAILURE;

   e.key = *nclkey;

   switch (*type)
      {
      case NCLI_VECTOR:
         ur_setup_data(NCL_VECTOR_REL, &e, sizeof(struct NCL_vector_rec));
         status = ncl_nclvec_to_vector(buf, &e);
         break;
      case NCLI_POINT:
         ur_setup_data(NCL_POINT_REL, &e, sizeof(struct NCL_nclpt_rec));
         status = ncl_nclpt_to_point(buf, &e);
         break;
      case NCLI_LINE:
         ur_setup_data(NCL_LINE_REL, &e, sizeof(struct NCL_nclln_rec));
         status = ncl_nclline_to_line(buf, &e);
         break;
      case NCLI_CIRCLE:
         ur_setup_data(NCL_CIRCLE_REL, &e, sizeof(struct NCL_nclci_rec));
         status = ncl_nclcirc_to_circ(buf, &e);
         break;
      case NCLI_CURVE:
         status = ncl_nclcrv_to_curve(buf, &e);
         break;
      case NCLI_PLANE:
         ur_setup_data(NCL_PLN_REL, &e, sizeof(struct NCL_nclpl_rec));
         status = ncl_nclpln_to_plane(buf, &e);
         break;
      case NCLI_MATRIX:
         ur_setup_data(NCL_MATRIX_REL, &e, sizeof(struct NCL_matrix_rec));
         status = ncl_nclmatrix_to_matrix(buf, &e);
         break;
      case NCLI_SCALAR:
         status = ncl_put_scalar(buf, &e);
         break;
      case NCLI_POINTVEC:
         ur_setup_data(NCL_POINTVEC_REL, &e, sizeof(struct NCL_nclpv_rec));
         status = ncl_put_pointvec(buf, &e);
         break;
      case NCLI_SHAPE:
         status = UU_FAILURE;
         break;
      default:
         status = UU_FAILURE;
         break;
      }
   if (status == UU_SUCCESS)
      {
      ityp = *type;
      status = ncl_create_entity(&e, ityp); /* pass geo type. kathy */
      *nclkey = e.key;
      }
   if (status != UU_SUCCESS)
      uu_dprint(-1,(us,"ptgeo returned FAILURE"));
   uu_dexit;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : int ptshd(buf, nclkey)
**       Create a regular NCL surface in UNIBASE and return the UNIBASE
**       key of the surface. The surface is initialized to an empty
**       surface.
**    PARAMETERS
**       INPUT  :
**          buf                  NCLI buffer of surface
**       OUTPUT :
**          nclkey               UNIBASE key of surface
**    RETURNS      :
**       UU_SUCCESS iff no error
**       UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ptshd(buf, nclkey)
   UM_real8 buf[];
   UU_KEY_ID *nclkey;

   {
   int status, i;
   struct NCL_surface_rec surf;
   struct NCLI_srfhead_rec *srfhead;
	struct UM_surfattr_rec surfattr;

   uu_denter(UU_MTRC,(us,"ptshd(buf=%x, nclkey=%x)",buf, nclkey));

   ur_setup_data(NCL_SURF_REL, &surf, sizeof(surf));

   /* initialize key to zero to force create */
	surf.key = 0;
	for (i=0;i<3;i++)
	{
		surf.labloc[i] = 0.0;
/*
.....initialize leader line location
*/

		surf.ldrloc[i] = 0.0;	
	}
	surf.surf_type = NCLI_REGSURF;
   surf.no_panelkey = 0;
   srfhead = (struct NCLI_srfhead_rec *) buf;
   surf.offset = srfhead->offset;
   surf.offdist = srfhead->offdist;

   /* Initialize ruled-in-u/v variables */
   surf.rldnu = -1;
   surf.swapuv = 0;
   surf.closdinu = 0;
   surf.closdinv = 0;
   surf.rev_normal = UU_FALSE;

	surf.primitive = NCLSF_UNKNOWN;

   status = ncl_create_entity((struct UC_entitydatabag *)&surf, 9); /* pass geo type. kathy */
   *nclkey = surf.key;
/*
.....Update the attribute record
*/
	surfattr.key = surf.key;
	status = ur_retrieve_attr(&surfattr);
	if (status == UU_SUCCESS)
	{
		surfattr.material = UM_srfattr.material;
		surfattr.numupaths = srfhead->numupaths;
		surfattr.numvpaths = srfhead->numvpaths;
		surfattr.ptsperucrv = srfhead->ptsperucrv;
		surfattr.ptspervcrv = srfhead->ptspervcrv;
/*		surfattr.ecolor = -1;*/
		surfattr.shaded = UM_srfattr.shaded;
		surfattr.lucency = UM_srfattr.lucency;
		status = ur_update_attr(&surfattr);
	}

   if (status != UU_SUCCESS) uu_dprint(-1,(us,"ptshd returned FAILURE"));
   uu_dexit;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : int ptspan(nclkey, ispan, buf)
**       Store an entire surface span (ISPAN) for a regular NCL
**       surface identified by a UNIBASE key (nclkey). The data
**       defining the surface span is contained in NCLI format
**       in a buffer (BUF).
**    PARAMETERS
**       INPUT  :
**          nclkey               UNIBASE key of surface to put span
**          ispan                number of span to add to surface
**          buf                  NCLI format of span
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_SUCCESS  iff no error
**       UU_FAILURE  otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ptspan(nclkey, ispan, buf)
   UU_KEY_ID   *nclkey;
   UM_int2     *ispan;
   UM_real8    buf[];

   {
   int status;
   UU_KEY_ID key;
   int span;
   int rel_num;
   UM_real8    *bufadr;
   struct NCL_surface_rec surf;
   struct NCL_panel_rec panel;
   struct NCLI_srfpanel_rec *srfpanel;
   struct NCLI_srfpatch_rec *srfpatch;
   int panelsize;
   int patchsize;
   int i, krho;

   uu_denter(UU_MTRC,(us,"ptspan(key=%x, ispan=%d, buf=%x)", *nclkey,
      *ispan, buf));

   key = *nclkey;
   span = *ispan;
   status = UU_SUCCESS;
   if (ur_retrieve_data_relnum(key, &rel_num) != 0)
      status = UU_FAILURE;
   else if (rel_num != NCL_SURF_REL)
      status = UU_FAILURE;
   else
      {
      surf.key = key;
      if (ur_retrieve_data_fixed(&surf) != 0)
         status = UU_FAILURE;
      else if ((span < 1) || (span > surf.no_panelkey+1))
         status = UU_FAILURE;
      else
         {
         srfpanel = (struct NCLI_srfpanel_rec *) buf;

         ur_setup_data(NCL_PANEL_REL, &panel, sizeof(panel));
/*
..........Initialize these never used fields to something (so they
..........don't come up with random values later...RAZ
*/
         strcpy(panel.label,"");
         panel.subscr = 0;

       /*ncl_p_srfpanel(srfpanel);*/
         ncl_put_srfpanel(srfpanel, &panel);
         panelsize = ncl_srfpanelsize(srfpanel);
         patchsize = ncl_srfpatchsize(srfpanel->paneltype);
         bufadr = &buf[0];
         bufadr = bufadr + panelsize;
         srfpatch = (struct NCLI_srfpatch_rec *) (bufadr );
         if (srfpanel->paneltype == 0) krho = 7; else krho = 3;
         for (i=0; i<panel.no_patch; i++)
            {
           /* zero out unsed rho of last patch to prevent res op fault on vax */
            if (i == panel.no_patch - 1) srfpatch->delta[krho][0] = 0.0;
            ncl_put_srfpatch(srfpanel->paneltype, srfpatch, &panel.patch[i]);
            bufadr = bufadr + patchsize;
            srfpatch = (struct NCLI_srfpatch_rec *) (bufadr);
            }
         if (span <= surf.no_panelkey)
            {
            if (ur_retrieve_data_varlist(surf.key, 1, &panel.key, span, 1) != 0)
               status = UU_FAILURE;
            else if (ur_update_data(&panel) != 0)
               status = UU_FAILURE;
            }
         else
            {
            if (ur_create_data(&panel) != 0)
               status = UU_FAILURE;
            else if (ur_update_data_varlist(surf.key, 1, &panel.key,
                        surf.no_panelkey+1, 1) != 0)
               status = UU_FAILURE;
            }
         }
      }
   if (status != UU_SUCCESS) uu_dprint(-1,(us,"ptspan returned FAILURE"));
   uu_dexit;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : int ptmhed(buf, nclkey)
**       Create an NCL mesh surface entity in UNIBASE using the given
**       mesh surface header data (BUF) and return the UNIBASE key of
**       the newly created mesh surface entity.
**    PARAMETERS
**       INPUT  :
**          buf               mesh surface header data
**       OUTPUT :
**          nclkey            UNIBASE key of created entity
**    RETURNS      :
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ptmhed(buf, nclkey)
   UM_real8 buf[];
   UU_KEY_ID *nclkey;

   {
   int status;
   struct NCL_meshsf_rec surf;
   struct NCLI_meshhead_rec *meshhead;
	struct UM_surfattr_rec surfattr;

   uu_denter(UU_MTRC,(us,"ptmhed(buf=%x, nclkey=%x)",buf, nclkey));

   ur_setup_data(NCL_MESHSURF_REL, &surf, sizeof(surf));

   /* initialize key to zero to force create */
   surf.key = 0;
   surf.surf_type = NCLI_MESHSURF;
   surf.no_mpatch = 0;

   meshhead = (struct NCLI_meshhead_rec *) buf;
   surf.m = meshhead->m;
   surf.n = meshhead->n;
   surf.offset = meshhead->offset;
   surf.offdist = meshhead->offdist;

   /* Initialize ruled-in-u/v variables */
   surf.rldnu = -1;
   surf.swapuv = 0;
   surf.closdinu = 0;
   surf.closdinv = 0;
   surf.rev_normal = UU_FALSE;

   status = ncl_create_entity((struct UC_entitydatabag *)&surf, 9); /* pass geo type. kathy */
   *nclkey = surf.key;
/*
.....Update the attribute record
*/
	surfattr.key = surf.key;
	status = ur_retrieve_attr(&surfattr);
	if (status == UU_SUCCESS)
	{
   	surfattr.material = UM_srfattr.material;
   	surfattr.numupaths = meshhead->numupaths;
   	surfattr.numvpaths = meshhead->numvpaths;
   	surfattr.ptsperucrv = meshhead->ptsperucrv;
   	surfattr.ptspervcrv = meshhead->ptspervcrv;
/*   	surfattr.ecolor = -1;*/
		surfattr.shaded = UM_srfattr.shaded;
		surfattr.lucency = UM_srfattr.lucency;
		status = ur_update_attr(&surfattr);
	}
   if (status != UU_SUCCESS) uu_dprint(-1,(us,"ptmhed returned FAILURE"));
   uu_dexit;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : int ptmpat(nclkey, ipatch, buf)
**       Either append or replace a patch (IPATCH) in an NCL
**       mesh surface (NCLKEY) with the data in BUF.
**    PARAMETERS
**       INPUT  :
**          nclkey               UNIBASE key of mesh surface
**          ipatch               number of patch
**          buf                  patch data
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ptmpat(nclkey, ipatch, buf)
   UU_KEY_ID *nclkey;
   UM_int2 *ipatch;
   UM_real8 buf[];

   {
   int status;
   UU_KEY_ID key;
   int patch;
   int rel_num;
   struct NCL_meshsf_rec surf;
   struct NCL_mpatch_rec meshpatch;

   uu_denter(UU_MTRC,(us,"ptmpat(key=%x, ipatch=%d, buf=%x)", *nclkey,
      *ipatch, buf));

   key = *nclkey;
   patch = *ipatch;
   status = UU_SUCCESS;
   if (ur_retrieve_data_relnum(key, &rel_num) != 0)
      status = UU_FAILURE;
   else if (rel_num != NCL_MESHSURF_REL)
      status = UU_FAILURE;
   else
      {
      surf.key = key;
      if (ur_retrieve_data_fixed(&surf) != 0)
         status = UU_FAILURE;
      else
         {
         ncl_put_meshpatch(buf, &meshpatch);
         if (ur_update_data_varlist(surf.key, 1, &meshpatch, patch, 1) != 0)
            status = UU_FAILURE;
         }
      }

   uu_dexit;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : int ptqhed(buf, nclkey)
**       Create an NCL quilt surface entity in UNIBASE using the given
**       quilt surface header data (BUF) and return the UNIBASE key of
**       the newly created quilt surface entity.
**    PARAMETERS
**       INPUT  :
**          buf               quilt surface header data
**       OUTPUT :
**          nclkey            UNIBASE key of created entity
**    RETURNS      :
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ptqhed(buf, nclkey)
   UM_real8 buf[];
   UU_KEY_ID *nclkey;

   {
   int status;
   int i,j;
   struct NCL_quiltsf_rec surf;
   struct NCLI_quilthead_rec *quilt;

   uu_denter(UU_MTRC,(us,"ptqhed(buf=%x, nclkey=%x)",buf, nclkey));

   ur_setup_data(NCL_QUILTSURF_REL, &surf, sizeof(surf));

   /* initialize key to zero to force create */
   surf.key = 0;

   /* Initialize ruled-in-u/v variables */
   surf.rldnu = -1;
   surf.rldnv = 0;

   quilt = (struct NCLI_quilthead_rec *) buf;
   surf.surf_type = NCLI_QUILTSURF;
   surf.numpatches = quilt->numpatches;
   surf.offset = quilt->offset;
         surf.offdist = quilt->offdist;
   for (j=0; j<12; j++)
      for (i=0; i<3; i++) surf.midpt[j][i] = quilt->midpt[j][i];
   surf.no_qpatch = 0;

   status = ncl_create_entity((struct UC_entitydatabag *)&surf, 9); /* pass geo type. kathy */
   *nclkey = surf.key;

   if (status != UU_SUCCESS) uu_dprint(-1,(us,"ptqhed returned FAILURE"));
   uu_dexit;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : int ptqpat(nclkey, ipatch, buf)
**       Either append or replace a patch (IPATCH) in an NCL
**       quilt surface (NCLKEY) with the data in BUF.
**    PARAMETERS
**       INPUT  :
**          nclkey               UNIBASE key of quilt surface
**          ipatch               number of patch
**          buf                  patch data
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ptqpat(nclkey, ipatch, buf)
   UU_KEY_ID *nclkey;
   UM_int2 *ipatch;
   UM_real8 buf[];

   {
   int status;
   UU_KEY_ID key;
   int patch;
   int rel_num;
   struct NCL_quiltsf_rec surf;
   struct NCL_qpatch_rec quiltpatch;

   uu_denter(UU_MTRC,(us,"ptqpat(key=%x, ipatch=%d, buf=%x)", *nclkey,
      *ipatch, buf));

   key = *nclkey;
   patch = *ipatch;
   status = UU_SUCCESS;
   if (ur_retrieve_data_relnum(key, &rel_num) != 0)
      status = UU_FAILURE;
   else if (rel_num != NCL_QUILTSURF_REL)
      status = UU_FAILURE;
   else
      {
      surf.key = key;
      if (ur_retrieve_data_fixed(&surf) != 0)
         status = UU_FAILURE;
      else if ((patch < 1) || (patch > surf.no_qpatch+1))
         status = UU_FAILURE;
      else
         {
         ncl_put_quiltpatch(buf, &quiltpatch);
         if (patch <= surf.no_qpatch)
            {
            if (ur_update_data_varlist(surf.key, 1, &quiltpatch, patch, 1) != 0)
               status = UU_FAILURE;
            }
         else
            {
            if (ur_update_data_varlist(surf.key, 1, &quiltpatch,
                        surf.no_qpatch+1, 1) != 0)
               status = UU_FAILURE;
            }
         }
      }
   if (status != UU_SUCCESS) uu_dprint(-1,(us,"ptqpat returned FAILURE"));
   uu_dexit;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : int ptpnhd(buf, ityp, nclkey, ifirst)
**       Create an NCL patern entity in UNIBASE using the given
**       patern header data (BUF) and return the UNIBASE key of
**       the newly created patern entity.
**    PARAMETERS
**       INPUT  :
**          buf               patern header data
**          ityp              patern type
**          ifirst            = 0 - do nothing (dummy call needed by NCL501)
**       OUTPUT :
**          nclkey            UNIBASE key of created entity
**    RETURNS      :
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ptpnhd(buf, ityp, nclkey, ifirst)
   UM_real8 buf[];
   UU_KEY_ID *nclkey;
   UM_int2 *ifirst;
   int *ityp;

   {
   int status;
   struct NCL_patern_rec patern;

   uu_denter(UU_MTRC,(us,"ptpnhd(buf=%x, nclkey=%x)",buf, nclkey));

   if (*ifirst == 1)
      {
      ur_setup_data(NCL_PATERN_REL, &patern, sizeof(patern));

  /* initialize key to zero to force create */
      patern.key = 0;
      patern.no_patpnt = 0;
      patern.pntype = *ityp;  /* vp3.26.93 patern type */
      patern.markertype = UM_ptattr.markertype;

      status = ncl_create_entity((struct UC_entitydatabag *)&patern, 20); /* pass geo type for patern */
      *nclkey = patern.key;

      if (status != UU_SUCCESS) uu_dprint(-1,(us,"ptpnhd returned FAILURE"));
      }

   uu_dexit;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : int ptpnpt(buf, nclkey, ipt)
**       Either append or replace a point (IPT) in an NCL
**       patern (NCLKEY) with the data in BUF.
**    PARAMETERS
**       INPUT  :
**          nclkey               UNIBASE key of patern
**          ipt                  number of point
**          buf                  patch data
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ptpnpt(buf, nclkey, ipt)
   UM_real8 buf[];
   UU_KEY_ID *nclkey;
   UM_int2 *ipt;

   {
   int status;
   UU_KEY_ID key;
   int point;
   int rel_num;
   struct NCL_patern_rec patern;
   UU_REAL pntrec[6];
   int i,m;

   uu_denter(UU_MTRC,(us,"ptpnpt(key=%x, ipt=%d, buf=%x)", *nclkey,
      *ipt, buf));

   key = *nclkey;
   point = *ipt;
   status = UU_SUCCESS;
   if (ur_retrieve_data_relnum(key, &rel_num) != 0)
      status = UU_FAILURE;
   else if (rel_num != NCL_PATERN_REL)
      status = UU_FAILURE;
   else
      {
      patern.key = key;
      if (ur_retrieve_data_fixed(&patern) != 0)
         status = UU_FAILURE;
      else if ((point < 1) || (point > patern.no_patpnt+1))
         status = UU_FAILURE;
      else
         {
         m    = patern.pntype * 3; /* vp3.26.93 PT or PV support */ 
         for (i=0; i<m; i++) pntrec[i]=buf[i];
         point = (point - 1) * m + 1;
         if (ur_update_data_varlist(patern.key, 1, pntrec, point, m) != 0)
               status = UU_FAILURE;
         }
      }
   if (status != UU_SUCCESS) uu_dprint(-1,(us,"ptpnpt returned FAILURE"));
   uu_dexit;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : int ptntsf(buf, nclkey)
**       Create a net NCL surface in UNIBASE and return the UNIBASE
**       key of the surface. The surface is completely created.
**    PARAMETERS
**       INPUT  :
**          buf                  NCLI buffer of surface
**       OUTPUT :
**          nclkey               UNIBASE key of surface
**    RETURNS      :
**       UU_SUCCESS iff no error
**       UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ptntsf(buf, nclkey)
   UM_real8 buf[];
   UU_KEY_ID *nclkey;

   {
   int status,rel_num,i,j;
   UM_int2 i2;
   UU_KEY_ID key1, key2;
   struct NCL_netsf_rec surf;
   struct NCLI_netsf_rec *srfhead;

   uu_denter(UU_MTRC,(us,"ptntsf(buf=%x, nclkey=%x)",buf, nclkey));

   ur_setup_data(NCL_NETSF_REL, &surf, sizeof(surf));

   /* initialize key to zero to force create */
   surf.key = 0;

   surf.surf_type = NCLI_NETSF;
   surf.no_netkey = 0;

   srfhead = (struct NCLI_netsf_rec *) buf;

   for (i=0;i<srfhead->numsfs;i++)
      for (j=0;j<4;j++)
         surf.bndsfs[i][j]=srfhead->bndsfs[i][j];

   status = ncl_create_entity((struct UC_entitydatabag *)&surf, 9); /* pass geo type for surface. kathy */
   *nclkey = surf.key;

   i = 0;
/*
.....vp 11-aug-97 turn of label maker, force @UN for all subsurfaces
*/
   i2 = 1;
   stunlb(&i2);
   while (i<srfhead->numsfs && status == UU_SUCCESS)
      {
      key1 = srfhead->sfkeys[i];
      ur_retrieve_data_relnum(key1, &rel_num);
      switch (rel_num)
         {
         case NCL_SURF_REL:
            status = ncl_net_nclsf(key1, &key2);
            break;
         case NCL_REVSURF_REL:
            status = ncl_net_revsf(key1, &key2);
            break;
         case NCL_MESHSURF_REL:
            status = ncl_net_mshsf(key1, &key2);
            break;
         case NCL_EVALSF_REL:
            status = ncl_net_evalsf(key1, &key2); 
            break;
         case UM_RBSPLSRF_REL:
            status = ncl_net_rbsf(key1, &key2);
            break;
         case UM_AGSRF_REL:
            status = ncl_net_agsrf(key1, &key2);
            break;
         case NCL_TRIMSF_REL:
            status = ncl_net_trimsf(key1, &key2);
            break; 
		 case NCL_NETSF_REL:
            status = ncl_net_netsf(key1, &key2);
            break;
         default:
            status = UU_FAILURE;
            break;
         }
         if (ur_update_data_varlist(surf.key, 1, &key2,
                         i+1, 1) != 0)
            status = UU_FAILURE;
         i++;
      }

   stunlb(&i2);
   uu_dexit;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : int ptshap(buf, nclkey, nw)
**       Create an NCL shape in UNIBASE and return the UNIBASE
**       key of the shape.
**    PARAMETERS
**       INPUT  :
**          buf                  NCLI buffer of shape
**          nw                   Number of words in shape entity.
**       OUTPUT :
**          nclkey               UNIBASE key of shape
**    RETURNS      :
**       UU_SUCCESS iff no error
**       UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ptshap(buf, nclkey, nw)
   UM_real8 buf[];
   UM_int4 *nclkey;
   UM_int2 *nw;

   {
   int status,nwds;
	UM_int2 ifl,val;
   struct NCL_shape_rec shape;

   uu_denter(UU_MTRC,(us,"ptshap(buf=%x, nclkey=%x)",buf, nclkey));

	shape.key=*nclkey;

	ur_retrieve_data_relnum(shape.key, &shape.rel_num); 
	if (shape.rel_num==NCL_SHAPE_REL)
	{
/*
.....If using the NSHAPE command to get here, then the shape has already been
.....created and we need to skip down past ncl_create_entity.
.....JLS 9-18-1998
*/
		if (ncl_retrieve_data(&shape,sizeof(struct NCL_shape_rec))==UU_SUCCESS) 
			goto jump; 
	}

   ur_setup_data(NCL_SHAPE_REL, &shape, sizeof(shape));

/*   shape.no_shdata = 0; */
   /* initialize key to zero to force create */
   shape.key = 0;

	ifl = 344;
	getifl(&ifl,&val);
	shape.f2d3d = val;
/*	if (shape.f2d3d <2)
		shape.shaded = 0;*/

   status = ncl_create_entity((struct UC_entitydatabag *)&shape, 18); /* pass geo type for shape. kathy */
   *nclkey = shape.key;

jump:;
   nwds=(*nw);
   if (ur_update_data_varlist(shape.key, 1, buf, 1, nwds) != 0)
            status = UU_FAILURE;

   uu_dexit;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : int upsfuv(nclkey, kuvs)
**       Update the U & V values for NCL, mesh, RBspline or trim SURF.
**    PARAMETERS
**       INPUT  :
**          nclkey               UNIBASE key of surface
**          uval                 New U value
**          vval                 New V value
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
upsfuv (nclkey, kuvs)
   UU_KEY_ID nclkey;
   UM_int4 kuvs[];

   {
   int status;
   UU_KEY_ID key;
   int u_val, v_val, u_pts, v_pts;
   int rel_num;
   struct NCL_fixed_databag e;
   struct NCL_surface_rec *surf;
	struct UM_surfattr_rec surfattr;

   uu_denter(UU_MTRC,(us,"upsfuv(key=%x, uval=%d, vval=%x)", nclkey,
      uval, vval));

   surf = (struct NCL_surface_rec *)&e;
   key = nclkey;
   u_pts = kuvs[0];
   u_val = kuvs[1];
   v_pts = kuvs[2];
   v_val = kuvs[3];

   status = UU_SUCCESS;
   if (ur_retrieve_data_relnum(key, &rel_num) != 0)
      status = UU_FAILURE;
   else if (rel_num != NCL_SURF_REL && rel_num != UM_RBSPLSRF_REL &&
            rel_num != NCL_TRIMSF_REL && rel_num != NCL_MESHSURF_REL &&
            rel_num != NCL_REVSURF_REL)
      status = UU_FAILURE;
   else
      {
      surf->key = key;
      if (ur_retrieve_data_fixed(surf) !=0)
          status = UU_FAILURE;
      else
		{
			surfattr.key = surf->key;
			status = ur_retrieve_attr(&surfattr);
			if (status == UU_SUCCESS)
			{
				surfattr.numupaths = u_val;
				surfattr.numvpaths = v_val;
				surfattr.ptsperucrv = u_pts;
				surfattr.ptspervcrv = v_pts;
/*
..... we recalculate the tessellation when the number of points on
..... u- and v- curves is set in a DISPLY/sf1,10,10,12,12 command
*/
				ncl_lst_delete(TESSELLATION_LIST,&surf->key);
				ncl_set_tess_parms (1,0.,u_pts,v_pts);

				ur_update_data_fixed(surf);
				status = ur_update_attr(&surfattr);
			}
		}
	}
   if (status != UU_SUCCESS) uu_dprint(-1,(us,"upsfuv returned FAILURE"));
   uu_dexit;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : int upsurf (nclkey, kuvs)
**       Update the display U & V values for general surface.
**    PARAMETERS
**       INPUT  :
**          nclkey               UNIBASE key of surface
**          kuvs                 D4 int array of U-pts, U-lines, 
**                               V-pts & V-lines
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
upsurf (nclkey, kuvs)
   UU_KEY_ID *nclkey;
   UM_int4 kuvs[];

   {
   int status;
   UU_KEY_ID key;
   int rel_num;
   struct NCL_netsf_rec netsf;
   UU_KEY_ID sfkey;
   int i, nsuf;

   uu_denter(UU_MTRC,(us,"upsurf(key=%x, uval=%d, vval=%x)", nclkey,
      kuvs[0], kuvs[1]));

   key = *nclkey;
   status = UU_SUCCESS;
   ncl_get_tess_parms (&tesstyp,&tesstol,&kupts,&kvpts);
   if (ur_retrieve_data_relnum(key, &rel_num) != 0)
      status = UU_FAILURE;
   else if (rel_num != NCL_NETSF_REL)
      status = upsfuv (key,kuvs);
   else
      {
      netsf.key = key;
      if (ur_retrieve_data_fixed(&netsf) !=0)
          status = UU_FAILURE;
      else
          {
           nsuf = netsf.no_netkey;
           for (i=1; i<=nsuf; i++)
              {
               if (ur_retrieve_data_varlist(key, 1, &sfkey, i, 1) == 0)
                   status = upsfuv (sfkey,kuvs);
               else
                   status = UU_FAILURE;
               if (status == UU_FAILURE) goto done;
              }
          }
      }
done:
   if (status != UU_SUCCESS) uu_dprint(-1,(us,"upmsuv returned FAILURE"));
   uu_dexit;
   return (status);
   }    

/*********************************************************************
**    E_FUNCTION     : void upsrst
**       Reset the original tessellation parameters.
*********************************************************************/
void upsrst()
{
   ncl_set_tess_parms (tesstyp,tesstol,kupts,kvpts);
}
