/*********************************************************************
**    NAME         :  nget.c
**       CONTAINS: data retrieval routines for mapping UNIBASE entities
**                   to the data representation for NCL; these routines
**                   are FORTRAN callable and hence follow the standard
**                   FORTRAN/C interface conventions
**       int gtgeo1(nclkey, buf)
**       int gtreln(nclkey, relnum)
**       int gtncsg(nclkey, nsegs)
**       int gtcvhd(nclkey, buf)
**       int gtcvsg(nclkey, iseg, buf)
**       int gtshed(nclkey, type, nopanel) ** COMMENTED OUT **
**       int gtspan(nclkey, ispan, buf)
**       int gtspat(nclkey, ispan, ipatch, buf)
**       int gtmpat(nclkey, ipatch, buf)
**       int gtqpat(nclkey, ipatch, buf)
**       int gtpnnp1(nclkey, npts)
**       int gtpnpt1(buf, nclkey, ipoint)
**       int gtshap1(buf, nclkey)
**		 int gtcir(nclkey, buf)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       nget.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:09:01
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "class.h"
#include "mfort.h"
#include "mdrel.h"
#include "modef.h"
#include "mcrv.h"

#include "nccs.h"
#include "ncl.h"
#include "nclfc.h"

/*********************************************************************
**    E_FUNCTION     : int gtgeo1(nclkey, buf)
**       Retrieve the NCL  geometric representation for any of the
**       legal NCL entities.
**    PARAMETERS
**       INPUT  :
**          nclkey            UNIBASE key of the entity
**       OUTPUT :
**          buf               buffer to place the data
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
gtgeo1(nclkey, buf)
   UU_KEY_ID *nclkey;
   UM_real8 buf[];

   {
   int status;
   UU_KEY_ID key;
   int rel_num;
   struct NCL_fixed_databag e;
   UU_LOGICAL ncl_legal_relation();

   uu_denter(UU_MTRC,(us,"gtgeo1(nclkey=%x, buf=%x)", *nclkey, buf));
   key = *nclkey;
   ur_retrieve_data_relnum(key, &rel_num);
   status = UU_FAILURE;
   if (ncl_legal_relation(rel_num))
      {
      /*status = uc_retrieve_data(key, &e);*/
      e.key = key;
      if (ur_retrieve_data_fixed(&e)!= 0)
         status = UU_FAILURE;
      else
         {
         status = UU_SUCCESS;
         switch (rel_num)
            {
            case UM_POINT_REL:
               ncl_unipt_to_nclpt(&e, buf);
               break;
            case UM_LINE_REL:
               ncl_uline_to_nclline(&e, buf);
               break;
            case UM_CIRCLE_REL:
               ncl_ucirc_to_nclcirc(&e, buf,UU_FALSE);
               break;
/*            case UM_PLN_REL:                  */
/*               ncl_uplane_to_nclpln(&e, buf); */
/*               break;                         */
            case NCL_VECTOR_REL:
               ncl_vector_to_nclvec(&e, buf);
               break;
            case NCL_POINT_REL:
               ncl_point_to_nclpt(&e, buf);
               break;
            case NCL_LINE_REL:
               ncl_line_to_nclline(&e, buf);
               break;
            case NCL_CIRCLE_REL:
               ncl_circ_to_nclcirc(&e, buf);
               break;
            case NCL_MATRIX_REL:
               ncl_matrix_to_nclmatrix(&e, buf);
               break;
            case NCL_CURVE_REL:
               ncl_curve_to_nclcrv(&e, buf);
               break;
            case NCL_PLN_REL:
               ncl_plane_to_nclpln(&e, buf);
               break;
            case NCL_SURF_REL:
               ncl_get_srfheader(&e, buf);
               break;
            case NCL_MESHSURF_REL:
               ncl_get_meshheader(&e, buf);
               break;
            case NCL_QUILTSURF_REL:
               ncl_get_quiltheader(&e, buf);
               break;
            case NCL_NETSF_REL:
               ncl_get_netsf(&e, buf);
               break;
            case NCL_PATERN_REL:
               ncl_patern_to_nclpatern(&e, buf);
               break;
            case NCL_SCALAR_REL:
               ncl_get_scalar(&e, buf);
               break;
            case NCL_POINTVEC_REL:
               ncl_get_pointvec(&e, buf);
               break;
            case NCL_SHAPE_REL:
               status = UU_FAILURE;
               break;
            default:
               status = UU_FAILURE;
               break;
            }
         }
      }
   if (status != UU_SUCCESS) uu_dprint(-1,(us,"gtgeo1 returned FAILURE"));
   uu_dexit;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : int gtreln(nclkey,relnum)
**       Returns the Relation Number associated with the Unibase Key.
**    PARAMETERS
**       INPUT  :
**          nclkey            UNIBASE key of the entity
**       OUTPUT :
**          relnum            Relation number of entity
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void gtreln (nclkey,relnum)
UM_int4 *nclkey;
UM_int4 *relnum;
{
/*
.....Get relation number of entity
*/
	ur_retrieve_data_relnum(*nclkey,relnum);
}

/*********************************************************************
**    E_FUNCTION     : int gtncsg(nclkey, nsegs)
**       Retrieve number of segments in an NCL curve.
**    PARAMETERS
**       INPUT  :
**          nclkey            UNIBASE key of the entity
**       OUTPUT :
**          nsegs             number of segments
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
gtncsg (nclkey, nsegs)
   UM_int4 *nclkey;
   UM_int2 *nsegs;

   {
   int status;
   int rel_num;
   struct NCL_curve_rec e;

   uu_denter(UU_MTRC,(us,"gtncsg(nclkey=%x)", *nclkey));

   *nsegs = 0;
   e.key = *nclkey;
   status = UU_FAILURE;
   if (ur_retrieve_data_relnum(e.key, &rel_num) == 0 && rel_num == NCL_CURVE_REL)
      {
      if (ur_retrieve_data_fixed(&e) == 0)
         {
         *nsegs = e.no_param + 1;
         status = UU_SUCCESS;
         }
      }
 
   uu_dexit;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : int gtcvhd(nclkey, buf)
**       Retrieve the defining data for a curve header for the
**       specified curve (NCLKEY).
**    PARAMETERS
**       INPUT  :
**          nclkey            UNIBASE key of the entity
**       OUTPUT :
**          buf               buffer to place the data
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
gtcvhd(nclkey, buf)
   UU_KEY_ID *nclkey;
   UM_real8 buf[];

   {
   int status;
   UU_KEY_ID key;
   int rel_num;
   struct NCL_curve_rec e;
   float param[100];
   int i;

   uu_denter(UU_MTRC,(us,"gtcvhd(nclkey=%x, buf=%x)", *nclkey, buf));
   key = *nclkey;
   status = UU_FAILURE;
   if (ur_retrieve_data_relnum(key, &rel_num) != 0)
      status = UU_FAILURE;
   else if (rel_num == NCL_CURVE_REL)
      {
      e.key = key;
      if (ur_retrieve_data_fixed(&e) != 0)
         status = UU_FAILURE;
      else if (ur_retrieve_data_varlist(e.key, 1, param, 1, e.no_param) != 0)
         status = UU_FAILURE;
      else
         {
         ncl_get_crvhead(e.no_param, param, buf);
         status = UU_SUCCESS;
         }
      }
   if (status != UU_SUCCESS) uu_dprint(-1,(us,"gtcvhd returned FAILURE"));
   uu_dexit;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : int gtcvsg(nclkey, iseg, buf)
**       Retrieve the data for defining a segment (ISEG) for an
**       NCL curve (NCLKEY).
**    PARAMETERS
**       INPUT  :
**          nclkey            UNIBASE key of the entity
**          iseg              retrieve data for this segment
**       OUTPUT :
**          buf               buffer to place the data
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
gtcvsg(nclkey, iseg, buf)
   UU_KEY_ID *nclkey;
   UM_int2  *iseg;
   UM_real8 buf[];

   {
   int status;
   int segment;
   int rel_num;
   UU_KEY_ID key;
   struct NCL_curve_rec e;
   struct NCL_segment_rec segdata;
   struct NCLI_crvseg_rec *crvseg;
   int i;

   uu_denter(UU_MTRC,(us,"gtcvsg(nclkey=%x, iseg=%d, buf=%x)",
      *nclkey, *iseg, buf));
   key = *nclkey;
   segment = *iseg;
   if (ur_retrieve_data_relnum(key, &rel_num) != 0)
      status = UU_FAILURE;
   else if (rel_num != NCL_CURVE_REL)
      status = UU_FAILURE;
   else
      {
      e.key = key;
      if (ur_retrieve_data_fixed(&e) != 0)
         status = UU_FAILURE;
      else if ((segment < 1) || (segment > e.no_segment))
         status = UU_FAILURE;
      else
         {
         if (ur_retrieve_data_varlist(e.key, 2, &segdata, segment, 1) != 0)
            status = UU_FAILURE;
         else
            {
            ncl_get_crvseg(&segdata, buf);
            status = UU_SUCCESS;
            }
         }
      }
   if (status != UU_SUCCESS) uu_dprint(-1,(us,"gtcvsg returned FAILURE"));
   uu_dexit;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : int gtshed(nclkey, type, nopanel)
**       Retrieve the defining data for a surface header for the
**       specified surface (NCLKEY).
**    PARAMETERS
**       INPUT  :
**          nclkey            UNIBASE key of the entity
**       OUTPUT :
**          type              surface type
**          nopanel           number of panels
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
#ifdef OLDNCL
int
gtshed(nclkey, type, nopanel)
   UU_KEY_ID *nclkey;
   UM_int2 *type;
   UM_int2 *nopanel;

   {
   int status;
   UU_KEY_ID key;
   int rel_num;
   struct NCL_surface_rec e;

   uu_denter(UU_MTRC,(us,"gtshed(nclkey=%x)", *nclkey));
   key = *nclkey;
   status = UU_FAILURE;
   if (ur_retrieve_data_relnum(key, &rel_num) != 0)
      status = UU_FAILURE;
   else if (rel_num == NCL_SURF_REL)
      {
      e.key = key;
      if (ur_retrieve_data_fixed(&e) != 0)
         status = UU_FAILURE;
      else
         {
         *type = e.surf_type;
         *nopanel = e.no_panelkey;
         status = UU_SUCCESS;
         }
      }
   if (status != UU_SUCCESS) uu_dprint(-1,(us,"gtshed returned FAILURE"));
   uu_dexit;
   return (status);
   }
#endif

/*********************************************************************
**    E_FUNCTION     : int gtspan(nclkey, ispan, buf)
**       Retrieve the panel header data for the specified span (ISPAN)
**       of the surface (NCLKEY).
**    PARAMETERS
**       INPUT  :
**          nclkey            UNIBASE key of the surface entity
**          ispan             span to get panel header
**       OUTPUT :
**          buf               buffer to place the data
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
gtspan(nclkey, ispan, buf)
   UU_KEY_ID *nclkey;
   UM_int2  *ispan;
   UM_real8 buf[];

   {
   int status;
   int span;
   int rel_num;
   UU_KEY_ID key;
   UU_KEY_ID panelkey;
   struct NCL_panel_rec panel;
   struct NCLI_crvseg_rec *crvseg;
   int i;

   uu_denter(UU_MTRC,(us,"gtspan(nclkey=%x, ispan=%d, buf=%x)",
      *nclkey, *ispan, buf));
   key = *nclkey;
   span = *ispan;
   if (ur_retrieve_data_relnum(key, &rel_num) != 0)
      status = UU_FAILURE;
   else if (rel_num != NCL_SURF_REL)
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
            status = UU_SUCCESS;
            }
         }
      }
   if (status != UU_SUCCESS) uu_dprint(-1,(us,"gtspan returned FAILURE"));
   uu_dexit;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : int gtspat(nclkey, ispan, ipatch, buf)
**       Retrieve the data defining a patch (IPATCH) within a
**       span (ISPAN) of an NCL surface entity.
**    PARAMETERS
**       INPUT  :
**          nclkey            UNIBASE key of the entity
**          ispan             span within surface
**          ipatch            patch within span
**       OUTPUT :
**          buf               buffer to place the data
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
gtspat(nclkey, ispan, ipatch, buf)
   UU_KEY_ID *nclkey;
   UM_int2  *ispan;
   UM_int2  *ipatch;
   UM_real8 buf[];

   {
   int status;
   int span;
   int patch;
   int rel_num;
   UU_KEY_ID key;
   UU_KEY_ID panelkey;
   struct NCL_surface_rec surf;
   struct NCL_panel_rec panel;
   struct NCL_patch_rec patchdata;

   uu_denter(UU_MTRC,(us,"gtspat(nclkey=%x, ispan=%d, ipatch=%d, buf=%x)",
      *nclkey, *ispan, *ipatch, buf));
   key = *nclkey;
   span = *ispan;
   patch = *ipatch;
   if (ur_retrieve_data_relnum(key, &rel_num) != 0)
      status = UU_FAILURE;
   else if (rel_num != NCL_SURF_REL)
      status = UU_FAILURE;
   else
      {
      surf.key = key;
      if (ur_retrieve_data_fixed(&surf) != 0)
         status = UU_FAILURE;
      else if ((span < 1) || (span > surf.no_panelkey))
         status = UU_FAILURE;
      else
         {
         if (ur_retrieve_data_varlist(surf.key, 1, &panelkey, span, 1) != 0)
            status = UU_FAILURE;
         else
            {
            panel.key = panelkey;
            if(ur_retrieve_data_fixed(&panel) != 0) status = UU_FAILURE;
            else
               {
               if (ur_retrieve_data_varlist(panelkey, 1, &patchdata, patch, 1)
                    != 0) status = UU_FAILURE;
               else
                  {
                  ncl_get_srfpatch(panel.type, &patchdata, buf);
                  status = UU_SUCCESS;
                  }
               }

            }
         }
      }
   if (status != UU_SUCCESS) uu_dprint(-1,(us,"gtspat returned FAILURE"));
   uu_dexit;
   return (status);
   }
/*********************************************************************
**    E_FUNCTION     : int gtmpat(nclkey, ipatch, buf)
**       Retrieve the data defining a patch (IPATCH) of an NCL mesh
**       surface entity.
**    PARAMETERS
**       INPUT  :
**          nclkey            UNIBASE key of the entity
**          ipatch            patch to retrieve
**       OUTPUT :
**          buf               buffer to place the data
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
gtmpat(nclkey, ipatch, buf)
   UU_KEY_ID *nclkey;
   UM_int2  *ipatch;
   UM_real8 buf[];

   {
   int status;
   int patch;
   struct NCL_meshsf_rec surf;
   struct NCL_mpatch_rec patchdata;

   uu_denter(UU_MTRC,(us,"gtmpat(nclkey=%x, ipatch=%d, buf=%x)",
      *nclkey, *ipatch, buf));

   surf.key = *nclkey;
   patch = *ipatch;
   if (ur_retrieve_data_relnum(surf.key, &surf.rel_num) != 0)
      status = UU_FAILURE;
   else if (surf.rel_num != NCL_MESHSURF_REL)
      status = UU_FAILURE;
   else
      {
      if (ur_retrieve_data_fixed(&surf) != 0)
         status = UU_FAILURE;
      else if ((patch < 1) || (patch > surf.no_mpatch))
         status = UU_FAILURE;
      else
         {
         if (ur_retrieve_data_varlist(surf.key, 1, &patchdata, patch, 1)
              != 0) status = UU_FAILURE;
         else
            {
            ncl_get_meshpatch(&patchdata, buf);
            status = UU_SUCCESS;
            }
         }
      }
   if (status != UU_SUCCESS) uu_dprint(-1,(us,"gtmpat returned FAILURE"));
   uu_dexit;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : int gtqpat(nclkey, ipatch, buf)
**       Retrieve the data defining a patch (IPATCH) of a NCL
**       quilt entity (NCLKEY).
**    PARAMETERS
**       INPUT  :
**          nclkey            UNIBASE key of the entity
**          ipatch            patch number to retrieve
**       OUTPUT :
**          buf               buffer to place the data
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
gtqpat(nclkey, ipatch, buf)
   UU_KEY_ID *nclkey;
   UM_int2  *ipatch;
   UM_real8 buf[];

   {
   int status;
   int patch;
   struct NCL_quiltsf_rec surf;
   struct NCL_qpatch_rec patchdata;

   uu_denter(UU_MTRC,(us,"gtqpat(nclkey=%x, ipatch=%d, buf=%x)",
      *nclkey, *ipatch, buf));

   surf.key = *nclkey;
   patch = *ipatch;
   if (ur_retrieve_data_relnum(surf.key, &surf.rel_num) != 0)
      status = UU_FAILURE;
   else if (surf.rel_num != NCL_QUILTSURF_REL)
      status = UU_FAILURE;
   else
      {
      if (ur_retrieve_data_fixed(&surf) != 0)
         status = UU_FAILURE;
      else if ((patch < 1) || (patch > surf.no_qpatch))
         status = UU_FAILURE;
      else
         {
         if (ur_retrieve_data_varlist(surf.key, 1, &patchdata, patch, 1)
              != 0) status = UU_FAILURE;
         else
            {
            ncl_get_quiltpatch(&patchdata, buf);
            status = UU_SUCCESS;
            }
         }
      }
   if (status != UU_SUCCESS) uu_dprint(-1,(us,"gtqpat returned FAILURE"));
   uu_dexit;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : int gtpnnp1(nclkey, npts, ntyp)
**       Return the type & number of points in an NCL patern.
**    PARAMETERS
**       INPUT  :
**          nclkey            UNIBASE key of the entity
**       OUTPUT :
**          npts              number of points.
**          ntyp              type of patern (1 - PT, 2 - PV).
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
gtpnnp1(nclkey, npts, ntyp)
   UU_KEY_ID *nclkey;
   UM_int2  *npts;
   int *ntyp;

   {
   int status;
   struct NCL_patern_rec patern;

   uu_denter(UU_MTRC,(us,"gtpnnp(nclkey=%x, npts=%d)",
      *nclkey, *npts));

   patern.key = *nclkey;
   if (ur_retrieve_data_relnum(patern.key, &patern.rel_num) != 0)
      status = UU_FAILURE;
   else if (patern.rel_num != NCL_PATERN_REL)
      status = UU_FAILURE;
   else
      {
      if (ur_retrieve_data_fixed(&patern) != 0)
         status = UU_FAILURE;
      else 
         *ntyp = patern.pntype;
         *npts = patern.no_patpnt / (patern.pntype * 3);
      }
   if (status != UU_SUCCESS) uu_dprint(-1,(us,"gtpnnp returned FAILURE"));
   uu_dexit;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : int gtpnpt1(buf, ityp, nclkey, ipoint)
**       Retrieve the data defining a point (IPOINT) of an NCL patern
**       entity.
**    PARAMETERS
**       INPUT  :
**          nclkey            UNIBASE key of the entity
**          ipoint            point to retrieve
**       OUTPUT :
**          buf               buffer to place the data
**          ityp              type of patern (1 - point, 2 - pntvec)
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
gtpnpt1(buf, ityp, nclkey, ipoint)
   UM_real8 buf[];
   UU_KEY_ID *nclkey;
   UM_int2  *ipoint;
   int *ityp;

   {
   int status;
   int point;
   struct NCL_patern_rec patern;
   UU_REAL pntrec[6];
   int i,m;

   uu_denter(UU_MTRC,(us,"gtpnpt(nclkey=%x, ipoint=%d, buf=%x)",
      *nclkey, *ipoint, buf));

   patern.key = *nclkey;
   point = *ipoint;
   if (ur_retrieve_data_relnum(patern.key, &patern.rel_num) != 0)
      status = UU_FAILURE;
   else if (patern.rel_num != NCL_PATERN_REL)
      status = UU_FAILURE;
   else
      {
      if (ur_retrieve_data_fixed(&patern) != 0)
         status = UU_FAILURE;
      else if ((point < 1) || (point > patern.no_patpnt))
         status = UU_FAILURE;
      else
         {
          *ityp = patern.pntype;
          m    = *ityp * 3;
          point = (point - 1) * m + 1;
          if (ur_retrieve_data_varlist(patern.key, 1, pntrec, point, m)
              != 0) status = UU_FAILURE;
          else
            {
             for (i=0; i<m; i++) buf[i]=pntrec[i];
             status = UU_SUCCESS;
            }
         }
      }
   if (status != UU_SUCCESS) uu_dprint(-1,(us,"gtpnpt returned FAILURE"));
   uu_dexit;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : int gtshap1(buf, nclkey)
**       Retrieve an NCL shape from the unibase.
**    PARAMETERS
**       INPUT  :
**          nclkey            UNIBASE key of the entity
**       OUTPUT :
**          buf               buffer to place the data
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
gtshap1(buf, nclkey)
   UM_real8 buf[];
   UM_int4 *nclkey;

   {
   int status;
   struct NCL_shape_rec shape;
   int i;

   uu_denter(UU_MTRC,(us,"gtshap(nclkey=%x, buf=%x)", *nclkey, buf));

   status = UU_SUCCESS;
   shape.key = *nclkey;
   if (ur_retrieve_data_relnum(shape.key, &shape.rel_num) != 0)
      status = UU_FAILURE;
   else if (shape.rel_num != NCL_SHAPE_REL)
      status = UU_FAILURE;
   else
      {
      if (ur_retrieve_data_fixed(&shape) != 0)
         status = UU_FAILURE;
      else
         {
         if (ur_retrieve_data_varlist(shape.key, 1, buf, 1, shape.no_shapwd)
              != 0) status = UU_FAILURE;
/*
.....Need to get all the information about shape, including shape.displst.
*/
			else
				{
				ncl_retrieve_data(&shape,sizeof(struct NCL_shape_rec));
				ur_update_data_varlist(shape.key,2,shape.displst,1,shape.no_displst);
				}
				
         }
      }
   if (status != UU_SUCCESS) uu_dprint(-1,(us,"gtshap returned FAILURE"));
   uu_dexit;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : int gtcir(nclkey, buf)
**       Retrieve the NCL circle geometric representation.
**    PARAMETERS
**       INPUT  :
**          nclkey            UNIBASE key of the entity
**       OUTPUT :
**          buf               buffer to place the data
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
gtcir(nclkey, buf)
   UU_KEY_ID *nclkey;
   UM_real8 buf[];
{
	int status;
	UU_KEY_ID key;
	int rel_num;
	UU_REAL tmp;
	struct NCL_fixed_databag e;
	struct UM_circle_rec *ci;
	UU_LOGICAL ncl_legal_relation();
	UU_LOGICAL lcirijk = UU_FALSE;

	uu_denter(UU_MTRC,(us,"gtcir(nclkey=%x, buf=%x)", *nclkey, buf));
	key = *nclkey;
	ur_retrieve_data_relnum(key, &rel_num);
	status = UU_FAILURE;
	if (ncl_legal_relation(rel_num))
	{
		e.key = key;
		if (ur_retrieve_data_fixed(&e)!= 0)
			status = UU_FAILURE;

		if (rel_num == UM_CIRCLE_REL)
		{
			status = UU_SUCCESS;
			ci = (struct UM_circle_rec *) &e;		
			if (fabs(UM_TWOPI - fabs(ci->dang)) < UM_FUZZ)
				lcirijk = UU_TRUE;
			ncl_ucirc_to_nclcirc(&e, buf, lcirijk);
		}
		else
			status = UU_FAILURE;
	}

	if (status != UU_SUCCESS) uu_dprint(-1,(us,"gtcir returned FAILURE"));
	uu_dexit;
	return (status);
}
