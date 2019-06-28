/*********************************************************************
**    NAME         :  nepl.c
**       CONTAINS: routines to handle NCL planes
**       int ncl_plane_to_nclpln(e, ncl)
**       int ncl_uplane_to_nclpln(e, ncl)
**       int ncl_nclpln_to_plane(ncl, e)
**       ncl_p_nclpln(ncl)
**       int ncl_plane_transl(e, offset)
**       int projpt(nclkey, buf)
**       int ncl_nclpln_to_pln(e, pln)
**       int ncl_plane_to_vector(level, pickpath, pickloc, vec)
**       int ncl_plane_to_coord(level, pickpath, pickloc, pt)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       nepl.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:40
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "mfort.h"
#include "mdrel.h"
#include "mdebug.h"
#include "mdcoord.h"
#include "msrf.h"
#include "mdpick.h"		/* define picking stuff */
#include "dasnog.h"		/* defined UD_PLANE_REC for ncl_nclpln_to_pln() */

#include "ncl.h"
#include "nccs.h"
#include "nclfc.h"
/*********************************************************************
**    E_FUNCTION     : int ncl_plane_to_nclpln(e, ncl)
**       Convert a UNICAD plane to an NCL plane.
**    PARAMETERS   
**       INPUT  : 
**          e                 UNICAD plane entity
**       OUTPUT :  
**          ncl               buffer to place NCL plane
**    RETURNS      : 
**       UU_SUCCESS iff no error
**       UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_plane_to_nclpln(e, ncl)
   struct NCL_nclpl_rec *e;
   struct NCLI_plane_rec *ncl;

   {
   int status;
   UU_REAL dist;

   uu_denter(UU_MTRC,(us,"ncl_plane_to_nclpln(key=%x, ncl=%x)",
      e->key, ncl));

   status = UU_SUCCESS;
   dist = um_dot(e->nvec, e->pt);
   ncl_uureal_to_real8(3, e->nvec, ncl->ijk);
   ncl_uureal_to_real8(1, &dist, &ncl->dist);
   uu_dexit;
   return (status);
   }


/*********************************************************************
**    E_FUNCTION     : int gtdpt(nclkey, buf)
**       Get the plane's point into buf.
**    PARAMETERS   
**       INPUT  : 
**          nclkey
**       OUTPUT :  
**          buf
**    RETURNS      : 
**       UU_SUCCESS iff no error
**       UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
gtdpt(nclkey,buf)
   UU_KEY_ID *nclkey;
   UM_coord buf;

   {
   struct NCL_nclpl_rec e;

   uu_denter(UU_MTRC,(us,"gtdpt(buf=%x)", buf));

   e.key = *nclkey;
   ur_retrieve_data(&e,sizeof(e));
   um_vctovc(e.pt, buf);

   uu_dexit;
   }

/*********************************************************************
**    E_FUNCTION     : int ncl_uplane_to_nclpln(e, ncl)
**       Convert a UNICAD plane to an NCL plane.
**    PARAMETERS   
**       INPUT  : 
**          e                 UNICAD plane entity
**       OUTPUT :  
**          ncl               buffer to place NCL plane
**    RETURNS      : 
**       UU_SUCCESS iff no error
**       UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_uplane_to_nclpln(e, ncl)
   struct NCL_nclpl_rec *e;
   struct NCLI_plane_rec *ncl;

   {
   int status;
   UU_REAL dist;

   uu_denter(UU_MTRC,(us,"ncl_plane_to_nclpln(key=%x, ncl=%x)",
      e->key, ncl));

   status = UU_SUCCESS;
   dist = um_dot(e->nvec, e->pt);
   ncl_uureal_to_real8(3, e->nvec, ncl->ijk);
   ncl_uureal_to_real8(1, &dist, &ncl->dist);
   uu_dexit;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : int ncl_nclpln_to_plane(ncl, e)
**       Convert an NCL plane to a UNICAD plane.
**    PARAMETERS   
**       INPUT  : 
**          ncl                  buffer holding NCL plane
**       OUTPUT :  
**          e                    UNICAD plane entity
**    RETURNS      : 
**       UU_SUCCESS iff no error
**       UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_nclpln_to_plane(ncl, e)
   struct NCLI_plane_rec *ncl;
   struct NCL_nclpl_rec *e;

   {
   int status;
   UU_REAL dist;

   uu_denter(UU_MTRC,(us,"ncl_nclpln_to_plane(ncl=%x, e=%x)",
      ncl, e));
   status = UU_SUCCESS;
   e->rel_num = NCL_PLN_REL;
   e->radius = 0.0;
   dist = ncl->dist;
   ncl_real8_to_uureal(3, ncl->ijk, e->nvec);
   um_vctmsc(e->nvec, dist, e->pt);
   sprintf(UM_sbuf, "pln point=(%g,%g,%g)", e->pt[0], e->pt[1], e->pt[2]);
   um_pscroll(UM_sbuf);
   sprintf(UM_sbuf, "pln nvec=(%g,%g,%g)", e->nvec[0], e->nvec[1], e->nvec[2]);
   um_pscroll(UM_sbuf);
   uu_dexit;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : ncl_p_nclpln(ncl)
**       Print an NCL plane.
**    PARAMETERS   
**       INPUT  : 
**          ncl               buffer holding NCL plane.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_p_nclpln(ncl)
   struct NCLI_plane_rec *ncl;

   {
   uu_denter(UU_MTRC,(us,"ncl_p_nclpln(ncl=%x)",ncl));
   sprintf(UM_sbuf,"NCLPLN:");
   um_pscroll(UM_sbuf);
   sprintf(UM_sbuf,"  ijk=(%g,%g,%g)", ncl->ijk[0], ncl->ijk[1], ncl->ijk[2]);
   um_pscroll(UM_sbuf);
   sprintf(UM_sbuf,"  dist=%g", ncl->dist);
   um_pscroll(UM_sbuf);
   uu_dexit;
   }

/*********************************************************************
**    E_FUNCTION     : int ncl_plane_transl(e, offset)
**       Translate an NCL plane along a vector.
**    PARAMETERS   
**       INPUT  : 
**          e             NCL plane entity
**          offset        vector
**       OUTPUT : 
**          none
**    RETURNS      : 
**       UU_SUCCESS iff no error
**       UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_plane_transl(e, offset)
   struct NCL_nclpt_rec *e;
   UM_vector offset;

   {
   int status;

   uu_denter(UU_MTRC,(us,"ncl_plane_transl(key=%x)",
      e->key));

   status = UU_SUCCESS;

/*   um_vcplvc(e->pt, offset, e->pt);
/*   um_update_geom(e, UM_DEFAULT_TF);
*/
   uu_dexit;
   return (status);
}

/*********************************************************************
**    E_FUNCTION     : int projpt(nclkey, buf)
**       Translate an NCL plane along a vector.
**    PARAMETERS   
**       INPUT  : 
**          offset        vector
**       OUTPUT : 
**          none
**    RETURNS      : 
**       UU_SUCCESS iff no error
**       UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
projpt(nclkey, buf)
   UM_real8 buf[];
   UU_KEY_ID *nclkey;

   {
   UM_vector normal;
   UM_coord nwpt;
   struct NCL_nclpl_rec e;

   uu_denter(UU_MTRC,(us,"projpt( buf=%x)", buf));

   e.key = *nclkey;
   ur_retrieve_data(&e,sizeof(e));
   um_vctovc(e.nvec, normal);
   um_nptpln(buf, e.pt, normal, nwpt);
   um_vctovc(nwpt, e.pt);
   ur_update_data(&e);
   um_vctovc(nwpt, buf);

   uu_dexit;
   return;
}
/*********************************************************************
**    E_FUNCTION     : int ncl_nclpl_to_pln(nclkey, pln)
**       Convert an NCL_plane_rec to a UD_PLANE_REC
**    PARAMETERS   
**       INPUT  : 
**          nclkey        key of plane to convert
**       OUTPUT : 
**          pln			  converted UD_PLANE_REC
**    RETURNS      : 
**       UU_SUCCESS iff no error
**       UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_nclpln_to_pln(nclkey, pln)
UU_KEY_ID nclkey;
UD_PLANE_REC *pln;
	{
	int i;
	struct NCL_nclpl_rec e;

/*
.....This routine added support picking of an NCL plane when a
.....DAS PLANE INPUT is expected.  See das/d2plane.c - RAZ
*/
	e.key = nclkey;
	ur_retrieve_data(&e, sizeof(e));

	for (i = 0; i < 3; i++)
		pln->org[i] = e.pt[i];

	for (i = 0; i < 3; i++)
		pln->normal_vector[i] = e.nvec[i];

	return;
	}
/*********************************************************************
**    E_FUNCTION    : int ncl_plane_to_vector(level,pickpath,pickloc,vec)
**			Determine a vector (VEC) given a picked location
**			on a NCL plane (LEVEL, PICKPATH, PICKLOC).
**    PARAMETERS   
**       INPUT  : 
**				level					level of entity picked within pickpath
**				pickpath				DAS pick path
**				pickloc				DAS pickloc record
**       OUTPUT :  
**				vec					vector
**    RETURNS      : 
**			UU_SUCCESS  iff no errors; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int 
ncl_plane_to_vector(level, pickpath, pickloc, vec)
int level;
UD_PPICKREC *pickpath;
UD_NDCLOCREC *pickloc;
UM_vector vec;
	{
	UU_KEY_ID um_get_pickkey();
	UM_PICKENT pent;
	struct NCL_nclpl_rec e;
	int status;

	status = um_d_pickresolve(pickpath, level, &pent);
	if (status != UU_SUCCESS) goto done;

	e.key = um_get_pickkey(&pent, level);
	status = uc_retrieve_data(&e, sizeof(e));
	if (status != UU_SUCCESS) goto done;

	um_vctovc(e.nvec, vec);

done:;
	return(status);
	}
/*********************************************************************
**    E_FUNCTION    : int ncl_plane_to_coord(level,pickpath,pickloc,pt)
**			Determine a cartesian coordinate (PT) from the picked NCL plane
**          The point will be the display origin of the plane.
**    PARAMETERS   
**       INPUT  : 
**				level					level of entity picked within pickpath
**				pickpath				DAS pick path record
**				pickloc				DAS pickloc record
**       OUTPUT :  
**				pt						cartesian coordinate (MCS) corresponding
**										to picked location on entity
**    RETURNS      : 
**			UU_SUCCESS  iff no errors; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int 
ncl_plane_to_coord(level, pickpath, pickloc, pt)
int level;
UD_PPICKREC *pickpath;
UD_NDCLOCREC *pickloc;
UM_coord pt;
	{
	UM_PICKENT pent;
	struct NCL_nclpl_rec e;
	int status;

	status = um_d_pickresolve(pickpath, level, &pent);
	if (status != UU_SUCCESS) goto done;

	e.key = um_get_pickkey(&pent, level);
	status = uc_retrieve_data(&e, sizeof(e));
	if (status != UU_SUCCESS) goto done;

	um_vctovc(e.pt, pt);

done:;
	return (status);
	}
