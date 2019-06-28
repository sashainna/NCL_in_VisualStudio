/*********************************************************************
**    NAME         :  necirc.c
**       CONTAINS: routines to manipulate NCL circles
**       int ncl_circ_to_nclcirc(e, buf)
**       int ncl_ucirc_to_nclcirc(e, ncl)
**       int ncl_nclcirc_to_circ(buf, e)
**       ncl_nclci_to_unici(nclci, unici)
**       ncl_p_nclcirc(buf)
**       ncl_p90_circle(ptr)
**       int ncl_circle_transl(e, offset)
**       int ncl_ev_circle(evflag,u,eptr,tfmat, evout)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       necirc.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:26
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "mfort.h"
#include "mdcoord.h"
#include "mdrel.h"
#include "modef.h"
#include "mcrv.h"
#include "mdebug.h"

#include "ncl.h"
#include "nccs.h"
#include "nclfc.h"
#include "nclvx.h"

/*********************************************************************
**    E_FUNCTION     : int ncl_circ_to_nclcirc(e, ncl)
**       Convert a UNICAD circle to an NCL circle.
**    PARAMETERS   
**       INPUT  : 
**          e							UNICAD circle
**       OUTPUT :  
**          ncl						buffer to hold NCL circle
**    RETURNS      : 
**			UU_SUCCESS iff no error
**			UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_circ_to_nclcirc(e, ncl)
	struct NCL_nclci_rec *e;
	struct NCLI_circle_rec *ncl;

	{
	int status;
	UM_coord spt, npt;
	UM_vector yaxis, xcomp, ycomp, normal, ijk;
	UU_REAL dist;

	uu_denter(UU_MTRC,(us,"ncl_circ_to_nclcirc(key=%x, ncl=%x)",
		e->key, ncl));
	status = UU_SUCCESS;

	/* simply move the center and radius of the circle */
	ncl_uureal_to_real8(3, e->center, ncl->center);
	ncl_uureal_to_real8(1, &e->radius, &ncl->radius);

	/* determine the normal to the plane of the circle */
	um_vctovc(e->nvec, normal);
	if (e->dang < 0.0) um_vctmsc(normal, -1.0, normal);
	ncl_uureal_to_real8(3, normal, ncl->normal);

	/* calculate plane normal which points towards interior of circular arc 
		and the distance from the origin to this plane */
	if (fabs(UM_TWOPI - fabs(e->dang)) < UM_FUZZ)
		{
		ncl_uureal_to_real8(3, UM_zerovec, ncl->ijk);
		ncl_uureal_to_real8(1, UM_zerovec, &ncl->dist);
		}
	else
		{
		um_cross(e->nvec, e->svec, yaxis);
		um_unitvc(yaxis, yaxis);
		um_vctmsc(e->svec, cos(e->dang), xcomp);
		um_vctmsc(yaxis, sin(e->dang), ycomp);
		um_vcplvc(xcomp, ycomp, ijk);
		um_vcmnvc(e->svec, ijk, ijk);
		um_unitvc(ijk, ijk);
		um_cross(e->nvec, ijk, ijk);
		um_unitvc(ijk, ijk);
		if (e->dang < 0.0)
			um_vctmsc(ijk, -1.0, ijk);
		ncl_uureal_to_real8(3, ijk, ncl->ijk);
	
		um_vctmsc(e->svec, e->radius, spt);
		um_vcplvc(e->center, spt, spt);
		um_nptpln(UM_zerovec, spt, ijk, npt);
		dist = um_dcccc(UM_zerovec, npt);
		um_unitvc(npt, npt);
		if (um_dot(npt, ijk) < 0.0) dist = -dist;
		ncl_uureal_to_real8(1, &dist, &ncl->dist);
		}

	uu_dexit;
	return (status);
	}

/*********************************************************************
**    E_FUNCTION     : int ncl_ucirc_to_nclcirc(e, ncl)
**       Convert a UNICAD circle to an NCL circle.
**    PARAMETERS   
**       INPUT  : 
**          e							UNICAD circle
**			lcirijk					output circle ijk or nor
**       OUTPUT :  
**          ncl						buffer to hold NCL circle
**    RETURNS      : 
**			UU_SUCCESS iff no error
**			UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_ucirc_to_nclcirc(e, ncl,lcirijk)
	struct UM_circle_rec *e;
	struct NCLI_circle_rec *ncl;
	UU_LOGICAL lcirijk;

	{
	int status;
	UM_coord spt, npt;
	UM_vector yaxis, xcomp, ycomp, normal, ijk;
	UU_REAL dist;

	uu_denter(UU_MTRC,(us,"ncl_ucirc_to_nclcirc(key=%x, ncl=%x)",
		e->key, ncl));
	status = UU_SUCCESS;

	/* simply move the center and radius of the circle */
	ncl_uureal_to_real8(3, e->center, ncl->center);
	ncl_uureal_to_real8(1, &e->radius, &ncl->radius);

	/* determine the normal to the plane of the circle */
	um_vctovc(e->nvec, normal);
	if (e->dang < 0.0) um_vctmsc(normal, -1.0, normal);
	ncl_uureal_to_real8(3, normal, ncl->normal);

	/* calculate plane normal which points towards interior of circular arc 
		and the distance from the origin to this plane */
	if (fabs(UM_TWOPI - fabs(e->dang)) < UM_FUZZ && !lcirijk)
		{
		ncl_uureal_to_real8(3, UM_zerovec, ncl->ijk);
		ncl_uureal_to_real8(1, UM_zerovec, &ncl->dist);
		}
	else
		{
		um_cross(e->nvec, e->svec, yaxis);
		um_unitvc(yaxis, yaxis);
		um_vctmsc(e->svec, cos(e->dang), xcomp);
		um_vctmsc(yaxis, sin(e->dang), ycomp);
		um_vcplvc(xcomp, ycomp, ijk);
		um_vcmnvc(e->svec, ijk, ijk);
		um_unitvc(ijk, ijk);
		um_cross(e->nvec, ijk, ijk);
		um_unitvc(ijk, ijk);
		if (e->dang < 0.0)
			um_vctmsc(ijk, -1.0, ijk);
		ncl_uureal_to_real8(3, ijk, ncl->ijk);
	
		um_vctmsc(e->svec, e->radius, spt);
		um_vcplvc(e->center, spt, spt);
		um_nptpln(UM_zerovec, spt, ijk, npt);
		dist = um_dcccc(UM_zerovec, npt);
		um_unitvc(npt, npt);
		if (um_dot(npt, ijk) < 0.0) dist = -dist;
		ncl_uureal_to_real8(1, &dist, &ncl->dist);
		}

	uu_dexit;
	return (status);
	}

/*********************************************************************
**    E_FUNCTION     : int ncl_nclcirc_to_circ(ncl, e)
**			Convert an NCL circle to a UNICAD circle
**    PARAMETERS   
**       INPUT  : 
**          ncl						buffer holding NCL circle
**       OUTPUT :  
**          e							UNICAD circle
**    RETURNS      : 
**			UU_SUCCESS iff no error
**			UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_nclcirc_to_circ(ncl, e)
	struct NCLI_circle_rec * ncl;
	struct NCL_nclci_rec *e;

	{
	int status;
	UU_REAL dist, d1, d2;
	UM_vector ijk;
	UM_coord pt;
	UM_vector evec;
	UM_vector temp;
	UM_vector lvec;
	UM_coord lpt;
	UM_int2 idx = 169;
	UM_real8 ver;
	UM_int2 ifl1;
	UM_transf lref_tm;

	uu_denter(UU_MTRC,(us,"ncl_nclcirc_to_circ(ncl=%x, e=%x)",
		ncl, e));
	status = UU_SUCCESS;

	e->rel_num = NCL_CIRCLE_REL;

	/* If key exists, retrieve entity */
	if (e->key != 0)
		status = ur_retrieve_data(e, sizeof(struct NCL_nclci_rec));

	if (status != UU_SUCCESS)
		goto done;

	/* simply move circle center, normal, and radius */
	ncl_real8_to_uureal(3, ncl->center, e->center);
	ncl_real8_to_uureal(3, ncl->normal, e->nvec);
	ncl_real8_to_uureal(1, &ncl->radius, &e->radius);

	/* calculate the start vector and angle ..................*/
	ncl_real8_to_uureal(1, &ncl->dist, &dist);
	ncl_real8_to_uureal(3, ncl->ijk, ijk);
	if (um_cceqcc(UM_zerovec, ijk))
		/* arbitrarily if ijk is (0.0, 0.0, 0.0) */
	{ 
		getsc(&idx,&ver);
		if (ver < 9.04999)
		{
			um_perpvc(e->nvec, e->svec);
			um_unitvc(e->svec, e->svec);
		}
		else
		{
			um_vctovc (UM_xaxis, lvec);
			gtref (lref_tm,&ifl1);
			if (ifl1) um_vctmtf (lvec,lref_tm,lvec);
			um_cross(e->nvec, lvec, e->svec);
			um_cross(e->svec, e->nvec, e->svec);
			if (um_mag(e->svec) < UM_FUZZ)
			{
				lvec[0] = 0.0;
				lvec[1] = 0.0;
				lvec[2] = -1.0;
				if (ifl1) um_vctmtf (lvec,lref_tm,lvec);
				ncl_mcstowcs(1,lvec,e->svec);
/*				um_vctovc (lvec, e->svec);*/
			}
			um_unitvc(e->svec, e->svec);
		}
		e->dang = UM_TWOPI;
	}
	else
	{
/*
.....Calculate start vector and angle.
.....d1   = dist from center to limit plane
.....d2   = half length of chord between arc end points
.....lpt  = center point of chord
.....lvec = vector along chord
*/
		d1 = dist - um_dot(e->center, ijk);
		d2 = e->radius*e->radius - d1*d1;
		um_vctmsc(ijk, d1, temp);
		um_vcplvc(e->center, temp, lpt);
		if (d2 <= 0.0)
		{
			um_vcmnvc(lpt, e->center, e->svec);
			um_unitvc(e->svec, e->svec);
			e->dang = UM_TWOPI;
		}
		else
		{
			um_cross(ijk, e->nvec, lvec);
			um_unitvc(lvec, lvec);
			d2 = sqrt(d2);
			um_vctmsc(lvec, d2, lvec);
			um_vcplvc(lpt, lvec, pt);
			um_vcmnvc(pt, e->center, e->svec);
			um_vcmnvc(lpt, lvec, pt);
			um_vcmnvc(pt, e->center, evec);
			um_unitvc(e->svec, e->svec);
			um_unitvc(evec, evec);
			e->dang = um_angle2p(e->svec, evec, e->nvec);
		}
	}

done:;
	uu_dexit;
	return (status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_nclci_to_unici(nclci, unici)
**       Convert an NCL circle to a UNICAD circle.
**    PARAMETERS   
**       INPUT  : 
**          nclci						NCL circle entity
**       OUTPUT :  
**          unici						UNICAD  circle entity
**    RETURNS      : 
**			UU_SUCCESS iff no error
**			UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_nclci_to_unici(nclci, unici)
	struct NCL_nclci_rec *nclci;
	struct UM_circle_rec *unici;

	{
	char label[100];

	uu_denter(UU_MTRC,(us,"ncl_nclci_to_unici(key=%x)", nclci->key));

	/* added  to correct the label on drawing.  kathy*/
	ncl_get_label_with_key(nclci->key, label);
	strncpy (unici->label, label, NCL_MAX_LABEL);

	unici->rel_num = UM_CIRCLE_REL;
	unici->radius = nclci->radius;
	unici->dang = nclci->dang;
	um_vctovc(nclci->center, unici->center);
	um_vctovc(nclci->svec, unici->svec);
	um_vctovc(nclci->nvec, unici->nvec);
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : ncl_p90_circle(ptr)
**       Print the data defining an NCL circle (arc).
**    PARAMETERS   
**       INPUT  : 
**				ptr					pointer to circle record
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_p90_circle(ptr)
	struct  NCL_nclci_rec  *ptr;

	{

	uu_denter( UU_MTRC,(us,"ncl_p90_circle(key=%x)",ptr->key));

	sprintf( UM_sbuf, "CIRCLE %d", ptr->key);
	um_pscroll(UM_sbuf);
	sprintf(UM_sbuf, "label %s:6", ptr->label);
	um_pscroll(UM_sbuf);
	sprintf(UM_sbuf, "suvscr %d", ptr->subscr);
	um_pscroll(UM_sbuf);
	um_p_ary(UM_PFLOAT, "radius", 1, &ptr->radius);
	um_p_ary(UM_PFLOAT, "dang",   1, &ptr->dang);
	um_p_ary(UM_PFLOAT, "center", 3, ptr->center);
	um_p_ary(UM_PFLOAT, "svec",   3, ptr->svec);
	um_p_ary(UM_PFLOAT, "nvec",   3, ptr->nvec);

	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION     : int ncl_circle_transl(e, offset)
**       Translate an NCL circle along a vector.
**    PARAMETERS   
**       INPUT  : 
**          e             NCL circle entity
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
ncl_circle_transl(e, offset)
	struct NCL_nclci_rec *e;
   UM_vector offset;

   {
   int status;

   uu_denter(UU_MTRC,(us,"ncl_circle_transl(key=%x)",
      e->key));

   status = UU_SUCCESS;

   um_vcplvc(e->center, offset, e->center);
   um_update_geom(e, UM_DEFAULT_TF);

   uu_dexit;
   return (status);
}
/*********************************************************************
**    E_FUNCTION     : int ncl_ev_circle(evflag,u,eptr,tfmat,evout)
**       Evaluate an NCL line at a parameter
**      PARAMETERS:
**          INPUT:
**              evflag        UM_POINT= calculate point on line only;
**                        UM_FRSTDERIV= calculate point and 1st
**                                  derivative;
**                        UM_SECDERIV= calculate point, 1st and
**                                  2nd derivative;
**                        UM_CURVATURE= calc point, 1st, 2nd deriv,
**                                 and curvature;
**              u                   the parameter value
**              eptr                pointer to the entity data
**              tfmat               transformation matrix.
**          OUTPUT:
**              evout               pointer to a curve evaluator
**                         record containing both the requested
**                         information, and (ultimately) a
**                         status value indicating the validity
**                         of the requested information.
**  RETURNS : nothing currently, ultimately will return one of the
**              following:
**            UM_VALID: all requested fields are valid;
**            UM_BADFIELDS: at least one requested fields is invalid;
**            UM_BADRECORDS: at least one entire record is invalid;
**            UM_INVALID: all output is suspect.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
int
ncl_ev_circle(evflag,u,eptr,tfmat, evout)
int evflag;
UM_param u;
struct NCL_nclci_rec *eptr;
UM_transf tfmat;
struct UM_evcrvout *evout;
	{
	int status;
	struct UM_circle_rec unientity;

	ncl_nclci_to_unici(eptr, &unientity);

	status = uc_evcrv(evflag, u, &unientity, tfmat, evout);

	return (status);
	}
