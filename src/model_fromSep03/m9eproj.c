/*********************************************************************
**    NAME         :  m9eproj.c
**       CONTAINS: Routines to project entities to a plane (along the
**							normal to the plane).
**			int um_proj1_pt_to_plane(eptr, point, normal, neweptr)
**			int um_proj2_line_to_plane(eptr, point, normal, neweptr)
**			int um_proj3_circ_to_plane(eprt, point, normal, neweptr)
**			int um_proj4_conic_to_plane(eptr, point, normal, neweptr)
**			int um_proj5_compcrv_to_plane(eptr, point, normal, neweptr)
**			int um_proj7_rbspl_to_plane(eptr, point, normal, neweptr)
**			int um_proj40_polygon_to_plane(eptr, point, normal, neweptr)
**			int um_proj42_polyline_to_plane(eptr, point, normal, neweptr)
**			int um_proj_geom_to_plane(eptr, point, normal, neweptr)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m9eproj.c , 25.2
**    DATE AND TIME OF LAST  MODIFICATION
**       08/17/15 , 17:39:44
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "uhep.h"
#include "udebug.h"
#include "class.h"
#include "mdcoord.h"
#include "mdrel.h"
#include "mdgenent.h"
#include "mcrv.h"
#include "modef.h"
#include "mdebug.h"
#include "mdeval.h"
#include "nclvx.h"

/*********************************************************************
**    E_FUNCTION     : int um_proj1_pt_to_plane(eptr, point, normal, neweptr)
**       Project a point onto the given plane.
**    PARAMETERS   
**       INPUT  : 
**          eptr							point entity
**				point							point defining plane
**				normal						normal defining plane
**       OUTPUT :  
**          neweptr						projected point
**    RETURNS      : 
**			UU_SUCCESS iff no errors; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_proj1_pt_to_plane(eptr, point, normal, neweptr)
	struct UM_point_rec *eptr;
	UM_coord point;
	UM_vector normal;
	struct UM_point_rec *neweptr;

	{

	uu_denter(UU_MTRC,(us,"um_proj1_pt_to_plane(key=%d)",eptr->key));
	neweptr->rel_num = UM_POINT_REL;
	ur_setup_data (UM_POINT_REL,neweptr,sizeof(struct UM_point_rec));
	/* added for correct label on drawing. kathy */
	strncpy (neweptr->label, eptr->label, NCL_MAX_LABEL);
	neweptr->subscr = eptr->subscr;

	neweptr->markertype = eptr->markertype;
	neweptr->snap_node = eptr->snap_node;
	um_nptpln(eptr->pt, point, normal, neweptr->pt);
	uu_dexit;
	return (UU_SUCCESS);
	}

/*********************************************************************
**    E_FUNCTION     : int um_proj2_line_to_plane(eptr, point, normal, neweptr)
**       Project a line entity (EPTR) onto a plane (POINT, NORMAL).
**    PARAMETERS   
**       INPUT  : 
**          eptr							line entity
**				point							point defining plane
**				normal						normal defining plane
**       OUTPUT :  
**          neweptr						projected entity
**    RETURNS      : 
**			UU_SUCCESS iff no errors; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_proj2_line_to_plane(eptr, point, normal, neweptr)
	struct UM_line_rec *eptr;
	UM_coord point;
	UM_vector normal;
	struct UM_crvdatabag *neweptr;

	{
	UM_coord spt;
	UM_coord ept;
	int status;

	uu_denter(UU_MTRC,(us,"um_proj2_line_to_plane(key=%d)",eptr->key));
	/* added  for correct label on drawing kathy */
	status = UU_SUCCESS;
	strncpy (neweptr->label, eptr->label, NCL_MAX_LABEL);
	neweptr->subscr = eptr->subscr;

	um_nptpln(eptr->spt, point, normal, spt);
	um_nptpln(eptr->ept, point, normal, ept);
	if (um_cceqcc(spt, ept))
/*
.....Do not project lines which are perpendicular
.....to the plane onto the plane
.....Bobby  -  6/2/92
*/
/*		um_c1_pt(spt, neweptr);*/
		status = UU_FAILURE;
	else
	{
		ur_setup_data (UM_LINE_REL,neweptr,sizeof(struct UM_line_rec));
		um_c2_pp(spt, ept, neweptr);
	}
	uu_dexit;
	return (status);
	}
/*********************************************************************
**    E_FUNCTION     : int um_proj3_circ_to_plane(eprt, point, normal, neweptr)
**       Project a circle entity (EPTR) onto a plane (POINT, NORMAL).
**    PARAMETERS   
**       INPUT  : 
**          eptr						circle entity to project
**				point						point defining plane
**				normal					normal defining plane
**       OUTPUT :  
**          neweptr					projected entity
**    RETURNS      : 
**			UU_SUCCESS iff no errors; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_proj3_circ_to_plane(eptr, point, normal, neweptr)
	struct UM_circle_rec *eptr;
	UM_coord point;
	UM_vector normal;
	struct UM_crvdatabag *neweptr;

	{
	int status;

	uu_denter(UU_MTRC,(us,"um_proj3_circ_to_plane(key=%d)",eptr->key));

	/* added  for correct label on drawing kathy*/
	strncpy (neweptr->label, eptr->label, NCL_MAX_LABEL);
	neweptr->subscr = eptr->subscr;

	/* if the plane of the circle is parallel to the projection plane,
		the projected curve is a circle */
	if (um_vcparall(normal, eptr->nvec))
		{
		struct UM_circle_rec *new;
		new= (struct UM_circle_rec *) neweptr;

		new->rel_num = UM_CIRCLE_REL;
		ur_setup_data (UM_CIRCLE_REL,neweptr,sizeof(struct UM_circle_rec));
		um_nptpln(eptr->center, point, normal, new->center);
		um_vctovc(eptr->nvec, new->nvec);
		um_vctovc(eptr->svec, new->svec);
		new->radius = eptr->radius;
		new->dang = eptr->dang;
		status = UU_SUCCESS;
		}

	/* else if the plane of the circle is perpendicular to the projection
		plane, the projected curve is a line */
	else if (um_vcperp(normal, eptr->nvec))

		{
		struct UM_line_rec *new;
		UM_coord spt, ept, pt1, pt2;
		UM_vector vec1, vec2;
		UM_angle ang, ang1, ang2;
		UU_LOGICAL vec1_between, vec2_between;

		new= (struct UM_line_rec *) neweptr;
		ur_setup_data (UM_LINE_REL,neweptr,sizeof(struct UM_line_rec));
		um_get_endpts(eptr, UM_idmat, spt, ept);
		ang = eptr->dang;
	
		/* get the data for one of the extrema points */
		um_cross(normal, eptr->nvec, vec1);
		ang1 = um_angle2p(eptr->svec, vec1, eptr->nvec);
		um_vctmsc(vec1, eptr->radius, pt1);
		um_vcplvc(eptr->center, pt1, pt1);
	
		/* get the data for the other extrema point */
		um_vctmsc(vec1, (UU_REAL) -1.0, vec2);
		ang2 = um_angle2p(eptr->svec, vec2, eptr->nvec);
		um_vctmsc(vec2, eptr->radius, pt2);
		um_vcplvc(eptr->center, pt2, pt2);
	
		if (eptr->dang < 0.0)
			{
			ang  = -(eptr->dang);
			ang1 = UM_TWOPI - ang1;
			ang2 = UM_TWOPI - ang2;
			}
	
		vec1_between = (ang1 <= ang);
		vec2_between = (ang2 <= ang);

		if (vec1_between && vec2_between)
			{
			}
		else if (vec1_between)
			{
			if (ang1 > (ang/2.0))
				um_vctovc(spt, pt2);
			else
				um_vctovc(ept, pt2);
			}
		else if (vec2_between)
			{
			if (ang2 > (ang/2.0))
				um_vctovc(spt, pt1);
			else
				um_vctovc(ept, pt1);
			}
		else
			{
			um_vctovc(spt, pt1);
			um_vctovc(ept, pt2);
			}
		new->rel_num = UM_LINE_REL;
		um_nptpln(pt1, point, normal, new->spt);
		um_nptpln(pt2, point, normal, new->ept);
		status = UU_SUCCESS;
		}
	
	/* otherwise, the projected curve is an ellipse */
	else
		{
		struct UM_conic_rec *new;

		new= (struct UM_conic_rec *) neweptr;
		new->rel_num = UM_CONIC_REL;
		ur_setup_data (UM_CONIC_REL,neweptr,sizeof(struct UM_conic_rec));
		um_projcirc(eptr, UM_idmat, normal, point, new, &status);
		status = UU_SUCCESS;
		}
	uu_dexit;
	return (status);
	}

/*********************************************************************
**    E_FUNCTION     : int um_proj4_conic_to_plane(eptr, point, normal, neweptr)
**			Project an ellipse onto a plane
**    PARAMETERS   
**       INPUT  : 
**          eptr						conic entity to project
**				point						point defining plane
**				normal					normal defining plane
**       OUTPUT :  
**          neweptr					projected entity
**    RETURNS      : 
**			UU_SUCCESS iff no errors; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_proj4_conic_to_plane(eptr, point, normal, neweptr)
	struct UM_conic_rec *eptr;
	UM_coord point;
	UM_vector normal;
	struct UM_crvdatabag *neweptr;

	{
	int status;

	uu_denter(UU_MTRC,(us,"um_proj4_conic_to_plane(key=%d)",eptr->key));

	/* if the normal to the plane of the ellipse is perpendicular to the
		normal of the plane of projection, the curve is a line */
	if (um_vcperp(normal, eptr->tfmat[2]))
		{
		struct UM_line_rec *new;
		struct UM_evcrvout evcrv;
		int i,j;
		UM_coord pt[4];
		UM_vector linevec;
		UM_vector newvec;
		UM_length length;
		UM_length dist;
		UU_REAL sign;
		UM_param t[4];
		UM_param u;
		UM_param startparam;
		UM_param endparam;

		new = (struct UM_line_rec *) neweptr;
		new->rel_num = UM_LINE_REL;
		ur_setup_data (UM_LINE_REL,neweptr,sizeof(struct UM_line_rec));
		um_ev4_conic(UM_POINT, (UU_REAL) 0.0, eptr, UM_DEFAULT_TF, &evcrv);
		um_nptpln(evcrv.cp, point, normal, new->spt);
		um_ev4_conic(UM_POINT, (UU_REAL) 1.0, eptr, UM_DEFAULT_TF, &evcrv);
		um_nptpln(evcrv.cp, point, normal, new->ept);
		j=0;
		t[0] = -1.0; t[1] = 0.0; t[2] = 1.0; t[3] = 2.0;
		startparam = eptr->t0;
		endparam = eptr->t1;
		if (startparam > endparam) endparam += 2.0;
		for (i=0; i<4; i++)
			{
			if ((startparam < t[i]) && (t[i] < endparam))
				{
				umi_cc4_ttou(t[i], eptr, &u);
				um_ev4_conic(UM_POINT, u, eptr, UM_DEFAULT_TF, &evcrv);
				um_nptpln(evcrv.cp, point, normal, pt[j]);
				j++;
				}
			}
		length = um_dcccc(new->spt, new->ept);
		um_vcmnvc(new->ept, new->spt, linevec);
		um_unitvc(linevec, linevec);
		for (i=0; i<j; i++)
			{
			um_vcmnvc(pt[i], new->spt, newvec);
			um_unitvc(newvec, newvec);
			dist = um_dcccc(new->spt, pt[i]);
			if (length <= UM_FUZZ) um_vctovc(newvec, linevec);
			sign = um_dot(newvec, linevec);
			if (sign < 0.0)
				{
				um_vctovc(pt[i], new->spt);
				length = length + dist;
				}
			else if (dist > length)
				{
				um_vctovc(pt[i], new->ept);
				length = length + dist;
				}
			}
		status = UU_SUCCESS;
		}
	/* otherwise, the projected curve might be an ellipse or a rational
		bspline */
	else
		{
		struct UM_rbsplcrv_rec rbspl;

		um_c7_frmconic(eptr, &rbspl);
		status = um_proj7_rbspl_to_plane(&rbspl, point, normal, neweptr);
		}
	uu_dexit;
	return (status);
	}

/*********************************************************************
**    E_FUNCTION     : int um_proj5_compcrv_to_plane(eptr,
**										point, normal, neweptr)
**			Project a composite curve onto a plane
**    PARAMETERS   
**       INPUT  : 
**          eptr						composite curve to project
**				point						point defining plane
**				normal					normal defining plane
**       OUTPUT :  
**          neweptr					projected entity
**    RETURNS      : 
**			UU_SUCCESS iff no errors; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_proj5_compcrv_to_plane(eptr, point, normal, neweptr)
	struct UM_compcrv_rec *eptr;
	UM_coord point;
	UM_vector normal;
	struct UM_compcrv_rec *neweptr;

	{
	int i;
	int numkey;
	UU_KEY_ID keys[5000];
	int status;
	struct UC_entitydatabag *cons;
	struct UC_entitydatabag *copy;

	uu_denter(UU_MTRC,(us,"um_proj5_compcrv_to_plane(key=%d)",eptr->key));

	cons = (struct UC_entitydatabag *)
						uu_malloc(sizeof(struct  UC_entitydatabag));

	copy = (struct UC_entitydatabag *)
						uu_malloc(sizeof(struct  UC_entitydatabag));

	ur_setup_data(UM_COMPCRV_REL, neweptr, sizeof(struct UM_compcrv_rec));
	/* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
	strcpy (neweptr->label, "");
	neweptr->subscr = 0;
	status = UU_FAILURE;
	numkey = 0;
	for (i=0; i<eptr->no_cid; i++)
		{
		cons->key = eptr->cid[i].crvid;
		status = uc_retrieve_data(cons, sizeof(struct UC_entitydatabag));
		if (status == UU_SUCCESS)
			status = uc_project_to_plane(cons, point, normal, copy);
		if ((status == UU_SUCCESS) && (copy->rel_num != UM_POINT_REL))
			{
			uc_create_data(copy, UM_DEFAULT_TF, UM_CURRENT_ATTR);
			keys[numkey] = copy->key;
			numkey++;
			}
		}
	if (numkey > 0)
		{
		neweptr->key = 0;
		status = um_c5_mergecrv(numkey, keys, neweptr);
		if (status != 0)
			for (i=0; i<numkey; i++) uc_delete(keys[i]);
		}

	uu_dexit;
	return (status);
	}

/*********************************************************************
**    E_FUNCTION     : int um_proj7_rbspl_to_plane(eptr, point, normal, neweptr)
**			Project a rational bspline curve onto a plane
**    PARAMETERS   
**       INPUT  : 
**          eptr						rational bspline entity to project
**				point						point defining plane
**				normal					normal defining plane
**       OUTPUT :  
**          neweptr					projected entity
**    RETURNS      : 
**			UU_SUCCESS iff no errors; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_proj7_rbspl_to_plane(eptr, point, normal, neweptr)
	struct UM_rbsplcrv_rec *eptr;
	UM_coord point;
	UM_vector normal;
	struct UM_rbsplcrv_rec *neweptr;

	{
	int i,j;
	int status;

	uu_denter(UU_MTRC,(us,"um_proj7_rbspl_to_plane(key=%d)",eptr->key));
	status = UU_SUCCESS;
	ur_setup_data(UM_RBSPLCRV_REL, neweptr, (um_curve_size(eptr)));
	/* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
	strcpy (neweptr->label, "");
	neweptr->subscr = 0;
	um_c7_frmrbsplcrv(eptr, neweptr);
	for (i=0,j=0; i< neweptr->no_pt; i++,j=j+3)
		{
		um_nptpln(&(eptr->pt[j]), point, normal, &(neweptr->pt[j]));
		}
	uu_dexit;
	return (status);
	}

/*********************************************************************
**    E_FUNCTION     : int um_proj40_polygon_to_plane(eptr, point, normal, neweptr)
**			Project a polygon entity onto a plane
**    PARAMETERS   
**       INPUT  : 
**          eptr						polygon entity to project
**				point						point defining plane
**				normal					normal defining plane
**       OUTPUT :  
**          neweptr					projected entity
**    RETURNS      : 
**			UU_SUCCESS iff no errors; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_proj40_polygon_to_plane(eptr, point, normal, neweptr)
	struct UM_poly_rec *eptr;
	UM_coord point;
	UM_vector normal;
	struct UM_poly_rec *neweptr;

	{
	int i;

	uu_denter(UU_MTRC,(us,"um_proj40_polygon_to_plane(key=%d)",eptr->key));
	ur_setup_app_data(UM_POLY_REL, neweptr, sizeof(struct UM_poly_rec));
	neweptr->fcolor = eptr->fcolor;
	neweptr->numvtx = eptr->numvtx;
	for (i=0; i< neweptr->numvtx; i++)
		{
		um_nptpln(eptr->vertex[i], point, normal, neweptr->vertex[i]);
		}
	for (i=0; i<200; i++)
		{
		um_vctovc(eptr->vertex[i], neweptr->vertex[i]);
		}
	uu_dexit;
	return (UU_SUCCESS);
	}

/*********************************************************************
**    E_FUNCTION     : int um_proj42_polyline_to_plane(eptr, point, normal, neweptr)
**			Project a polyline curve onto a plane
**    PARAMETERS   
**       INPUT  : 
**          eptr						polyline entity to project
**				point						point defining plane
**				normal					normal defining plane
**       OUTPUT :  
**          neweptr					projected entity
**    RETURNS      : 
**			UU_SUCCESS iff no errors; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_proj42_polyline_to_plane(eptr, point, normal, neweptr)
	struct UM_polyline_rec *eptr;
	UM_coord point;
	UM_vector normal;
	struct UM_polyline_rec *neweptr;

	{
	int i,j;
	UM_coord pt;

	uu_denter(UU_MTRC,(us,"um_proj42_polyline_to_plane(key=%d)",eptr->key));
	ur_setup_app_data(UM_POLYLINE_REL, neweptr, sizeof(struct UM_polyline_rec));
	neweptr->no_pt = 0;
	for (i=0,j=0; i<eptr->no_pt; i++,j=j+3)
		{
		um_nptpln(&(eptr->pt[j]), point, normal, pt);
		ur_update_app_data_varlist(neweptr, 1, pt, neweptr->no_pt+1, 1);
		}
	uu_dexit;
	return (UU_SUCCESS);
	}

/*********************************************************************
**    E_FUNCTION     : int um_proj_geom_to_plane(eptr, point, normal, neweptr)
**       Dispatcher to project a geometry onto a plane. At present, only
**			points and curves are projected onto a plane.
**    PARAMETERS   
**       INPUT  : 
**          eptr						curve entity to project
**				point						point defining plane
**				normal					normal defining plane
**       OUTPUT :  
**          neweptr					projection of curve onto plane
**    RETURNS      : 
**			UU_SUCCESS iff no errors; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_proj_geom_to_plane(eptr, point, normal, neweptr)
	struct UM_crvdatabag *eptr;
	UM_coord point;
	UM_vector normal;
	struct UM_crvdatabag *neweptr;

	{
	int status;

	uu_denter(UU_MTRC,(us,"um_proj_geom_to_plane(key=%d)",eptr->key));
	switch (eptr->rel_num)
		{
		case UM_POINT_REL:
			status = um_proj1_pt_to_plane((struct UM_point_rec *)eptr, point,
				normal, (struct UM_point_rec *)neweptr);
			break;
		case UM_LINE_REL:
			status = um_proj2_line_to_plane((struct UM_line_rec *)eptr, point,
				normal, (struct UM_crvdatabag *)neweptr);
			break;
		case UM_CIRCLE_REL:
			status = um_proj3_circ_to_plane((struct UM_circle_rec *)eptr, point,
				normal, (struct UM_crvdatabag *)neweptr);
			break;
		case UM_CONIC_REL:
			status = um_proj4_conic_to_plane((struct UM_conic_rec *)eptr, point,
				normal, (struct UM_crvdatabag *)neweptr);
			break;
		case UM_COMPCRV_REL:
			status = um_proj5_compcrv_to_plane((struct UM_compcrv_rec *)eptr,
				point, normal, (struct UM_compcrv_rec *)neweptr);
			break;
		case UM_RBSPLCRV_REL:
			status = um_proj7_rbspl_to_plane((struct UM_rbsplcrv_rec *)eptr, point,
				normal, (struct UM_rbsplcrv_rec *)neweptr);
			break;
		case UM_POLY_REL:
			status = um_proj40_polygon_to_plane((struct UM_poly_rec *)eptr, point,
				normal, (struct UM_poly_rec *)neweptr);
			break;
		case UM_POLYLINE_REL:
			status = um_proj42_polyline_to_plane((struct UM_polyline_rec *)eptr,
				point, normal, (struct UM_polyline_rec *)neweptr);
			break;
		default:
			sprintf(UM_sbuf,"don't know how to project relation %d to plane",
				eptr->rel_num);
			um_pscroll(UM_sbuf);
			status = UU_FAILURE;
			break;
		}
	uu_dexit;
	return (status);
	}

