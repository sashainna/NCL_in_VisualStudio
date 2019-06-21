/*********************************************************************
**    NAME         :   m3efilet.c
**       CONTAINS:  various fillet routines
**			int umi_trim_crv_to_fillet(crv, ploc, isect, fillet)
**			int um_crv_fillet(radius, eptr1, ploc1, eptr2, ploc2,
**									isect1, isect2, fillet)
**			int um_lnln_fillet(radius, eptr1, ploc1,
**									eptr2, ploc2, isect1, isect2, fillet)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m3efilet.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:07:53
*********************************************************************/
#include "zsysdep.h"
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "go.h"
#include "dasnog.h"
#include "dselmask.h"
#include "class.h"
#include "mdrel.h"
#include "mdcoord.h"
#include "mcrv.h"
#include	"mdpick.h"
#include "mdebug.h"
#include "mdeval.h"
#include "misect.h"
#include "mattr.h"
#include "modef.h"
#include "nclvx.h"

/*********************************************************************
**    E_FUNCTION     : int umi_trim_crv_to_fillet(crv, ploc, isect,
**									fillet)
**       description
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
umi_trim_crv_to_fillet(crv, ploc, isect, fillet)
	struct UC_entitydatabag *crv;
	UD_NDCLOCREC *ploc;
	UM_isect *isect;
	struct UC_entitydatabag *fillet;

	{
	int bagsize;
	int status;
	struct UC_entitydatabag *part1;
	struct UC_entitydatabag *part2;
	struct UM_evcrvout *evcrv;
	UM_vector fillet_tangent;
	UM_vector crv_tangent;
	UD_NDCLOCREC trimploc;
	struct UM_crvdatabag *p1ptr, *p2ptr, *cptr;	/*RAH: tmp ptrs to copy labels */
	int dsegid;
   UM_param u[2], udel, dist;
   UM_coord ptp;
   UM_transf tfmat;

	uu_denter(UU_MTRC,(us,"umi_trim_crv_to_fillet(e(r,k)=(%d,%x), f(r,k)=(%d,%x))",
		crv->rel_num, crv->key, fillet->rel_num, fillet->key));

	bagsize = sizeof(struct UC_entitydatabag);
	part1 = (struct UC_entitydatabag *) uu_malloc(bagsize);
	part2 = (struct UC_entitydatabag *) uu_malloc(bagsize);
	evcrv = (struct UM_evcrvout *) uu_malloc(sizeof(struct UM_evcrvout));

	/* determine unit tangent vectors on both curve and fillet at point
		where curve is tangent to fillet */
	uc_init_evcrvout(crv, evcrv);
	uc_evcrv(UM_FRSTDERIV, isect->u0, crv, UM_DEFAULT_TF, evcrv);
	um_unitvc(evcrv->dcdu, crv_tangent);
	uc_init_evcrvout(fillet, evcrv);
	uc_evcrv(UM_FRSTDERIV, isect->u1, fillet, UM_DEFAULT_TF, evcrv);
	um_unitvc(evcrv->dcdu, fillet_tangent);
	if (isect->u1 < UM_FUZZ)
		um_vctmsc(fillet_tangent, (UU_REAL) -1.0, fillet_tangent);
/*
...set udel to picked end point of the curve
*/
   uc_retrieve_transf(crv->key, tfmat);
   uc_near_on_entity (crv->key,ploc,ptp);
   uc_cctou (crv, tfmat, ptp, &udel, &dist);

	if (isect->u0 < 0.0) /* extend starting point to fillet */
		{
      if (um_dot(crv_tangent, fillet_tangent) > 0.0) udel = UM_FUZZ;
/*    if (um_dot(crv_tangent, fillet_tangent) > 0.0)
			{
			um_pscroll("can't extend curve to fillet curve");
			status = UU_FAILURE;
			goto done;
			} */
		uc_init_evcrvout(crv, evcrv);
		uc_evcrv(UM_POINT, (UU_REAL) 0.0, crv, UM_DEFAULT_TF, evcrv);
		trimploc.transform = ploc->transform;
		trimploc.choice = ploc->choice;
		gsnormtran(ploc->transform);
		gwndc3(&trimploc.cord[0],&trimploc.cord[1],&trimploc.cord[2],
				evcrv->cp[0], evcrv->cp[1], evcrv->cp[2]);
		status = um_extend_curve(&trimploc, isect->pt, isect->u0, &udel, crv);
		if (status != UU_SUCCESS) goto done;
		um_update_geom(crv, UM_DEFAULT_TF);
		uc_display(crv);
		}
	else if (isect->u0 > 1.0) /* extend ending point to fillet */
		{
      if (um_dot(crv_tangent, fillet_tangent) > 0.0) udel = UM_FUZZ;
          
/*    if (um_dot(crv_tangent, fillet_tangent) < 0.0)
			{
         um_pscroll("can't extend curve to fillet curve");
         status = UU_FAILURE;
         goto done;
			} */
		uc_init_evcrvout(crv, evcrv);
		uc_evcrv(UM_POINT, (UU_REAL) 1.0, crv, UM_DEFAULT_TF, evcrv);
		gsnormtran(ploc->transform);
		trimploc.transform = ploc->transform;
		gwndc3(&trimploc.cord[0],&trimploc.cord[1],&trimploc.cord[2],
				evcrv->cp[0], evcrv->cp[1], evcrv->cp[2]);
		status = um_extend_curve(&trimploc, isect->pt, isect->u0, &udel, crv);
		if (status != UU_SUCCESS) goto done;
		ur_retrieve_disp_segid(crv->key, &dsegid);
		uv_blanksegs(dsegid, crv->key);
		um_update_geom(crv, UM_DEFAULT_TF);
		uc_display(crv);
		}
	else /* trim (start or end point of) curve to fillet */
		{
      if (isect->u0 < UM_DFUZZ || isect->u0 > 1.0 - UM_DFUZZ) goto done;
		/* split first curve at tangency and create the two new curves */
      u[0] = isect->u0;
      if (um_is_curve_closed(crv, UM_DEFAULT_TF))
         {
          u[1] = (um_dot(crv_tangent, fillet_tangent) > 0.0)? 1.0: 0.0;
          udel = 1.0 - u[1];
         }
		status = um_splitcurve(crv, &u[0], &udel, part1, part2);
		if (status != UU_SUCCESS)
			{
			um_pscroll("can't split curve");
			goto done;
			}

		/*RAH: give both part1 and part2 the label from crv.
		  This will preserve the label. */
		p1ptr = (struct UM_crvdatabag *)part1;
		p2ptr = (struct UM_crvdatabag *)part2;
		cptr = (struct UM_crvdatabag *)crv;

		/* RAH: setup key, label and subscript of potential replacement geom */
		p1ptr->key = cptr->key;
		strncpy(p1ptr->label, cptr->label, NCL_MAX_LABEL);
		p1ptr->subscr = cptr->subscr;

		/* RAH: setup key, label and subscript of potential replacement geom */
		p2ptr->key = cptr->key;
		strncpy(p2ptr->label, cptr->label, NCL_MAX_LABEL);
		p2ptr->subscr = cptr->subscr;

		/* now, determine which to keep and display and which to
			throw away */
/*		uc_delete(crv->key);*/
		udel = um_dot(crv_tangent, fillet_tangent); 
      if (um_dot(crv_tangent, fillet_tangent) < 0.0)  
			{
			part1->key = crv->key;
			ur_retrieve_disp_segid(crv->key, &dsegid);
			uv_blanksegs(dsegid, crv->key);
			um_update_geom(part1, UM_DEFAULT_TF);
			uc_display(part1);
			}
		else
			{
			part2->key = crv->key;
			ur_retrieve_disp_segid(crv->key, &dsegid);
			uv_blanksegs(dsegid, crv->key);
			um_update_geom(part2, UM_DEFAULT_TF);
			uc_display(part2);
			}
		}

done:;
	uu_free(part1);
	uu_free(part2);
	uu_free(evcrv);
	uu_dexitstatus("umi_trim_crv_to_fillet", status);
	return (status);
	}

/*********************************************************************
**    E_FUNCTION     : int um_crv_fillet(radius, eptr1, ploc1,
**									eptr2, ploc2, isect1, isect2, fillet)
**			Create a fillet (FILLET) tangent to two entities (EPTR1)
**			and (EPTR2) near to where the entities were picked
**			(PLOC1 and PLOC2). No trimming will be performed.
**    PARAMETERS   
**       INPUT  : 
**				radius			fillet radius
**          eptr1				first entity
**				ploc1				picked location on first entity
**          eptr2				second entity
**				ploc2				picked location on second entity
**       OUTPUT :  
**				isect1			fillet tangency point on first entity
**				isect1			fillet tangency point on second entity
**          fillet			fillet between the two entities
**    RETURNS      : 
**			UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_crv_fillet(radius, eptr1, ploc1, eptr2, ploc2, 
	isect1, isect2, fillet)
	UM_length radius;
	struct UM_crvdatabag *eptr1;
	UD_NDCLOCREC *ploc1;
	struct UM_crvdatabag *eptr2;
	UD_NDCLOCREC *ploc2;
	UM_isect *isect1;
	UM_isect *isect2;
	struct UM_crvdatabag *fillet;

	{
	int status = UU_FAILURE;
	struct UM_crvdatabag e[2];
	UD_NDCLOCREC ploc[3];
	UU_LOGICAL err;
	UM_coord npt[2];
	UU_REAL dist;
	int numint;

	uu_denter( UU_MTRC,(us,"um_crv_fillet(e1(r,k)=(%d,%x), e2(r,k)=(%d,%x), radius=%f)",
		eptr1->rel_num, eptr1->key, eptr2->rel_num, eptr2->key, radius));

	/* dispatch to appropriate routine */
	if ((eptr1->rel_num == UM_LINE_REL) && (eptr2->rel_num == UM_LINE_REL))
		status = um_lnln_fillet(radius, eptr1, ploc1, eptr2, ploc2, 
			isect1, isect2, fillet);
	else if (((eptr1->rel_num==UM_LINE_REL) && (eptr2->rel_num==UM_CIRCLE_REL))
		|| ((eptr1->rel_num == UM_CIRCLE_REL) && (eptr2->rel_num == UM_LINE_REL))
		|| ((eptr1->rel_num == UM_CIRCLE_REL) && (eptr2->rel_num == UM_CIRCLE_REL)))

		{

		/* rearrange data for now */
		zbytecp(e[0], *eptr1);
		zbytecp(e[1], *eptr2);
		zbytecp(ploc[0], *ploc1);
		zbytecp(ploc[1], *ploc2);

		if ((eptr1->rel_num == UM_CIRCLE_REL)&&(eptr2->rel_num == UM_CIRCLE_REL))
			{
			ud_ldas(UD_DASNDC, /*approximate center*/UM_MODEL, 34, &ploc[2], 
				 1, &numint, UD_NODEFAULT);
			}
	
		/* calculate fillet curve */
		um_cir_ttr(UU_FALSE, ploc, e, radius, fillet, npt, &err);
		if (err) goto done;
	
		/* calculate information for tangency point on first entity */
		um_vctovc(npt[0], isect1->pt);
		um_cctou(eptr1, UM_DEFAULT_TF, isect1->pt, &isect1->u0, &dist);
		um_cctou(fillet, UM_DEFAULT_TF, isect1->pt, &isect1->u1, &dist);
	
		/* calculate information for tangency point on second entity */
		um_vctovc(npt[1], isect2->pt);
		um_cctou(eptr2, UM_DEFAULT_TF, isect2->pt, &isect2->u0, &dist);
		um_cctou(fillet, UM_DEFAULT_TF, isect2->pt, &isect2->u1, &dist);

		status = UU_SUCCESS;
		}
/* else
		status = um_agcrv_fillet(radius, eptr1, ploc1, eptr2, ploc2, 
			isect1, isect2, fillet);  */

done:;
	uu_dexitstatus("um_crv_fillet", status);
	return (status);
	}

/*********************************************************************
**    E_FUNCTION     : int um_lnln_fillet(radius, eptr1, ploc1,
**									eptr2, ploc2, isect1, isect2, fillet)
**			Create a fillet (FILLET) tangent to two lines (EPTR1)
**			and (EPTR2) near to where the entities were picked
**			(PLOC1 and PLOC2). No trimming will be performed.
**    PARAMETERS   
**       INPUT  : 
**				radius			fillet radius
**          eptr1				first line
**				ploc1				picked location on first line
**          eptr2				second line
**				ploc2				picked location on second line
**       OUTPUT :  
**				isect1			fillet tangency point on first line
**				isect1			fillet tangency point on second line
**          fillet			fillet between the two line
**    RETURNS      : 
**			UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_lnln_fillet(radius, eptr1, ploc1, eptr2, ploc2, 
	isect1, isect2, fillet)
	UM_length radius;
	struct UM_line_rec *eptr1;
	UD_NDCLOCREC *ploc1;
	struct UM_line_rec *eptr2;
	UD_NDCLOCREC *ploc2;
	UM_isect *isect1;
	UM_isect *isect2;
	struct UM_circle_rec *fillet;

	{
	int status = UU_FAILURE;
	UM_vector lvec1;
	UM_vector ulvec1;
	UM_vector vec1;
	UM_vector uvec1;
	UM_coord npt1;
	UM_coord nptln1;
	UM_vector lvec2;
	UM_vector ulvec2;
	UM_vector vec2;
	UM_vector uvec2;
	UM_coord npt2;
	UM_coord nptln2;
	UM_vector normal;
	int nint;
	UM_coord ipt;
	UM_angle ang;
	UM_length hyp;
	UM_vector ubvec;

	uu_denter( UU_MTRC,(us,"um_lnln_fillet(e1(r,k)=(%d,%x), e2(r,k)=(%d,%x), radius=%f)",
		eptr1->rel_num, eptr1->key, eptr2->rel_num, eptr2->key, radius));

	/* calculate the unit vectors defining the lines */
	um_vcmnvc(eptr1->ept, eptr1->spt, lvec1);
	um_unitvc(lvec1, ulvec1);
	um_vcmnvc(eptr2->ept, eptr2->spt, lvec2);
	um_unitvc(lvec2, ulvec2);

	/* determine the plane that the lines lie in */
	um_cross(ulvec1, ulvec2, normal);
	um_unitvc(normal, normal);

	/* use the intersection point of the two lines to define two other
		lines which are parameterized in the direction of the picked
		location on the given lines */
	um_ilnln(eptr1->spt, ulvec1, eptr2->spt, ulvec2, &nint, ipt);
	if (nint != 1) goto done;
	um_projploctopln(ploc1, eptr1->spt, normal, npt1);
	um_nptln(npt1, eptr1->spt, ulvec1, nptln1);
	um_vcmnvc(nptln1, ipt, uvec1);
	um_unitvc(uvec1, uvec1);
	um_projploctopln(ploc2, eptr1->spt, normal, npt2);
	um_nptln(npt2, eptr2->spt, ulvec2, nptln2);
	um_vcmnvc(nptln2, ipt, uvec2);
	um_unitvc(uvec2, uvec2);

	/* now, we have enough information to calculate the fillet; the
		fillet center lies along the bisector of the two new unit
		vectors */
	ur_setup_data(UM_CIRCLE_REL, fillet, sizeof(struct UM_circle_rec));
	/* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
	strcpy (fillet->label, "");
	fillet->subscr = 0;
	um_vcplvc(uvec1, uvec2, ubvec);
	um_unitvc(ubvec, ubvec);
	ang = um_angle(ubvec, ulvec1);
	hyp = radius / sin(ang);
	um_vctmsc(ubvec, hyp, fillet->center);
	um_vcplvc(ipt, fillet->center, fillet->center);
	um_nptln(fillet->center, eptr1->spt, ulvec1, isect1->pt);
	um_nptln(fillet->center, eptr2->spt, ulvec2, isect2->pt);
	um_vcmnvc(isect1->pt, fillet->center, vec1);
	um_vcmnvc(isect2->pt, fillet->center, vec2);
	um_unitvc(vec1, fillet->svec);
	um_cross(vec1, vec2, fillet->nvec);
	um_unitvc(fillet->nvec, fillet->nvec);
	fillet->dang = um_angle(vec1, vec2);
	fillet->radius = radius;

	/* fill in the remainder of the intersection buffer for line 1 */
	um_vcmnvc(isect1->pt, eptr1->spt, vec1);
	isect1->u0 = um_dot(vec1, ulvec1) / um_mag(lvec1);
	isect1->t0 = isect1->u0;
	isect1->u1 = 0.0;
	isect1->t1 = 0.0;

	/* fill in the remainder of the intersection buffer for line 2 */
	um_vcmnvc(isect2->pt, eptr2->spt, vec2);
	isect2->u0 = um_dot(vec2, ulvec2) / um_mag(lvec2);
	isect2->t0 = isect2->u0;
	isect2->u1 = 1.0;
	isect2->t1 = 1.0;

	status = UU_SUCCESS;

done:;
	uu_dexitstatus("um_lnlnfillet", status);
	return (status);
	}
