
/*********************************************************************
**    NAME         :  m3eextr
**       CONTAINS:
**			int um_circular_curve_sweep(eptr, pt, vec, ang)
**			int um_linear_curve_extrusion(eptr, offset)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m3eextr.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:07:53
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "class.h"
#include "mdrel.h"
#include "mcrv.h"
#include "modef.h"
#include	"mdeval.h"

/*********************************************************************
**    E_FUNCTION     : int um_circular_curve_sweep(eptr, pt, vec, ang)
**       Given a curve, an axis, and an angle, create the
**			wireframe geometry corresponding to a circular sweep of
**			the curve about the axis by the specified angle.
**    PARAMETERS   
**       INPUT  : 
**          eptr						planar curve to extrude
**				pt							point of axis
**				vec						vector of axis
**				ang						angle to sweep curve around axis
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_circular_curve_sweep(eptr, pt, vec, ang)
	struct UC_entitydatabag *eptr;
	UM_coord pt;
	UM_vector vec;
	UM_angle ang;

	{
	struct UC_entitydatabag copy;
	struct UM_circle_rec circle;
	struct UM_evcrvout evout;
	struct UC_attributedatabag attr;
	struct UM_compcrv_rec *cptr;
	UM_transf tfmat;
	UM_transf rotmat;
	UM_transf tempmat;
	UM_coord pt_to_origin;
	UM_coord spt;
	UM_coord ept;
	UM_coord center;
	int i;
	int status;

	uu_denter(UU_MTRC,(us,"um_circular_curve_sweep(key=%d,pt=%x,vec=%x,ang=%f)",
		eptr->key, pt, vec, ang));
	status = UU_SUCCESS;

	/* get the transformation and attributes for the specified curve */
	status = uc_retrieve_transf(eptr->key, tfmat);
	if (status != UU_SUCCESS) goto done;
	status = uc_retrieve_attr(eptr->key, &attr);
	if (status != UU_SUCCESS) goto done;

	/* if the sweep operation is not a complete circle, copy the original
		curve, rotate around the axis, and display */
	if ((UM_TWOPI - fabs(ang)) > UM_FUZZ)
		{
		uc_copy(eptr, &copy, sizeof(copy));
		um_rottf(vec, ang, rotmat);
		um_vctmsc(pt, (UU_REAL) -1.0, pt_to_origin);
		um_disptf(pt_to_origin, tempmat);
		um_tftmtf(tempmat, rotmat, rotmat);
		um_disptf(pt, tempmat);
		um_tftmtf(rotmat, tempmat, rotmat);
		uc_rotate(&copy, pt, vec, ang, rotmat);
		uc_display(&copy);
		}

	/* determine the start point of the circular arc (e.g. point on
		curve) and the center of the circular arc (e.g. nearest point on
		the axis to the start point); create a circular arc; store it in
		UNIBASE; and display it */
	uc_init_evcrvout(eptr, &evout);
	uc_evcrv(UM_POINT, (UU_REAL) 0.0, eptr, tfmat, &evout);
	um_vctovc(evout.cp, spt);
	um_nptln(evout.cp, pt, vec, center);
	um_c3_arccpan(center, evout.cp, ang, vec, &circle);
	uc_create_data(&circle, UM_DEFAULT_TF, &attr);
	uc_display(&circle);

	/* if the curve is a composite curve, evaluate the curve at the end
		point of each component, and create a circular arc */
	if (eptr->rel_num == UM_COMPCRV_REL)
		{
		cptr = (struct UM_compcrv_rec *) eptr;
		for (i=0; i<(cptr->no_cid - 1); i++)
			{
			uc_evcrv(UM_POINT, cptr->cid[i].endparam, cptr, tfmat, &evout);
			um_nptln(evout.cp, pt, vec, center);
			um_c3_arccpan(center, evout.cp, ang, vec, &circle);
			uc_create_data(&circle, UM_DEFAULT_TF, &attr);
			uc_display(&circle);
			}
		}

	/* if the curve is not closed, determine the final circular arc;
		create the curve in UNIBASE; and display it */
	uc_evcrv(UM_POINT, (UU_REAL) 1.0, eptr, tfmat, &evout);
	if (!um_cceqcc(spt, evout.cp))
		{
		um_nptln(evout.cp, pt, vec, center);
		um_c3_arccpan(center, evout.cp, ang, vec, &circle);
		uc_create_data(&circle, UM_DEFAULT_TF, &attr);
		uc_display(&circle);
		}

done:;
	uu_dexitstatus("um_circular_curve_extrusion", status);
	return (status);
	}

/*********************************************************************
**    E_FUNCTION     : int um_linear_curve_extrusion(eptr, offset)
**       Given a planar curve and a non-zero offset vector which
**			does not lie in the plane of the curve, create the wireframe
**			geometry corresponding to a linear extrusion operation.
**    PARAMETERS   
**       INPUT  : 
**          eptr						planar curve to extrude
**				offset					non-zero offset vector not lying
**											in the plane of the curve
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_linear_curve_extrusion(eptr, offset)
	struct UC_entitydatabag *eptr;
	UM_vector offset;

	{
	struct UC_entitydatabag copy;
	struct UM_line_rec line;
	struct UM_evcrvout evout;
	struct UC_attributedatabag attr;
	struct UM_compcrv_rec *cptr;
	UM_transf tfmat;
	UM_coord spt;
	UM_coord ept;
	int i;
	int status;

	uu_denter(UU_MTRC,(us,"um_linear_curve_extrusion(key=%d rel_num=%d)",
		eptr->key, eptr->rel_num));
	status = UU_SUCCESS;

	/* get the transformation and attributes for the specified curve */
	status = uc_retrieve_transf(eptr->key, tfmat);
	if (status != UU_SUCCESS) goto done;
	status = uc_retrieve_attr(eptr->key, &attr);
	if (status != UU_SUCCESS) goto done;

	/* copy the original curve, translate along the vector, and display */
	uc_copy(eptr, &copy, sizeof(copy));
	uc_translate(&copy, offset);
	uc_display(&copy);

	/* determine the start and end points of the first linear extrusion,
		create the line in UNIBASE, and display it */
	uc_init_evcrvout(eptr, &evout);
	uc_evcrv(UM_POINT, (UU_REAL) 0.0, eptr, tfmat, &evout);
	um_vctovc(evout.cp, spt);
	um_vcplvc(evout.cp, offset, ept);
	um_c2_pp(evout.cp, ept, &line);
	uc_create_data(&line, UM_DEFAULT_TF, &attr);
	uc_display(&line);

	/* if the curve is a composite curve, evaluate the curve at the end
		point of each component, and create a linear extrusion segment
		component segments */
	if (eptr->rel_num == UM_COMPCRV_REL)
		{
		cptr = (struct UM_compcrv_rec *) eptr;
		for (i=0; i<(cptr->no_cid - 1); i++)
			{
			uc_evcrv(UM_POINT, cptr->cid[i].endparam, cptr, tfmat, &evout);
			um_vcplvc(evout.cp, offset, ept);
			um_c2_pp(evout.cp, ept, &line);
			uc_create_data(&line, UM_DEFAULT_TF, &attr);
			uc_display(&line);
			}
		}

	/* if the curve is not closed, determine the start and end points
		of the last linear extrusion, create the line in UNIBASE, and
		display it */
	uc_evcrv(UM_POINT, (UU_REAL) 1.0, eptr, tfmat, &evout);
	if (!um_cceqcc(spt, evout.cp))
		{
		um_vcplvc(evout.cp, offset, ept);
		um_c2_pp(evout.cp, ept, &line);
		uc_create_data(&line, UM_DEFAULT_TF, &attr);
		uc_display(&line);
		}

done:;
	uu_dexitstatus("um_linear_curve_extrusion", status);
	return (status);
	}
