
/*********************************************************************
**    NAME         :  m3ecn2.c
**       CONTAINS: conic support routines
**			int um_cn_defn(eptr, tfmat, cn_defn)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m3ecn2.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:07:52
*********************************************************************/
#include "zsysdep.h"
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "go.h"
#include "mdcoord.h"
#include "mdrel.h"
#include "mattr.h"
#include "mcrv.h"
#include "modef.h"
#include "mdeval.h"
#include "mdebug.h"

#define	UM_ECN2TRC	1

/*********************************************************************
**    E_FUNCTION     : int um_cn_defn(eptr, tfmat, cn_defn)
**       Calculate the geometric definition and analytical definition
**			of a conic.
**    PARAMETERS   
**       INPUT  : 
**          eptr					pointer to conic entity
**				tfmat					matrix to position conic in MCS
**       OUTPUT :  
**          cn_defn				geometrical and analytical definition
**    RETURNS      : 
**			UU_SUCCESS iff no errors; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_cn_defn(eptr, tfmat, cn_defn)
	struct UM_conic_rec *eptr;
	UM_transf tfmat;
	UM_cn_defn *cn_defn;

	{
	int status;
	struct UM_evcrvout evcrv;
	UM_transf mcs_tfmat;
	UU_REAL asq, bsq;
	int i;

	uu_denter(UU_MTRC,(us,"um_cn_defn(key=%x)", eptr->key, tfmat));

	/* assume success */
	status = UU_SUCCESS;

	/* initialize type of conic */
	cn_defn->type = eptr->type;

	/* initialize evaluator structure */
	uc_init_evcrvout(eptr, &evcrv);

	/* calculate start point and tangent */
	uc_evcrv(UM_FRSTDERIV, (UU_REAL) 0.0, eptr, tfmat, &evcrv);
	um_vctovc(evcrv.cp, cn_defn->spt);
	um_vctovc(evcrv.dcdu, cn_defn->spt_derv);

	/* calculate end point and tangent */
	uc_evcrv(UM_FRSTDERIV, (UU_REAL) 1.0, eptr, tfmat, &evcrv);
	um_vctovc(evcrv.cp, cn_defn->ept);
	um_vctovc(evcrv.dcdu, cn_defn->ept_derv);

	/* calculate arc length of conic */
	cn_defn->arc_length = um_getarclen(eptr, tfmat);

	/* calculate conic plane definition */
	um_cctmtf(eptr->tfmat[3], tfmat, cn_defn->pln_point);
	um_vctmtf(eptr->tfmat[2], tfmat, cn_defn->pln_normal);

	/* calculate the total transformation to place the conic
		from definition space into MCS */
	um_tftmtf(eptr->tfmat, tfmat, mcs_tfmat);

	/* calculate entity specific geometric data */
	switch (eptr->type)
		{
		case UM_PARABOLA:
		case UM_HYPERBOLA:
		case UM_ELLIPSE:
			um_cctmtf(UM_zerovec, mcs_tfmat, cn_defn->center);
			um_vctmtf(UM_xaxis, mcs_tfmat, cn_defn->major_axis);
			um_vctmtf(UM_yaxis, mcs_tfmat, cn_defn->minor_axis);
			cn_defn->major_length = eptr->invariants[0];
			cn_defn->minor_length = eptr->invariants[1];
			break;
		default:
			status = UU_FAILURE;
			break;
		}

	/* calculate the entity specific analytic data */
	for (i=0; i<6; i++) cn_defn->cn[i] = 0.0;
	switch (eptr->type)
		{
		case UM_PARABOLA:
			cn_defn->major_length = 1/(4*eptr->invariants[0]);
			cn_defn->cn[2] = eptr->invariants[0];
			cn_defn->cn[3] = -1.0;
			break;
		case UM_HYPERBOLA:
			asq = eptr->invariants[0] * eptr->invariants[0];
			bsq = eptr->invariants[1] * eptr->invariants[1];
			cn_defn->cn[0] = 1.0;
			cn_defn->cn[2] = -(asq/bsq);
			cn_defn->cn[5] = -(asq);
			break;
		case UM_ELLIPSE:
			asq = eptr->invariants[0] * eptr->invariants[0];
			bsq = eptr->invariants[1] * eptr->invariants[1];
			cn_defn->cn[0] = 1.0;
			cn_defn->cn[2] = asq/bsq;
			cn_defn->cn[5] = -(asq);
			break;
		default:
			status = UU_FAILURE;
			break;
		}
	
#if UM_ECN2TRC
	um_p_ary(UM_PINT,		"type",		1,	&cn_defn->type);
	um_p_ary(UM_PFLOAT,	"spt",		3,	cn_defn->spt);
	um_p_ary(UM_PFLOAT,	"spt_derv",	3,	cn_defn->spt_derv);
	um_p_ary(UM_PFLOAT,	"ept",		3,	cn_defn->ept);
	um_p_ary(UM_PFLOAT,	"ept_derv",	3,	cn_defn->ept_derv);
	um_p_ary(UM_PFLOAT,	"arc_len",	1,	&cn_defn->arc_length);
	um_p_ary(UM_PFLOAT,	"pln_pt",	3,	cn_defn->pln_point);
	um_p_ary(UM_PFLOAT,	"pln_normal",	3,	cn_defn->pln_normal);
	um_p_ary(UM_PFLOAT,	"center",	3,	cn_defn->center);
	um_p_ary(UM_PFLOAT,	"maj_axis",	3,	cn_defn->major_axis);
	um_p_ary(UM_PFLOAT,	"min_axis",	3,	cn_defn->minor_axis);
	um_p_ary(UM_PFLOAT,	"maj_len",	1,	&cn_defn->major_length);
	um_p_ary(UM_PFLOAT,	"min_len",	1,	&cn_defn->minor_length);
	um_p_ary(UM_PFLOAT,	"defn eq",	6,	cn_defn->cn);
#endif

	uu_dexit;
	return (status);
	}
