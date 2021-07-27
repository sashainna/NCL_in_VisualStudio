/*********************************************************************
**    NAME         :  m4iconv.c
**       CONTAINS: internal routines to convert AG curves to UNICAD curves
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**			int umi_agcv_to_line(cv, line)
**			int umi_agcv_to_circ(cv, circ)
**			int umi_agcv_to_unicrv(cv, unicrv)
**			int umi_ag_pro_conic(bs, conic_data)
**     MODULE NAME AND RELEASE LEVEL 
**       m4iconv.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:04
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "uhep.h"
#include "ulist.h"
#include "go.h"
#include "dasnog.h"
#include "class.h"
#include "mdrel.h"
#include "mattr.h"
#include "mdebug.h"
#include "mdeval.h"
#include "mcrv.h"
#include "modef.h"
#include "mderror.h"

#include "ag_incl.h"
#include "ag_crv_def.h"
#include "ag_global.h"

/*********************************************************************
**    I_FUNCTION     : int umi_agcv_to_line(cv, line)
**			Convert a AG curve (CV) to a line (LINE).
**    PARAMETERS   
**       INPUT  : 
**				cv					AG curve
**       OUTPUT :  
**				line				equivalent line entity
**    RETURNS      : 
**			UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
umi_agcv_to_line(cv, line)
	AG_CURVEP cv;
	struct UM_line_rec *line;

	{
	int status;
	struct UM_agcrv_rec crv;
	struct UM_evcrvout evcrv;

	uu_denter( UU_MTRC,(us,"umi_agcv_to_line(cv=%x)", cv));

	ur_setup_data(UM_LINE_REL, line, sizeof(struct UM_line_rec));
	/* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
	strcpy (line->label, "");
	line->subscr = 0;

	crv.crvaddr = (int) cv;
	um_agcrv_evaluate(UM_POINT, (UU_REAL) 0.0, &crv, UM_idmat, &evcrv);
	um_vctovc(evcrv.cp, line->spt);
	um_agcrv_evaluate(UM_POINT, (UU_REAL) 1.0, &crv, UM_idmat, &evcrv);
	um_vctovc(evcrv.cp, line->ept);

	status = UU_SUCCESS;

done:;
	uu_dexitstatus("umi_agcv_to_line", status);
	return (status);
	}

/*********************************************************************
**    I_FUNCTION     : int umi_agcv_to_circ(cv, circ)
**			Convert a AG curve (CV) to a circle or arc (CIRC).
**			It is assumed that the curve really is an arc/circle.
**    PARAMETERS   
**       INPUT  : 
**				cv					AG curve pointer
**       OUTPUT :  
**				circ				equivalent circle/arc entity
**    RETURNS      : 
**			UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
umi_agcv_to_circ(cv, circ)
	AG_CURVEP cv;
	struct UM_circ_rec *circ;

	{
	int status = UU_FAILURE;
	struct UM_agcrv_rec crv;
	UM_coord p0, p33, p66, p1;
	struct UM_evcrvout evcrv;

	uu_denter( UU_MTRC,(us,"umi_agcv_to_circ(cv=%x)", cv));

	/* set up AG curve entity */
	um_agcrv_setup_data(UM_AGCRV_REL, &crv, sizeof(crv));
	crv.crvaddr = (int) cv;

	/* evaluate to obtain points along curve; for now, assume
		that they are good enough to calculate circle */
	um_agcrv_evaluate(UM_POINT, (UU_REAL) 0.0, &crv, UM_idmat, &evcrv);
	um_vctovc(evcrv.cp, p0);
	um_agcrv_evaluate(UM_POINT, (UU_REAL) 0.33, &crv, UM_idmat, &evcrv);
	um_vctovc(evcrv.cp, p33);
	um_agcrv_evaluate(UM_POINT, (UU_REAL) 0.66, &crv, UM_idmat, &evcrv);
	um_vctovc(evcrv.cp, p66);
	um_agcrv_evaluate(UM_POINT, (UU_REAL) 1.0, &crv, UM_idmat, &evcrv);
	um_vctovc(evcrv.cp, p1);

	/* calculate either a circle or arc */
	if (um_cceqcc(p0, p1))
		status = um_c3_3pt(p0, p33, p66, circ);
	else
		status = um_c3_arc3pt(p0, p33, p1, circ);

done:;
	uu_dexitstatus("umi_agcv_to_circ", status);
	return (status);
	}

/*********************************************************************
**    I_FUNCTION     : int umi_agcv_to_unicrv(cv, unicrv)
**			Convert a AG curve (CV) to its simplest UNICAD curve
**			representation. At present, only lines or circles/arcs 
**			will be created. All other bsplines will be left as AG
**			curves.
**    PARAMETERS   
**       INPUT  : 
**				cv					AG curve pointer
**       OUTPUT :  
**				unicrv			equivalent UNICAD curve
**    RETURNS      : 
**			UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
umi_agcv_to_unicrv(cv, unicrv)
	AG_CURVEP cv;
	struct UC_entitydatabag *unicrv;

	{
	int status = UU_FAILURE;
	AG_CON_DATA conic_data;

	uu_denter( UU_MTRC,(us,"umi_agcv_to_unicrv(cv=%x)", cv));

	if (cv->nbs == 1) 
		{
		switch(cv->bs0->ctype)
			{
			case AG_LINE:
				status = umi_agcv_to_line(cv, unicrv);
				break;
			case AG_CIRCLE:
				status = umi_agcv_to_circ(cv, unicrv);
				break;
			default:
				{
				umi_ag_pro_conic(cv->bs0, &conic_data);
				switch (conic_data.ctype)
					{
					case AG_LINE:
						status = umi_agcv_to_line(cv, unicrv);
						break;
					case AG_CIRCLE:
						status = umi_agcv_to_circ(cv, unicrv);
						break;
					case AG_ELLIPSE:
					case AG_PARABOLA:
					case AG_HYPERBOLA:
					case AG_OTHER:
					default:
						status = UU_FAILURE;
						break;
					}
				break;
				}
			}
		}

done:;
	uu_dexitstatus("umi_agcv_to_unicrv", status);
	return (status);
	}

/*********************************************************************
**    I_FUNCTION     : int umi_ag_pro_conic(bs, conic_data)
**			Determine the conic properties (CONIC_DATA) of the bspline (BS).
**			At present, the bspline is the same conic iff all spans of
**			the bspline give the same conic properties.
**    PARAMETERS   
**       INPUT  : 
**				bs					AG bspline pointer
**       OUTPUT :  
**				conic_data		conic property definition
**    RETURNS      : 
**			UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
umi_ag_pro_conic(bs, data)
	AG_SPLINEP bs;
	AG_CON_DATAP data;

	{
	int i;
	UU_LOGICAL same;
	AG_CON_DATA span_data;
	AG_CIR_DATAP circle, scircle;

	uu_denter( UU_MTRC,(us,"umi_ag_pro_conic(bs=%x)", bs));

	bs->node = bs->node0;
	ag_pro_conic(bs, data);

	for (i=1; i<bs->n; i++)
		{
		ag_pro_conic(bs, &span_data);
		if (span_data.ctype != data->ctype)
			{
			data->ctype = AG_OTHER;
			goto done;
			}
		switch (span_data.ctype)
			{
			case AG_CIRCLE:
				{
				circle = &data->conic_data.circle;
				scircle = &span_data.conic_data.circle;
				same = (um_cceqcc(circle->center, scircle->center) &&
							um_vcparall(circle->normal, scircle->normal) &&
							(fabs(circle->radius - scircle->radius) < AG_tol_dist));
				if (!same)
					{
					data->ctype = AG_OTHER;
					goto done;
					}
				}
				break;
				
			default:
				data->ctype = AG_OTHER;
				goto done;
			}
		bs->node = bs->node->next;
		}

done:;
	uu_dexit;
	}

