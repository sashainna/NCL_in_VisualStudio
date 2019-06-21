
/*********************************************************************
**    NAME         :  m9edrf.c
**       CONTAINS: class dispatcher for MODEL entities
**			int um_draft_type(rel_num, type)
**			int um_get_circle_data
**			int um_get_conic_data
**			int um_get_line_data
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m9edrf.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:11
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "ulist.h"
#include "class.h"
#include "mdcoord.h"
#include "mdrel.h"
#include "mdgenent.h"
#include "mdebug.h"
#include "umath.h"
#include "mdclass.h"
#include "mdcoord.h"
#include "mdmatrix.h"
#include "mcrv.h"
#include "modef.h"
#include "mdcpln.h"
#include "mdunits.h"
#include "mdeval.h"
#include "adrfcom.h"

/*********************************************************************
**    E_FUNCTION     : int um_draft_type(rel_num, type)
**       Determine the drafting class for an entity with rel num rel_num.
**    PARAMETERS   
**       INPUT  : 
**          rel_num						NCL entity relation number
**       OUTPUT :  
**          type                    drafting type
**    RETURNS      : 
**			UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : 
**			The only data which must be initialized in the entity 
**			are the UNIBASE key and relation number.
*********************************************************************/
int
um_draft_type(rel_num, type)
	int rel_num;
	int *type;

	{
	int status;

	uu_denter(UU_MTRC,(us,"um_draft_type(rel_num=%d)", rel_num));
	status = UU_SUCCESS;
	switch(rel_num)
		{
		case  UM_POINT_REL:
				*type = UA_DRAFT_POINT;
				break;
		case  UM_LINE_REL:
				*type = UA_DRAFT_LINE;
				break;
		case  UM_CIRCLE_REL:
				*type = UA_DRAFT_ARC;
				break;
		case  UM_CONIC_REL:
				*type = UA_DRAFT_CONIC;
				break;
		case  UM_COMPCRV_REL:
		case  UM_RBSPLCRV_REL:
		case	UM_POLY_REL:
		case	UM_POLYLINE_REL:
				*type = UA_DRAFT_CURVE;
				break;
		default:
				*type = UA_DRAFT_UNKNOWN;
				status = UU_FAILURE;
				break;
		}
	uu_dexit;
	return(status);
	}
/*********************************************************************
**    E_FUNCTION     : int um_get_circle_data(key, center, radius, dtheta,
								normal, start_pt, end_pt, length)
**       Return the center, radius, and normal for a circle/arc having
**			the specified key.
**    PARAMETERS   
**       INPUT  : 
**          key							key of entity in UNIBASE
**       OUTPUT :  
**          center						center of circle
**          radius						radius of circle
**          dtheta						sweep angle of circle
**          normal						normal of circle
**          s_pt							start point
**          e_pt							end point
**          length						arc length
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_get_circle_data(key, center, radius, dtheta, normal, spt, ept, length)
	UU_KEY_ID  key;
	UM_coord center;
	UM_length *radius;
	UM_angle *dtheta;
	UM_vector normal;
	UM_coord spt;
	UM_coord ept;
	UM_length *length;

	{
	struct UM_crvdatabag e;
	struct UM_circle_rec *c;
	int rel_num;
	int status;
	UM_transf tfmat;
	UU_REAL um_getarclen();

	uu_denter(UU_MTRC,(us,"um_get_circle_data(%d,,,)",key));
	e.key = key;
	um_get_all_geom(&e, sizeof(struct UM_crvdatabag));
	if (e.rel_num != UM_CIRCLE_REL) status = -1;
	else
		{
		c = (struct UM_circle_rec *) &e;
		um_vctovc(c->center, center);
		*radius = c->radius;
		*dtheta = c->dang;
		um_vctovc(c->nvec, normal);
		um_tftotf(UM_idmat, tfmat);
		um_get_endpts(c, tfmat, spt, ept);
		*length = um_getarclen(c, tfmat);
		status = 0;
		}
	uu_dexit;
	return (status);
	}
/*********************************************************************
**    E_FUNCTION     : int um_get_conic(key, center, start_pt, end_pt)
**       Return the center, start point , end point for a conic having
**			the specified key.
**    PARAMETERS   
**       INPUT  : 
**          key							key of entity in UNIBASE
**       OUTPUT :  
**          center						center of circle
**          s_pt							start point
**          e_pt							end point
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_get_conic_data(key, center, spt, ept)
	UU_KEY_ID  key;
	UM_coord center;
	UM_coord spt;
	UM_coord ept;

	{
	struct UM_crvdatabag e;
	struct UM_conic_rec *c;
	int rel_num;
	int status;
	UM_transf tfmat;

	uu_denter(UU_MTRC,(us,"um_get_conic_data(%d,,,)",key));
	e.key = key;
	um_get_all_geom(&e, sizeof(struct UM_crvdatabag));
	if (e.rel_num != UM_CONIC_REL) status = -1;
	else
		{
		c = (struct UM_conic_rec *) &e;
		um_vctovc(c->tfmat[3], center);
		um_tftotf(UM_idmat, tfmat);
		um_get_endpts(c, tfmat, spt, ept);
		status = 0;
		}
	uu_dexit;
	return (status);
	}
/*********************************************************************
**    E_FUNCTION     : int um_get_line_data(key, spt, ept)
**       Return end points of the line specified by key
**    PARAMETERS   
**       INPUT  : 
**          key							key of entity in UNIBASE
**       OUTPUT :  
**          spt							start point
**          ept							end point
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_get_line_data(key, spt, ept)
	UU_KEY_ID  key;
	UM_coord spt;
	UM_coord ept;

	{
	struct UM_crvdatabag e;
	struct UM_line_rel *l;
	int rel_num;
	int status;
	UM_transf tfmat;

	uu_denter(UU_MTRC,(us,"um_get_line_data(%d,,,)",key));
	e.key = key;
	um_get_all_geom(&e, sizeof(struct UM_crvdatabag));
	if (e.rel_num != UM_LINE_REL) status = -1;
	else
		{
		l = (struct UM_line_rel *) &e;
		um_tftotf(UM_idmat, tfmat);
		um_get_endpts(l, tfmat, spt, ept);
		status = 0;
		}
	uu_dexit;
	return (status);
	}
