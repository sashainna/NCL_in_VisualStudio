/*********************************************************************
**    NAME         :  nesupt2.c
**       CONTAINS:
**           ncl_geom_type()
**           wf_geom_type()
**           ncl_label_type()
**    COPYRIGHT 1984 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       nesupt2.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:52
*********************************************************************/

#include <errno.h>
#include "usysdef.h"
#include "udebug.h"
#include "mdrel.h"

/*********************************************************************
**    E_FUNCTION     : int ncl_geom_type(rel_num)
**       Determine if the relation rel_num is an NCL geometry entity.
**    PARAMETERS   
**       INPUT  : 
**          rel_num              Relation number of entity.
**       OUTPUT :  
**          none
**    RETURNS      : 
**       UU_SUCCESS iff entity is NCL geometry type; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_geom_type(rel_num)
int rel_num;

{
int status;
  switch (rel_num)
    {
    case NCL_POINT_REL:
    case NCL_LINE_REL:
    case NCL_CIRCLE_REL:
    case UM_POINT_REL:
	case UM_RBSPLCRV_REL:
    case UM_LINE_REL:
    case UM_CIRCLE_REL:
    case NCL_PLN_REL:
    case NCL_VECTOR_REL:
    case NCL_CURVE_REL:
    case NCL_SURF_REL:
    case NCL_MESHSURF_REL:
    case NCL_QUILTSURF_REL:
    case NCL_PATERN_REL:
    case NCL_NETSF_REL:
    case NCL_EVALCV_REL:
    case NCL_EVALSF_REL:
    case NCL_SHAPE_REL:
    case NCL_POINTVEC_REL:
       status = UU_SUCCESS;
       break;
    default:
       status = UU_FAILURE;
       break;
    }
    return(status);
}

/*********************************************************************
**    E_FUNCTION     : int wf_geom_type(rel_num)
**       Determine if the relation rel_num is an NCL wireframe entity.
**    PARAMETERS   
**       INPUT  : 
**          rel_num              Relation number of entity.
**       OUTPUT :  
**          none
**    RETURNS      : 
**       UU_SUCCESS iff entity is wireframe geometry type; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int wf_geom_type(rel_num)
int rel_num;

{
int status;
  switch (rel_num)
    {
    case UM_POINT_REL:
    case UM_LINE_REL:
    case UM_CIRCLE_REL:
    case NCL_PLN_REL:
    case NCL_VECTOR_REL:
    case NCL_POINTVEC_REL:
    case NCL_PATERN_REL:
    case UM_CONIC_REL:
    case NCL_CURVE_REL:
    case UM_RBSPLCRV_REL:
    case UM_UVCVONSF_REL:
    case UM_COMPCRV_REL:
    case NCL_SURF_REL:
    case NCL_REVSURF_REL:
    case NCL_MATRIX_REL:
    case NCL_MESHSURF_REL:
    case UM_RBSPLSRF_REL:
    case NCL_TRIMSF_REL:
    case NCL_NETSF_REL:
    case UM_AGCRV_REL:
    case UM_AGSRF_REL:
    case UM_AGSHELL_REL:
    case NCL_SHAPE_REL:
/*
.....Added Polylines to valid wireframe geometry
.....So that surfaces projected onto drawings
.....would have their labels displayed
.....Bobby  -  2/24/94
*/
    case UM_POLYLINE_REL:
    case UA_TEXT_REL:
    case UM_SOLID_REL:
       status = UU_SUCCESS;
       break;
    default:
       status = UU_FAILURE;
       break;
    }
    return(status);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_label_type(rel_num)
**       Determine if the relation rel_num can be labeled.
**    PARAMETERS   
**       INPUT  : 
**          rel_num              Relation number of entity.
**       OUTPUT :  
**          none
**    RETURNS      : 
**       UU_SUCCESS if entity can be labeled; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_label_type(rel_num) int rel_num;
{
int status;
	status = UU_FAILURE;
	if (ncl_geom_type(rel_num) == UU_SUCCESS ||
		wf_geom_type(rel_num) == UU_SUCCESS ||
		rel_num == UB_INSTANCE_REL) status = UU_SUCCESS;
	return(status);
}
