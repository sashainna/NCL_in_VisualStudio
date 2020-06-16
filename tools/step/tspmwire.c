/*********************************************************************
**    NAME         :  tspmwire.c
**       CONTAINS:
**             utp_in_construct_geo
**             utp_in_point
**             utp_in_line
**             utp_in_circle
**             utp_in_ellipse
**             utp_in_plane
**    COPYRIGHT 2013 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       tspmwire.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:13:24
*********************************************************************/

#include "class.h"
#include "nccs.h"
#include "tstep.h"
#include "mgeom.h"
#include "mcrv.h"
#include "udebug.h"
#include "tigglobal.h"
#include "mlabddl.h"
#include "nconst.h"

/*********************************************************************
**    E_FUNCTION     :  utp_in_construct_geo(ptr,relnum)
**				CONSTRUCTIVE_GEOMETRY_REPRESENTATION_RELATIONSHIP
**          handling routine.  Typically a top level designator for
**          construction geometry definitions (planes, etc.).
**    PARAMETERS   
**       INPUT  : 
**          ptr      Pointer to STEP record in global array.
**       OUTPUT : 
**          relnum   Relation number of created entity.
**    RETURNS      :
**          Key of entity created.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_KEY_ID utp_in_construct_geo(ptr,relnum)
UTPs_step_record *ptr;
int *relnum;
{
	int status,i,j;
	UU_KEY_ID key;
	UTPs_step_record *tptr;
/*
.....Get the active Units
*/
	for (i=0;i<ptr->nparm;i++)
	{
		if (ptr->parm[i].type == UTP_RECNO)
		{
			tptr = utp_get_record(ptr->parm[i].ptype.recno);
			if (tptr != UU_NULL && tptr->command == SHAPE_REPRESENTATION)
			{
				utp_in_shape_rep(tptr);
				break;
			}
		}
	}
/*
.....Initialize the color attribute
*/
	utp_set_attr(ptr,UU_TRUE);
/*
.....Define the construction geometry
*/
	for (i=0;i<ptr->nparm;i++)
	{
		if (ptr->parm[i].type == UTP_RECNO)
		{
			tptr = utp_get_record(ptr->parm[i].ptype.recno);
			if (tptr != UU_NULL &&
				tptr->command == CONSTRUCTIVE_GEOMETRY_REPRESENTATION)
			{
				for (j=1;j<tptr->nparm;j++)
				{
					if (tptr->parm[j].type == UTP_RECNO)
					{
						utp_set_attr(ptr,UU_FALSE);
						key = utp_in_dispat_recno(tptr->parm[j].ptype.recno,relnum);
					}
				}
			}
		}
	}
/*
.....End of routine
*/
done:;
	return(key);
}

/*********************************************************************
**    E_FUNCTION     : utp_in_point(ptr,relnum)
**         Handles the a standalone Point definition.
**    PARAMETERS   
**       INPUT  : 
**          ptr      Pointer to STEP record in global array.
**       OUTPUT : 
**          relnum   Relation number of created entity.
**    RETURNS      :
**          Key of entity created.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_KEY_ID utp_in_point(ptr,relnum)
UTPs_step_record *ptr;
int *relnum;
{
	int status,marker;
	UU_KEY_ID key;
	char label[NCL_MAX_LABEL+1];
	UM_coord pt[2];
	struct UM_point_rec eptr;
	UM_transf tf;
/*
.....Initialize routine
*/
	key = 0;
	*relnum = UM_POINT_REL;
	strcpy(label,"@UN");
/*
.....Get point coordinates
*/
	status = utp_map_point(ptr,pt);
/*
.....Get marker type
*/
	marker = utp_get_marker(ptr->recno);
/*
.....Store point
*/
	if (status == UU_SUCCESS)
		key = utp_store_point(label,pt,marker);
/*
.....End of routine
*/
	return(key);
}

/*********************************************************************
**    E_FUNCTION     :  utp_in_line(ptr,relnum)
**				LINE handling routine.  Typically a top level
**          designator for lines.  Builds a trimmed line
**          based on the coordinate and vectors given in the
**          start point and vector records.
**    PARAMETERS   
**       INPUT  : 
**          ptr      Pointer to STEP record in global array.
**       OUTPUT : 
**          relnum   Relation number of created entity.
**    RETURNS      :
**          Key of entity created.
**    SIDE EFFECTS : none
**    WARNINGS     : Wireframe geometry is not currently supported so
**                   this routine is only used to define base geometry.
*********************************************************************/
UU_KEY_ID utp_in_line(ptr,relnum)
UTPs_step_record *ptr;
int *relnum;
{
	int i,status;
	UU_REAL rnum;
	UU_KEY_ID key;
	char label[NCL_MAX_LABEL+1];
	UM_coord pt[2];
	UM_vector vec;
	UTPs_step_record *tptr;
/*
.....Initialize routine
*/
/*	if (!UIG_from_trimsrf) goto failed;*/
	*relnum = UM_LINE_REL;
	strcpy(label,"@UN");
	if (ptr->parm[1].type != UTP_RECNO)
	{
		tptr = ptr;
		goto failed;
	}
/*
.....Get line start point
*/
	tptr = utp_get_record(ptr->parm[1].ptype.recno);
	if (tptr == UU_NULL) goto failed;
	status = utp_map_point(tptr,pt[0]);
	if (status == UU_FAILURE) goto failed;
/*
.....Get line end point
*/
	if (ptr->parm[2].type != UTP_RECNO)
	{
		tptr = ptr;
		goto failed;
	}
	tptr = utp_get_record(ptr->parm[2].ptype.recno);
	if (tptr == UU_NULL || tptr->command != VECTOR ||
		tptr->parm[1].type != UTP_RECNO || tptr->parm[2].type != UTP_REAL)
			goto failed;
	rnum = utp_convert(tptr->parm[2].ptype.value);
	tptr = utp_get_record(tptr->parm[1].ptype.recno);
	if (tptr == UU_NULL || tptr->command != DIRECTION) goto failed;
	for (i=0;i<3;i++)
	{
		if (tptr->parm[i+1].type != UTP_REAL) goto failed;
		vec[i] = tptr->parm[i+1].ptype.value; // * rnum;
	}
	um_unitvc(vec,vec);
	utp_transform_vector(vec);
	um_translate_point(pt[0],rnum,vec,pt[1]);
/*
.....Store the line
*/
	key = utp_store_line(label,pt[0],pt[1]);
	return(key);
failed:
	return(0);
}

/*********************************************************************
**    E_FUNCTION     :  utp_in_circle(ptr,relnum)
**				CIRCLE handling routine.  Typically a top level
**          designator for circles.  Builds a full circle
**          based on the coordinate and vectors given in the
**          AXIS2_PLACEMENT_3D and the radius.
**    PARAMETERS   
**       INPUT  : 
**          ptr      Pointer to STEP record in global array.
**       OUTPUT : 
**          relnum   Relation number of created entity.
**    RETURNS      :
**          Key of entity created.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_KEY_ID utp_in_circle(ptr,relnum)
UTPs_step_record *ptr;
int *relnum;
{
	int i,status;
	UU_REAL rad;
	UU_KEY_ID key;
	char label[NCL_MAX_LABEL+1];
	UM_coord pt[2];
	struct UM_circle_rec eptr;
	UTPs_step_record *tptr;
	UM_transf tf;
/*
.....Initialize routine
*/
/*	if (!UIG_from_trimsrf) goto failed;*/
	*relnum = UM_CIRCLE_REL;
	strcpy(label,"@UN");
	if (ptr->parm[1].type != UTP_RECNO)
	{
		tptr = ptr;
		goto failed;
	}
	tptr = utp_get_record(ptr->parm[1].ptype.recno);
	if (tptr == UU_NULL) goto failed;
	status = utp_map_transform(tptr,tf);
	if (status == UU_FAILURE) goto failed;
/*
.....Use the x axis associated with the circle as the svec.
*/
	rad = utp_convert(ptr->parm[2].ptype.value);
	um_vctovc(tf[3],pt[0]);
	um_translate_point(pt[0],rad,tf[0],pt[0]);
	um_vctovc(pt[0],pt[1]);
	status = utp_map_circle(ptr,pt[0],pt[1],UU_TRUE,&eptr);
	if (status == UU_SUCCESS) key = utp_store_circle(&eptr,label);
	return(key);
failed:
	return(0);
}

/*********************************************************************
**    E_FUNCTION     :  utp_in_ellipse(ptr,relnum)
**				ELLIPSE handling routine.  Typically a top level
**          designator for circles.  Builds a full circle
**          based on the coordinate and vectors given in the
**          AXIS2_PLACEMENT_3D and the radii.
**    PARAMETERS   
**       INPUT  : 
**          ptr      Pointer to STEP record in global array.
**       OUTPUT : 
**          relnum   Relation number of created entity.
**    RETURNS      :
**          Key of entity created.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_KEY_ID utp_in_ellipse(ptr,relnum)
UTPs_step_record *ptr;
int *relnum;
{
	int i,status;
	UU_REAL rad;
	UU_KEY_ID key;
	char label[NCL_MAX_LABEL+1];
	UM_coord pt[2];
	struct UM_conic_rec eptr;
	UTPs_step_record *tptr;
	UM_transf tf;
/*
.....Initialize routine
*/
	*relnum = UM_CONIC_REL;
	strcpy(label,"@UN");
	if (ptr->parm[1].type != UTP_RECNO) goto failed;
	tptr = utp_get_record(ptr->parm[1].ptype.recno);
	if (tptr == UU_NULL) goto failed;
	status = utp_map_transform(tptr,tf);
	if (status == UU_FAILURE) goto failed;
/*
.....Use the x axis associated with the ellipse as the svec.
*/
	rad = utp_convert(ptr->parm[2].ptype.value);
	um_vctovc(tf[3],pt[0]);
	um_translate_point(pt[0],rad,tf[0],pt[0]);
	um_vctovc(pt[0],pt[1]);
	status = utp_map_ellipse(ptr,pt[0],pt[1],UU_TRUE,&eptr);
	if (status == UU_SUCCESS) key = utp_store_conic(&eptr,label);
	return(key);
failed:
	return(0);
}

/*********************************************************************
**    E_FUNCTION     :  utp_in_plane(ptr,relnum)
**				Construction Plane handling routine.
**    PARAMETERS   
**       INPUT  : 
**          ptr      Pointer to STEP record in global array.
**       OUTPUT : 
**          relnum   Relation number of created entity.
**    RETURNS      :
**          Key of entity created.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_KEY_ID utp_in_plane(ptr,relnum)
UTPs_step_record *ptr;
int *relnum;
{
	int stat;
	char label[20];
	UU_KEY_ID key;
	struct NCL_nclpl_rec pl;
/*
.....Planes are disabled
*/
	key = 0;
	if (entity_mask[3] == 0) goto done;
/*
.....Define the plane
*/
	stat = utp_map_plane(ptr,&pl);
	if (stat != UU_SUCCESS) goto failed;
/*
.....Store the plane
*/
	key = utp_store_plane("NONE",ptr->recno,&pl);
	if (key == 0) goto failed;
	utp_count_translated(NCL_PLN_REL,1,UU_TRUE);
	goto done;
/*
.....Could not create plane
*/
failed:;
	key = 0;
	utp_plane_error(ptr);
	utp_count_translated(NCL_PLN_REL,1,UU_FALSE);
	goto done;
/*
....End of routine
*/
done:;
	return(key);
}
