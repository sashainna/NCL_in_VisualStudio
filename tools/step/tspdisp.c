/*********************************************************************
**    NAME         :  tspdisp.c
**       CONTAINS:
**          utp_in_dispat_recno
**          utp_in_dispat
**          utp_in_shape_def
**          utp_in_shape_rep
**          utp_in_dependent_shape
**          utp_in_mechanical_design
**          utp_in_bounded_wireframe
**          utp_in_curve_set
**
**    COPYRIGHT 2013 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       tspdisp.c , 25.2
**    DATE AND TIME OF LAST  MODIFICATION
**       10/27/16 , 15:16:28
*********************************************************************/

#include "tiges.h"
#include "tigdefs.h"
#include "tstep.h"
#include "mcrv.h"
#include "nconst.h"
#include "udebug.h"

extern char Mlabel[];

UU_KEY_ID utp_in_dispat();
UU_KEY_ID utp_in_shape_def();
void utp_in_shape_rep();
void utp_in_dependent_shape();
void utp_in_mechanical_design();
void utp_in_bounded_wireframe();
void utp_in_curve_set();

/*********************************************************************
**    I_FUNCTION     :  utp_in_dispat_recno(recno,relnum)
**				Call the Geometry dispatch routine with the STEP record
**          pointed to by 'recno'.
**    PARAMETERS   
**       INPUT  : 
**          recno    Record number of STEP record to process.
**       OUTPUT : 
**          relnum   Relation number of created entity.
**    RETURNS      :
**          Key of entity created.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_KEY_ID utp_in_dispat_recno(recno,relnum)
int recno;
int *relnum;
{
	UU_KEY_ID key;
	UTPs_step_record *ptr;
/*
.....Get STEP record to process
*/
	ptr = utp_get_record(recno);
/*
.....Convert record
*/
	if (ptr != UU_NULL)
		key = utp_in_dispat(ptr,relnum);
	return(key);
}

/*********************************************************************
**    I_FUNCTION     :  utp_in_dispat(ptr,relnum)
**				Convert a STEP entity record and store it in the Unibase.
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
UU_KEY_ID utp_in_dispat(ptr,relnum)
UTPs_step_record *ptr;
int *relnum;
{
	int key;
/*
.....Start of dispatch logic
*/
	key = 0;
	switch(ptr->command)
	{
	case ADVANCED_FACE:
		key = utp_in_advanced_face(ptr,relnum);
		break;
	case APPLIED_GROUP_ASSIGNMENT:
		key = utp_in_group_def(ptr,relnum);
		break;
	case BREP_WITH_VOIDS:
	case MANIFOLD_SOLID_BREP:
		key = utp_in_brep(ptr,relnum);
		break;
	case B_SPLINE_CURVE:
	case B_SPLINE_CURVE_WITH_KNOTS:
	case BOUNDED_CURVE:
	case QUASI_UNIFORM_CURVE:
		key = utp_in_curve(ptr,relnum);
		break;
	case CARTESIAN_POINT:
		key = utp_in_point(ptr,relnum);
		break;
	case CIRCLE:
		key = utp_in_circle(ptr,relnum);
		break;
	case COMPOSITE_CURVE:
		key = utp_in_comp_curve(ptr,relnum);
		break;
	case CONSTRUCTIVE_GEOMETRY_REPRESENTATION:
	case CONTEXT_DEPENDENT_SHAPE_REPRESENTATION:
		utp_in_dependent_shape(ptr);
		break;
	case CONSTRUCTIVE_GEOMETRY_REPRESENTATION_RELATIONSHIP:
		key = utp_in_construct_geo(ptr,relnum);
		break;
	case EDGE_CURVE:
		key = utp_in_edge_curve(ptr,relnum);
		break;
	case EDGE_LOOP:
		key = utp_in_edge_loop(ptr,relnum);
		break;
	case ELLIPSE:
		key = utp_in_ellipse(ptr,relnum);
		break;
	case FACE_OUTER_BOUND:
	case FACE_BOUND:
		key = utp_in_outer_bound(ptr,relnum);
		break;
	case GEOMETRICALLY_BOUNDED_SURFACE_SHAPE_REPRESENTATION:
	case GEOMETRICALLY_BOUNDED_WIREFRAME_SHAPE_REPRESENTATION:
		utp_in_bounded_wireframe(ptr);
		break;
	case GEOMETRIC_CURVE_SET:
	case GEOMETRIC_SET:
		utp_in_curve_set(ptr);
		break;
	case LINE:
		key = utp_in_line(ptr,relnum);
		break;
	case MANIFOLD_SURFACE_SHAPE_REPRESENTATION:
	case ADVANCED_BREP_SHAPE_REPRESENTATION:
		key = utp_in_manifold_surface(ptr,relnum);
		break;
	case MECHANICAL_DESIGN_GEOMETRIC_PRESENTATION_REPRESENTATION:
		utp_in_mechanical_design(ptr);
	case OPEN_SHELL:
	case CLOSED_SHELL:
		key = utp_in_open_shell(ptr,relnum);
		break;
	case ORIENTED_CLOSED_SHELL:
		key = utp_in_oriented_shell(ptr,relnum);
		break;
	case ORIENTED_EDGE:
		key = utp_in_oriented_edge(ptr,relnum);
		break;
	case PLANE:
		key = utp_in_plane(ptr);
		break;
	case PRE_DEFINED_MARKER:
		utp_in_marker_type(ptr);
		break;
	case PRESENTATION_LAYER_ASSIGNMENT:
		utp_presentation_layer(ptr);
		break;
	case SHAPE_REPRESENTATION:
		utp_in_shape_rep(ptr,relnum);
		key = 0;
		break;
	case SHAPE_DEFINITION_REPRESENTATION:
	case SHAPE_REPRESENTATION_RELATIONSHIP:
		key = utp_in_shape_def(ptr,relnum);
		break;
	case SHELL_BASED_SURFACE_MODEL:
		key = utp_in_shell_based(ptr,relnum);
		break;
	case STYLED_ITEM:
	case OVER_RIDING_STYLED_ITEM:
		utp_styled_item(ptr);
		break;
	case SURFACE_CURVE:
		key = utp_in_surface_curve(ptr,relnum);
		break;
	case SURFACE_SIDE_STYLE:
	case SURFACE_STYLE_FILL_AREA:
	case SURFACE_STYLE_USAGE:
	case FILL_AREA_STYLE:
	case FILL_AREA_STYLE_COLOUR:
	case PRESENTATION_STYLE_ASSIGNMENT:
	case CURVE_STYLE:
	case POINT_STYLE:
		utp_in_attr_rec(ptr);
		break;
	case TRIMMED_CURVE:
		key = utp_in_trimmed_curve(ptr,relnum);
		break;
	case COLOUR_RGB:
	case DRAUGHTING_PRE_DEFINED_COLOUR:
		utp_in_attr_color(ptr);
		break;
	case DRAUGHTING_PRE_DEFINED_CURVE_FONT:
		utp_in_attr_line_style(ptr);
		break;
	case VERTEX_LOOP:
		key = utp_in_vertex_loop(ptr,relnum);
		break;
	default:
		*relnum = 0;
		key = 0;
		break;
	}
	return(key);
}

/*********************************************************************
**    E_FUNCTION     :  utp_in_shape_def(ptr,relnum)
**				SHAPE_DEFINITION_REPRESENTATION and
**          SHAPE_REPRESENTATION_RELATIONSHIP handling routine.
**          Typically a top level designator that contains links to the
**          actual geometric entity.
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
UU_KEY_ID utp_in_shape_def(ptr,relnum)
UTPs_step_record *ptr;
int *relnum;
{
	int i,ind,nrec,key,inc=0,recno,trecno;
	UU_LOGICAL xflag,store = UU_FALSE,found = UU_FALSE;
	char label[NCL_MAX_LABEL];
	UTPs_step_record *tptr;
/*
.....Find the property definition and entity pointers
*/
	ind = nrec = 0;
	key = 0;
	Mlabel[0] = '\0';
	label[0] = '\0';
	trecno = recno = 0;
	utp_create_unique_layer(ptr);
	for (i=0;i<ptr->nparm;i++)
	{
		if (ptr->parm[i].type == UTP_RECNO)
		{
			nrec++;
/*
........Property definition
.........Modified to handle SHAPE_REPRESENTATION_RELATIONSHIP records with the
.........first record number is not for a SHAPE_REPRESENTATION - ASF 10/17/13.
.........Added check if desired record for label was found - ASF 1/23/14.
*/
			if ((ptr->command == SHAPE_DEFINITION_REPRESENTATION && nrec == 2) ||
				ptr->command == SHAPE_REPRESENTATION_RELATIONSHIP)
			{
				if (!found)
					utp_define_shape_label(ptr->parm[i].ptype.recno,label,UU_TRUE);
//				sprintf(label,"%d",ptr->parm[i].ptype.recno);
				tptr = utp_get_record(ptr->parm[i].ptype.recno);
				if (tptr == UU_NULL) goto done;
				if (ptr->command == SHAPE_REPRESENTATION_RELATIONSHIP)
				{
					if (tptr->command == SHAPE_REPRESENTATION) store = UU_TRUE;
					else ind = i;
				}
				else
				{
					store = UU_TRUE;
					ind = i;
				}
			}
			if (store)
			{
				found = UU_TRUE;
				store = UU_FALSE;
				recno = tptr->recno;
				if (nrec == 2) trecno = recno;
				xflag = utp_activate_transform(recno,UU_FALSE);
			}
			if (nrec == 2) break;
		}
	}
/*
.....Make sure entity label has been selected
*/
	if (label[0] != '\0' && !utp_valid_shape_range(label)) goto done;
/*
.....Found the entity pointer
*/
	do
	{
		if (nrec == 2 && ptr->parm[ind].type == UTP_RECNO)
		{
			key = utp_in_dispat_recno(ptr->parm[ind].ptype.recno,relnum);
			if (key==0 && xflag && ptr->command==SHAPE_DEFINITION_REPRESENTATION)
			{
				utp_deactivate_transform(trecno);
				xflag = UU_FALSE;
			}
			if (xflag)
			{
//				utp_clear_transform();
				xflag = utp_activate_transform(recno,UU_FALSE);
				sprintf(Mlabel,"_%d",++inc);
			}
		}
		else
			break;
	} while (xflag);
/*
.....End of routine
*/
done:
	return(key);
}

/*********************************************************************
**    E_FUNCTION     :  utp_in_shape_rep(ptr)
**          SHAPE_REPRESENTATION handling routine.  Sometimes used
**          to reference solid definitions.
**    PARAMETERS   
**       INPUT  : 
**          ptr      Pointer to STEP record in global array.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS :
**          Modified this routine from defining the units using the
**          GEOMETRIC_REPRESENTATION_CONTEXT command to dispatching
**          geometry definition records.  This updated was mandated
**          by QAR 100148 - a-c_head.stp.
**    WARNINGS     : none
*********************************************************************/
void utp_in_shape_rep(ptr)
UTPs_step_record *ptr;
{
	int i,relnum;
	UTPs_step_record *tptr;
/*
.....Define units for this entity
*/
	for (i=2;i<ptr->nparm;i++)
	{
		if (ptr->parm[i].type == UTP_RECNO)
		{
			tptr = utp_get_record(ptr->parm[i].ptype.recno);
			if (tptr != UU_NULL &&
				tptr->command != GEOMETRIC_REPRESENTATION_CONTEXT)
			{
/*				utp_map_units(tptr);*/
				utp_in_dispat(tptr,&relnum);
			}
		}
	}
}

/*********************************************************************
**    E_FUNCTION     :  utp_in_dependent_shape(ptr)
**          CONTEXT_DEPENDENT_SHAPE_REPRESENTATION handling routine.
**          Typically a top level designator that contains links to the
**          actual geometric entity.
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
void utp_in_dependent_shape(ptr)
UTPs_step_record *ptr;
{
	int i,j,nrec,key,status,ncnt,relnum,rec1,rec2;
	UTPs_step_record *tptr,*uptr,*vptr;
	UM_transf tf,tf1;
/*
.....Get the record of the shape
.....to store attributes for
*/
	if (ptr->parm[0].type != UTP_RECNO) goto done;
	tptr = utp_get_record(ptr->parm[0].ptype.recno);
	if (tptr == UU_NULL || tptr->command != REPRESENTATION_RELATIONSHIP)
		goto done;
/*
.....Get first record
*/
	rec1 = 0;
	rec2 = 0;
	if (tptr->parm[2].type != UTP_RECNO) goto done;
	uptr = utp_get_record(tptr->parm[2].ptype.recno);
	if (uptr == UU_NULL) goto done;
/*
........First record contains the reference
*/
	rec1 = uptr->recno;
	if (tptr->parm[3].type != UTP_RECNO) goto done;
	vptr = utp_get_record(tptr->parm[3].ptype.recno);
	if (vptr == UU_NULL) goto done;
	rec2 = vptr->recno;
/*
.....Get count of attributes
*/
	ncnt = 0;
	if (uptr != UU_NULL && uptr->command == SHAPE_REPRESENTATION)
	{
		for (j=0;j<uptr->nparm-1;j++)
		{
			if (uptr->parm[j].type == UTP_RECNO) ncnt++;
		}
	}
/*
...........Get the active units
*/
	if (uptr->parm[uptr->nparm-1].type == UTP_RECNO)
	{
		vptr = utp_get_record(uptr->parm[uptr->nparm-1].ptype.recno);
		if (vptr != UU_NULL) utp_map_units(vptr);
	}
/*
.....Get the name of the entity
.....from second record

	if (rec1 == 0)
	{
		if (tptr->parm[3].type != UTP_RECNO) goto done;
		uptr = utp_get_record(tptr->parm[3].ptype.recno);
		if (uptr == UU_NULL) goto done;
		if (uptr->command != SHAPE_REPRESENTATION) goto done;
		rec1 = uptr->recno;
	}
/*
.....Get the xform matrix
*/
	i = 4;
	if (tptr->parm[i].type != UTP_COMMAND ||
		tptr->parm[i].ptype.cmd !=
		REPRESENTATION_RELATIONSHIP_WITH_TRANSFORMATION)
			goto done;
/*
........ITEM_DEFINED_TRANSFORMATION
*/
	i++;
	if (tptr->parm[i].type != UTP_RECNO) goto done;
	uptr = utp_get_record(tptr->parm[i].ptype.recno);
	if (uptr == UU_NULL) goto done;
/*
........AXIS2_PLACEMENT_3D
*/
	if (uptr->parm[2].type != UTP_RECNO) goto done;
	tptr = utp_get_record(uptr->parm[2].ptype.recno);
	if (tptr == UU_NULL) goto done;
	status = utp_map_transform(tptr,tf);
	if (status != UU_SUCCESS) goto done;
	if (uptr->parm[3].type == UTP_RECNO)
	{
		tptr = utp_get_record(uptr->parm[3].ptype.recno);
		if (tptr != UU_NULL)
		{
			status = utp_map_transform(tptr,tf1);
			if (status == UU_SUCCESS) um_tftmtf(tf,tf1,tf);
		}
	}
/*
.....Get the assembly that this part is attached to

	strcpy(labref,label);
	if (ptr->parm[1].type == UTP_RECNO)
	{
		tptr = utp_get_record(ptr->parm[1].ptype.recno);
		if (tptr != UU_NULL && tptr->command == PRODUCT_DEFINITION_SHAPE &&
			tptr->parm[tptr->nparm-1].type == UTP_RECNO)
		{
			uptr = utp_get_record(tptr->parm[tptr->nparm-1].ptype.recno);
			if (uptr != UU_NULL &&
				uptr->command == NEXT_ASSEMBLY_USAGE_OCCURRENCE &&
				uptr->parm[3].type == UTP_RECNO)
			{
				tptr = utp_get_record(uptr->parm[3].ptype.recno);
				if (tptr != UU_NULL &&
					tptr->command == PRODUCT_DEFINITION &&
					tptr->parm[2].type == UTP_RECNO)
				{
					uptr = utp_get_record(tptr->parm[2].ptype.recno);
					if (uptr != UU_NULL &&
						(uptr->command ==
						PRODUCT_DEFINITION_FORMATION_WITH_SPECIFIED_SOURCE ||
						uptr->command == PRODUCT_DEFINITION_FORMATION) &&
						uptr->parm[2].type == UTP_RECNO)
					{
						tptr = utp_get_record(uptr->parm[2].ptype.recno);
						if (tptr != UU_NULL && tptr->command == PRODUCT && 
							tptr->parm[0].type == UTP_STRING)
								strcpy(labref,tptr->parm[0].ptype.str);
					}
				}
			}
		}
	}
*/
/*
........Push transformation onto stack
*/
//	if (use_tlabref) strcpy(labref,tlabref);
	status = utp_push_transform(rec1,rec2,tf,ncnt);
/*
.....End of routine
*/
done:;
}

/*********************************************************************
**    E_FUNCTION     :  utp_in_mechanical_design(ptr)
**          MECHANICAL_DESIGN_GEOMETRIC_PRESENTATION_REPRESENTATION
**          handling routine.  Attribute record dispatch routine.
**    PARAMETERS   
**       INPUT  : 
**          ptr      Pointer to STEP record in global array.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_in_mechanical_design(ptr)
UTPs_step_record *ptr;
{
	int i,relnum;
/*
.....Loop through attribute records
*/
	for (i=1;i<ptr->nparm;i++)
		if (ptr->parm[i].type == UTP_RECNO)
			utp_in_dispat_recno(ptr->parm[i].ptype.recno,&relnum);
	ptr->used = -1;
}

/*********************************************************************
**    E_FUNCTION     :  utp_in_bounded_wireframe(ptr)
**          GEOMETRICALLY_BOUNDED_WIREFRAME_SHAPE_REPRESENTATION handling
**          Usually used to reference wireframe definitions.
**    PARAMETERS   
**       INPUT  : 
**          ptr      Pointer to STEP record in global array.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_in_bounded_wireframe(ptr)
UTPs_step_record *ptr;
{
	int i,inc,relnum,irec;
	UU_KEY_ID key;
	UTPs_step_record *tptr;
/*
.....Define units for this entity
*/
	for (i=0;i<ptr->nparm;i++)
	{
		if (ptr->parm[i].type == UTP_RECNO)
		{
			tptr = utp_get_record(ptr->parm[i].ptype.recno);
			if (tptr != UU_NULL &&
				tptr->command == GEOMETRIC_REPRESENTATION_CONTEXT)
			{
				utp_map_units(tptr);
				break;
			}
		}
	}
/*
.....Initialize the attribute bundle
*/
	utp_set_attr(ptr,UU_TRUE);
/*
.....Create wireframe geometry
*/
	inc = i;
	for (i=0;i<ptr->nparm;i++)
	{
		if (ptr->parm[i].type == UTP_RECNO && i != inc)
		{
			irec = ptr->parm[i].ptype.recno;
			key = utp_in_dispat_recno(irec,&relnum);
			if (key != 0)
				utp_store_step_label(key,"",irec);
		}
	}
}

/*********************************************************************
**    E_FUNCTION     :  utp_in_curve_set(ptr)
**          GEOMETRIC_CURVE_SET handling routine. Usually used to
**          reference wireframe definitions.
**    PARAMETERS   
**       INPUT  : 
**          ptr      Pointer to STEP record in global array.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_in_curve_set(ptr)
UTPs_step_record *ptr;
{
	int i,relnum,irec;
	char label[NCL_MAX_LABEL];
	UU_KEY_ID key;
	UTPs_step_record *tptr;
/*
.....Get Curve set label
*/
	label[0] = '\0';
	if (ptr->parm[0].type == UTP_STRING)
		strcpy(label,ptr->parm[0].ptype.str);
/*
.....Initialize the attribute bundle
*/
	utp_set_attr(ptr,UU_TRUE);
/*
.....Create wireframe geometry
*/
	for (i=1;i<ptr->nparm;i++)
	{
		if (ptr->parm[i].type == UTP_RECNO)
		{
			irec = ptr->parm[i].ptype.recno;
			tptr = utp_get_record(irec);
			if (tptr->command == CARTESIAN_POINT)
			{
				 if (entity_mask[4] == 0) continue;
			}
			else 
			{
				 if (entity_mask[2] == 0) continue;
			}
			if (tptr->parm[0].type == UTP_STRING)
			{
				if (tptr->parm[0].ptype.str[0] != '\0' &&
					strcmp(tptr->parm[0].ptype.str,"NONE") != 0)
				{
/*					strcpy(label,tptr->parm[0].ptype.str); */
					strncpy(label, tptr->parm[0].ptype.str, NCL_MAX_LABEL-1);
					label[NCL_MAX_LABEL-1] = '\0';
				}
			}
			utp_set_attr(tptr,UU_FALSE);
			key = utp_in_dispat(tptr,&relnum);
			if (key != 0)
			{
				if (UIG_nodups && uig_match_entity(key) == UU_SUCCESS)
					utp_remove_dup(&key);
				else
				{
					utp_count_translated(relnum,1,UU_TRUE);
					utp_store_step_label(key,label,irec);
				}
			}
			else
				utp_count_translated(relnum,1,UU_FALSE);
		}
	}
}
