/*********************************************************************
**    NAME         :  tspsupt.c
**       CONTAINS:
**					utp_boundary_error
**					utp_circle_error
**					utp_curve_error
**					utp_ellipse_error
**					utp_units_error
**					utp_plane_error
**					utp_solid_error
**					utp_surface_error
**             utp_radius_error
**					utp_syntax_error
**					utp_wireframe_error
**					utp_xform_error
**					utp_invalid_ptr
**    COPYRIGHT 2013 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       tsperror.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:13:22
*********************************************************************/

#include "tiges.h"
#include "tigdefs.h"
#include "tstep.h"
#include "mcrv.h"
#include "udebug.h"

static void S_output();

/*********************************************************************
**    E_FUNCTION     :  utp_boundary_error(ptr)
**				Outputs an error message to the Status Window and Log file
**          stating that a surface boundary curve could not be created.
**    PARAMETERS   
**       INPUT  : 
**          ptr      Pointer to STEP record in global array.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_boundary_error(ptr)
UTPs_step_record *ptr;
{
/*
.....Output error message
*/
	S_output(ptr,"Error - could not create surface boundary curve.");
	return;
}

/*********************************************************************
**    E_FUNCTION     :  utp_circle_error(ptr)
**				Outputs an error message to the Status Window and Log file
**          stating that a circle could not be created.
**    PARAMETERS   
**       INPUT  : 
**          ptr      Pointer to STEP record in global array.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_circle_error(ptr)
UTPs_step_record *ptr;
{
/*
.....Output error message
*/
	S_output(ptr,"Error - could not create circle.");
	return;
}

/*********************************************************************
**    E_FUNCTION     :  utp_curve_error(ptr)
**				Outputs an error message to the Status Window and Log file
**          stating that a B-spline curve could not be created.
**    PARAMETERS   
**       INPUT  : 
**          ptr      Pointer to STEP record in global array.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_curve_error(ptr)
UTPs_step_record *ptr;
{
/*
.....Output error message
*/
	S_output(ptr,"Error - could not create B-spline curve.");
	return;
}

/*********************************************************************
**    E_FUNCTION     :  utp_ellipse_error(ptr)
**				Outputs an error message to the Status Window and Log file
**          stating that an Ellipse could not be created.
**    PARAMETERS   
**       INPUT  : 
**          ptr      Pointer to STEP record in global array.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_ellipse_error(ptr)
UTPs_step_record *ptr;
{
/*
.....Output error message
*/
	S_output(ptr,"Error - could not create Ellipse.");
	return;
}

/*********************************************************************
**    E_FUNCTION     :  utp_plane_error(ptr)
**				Outputs an error message to the Status Window and Log file
**          stating that a plane could not be created.
**    PARAMETERS   
**       INPUT  : 
**          ptr      Pointer to STEP record in global array.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_plane_error(ptr)
UTPs_step_record *ptr;
{
/*
.....Output error message
*/
	S_output(ptr,"Error - could not create plane geometry.");
	return;
}

/*********************************************************************
**    E_FUNCTION     :  utp_solid_error(ptr)
**				Outputs an error message to the Status Window and Log file
**          stating that a solid could not be created.
**    PARAMETERS   
**       INPUT  : 
**          ptr      Pointer to STEP record in global array.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_solid_error(ptr)
UTPs_step_record *ptr;
{
/*
.....Output error message
*/
	S_output(ptr,"Error - could not create composite solid geometry.");
	return;
}

/*********************************************************************
**    E_FUNCTION     :  utp_surface_error(ptr)
**				Outputs an error message to the Status Window and Log file
**          stating that a surface could not be created.
**    PARAMETERS   
**       INPUT  : 
**          ptr      Pointer to STEP record in global array.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_surface_error(ptr)
UTPs_step_record *ptr;
{
/*
.....Output error message
*/
	S_output(ptr,"Error - could not create surface geometry.");
	return;
}

/*********************************************************************
**    E_FUNCTION     :  utp_radius_error(ptr)
**				Outputs an error message to the Status Window and Log file
**          stating that a radius is too small.
**    PARAMETERS   
**       INPUT  : 
**          ptr      Pointer to STEP record in global array.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_radius_error(ptr)
UTPs_step_record *ptr;
{
/*
.....Output error message
*/
	S_output(ptr,"Error - could not create entity. Radius too small.");
	return;
}

/*********************************************************************
**    E_FUNCTION     :  utp_syntax_error(ptr)
**				Outputs an error message to the Status Window and Log file
**          stating that an input STEP record has an unrecognized
**          syntax.
**    PARAMETERS   
**       INPUT  : 
**          ptr      Pointer to STEP record in global array.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_syntax_error(ptr)
UTPs_step_record *ptr;
{
/*
.....Output error message
*/
	S_output(ptr,"Error processing STEP record.  Unrecognized syntax.");
	return;
}

/*********************************************************************
**    E_FUNCTION     :  utp_units_error(ptr)
**				Outputs an error message to the Status Window and Log file
**          stating that a Units record (tree) was unrecognized.
**    PARAMETERS   
**       INPUT  : 
**          ptr      Pointer to STEP record in global array.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_units_error(ptr)
UTPs_step_record *ptr;
{
/*
.....Output error message
*/
	S_output(ptr,"Error - unrecognized Units specification.");
	return;
}

/*********************************************************************
**    E_FUNCTION     :  utp_wireframe_error(ptr)
**				Outputs an error message to the Status Window and Log file
**          stating that a wireframe entity could not be created.
**    PARAMETERS   
**       INPUT  : 
**          ptr      Pointer to STEP record in global array.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_wireframe_error(ptr)
UTPs_step_record *ptr;
{
/*
.....Output error message
*/
	S_output(ptr,"Error - could not create wireframe entity.");
	return;
}


/*********************************************************************
**    E_FUNCTION     :  utp_xform_error(ptr)
**				Outputs an error message to the Status Window and Log file
**          stating that an xform matrix could not be created.
**    PARAMETERS   
**       INPUT  : 
**          ptr      Pointer to STEP record in global array.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_xform_error(ptr)
UTPs_step_record *ptr;
{
/*
.....Output error message
*/
	S_output(ptr,"Error - could not create transformation matrix.");
	return;
}

/*********************************************************************
**    E_FUNCTION     :  utp_invalid_ptr(recno)
**				Outputs an error message to the Status Window and Log file
**          stating that a pointer to a STEP record is not valid.
**    PARAMETERS   
**       INPUT  : 
**          recno    Invalid record number.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_invalid_ptr(recno)
int recno;
{
	char sbuf[80];
/*
.....Output error message
*/
	sprintf(sbuf,"Error - Reference to nonexistent record number %d.\n\n",recno);
	uig_list_out(sbuf,UU_TRUE);
	return;
}

/*********************************************************************
**    E_FUNCTION     :  S_output(ptr,msg)
**				Outputs an error message to the Status Window and Log file.
**    PARAMETERS   
**       INPUT  : 
**          ptr      Pointer to STEP record in global array.
**          msg      Error message to output.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_output(ptr,msg)
UTPs_step_record *ptr;
char *msg;
{
/*
.....Output error message
*/
	uig_list_out(msg,UU_TRUE);
	uig_list_out("\n",UU_TRUE);
/*
.....Output STEP command
*/
	if (ptr != UU_NULL) utp_debug_record(ptr,UU_TRUE,0);
	uig_list_out(" \n",UU_TRUE);
	return;
}

