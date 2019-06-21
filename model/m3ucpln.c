/*********************************************************************
**    NAME         :  m3ucpln
**       CONTAINS: user interface routines to set construction plane
**			umu_setcpln(option)
**			umu_snap_cpln()
**			umu_snap_cpln_to_plane()
**			umu_rot_cpln()
**			umu_align_cpln()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**      m3ucpln.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**      04/29/15 , 15:07:58
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "dasnog.h"
#include "mdcpln.h"
#include "view.h"
#include "mdrel.h"
#include "mdpick.h"
#include "mcrv.h"
#include "mpopmenu.h"
#include "nclvx.h"

/*********************************************************************
**    U_FUNCTION     : umu_setcpln(option)
**			Prompt the user for the parameters necessary for performing
**			the following operations concerning modification of the 
**			location and orientation of the construction plane:
**				1. reset to model coordinate system
**				2. snap to a viewing plane
**				3. change the origin
**				4. change the z depth
**    PARAMETERS   
**       INPUT  : 
**				option			0 => reset to model coordinate system;
**									1 => set to viewing plane;
**									2 => change origin; 
**									3 => change z-depth; 
**       OUTPUT :  
**          orig_label: when change origin and use use pick to pck a PT label
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void umu_setcpln(option, orig_label)
int		option;
char *orig_label;
	{              
	UV_vport		vport;				/* definition of viewport */
	UM_length	zdepth;				/* new z depth */
/**	UM_coord		origin;				 new origin */
    UD_NDCLOCREC origin;

	UM_vector	normal;				/* new normal vector */
	UM_vector	upvec;				/* new up vector */
	int			numint;				/* For das calls */
	int			status;				/* 0 iff const. plane set, else -1 */

	uu_denter( UU_MTRC,(us,"umu_setcpln(option=%d)",option));

	orig_label[0] = '\0';
	switch(option)
		{
		case 0:
			status = um_setcpln_reset();
			break;
		case 1:
			uvu_pickvp(&vport);
			uv_getvinfo(vport.key, &origin, normal, upvec);
			status = um_setcpln_view(&origin, normal, upvec);
			break;
		case 2:
			um_vctovc(UM_cpln.origin, &origin);
			origin.label[0] = '\0';
			ud_ldas(UD_SCACART2, /*construction plane origin*/UM_MODEL, 44,
					  &origin, 1, &numint, UD_DEFAULT);
			if (numint <= 0) goto done;
			status = um_setcpln_origin(&origin);
			strncpy(orig_label, origin.label, NCL_MAX_LABEL);
			break;
		case 3:
			zdepth = UM_cpln.zdepth;
    		ud_ldas(UD_DASDISTANCE, /*construction plane z depth:*/UM_MODEL,
					43, &zdepth, 1, &numint, UD_DEFAULT);
			if (numint <= 0) goto done;
			status = um_setcpln_zdepth(zdepth);
			break;
    	}
	if (option != 3) um_drw_cpl_axis(UU_FALSE, UU_TRUE);

done:
	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION     : umu_snap_cpln()
**			Prompt the user to pick an entity. The picked location is
**			used to define the origin and an axis (via popup menu) by
**			using the class routines uc_ploc_to_coord and uc_ploc_to_vector
**			to define the new origin and axis vector.
**    PARAMETERS   
**       INPUT  : 
**				none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void umu_snap_cpln()

	{
	int option;
	UD_PLOCREC pick;					/* pick information */
	int numint;							/* number of DAS entries returned */
	int	status;						/* status of return from um_n1_nrendpt */
	UM_vector	new_axis;
	UM_coord		origin;

	uu_denter(UU_MTRC,(us,"umu_snap_cpln()"));

	um_popupmenu(UM_SNAP_TO_LINE, &option);

	ud_ldas(UD_DASPCKLOC,/*pick a line*/UM_MODEL, 137, &pick, 1, &numint);
	if (numint <= 0) goto done;

	/* get the new origin */
	status = uc_ploc_to_coord(2, &pick.ppath, &pick.pndc, origin);
	if (status != UU_SUCCESS) goto done;

	/* get the new axis vector */
	status = uc_ploc_to_vector(2, &pick.ppath, &pick.pndc, new_axis);
	if (status != UU_SUCCESS) goto done;

	/* set the new origin and axis vector */
	status = um_setcpln_origin(origin);

	/* set the new axis */
	switch (option)
		{
		case 1: /* set the new x direction */
			um_setcpln_xaxis(new_axis);
			break;
		case 2: /* set the new y direction */
			um_setcpln_yaxis(new_axis);
			break;
		case 3: /* set the new z xaxis */
			um_setcpln_zaxis(new_axis);
			break;
		}

	/* draw the new axis */
	um_drw_cpl_axis(UU_FALSE, UU_TRUE);

done:
	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION     : umu_snap_cpln_to_plane()
**			Prompt the user to define a plane and origin. Then snap the
**			construction plane's normal to the plane normal.
**    PARAMETERS   
**       INPUT  : 
**				none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void umu_snap_cpln_to_plane()

	{
	UD_PLANE_REC   plane;				/* data return buffer */
	int numint;							/* number of DAS entries returned */

	uu_denter(UU_MTRC,(us,"umu_snap_cpln_to_plane()"));

/*
.....Initialize transform to something meaningfull in case is doesn't
.....get set.  This corrects bad display of plane arrows.
*/
	plane.transform = 1;
	ud_get_plane(UM_MODEL, 182, &plane, 1, &numint, UD_NODEFAULT);
	if (numint <= 0) goto done;   

	/* set the new origin and axis vector */
	um_setcpln_origin(plane.org);

		/* set the new z xaxis */
	um_setcpln_zaxis(plane.normal_vector);

	/* draw the new axis */
	um_drw_cpl_axis(UU_FALSE, UU_TRUE);

done:
	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION     : umu_rot_cpln()
**       Rotate the construction plane about the x, y, or z axis.
**    PARAMETERS   
**       INPUT  : 
**				none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void umu_rot_cpln()
	{
	int option;
	int numint;							/* number of DAS entries returned */
	UM_transf rotmat;					/* rotation matrix */
	UU_REAL	ang;						/* clockwise angle of rotation */
	UM_vector temp;					/* temporary for vector manipulations */

	uu_denter(UU_MTRC,(us,"umu_rot_cpln()"));

	um_popupmenu(UM_ROTATE_ABOUT_AXIS, &option);

	ud_ldas(UD_DASANGLE, /*angle of rotation*/UM_MODEL, 77, &ang, 
					 1, &numint, UD_NODEFAULT);
	if (numint > 0)
		{
		switch (option)
			{
			case 1:
				/* get the rotation matrix */
				um_rottf(UM_cpln.xaxis, ang, rotmat);

				/* rotate the x and y axis */
				um_cctmtf(UM_cpln.zaxis, rotmat, UM_cpln.zaxis);
				um_cross(UM_cpln.zaxis, UM_cpln.xaxis, temp);
				um_cross(UM_cpln.xaxis, temp, UM_cpln.zaxis);
				um_unitvc(UM_cpln.xaxis, UM_cpln.xaxis);
				um_unitvc(UM_cpln.zaxis, UM_cpln.zaxis);
				um_cross(UM_cpln.zaxis, UM_cpln.xaxis, UM_cpln.yaxis);

				break;

			case 2:
				/* get the rotation matrix */
				um_rottf(UM_cpln.yaxis, ang, rotmat);

				/* rotate the x and z axis */
				um_cctmtf(UM_cpln.xaxis, rotmat, UM_cpln.xaxis);
				um_cross(UM_cpln.xaxis, UM_cpln.yaxis, temp);
				um_cross(UM_cpln.yaxis, temp, UM_cpln.xaxis);
				um_unitvc(UM_cpln.xaxis, UM_cpln.xaxis);
				um_unitvc(UM_cpln.yaxis, UM_cpln.yaxis);
				um_cross(UM_cpln.xaxis, UM_cpln.yaxis, UM_cpln.zaxis);

				break;

			case 3:
				/* get the rotation matrix */
				um_rottf(UM_cpln.zaxis, ang, rotmat);

				/* rotate the x and y axis */
				um_cctmtf(UM_cpln.yaxis, rotmat, UM_cpln.yaxis);
				um_cross(UM_cpln.yaxis, UM_cpln.zaxis, temp);
				um_cross(UM_cpln.zaxis, temp, UM_cpln.yaxis);
				um_unitvc(UM_cpln.zaxis, UM_cpln.zaxis);
				um_unitvc(UM_cpln.yaxis, UM_cpln.yaxis);
				um_cross(UM_cpln.yaxis, UM_cpln.zaxis, UM_cpln.xaxis);

				break;

			}
		/* draw the new axis */
		um_drw_cpl_axis(UU_FALSE, UU_TRUE);
		}
	uu_dexit;
	}
/*********************************************************************
**    U_FUNCTION     : umu_align_cpln()
**			Display a pop up menu to determine which construction plane
**			axis to align with the user specified vector.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          flag: 0: nothing picked
**					1: X-axis picked
**					2: Y-axis picked
**					3: Z-axis picked
**			axis_label: picked VE/PE entity label
**					label[0] = '\0', not picked anything, just input Vector
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void umu_align_cpln(flag, axis_label)
int *flag;
char *axis_label;
	{              
	int		option;
	UM_vector	normal;				/* new normal vector */
	UD_DASCORD	upvec;				/* new up vector */
	UD_DASCORD	xaxis;				/* new x vector */
	int			numint;				/* For das calls */
	int			status;				/* 0 iff const. plane set, else -1 */

	uu_denter( UU_MTRC,(us,"umu_align_cpln()"));

	*flag = 0;
	axis_label[0] = '\0';
	um_popupmenu(UM_ALIGN_ALONG_VEC, &option);

	switch(option)
		{
		case 1:
			um_vctovc(UM_cpln.xaxis, xaxis.cord);
			xaxis.label[0] = '\0';
			ud_ldas(UD_SCAVEC2, /*construction plane x axis*/UM_MODEL, 260, 
				&xaxis, 1, &numint, UD_DEFAULT);
			if (numint <= 0) goto done;
			status = um_setcpln_xaxis(xaxis.cord);
			strncpy(axis_label, xaxis.label, NCL_MAX_LABEL);
			*flag = 1;
			break;
		case 2:
			um_vctovc(UM_cpln.yaxis, upvec.cord);
			upvec.label[0] = '\0';
			ud_ldas(UD_SCAVEC2, /*construction plane y axis*/UM_MODEL, 46, 
				&upvec, 1, &numint, UD_DEFAULT);
			if (numint <= 0) goto done;
			status = um_setcpln_yaxis(upvec.cord);
			strncpy(axis_label, upvec.label, NCL_MAX_LABEL);
			*flag = 2;
			break;
		case 3:
			um_vctovc(UM_cpln.zaxis, normal);
			ud_ldas(UD_DASVEC, /*construction plane z axis*/UM_MODEL, 45,
						 normal, 1, &numint, UD_DEFAULT);
			if (numint <= 0) goto done;
			status = um_setcpln_zaxis(normal);
			*flag = 3;
			break;
    	}
	um_drw_cpl_axis(UU_FALSE, UU_TRUE);

done:
	uu_dexit;
	}
