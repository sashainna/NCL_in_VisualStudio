/*********************************************************************
**    NAME         :  m3ecpln2.c
**       CONTAINS: routine to set/get construction plane
**			um_cplinit()
**			um_drw_cpl_axis(visible, modified)
**			um_setcpln_view()
**			um_setcpln_origin(base_pt)
**			um_setcpln_zaxis(normal)
**			um_setcpln_yaxis(upvec)
**			um_setcpln_zdepth(zdepth)
**			um_setcpln_reset()
**			um_cpltrans(cplpt,world_pt)
**			um_getcpln(origin, xaxis, yaxis, zaxis)
**			um_setcpln(origin, xaxis, yaxis, zaxis)
**			um_linear_units_str(unit, str)
**			um_setcpln_xaxis(xaxis)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       m3ecpln2.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:07:53
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "go.h"
#include "dasnog.h"
#include "lcom.h"
#include "modef.h"
#include "mdrel.h"
#include "mxxx.h"
#include "mattr.h"
#include "mdattr.h"
#include "mdunits.h"
#include "mdcpln.h"
#include "nclfc.h"
#include "nccs.h"
#include "mcrv.h"

/*********************************************************************
**    I_FUNCTION     : um_cplinit()
**      Initialize the parameters defining the construction plane
**			to be identical with the model (world) coordinate system.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_cplinit()

	{
	struct UM_coordsys_rec ccs;		/* construction coordinate system */
	struct UM_attrdata_rec attr;		/* display attributes */

	struct UM_point_rec *e1;

	uu_denter( UU_MTRC,(us,"um_cplinit()"));

	/* assign initial values to construction coordinate system in UNIBASE */
/*
.....Assigned initial value to label to avoid the UMR error in purify
*/
	e1 = (struct UM_point_rec *) &ccs;
	e1->label[0]='\0';
	um_vctovc(UM_zerovec, ccs.origin);
	um_vctovc(UM_xaxis, ccs.xaxis);
	um_vctovc(UM_yaxis, ccs.yaxis);
	um_vctovc(UM_zaxis, ccs.zaxis);
	ccs.z_depth = 0.0;
	strcpy(ccs.name, "CPLN");
	attr.key = -1;
	attr.rel_num = UM_ATTR_REL;
	attr.color = UL_color_mod.waxis;
	attr.layer = 1;
	attr.line_style = UM_SOLID_LINE;
	attr.line_width = 0.0;
	attr.line_weight = 1.0;
	attr.pen = 1;
	attr.displayable = UM_DISPLAYABLE;
	attr.selectable = UU_FALSE;
	ur_setup_data(UM_COORDSYS_REL, &ccs, sizeof(struct UM_coordsys_rec));
	um_create_geom(&ccs, UM_DEFAULT_TF, &attr);
	ur_update_blanked(ccs.key, UU_TRUE);
	ur_put_dispattr_cpln_key(ccs.key);

	uu_dexit;
	return(UU_SUCCESS);
	}
/*********************************************************************
**
**      FUNCTION:  umf_drw_cpl_axis
**
**      Is a buffer between the fortran routine disply and
**      um_drw_cpl_axis
**
**********************************************************************/
umf_drw_cpl_axis()

	{
	uu_denter(UU_MTRC,(us,"umf_drw_cpl_axis()"));
	um_drw_cpl_axis(UU_TRUE,UU_FALSE);

	return(UU_SUCCESS);
	}

/*********************************************************************
**
**      FUNCTION:  umf_drw_cpl_axis_off
**
**      Is a buffer between the fortran routine delgeo and
**      um_drw_cpl_axis, to turn off the display.
**
**********************************************************************/
umf_drw_cpl_axis_off()

	{
	uu_denter(UU_MTRC,(us,"umf_drw_cpl_axis_off()"));
	um_drw_cpl_axis(UU_FALSE,UU_FALSE);

	return(UU_SUCCESS);
	}
/*********************************************************************
**    I_FUNCTION     : um_drw_cpl_axis(visible, modified)
**       Make the construction plane axis visible or invisible.
**    PARAMETERS   
**       INPUT  : 
**          visible			UU_TRUE => make construction axis visible
**									UU_FALSE => update display if modified and
**													axis already displayed or turn
**													axis off if not modified
**				modified			UU_TRUE => axis data modified
**									UU_FALSE => data not modified
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_drw_cpl_axis(visible, modified)
	UU_LOGICAL visible;
	UU_LOGICAL modified;

	{
	struct NCL_fixed_databag e;
	struct UM_coordsys_rec *ccsp;
	int dsegid;
	UU_LOGICAL turn_on;
	UU_LOGICAL blanked;
	UU_KEY_ID cpln_key;
	int status;
		
	ccsp = (struct UM_coordsys_rec *)&e;
	cpln_key = ur_get_dispattr_cpln_key();
	ur_retrieve_disp_segid(cpln_key, &dsegid);
	ur_retrieve_blanked(cpln_key, &blanked);
	turn_on = (modified && (dsegid != -1) && !blanked) || visible;
	if (turn_on)
	{
		ur_update_blanked(cpln_key, UU_FALSE);
		ccsp->key = cpln_key;
		status = ncl_retrieve_data_fixed(ccsp);
		if (status == UU_SUCCESS && ccsp->rel_num == UM_COORDSYS_REL)
		{
			um_vctovc(UM_cpln.origin, ccsp->origin);
			um_vctovc(UM_cpln.xaxis, ccsp->xaxis);
			um_vctovc(UM_cpln.yaxis, ccsp->yaxis);
			um_vctovc(UM_cpln.zaxis, ccsp->zaxis);
			ccsp->z_depth = UM_cpln.zdepth;
			um_update_geom(ccsp, UM_DEFAULT_TF);
			uc_display(ccsp);
		}
	}
	else
		{
		if (dsegid != -1)
			{
			uv_delsegs(dsegid);
			ur_update_disp_segid(cpln_key, -1);
			}
		ur_update_blanked(cpln_key, UU_TRUE);
		}
	if (modified && UM_cpln.grid.disp) um_draw_grid(UU_FALSE);
	uu_dexit;
	return(UU_SUCCESS);
	}

/*********************************************************************
**    E_FUNCTION     : um_setcpln_view()
**      Set the construction plane using the parameters defining a 
**			viewing plane.
**       INPUT  : 
**				origin					origin of view plane
**				normal					normal of view plane
**				upvect					up vector of view plane
**       OUTPUT :  
**          none
**    RETURNS      :
**				0 iff construction plane reset; -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_setcpln_view(origin, normal, upvect)
	UM_coord origin;
	UM_vector normal;
	UM_vector upvect;

	{              
	int status;
	UM_vector temp;

	uu_denter( UU_MTRC,(us,"um_setcpln_view()"));
	status = UU_FAILURE;
	if (!um_vcparall(normal, upvect))
		{
		um_vctovc(origin, UM_cpln.origin);
		um_unitvc(normal, UM_cpln.zaxis);
		um_vcplvc(origin, upvect, temp);
		um_nptpln(temp, origin, normal, UM_cpln.yaxis);
		um_vcmnvc(UM_cpln.yaxis, UM_cpln.origin, UM_cpln.yaxis);
		um_unitvc(UM_cpln.yaxis, UM_cpln.yaxis);
		um_cross(UM_cpln.yaxis, UM_cpln.zaxis, UM_cpln.xaxis);
		status = UU_SUCCESS;
		}
	uu_dexit;
	return(status);
	}
/*********************************************************************
**    E_FUNCTION     : um_setcpln_origin(base_pt)
**      Set the construction plane origin.
**    PARAMETERS   
**       INPUT  : 
**				base_pt:			New origin of the construction plane.
**       OUTPUT :  
**          none
**    RETURNS      :
**				0 iff construction plane is reset; -1 otherwise.
**    SIDE EFFECTS : 		none
**    WARNINGS     : 		none
*********************************************************************/
int
um_setcpln_origin(origin)
	UM_coord		origin;

	{              

	uu_denter(UU_MTRC,(us,"um_setcpln_origin()"));
	um_vctovc(origin, UM_cpln.origin);
	uu_dexit;
	return(0);
	}
/*********************************************************************
**    E_FUNCTION     : um_setcpln_zaxis(normal)
**			Set the construction plane using a new construction plane normal.
**			The vectors are checked for ortho-normality.
**    PARAMETERS   
**       INPUT  : 
**				normal				   new construction plane normal
**       OUTPUT :  
**          none
**    RETURNS      : 
**				0 iff construction plane is reset; -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_setcpln_zaxis(normal)
	UM_vector	normal;				/* new construction plane normal */

	{              
	UM_angle		ang;				/* Angle through which to rotate up-vector */
	UU_REAL		signtest;		/* determines sign of angle */
	UM_vector	temp;				/* Temporary for vector manipulations */
	UM_transf	matrix;			/* Rotation matrix */
	int			status;			/* 0 iff const. plane reset, -1 otherwise */

	uu_denter( UU_MTRC,(us,"um_setcpln_zaxis()"));

	um_unitvc(normal,normal);
	if (um_cceqcc(normal,UM_zerovec))
		{
		uu_uerror0(/*vector is essentially a zero vector*/UM_MODEL,81);
		status = -1;
		}
	else
		{
		status = 0;
		if (um_vcparall(normal,UM_cpln.zaxis))
			{
			ang = 180.0;
			um_rottf( UM_cpln.yaxis,ang,matrix);
			}
		else
			{
			um_cross(UM_cpln.zaxis,normal,temp);
			ang = um_angle(UM_cpln.zaxis,normal);
			signtest = um_dot(UM_cpln.zaxis,temp);
			if (signtest < 0.0) ang = -ang;
			um_rottf(temp,ang,matrix);
			}
		um_cctmtf(UM_cpln.yaxis,matrix,UM_cpln.yaxis);
		um_vctovc(normal,UM_cpln.zaxis);

		um_cross(UM_cpln.yaxis,  UM_cpln.zaxis, temp);
		um_cross(UM_cpln.zaxis, temp,  UM_cpln.yaxis);
		um_unitvc(UM_cpln.zaxis,  UM_cpln.zaxis);
		um_unitvc(UM_cpln.yaxis,  UM_cpln.yaxis);
		um_cross(UM_cpln.yaxis,  UM_cpln.zaxis, UM_cpln.xaxis);
		}

	uu_dexit;
	return( status);
	}
/*********************************************************************
**    E_FUNCTION     : um_setcpln_yaxis(upvec)
**     Set the construction plane using a new up vector.
**      The vectors are checked for ortho-normality.
**    PARAMETERS   
**       INPUT  : 
**				upvec;				   new up vector
**       OUTPUT :  
**          none
**    RETURNS      : status
**				0 iff const. plane reset; -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_setcpln_yaxis(upvec)
	UM_vector	upvec;				/* new up vector */

	{              
	UM_angle		ang;				/* Angle through which to rotate up-vector */
	UU_REAL		signtest;		/* determines sign of angle */
	UM_vector	temp;				/* Temporary for vector manipulations */
	UM_transf	matrix;			/* Rotation matrix */
	int 			status;			/* 0 iff const. plane reset; else -1 */

	uu_denter( UU_MTRC,(us,"um_setcpln_yaxis()"));

	um_unitvc(upvec, upvec);
	if (um_cceqcc(upvec, UM_zerovec))
		{
		uu_uerror0(/* vector is essentially the zero vector*/UM_MODEL, 81);
		status = -1;
		}
	else
		{
		status = 0;
		if (um_vcparall(upvec,UM_cpln.yaxis))
			{
			signtest = um_dot(upvec,UM_cpln.yaxis);
			if (signtest < 0.0) ang = UM_PI; else ang = 0.0;
			um_rottf(UM_cpln.zaxis,ang,matrix);
			}
		else
			{
			um_cross(UM_cpln.yaxis,upvec,temp);
			ang = um_angle(upvec,UM_cpln.yaxis);
/*
.....Work around for problem on windows NT when angle between
.....vectors ~= 7.5e-8, ang is sometimes returned as indefinite
.....value (1.#IND000000000). Calling um_angle twice corrects
.....problem. IJD 25-FEB-2002
*/
#if UU_COMP == UU_WIN2K
			ang = um_angle(upvec,UM_cpln.yaxis);
#endif
			signtest = um_dot(UM_cpln.zaxis,temp);
			if (signtest < 0.0) ang = -ang;
			um_rottf(temp,ang,matrix);
			}
		um_cctmtf(UM_cpln.zaxis,matrix,UM_cpln.zaxis);
		um_vctovc(upvec, UM_cpln.yaxis);

		um_cross(UM_cpln.yaxis,  UM_cpln.zaxis, temp);
		um_cross(UM_cpln.zaxis, temp,  UM_cpln.yaxis);
		um_unitvc(UM_cpln.zaxis,  UM_cpln.zaxis);
		um_unitvc(UM_cpln.yaxis,  UM_cpln.yaxis);
		um_cross(UM_cpln.yaxis,  UM_cpln.zaxis, UM_cpln.xaxis);
		}

	uu_dexit;
	return( status);
	}
/*********************************************************************
**    E_FUNCTION     : um_setcpln_zdepth(zdepth)
**      Set the construction plane using z-depth. The vectors are 
**			checked for ortho-normality.
**    PARAMETERS   
**       INPUT  : 
**				zdepth				   new z - depth 
**       OUTPUT :  
**          none
**    RETURNS      : status
**			0 iff construction plane reset; -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_setcpln_zdepth(zdepth)
	UM_length	zdepth;

	{              
	int		status;				/* 0 iff const. plane reset, else -1 */

	uu_denter( UU_MTRC,(us,"um_setcpln_zdepth()"));
	status = 0;
	UM_cpln.zdepth = zdepth;
	uu_dexit;
	return( status);
	}
/*********************************************************************
**    E_FUNCTION     : um_setcpln_reset()
**      Reset the construction plane. The vectors are checked
**			for ortho-normality.
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**          none
**    RETURNS      : status
**			0 iff const. plane reset, -1 otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_setcpln_reset()

	{              

	uu_denter( UU_MTRC,(us,"um_setcpln()"));

	um_vctovc(UM_zerovec, UM_cpln.origin);
	um_vctovc(UM_xaxis, UM_cpln.xaxis);
	um_vctovc(UM_yaxis, UM_cpln.yaxis);
	um_vctovc(UM_zaxis, UM_cpln.zaxis);
	UM_cpln.zdepth = 0.0;

	uu_dexit;
	return(UU_SUCCESS);
	}

/*********************************************************************
**    E_FUNCTION     : um_cpltrans(cplpt,world_pt)
**       Process a point in user's  coordinates through the construction plane
**       definition to yield a point in world  coordinates.
**    PARAMETERS   
**       INPUT  : 
**				cplpt    input point in user's  coordinates
**       OUTPUT :  
**				world_pt output point in world  coordinates
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_cpltrans(cplpt,world_pt)
	 UM_coord  cplpt;
	 UM_coord  world_pt;

	{


    world_pt[ UM_X] =  UM_cpln.origin[ UM_X] + (cplpt[ UM_X] * UM_cpln.xaxis[ UM_X]  +  cplpt[ UM_Y] *  UM_cpln.yaxis[ UM_X]
                                     + cplpt[ UM_Z] *  UM_cpln.zaxis[ UM_X] );
    world_pt[ UM_Y] =  UM_cpln.origin[ UM_Y] + (cplpt[ UM_X] * UM_cpln.xaxis[ UM_Y]  +  cplpt[ UM_Y] *  UM_cpln.yaxis[ UM_Y]
                                     + cplpt[ UM_Z] *  UM_cpln.zaxis[ UM_Y] );
    world_pt[ UM_Z] =  UM_cpln.origin[ UM_Z] + (cplpt[ UM_X] * UM_cpln.xaxis[ UM_Z]  +  cplpt[ UM_Y] *  UM_cpln.yaxis[ UM_Z]
                                     + cplpt[ UM_Z] *  UM_cpln.zaxis[ UM_Z] );

	return(UU_SUCCESS);
	}
/*********************************************************************
**    E_FUNCTION     : um_getcpln(origin, xaxis, yaxis, zaxis)
**       Get the definition of the current construction plane.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          origin					origin of construction plane
**          xaxis						x axis of construction plane
**          yaxis						y axis of construction plane
**          zaxis						z axis of construction plane
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_getcpln(origin, xaxis, yaxis, zaxis)
	UM_coord origin;
	UM_vector xaxis;
	UM_vector yaxis;
	UM_vector zaxis;

	{
	uu_denter(UU_MTRC,(us,"um_getcpln()"));
	um_vctovc(UM_cpln.origin, origin);
	um_vctovc(UM_cpln.xaxis, xaxis);
	um_vctovc(UM_cpln.yaxis, yaxis);
	um_vctovc(UM_cpln.zaxis, zaxis);
	uu_dexit;
	return(UU_SUCCESS);
	}
/*********************************************************************
**    E_FUNCTION     : um_setcpln(origin, xaxis, yaxis, zaxis)
**       Set the definition of the current construction plane.
**    PARAMETERS   
**       INPUT  : 
**          origin					origin of construction plane
**          xaxis						x axis of construction plane
**          yaxis						y axis of construction plane
**          zaxis						z axis of construction plane
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : 
**			no checking is performed to see that the vectors are
**			orthogonal unit vectors.
*********************************************************************/
um_setcpln(origin, xaxis, yaxis, zaxis)
	UM_coord origin;
	UM_vector xaxis;
	UM_vector yaxis;
	UM_vector zaxis;

	{
	uu_denter(UU_MTRC,(us,"um_setcpln()"));
/* 	um_vctovc( origin, UM_cpln.origin); */
/* 	um_vctovc( xaxis, UM_cpln.xaxis); */
/* 	um_vctovc( yaxis, UM_cpln.yaxis); */
/* 	um_vctovc( zaxis, UM_cpln.zaxis); */
	ncl_mcstowcs(0, origin, UM_cpln.origin);
	ncl_mcstowcs(1, xaxis, UM_cpln.xaxis);
	ncl_mcstowcs(1, yaxis, UM_cpln.yaxis);
	ncl_mcstowcs(1, zaxis, UM_cpln.zaxis);
	uu_dexit;
	return(UU_SUCCESS);
	}
/*********************************************************************
**    E_FUNCTION     :	um_setcpln_xaxis(xaxis)
**       Set the construction plane using the new x axis.
**    PARAMETERS   
**       INPUT  : 
**          xaxis				new x axis
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_setcpln_xaxis(xaxis)
	UM_vector	xaxis;				/* new construction plane xaxis */

	{              
	UM_angle		ang;				/* Angle through which to rotate up-vector */
	UU_REAL		signtest;		/* determines sign of angle */
	UM_vector	temp;				/* Temporary for vector manipulations */
	UM_transf	matrix;			/* Rotation matrix */
	int			status;			/* 0 iff const. plane reset, -1 otherwise */

	uu_denter( UU_MTRC,(us,"um_setcpln_xaxis()"));

	um_unitvc(xaxis,xaxis);
	if (um_cceqcc(xaxis,UM_zerovec))
		{
		uu_uerror0(/*vector is essentially a zero vector*/UM_MODEL,81);
		status = -1;
		}
	else
		{
		status = 0;
		if (um_vcparall(xaxis,UM_cpln.xaxis))
			{
			ang = 180.0;
			um_rottf( UM_cpln.yaxis,ang,matrix);
			}
		else
			{
			um_cross(UM_cpln.xaxis,xaxis,temp);
			ang = um_angle(UM_cpln.xaxis,xaxis);
			signtest = um_dot(UM_cpln.xaxis,temp);
			if (signtest < 0.0) ang = -ang;
			um_rottf(temp,ang,matrix);
			}
		um_cctmtf(UM_cpln.yaxis,matrix,UM_cpln.yaxis);
		um_vctovc(xaxis,UM_cpln.xaxis);

		um_cross(UM_cpln.xaxis,  UM_cpln.yaxis, temp);
		um_cross(UM_cpln.yaxis, temp,  UM_cpln.xaxis);
		um_unitvc(UM_cpln.xaxis,  UM_cpln.xaxis);
		um_unitvc(UM_cpln.yaxis,  UM_cpln.yaxis);
		um_cross(UM_cpln.xaxis,  UM_cpln.yaxis, UM_cpln.zaxis);
		}

	uu_dexit;
	return( status);
	}
