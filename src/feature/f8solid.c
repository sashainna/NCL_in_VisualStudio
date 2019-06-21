/*********************************************************************
**    NAME         :  f8solid.c 
**       CONTAINS: features for Visual Solids
**         um_f8solid
**    COPYRIGHT 2008 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       f8solid.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:05:44
*********************************************************************/

#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "dasnog.h"
#include "mdcoord.h"
#include "mcrv.h"
#include "mdeval.h"
#include "modef.h"
#include "msol.h"

/*********************************************************************
**    E_FUNCTION     :  int um_f8solid(ent, tfmat, feature_order, dploc)
**      calculate the desired features for a given order 
**       for the geometry type "solid" 
**    PARAMETERS   
**       INPUT  : 
**				ent				pointer to the geometric information
**				tfmat				transformation matrix.
**          feature_order	1 => center, radius, tangents
**									2 => arc length, angle, quarter points
**									3 => normal
**				dploc				picked location on circle
**       OUTPUT :  
**          none
**    RETURNS      : 
**			UU_SUCCESS if no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_f8solid(ent, tfmat, feature_order, dploc)
struct UM_solid_rec *ent;
UM_transf tfmat;
int feature_order;
UD_PLOCREC *dploc;
{
	int status;
/*
.....Initialize routine
*/
	status = UU_SUCCESS;
/*
.....Transform geometry to model space
*/
	ncl_transform_solid(ent,tfmat,UU_FALSE);
	switch (ent->type)
	{
/*
.....Box solid
*/
	case UM_BOX_SOLID:
		status = S_box_feature(ent,tfmat,feature_order,dploc);
		break;
/*
.....Cone, Cylinder solid
*/
	case UM_CONE_SOLID:
	case UM_CYLINDER_SOLID:
		status = S_cone_feature(ent,tfmat,feature_order,dploc);
		break;
/*
.....Sphere solid
*/
	case UM_SPHERE_SOLID:
		status = S_sphere_feature(ent,tfmat,feature_order,dploc);
		break;
/*
.....Torus solid
*/
	case UM_TORUS_SOLID:
		status = S_torus_feature(ent,tfmat,feature_order,dploc);
		break;
/*
.....Extruded solid
*/
	case UM_EXTRUDED_SOLID:
	case UM_CONTOUR_SOLID:
		status = S_extruded_feature(ent,tfmat,feature_order,dploc);
		break;
/*
.....Revolved solid
*/
	case UM_REVOLVED_SOLID:
		status = S_revolved_feature(ent,tfmat,feature_order,dploc);
		break;
/*
.....Unrecognized solid
*/
	default:
		uu_uerror0(UU_FEATERROR,3);
		status = UU_FAILURE;
		break;
	}
	return(status);
}

/*********************************************************************
**    E_FUNCTION     :  S_box_feature(ent,tfmat,feature_order,dploc)
**      calculate the desired features for a given order 
**       for the solid type Box.
**    PARAMETERS   
**       INPUT  : 
**				ent				pointer to the geometric information
**				tfmat				transformation matrix.
**          feature_order	1 => center, length, height
**                       	2 => lower left, upper right
**                       	3 => all other corners
**				dploc				picked location on circle
**       OUTPUT :  
**          none
**    RETURNS      : 
**			UU_SUCCESS if no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_box_feature(ent,tfmat,feature_order,dploc)
struct UM_solid_rec *ent;
UM_transf tfmat;
int feature_order;
UD_PLOCREC *dploc;
{
	int status;
	UU_LOGICAL dflag;
	UU_REAL char_height,lnx,lny,lnz;
	UM_coord pos,cp,pt1,ptx[8];
	UM_vector vcx,vcy,vcz,vc1;

	UM_coord vporigin;
	UM_vector vpxaxis;
	UM_vector vpyaxis;
	UM_vector vpzaxis;
	UU_REAL aperture;
/*
.....Initialize routine
*/
	status = UU_SUCCESS;
/*
.....Get current view definition
*/
	um_get_current_viewplane(vporigin, vpxaxis, vpyaxis, vpzaxis, &aperture);
/*
.....Set feature character height
*/
	um_set_feature_char_height(&char_height);
/*
.....Calculate all corners of box
*/
	lnx = um_mag(&ent->sdata[3]);
	lny = um_mag(&ent->sdata[6]);
	lnz = um_mag(&ent->sdata[9]);
	um_unitvc(&ent->sdata[3],vcx);
	um_unitvc(&ent->sdata[6],vcy);
	um_unitvc(&ent->sdata[9],vcz);
	um_vctovc(&ent->sdata[0],ptx[0]);
	um_translate_point(ptx[0],lnx,vcx,ptx[1]);
	um_translate_point(ptx[1],lny,vcy,ptx[2]);
	um_translate_point(ptx[0],lny,vcy,ptx[3]);
	um_translate_point(ptx[0],lnz,vcz,ptx[4]);
	um_translate_point(ptx[1],lnz,vcz,ptx[5]);
	um_translate_point(ptx[2],lnz,vcz,ptx[6]);
	um_translate_point(ptx[3],lnz,vcz,ptx[7]);

	switch (feature_order)
	{
/*
.....Output center, length, width, height
*/
	case 1:
		um_vcmnvc(ptx[6],ptx[0],vc1);
		um_translate_point(ptx[0],.5,vc1,cp);
		status = um_feacoord(cp);
		if (status != UU_SUCCESS) goto done;

		um_translate_point(ptx[7],lnx/2.,vcx,cp);
		um_translate_point(cp,2.0*char_height,vcy,pos);
		dflag = UU_TRUE;
		status = um_fealength(" Length = ",lnx,pos,cp,dflag);
		if (status!=0) goto done;

		um_translate_point(ptx[5],lny/2.,vcy,cp);
		um_translate_point(cp,2.0*char_height,vcx,pos);
		dflag = UU_TRUE;
		status = um_fealength(" Width = ",lny,pos,cp,dflag);
		if (status!=0) goto done;

		um_translate_point(ptx[2],lnz/2.,vcz,cp);
		um_translate_point(cp,2.0*char_height,vcy,pos);
		dflag = UU_TRUE;
		status = um_fealength(" Height = ",lnz,pos,cp,dflag);
		if (status!=0) goto done;
		break;
/*
.....Output lower left, upper right
*/
	case 2:
		status = um_feacoord(ptx[0]);
		if (status != UU_SUCCESS) goto done;

		status = um_feacoord(ptx[6]);
		if (status != UU_SUCCESS) goto done;
		break;
/*
.....Output other corner points
*/
	case 3:
		status = um_feacoord(ptx[1]);
		if (status != UU_SUCCESS) goto done;

		status = um_feacoord(ptx[2]);
		if (status != UU_SUCCESS) goto done;

		status = um_feacoord(ptx[3]);
		if (status != UU_SUCCESS) goto done;

		status = um_feacoord(ptx[4]);
		if (status != UU_SUCCESS) goto done;

		status = um_feacoord(ptx[5]);
		if (status != UU_SUCCESS) goto done;

		status = um_feacoord(ptx[7]);
		if (status != UU_SUCCESS) goto done;
		break;
/*
.....Undefined feature level
*/
	default:
		uu_uerror0(UU_FEATERROR,1);
		status = UU_FAILURE;
		break;
	}
/*
.....End of routine
*/
done:
	return(status);
}

/*********************************************************************
**    E_FUNCTION     :  S_cone_feature(ent,tfmat,feature_order,dploc)
**      calculate the desired features for a given order 
**       for the solid types Cone and Cylinder.
**    PARAMETERS   
**       INPUT  : 
**				ent				pointer to the geometric information
**				tfmat				transformation matrix.
**          feature_order	1 => center, normal, radius
**                       	2 => top center, top radius, height
**				dploc				picked location on circle
**       OUTPUT :  
**          none
**    RETURNS      : 
**			UU_SUCCESS if no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_cone_feature(ent,tfmat,feature_order,dploc)
struct UM_solid_rec *ent;
UM_transf tfmat;
int feature_order;
UD_PLOCREC *dploc;
{
	int status;
	UU_LOGICAL dflag;
	UU_REAL hgt,char_height,um_mag();
	UM_coord pos,cp,pt1;
	UM_vector vc1,nvec;

	UM_coord vporigin;
	UM_vector vpxaxis;
	UM_vector vpyaxis;
	UM_vector vpzaxis;
	UU_REAL aperture;
/*
.....Initialize routine
*/
	status = UU_SUCCESS;
/*
.....Get current view definition
*/
	um_get_current_viewplane(vporigin, vpxaxis, vpyaxis, vpzaxis, &aperture);
/*
.....Set feature character height
*/
	um_set_feature_char_height(&char_height);

	um_unitvc(&ent->sdata[3],nvec);
	um_perpvc(nvec,vc1);
	switch (feature_order)
	{
/*
.....Output center, normal, radius
*/
	case 1:
		status = um_feacoord(&ent->sdata[0]);
		if (status != UU_SUCCESS) goto done;

		status = um_feavect(&ent->sdata[0],nvec);

		um_translate_point(&ent->sdata[0],ent->sdata[6],vc1,cp);
		um_translate_point(cp,2.0*char_height,vc1,pos);
		dflag = UU_TRUE;
		status = um_fealength(" Radius = ",ent->sdata[6],pos,cp,dflag);
		if (status!=0) goto done;
		break;
/*
.....Output top center, top radius, height
*/
	case 2:
		um_translate_point(&ent->sdata[0],1.0,&ent->sdata[3],pt1);
		status = um_feacoord(pt1);
		if (status != UU_SUCCESS) goto done;

		if (ent->type == UM_CONE_SOLID)
			um_translate_point(pt1,ent->sdata[7],vc1,cp);
		else
			um_translate_point(pt1,ent->sdata[6],vc1,cp);
		um_translate_point(cp,2.0*char_height,vc1,pos);
		dflag = UU_TRUE;
		if (ent->type == UM_CONE_SOLID)
		{
			status = um_fealength(" Top Radius = ",ent->sdata[7],pos,cp,dflag);
			if (status!=0) goto done;
			dflag = UU_FALSE;
		}
		um_translate_point(pos,1.5*char_height,vpyaxis,pos);

		hgt = um_mag(&ent->sdata[3]);
		status = um_fealength(" Height = ",hgt,pos,cp,dflag);
		if (status!=0) goto done;
		break;

	default:
		uu_uerror0(UU_FEATERROR,1);
		status = UU_FAILURE;
		break;
	}
/*
.....End of routine
*/
done:
	return(status);
}

/*********************************************************************
**    E_FUNCTION     :  S_sphere_feature(ent,tfmat,feature_order,dploc)
**      calculate the desired features for a given order 
**       for the solid type Sphere.
**    PARAMETERS   
**       INPUT  : 
**				ent				pointer to the geometric information
**				tfmat				transformation matrix.
**          feature_order	1 => center, radius
**				dploc				picked location on circle
**       OUTPUT :  
**          none
**    RETURNS      : 
**			UU_SUCCESS if no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_sphere_feature(ent,tfmat,feature_order,dploc)
struct UM_solid_rec *ent;
UM_transf tfmat;
int feature_order;
UD_PLOCREC *dploc;
{
	int status;
	UU_LOGICAL dflag;
	UU_REAL char_height;
	UM_coord pos,cp;

	UM_coord vporigin;
	UM_vector vpxaxis;
	UM_vector vpyaxis;
	UM_vector vpzaxis;
	UU_REAL aperture;
/*
.....Initialize routine
*/
	status = UU_SUCCESS;
/*
.....Get current view definition
*/
	um_get_current_viewplane(vporigin, vpxaxis, vpyaxis, vpzaxis, &aperture);
/*
.....Set feature character height
*/
	um_set_feature_char_height(&char_height);

	switch (feature_order)
	{
/*
.....Output center, normal, radius
*/
	case 1:
		status = um_feacoord(&ent->sdata[0]);
		if (status != UU_SUCCESS) goto done;

		um_translate_point(&ent->sdata[0],ent->sdata[3],vpyaxis,cp);
		um_translate_point(cp,2.0*char_height,vpyaxis,pos);
		dflag = UU_TRUE;
		status = um_fealength(" Radius = ",ent->sdata[3],pos,cp,dflag);
		if (status!=0) goto done;
		break;

	default:
		uu_uerror0(UU_FEATERROR,1);
		status = UU_FAILURE;
		break;
	}
/*
.....End of routine
*/
done:
	return(status);
}

/*********************************************************************
**    E_FUNCTION     :  S_torus_feature(ent,tfmat,feature_order,dploc)
**      calculate the desired features for a given order 
**       for the solid type Torus.
**    PARAMETERS   
**       INPUT  : 
**				ent				pointer to the geometric information
**				tfmat				transformation matrix.
**          feature_order	1 => center, normal, axial and circular radii
**				dploc				picked location on circle
**       OUTPUT :  
**          none
**    RETURNS      : 
**			UU_SUCCESS if no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_torus_feature(ent,tfmat,feature_order,dploc)
struct UM_solid_rec *ent;
UM_transf tfmat;
int feature_order;
UD_PLOCREC *dploc;
{
	int status;
	UU_LOGICAL dflag;
	UU_REAL char_height;
	UM_coord pos,cp,pt1;
	UM_vector vc1,nvec;

	UM_coord vporigin;
	UM_vector vpxaxis;
	UM_vector vpyaxis;
	UM_vector vpzaxis;
	UU_REAL aperture;
/*
.....Initialize routine
*/
	status = UU_SUCCESS;
/*
.....Get current view definition
*/
	um_get_current_viewplane(vporigin, vpxaxis, vpyaxis, vpzaxis, &aperture);
/*
.....Set feature character height
*/
	um_set_feature_char_height(&char_height);

	switch (feature_order)
	{
/*
.....Output center, normal, radii
*/
	case 1:
		status = um_feacoord(&ent->sdata[0]);
		if (status != UU_SUCCESS) goto done;

		um_unitvc(&ent->sdata[3],nvec);
		status = um_feavect(&ent->sdata[0],nvec);

		um_perpvc(nvec,vc1);
		um_translate_point(&ent->sdata[0],ent->sdata[6],vc1,pt1);
		um_translate_point(pt1,ent->sdata[7],nvec,cp);
		um_translate_point(cp,2.0*char_height,nvec,pos);
		dflag = UU_TRUE;
		status = um_fealength(" Axial Radius = ",ent->sdata[6],pos,cp,dflag);
		if (status!=0) goto done;

		um_translate_point(pt1,ent->sdata[7],vc1,cp);
		um_translate_point(cp,2.0*char_height,vc1,pos);
		dflag = UU_TRUE;
		status = um_fealength(" Circular Radius = ",ent->sdata[7],pos,cp,dflag);
		if (status!=0) goto done;
		break;

	default:
		uu_uerror0(UU_FEATERROR,1);
		status = UU_FAILURE;
		break;
	}
/*
.....End of routine
*/
done:
	return(status);
}

/*********************************************************************
**    E_FUNCTION     :  S_extruded_feature(ent,tfmat,feature_order,dploc)
**      calculate the desired features for a given order 
**       for the solid type Extruded.
**    PARAMETERS   
**       INPUT  : 
**				ent				pointer to the geometric information
**				tfmat				transformation matrix.
**          feature_order	1 => 1st point of curve, normal, height
**				dploc				picked location on circle
**       OUTPUT :  
**          none
**    RETURNS      : 
**			UU_SUCCESS if no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_extruded_feature(ent,tfmat,feature_order,dploc)
struct UM_solid_rec *ent;
UM_transf tfmat;
int feature_order;
UD_PLOCREC *dploc;
{
	int status,inc;
	UU_LOGICAL dflag;
	UU_REAL hgt,char_height,um_mag();
	UM_coord pos,cp,*pts;
	UM_vector nvec;

	UM_coord vporigin;
	UM_vector vpxaxis;
	UM_vector vpyaxis;
	UM_vector vpzaxis;
	UU_REAL aperture;
/*
.....Initialize routine
*/
	status = UU_SUCCESS;
	if (ent->type == UM_EXTRUDED_SOLID)
	{
		inc = 4;
		um_vctovc(&ent->sdata[0],nvec);
	}
	else
	{
		inc = 6;
		um_vctovc(&ent->sdata[2],nvec);
	}
/*
.....Get current view definition
*/
	um_get_current_viewplane(vporigin, vpxaxis, vpyaxis, vpzaxis, &aperture);
/*
.....Set feature character height
*/
	um_set_feature_char_height(&char_height);

	switch (feature_order)
	{
/*
.....Output 1st point, normal, height
*/
	case 1:
		pts = (UM_coord *)&ent->sdata[inc];
		hgt = um_mag(nvec);
		um_unitvc(nvec,nvec);
		status = um_feacoord(pts[0]);
		if (status != UU_SUCCESS) goto done;

		status = um_feavect(pts[0],nvec);
		if (status != UU_SUCCESS) goto done;

		um_translate_point(pts[0],hgt,nvec,cp);
		um_translate_point(cp,char_height*2.0,nvec,pos);
		dflag = UU_TRUE;
		status = um_fealength(" Height = ",hgt,pos,cp,dflag);
		if (status != UU_SUCCESS) goto done;
		break;

	default:
		uu_uerror0(UU_FEATERROR,1);
		status = UU_FAILURE;
		break;
	}
/*
.....End of routine
*/
done:
	return(status);
}

/*********************************************************************
**    E_FUNCTION     :  S_revolved_feature(ent,tfmat,feature_order,dploc)
**      calculate the desired features for a given order 
**       for the solid type Revolved.
**    PARAMETERS   
**       INPUT  : 
**				ent				pointer to the geometric information
**				tfmat				transformation matrix.
**          feature_order	1 => center, normal, 1st point of curve
**                       	2 => start angle, end angle
**				dploc				picked location on circle
**       OUTPUT :  
**          none
**    RETURNS      : 
**			UU_SUCCESS if no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_revolved_feature(ent,tfmat,feature_order,dploc)
struct UM_solid_rec *ent;
UM_transf tfmat;
int feature_order;
UD_PLOCREC *dploc;
{
	int status;
	UU_LOGICAL dflag;
	UU_REAL char_height,sang,eang,dang;
	UM_coord pos,cp,pt0,*pts;
	UM_vector vc1,nvec,vx,vn;
	UM_transf tm;

	UM_coord vporigin;
	UM_vector vpxaxis;
	UM_vector vpyaxis;
	UM_vector vpzaxis;
	UU_REAL aperture;
/*
.....Initialize routine
*/
	status = UU_SUCCESS;
	sang = (ent->sdata[6] / UM_RADIAN);
	eang = (ent->sdata[7] / UM_RADIAN);
/*
.....Get current view definition
*/
	um_get_current_viewplane(vporigin, vpxaxis, vpyaxis, vpzaxis, &aperture);
/*
.....Set feature character height
*/
	um_set_feature_char_height(&char_height);

	pts = (UM_coord *)&ent->sdata[9];
	um_unitvc(&ent->sdata[3],nvec);
	switch (feature_order)
	{
/*
.....Output center, normal, 1st point
*/
	case 1:
		status = um_feacoord(&ent->sdata[0]);
		if (status != UU_SUCCESS) goto done;

		status = um_feavect(&ent->sdata[0],nvec);
		if (status != UU_SUCCESS) goto done;

		status = um_feacoord(pts[0]);
		if (status != UU_SUCCESS) goto done;
		break;
/*
.....Output starting & ending angles
*/
	case 2:
		pt0[0] = pt0[1] = pt0[2] = 0;
/*
........Calculate starting vector
*/
		um_vcmnvc(pts[0],&ent->sdata[0],vc1);
		if (um_mag(vc1) < UM_FUZZ) um_vcmnvc(&pts[1],&ent->sdata[0],vc1);
		um_cross(vc1,nvec,vn);
		if (um_mag(vn) < UM_FUZZ)
		{
			um_vcmnvc(&pts[1],&ent->sdata[0],vx);
			um_cross(vx,nvec,vn);
		}
		um_unitvc(vn,vn);
		um_cross(nvec,vn,vc1);
			
		um_rotatept(vc1,nvec,pt0,sang,UU_TRUE,tm);
		status = um_feavect(&ent->sdata[0],vc1);
		if (status != UU_SUCCESS) goto done;

		if (eang-sang+UM_FUZZ < UM_TWOPI)
		{
			um_vctovc(pts[0],cp);
			um_rotatept(cp,nvec,&ent->sdata[0],sang,UU_FALSE,tm);
			um_translate_point(pts[0],char_height*2.0,vpyaxis,pos);
			dflag = UU_FALSE;
			status = um_feaangle(" Start Angle = ",sang,pos,cp,dflag);
			if (status != UU_SUCCESS) goto done;

			dang = eang - sang;
			um_rotatept(vc1,nvec,pt0,dang,UU_TRUE,tm);
			status = um_feavect(&ent->sdata[0],vc1);
			if (status != UU_SUCCESS) goto done;

			um_rotatept(cp,nvec,&ent->sdata[0],dang,UU_FALSE,tm);
			um_translate_point(cp,char_height*2.0,vpyaxis,pos);
			status = um_feaangle(" End Angle = ",eang,pos,cp,dflag);
			if (status != UU_SUCCESS) goto done;
		}
		break;

	default:
		uu_uerror0(UU_FEATERROR,1);
		status = UU_FAILURE;
		break;
	}
/*
.....End of routine
*/
done:
	return(status);
}
