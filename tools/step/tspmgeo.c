/*********************************************************************
**    NAME         :  tspmgeo.c
**       CONTAINS:
**					utp_convert
**					utp_init_units
**					utp_map_bspline_curve
**					utp_map_bspline_surf
**					utp_map_circle
**					utp_map_cone
**					utp_map_cylinder
**					utp_map_ellipse
**					utp_map_extruded_surf
**					utp_map_plane
**					utp_map_point
**					utp_map_revolved_surf
**					utp_map_sphere
**					utp_map_torus
**					utp_map_transform
**					utp_map_units
**    COPYRIGHT 2013 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       tspmgeo.c , 25.3
**    DATE AND TIME OF LAST  MODIFICATION
**       04/05/18 , 15:04:55
*********************************************************************/

#include "tiges.h"
#include "tigdefs.h"
#include "tstep.h"
#include "mdrel.h"
#include "mcrv.h"
#include "msrf.h"
#include "modef.h"
#include "mdeval.h"
#include "nccs.h"
#include "ncl.h"
#include "udebug.h"

static UU_REAL Scnv=1.,Sangcnv=1.;

static void S_fix_curve();


extern int writeFile1( char* value);

/*********************************************************************
**    E_FUNCTION     :  utp_init_units()
**       Initializes the units to MM and Radians.
**    PARAMETERS   
**       INPUT  : 
**          val      Input value to convert.
**       OUTPUT : none
**    RETURNS      :
**          Converted value.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_init_units()
{
	Scnv=1.;
	Sangcnv=1.;
}

/*********************************************************************
**    E_FUNCTION     :  utp_convert(val)
**       Converts a number from the input units to inches.
**    PARAMETERS   
**       INPUT  : 
**          val      Input value to convert.
**       OUTPUT : none
**    RETURNS      :
**          Converted value.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_REAL utp_convert(val)
UU_REAL val;
{
	UU_REAL result = val * Scnv;
	return(result);
}

/*********************************************************************
**    E_FUNCTION     :  utp_convert_angle(val)
**       Converts an angle from the input units to degrees.
**    PARAMETERS   
**       INPUT  : 
**          val      Input value to convert.
**       OUTPUT : none
**    RETURNS      :
**          Converted value.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_REAL utp_convert_angle(val)
UU_REAL val;
{
	UU_REAL result = val * Sangcnv;
	return(result);
}

/*********************************************************************
**    E_FUNCTION     :  utp_get_units()
**       Returns the default INCH/MM setting.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT : none
**    RETURNS   :
**       1 if Inches, 2 if Metric.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int utp_get_units()
{
	if (Scnv == 1.) return(1);
	else return(2);
}
	
/*********************************************************************
**    E_FUNCTION     :  utp_map_bspline_curve(ptr,cv)
**				B_SPLINE_CURVE_WITH_KNOTS surface handling routine.
**    PARAMETERS   
**       INPUT  : 
**          ptr      Pointer to STEP record in global array.
**       OUTPUT : 
**          cv       Filled in curve structure.
**    RETURNS      :
**          UU_FAILURE if error, UU_SUCCESS otherwise.
**    SIDE EFFECTS :
**          Memory is allocated for the variable lists in the curve
**          structure and should be freed by the calling routine.
**    WARNINGS     : none
*********************************************************************/
int utp_map_bspline_curve(ptr,cv)
UTPs_step_record *ptr;
struct UM_rbsplcrv_rec *cv;
{
	int i,stat,kpt,npts,inc,kst;
	UU_KEY_ID key;
	UU_LOGICAL flags[3];
	UU_REAL degu;
	UM_coord *pts;
	UTPs_step_record *sptr;
/*
.....Initialize curve structure
*/
if (ptr->recno==1415)
{
	i = 0;
}
	ur_setup_data(UM_RBSPLCRV_REL,cv,sizeof(struct UM_rbsplsrf_rec));
	cv->key = 0;
/*
.....Check syntax of STEP record
*/
	if (ptr->parm[0].type == UTP_REAL) kst = 1;
	else kst = 2;
	if (ptr->parm[kst-1].type != UTP_REAL) goto synerr;
	for (i=kst;i<ptr->nparm;i++)
	{
		if (ptr->parm[i].type != UTP_RECNO) break;
	}
	npts = i - kst;
	stat = utp_get_logicals(&ptr->parm[i+1],flags,2);
	if (stat != UU_SUCCESS) goto synerr;
	kpt = i + 3;
/*
.....Initialize curve fixed data
*/
	cv->planar = UU_FALSE;
	cv->open = UU_FALSE;
	degu = ptr->parm[kst-1].ptype.value;
	cv->k = degu + 1;
	cv->closdinu = flags[0];
	cv->no_t = cv->no_pt = cv->no_wt = cv->no_displst = 0;
/*
.....Get Knots and Weights
*/
	stat = S_get_spline_knots(ptr,kpt,npts,cv);
	if (stat != UU_SUCCESS) goto synerr;
/*
.....Store the n,t0,t1 values
*/
	cv->n = cv->no_t - cv->k - degu;  /* npts-3 ? */
	cv->t0 = cv->t[0];
	cv->t1 = cv->t[cv->no_t-1];
/*
.....Create the point data
*/
	pts = (UM_coord *)uu_malloc(npts*sizeof(UM_coord));
	if (pts == UU_NULL) goto failed;
	inc = 0;
	for (i=0;i<npts;i++)
	{
		sptr = utp_get_record(ptr->parm[i+kst].ptype.recno);
		if (sptr == UU_NULL) goto failed;
		stat = utp_map_point(sptr,pts[inc++]);
		if (stat != UU_SUCCESS) goto done;
	}
	cv->no_pt = npts;
	cv->pt = (UU_REAL *)pts;
/*
.....Fix any irregularities in curve
*/
	S_fix_curve(cv);
/*
.....Apply STEP transformation
*/
	utp_transform(cv);
	goto done;
/*
.....Could not create curve
*/
failed:;
	stat = UU_FAILURE;
	utp_curve_error(ptr);
	goto done;
/*
.....STEP file syntax error
*/
synerr:;
	stat = UU_FAILURE;
	utp_syntax_error(ptr);
	goto done;
/*
....End of routine
*/
done:;
	return(stat);
}

/*********************************************************************
**    E_FUNCTION     :  utp_map_bspline_surf(ptr,sf)
**				B_SPLINE_SURFACE_WITH_KNOTS surface handling routine.
**    PARAMETERS   
**       INPUT  : 
**          ptr      Pointer to STEP record in global array.
**       OUTPUT : 
**          sf       Filled in surface structure.
**    RETURNS      :
**          UU_FAILURE if error, UU_SUCCESS otherwise.
**    SIDE EFFECTS :
**          Memory is allocated for the variable lists in the surface
**          structure and should be freed by the calling routine.
**    WARNINGS     : none
*********************************************************************/
int utp_map_bspline_surf(ptr,sf)
UTPs_step_record *ptr;
struct UM_rbsplsrf_rec *sf;
{
	int i,j,stat,kpt,npts,inc,nupts,nvpts,kst,ipt;
	UU_KEY_ID key;
	UU_LOGICAL flags[3];
	UU_REAL degu,degv,prm[16],rnum;
	UM_coord *pts;
	UTPs_step_record *sptr;
/*
.....Initialize surface structure
*/
	ur_setup_data(UM_RBSPLSRF_REL,sf,sizeof(struct UM_rbsplsrf_rec));
	sf->key = 0;
if (ptr->recno == 26239 || ptr->recno == 26441)
{
	i = 0;
}
/*
.....Check syntax of STEP record
*/
	if (ptr->parm[0].type == UTP_REAL) kst = 2;
	else if (ptr->parm[1].type == UTP_REAL) kst = 3;
	else goto synerr;
	if (ptr->parm[kst-1].type != UTP_REAL) goto synerr;
	for (i=kst;i<ptr->nparm;i++)
	{
		if (ptr->parm[i].type != UTP_RECNO) break;
	}
	npts = i - kst;
	stat = utp_get_logicals(&ptr->parm[i+1],flags,3);
	if (stat != UU_SUCCESS) goto synerr;
	kpt = i + 4;
/*
.....Initialize surface fixed data
*/
	sf->rldnu = -1;
	sf->swapuv = 0;
	sf->rev_normal = UU_FALSE;
	degu = ptr->parm[kst-2].ptype.value;
	degv = ptr->parm[kst-1].ptype.value;
	sf->ku = degu + 1;
	sf->kv = degv + 1;
	sf->closdinu = flags[0];
	sf->closdinv = flags[1];
	sf->offdist = 0.;
	sf->no_tu = sf->no_tv = sf->no_pt = sf->no_wt = sf->no_sskey = 0;
	sf->no_displst = sf->no_tesslst = sf->no_boxlst = sf->no_xyzbylst = 0;
/*
.....Get knots and Weights
*/
	stat = S_get_surf_knots(ptr,kpt,npts,sf);
	if (stat != UU_SUCCESS) goto synerr;
/*
.....Create the point data
*/
	nupts = degu + sf->nu;
	nvpts = degv + sf->nv;
	if (nupts*nvpts != npts) goto failed;
	pts = (UM_coord *)uu_malloc(npts*sizeof(UM_coord));
	if (pts == UU_NULL) goto failed;
	inc = 0;
/*
........Points are stored in V and then U
*/
	for (i=0;i<nvpts;i++)
	{
		for (j=0;j<nupts;j++)
		{
			ipt = i + j*nvpts;
			sptr = utp_get_record(ptr->parm[ipt+kst].ptype.recno);
			if (sptr == UU_NULL) goto failed;
			stat = utp_map_point(sptr,pts[inc]);
			if (stat != UU_SUCCESS) goto done;
			inc++;
		}
	}
	sf->no_pt = npts;
	sf->pt = (UU_REAL *)pts;
/*
.....Apply STEP transformation
*/
	utp_transform(sf);
	goto done;
/*
.....Could not create surface
*/
failed:;
	stat = UU_FAILURE;
	utp_surface_error(ptr);
	goto done;
/*
.....STEP file syntax error
*/
synerr:;
	stat = UU_FAILURE;
	utp_syntax_error(ptr);
	goto done;
/*
....End of routine
*/
done:;
	return(stat);
}

/*********************************************************************
**    E_FUNCTION     :  utp_map_circle(ptr,spt,ept,crec)
**				Returns a circle record containing the circle defined in
**          the STEP file using a CIRCLE record.
**    PARAMETERS   
**       INPUT  : 
**          ptr      Pointer to STEP record in global array.
**          spt      Starting point of circle.
**          ept      Ending point of circle.
**          revfl    Circle orientation flag
**                   UU_TRUE:  Counterclockwise
**                   UU_FALSE: Clockwise
**       OUTPUT : 
**          crec     NCL circle structure to receive circle.
**    RETURNS      :
**          0 on succcess, -1 if could not create circle, -2 if small
**          circle should be converted to a line.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int utp_map_circle(ptr,spt,ept,revfl,crec)
UTPs_step_record *ptr;
UM_coord spt,ept;
struct UM_circle_rec *crec;
UU_LOGICAL revfl;
{
	int i,status;
	UU_REAL rad,dot,tol;
	UTPs_step_record *sptr,*tptr;
	UM_transf tf,tfi;
	UM_coord pt1,pt2,cpt;
	UM_vector v0,v1,tnvec;
	gettol(&tol);
/*
.....Initialize circle record
*/
	ur_setup_data(UM_CIRCLE_REL,crec,sizeof(struct UM_circle_rec));
/*
.....Get radius
*/
	if (ptr->parm[2].type != UTP_REAL) goto failed;
	rad = utp_convert(ptr->parm[2].ptype.value);
	if (rad < tol) goto raderr;
/*
.....Get transformation matrix
*/
	sptr = utp_get_record(ptr->parm[1].ptype.recno);
	if (sptr == UU_NULL) goto failed;
	status = utp_map_transform(sptr,tf);
	if (status != UU_SUCCESS) goto done;
/*
.....Get circle paramters
*/
	um_vctovc(tf[3],crec->center);
	um_vctovc(tf[2],crec->nvec);
	um_unitvc(crec->nvec,crec->nvec);
	if (um_cceqcc(spt,crec->center) || um_cceqcc(ept,crec->center)) 
		goto failed;
/*
.....Set remaining circle parameters
*/
	um_vcmnvc(spt, crec->center, v0);
	um_unitvc(v0, crec->svec);
	um_vcmnvc(ept, crec->center, v1);
	um_unitvc(v1,v1);
	crec->radius = um_dcccc(spt, crec->center);
	if (!revfl)
		um_negvc(crec->nvec,crec->nvec);
	crec->dang = um_angle2p(crec->svec,v1,crec->nvec);
	if (crec->dang <= 10.*UM_DFUZZ) crec->dang = UM_TWOPI;
	if (crec->radius * fabs(crec->dang) < UM_DFUZZ) goto failed;
/*
.....Apply STEP transformation
*/
	utp_transform(crec);
	goto done;
/*
.....Radius error
*/
raderr:
//	utp_radius_error(ptr);
	status = -2;
	goto done;
/*
.....Syntax error
*/
failed:;
	status = UU_FAILURE;
	utp_circle_error(ptr);
/*
.....End of routine
*/
done:;
	return(status);
}

/*********************************************************************
**    E_FUNCTION     :  utp_map_cone(ptr,cyl)
**				Returns the canonical data of a cone defined in the STEP
**          file using a CONICAL_SURFACE record.
**    PARAMETERS   
**       INPUT  : 
**          ptr      Pointer to STEP record in global array.
**       OUTPUT : 
**          cyl      Canonical data of cone.  The conical height
**                   will not be calculated due to insufficient data.
**    RETURNS      :
**          UU_FAILURE if error, UU_SUCCESS otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int utp_map_cone(ptr,cyl)
UTPs_step_record *ptr;
UU_REAL cyl[];
{
	int i,status;
	UU_REAL tol;
	UTPs_step_record *sptr;
	UM_transf tf;
if (ptr->recno == 96682)
{
	i=0;
}
	gettol(&tol);
/*
.....Get cone radius
*/
	if (ptr->parm[2].type != UTP_REAL) goto failed;
	cyl[6] = utp_convert(ptr->parm[2].ptype.value);
	if (cyl[6] < 0.) goto raderr;
/*
.....Get cone angle
*/
	if (ptr->parm[3].type != UTP_REAL) goto failed;
	cyl[8] = utp_convert_angle(ptr->parm[3].ptype.value);
/*
.....Get cone center and axis
*/
	if (ptr->parm[1].type != UTP_RECNO) goto failed;
	sptr = utp_get_record(ptr->parm[1].ptype.recno);
	if (sptr == UU_NULL) goto failed;
	
	status = utp_map_transform(sptr,tf);
	if (status != UU_SUCCESS) goto done;

	um_vctovc(tf[3],&cyl[0]);
	um_vctovc(tf[2],&cyl[3]);
	cyl[7] = 1.;
/*
.....Apply STEP transformation
*/
	utp_transform_cyl(cyl);
	status = UU_SUCCESS;
	goto done;
/*
.....Radius error
*/
raderr:
	utp_radius_error(ptr);
	status = UU_FAILURE;
	goto done;
/*
.....Syntax error
*/
failed:;
	status = UU_FAILURE;
	utp_syntax_error(ptr);
/*
.....End of routine
*/
done:;
	return(status);
}

/*********************************************************************
**    E_FUNCTION     :  utp_map_cylinder(ptr,cyl)
**				Returns the canonical data of a cylinder defined in the STEP
**          file using a CYLINDRICAL_SURFACE record.
**    PARAMETERS   
**       INPUT  : 
**          ptr      Pointer to STEP record in global array.
**       OUTPUT : 
**          cyl      Canonical data of cylinder.  The cylinder height
**                   will not be calculated due to insufficient data.
**    RETURNS      :
**          UU_FAILURE if error, UU_SUCCESS otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int utp_map_cylinder(ptr,cyl)
UTPs_step_record *ptr;
UU_REAL cyl[];
{
	int i,status;
	UU_REAL tol;
	UTPs_step_record *sptr;
	UM_transf tf;
if (ptr->recno == 100216)
{
	i=0;
}
	gettol(&tol);
/*
.....Get cylinder radius
*/
	if (ptr->parm[2].type != UTP_REAL) goto failed;
	cyl[6] = utp_convert(ptr->parm[2].ptype.value);
	if (cyl[6] < tol) goto raderr;
/*
.....Get cylinder center and axis
*/
	if (ptr->parm[1].type != UTP_RECNO) goto failed;
	sptr = utp_get_record(ptr->parm[1].ptype.recno);
	if (sptr == UU_NULL) goto failed;
	
	status = utp_map_transform(sptr,tf);
	if (status != UU_SUCCESS) goto done;

	um_vctovc(tf[3],&cyl[0]);
	um_vctovc(tf[2],&cyl[3]);
	cyl[7] = 1.;
	cyl[8] = 0.;
/*
.....Apply STEP transformation
*/
	utp_transform_cyl(cyl);
	status = UU_SUCCESS;
	goto done;
/*
.....Radius error
*/
raderr:
	utp_radius_error(ptr);
	status = UU_FAILURE;
	goto done;
/*
.....Syntax error
*/
failed:;
	status = UU_FAILURE;
	utp_syntax_error(ptr);
/*
.....End of routine
*/
done:;
	return(status);
}

/*********************************************************************
**    E_FUNCTION     :  utp_map_ellipse(ptr,spt,ept,crec)
**				Returns a conic record containing the ellipse defined in
**          the STEP file using a ELLIPSE record.
**    PARAMETERS   
**       INPUT  : 
**          ptr      Pointer to STEP record in global array.
**          spt      Starting point of ellipse.
**          ept      Ending point of ellipse.
**          revfl    Circle orientation flag
**                   UU_TRUE:  Counterclockwise
**                   UU_FALSE: Clockwise
**       OUTPUT : 
**          crec     NCL ellipse structure to receive ellipse.
**    RETURNS      :
**          UU_FAILURE if error, UU_SUCCESS otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int utp_map_ellipse(ptr,spt,ept,revfl,crec)
UTPs_step_record *ptr;
UM_coord spt,ept;
struct UM_conic_rec *crec;
UU_LOGICAL revfl;
{
	int i,status;
	UU_REAL rad1,rad2,tol;
	UTPs_step_record *sptr,*tptr;
	UM_transf tf,tfi;
	UM_coord pt1,pt2,cpt;
	UM_vector v0,v1;
	gettol(&tol);
/*
.....Initialize conic record
*/
	ur_setup_data(UM_CONIC_REL,crec,sizeof(struct UM_conic_rec));
/*
.....Get major and minor radii
*/
	if (ptr->parm[2].type != UTP_REAL || ptr->parm[2].type != UTP_REAL)
		goto failed;
	rad1 = utp_convert(ptr->parm[2].ptype.value);
	rad2 = utp_convert(ptr->parm[3].ptype.value);
	if (rad1 < tol || rad2 < tol) goto raderr;
/*
.....Get transformation matrix
*/
	sptr = utp_get_record(ptr->parm[1].ptype.recno);
	if (sptr == UU_NULL) goto failed;
	status = utp_map_transform(sptr,tf);
	if (status != UU_SUCCESS) goto done;
	um_inverttf(tf,tfi);
/*
.....Transform end points points
*/
	crec->rel_num = UM_CONIC_REL;
	um_cctmtf(spt,tfi,pt1);
	um_cctmtf(ept,tfi,pt2);
	um_nullvc(cpt);
	if (um_cceqcc(cpt,pt1) || um_cceqcc(cpt,pt2)) goto failed;
/*
.....Define ellipse
*/
	if (rad1 > rad2)
	{
		crec->invariants[0] = rad1;
		crec->invariants[1] = rad2;
	}
	else
	{
		crec->invariants[0] = rad2;
		crec->invariants[1] = rad1;
	}
/*
.....Ellipse is built in XY-plane
.....and transformed later
*/
	um_identtf(crec->tfmat);
/*
.....Store starting and ending points for ellipse
*/
	crec->type = UM_ELLIPSE;
	crec->t0 = -2;
	crec->t1 = 2;
	if (revfl)
		um_cn4_endpoints(crec,pt1,pt2,UM_idmat);
	else
		um_cn4_endpoints(crec,pt2,pt1,UM_idmat);
/*
.....Restore transformation matrix
*/
	um_tftotf(tf,crec->tfmat);
/*
.....Apply STEP transformation
*/
	utp_transform(crec);
	goto done;
/*
.....Radius error
*/
raderr:
	utp_radius_error(ptr);
	status = UU_FAILURE;
	goto done;
/*
.....Syntax error
*/
failed:;
	status = UU_FAILURE;
	utp_ellipse_error(ptr);
/*
.....End of routine
*/
done:;
	return(status);
}

/*********************************************************************
**    E_FUNCTION     :  utp_map_extruded_surf(ptr,sf)
**				SURFACE_OF_LINEAR_EXTRUSION surface handling routine.
**    PARAMETERS   
**       INPUT  : 
**          ptr      Pointer to STEP record in global array.
**          cvkey    Key of boundary curve.
**       OUTPUT : 
**          sf       Filled in surface structure.
**    RETURNS      :
**          UU_FAILURE if error, UU_SUCCESS otherwise.
**    SIDE EFFECTS :
**          Memory is allocated for the variable lists in the surface
**          structure and should be freed by the calling routine.
**    WARNINGS     : none
*********************************************************************/
int utp_map_extruded_surf(ptr,sf,cvkey)
UTPs_step_record *ptr;
struct UM_rbsplsrf_rec *sf;
UU_KEY_ID cvkey;
{
	int i,stat,relnum;
	UU_LOGICAL first;
	UU_REAL rnum,sgn,mindis,maxdis,mdis,xdis,dis,tol,npts;
	UU_KEY_ID key;
	UM_coord *pts,cpt;
	UM_vector vec,rvec;
	UM_transf tfmat;
	UU_LIST ptlist;
	UTPs_step_record *tptr;
	struct NCL_fixed_databag cv;
	struct UM_crvdatabag crv;
/*
.....Get the extrusion vector
*/
	if (ptr->parm[1].type != UTP_RECNO || ptr->parm[2].type != UTP_RECNO)
		goto synerr;
	tptr = utp_get_record(ptr->parm[2].ptype.recno);
	if (tptr == UU_NULL || tptr->command != VECTOR ||
		tptr->parm[1].type != UTP_RECNO || tptr->parm[2].type != UTP_REAL)
			goto synerr;
	rnum = tptr->parm[2].ptype.value;
	tptr = utp_get_record(tptr->parm[1].ptype.recno);
	if (tptr == UU_NULL || tptr->command != DIRECTION) goto synerr;
	for (i=0;i<3;i++)
	{
		if (tptr->parm[i+1].type != UTP_REAL) goto synerr;
		vec[i] = tptr->parm[i+1].ptype.value; // * rnum;
	}
	um_unitvc(vec,vec);
	utp_transform_vector(vec);
/*
.....Define the extruded curve
*/
	tptr = utp_get_record(ptr->parm[1].ptype.recno);
	if (tptr == UU_NULL) goto failed;
	key = utp_in_curve(tptr,&relnum);
	if (key == 0) goto failed;
/*
.....Get extrusion curve
*/
   crv.key = key;
   ncl_retrieve_data_fixed(&crv);
   uc_retrieve_transf (crv.key,tfmat);
/*
.....Evolve points on the extrusion curve
*/
   gettol(&tol);
   uu_list_init(&ptlist,sizeof(UM_coord),200,200);
   npts = ncl_evolve_curve(&crv,tfmat,tol,&ptlist,UU_NULL,UU_NULL,0);
/*
.....Determine the base of the extrusion
*/
	pts = (UM_coord *)UU_LIST_ARRAY(&ptlist);
	um_vctovc(pts[0],cpt);
	mindis = maxdis = 0.;
	for (i=1;i<npts;i++)
	{
		um_vcmnvc(pts[i],cpt,rvec);
		dis = um_dot(rvec,vec);
		if (dis < mindis) mindis = dis;
		if (dis > maxdis) maxdis = dis;
	}
	uu_list_free(&ptlist);
/*
....Get boundary curve
*/
   crv.key = cvkey;
   ncl_retrieve_data_fixed(&crv);
   uc_retrieve_transf (crv.key,tfmat);
/*
.....Evolve points on the curve
*/
   gettol(&tol);
   uu_list_init(&ptlist,sizeof(UM_coord),200,200);
   npts = ncl_evolve_curve(&crv,tfmat,tol,&ptlist,UU_NULL,UU_NULL,0);
   if (npts < 2)
	{
		uu_list_free(&ptlist);
		goto failed;
	}
/*
.....Determine the direction and length
.....of the extrusion
.....based on the boundary curve
*/
	pts = (UM_coord *)UU_LIST_ARRAY(&ptlist);
	first = UU_TRUE;
	mdis = xdis = 0.;
	for (i=0;i<npts;i++)
	{
		um_vcmnvc(pts[i],cpt,rvec);
		dis = um_dot(rvec,vec);
		if (dis < mdis || first) mdis = dis;
		if (dis > xdis || first) xdis = dis;
		first = UU_FALSE;
	}
	uu_list_free(&ptlist);
	if (fabs(xdis) > fabs(mdis))
	{
		sgn = fabs(xdis) / xdis;
		dis = fabs(xdis-mindis);
	}
	else 
	{
		sgn = fabs(mdis) / mdis;
		dis = fabs(maxdis-mdis);
	}
	um_vctmsc(vec,dis*sgn,vec);
/*
.....Define the extruded surface
*/
	cv.key = key;
	ncl_retrieve_data_fixed(&cv);
	stat = ncl_tabcyl(&cv,vec,sf);
	if (stat != UU_SUCCESS) goto failed;
	goto done;
/*
.....Could not create surface
*/
failed:;
	stat = UU_FAILURE;
	utp_surface_error(ptr);
	goto done;
/*
.....STEP file syntax error
*/
synerr:;
	stat = UU_FAILURE;
	utp_syntax_error(ptr);
	goto done;
/*
....End of routine
*/
done:;
	return(stat);
}

/*********************************************************************
**    E_FUNCTION     :  utp_map_plane(ptr,pl)
**				Returns the canonical data of a plane defined in the STEP
**          file using a PLANE record.
**    PARAMETERS   
**       INPUT  : 
**          ptr      Pointer to STEP record in global array.
**       OUTPUT : 
**          pl       NCL plane structure to receive plane.
**    RETURNS      :
**          UU_FAILURE if error, UU_SUCCESS otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int utp_map_plane(ptr,pl)
UTPs_step_record *ptr;
struct NCL_nclpl_rec *pl;
{
	int i,status;
	UTPs_step_record *sptr;
	UM_transf tf;
/*
.....Get plane record
*/
	sptr = utp_get_record(ptr->parm[1].ptype.recno);
	if (sptr == UU_NULL) goto failed;
	status = utp_map_transform(sptr,tf);
	if (status != UU_SUCCESS) goto done;
/*
.....Store plane
*/
	um_vctovc(tf[3],pl->pt);
	um_vctovc(tf[2],pl->nvec);
/*
.....Apply STEP transformation
*/
	utp_transform_plane(pl);
	status = UU_SUCCESS;
	goto done;
/*
.....Syntax error
*/
failed:;
	status = UU_FAILURE;
	utp_syntax_error(ptr);
/*
.....End of routine
*/
done:;
	return(status);
}

/*********************************************************************
**    E_FUNCTION     :  utp_map_point(ptr,pt)
**				Returns the coordinates of a point defined in the STEP
**          file using a VERTEX_POINT or CARTESIAN_POINT record.
**    PARAMETERS   
**       INPUT  : 
**          ptr      Pointer to STEP record in global array.
**       OUTPUT : 
**          cpt      Point coordinates.
**    RETURNS      :
**          UU_FAILURE if error, UU_SUCCESS otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int utp_map_point(ptr,cpt)
UTPs_step_record *ptr;
UM_coord cpt;
{
	int i,status;
	UTPs_step_record *tptr;
/*
.....Get point coordinates
*/
	tptr = ptr;
	if (tptr->command == VERTEX_POINT)
		tptr = utp_get_record(ptr->parm[1].ptype.recno);
	if (tptr == UU_NULL) return(UU_FAILURE);
	if (tptr->command != CARTESIAN_POINT) goto failed;
	for (i=0;i<3;i++)
	{
		if (tptr->parm[i+1].type != UTP_REAL) goto failed;
		cpt[i] = utp_convert(tptr->parm[i+1].ptype.value);
	}
	status = UU_SUCCESS;
	goto done;
/*
.....Syntax error
*/
failed:;
	status = UU_FAILURE;
	utp_syntax_error(tptr);
/*
.....End of routine
*/
done:;
	return(status);
}

/*********************************************************************
**    E_FUNCTION     :  utp_map_revolved_surf(ptr,cpt,nvec,svec,key)
**				Returns the canonical data of a surface of revolution
**          defined in the STEP file using a SURFACE_OF_REVOLUTION record.
**    PARAMETERS   
**       INPUT  : 
**          ptr      Pointer to STEP record in global array.
**       OUTPUT : 
**          cpt      Center of revolved surface.
**          nvec     Axis of revolved surface.
**          svec     Starting vector of surface based on revolved curve.
**          key      Key of revolved curve.
**    RETURNS      :
**          UU_FAILURE if error, UU_SUCCESS otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int utp_map_revolved_surf(ptr,cpt,nvec,svec,key)
UTPs_step_record *ptr;
UM_coord cpt;
UM_vector nvec,svec;
UU_KEY_ID *key;
{
	int i,status,relnum;
	UU_REAL u;
	UM_coord ptx;
	UM_transf tf;
	UTPs_step_record *sptr,*tptr;
	struct UM_crvdatabag crv;
	struct UM_evcrvout evout;
/*
.....Get revolved center and axis
*/
	if (ptr->parm[2].type != UTP_RECNO) goto failed;
	sptr = utp_get_record(ptr->parm[2].ptype.recno);
	if (sptr == UU_NULL) goto failed;
	
	status = utp_map_transform(sptr,tf);
	if (status != UU_SUCCESS) goto done;

	um_vctovc(tf[3],cpt);
	um_vctovc(tf[2],nvec);
/*
.....Get revolved curve
*/
	if (ptr->parm[2].type != UTP_RECNO) goto failed;
   tptr = utp_get_record(ptr->parm[1].ptype.recno);
   if (tptr == UU_NULL) goto failed;
   *key = utp_in_curve(tptr,&relnum);
   if (*key == 0) goto failed;
/*
.....Apply STEP transformation to axis point
.....Since revolved curve is already transformed
.....Bobby - 9/25/15
*/
	utp_transform_point(cpt);
	utp_transform_vector(nvec);
/*
.....Calculate starting vector of rotation
*/
	crv.key = *key;
	ncl_retrieve_data_fixed(&crv);
	uc_retrieve_transf(crv.key,tf);
	uc_init_evcrvout(&crv,&evout);
	u = 0.; uc_evcrv(UM_POINT,u,&crv,tf,&evout);
	um_nptpln(evout.cp,cpt,nvec,ptx);
	um_vcmnvc(ptx,cpt,svec); um_unitvc(svec,svec);
	if (um_mag(svec) < .1)
	{
		u = 0.01; uc_evcrv(UM_POINT,u,&crv,tf,&evout);
		um_nptpln(evout.cp,cpt,nvec,ptx);
		um_vcmnvc(ptx,cpt,svec); um_unitvc(svec,svec);
	}
	status = UU_SUCCESS;
	goto done;
/*
.....Syntax error
*/
failed:;
	status = UU_FAILURE;
	utp_syntax_error(ptr);
/*
.....End of routine
*/
done:;
	return(status);
}

/*********************************************************************
**    E_FUNCTION     :  utp_map_sphere(ptr,cpt,rad)
**				Returns the canonical data of a sphere defined in the STEP
**          file using a SPHERICAL_SURFACE record.
**    PARAMETERS   
**       INPUT  : 
**          ptr      Pointer to STEP record in global array.
**       OUTPUT : 
**          cpt      Center of sphere.
**          nvec     Normal of plane to construct sphere's circle on.
**          svec     Vector of starting point of sphere's circle.
**          rad      Radius of sphere.
**    RETURNS      :
**          UU_FAILURE if error, UU_SUCCESS otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int utp_map_sphere(ptr,cpt,nvec,svec,rad)
UTPs_step_record *ptr;
UM_coord cpt;
UM_vector nvec,svec;
UU_REAL *rad;
{
	int i,status;
	UTPs_step_record *sptr;
	UM_transf tf;
	UU_REAL tol;
	gettol(&tol);
/*
.....Get sphere radius
*/
	if (ptr->parm[2].type != UTP_REAL) goto failed;
	*rad = utp_convert(ptr->parm[2].ptype.value);
	if (*rad < tol) goto raderr;
/*
.....Get sphere center
*/
	if (ptr->parm[1].type != UTP_RECNO) goto failed;
	sptr = utp_get_record(ptr->parm[1].ptype.recno);
	if (sptr == UU_NULL) goto failed;
	
	status = utp_map_transform(sptr,tf);
	if (status != UU_SUCCESS) goto done;

	um_vctovc(tf[3],cpt);
	um_vctovc(tf[2],nvec);
	um_vctovc(tf[0],svec);
/*
.....Apply STEP transformation
*/
	utp_transform_sphere(cpt,nvec,svec);
	status = UU_SUCCESS;
	goto done;
/*
.....Radius error
*/
raderr:
	utp_radius_error(ptr);
	status = UU_FAILURE;
	goto done;
/*
.....Syntax error
*/
failed:;
	status = UU_FAILURE;
	utp_syntax_error(ptr);
/*
.....End of routine
*/
done:;
	return(status);
}

/*********************************************************************
**    E_FUNCTION     :  utp_map_torus(ptr,cpt,nvec,svec,rad1,rad2)
**				Returns the canonical data of a torus defined in the STEP
**          file using a TOROIDAL_SURFACE record.
**    PARAMETERS   
**       INPUT  : 
**          ptr      Pointer to STEP record in global array.
**       OUTPUT : 
**          cpt      Center of torus.
**          nvec     Axis of revolution of torus.
**          svec     Vector of starting point of torus' circle.
**          rad1     Major radius of torus.
**          rad2     Minor radius of torus.
**    RETURNS      :
**          UU_FAILURE if error, UU_SUCCESS otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int utp_map_torus(ptr,cpt,nvec,svec,rad1,rad2)
UTPs_step_record *ptr;
UM_coord cpt;
UM_vector nvec,svec;
UU_REAL *rad1,*rad2;
{
	int i,status;
	UTPs_step_record *sptr;
	UM_transf tf;
	UU_REAL tol;
	gettol(&tol);
/*
.....Get torus radii
*/
	if (ptr->parm[2].type != UTP_REAL) goto failed;
	*rad1 = utp_convert(ptr->parm[2].ptype.value);
	if (*rad1 < tol) goto raderr;
	if (ptr->parm[3].type != UTP_REAL) goto failed;
	*rad2 = utp_convert(ptr->parm[3].ptype.value);
	if (*rad2 < tol) goto raderr;
	if (fabs((*rad1)-(*rad2)) < UM_FUZZ) *rad2 = *rad2 - .0005;
/*
.....Get torus center
*/
	if (ptr->parm[1].type != UTP_RECNO) goto failed;
	sptr = utp_get_record(ptr->parm[1].ptype.recno);
	if (sptr == UU_NULL) goto failed;
	
	status = utp_map_transform(sptr,tf);
	if (status != UU_SUCCESS) goto done;

	um_vctovc(tf[3],cpt);
	um_vctovc(tf[2],nvec);
	um_vctovc(tf[0],svec);
/*
.....Apply STEP transformation
*/
	utp_transform_sphere(cpt,nvec,svec);
	status = UU_SUCCESS;
	goto done;
/*
.....Radius error
*/
raderr:
	utp_radius_error(ptr);
	status = UU_FAILURE;
	goto done;
/*
.....Syntax error
*/
failed:;
	status = UU_FAILURE;
	utp_syntax_error(ptr);
/*
.....End of routine
*/
done:;
	return(status);
}

/*********************************************************************
**    E_FUNCTION     :  utp_map_transform(ptr,tf)
**				Returns a transformation matrix defined in the STEP
**          file using an AXIS1_PLACEMENT or AXIS2_PLACEMENT_3D command.
**    PARAMETERS   
**       INPUT  : 
**          ptr      Pointer to STEP record in global array.
**       OUTPUT : 
**          tf       Transformation matrix.
**    RETURNS      :
**          UU_FAILURE if error, UU_SUCCESS otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int utp_map_transform(ptr,tf)
UTPs_step_record *ptr;
UM_transf tf;
{
	int i,j,status;
	UM_coord cpt;
	UM_vector vec[2];
	UTPs_step_record *sptr,*tptr;
/*
.....Process AXIS2_PLACEMENT_3D command
.....Point/Vector/Vector transformation
*/
	switch (ptr->command)
	{
	case AXIS1_PLACEMENT:
	case AXIS2_PLACEMENT_3D:
/*
........Get point on plane
*/
		if (ptr->parm[1].type != UTP_RECNO) goto failed;
		tptr = utp_get_record(ptr->parm[1].ptype.recno);
		if (tptr == UU_NULL) return(UU_FAILURE);
		if (tptr->command != CARTESIAN_POINT) goto failed;
		for (i=0;i<3;i++)
		{
			if (tptr->parm[i+1].type != UTP_REAL) goto failed;
			cpt[i] = utp_convert(tptr->parm[i+1].ptype.value);
		}
/*
........Get plane normal & X-axis vector
*/
		for (j=0;j<2;j++)
		{
			if (ptr->parm[j+2].type != UTP_RECNO)
			{
				if (j == 0) goto failed;
				um_perpvc(vec[0],vec[1]);
			}
			else
			{
				tptr = utp_get_record(ptr->parm[j+2].ptype.recno);
				if (tptr == UU_NULL) return(UU_FAILURE);
				if (tptr->command != DIRECTION) goto failed;
				for (i=0;i<3;i++)
				{
					if (tptr->parm[i+1].type != UTP_REAL) goto failed;
					vec[j][i] = tptr->parm[i+1].ptype.value;
				}
			}
			um_unitvc(vec[j],vec[j]);
		}
/*
........Calculate transformation
*/
		um_ptzx_tf(cpt,vec[0],vec[1],tf);
		status = UU_SUCCESS;
		break;
/*
.....Unrecognized transformation
*/
	default:
		goto failed;
	}
	goto done;
/*
.....Could not create transformation
*/
failed:;
	status = UU_FAILURE;
	utp_xform_error(ptr);
/*
.....End of routine
*/
done:;
	return(status);
}

/*********************************************************************
**    E_FUNCTION     :  utp_map_units(ptr)
**       Determines the Units specified in the STEP file and sets the
**       active Units.
**    PARAMETERS   
**       INPUT  : 
**          ptr      Pointer to STEP record in global array.
**       OUTPUT : none
**    RETURNS      :
**          UU_FAILURE if error, UU_SUCCESS otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int utp_map_units(ptr)
UTPs_step_record *ptr;
{
	int i,j,status;
	UU_REAL cnv;
	UTPs_step_record *tptr;

	char unitname[50];	// The four variables are to properly process unit strings
	char* store;		// Sasha, June 07, 2017
	char* tstore;
	int k;
/*
.....GEOMETRIC_REPRESENTATION_CONTEXT
*/
	switch (ptr->command)
	{
	case GEOMETRIC_REPRESENTATION_CONTEXT:
		for (i=2;i<ptr->nparm;i++)
		{
			if (ptr->parm[i].type != UTP_RECNO) continue;
			tptr = utp_get_record(ptr->parm[i].ptype.recno);
			if (tptr == UU_NULL) return(UU_FAILURE);
			if (tptr->command == UNCERTAINTY_MEASURE_WITH_UNIT)
			{
				if (tptr->parm[2].type != UTP_RECNO) goto failed;
				tptr = utp_get_record(tptr->parm[2].ptype.recno);
				if (tptr == UU_NULL) return(UU_FAILURE);
			}
/*
........CONVERSION_BASED_UNIT
..........Added more input options - ASF 1/7/14.
..........More flexible/effective way to read units with the toupper and strstr - Sasha, June 06, 2017
*/
			if (tptr->command == CONVERSION_BASED_UNIT)
			{
				k = 0;
				if (tptr->parm[0].type != UTP_STRING) goto failed;

				store = (char*)tptr->parm[0].ptype.str;
				tstore = store;
				while (*store) {
					*store = toupper((unsigned char) *store);
					unitname[k] = tstore[k];
					store++;
					k++;
				}
				if (strstr(unitname,"INCH") != 0)
					Scnv = 1.;
				else if (strstr(unitname,"MET") != 0) 
					Scnv = 1. / .0254;
				else if (strstr(unitname,"CENT")!= 0 ) 
					Scnv = 1. / 2.54;
				else if (strstr(unitname,"MILL")!= 0 ) 
					Scnv = 1. / 25.4;
				else if (strstr(unitname,"DEG") != 0)
					Sangcnv = 1. / UM_RADIAN;
				else if (strstr(unitname,"RAD") != 0)
					Sangcnv = 1.0;
				else
				{
					goto failed;
				}
			}
/*
........LENGTH_UNIT
*/
			else if (tptr->command == LENGTH_UNIT)
			{
				k = 0;
				if (tptr->parm[3].type != UTP_STRING) goto failed;

				store = (char*)tptr->parm[3].ptype.str;
				tstore = store;
				
				while (*store) {
					*store = toupper((unsigned char) *store);
					unitname[k] = tstore[k];
					store++;
					k++;
				}

				if (strstr(unitname,"INCH") != 0)
					Scnv = 1.;
				else if (strstr(unitname,"MET") != 0) 
					Scnv = 1. / .0254;
				else if (strstr(unitname,"CENT")!= 0 ) 
					Scnv = 1. / 2.54;
				else if (strstr(unitname,"MILL")!= 0 ) 
					Scnv = 1. / 25.4;
				else if (strstr(unitname,"DEG") != 0)
					Sangcnv = 1. / UM_RADIAN;
				else if (strstr(unitname,"RAD") != 0)
					Sangcnv = 1.0;
				else
				{
					goto failed;
				}
			}
		}
		status = UU_SUCCESS;
		break;
/*
.....Unrecognized units tree
*/
	default:
		goto failed;
	}
	goto done;
/*
.....Could not determine units
*/
failed:;
	status = UU_FAILURE;
	utp_units_error(ptr);
/*
.....End of routine
*/
done:;
	
	return(status);
}

/*********************************************************************
**    I_FUNCTION     :  S_get_spline_knots(ptr,kpt,npts,cv)
**       Retrieves or calculates the knot and weight values for a
**       B-spline curve using the input STEP data.
**    PARAMETERS   
**       INPUT  : 
**          ptr      Pointer to STEP record in global array.
**          kpt      Starting location in record to start parsing.
**          npts     Number of control points in spline.
**          cv       Spline record, cv->k must be defined.
**       OUTPUT : none
**          cv       Calculated knot and weight data.
**    RETURNS      :
**          UU_FAILURE if error, UU_SUCCESS otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int S_get_spline_knots(ptr,kpt,npts,cv)
UTPs_step_record *ptr;
int kpt,npts;
struct UM_rbsplcrv_rec *cv;
{
	int ipt,i,j,inc,status,nmult,ival,ist;
	UU_LOGICAL iswap;
	UU_REAL degu,rnum,rval,ratio;
	UTP_command_type cmd;
/*
.....Initialize routine
*/
	ipt = kpt;
	degu = cv->k - 1;
/*
.....B_SPLINE_CURVE command
.....See if next command is B_SPLINE_CURVE_WITH_KNOTS command
*/
	cmd = ptr->command;
	if (ptr->command != B_SPLINE_CURVE_WITH_KNOTS &&
		ptr->parm[ipt].type == UTP_COMMAND)
	{
		cmd = ptr->parm[ipt].ptype.cmd;
		ipt++;
	}
/*
.....Calculate the N and NO_T parameters
........With knots
*/
	if (cmd == B_SPLINE_CURVE_WITH_KNOTS)
	{
/*
.....Count number of U-Multiplicities
*/
		for (i=ipt;i<ptr->nparm;i++)
		{
			if (cv->no_t == npts+degu+1) break;
			if (ptr->parm[i].type != UTP_REAL) goto failed;
			ival = ptr->parm[i].ptype.value; rval = ival;
			if (ptr->parm[i].ptype.value == 0. || rval != ptr->parm[i].ptype.value)
				break;
			cv->no_t += ival;
		}
		nmult = i - ipt;
/*
.....Store Knot values
*/
		cv->t = (UU_REAL *)uu_malloc(cv->no_t*sizeof(UU_REAL));
		if (cv->t == UU_NULL) goto failed;
		inc = 0;
		ist = ipt;
		ipt = i;
		iswap = UU_FALSE;
		if (cv->no_t == npts+degu-1+2 && ptr->parm[ist].ptype.value == 1. &&
			ptr->parm[ipt-1].ptype.value == 1.) iswap = UU_TRUE;
			
		for (i=0;i<nmult;i++)
		{
			ival = ptr->parm[ist++].ptype.value;
			if (ptr->parm[ipt].type != UTP_REAL) goto failed;
			if (iswap && i == 0)
				ptr->parm[ipt].ptype.value = ptr->parm[ipt+1].ptype.value;
			else if (iswap && i == nmult-1)
				ptr->parm[ipt].ptype.value = ptr->parm[ipt-1].ptype.value;
			for (j=0;j<ival;j++)
				cv->t[inc++] = ptr->parm[ipt].ptype.value;
			ipt++;
		}
/*
		rval = cv->t[0];
		ratio = cv->t[cv->no_t-1] - cv->t[0];
		for (i=0;i<cv->no_t;i++)
			cv->t[i] = (cv->t[i]-rval) / ratio;
*/
	}
/*
........Without knots
*/
	else
	{
		cv->no_t = npts + degu + 1;
/*
...........Create the KNOT data
*/
		cv->t = (UU_REAL *)uu_malloc(cv->no_t*sizeof(UU_REAL));
		if (cv->t == UU_NULL) goto failed;
		inc = 0;
		for (i=0;i<cv->k;i++)
			cv->t[inc++] = 0.;
		rnum = 1. / (cv->no_t - cv->k*2 + 1);
		for (i=cv->k;i<cv->no_t-cv->k;i++)
		{
			cv->t[inc] = cv->t[inc-1] + rnum;
			inc++;
		}
		for (i=i;i<cv->no_t;i++)
			cv->t[inc++] = 1.;
	}
/*
.....Get the Weights if specified
*/
	for (ipt=ipt;ipt<ptr->nparm;ipt++)
	{
		if (ptr->parm[ipt].type == UTP_COMMAND &&
			ptr->parm[ipt].ptype.cmd == RATIONAL_B_SPLINE_CURVE)
		{
			cv->wt = (UU_REAL *)uu_malloc(sizeof(UU_REAL)*npts);
			if (cv->wt == UU_NULL) goto failed;
			cv->no_wt = npts;
			ipt++;
			for (i=0;i<npts;i++)
			{
				if (ipt > ptr->nparm || ptr->parm[ipt].type != UTP_REAL)
					goto failed;
				cv->wt[i] = ptr->parm[ipt++].ptype.value;
			}
			break;
		}
	}
	status = UU_SUCCESS;
	goto done;
/*
.....Error parsing command
*/
failed:;
	status = UU_FAILURE;
done:;
	return(status);
}

/*********************************************************************
**    I_FUNCTION     :  S_get_surf_knots(ptr,kpt,npts,sf)
**       Retrieves or calculates the knot and weight values for a
**       B-spline curve using the input STEP data.
**    PARAMETERS   
**       INPUT  : 
**          ptr      Pointer to STEP record in global array.
**          kpt      Starting location in record to start parsing.
**          nupts    Number of control points in U-spline.
**          nvpts    Number of control points in V-spline.
**          sf       Surface record, cv->k must be defined.
**       OUTPUT : none
**          sf       Calculated knot and weight data.
**    RETURNS      :
**          UU_FAILURE if error, UU_SUCCESS otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int S_get_surf_knots(ptr,kpt,npts,sf)
UTPs_step_record *ptr;
int kpt,npts;
struct UM_rbsplsrf_rec *sf;
{
	int ipt,i,j,inc,ist,status,nuary,nvary,nu,nv,n,degu,degv,nupts,nvpts;
	UU_REAL ratio,rnum;
	UTP_command_type cmd;
/*
.....Initialize routine
*/
	ipt = kpt;
	degu = sf->ku - 1;
	degv = sf->kv - 1;
/*
.....B_SPLINE_SURFACE command
.....See if next command is B_SPLINE_SURFACE_WITH_KNOTS command
*/
	cmd = ptr->command;
	if (ptr->command != B_SPLINE_SURFACE_WITH_KNOTS &&
		ptr->parm[ipt].type == UTP_COMMAND)
	{
		cmd = ptr->parm[ipt].ptype.cmd;
		ipt++;
	}
/*
.....Knots are provided
*/
	if (cmd == B_SPLINE_SURFACE_WITH_KNOTS)
	{
/*
........Calculate number of UV-multiplicities
........And number of UV-knots
*/
		ist = ipt;
		status = S_get_uv_count(ptr,&ipt,degu,degv,&nuary,&sf->no_tu,&nvary,
			&sf->no_tv);
		if (status != UU_SUCCESS) goto done;
		nu = sf->no_tu - 1+degu - 2*degu - 1;
		sf->nu = nu - degu + 1;
		nv = sf->no_tv - 1+degv - 2*degv - 1;
		sf->nv = nv - degv + 1;
/*
........Store U-knots
*/
		sf->no_tu += 2;
		sf->tu = (UU_REAL *)uu_malloc(sf->no_tu*sizeof(UU_REAL));
		if (sf->tu == UU_NULL) goto failed;
		inc = 0;
		for (i=0;i<nuary;i++)
		{
			n = ptr->parm[ist++].ptype.value;
			for (j=0;j<n;j++)
				sf->tu[inc++] = ptr->parm[ipt].ptype.value;
			ipt++;
		}
		ratio = sf->tu[sf->no_tu-3] - sf->tu[0];
		sf->tu[inc++] = 0.;
		sf->tu[inc++] = 1.;
		rnum = sf->tu[0];
		for (i=0;i<sf->no_tu-2;i++)
			sf->tu[i] = (sf->tu[i]-rnum) / ratio;
/*
........Store V-knots
*/
		sf->no_tv += 2;
		sf->tv = (UU_REAL *)uu_malloc(sf->no_tv*sizeof(UU_REAL));
		if (sf->tv == UU_NULL) goto failed;
		inc = 0;
		for (i=0;i<nvary;i++)
		{
			n = ptr->parm[ist++].ptype.value;
			for (j=0;j<n;j++)
				sf->tv[inc++] = ptr->parm[ipt].ptype.value;
			ipt++;
		}
		ratio = sf->tv[sf->no_tv-3] - sf->tv[0];
		sf->tv[inc++] = 0.;
		sf->tv[inc++] = 1.;
		rnum = sf->tv[0];
		for (i=0;i<sf->no_tv-2;i++)
			sf->tv[i] = (sf->tv[i]-rnum) / ratio;
	}
/*
.....Calculate the NU and NO_TU parameters
.....without knots
*/
	else
	{
		sf->no_tu = sf->ku + degu + 1;
		sf->no_tv = sf->kv + degv + 1;
/*
........Create the U-knot data
*/
		sf->tu = (UU_REAL *)uu_malloc((sf->no_tu+2)*sizeof(UU_REAL));
		if (sf->tu == UU_NULL) goto failed;
		inc = 0;
		for (i=0;i<sf->ku;i++)
			sf->tu[inc++] = 0.;
		rnum = 1. / (sf->no_tu - sf->ku*2 + 1);
		for (i=sf->ku;i<sf->no_tu-sf->ku;i++)
		{
			sf->tu[inc] = sf->tu[inc-1] + rnum;
			inc++;
		}
		for (i=i;i<sf->no_tu;i++)
			sf->tu[inc++] = 1.;
		sf->tu[inc++] = 0.;
		sf->tu[inc++] = 1.;
/*
........Create the V-knot data
*/
		sf->tv = (UU_REAL *)uu_malloc((sf->no_tv+2)*sizeof(UU_REAL));
		if (sf->tv == UU_NULL) goto failed;
		inc = 0;
		for (i=0;i<sf->kv;i++)
			sf->tv[inc++] = 0.;
		rnum = 1. / (sf->no_tv - sf->kv*2 + 1);
		for (i=sf->kv;i<sf->no_tv-sf->kv;i++)
		{
			sf->tv[inc] = sf->tv[inc-1] + rnum;
			inc++;
		}
		for (i=i;i<sf->no_tv;i++)
			sf->tv[inc++] = 1.;
		sf->tv[inc++] = 0.;
		sf->tv[inc++] = 1.;
/*
........Store the n values
*/
		sf->nu = sf->no_tu - sf->ku - degu;
		sf->nv = sf->no_tv - sf->kv - degv;
		sf->no_tu += 2;
		sf->no_tv += 2;
	}
/*
.....Get the Weights if specified
*/
	nupts = degu + sf->nu;
	nvpts = degv + sf->nv;
	for (ipt=ipt;ipt<ptr->nparm;ipt++)
	{
		if (ptr->parm[ipt].type == UTP_COMMAND &&
			ptr->parm[ipt].ptype.cmd == RATIONAL_B_SPLINE_SURFACE)
		{
			sf->wt = (UU_REAL *)uu_malloc(sizeof(UU_REAL)*npts);
			if (sf->wt == UU_NULL) goto failed;
			sf->no_wt = npts;
			ipt++;
			inc = 0;
			ist = ipt;
			for (i=0;i<nvpts;i++)
			{
				for (j=0;j<nupts;j++)
				{
					ipt = i + j*nvpts;
					if (ipt > ptr->nparm || ptr->parm[ipt+ist].type != UTP_REAL)
						goto failed;
					sf->wt[inc++] = ptr->parm[ipt+ist].ptype.value;
				}
			}
			break;
		}
	}
	status = UU_SUCCESS;
	goto done;
/*
.....Error parsing command
*/
failed:;
	status = UU_FAILURE;
done:;
	return(status);
}

/*********************************************************************
**    I_FUNCTION     :  S_get_uv_count(ptr,ipt,nuary,ntu,nvary,ntv)
**       Calculates the number of U and V multiplicities in a B-spline
**       surface record.
**    PARAMETERS   
**       INPUT  : 
**          ptr      Pointer to STEP record in global array.
**          ipt      Starting location in record to start parsing.
**          degu     Degree of surface in U.
**          degv     Degree of surface in V.
**       OUTPUT : none
**          nuary    Number of U multiplicities.
**          ntu      Total number of U-knot values.
**          nvary    Number of V multiplicities.
**          ntv      Total number of V-knot values.
**    RETURNS      :
**          UU_FAILURE if error, UU_SUCCESS otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int S_get_uv_count(ptr,ipt,degu,degv,nuary,ntu,nvary,ntv)
UTPs_step_record *ptr;
int *ipt,*nuary,*ntu,*nvary,*ntv;
{
	int i,inc,ist,nmult,status,ival,ntuv,n1,n2,method;
	UU_LOGICAL ok;
	UU_REAL rval;
/*
.....Initialize routine
*/
	status = UU_FAILURE;
	inc = *ipt;
	ntuv = 0;
	n1 = (degu+1) * 2;
	n2 = n1 + (degv+1) * 2;
	method = 0;
	ok = UU_FALSE;
/*
.....Find end of multiplicities and
.....start of knot values
.......Changed break condidtion to check for negative values too
.......ASF 7/2/13.
*/
	for (i=inc;i<ptr->nparm;i++)
	{
		if (ptr->parm[i].type != UTP_REAL) goto done;
		ival = ptr->parm[i].ptype.value; rval = ival;
		if (ptr->parm[i].ptype.value <= 0. || rval != ptr->parm[i].ptype.value)
			break;
		ntuv = ntuv + ival;
	}
/*
.....Now find exact number of U-multiplicities and
.....number of U-knots
*/
	ist = i;
	nmult = i - inc;
	do
	{
		inc = *ipt;
		*nuary = 0;
		*ntu = 0;
		for (i=0;i<nmult;i++)
		{
			*ntu = *ntu + ptr->parm[inc++].ptype.value;
			*nuary = *nuary + 1;
/*
........Base T-values on always ascending values
*/
			if (method == 0)
			{
				if (ntuv == n2 && *ntu == n1) break;
				if (ptr->parm[ist+i+1].type != UTP_REAL) break;
				if (ptr->parm[ist+i+1].ptype.value < ptr->parm[ist+i].ptype.value)
					break;
			}
/*
........Base T-values on Same start multiplicity as end multiplicity
*/
			else if (method == 1)
			{
				if (i != 0 && ptr->parm[inc-1].ptype.value ==
					ptr->parm[*ipt].ptype.value) break;
			}
/*
........Could not calculate number of U-values
*/
			else
				goto done;
		}
		method++;
		if (ptr->parm[ist+i+1].type != UTP_REAL) continue;
/*
.....Find number of V-multiplicities and
.....number of V-knots
*/
		*nvary = 0;
		*ntv = 0;
		for (i=i+1;i<nmult;i++)
		{
			*ntv = *ntv + ptr->parm[inc++].ptype.value;
			if (ptr->parm[ist+i].type != UTP_REAL) goto done;
			*nvary = *nvary + 1;
		}
		if (*nvary != 0) ok = UU_TRUE;
	} while (!ok);
/*
.....Return pointer to knot values
*/
	*ipt += nmult;
	status = UU_SUCCESS;
done:;
	return(status);
}

/*********************************************************************
**    I_FUNCTION     :  S_fix_curve(cv)
**       Fixes the following problems with STEP defined B-spline curves.
**
**          1. Trims a closed curve if it overlaps itself.
**
**       Files: 209.stp - TSF(2434), et. al.
**
**    PARAMETERS   
**       INPUT  : 
**          cv       B-spline curve to fix.
**       OUTPUT : none
**          nuary    Number of U multiplicities.
**          ntu      Total number of U-knot values.
**          nvary    Number of V multiplicities.
**          ntv      Total number of V-knot values.
**    RETURNS      :
**          UU_FAILURE if error, UU_SUCCESS otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_fix_curve(cv)
struct UM_rbsplcrv_rec *cv;
{
	UM_coord *pts;
/*
.....If curve is closed, then it is possible
.....that the STEP file definition causes it
.....to overlap.
.....In this case we have to trim the curve
*/
	if (cv->closdinu)
	{
		pts = (UM_coord *)cv->pt;
		if (!um_cceqcc(pts[0],pts[cv->no_pt-1]))
		{
			if (um_cceqcc(pts[0],pts[cv->no_pt-2]) &&
				um_cceqcc(pts[1],pts[cv->no_pt-1]))
			{
				cv->t0 = cv->t[cv->k-2];
				cv->t1 = cv->t[cv->no_t-cv->k+1];
			}
		}
	}
}

