
/*********************************************************************
**    NAME         :  m3ecirc3.c
**       CONTAINS:
**			int um_drw3_circle(ptr, tfmat, attrptr)
**			int um_p3_circle(ptr)
**			int um_cp3_copycirc(e1, e2, bagsize)
**			int um_tr3_trancirc(eptr, offset)
**			int um_rt3_rotcirc(eptr, rotpt, rotmat)
**			int um_sc3_scalcirc(eptr, scalpt, scalmat, scale)
**			int um_tf3_tranfcirc(eptr, tranfmat, store)
**			int um_mir3_mircirc(eptr, mirrpt, mirrnorm, mirrmat)
**			int um_c3_copy(cin, cout)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m3ecirc3.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:07:51
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "go.h"
#include "mdcoord.h"
#include "mdrel.h"
#include "mdattr.h"
#include "mattr.h"
#include "mcrv.h"
#include "modef.h"
#include "mdeval.h"
#include "mplot.h"
#include "mdebug.h"

/*********************************************************************
**    E_FUNCTION     : int um_drw3_circle(ptr,tfmat,attrptr)
**       Display a circle or arc in the current segment.
**    PARAMETERS   
**       INPUT  : 
**				ptr    			pointer to circle record
**				tfmat				transformation matrix
**				attrptr			pointer to attribute record
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_drw3_circle(ptr,tfmat,attrptr)
	struct  UM_circle_rec  *ptr;
	UM_transf tfmat;
	struct UM_attrdata_rec *attrptr;

	{
	UM_vector yaxis;			/* vector normal to nvec and svec */
	UM_vector xcomp;			/* component in "x" direction */
	UM_vector ycomp;			/* component in "y" direction */
	UM_coord pt;				/*  coordinates of point on circle */
	UM_coord mcpt;				/* model coordinates of point on circle */
	UM_angle ang;				/* angle */
	UM_angle deltang;			/* delta angle */
	int   nside;				/* the number of sides of the approximating
										polygon */
	int i;						/* index */
	UM_length xlen;				/* x length */
	UM_length ylen;				/* y length */
	Gwpoint3 gpt[70];			/* points to sent to GKS */
	Gint ngpt;					/* index into gpt */

	uu_denter(UU_MTRC,(us,"um_drw3_circle(ptr->key:%d,tfmat:%x,attrptr:%x)",
					ptr->key,tfmat,attrptr));

/*
.....Calling um_dash3_circle will override the line style pattern
.....set in the plotting routines.  It makes the dashes too small.
.....JLS 8/25/99
	if ((UM_plotting) && (attrptr->line_style == UM_DASHED_LINE))
		um_dash3_circle(ptr, tfmat, attrptr);
	else
		{
*/
	um_set_disp_attr(attrptr);

	um_vctmsc(ptr->svec, ptr->radius, pt);
	um_vcplvc(pt, ptr->center, pt);
	um_cross(ptr->nvec, ptr->svec, yaxis);
	um_cctmtf(pt, tfmat, mcpt);
	gpt[0].x = mcpt[0];
	gpt[0].y = mcpt[1];
	gpt[0].z = mcpt[2];
	um_set3_circle(ptr,&nside,&ang,&deltang);
	ngpt = 1;
	for (i = 0; i < nside; i++)
	{
		xlen = ptr->radius * cos(ang);
		um_vctmsc(ptr->svec, xlen, xcomp);
		ylen = ptr->radius * sin(ang);
		um_vctmsc(yaxis, ylen, ycomp);
		um_vcplvc(xcomp, ycomp, pt);
		um_vcplvc(ptr->center, pt, mcpt);
		um_cctmtf(mcpt, tfmat, mcpt);
		if (ngpt == 70)
		{
			um_gpolyline3(attrptr, ngpt, gpt);
			gpt[0].x = gpt[69].x;
			gpt[0].y = gpt[69].y;
			gpt[0].z = gpt[69].z;
			ngpt = 1;
		}
		gpt[ngpt].x = mcpt[0];
		gpt[ngpt].y = mcpt[1];
		gpt[ngpt].z = mcpt[2];
		ngpt++;
		ang = ang + deltang;
	}
	um_gpolyline3(attrptr, ngpt, gpt);
	uu_dexit;
	return (UU_SUCCESS);
	}
/*********************************************************************
**    E_FUNCTION     : int um_p3_circle(ptr)
**       Print the data defining a circle (arc).
**    PARAMETERS   
**       INPUT  : 
**				ptr					pointer to circle record
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_p3_circle(ptr)
	struct  UM_circle_rec  *ptr;
	{

	uu_denter( UU_MTRC,(us,"um_p3_circle(%8x)",ptr));
	sprintf( UM_sbuf, "CIRCLE %d", ptr->key);
	um_pscroll(UM_sbuf);
	sprintf(UM_sbuf, "label %7.7s", ptr->label);
	um_pscroll(UM_sbuf);
	um_p_ary(UM_PFLOAT, "radius", 1, &ptr->radius);
	um_p_ary(UM_PFLOAT, "dang",   1, &ptr->dang);
	um_p_ary(UM_PFLOAT, "center", 3, ptr->center);
	um_p_ary(UM_PFLOAT, "svec",   3, ptr->svec);
	um_p_ary(UM_PFLOAT, "nvec",   3, ptr->nvec);
	umi_print_transf(ptr->key);
	uu_dexit;
	return (UU_SUCCESS);
	}
/*********************************************************************
**    E_FUNCTION: int um_cp3_copycirc(e1, e2, bagsize)
**      Copy a circle into a new circle.  
**    PARAMETERS   
**       INPUT  : 
**				e1			pointer to entity to be copied
**				bagsize	size of storage for entity.
**       OUTPUT :  
**				e2       pointer to new entity
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_cp3_copycirc(e1, e2, bagsize)
	struct UM_circle_rec *e1;        /* The entity to translate */
	struct UM_circle_rec *e2;        /* New entity */
	int bagsize;

	{
	struct UM_attrdata_rec attrbag;

	uu_denter(UU_MTRC,(us,"um_cp3_copycirc(?,?,?)"));
                                            
	ur_setup_data(e1->rel_num, e2, bagsize);
	/* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
	strcpy (e2->label, "");
	e2->subscr = 0;
   um_vctovc(e1->center, e2->center);
   um_vctovc(e1->nvec,   e2->nvec);
   um_vctovc(e1->svec,   e2->svec);
   e2->radius = e1->radius;
   e2->dang   = e1->dang;

	um_get_disp_attr(e1->key, &attrbag);
	um_create_geom(e2, UM_DEFAULT_TF, &attrbag);
	uu_dexit;
	return (UU_SUCCESS);
	}
/*********************************************************************
**    E_FUNCTION     : int um_tr3_trancirc(eptr,offset)
**      Translate the specified circle from the "from" point to the "to" point.
**    PARAMETERS   
**       INPUT  : 
**				eptr		pointer to the  entity to be translated
**			   offset	vector by which to translate
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_tr3_trancirc(eptr,offset)
	struct UM_circle_rec *eptr;
	UM_vector   offset;

	{
	uu_denter( UU_MTRC,(us,"um_tr3_trancirc(?,?)"));
    
   um_vcplvc(eptr->center, offset, eptr->center);

	um_update_geom(eptr, UM_DEFAULT_TF);
	uu_dexit;
	return (UU_SUCCESS);
	}
/*********************************************************************
**    E_FUNCTION     : int um_rt3_rotcirc(eptr,rotpt,rotmat)
**      Rotate the specified circle by the given 4x3 matrix.
**    PARAMETERS   
**       INPUT  : 
**				eptr        pointer to the entity to be rotated
**          rotpt       point on rotation axis
**          rotmat      the 4x3 rotation matrix
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_rt3_rotcirc(eptr,rotpt,rotmat)
	struct UM_circle_rec *eptr;
	UM_coord    rotpt;
   UM_transf    rotmat;

	{                                     
   UM_coord    temppt;           /* Temporary point */

	uu_denter( UU_MTRC,(us,"um_rt3_rotcirc(?,?,?)"));
    
   um_vcmnvc(eptr->center, rotpt, temppt);          
   um_cctmtf(temppt, rotmat, temppt);
   um_vcplvc(temppt, rotpt, eptr->center);

   um_cctmtf(eptr->nvec, rotmat, eptr->nvec);
   um_cctmtf(eptr->svec, rotmat, eptr->svec);

	um_update_geom(eptr, UM_DEFAULT_TF);
	uu_dexit;
	return (UU_SUCCESS);
	}
/*********************************************************************
**    E_FUNCTION     : int um_sc3_scalcirc(eptr,scalpt,scalmat,scale)
**      Scale the specified circle by the given 4x3 matrix.
**    PARAMETERS   
**       INPUT  : 
**				eptr         pointer to the entity to be scaled
**          scalpt       point to be scaled about
**          scalmat      the 4x3 scaling matrix
**				scale 		 scale factor, for scaling lengths
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_sc3_scalcirc(eptr,scalpt,scalmat,scale)

	struct UM_circle_rec *eptr;
	UM_coord    scalpt;
   UM_transf   scalmat;
   UM_length   scale;

	{                                     

    UM_coord    temppt;           /* Temporary point */

	uu_denter(UU_MTRC,(us,"um_sc3_scalcirc(?,?,?)"));
    
   um_vcmnvc(eptr->center, scalpt, temppt);          
   um_cctmtf(temppt, scalmat, temppt);
   um_vcplvc(temppt, scalpt, eptr->center);

	eptr->radius *= scale;

	um_update_geom(eptr, UM_DEFAULT_TF);
	uu_dexit;
	return (UU_SUCCESS);
	}
/*********************************************************************
**    E_FUNCTION     : int um_tf3_tranfcirc(eptr,tranfmat,store)
**			Transform a circle by the given 4X3 transformation and
**			store in UNIBASE iff store is true.
**    PARAMETERS   
**       INPUT  : 
**			 eptr        	pointer to the entity to be transformed
**        tranfmat    	the 4x3 transformation matrix
**			 store 			TRUE iff we are to store the transformed entity in
**								UNIBASE.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_tf3_tranfcirc(eptr,tranfmat,store)
	struct UM_circle_rec *eptr;
	UM_transf tranfmat;
	UU_LOGICAL store;

	{                                     
   UM_vector rvec1, rvec2;

	uu_denter(UU_MTRC,(us,"um_tf3_tranfcirc(key:%d,tfmat:%x,store:%d)",
		eptr->key, tranfmat, store));

/*
...   calculate & transform tangent vector at start point of circle
...   for later comparison with tangent vector of transformed circle
*/
   um_cross (eptr->nvec, eptr->svec, rvec1);
   um_vctmtf(rvec1, tranfmat, rvec1);

   um_cctmtf(eptr->center, tranfmat, eptr->center);
   um_vctmtf(eptr->nvec, tranfmat, eptr->nvec);
   um_unitvc(eptr->nvec,eptr->nvec);
   um_vctmtf(eptr->svec, tranfmat, eptr->svec);
   eptr->radius = eptr->radius * um_mag(eptr->svec);
   um_unitvc(eptr->svec,eptr->svec);
/*
...   Calculate new tangent vector at start point of circle. If it opposes
...   saved vector, circle has been transformed through a mirror matrix &
...   angle must be reversed
*/
   um_cross (eptr->nvec, eptr->svec, rvec2);
   if (um_dot(rvec1,rvec2) < 0.0) eptr->dang = -(eptr->dang);

	if (store)
	 	um_update_geom(eptr, UM_DEFAULT_TF);
	uu_dexit;
	return (UU_SUCCESS);
	}

/*********************************************************************
**    E_FUNCTION : int um_mir3_mircirc(eptr, mirrpt, mirrnorm, mirrmat)
**       Mirror a circle entity.
**    PARAMETERS   
**       INPUT  : 
**          eptr		pointer to the circle entity that is to have its 
**							record changed so that it is the mirror of the 
**							image of its current image.
**				mirrpt	point defining mirror plane
**				mirrnorm	normal to mirror plane
**				mirrmat	mirroring matrix.
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_mir3_mircirc(eptr, mirrpt, mirrnorm, mirrmat)
	struct UM_circle_rec *eptr;
	UM_coord mirrpt;
	UM_vector mirrnorm;
	UM_transf	mirrmat;

	{
	uu_denter(UU_MTRC,(us,"um_mir3_mircirc(%d,%x)",eptr->key,mirrmat));
   um_tf3_tranfcirc(eptr, mirrmat, UU_TRUE);
/*    eptr->dang = -(eptr->dang);  */
	um_update_geom(eptr, UM_DEFAULT_TF);
	uu_dexit;
	return (UU_SUCCESS);
	}
/*********************************************************************
**    E_FUNCTION     : int um_c3_copy(cin, cout)
**       Copy a circle.
**    PARAMETERS   
**       INPUT  : 
**				cin     		pointer to original circle entity
**       OUTPUT :  
**				cout 		   pointer to copy of original
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_c3_copy(cin, cout)
	struct UM_circle_rec  *cin;
	struct UM_circle_rec  *cout;

	{
	uu_denter( UU_MTRC,(us,"um_c3_copy(?,?)"));
	ur_setup_data(UM_CIRCLE_REL, cout, sizeof(struct UM_circle_rec));
	/* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
	strcpy (cout->label, "");
	cout->subscr = 0;
	cout->key = 0;
	um_vctovc(cin->center, cout->center);
	cout->radius = cin->radius;
	cout->dang   = cin->dang;
	um_vctovc(cin->svec, cout->svec);
	um_vctovc(cin->nvec, cout->nvec);
	uu_dexit;
	return (UU_SUCCESS);
	}
