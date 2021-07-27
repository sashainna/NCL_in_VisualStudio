/********************************************************************* 
**  NAME: m2elight.c
**
**		CONTAINS:
**				
**
**  	COPYRIGHT  1987  UNICAD, Inc.
**  	MODULE NAME AND RELEASE LEVEL 
**       m2elight.c , 25.1
**  	DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:46
**
*********************************************************************/

#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "go.h"
#include "mdrel.h"
#include "mrenddl.h"
#include "mdcoord.h"
#include "mattr.h"
#include "mdattr.h"
#include "mcrv.h"
#include "mdeval.h"
#include "modef.h"
#include "mdebug.h"


/* #include "dasnog.h" */
/* #include "class.h" */
#include "mdpick.h"


#define DTOR(x)	( (UU_REAL) (x) * 3.141592645 / 180.0)
#define RTOD(x)	( (UU_REAL) (x) * 180.0 / 3.141592645)

/* The light bulb */
#define BULBLEN 42
static UM_coord bulbpts[BULBLEN] = {
	0.257908, -0.162440, 0.000000,
	0.293124, -0.083556, 0.000000,
	0.304793, 0.002040, 0.000000,
	0.291979, 0.087473, 0.000000,
	0.255710, 0.165878, 0.000000,
	0.198900, 0.230959, 0.000000,
	0.126112, 0.277487, 0.000000,
	0.043194, 0.301724, 0.000000,
	-0.043194, 0.301724, 0.000000,
	-0.126112, 0.277487, 0.000000,
	-0.198900, 0.230959, 0.000000,
	-0.255710, 0.165878, 0.000000,
	-0.291979, 0.087473, 0.000000,
	-0.304793, 0.002041, 0.000000,
	-0.293124, -0.083555, 0.000000,
	-0.257908, -0.162440, 0.000000,
	-0.105986, -0.609601, 0.000000,
	-0.113252, -0.550476, 0.000000,
	-0.123970, -0.491879, 0.000000,
	-0.176413, -0.321254, 0.000000,
	-0.200456, -0.266752, 0.000000,
	-0.227652, -0.213753, 0.000000,
	-0.257908, -0.162440, 0.000000,
	0.257908, -0.162440, 0.000000,
	0.227652, -0.213753, 0.000000,
	0.200456, -0.266752, 0.000000,
	0.176413, -0.321253, 0.000000,
	0.155605, -0.377070, 0.000000,
	0.138104, -0.434010, 0.000000,
	0.123970, -0.491878, 0.000000,
	0.113252, -0.550475, 0.000000,
	0.105986, -0.609600, 0.000000,
	-0.101600, -0.703905, 0.000000,
	0.101600, -0.703906, 0.000000,
	-0.050800, -0.729305, 0.000000,
	0.050800, -0.729305, 0.000000,
	-0.101600, -0.678505, 0.000000,
	0.101600, -0.678505, 0.000000,
	-0.101600, -0.653105, 0.000000,
	0.101600, -0.653105, 0.000000,
	-0.101600, -0.627705, 0.000000,
	0.101600, -0.627706, 0.000000
};

#define NBULBS 9
static int bulbvecs[NBULBS] = { 16, 7, 9, 2, 2, 2, 2, 2 };


/* The point light rays */
static UM_coord  prays[] = {
	0.000001, 0.609600, 0.000000,
	0.000001, 0.914400, 0.000000,
	-0.609600, 0.000000, 0.000000,
	-0.914400, 0.000000, 0.000000,
	0.609600, 0.000000, 0.000000,
	0.914400, 0.000000, 0.000000,
	0.431052, 0.431052, 0.000000,
	0.646578, 0.646578, 0.000000,
	-0.431052, 0.431053, 0.000000,
	-0.646577, 0.646579, 0.000000,
	-0.431052, -0.431052, 0.000000,
	-0.646578, -0.646578, 0.000000,
	0.431052, -0.431052, 0.000000,
	0.646578, -0.646578, 0.000000
};

#define PRAYLEN 7
static int prayvecs[] = { 2, 2, 2, 2, 2, 2, 2 };

/* The directional light rays */
static UM_coord drays[] = {
	0.609600, 0.000000, 0.000000,
	0.914400, 0.000000, 0.000000,
	0.609600, 0.254000, 0.000000,
	0.914400, 0.254000, 0.000000,
	0.609600, 0.508000, 0.000000,
	0.914400, 0.508000, 0.000000,
	0.609600, -0.254000, 0.000000,
	0.914400, -0.254000, 0.000000,
	0.609600, -0.508000, 0.000000,
	0.914400, -0.508000, 0.000000
};

#define DRAYLEN 5
static int drayvecs[] = { 2, 2, 2, 2, 2 };

/* The spot light ray */
static UM_coord srays[] = {
	0.000000, 0.000000, 0.000000,
	1.270000, 0.000000, 0.000000
};

#define SRAYLEN 1
static int srayvecs[] = { 2 };




/*********************************************************************
**    I_FUNCTION     : um_drw38_light(ptr, tfmat, attrptr)
**       
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**		SIDE EFFECTS : none
**    WARNINGS     : none
**
*************************************************************************/
int um_drw38_light(ptr,tfmat,attrptr)
struct  UM_light_rec  *ptr;
UM_transf tfmat;
struct UM_attrdata_rec *attrptr;
{

	int i,j,k;
	UM_coord pts[20];				/* Transformed coordinates */
	UM_coord endpt;
	UM_coord axis;
	UM_coord dir;
	UM_coord cross;
	UM_transf mat;
	UM_transf wxform;
	UU_REAL angle;

	uu_denter(UU_MTRC,(us,"um_drw38_light()" ));
/*
.....we no long need draw light
.....simply return
.....Yurong 2/4/99
*/
	return;

	/* Create wxform, transforms light to world coordinates */
	axis[0] = axis[1] = axis[2] = ptr->scale;
	um_scaletf( axis, wxform );		/* Scale */

	uu_dprint(UU_MTRC,(us,"transform scaled:"));
	print_transformation(wxform);

	um_xyztovc((UU_REAL) 1.0, (UU_REAL) 0.0, (UU_REAL) 0.0, axis);		/* Rays defined in this direction */
	um_unitvc(ptr->direction, dir);		/* User wants rays to point in dir */

	/* Calculate angle between axis and dir */
	angle = 0.0;

	/* The case when directions are not parallel */
	if( !um_vcparall( dir, axis ) ) {
		um_cross( axis, dir, cross );
		angle = um_angle2p( axis, dir, cross );
	}

	/* The case when directions are parallel but opposite */
	else {		/* Rotate 180 if direction is <-1,0,0> */
		um_xyztovc( (UU_REAL) -1.0, (UU_REAL) 0.0, (UU_REAL) 0.0, cross );
		if( um_cceqcc(cross, dir) ) {
			um_xyztovc((UU_REAL) 0.0, (UU_REAL) 0.0, (UU_REAL) 1.0, cross);
			angle = DTOR((UU_REAL) 180.0);
		}
	}

	/* Rotate so new direction is dir. */
	uu_dprint(UU_MTRC,(us,"rotation angle %f", angle));
	um_rottf( cross, angle, mat );
	um_tftmtf( wxform, mat, wxform );	/* Multiply, put results in wxform */

	uu_dprint(UU_MTRC,(us,"transform rotated %f degrees from x:", 
		RTOD(angle)));
	print_transformation(wxform);

	um_disptf(ptr->position, mat);		/* Translate */
	um_tftmtf( wxform, mat, wxform );	/* Multiply, put results in wxform */

	uu_dprint(UU_MTRC,(us,"final transform:"));
	print_transformation(wxform);

	/* Set the correct attributes */
	um_set_disp_attr(attrptr);

	/* Draw the bulb */
	for(i=0, k=0; i<NBULBS; i++) {

		for(j=0; j<bulbvecs[i]; j++, k++) {
			um_cctmtf(bulbpts[k], wxform, pts[j]);
		}
		gpolyline3( bulbvecs[i], pts );
		
	}

	/* Draw the rays */
	switch( ptr->type ) {

	case UG_DIRECTIONAL:
		for(i=0, k=0; i<DRAYLEN; i++) {
			for(j=0; j<drayvecs[i]; j++, k++) {
				um_cctmtf(drays[k], wxform, pts[j]);
			}
			gpolyline3( drayvecs[i], pts );
		}
		break;

	case UG_POINT:
		for(i=0, k=0; i<PRAYLEN; i++) {
			for(j=0; j<prayvecs[i]; j++, k++) {
				um_cctmtf(prays[k], wxform, pts[j]);
			}
			gpolyline3( prayvecs[i], pts );
		}
		break;

	case UG_SPOT:

		/* Rotate end point onto the cone */
		axis[0] = axis[1] = 0.0; axis[2] = 1.0;
		um_rottf( axis, DTOR(ptr->cone_angle/2.0), mat );
		um_cctmtf( srays[1], mat, endpt );

		/* Create rotation matrix for points in the cone */
		axis[0] = 1.0; axis[1] = axis[2] = 0.0;
		um_rottf( axis, DTOR(360.0/18.0), mat );

		/* Transform base of cone */
		um_cctmtf(srays[0], wxform, pts[0]);

		/* Calculate each point on end of cone */
		for(i=0; i<18; i++) {
			um_cctmtf( endpt, mat, endpt );
			um_cctmtf( endpt, wxform, pts[1]);
			gpolyline3( 2, pts );
		}

		break;
	}

	uu_dexit;
	return (UU_SUCCESS);

}

/*********************************************************************
**    E_FUNCTION     : int um_tf38_tranflight(eptr,tranfmat,store)
**      Transform a light by the given 4X3 matrix and update
**			UNIBASE iff store == UU_TRUE.
**    PARAMETERS   
**       INPUT  : 
**				eptr         pointer to the entity to be transformed
**          tranfmat     the 4x3 transformation matrix
**				store			 TRUE iff UNIBASE is to be updated here.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_tf38_tranflight(eptr,tfmat,store)
struct UM_light_rec *eptr;
UM_transf    tfmat;
UU_LOGICAL store;
{                                     
	UM_coord vc;

	uu_denter(UU_MTRC,(us,"um_tf38_tranflight(key:%d,tfmat:%x,store:%d)",
						eptr->key,tfmat,store));
    
	/* Transform current light by tfmat, and store in unibase */
	uu_dprint(UU_MTRC,(us,"org pos %f %f %f",
		eptr->position[0], eptr->position[1], eptr->position[2]));
	um_cctmtf( eptr->position, tfmat, eptr->position );
	uu_dprint(UU_MTRC,(us,"final pos %f %f %f",
		eptr->position[0], eptr->position[1], eptr->position[2]));

	uu_dprint(UU_MTRC,(us,"org dir %f %f %f",
		eptr->direction[0], eptr->direction[1], eptr->direction[2]));
	um_vctmtf( eptr->direction, tfmat, eptr->direction );
	uu_dprint(UU_MTRC,(us,"final dir %f %f %f", 
		eptr->direction[0], eptr->direction[1], eptr->direction[2]));

	/* Find scale factor by transforming a unit vector */
	uu_dprint(UU_MTRC,(us,"org scale %f", eptr->scale ));
	vc[0] = 1.0;	vc[1] = vc[2] = 0.0;
	um_vctmtf(vc, tfmat, vc);
	eptr->scale *= um_mag(vc);
	uu_dprint(UU_MTRC,(us,"final scale %f", eptr->scale));

	if (store) {
		ur_update_data(eptr);
	}

	uu_dexit;
	return(UU_SUCCESS);
}

/*********************************************************************
**    I_FUNCTION     : um_dl38_light(key)
**
**		Disables deleting lights.
**       
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**		SIDE EFFECTS : none
**    WARNINGS     : none
**
*************************************************************************/
int um_dl38_light(key)
UU_KEY_ID key;
{
	uu_denter(UU_MTRC,(us,"um_dl38_light(%d)", key));

	/* Print an error message */
	uu_uerror0(/*um_d_pdas: illegal entity picked*/UM_MODEL,43);

	uu_dexit;
	return (UU_SUCCESS);
}

/********************************************************************* 
**    I_FUNCTION     : um_p38_light(ptr)
**
**		Prints light description.
**       
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**		SIDE EFFECTS : none
**    WARNINGS     : none
**
*************************************************************************/
int um_p38_light(ptr)
struct UM_light_rec *ptr;
{
	int i;


	uu_denter(UU_MTRC,(us,"um_p38_light(%x)",ptr));

	uu_dprint(UU_MTRC,(us,"key %d", ptr->key));
	um_p_ary(UM_PINT, "key", 1, &ptr->key);

	uu_dprint(UU_MTRC,(us,"rel num %d", ptr->rel_num));
	um_p_ary(UM_PINT, "rel num", 1, &ptr->rel_num);

	uu_dprint(UU_MTRC,(us,"index %d", ptr->index));
	um_p_ary(UM_PINT, "index", 1, &ptr->index);

	uu_dprint(UU_MTRC,(us,"type %d", ptr->type));
	um_p_ary(UM_PINT, "type", 1, &ptr->type);

	uu_dprint(UU_MTRC,(us,"intens %d", ptr->intens));
	um_p_ary(UM_PINT, "intens", 1, &ptr->intens);

	uu_dprint(UU_MTRC,(us,"scale %f",
		ptr->scale ));
	um_p_ary(UM_PFLOAT, "scale", 1, &ptr->scale);

	uu_dprint(UU_MTRC,(us,"position %f %f %f", 
		ptr->position[0], ptr->position[1], ptr->position[2]));
	um_p_ary(UM_PFLOAT, "position", 3, ptr->position);

	uu_dprint(UU_MTRC,(us,"direction %f %f %f", 
		ptr->direction[0], ptr->direction[1], ptr->direction[2]));
	um_p_ary(UM_PFLOAT, "direction", 3, ptr->direction);

	uu_dprint(UU_MTRC,(us,"attenuation %f %f", 
		ptr->attenuation[0], ptr->attenuation[1] ));
	um_p_ary(UM_PFLOAT, "attenuation", 2, ptr->attenuation);

	uu_dprint(UU_MTRC,(us,"cone_angle %f",
		ptr->cone_angle ));
	um_p_ary(UM_PFLOAT, "cone_angle", 1, &ptr->cone_angle);

	uu_dexit;
	return (UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION    : int um_light_ploc_to_coord(level,pickpath,pickloc,pt)
**			Determine a cartesian coordinate (PT) given a picked location
**			on a light (LEVEL, PICKPATH, PICKLOC).
**    PARAMETERS   
**       INPUT  : 
**				level					level of entity picked within pickpath
**				pickpath				DAS pick path record
**				pickloc				DAS pickloc record
**       OUTPUT :  
**				pt						cartesian coordinate (MCS) corresponding
**										to picked location on entity
**    RETURNS      : 
**			UU_SUCCESS  iff no errors; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_light_ploc_to_coord(level, pickpath, pickloc, pt)
int level;
UD_PPICKREC *pickpath;
UD_NDCLOCREC *pickloc;
UM_coord pt;
{
	UM_PICKENT pent;
	struct UM_light_rec e;
	int status;

	uu_denter(UU_MTRC,(us, "um_light_ploc_to_coord(level=%d,path=%x,loc=%x)",
		level, pickpath, pickloc));

	status = um_d_pickresolve(pickpath, level, &pent);
	if (status != UU_SUCCESS) goto done;

	e.key = um_get_pickkey(&pent, 1);
	status = uc_retrieve_data(&e, sizeof(e));
	if (status != UU_SUCCESS) goto done;

	um_vctovc(e.position, pt);

done:;
	uu_dexitstatus("um_light_ploc_to_coord", status);
	return (status);
}

/*********************************************************************
**    E_FUNCTION    : int um_light_ploc_to_vector(level,pickpath,pickloc,pt)
**			Determine a vector (PT) given a picked location
**			on a light (LEVEL, PICKPATH, PICKLOC).
**    PARAMETERS   
**       INPUT  : 
**				level					level of entity picked within pickpath
**				pickpath				DAS pick path record
**				pickloc				DAS pickloc record
**       OUTPUT :  
**				pt						cartesian vectorinate (MCS) corresponding
**										to picked location on entity
**    RETURNS      : 
**			UU_SUCCESS  iff no errors; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_light_ploc_to_vector(level, pickpath, pickloc, pt)
int level;
UD_PPICKREC *pickpath;
UD_NDCLOCREC *pickloc;
UM_vector pt;
{
	UM_PICKENT pent;
	struct UM_light_rec e;
	int status;

	uu_denter(UU_MTRC,(us, "um_light_ploc_to_vector(level=%d,path=%x,loc=%x)",
		level, pickpath, pickloc));

	status = um_d_pickresolve(pickpath, level, &pent);
	if (status != UU_SUCCESS) goto done;

	e.key = um_get_pickkey(&pent, 1);
	status = uc_retrieve_data(&e, sizeof(e));
	if (status != UU_SUCCESS) goto done;

	um_vctovc(e.direction, pt);

done:;
	uu_dexitstatus("um_light_ploc_to_vector", status);
	return (status);
}


static print_transformation(tfmat)
UM_transf tfmat;
{
	uu_denter(UU_MTRC,(us,"TRANSFORMATION: "));
	uu_dprint(UU_MTRC,(us,"						%g, %g, %g", 
		tfmat[0][0],tfmat[0][1],tfmat[0][2]));
	uu_dprint(UU_MTRC,(us,"                %g, %g, %g",
		tfmat[1][0],tfmat[1][1],tfmat[1][2]));
	uu_dprint(UU_MTRC,(us,"                %g, %g, %g",
		tfmat[2][0],tfmat[2][1],tfmat[2][2]));
	uu_dprint(UU_MTRC,(us,"                %g, %g, %g",
		tfmat[3][0],tfmat[3][1],tfmat[3][2]));
	uu_dexit;
	return (UU_SUCCESS);
}
