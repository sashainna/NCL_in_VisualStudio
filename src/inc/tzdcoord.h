
/*********************************************************************
**    NAME         :  mdcoord.h
**       CONTAINS:
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       tzdcoord.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:06:59
*********************************************************************/

#ifndef UM_MDCOORD

#include "usysdef.h"

#define UM_X		0						/* x component of coordinate/vector */
#define UM_Y		1						/* y component of coordinate/vector */
#define UM_Z		2						/* z component of coordinate/vector */

#define UM_CARTESIAN		1				/* cartesian coordinate representation */
#define UM_CYLINDRICAL	2				/* cylindrical coordinate representation */
#define UM_SPHERICAL		3				/* spherical coordinate representation */

#define UM_VCARTESIAN	4				/* cartesian vector representation */
#define UM_VCYLINDRICAL	5				/* cylindrical vector representation */
#define UM_VSPHERICAL	6				/* spherical vector representation */

typedef UU_REAL	UM_length;			/* length/distance */
typedef UU_REAL	UM_angle;			/* angle */
typedef UU_REAL	UM_param;			/* parameter value */
typedef UU_REAL	UM_coord[3];		/* cart/cyl/sph coordinate */
typedef UU_REAL	UM_vector[3];		/* cart/cyl/sph vector */
typedef UU_REAL	UM_transf[4][3];	/* 4X3 transformation */
typedef UU_REAL	UM_ndc[3];			/* ndc coordinate */
						
typedef struct {	/* 3-D box with origin being the lower, left-hand corner */	
			UU_REAL origin[3];
			UU_REAL axis[3][3];
			UU_REAL length[3];
	} UM_BOX;


#define UM_DEFAULT_TF	0				/* flag for using default transformation */
#define UM_CURRENT_ATTR	0				/* flag for using default attributes */

#ifdef UM_MPGM
UM_transf UM_idmat =		 				/* identity transformation */
	{1.0, 0.0, 0.0,
 	0.0, 1.0, 0.0,
 	0.0, 0.0, 1.0,
 	0.0, 0.0, 0.0};
UM_vector UM_xaxis =						/* x axis vector */
	{1.0, 0.0, 0.0};
UM_vector UM_yaxis =						/* y axis vector */
	{0.0, 1.0, 0.0};
UM_vector UM_zaxis =						/* z axis vector */
	{0.0, 0.0, 1.0};
UM_vector UM_zerovec =					/* zero vector */
	{0.0, 0.0, 0.0};
#else
extern UM_transf UM_idmat;
extern UM_vector UM_xaxis;
extern UM_vector UM_yaxis;
extern UM_vector UM_zaxis;
extern UM_vector UM_zerovec;
#endif

#define UM_MDCOORD

#endif
