/*********************************************************************
**    NAME         :  mdcoord.h
**       CONTAINS:
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       mdcoord.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:06:29
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
typedef UU_REAL	UM_covec[6];		/* coordinate and vector */

#define UM_DOT(u,v) ((u)[0] * (v)[0] + (u)[1] * (v)[1] + (u)[2] * (v)[2])
#define UM_MAG(u) sqrt(UM_DOT(u,u))
#define PYTH(x,y,z) ((x)*(x) + (y)*(y) + (z)*(z))
#define UM_SQDIS(u,v) PYTH((u)[0]-(v)[0],(u)[1]-(v)[1],(u)[2]-(v)[2])
#define UM_DCCCC(u,v) sqrt(UM_SQDIS(u,v))
						
#define UM_DOT_2D(u,v) ((u)[0] * (v)[0] + (u)[1] * (v)[1])
#define UM_MAG_2D(u) sqrt(UM_DOT_2D(u,u))
#define PYTH_2D(x,y) ((x)*(x) + (y)*(y))
#define UM_SQDIS_2D(u,v) PYTH_2D((u)[0]-(v)[0],(u)[1]-(v)[1])
#define UM_DIST_2D(u,v) sqrt(UM_SQDIS_2D(u,v))
#define UM_CROSS_2D(u,v) ((u)[0]*(v)[1]-(u)[1]*(v)[0])

typedef struct {	/* 3-D box with origin being the lower, left-hand corner */	
			UU_REAL origin[3];
			UU_REAL axis[3][3];
			UU_REAL length[3];
	} UM_BOX;

typedef struct {	
      UM_coord point;
      UM_param param;
   } UM_pointd;

typedef UU_REAL UM_2Dcoord[2];
typedef UU_REAL UM_2Dtransf[3][3];	/* 3X3 transformation */

typedef struct
{
	UU_REAL xmin;
	UU_REAL xmax;
	UU_REAL ymin;
	UU_REAL ymax;
} UM_2box;

#if UU_COMP == UU_WIN2K || UU_COMP == UU_IRIS4D
#define UM_DEFAULT_TF	(void *)0	/* flag for using default transformation */
#define UM_CURRENT_ATTR	(void *)0	/* flag for using default attributes */
#else
#define UM_DEFAULT_TF	0				/* flag for using default transformation */
#define UM_CURRENT_ATTR	0				/* flag for using default attributes */
#endif

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
UM_vector UM_origin =					/* Origin */
	{0.0, 0.0, 0.0};
UM_vector UM_zerovec =					/* zero vector */
	{0.0, 0.0, 0.0};
UM_vector UM_zvector =					/* z axis vector const */
	{0.0, 0.0, 1.0};
#else
extern UM_transf UM_idmat;
extern UM_vector UM_xaxis;
extern UM_vector UM_yaxis;
extern UM_vector UM_zaxis;
extern UM_vector UM_zvector;
extern UM_vector UM_origin;
extern UM_vector UM_zerovec;
#endif

UU_REAL um_mag(), um_dot(), um_dcccc(), um_sqdis();
UU_REAL um_mag_2d(), um_dot_2d(), um_dist_2d(), um_sqdis_2d();
UU_REAL um_angle(), um_angle2p(), um_cosang_2d();
UU_REAL um_triangle_signed_area(), um_xmin_2d();

#define UM_MDCOORD

#endif
