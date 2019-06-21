
/*********************************************************************
**    NAME         :  mdunits.h
**       CONTAINS: linear and angular unit definitions
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       mdunits.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:06:30
*********************************************************************/

#ifndef UM_MDUNITS

/*********************************************************************
*
*                    Define Units
*
*********************************************************************/


#define      UM_INCH       0  /* linear units */
#define      UM_FEET       1
#define      UM_MILE       2
#define      UM_MM         3
#define      UM_CM         4
#define      UM_M          5
#define      UM_KM         6
#define      UM_MIL        7

#define      UM_DEGR       1  /* angular units */
#define      UM_RADI       0

#define      UM_2D         2  /* 2 dimensional */
#define      UM_3D         3  /* 3 dimensional */

#ifdef UM_MPGM
char *UM_linear_units_name[] = {
   "in", "ft", "mi", "mm", "cm", "m", "km", "mil" };
char *UM_angular_units_name[] = {
   "rad", "deg"};
int UM_2d3d_mode = 3;
#else
extern char *UM_linear_units_name[];
extern char *UM_angular_units_name[];
extern int UM_2d3d_mode;
#endif

#define UM_MDUNITS
#endif
