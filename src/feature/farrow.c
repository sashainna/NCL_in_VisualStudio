
/*********************************************************************
**    NAME         :  farrow
**       CONTAINS:
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       farrow.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:44
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "mdcoord.h"
#include "mfeatcom.h"
#include "mdeval.h"
#include "modef.h"

/********************************************************************* 
**  NAME:  um_arrow
**      display an arrow at the desired position
**  PARAMETERS   
**      input:  vec1 - real array containing the start point
**              vect - real array to hold the three 
**                     components of the vector feature
**      output: none
**  RETURNS      :  none
**  SIDE EFFECTS :  displays the arrow (vector) at the desired
**                  screen position
**  WARNINGS     :  none
*********************************************************************/
um_arrow (vec1,vect)
   UU_REAL vec1[]; /* real array containing the start point */
   UU_REAL vect[]; /* real array to hold the vector components */

{
/* local declarations */

	struct UM_rotmatrix matrix;
   UU_REAL ln_pt[6]; /* array to hold line points */
   UU_REAL theta; /* the yaw angle for the arrowhead calculations */
	UU_REAL tip[3];
	UU_REAL uvect[3]; 
	UU_REAL unvect[3];
	UU_REAL nvect[3];
	UU_REAL mag_vect;
	UU_REAL del;
	UU_REAL pt[3];
	UU_REAL mvec1;
	UU_REAL mvec2;
	UU_REAL vptan[3];
	UU_REAL dvec1[3];
	UU_REAL dvec2[3];
	UU_REAL ref_vect[3];
	UU_LOGICAL flag;
   int i,j;
	UU_REAL UM_vpup[3];
	UU_REAL UM_vpnorm[3];

	um_vctovc(UM_zaxis, UM_vpnorm);
	um_vctovc(UM_yaxis, UM_vpup);
	  /* unitize the vector */
		 um_unitvc(vect,uvect);

		 /* make the tail */
         um_vctovc (vec1,ln_pt);
			um_vcplvc (vec1,vect,&ln_pt[3]);

       /* make the head */
			/* define the position of the arrowhead tip */
           um_vctovc (&ln_pt[3], tip);

			/* define the third view plane vector */
			  um_cross (UM_vpup, UM_vpnorm, vptan);

			/* determine which view plane vector is nearest to 90
				degrees from the line vector */
			  um_cross (uvect, UM_vpup,  dvec1);
			  um_cross (uvect, vptan, dvec2);
           mvec1 = um_mag (dvec1);
			  mvec2 = um_mag (dvec2);

			  if (mvec1 > mvec2)
				{
				 /* use view plane up vector */
					um_vctovc (UM_vpup, ref_vect);
				}
			  else
				{
				 /* use view plane tangent vector */
					um_vctovc (vptan, ref_vect);
				}

         /* define the normal to the viewing plane and the line */
           um_cross (uvect, ref_vect, nvect);

			/* calculate the vector normal to the line in the plane */
			  um_cross (nvect, uvect, unvect);
			
			/* get the magnitude of the vector */
			  mag_vect = um_mag (vect);

			/* define the delta distance for the offset */
			  del = mag_vect / 3.0;

			/* multiply the vector by the scalar delta */
			  um_vctmsc (uvect, del, uvect);

			/* subtract the scaled unit vector from the tip */
			  um_vcmnvc (tip, uvect, pt);

			/* redefine delta */
			  del = 0.5 * del;

			/* multiply the normal vector by the new delta */
			  um_vctmsc (unvect, del, nvect);

			/* get the first point for the arrowhead */
			  um_vcplvc (pt, nvect, pt);

         /* calculate the x,y,z for other points around the line */
			  /* set flag */
				 flag = UU_TRUE;
			  /* set theta */
			    theta = 0.;

          /* display the tail */
				/* display the tail segment */
              gpolyline3 (2,ln_pt);

			 /* display the arrowhead line segments */
				/* put the tip in as one end of all segments */
				  um_vctovc(tip,ln_pt);

				for (i=0 ; i<3 ; i++)
				  {
				  /* define the second end of the segment */
					 um_vctovc (pt, &ln_pt[3]);

				  /* display the point */
                gpolyline3 (2,ln_pt);

				  /* calculate a new point rotated 120 degrees
					  around the line */
				  /* increment theta in radians */
				    theta = theta + UM_PI/3;

				  /* rotate the point */
					 um_rotatept(pt,vect,tip,theta,flag,&matrix);

              }

} /* end of main program body */
