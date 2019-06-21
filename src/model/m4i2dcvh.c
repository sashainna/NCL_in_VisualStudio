/*********************************************************************
**    NAME         :  m2dcvh
**       CONTAINS:
**							um_2dcvh
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m4i2dcvh.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:03
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "mcvh.h"
#include "modef.h"

#define UM_INTFUZZ (UU_REAL) 0.001
#define UM_INTFUZZSQ (UU_REAL) 0.01

struct hull{								/* data for convex hull calculation */
	UU_REAL phi;
	UU_REAL rad;
	UU_REAL x;
	UU_REAL y;
	int index;
};

/*********************************************************************
**    E_FUNCTION     : um_2dcvh(ix,iy,npnts,pt,mpnts,indx,min_indx,max_rad,cvh)
**       Compute the 2-d convex hull of the rational b-spline control
**       points. Uses the Graham algorithm as discribed in the paper
**			by Allison et. al. (BIT 24 ).
**    PARAMETERS   
**       INPUT  : 
**				ix									index of first coordinate
**				iy 								index of second coordinate
**				npnts								number of points in the control polygon
**				pt									control polygon
**       OUTPUT :  
**				mpnts								number of points in the convex hull
**				indx								sorted array of indices to the control
**													polygon which represents the convex hull.
**				min_indx							index of point used to calculate sweep
**				max_rad							maximum radius
**				cvh								flag to identify cvh type
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void um_2dcvh(ix,iy,npnts,pt,mpnts,indx,min_indx,max_rad,cvh)
	 
	int npnts;									/* number of control points */
	int ix;										/* first coordinate index */
	int iy;										/* second coordinate index */
	UU_REAL pt[][3];							/* control polygon */
	int *mpnts;									/* number of points in the convex hull */
	int indx[];									/* array of indices */
	int *min_indx;								/* index of min sweep point */
	UU_REAL *max_rad;							/* maximum radius */
	int *cvh;									/* type flag */

	{
	struct hull pm[100];						/* convex hull data */
	int i,j,count;
	int yloc;									/* index of point with minimum y */
	UU_REAL vec1[3],vec2[3];		/* local coordinate system */
	UU_REAL xmin;								/* minimum x value */
	UU_REAL ymin;								/* minimum y value */
	UU_REAL xlen;								/* x-coord */
	UU_REAL ylen;								/* y-coord */
	UU_REAL xdis;								/* x-distance */
	UU_REAL ydis;								/* y-distance */
	UU_REAL radius;							/* dist between given pt and sweep pt */
	int um_phicmp();							/* sort function for qsort */
	UU_REAL um_dot();
	/*------------------------------------------------------------------------
	**  Start of Executable Code
	**-----------------------------------------------------------------------*/

	uu_denter(UU_MTRC,(us,"um_2dcvh(%d,%8x,?,?)",npnts,pt));

										/* check if hull projects to a point */

	count = 0;
	for(i=1;i<npnts;i++)
		{
		xlen = pt[i][ix] - pt[0][ix];
		if(fabs(xlen) > UM_FUZZ)
			{
			count = 1;
			break;
			}
		else
			{
			ylen = pt[i][iy] - pt[0][iy];
			if(fabs(ylen) > UM_FUZZ)
				{
				count = 1;
				break;
				}
			}
		}
	if(count == 0)
		{
		*cvh = UM_CVH_POINT;
		*mpnts = 0;
		*min_indx = -1;
		*max_rad = 0.0;
		uu_dexit;
		return;
		}

													/* ok! branch on num of pts */
	if(npnts <= 2)
		{
		indx[0] = 0;
		indx[1] = 1;
		indx[2] = 0;
		*mpnts = 3;
		*min_indx = -1;
		xlen = pt[1][ix] - pt[0][ix];
		ylen = pt[1][iy] - pt[0][iy];
		/* *max_rad = sqrt(xlen*xlen + ylen*ylen);*/
		*max_rad = xlen*xlen + ylen*ylen;
		*cvh = UM_CVH_LINEAR;
		}
	else if(npnts == 3)
		{
		vec1[0] = pt[1][ix] - pt[0][ix];
		vec1[1] = pt[1][iy] - pt[0][iy];
		vec1[2] = 0.0;
		vec2[0] = pt[2][ix] - pt[0][ix];
		vec2[1] = pt[2][iy] - pt[0][iy];
		vec2[2] = 0.0;
		xlen = (vec1[0]*vec1[0] + vec1[1]*vec1[1]);
		ylen = (vec2[0]*vec2[0] + vec2[1]*vec2[1]);
		if(xlen < UM_FUZZ)
			{
			indx[0] = 0;
			indx[1] = 2;
			indx[2] = 0;
			*mpnts = 3;
			*min_indx = -1;
			*max_rad = ylen*ylen;
			*cvh = UM_CVH_LINEAR;
			}
		else if (ylen < UM_FUZZ)
			{
			indx[0] = 0;
			indx[1] = 1;
			indx[2] = 0;
			*mpnts = 3;
			*min_indx = -1;
			*max_rad = xlen*xlen;
			*cvh = UM_CVH_LINEAR;
			}
		else
			{
			um_unitvc(vec1,vec1);
			um_unitvc(vec2,vec2);
			xmin = um_dot(vec1,vec2);
													/* check for co-linear points */
			if(fabs(fabs(xmin) - 1.0) < UM_FUZZ)
				{										/* pt's are co-linear - remove one */

				if(xmin < 0.0)						/* pt[0] is mid-point */
					{
					indx[0] = 1;
					indx[1] = 2;
					indx[2] = 1;
					*mpnts = 3;
					*min_indx = -1;
					xlen = pt[1][ix] - pt[2][ix];
					ylen = pt[1][iy] - pt[2][iy];
					*max_rad = xlen*xlen + ylen*ylen;
					*cvh = UM_CVH_LINEAR;
					}
				else
					{
					if(xlen > ylen)				/* remove pt[2] */
						{
						indx[0] = 0;
						indx[1] = 1;
						indx[2] = 0;
						*mpnts = 3;
						*min_indx = -1;
						xlen = pt[1][ix] - pt[0][ix];
						ylen = pt[1][iy] - pt[0][iy];
						*max_rad = xlen*xlen + ylen*ylen;
						*cvh = UM_CVH_LINEAR;
						}
					else								/* remove pt[1] */
						{
						indx[0] = 0;
						indx[1] = 2;
						indx[2] = 0;
						*mpnts = 3;
						*min_indx = -1;
						xlen = pt[2][ix] - pt[0][ix];
						ylen = pt[2][iy] - pt[0][iy];
						*max_rad = xlen*xlen + ylen*ylen;
						*cvh = UM_CVH_LINEAR;
						}
					}
				}
			else
				{
				indx[0] = 0;
				indx[1] = 1;
				indx[2] = 2;
				indx[3] = 0;
				*mpnts = 4;
				*min_indx = -1;
				xlen = pt[1][ix] - pt[0][ix];
				ylen = pt[1][iy] - pt[0][iy];
				*max_rad = (xlen*xlen + ylen*ylen);
				xlen = pt[2][ix] - pt[0][ix];
				ylen = pt[2][iy] - pt[0][iy];
				xdis = (xlen*xlen + ylen*ylen);
				if(*max_rad < xdis) *max_rad = xdis;
				xlen = pt[2][ix] - pt[1][ix];
				ylen = pt[2][iy] - pt[1][iy];
				xdis = (xlen*xlen + ylen*ylen);
				if(*max_rad < xdis) *max_rad = xdis;
				/* *max_rad = sqrt(*max_rad);*/
				*cvh = UM_CVH_COMPLEX;
				}
			}
		}
	else
		{
		if(npnts == 4)
			{
			vec1[0] = pt[1][ix] - pt[0][ix];
			vec1[1] = pt[1][iy] - pt[0][iy];
			vec1[2] = 0.0;
			vec2[0] = pt[2][ix] - pt[0][ix];
			vec2[1] = pt[2][iy] - pt[0][iy];
			vec2[2] =  0.0;
			xlen = (vec1[0]*vec1[0] + vec1[1]*vec1[1]);
			ylen = (vec2[0]*vec2[0] + vec2[1]*vec2[1]);
			if(xlen < UM_FUZZ)
				{	
				xmin = 1.0;
				vec1[0] = vec2[0];
				vec1[1] = vec2[1];
				}
			else if (ylen < UM_FUZZ)
				{
				xmin = 1.0;
				}
			else
				{
				um_unitvc(vec1,vec1);
				um_unitvc(vec2,vec2);
				xmin = um_dot(vec1,vec2);
				}
			if(fabs(fabs(xmin) - 1.0) < UM_FUZZ)
				{
				vec2[0] = pt[3][ix] - pt[0][ix];
				vec2[1] = pt[3][iy] - pt[0][iy];
				vec2[2] = 0.0;
				ylen = (vec2[0]*vec2[0] + vec2[1]*vec2[1]);
				if(ylen < UM_FUZZ)
					{
					xmin = 1.0;
					}
				else
					{
					um_unitvc(vec2,vec2);
					xmin = um_dot(vec1,vec2);
					}
				if(fabs(fabs(xmin) - 1.0) < UM_FUZZ)
					{
					xmin = pt[0][ix];
					ymin = pt[0][iy];
					yloc = 0;
					for(i=1;i<npnts;i++)
						{
						xlen = pt[i][ix];
						ylen = pt[i][iy];
						if(fabs(ymin - ylen) < UM_FUZZ)
							{
							if(xmin > xlen)
								{
								yloc = i;
								xmin = xlen;
								}
							}
						else if(ymin > ylen )
							{
							ymin = ylen;
							yloc = i;
							xmin = xlen;
							}
			 			}
					indx[0] = yloc;
					*max_rad = 0;
					for(i=0;i<npnts;i++)
						{
						if(yloc != i)
							{
							xlen = pt[i][ix] - pt[yloc][ix];
							ylen = pt[i][iy] - pt[yloc][iy];
							radius = (xlen*xlen + ylen*ylen);
							if(*max_rad < radius)
								{
								*max_rad = radius;
								indx[1] = i;
								}
							}
						}
					indx[2] = indx[0];
					*mpnts = 3;
					*min_indx = indx[0];
					*cvh = UM_CVH_LINEAR;
					uu_dexit;
					return;
					}
				}
			}	
													/* determine point with min x or y */
													/* in the local coor. system  */
		yloc = 0;
		xmin = pt[0][ix];
		ymin = pt[0][iy];
		for(i=1;i<npnts;i++)
			{
			xlen = pt[i][ix];
			ylen = pt[i][iy];
			if(fabs(ymin - ylen) < UM_FUZZ)
				{
				if(xmin > xlen)
					{
					yloc = i;
					xmin = xlen;
					}
				}
			else if(ymin > ylen )
				{
				ymin = ylen;
				yloc = i;
				xmin = xlen;
				}
			 }

		 count = 0;
		 *min_indx = yloc;
		 *max_rad = 0.0;
															/* now compute (theta,r) for each
																point in control polygon */
		 for(i=0;i<npnts;i++)
			 {
	 		 if(i != yloc)
				{
				xdis = pt[i][ix] - pt[yloc][ix];
				ydis = pt[i][iy] - pt[yloc][iy];
				radius = (xdis * xdis + ydis * ydis);
				if(radius > *max_rad) *max_rad = radius;
				if(radius > UM_FUZZ)
					{
					pm[count].phi = -xdis/(fabs(xdis)+fabs(ydis));
					pm[count].rad = radius;
					pm[count].x = pt[i][ix];
					pm[count].y = pt[i][iy];
					pm[count].index = i;
					count++;
					 }
				 }
			 else
				 {
				 pm[count].phi = -2.0;
				 pm[count].rad = 0.0;
				 pm[count].x = pt[i][ix];
		 		 pm[count].y = pt[i][iy];
				 pm[count].index = i;
				 count++;
			    }
			}
															/* sort */

			uu_qsort((UU_REAL*)pm,count,sizeof(pm[0]),um_phicmp);

															/* eliminate points along
																identical rays */
			i = 0;
			while(i<count-1)
				{
				if(fabs(pm[i].phi - pm[i+1].phi) < UM_FUZZ)
					{
					if(pm[i].rad < pm[i+1].rad) 
						{
						um_replac(&pm[i],&pm[i+1]);
						}
					for(j=i+1;j<count-1;j++)
						{
						um_replac(&pm[j],&pm[j+1]);
						}
					count--;
			  	   }
				 else 
				   {
				 	i++;
				   }
				}
															/* now have star-shaped polygon 
																test vertices for concavity */
			 if(count > 2)
				{
				 i = 0;
				 while(i<count-2)
				   {
					xdis = (pm[i+1].x - pm[i].x) * (pm[i+2].y - pm[i].y);
					ydis = (pm[i+1].y - pm[i].y) * (pm[i+2].x - pm[i].x);
					if(xdis < ydis || fabs(xdis-ydis) < UM_FUZZ)
						{									/* remove mid-point from hull */
						for(j=i+1;j<count-1;j++)
							{
				 	 		um_replac(&pm[j],&pm[j+1]);
							}
						count--;
						if(i != 0) i--;
					   }
					 else
					   {
						i++;
					   }
			  		 }
				}
															/* check for linearity  */	
				if(count == 3)
					{
					if(fabs(fabs(pm[1].phi)-fabs(pm[2].phi)) < UM_FUZZ)
						{
						if(pm[1].phi * pm[2].phi >= 0)
							{
							if(pm[1].rad < pm[2].rad) um_replac(&pm[1],&pm[2]);
							count--;
							}
						else
							{
							um_replac(&pm[0],&pm[1]);
							um_replac(&pm[1],&pm[2]);
							count--;
							}
						}
					}
														/* now return indx array */
		  		for(i=0;i<count;i++)
					{
					indx[i] = pm[i].index;
					}
				indx[count] = indx[0];
				*mpnts = count+1;
				/* *max_rad = sqrt(*max_rad); */
				if(count == 2)
					{
					*cvh = UM_CVH_LINEAR;
					}
				else
					{
					*cvh = UM_CVH_COMPLEX;
					}
			}
	uu_dexit;
	}
